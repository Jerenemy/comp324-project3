(* C- interpreter with dynamic security enforcement.
 *
 * N. Danner
 *)

(* Raised when a function body terminates without executing `return`.
*)
exception NoReturn of Ast.Id.t

(* MultipleDeclaration x is raised when x is declared more than once in a
* block.
*)
exception MultipleDeclaration of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
*)
exception UnboundVariable of Ast.Id.t

(* UndefinedFunction f is raised when f is called but has not been defined.
*)
exception UndefinedFunction of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
* of the incorrect type.  s is any (hopefuly useful) message.
*)
exception TypeError of string

(* Raised when public output depends on private input.
*)
exception SecurityError


(* Values.
 *)
module PrimValue = struct
  type t = 
    | V_Undefined
    | V_None
    | V_Int of int
    | V_Bool of bool
    | V_Str of string
    (* [@@deriving show] *)

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined -> "?"
    | V_None -> "None"
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Str s -> s
end

(* Security labels.
 *
 * This module defines the two-point security lattice Low <= High.
 *)
module SecLab = struct

  type t = Low | High
  (* [@@deriving show] *)

  let to_string (lambda: t) : string =
    match lambda with
    | Low -> "*low*"
    | High -> "*high*"

  let get_max (lambda: t) (lambda': t) : t =
    match (lambda, lambda') with
    | (Low, Low) -> Low
    | (High, _) -> High
    | (_, High) -> High

  let less_or_equal (lambda: t) (lambda': t) : bool = 
    match (lambda, lambda') with 
    | (High, Low) -> false
    | (_) -> true
end

module Value = struct
  type t = PrimValue.t * SecLab.t

  let get_sec_lab (v_sec : t) : SecLab.t = 
    match v_sec with 
    | (_, lambda) -> lambda

  let get_v_prim (v_sec : t) : PrimValue.t = 
    match v_sec with 
    | (v, _) -> v 
  
  let get_max_lab(v_sec : t) (lambda : SecLab.t) : SecLab.t = 
    SecLab.get_max (get_sec_lab(v_sec)) lambda

  let update_sec_lab (v_sec : t) (lambda : SecLab.t) : t = 
    let v_prim = get_v_prim v_sec in
    (v_prim, get_max_lab v_sec lambda)

  (* let return_nsu (v_sec : t) :  *)

  (* let assign_sec_lab (_) (lambda) *)

  let to_string (v_sec : t) : string = 
    "(" ^ PrimValue.to_string(get_v_prim(v_sec)) ^ ", " ^  SecLab.to_string(get_sec_lab(v_sec)) ^ ")"
end


(* Module for input/output built-in functions.
 *)
module Io = struct

  (* Module for maps with Id.t domain.
   *)
  module IdMap = Map.Make(Ast.Id)

  (* Raised when there is no security policy for a function in the API.
   *)
  exception No_policy of Ast.Id.t

  (* Raised when a function is invoked that is not in the API.
   *)
  exception ApiError of string

  (* The input source and output destination is abstracted, because there
   * are two use cases that are rather different.  The interactive
   * interpreter uses standard input and standard output for input and
   * output.  But the automated tests need the input source and output
   * destination to be programmatic values (the former is read from a test
   * specification, and the latter has to be compared to the test
   * specification).  The "right" way to do this is to make the interpreter
   * itself a functor that takes an IO module as an argument, but that is a
   * little much for this project, so instead we define this Io module with
   * the input source (`in_channel`) and output destination (`output`)
   * references that can be changed by the client that is using the
   * interpreter.
   *)

  (* The input channel.  get_* and prompt_* read from this channel.  Default
   * is standard input.
   *)
  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  (* The output function.  print_* and prompt_* (if !show_prompts = true)
   * call this function for output.  Default is to print the string to
   * standard output and flush.
   *)
  let output : (string -> unit) ref = 
    ref (
      fun s ->
        Out_channel.output_string Out_channel.stdout s ;
        Out_channel.flush Out_channel.stdout
    )

  (* If !show_prompts = true, then the argument to `prompt_*` is output via
   * `output`; otherwise it is ignored.  We have this flexibility so that
   * the testing framework can set it to `false`, and therefore the prompts
   * do not have to be specified as part of the expected output of the
   * programs.
   *)
  let show_prompts : bool ref =
    ref true

  (* outputnl s: output s ^ "\n" via !output.
   *)
  let outputnl (s : string) : unit =
    (!output) (s ^ "\n")

  (* The API definition.  The API is specified by a
   * (string*(PrimValue.t->PrimValue.t)) list.  Each element names an API function
   * and provides the code to be executed when the function is called.
   *
   * This is really ugly; there must be a better way.
   *)
   let api : (PrimValue.t list -> PrimValue.t) IdMap.t =
    [
      ("print_bool", fun vs ->
        match vs with
        | [PrimValue.V_Bool n] -> 
          outputnl (Bool.to_string n) ; PrimValue.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_bool"
      )
    ; ("get_bool", fun vs ->
        match vs with
        | [] -> PrimValue.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for get_bool"
      )
    ; ("prompt_bool", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for prompt_bool"
      )
    ; ("print_int", fun vs ->
        match vs with
        | [PrimValue.V_Int n] -> 
          outputnl (Int.to_string n) ; PrimValue.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_int"
      )
    ; ("get_int", fun vs ->
        match vs with
        | [] -> PrimValue.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for get_int"
      )
    ; ("prompt_int", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for prompt_int"
      )
    ; ("print_string", fun vs ->
         match vs with
         | [PrimValue.V_Str s] -> 
           outputnl s ; PrimValue.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_string"
      )
    ; ("get_string", fun vs ->
        match vs with
        | [] -> PrimValue.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for get_str"
      )
    ; ("prompt_string", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for prompt_str"
      )
    ; ("print_bool_s", fun vs ->
        match vs with
        | [PrimValue.V_Bool n] -> 
          outputnl (Bool.to_string n) ; PrimValue.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_bool_s"
      )
    ; ("get_bool_s", fun vs ->
        match vs with
        | [] -> PrimValue.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for get_bool_s"
      )
    ; ("prompt_bool_s", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for prompt_bool_s"
      )
    ; ("print_int_s", fun vs ->
        match vs with
        | [PrimValue.V_Int n] -> 
          outputnl (Int.to_string n) ; PrimValue.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_int_s"
      )
    ; ("get_int_s", fun vs ->
        match vs with
        | [] -> PrimValue.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for get_int_s"
      )
    ; ("prompt_int_s", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for prompt_int_s"
      )
    ; ("print_string_s", fun vs ->
         match vs with
         | [PrimValue.V_Str s] -> 
           outputnl s ; PrimValue.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_string_s"
      )
    ; ("get_string_s", fun vs ->
        match vs with
        | [] -> PrimValue.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for get_str_s"
      )
    ; ("prompt_string_s", fun vs ->
        match vs with
        | [PrimValue.V_Str s] ->
          if !show_prompts then (!output) s else () ;
            PrimValue.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for prompt_str_s"
      )
    ] |> List.to_seq |> IdMap.of_seq

  (* get_policy f = ([l_0,...,l_{n-1}], l), where:
   * - l_i is an upper bound on the security level of the i-th argument to f;
   * - l is the security level with which to label the return value of f.
   *
   * Raises: No_policy if there is no security policy for f.
   * Raises: ApiError if f is not an API function.
   *
   *)
  let get_policy (f : Ast.Id.t) : (SecLab.t list)*SecLab.t =
    if not (IdMap.mem f api) then
      raise (ApiError f)
    else
      match f with
      | ("print_int" | "print_bool" | "print_string") ->
        ([Low], Low)
      | ("prompt_int" | "prompt_bool" | "prompt_string") ->
        ([Low], Low)
      | ("get_int_s" | "get_bool_s" | "get_string_s") ->
        ([], High)
      | ("prompt_int_s" | "prompt_bool_s" | "prompt_string_s") ->
        ([High], High)
      | _ ->
        raise @@ No_policy f

  (* do_call f vs invokes the API function corresponding to `f` with argument
   * list `vs`.
   *
   * Raises ApiError f: if f is not an API function.
   *)
  let do_call (f : string) (vs : PrimValue.t list) : PrimValue.t =
    try
      IdMap.find f api vs
    with
    | Not_found -> raise @@ ApiError f

end
(* Environments. An environment is a finite map from identifiers to values.
* We will interchangeably treat environments as functions or sets or lists
* of pairs in documentation. We will use ￿ as a metavariable over
* environments.
*)

(* Environments. An environment is a finite map from identifiers to values.
* We will interchangeably treat environments as functions or sets or lists
* of pairs in documentation. We will use ￿ as a metavariable over
* environments.
*)
module Env = struct
  module IdMap = Map.Make(Ast.Id)
  
  (* The type of environments.
  *)
  type t = Value.t IdMap.t
  
  (* to_string rho = a string representation of rho.
  *)
  let to_string (rho : t) : string=
    rho |> IdMap.to_list
    |> List.map (
      fun (id, v) -> id ^ ": " ^ Value.to_string v
    )
    |> String.concat ", "

  (* pp fmtr rho : pretty-print rho to `fmtr`. We don't use this directly, it
  * is just needed for `ppx_deriving` in `EnvBlock.t`.
  *)
  (* let pp (fmtr : Format.formatter) (rho : t) : unit=
    Format.fprintf fmtr "%s" (to_string rho) *)

  (* empty = [].
  *)
  let empty = IdMap.empty
    
  (* from_list bindings = rho, where dom rho = {x : (x, _) rho bindings} and
  *
  * rho(x) = v, where (x, v) rho bindings.
  *
  * Pre-condition: for any x, there is at most one pair (x, _) rho bindings.
  *)
  let from_list : (Ast.Id.t*Value.t) list -> t =
    IdMap.of_list

  (* lookup rho x = rho(x).
  *
  * Raises: Not_found if x not in dom rho.
  *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t =
    IdMap.find x rho

  (* reset rho x v = rho [x → v].
  *
  * Raises: Not_found if x not in dom rho.
  *)
  let reset (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (* TODO: add no-sensitive upgrade rule here *)
    (* NOTE dont need sec_context here, since sec_context of v already eval'd *)
    if IdMap.mem x rho
    then 
      let lambda_old = Value.get_sec_lab (IdMap.find x rho) in
      if (SecLab.less_or_equal (Value.get_sec_lab v) lambda_old)
      then IdMap.add x v rho 
      else raise @@ SecurityError

(* 
      | (SecLab.Low, SecLab.Low) -> IdMap.add x v rho
      | (SecLab.High, SecLab.High) -> IdMap.add x v rho
      | (SecLab.Low, SecLab.High) -> raise @@ SecurityError
      | (SecLab.High, SecLab.Low) -> IdMap.add x (Value.update_sec_lab v SecLab.High) rho *)
    else raise Not_found

  (* declare rho x v = rho [x→v]
  *
  * Raises: MultipleDeclaration if x in dom rho.
  *)
  let declare (rho : t) (sec_context : SecLab.t) (x : Ast.Id.t) (v : Value.t) : t =
    if IdMap.mem x rho (* 'mem' doesn't care about vals, only cares about keys *)
    then raise @@ MultipleDeclaration x
    else IdMap.add x (Value.update_sec_lab v sec_context) rho
  end


(* An environment block, which we think of as a non-empty list of
* environments representing block nesting in C-.
*)
module EnvBlock = struct
  (* A type for non-empty lists. The value (rho, rhos) represents what we think
  * of as rho :: rhos. In documentation, when we want to refer to "cons" for
  * values of type `t`, we'll write `:::`.
  *
  * This seems cleaner than using `Env.t list`, since now every value of
  * type `t` represents a non-empty list. It ensures that clients of
  * `EnvBlock`, like `Frame`, don't ever have to write code to check for
  * possibly empty lists of environments; that is all handled here.
  *)
  type t = Env.t * (Env.t list)
  (* [@@deriving show] *)
    
  (* empty = [{}]
  *)
  let empty : t = (Env.empty, [])

  (* push rho rhos = rho ::: rhos.
  *)
  let push (rho : Env.t) ((rho', rhos') : t) : t =
    (rho, rho' :: rhos')

  (* pop rhos = rhos', where rhos = _ ::: rhos'.
  *)
  let pop (rhos : t) : t =
    match rhos with
    | (_, []) -> Failures.impossible "EnvBlock.pop []"
    | (_, rho :: rhos) -> (rho, rhos)
  
  (* from_list bindings = [Env.from_list bindings].
  *)
  let from_list (bindings : (Ast.Id.t*Value.t) list) : t =
    (Env.from_list bindings, [])
  
  (* split [rho_0; rho_1; ...; rho_{n-1}] = (rho_0, [rho_1; ...; rho_{n-1}]).
  *
  * I.e., `split` converts a non-empty OCaml list of environments to a
  * value of type `t`.
  *)
  let split (rhos : Env.t list) : t =
    match rhos with
    | [] -> Failures.impossible "EnvBlock.split []"
    | rho :: rhos -> (rho, rhos)

  (* lookup rhos x = v, where rhos = [rho_0; ...; rho_{n-1}], rho_i(x) = v, and
  * x is not in dom rho_j for j < i.
  *
  * Raises UnboundVariable if there is no rho in rhos with x in dom rho.
  *)
  let rec lookup ((rho, rhos) : t) (x : Ast.Id.t) : Value.t =
    try Env.lookup rho x
    with
      | Not_found ->
      match rhos with
      | [] -> raise @@ UnboundVariable x
      | _ -> lookup (split rhos) x

  
  (* reset rhos x v = [rho_0; ...; rho_i[x→v]; ...]
  *
  * Raises UnboundVariable if x not in any rho_i.
  *)
  (* reset [rho_0,...,rho_{n-1}] x v = Envs [rho_0,...,rho_i[x→v],...],
  * where x rho dom(rho_i) and x not in dom(rho_j) for j < i.
  *
  * Raises UnboundVariable if x not in dom rho_i for any i.
  *)
  let rec reset ((rho, rhos) : t) (x : Ast.Id.t) (v : Value.t) : t =
    (* has 4 let stms, then do let l = get_max(s0, s1, context)
    move calculation into let stms, put vars down  *)
    try (Env.reset rho x v, rhos)
    with
      | Not_found ->
      match rhos with
        | [] -> raise @@ UnboundVariable x
        | _ -> push rho (reset (split rhos) x v)

  (* declare [rho_0,rho_1,...] x v = Envs [rho_0[x→v],rho_1,...]
  *
  * Raises: MultipleDeclaration if x in dom rho_0.
  *)
let declare ((rho, rhos) : t) (sec_context : SecLab.t) (x : Ast.Id.t) (v : Value.t) : t =
  (Env.declare rho sec_context x v, rhos)
end

module Frame= struct

  (* A frame is either an environment block or a return.
  *)
  type t =
    | Envs of EnvBlock.t
    | Return of Value.t
    (* NOTE how does this propagate down? 
    lets say an expr is eval'd under secerrframe, then does the expr eval to secerrval? 
    or do i raise SecurityError?  *)
    (* | SecErrFrame *)
  (* [@@deriving show] *)

  (* A base environment block frame.
  *)
  let base : t = Envs EnvBlock.empty


  (* from_list bindings = Envs (EnvBlock.from_list bindings)
  *)
  let from_list (bindings : (Ast.Id.t*Value.t) list) : t =
    Envs (EnvBlock.from_list bindings)

  (* lookup eta x = v, where eta = Envs [rho_0; ...; rho_{n-1}], rho_i(x) = v, and
  * x is not in dom rho_j for j < i.
  *
  * I.e., lookup eta x = rho_i(x), where rho_i is the first environment in eta with
  * x in its domain.
  *
  * Raises: Failures.impossible if eta is a return frame.
  * Raises UnboundVariable if x not in dom rho_i for any i.
  *)
  let lookup (eta : t) (x : Ast.Id.t) : Value.t =
    match eta with
    | Envs eta -> EnvBlock.lookup eta x
    | Return _ -> Failures.impossible "Frame.lookup (Return _)"
    (* | SecErrFrame -> Failures.impossible "Frame.lookup (SecErrFrame _)" *)

  (* reset eta x v = Envs [rho_0; ...; rho_i[x→v]; ...]
  *
  * Raises: Failures.impossible if eta is a return frame.
  * Raises UnboundVariable if x not in any rho_i.
  *)
  let reset (eta : t) (x : Ast.Id.t) (v : Value.t) : t =
    match eta with
    | Envs rhos -> Envs (EnvBlock.reset rhos x v)
    | Return _ -> Failures.impossible "Frame.reset (Return _)"
    (* | SecErrFrame -> Failures.impossible "Frame.lookup (SecErrFrame _)" *)
  

  (* declare eta x v = Envs [rho_0[x→v]; rho_1; ...]
  *
  * Raises: Failures.impossible if eta is a return frame.
  * Raises: MultipleDeclaration if x in dom rho_0.
  *)
  let declare (eta : t) (sec_context : SecLab.t) (x : Ast.Id.t) (v : Value.t) : t =
    match eta with
    | Envs rhos -> Envs (EnvBlock.declare rhos sec_context x v)
    | Return _ -> Failures.impossible "Frame.declare (Return _)"
    (* | SecErrFrame -> Failures.impossible "Frame.lookup (SecErrFrame _)" *)


  (* push eta = Envs (Env.empty :: rhos).
  * push (Return _): raises Failure.
  *)
  let push (eta : t) : t =
    match eta with
    | Envs rhos -> Envs (EnvBlock.push Env.empty rhos)
    | Return _ -> Failures.impossible "Frame.push (Return _)"
    (* | SecErrFrame -> Failures.impossible "Frame.lookup (SecErrFrame _)" *)
    
(* pop eta = Envs rhos, where eta = Envs (rho :: rhos).
  * pop (Return _): raises Failure
  *)
  let pop (eta : t) : t =
    match eta with
    | Envs rhos -> Envs (EnvBlock.pop rhos)
    | Return _ -> Failures.impossible "Frame.pop (Return _)"
    (* | SecErrFrame -> Failures.impossible "Frame.lookup (SecErrFrame _)" *)


    (* let return_nsu (v_sec : Value.t) : Frame.t *)
end


(* exec p: Execute the program `p`.
*)
let exec (Pgm fundefs : Ast.Prog.t) : unit=

  (* find_def f = (ps, stms), where FunDef(f, ps, stms) occurs in `fundefs`.
  *
  * I.e., find_def f returns the definition of `f` in the source program.
  *
  * Raises: Not_found if f is not defined in the source program.
  *)
  let find_def (f : Ast.Id.t) : (Ast.Id.t list)*(Ast.Stm.t list) =
    (* TODO: may need to add context in here too *)
    let 
      FunDef(_, params, body) = List.find (fun (Ast.Prog.FunDef(f', _, _)) -> f' = f) fundefs 
    in
      (params, body)
  in
  
  (* unop op v = v', where v' is the result of applying the metalanguage
  * operation corresponding to `op` to `v`.
  *)
  let unop (op : Ast.Expr.unop) (sec_context : SecLab.t) (v : Value.t) : Value.t =
      (* need function to return the int with the unop applied to the print, and the sec val and the sec_context 
      (check op sems to confirm) *)
      let sec_context' = Value.get_max_lab v sec_context in
      match (op, Value. get_v_prim v) with
      | (Neg, PrimValue.V_Int n) -> (PrimValue.V_Int (-n), sec_context')
      | (Not, PrimValue.V_Bool b) -> (PrimValue.V_Bool (not b), sec_context')
      | _ -> raise @@ TypeError (
        Printf.sprintf "Bad operand types: %s %s"
        (Ast.Expr.show_unop op) (PrimValue.to_string (Value.get_v_prim v))
        )
  in
  
  (* binop op v v' = the result of applying the metalanguage operation
  * corresponding to `op` to v and v'.
  *)
  let binop (op : Ast.Expr.binop) (sec_context : SecLab.t) (v : Value.t) (v' : Value.t) : Value.t =
    (* need function to return the int with the unop applied to the print, 
    and the max sec val of all the vals and the sec_context 
      (check op sems to confirm) *)
    let sec_context' = Value.get_max_lab v (Value.get_max_lab v' sec_context) in 
    match (op, Value.get_v_prim v, Value.get_v_prim v') with
    | (Plus, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Int (n + n'), sec_context')
    | (Minus,PrimValue.V_Int n,PrimValue.V_Int n') ->  (PrimValue.V_Int (n - n'), sec_context')
    | (Times,PrimValue.V_Int n,PrimValue.V_Int n') ->  (PrimValue.V_Int (n * n'), sec_context')
    | (Div, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Int (n / n'), sec_context')
    | (Mod, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Int (n mod n'), sec_context')
    | (And, PrimValue.V_Bool b, PrimValue.V_Bool b') ->  (PrimValue.V_Bool (b && b'), sec_context')
    | (Or, PrimValue.V_Bool b, PrimValue.V_Bool b') ->  (PrimValue.V_Bool (b || b'), sec_context')
    | (Eq, v, v') ->  (V_Bool (v = v'), sec_context')
    | (Ne, v, v') ->  (V_Bool (v <> v'), sec_context')
    | (Lt, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Bool (n < n'), sec_context')
    | (Le, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Bool (n <= n'), sec_context')
    | (Gt, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Bool (n > n'), sec_context')
    | (Ge, PrimValue.V_Int n, PrimValue.V_Int n') ->  (PrimValue.V_Bool (n >= n'), sec_context')
    | _ -> raise @@ TypeError (
      Printf.sprintf "Bad operand types: %s %s %s"
      (Value.to_string v) (Ast.Expr.show_binop op) (Value.to_string v')
    )
  in
  
  (* eval eta e = v, where eta |- e ↓ v.
  *)

  let rec eval (rhos : EnvBlock.t) (sec_context : SecLab.t) (e : Ast.Expr.t) : Value.t =
    match e with
    | Var x -> 
      (* need function in Value.t that when get v_sec = eta(x), evals to new sec_context v_sec. this is easy, just take max *)
      let v_sec_x = EnvBlock.lookup rhos x in
      Value.update_sec_lab v_sec_x sec_context
    | Num n -> (PrimValue.V_Int n, sec_context)
    | Bool b -> (PrimValue.V_Bool b, sec_context)
    | Str s -> (PrimValue.V_Str s, sec_context)
    | Unop (op, e) ->
      unop op sec_context (eval rhos sec_context e)
    | Binop (op, e, e') ->
      binop op sec_context (eval rhos sec_context e) (eval rhos sec_context e')
    | Call(f, es) ->
      try
        let (params, body) : (Ast.Id.t list)*(Ast.Stm.t list) = find_def f in
        let rhos' : EnvBlock.t =
          EnvBlock.from_list @@ List.combine params (List.map (eval rhos sec_context) es) in
        begin
          match exec_stms rhos' sec_context body with
          | Envs _ -> raise @@ NoReturn f
          (* | SecErrFrame -> raise @@ SecurityError *)
          | Return v -> v 
          (* would be redundant to check if this v was low, since if it got through a return stm in exec_stm it must be low  *)
        end
      with
        | Invalid_argument _ -> raise @@ TypeError "Incorrect number of arguments for call"
    | Not_found ->
      try
        let args = List.map (eval rhos sec_context) es in
        (* let prim_args = List.map Value.get_v_prim args in *)
        let args_sec_contexts = List.map Value.get_sec_lab args in 
        (* let sec_context' = List.fold_left SecLab.get_max sec_context args_sec_contexts in  *)
        let prim_args = List.map Value.get_v_prim args in
        begin
        try
          let (arg_policies, ret_policy) = Io.get_policy f in

          let args_ok = List.for_all2 SecLab.less_or_equal args_sec_contexts arg_policies in
              if not args_ok then
                raise SecurityError;

            (* Ensure the current security context is compatible with the return policy *)
            if not (SecLab.less_or_equal ret_policy sec_context) then
              raise SecurityError;
          let result_prim = Io.do_call f prim_args in
          (result_prim, ret_policy)
        with
      | Io.No_policy f ->
          (* Handle undefined policy: Allow permissive behavior or fail gracefully *)
          (* Example: Assume Low return level for undefined functions *)
          let result = Io.do_call f prim_args in
          (result, SecLab.Low)
      | Io.ApiError _ ->
          raise (UndefinedFunction f)
        end


(* 
        if sec_context' = SecLab.Low 
          then let _ = print_string (SecLab.to_string sec_context') in
            raise @@ SecurityError
          then (Io.do_call f prim_args, sec_context')
      else 
        let _ = print_string (SecLab.to_string sec_context') in
        raise @@ SecurityError *)
      

      with
        | Io.ApiError _ -> raise (UndefinedFunction f)
      
  
  (* do_decs eta [..., (x, Some e), ...] = eta'', where eta'' is obtained by adding
  * x → v to eta', where eta' |- e ↓ v.
  * do_decs eta [..., (x, None), ...] = eta'', where eta'' is obtained by adding
  * x → V_Undefined to eta.
  *
  * I.e., do_decs eta decs adds the declarations in `decs` to eta.
  *
  * Raises: MultipleDeclaration if there is a binding in the top
  * environment in eta for some variable in `decs`.
  *)
  and do_decs
      (rhos : EnvBlock.t)
      (sec_context : SecLab.t) 
      (decs : (Ast.Id.t * Ast.Expr.t option) list) : EnvBlock.t =
    match decs with
    | [] -> rhos
    | (x, None) :: decs ->
      let rhos' = EnvBlock.declare (rhos) sec_context x (V_Undefined, sec_context) in
        do_decs rhos' sec_context decs
    | (x, Some e) :: decs ->
      let v = eval rhos sec_context e in
      let rhos' = EnvBlock.declare rhos sec_context x v in
        do_decs rhos' sec_context decs
  

  (* exec_stm rhos stm = eta, where stm |- rhos → eta.
  *)
  (* and exec_stm (rhos : Frame.t) (sec_context : SecLab.t) (stm : Ast.Stm.t) : Frame.t = *)
  and exec_stm (rhos : EnvBlock.t) (sec_context : SecLab.t) (stm : Ast.Stm.t) : Frame.t =
    match stm with
    | VarDec decs -> Frame.Envs (do_decs rhos sec_context decs)
    | Assign(x, e) ->
      let v = eval rhos sec_context e in
        Frame.Envs (EnvBlock.reset rhos x v) (* TODO implement NSU in reset *)
    | Expr e ->
      let _ = eval rhos sec_context e in
        Frame.Envs rhos
    | Block ss ->
      let rhos' = (EnvBlock.push Env.empty rhos) in
      (* TODO is exec_stms supposed to return a tuple with the sec_context? 
      may need to implement NSU here and only allow return when in low sec context.
      may need to re-eval sec context for return v regardless (but with nsu just raise error if its low?) 
      which rules does NSU change?  *)
      begin
        match exec_stms rhos' sec_context ss with
        (* here, eval new sec context, then re-eval return stm and pass it back in to exec_stm to follow operational semantics *)
        | Return v -> Return v
        (* already checks in exec_stms and returns sec_error there *)
        | Envs rhos'' -> 
          Frame.Envs (EnvBlock.pop rhos'')
        (* | _ -> raise @@ TypeError "help" *)
        (* | Frame.SecErrFrame -> Failures.impossible "sef" *)
      end
    | IfElse(e, s0, s1) ->
      let v = eval rhos sec_context e in
      let sec_context' = Value.get_sec_lab v in (* get updated sec context of v after evaluation *)
      let v_prim = Value.get_v_prim v in 
      begin
        match v_prim with
        | PrimValue.V_Bool true -> exec_stm (EnvBlock.push Env.empty rhos) sec_context' (Block [s0])
        | PrimValue.V_Bool false -> exec_stm (EnvBlock.push Env.empty rhos) sec_context' (Block [s1])
        | _ -> raise @@ TypeError ("Conditional test not a boolean value: " ^ Value.to_string v)
      end
     | While(e, body) ->
      (* dowhile rhos = eta, where while e do body |- rhos → eta.
      *)
      let rec dowhile (rhos : EnvBlock.t) (sec_context : SecLab.t) : Frame.t =
        let v = eval rhos sec_context e in
        let sec_context' = SecLab.get_max (Value.get_sec_lab v) sec_context in (* get updated sec context of v after evaluation *)
        let v_prim = Value.get_v_prim v in 
        begin
          match v_prim with
          | PrimValue.V_Bool false -> Frame.Envs rhos
          | PrimValue.V_Bool true ->
            begin
              match exec_stm rhos sec_context' (Block [body]) with
              | Frame.Return v -> Frame.Return v
              | Frame.Envs rhos' -> dowhile rhos' sec_context'
              (* | _ -> raise @@ TypeError "unimplemented" *)
              (* | eta' -> dowhile eta' sec_context' *)
            end
          | _ -> raise @@ TypeError ("While test not a boolean value: " ^ Value.to_string v)
        end
      in
      dowhile rhos sec_context

    | Return (Some e) ->
      (* implement NSU *)
      let v = eval rhos sec_context e in
      begin
        match Value.get_sec_lab v with
        | SecLab.Low -> Frame.Return v 
        | SecLab.High -> raise @@ SecurityError
      end
      
    | Return None ->
      (* implement NSU *)
      begin
        match sec_context with 
        | SecLab.Low -> Frame.Return (PrimValue.V_None, sec_context) (* is there a prettier way of doing this? *)
        | SecLab.High -> raise @@ SecurityError
      end
    
    (* | _ -> raise @@ Failures.impossible "help" *)
      
  (* exec_stms rhos stms = eta, where stms |- rhos → eta.
  *)
  and exec_stms (rhos : EnvBlock.t) (sec_context : SecLab.t) (stms : Ast.Stm.t list) : Frame.t =
    match stms with
    | [] -> Frame.Envs rhos
    | stm :: stms ->
      begin
        match exec_stm rhos sec_context stm with
        | Return v -> Return v
        | Envs rhos' -> exec_stms rhos' sec_context stms
      end
  in

  let _ = eval EnvBlock.empty SecLab.Low (Call("main", [])) in
  ()