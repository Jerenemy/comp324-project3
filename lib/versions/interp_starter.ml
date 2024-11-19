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
    [@@deriving show]

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
  [@@deriving show]

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
   * (string*(Value.t->Value.t)) list.  Each element names an API function
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

(* exec p:  Execute the program `p`.
 *)
let exec (_ : Ast.Prog.t) : unit =
  Failures.unimplemented "exec"

