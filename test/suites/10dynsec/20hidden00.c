/*!tests!
 *
 * {
 *      "input":        ["true"],
 *      "exception":    "SecurityError"
 * }
 *
 * {
 *      "input":        ["false"],
 *      "exception":    "SecurityError",
 *      "fail_ok":      true
 * }
 *
 */

#include "cminus.h"

bool f(bool x) {
  bool y = false ;
  bool z = true ;

  /* Suppose we only track the security context.  Then when we get to this
  *  point, y = false(L) and z = false(L).  And:
  *  - x = true(H)  => first test true, second test false
  *                 => y = true(H), z = true(L).
  *  - x = false(H) => first test false, second test true
  *                 => z = false(L) (b/c !y = true(L)).
  *  
  *  With NSU, when x = true(H) the evaluation of y = true fails.  But when
  *  x = false(H), the evaluation of z = false succeeds with z = false(L),
  *  and we leak the information that x = false.
  */
  if (x) y = true ;
  if (!y) z = false ;

  return z ;
 
}

void main() {
  bool x = get_bool_s() ;

  print_bool(f(x)) ;

  return ;
}
