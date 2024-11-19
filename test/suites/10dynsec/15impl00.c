/*!tests!
 *
 * {
 *      "input":        ["0"],
 *      "exception":    "SecurityError"
 * }
 *
 * {
 *      "input":        ["1"],
 *      "exception":    "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
  int x = get_int_s() ;

  /* Because x is H, both 0 and 1 are evaluated under an H context,
   * and therefore are H, so this should raise Security.
   */
  if (x == 0) print_int(0) ;
  else print_int(1) ;

  return ;
}
