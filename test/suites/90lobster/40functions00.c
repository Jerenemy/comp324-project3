/*!tests!
 *
 * {
 *      "input":    [],
 *      "exception":  "UnboundVariable"
 * }
 *
 */

#include "cminus.h"

void f() {
    print_int(x);
    return ;
}
void main() {
    int x = 1;
    f() ;

    return ;
}
