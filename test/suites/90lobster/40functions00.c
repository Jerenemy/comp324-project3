/*!tests!
 *
 * {
 *      "input":    [],
 *      "exception":  "UnboundVariable"
 * }
 *
 */

#include "cminus.h"

/*
void f() {
    print_int(1);
    return ;
}
*/
void main() {
    int x = 1;
    /*
    f() ;
    */

    return ;
}
