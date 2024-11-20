/*!tests!
 *
 * {
 *      "input":    ["1"],
 *      "exception":  "SecurityError"
 * }
 *
 */

#include "cminus.h"

void f(int x) {
    print_int(x);
    return ;
}


void main() {
    int x = get_int_s();
    f(x);
    return ;
}