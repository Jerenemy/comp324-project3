/*!tests!
 *
 * {
 *      "input":    ["true","1"],
 *      "exception":   "SecurityError"
 * }
 *
 * {
 *      "input":    ["false","1"],
 *      "exception":   "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    bool b = get_bool() ;
    int x = get_int_s();
    while (b) {
        print_int(x);
        b = false;
    }
    print_int(x);

    return ;
}