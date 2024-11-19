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
    if (b) {
        print_int(x);
    }
    else x = 0;
    print_int(x);

    return ;
}