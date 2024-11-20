/*!tests!
 *
 * {
 *      "input":    ["true"],
 *      "exception":   "SecurityError"
 * }
 *
 * {
 *      "input":    ["false"],
 *      "exception":   "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    bool b = get_bool_s() ;
    int x = 0;
    if (b) {
        x = 5;
        print_int(x);
    }
    else x = 0;
    print_int(x);

    return ;
}