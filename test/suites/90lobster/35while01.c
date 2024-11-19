/*!tests!
 *
 * {
 *      "input":    ["true"],
 *      "exception":   "SecurityError"
 * }
 *
 * {
 *      "input":    ["false"],
 *      "output":   ["0"]
 * }
 *
 */

#include "cminus.h"

void main() {
    bool b = get_bool_s() ;
    int x = 0;
    while (b) {
        x = 5;
        print_int(x);
        b = false;
    }
    print_int(x);

    return ;
}