/*!tests!
 *
 * {
 *      "input":    ["true","true"],
 *      "exception":   "SecurityError"
 * }
 *
 * {
 *      "input":    ["true","false"],
 *      "exception":   "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    bool b = get_bool_s();
    bool c = get_bool():
    int x = 0;
    while (b) {
        x = 5;
        if (c) {
            print_int( x + 1 );
        }
        else x = x - 1;
        
        b = false;
    }
    print_int(x);

    return ;
}