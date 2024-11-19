/*!tests!
 *
 * {
 *      "input":    ["true"],
 *      "output":   ["5", "0"]
 * }
 *
 * {
 *      "input":    ["false"],
 *      "output":   ["0", "0"]
 * }
 *
 */

#include "cminus.h"

void main() {
    bool b = get_bool() ;
    int x = 0;
    if (b) {
        int x = 5;
        print_int(x);
    }
    else print_int(x);

    print_int(x);

    return ;
}
