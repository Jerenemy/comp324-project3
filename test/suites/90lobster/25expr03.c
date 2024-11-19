/*!tests!
 *
 * {
 *      "input":    ["1","2","3"],
 *      "exception":  "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    int x = get_int();
    int y = get_int();
    int z = get_int_s();

    print_int(x+y+z);

    return ;
}