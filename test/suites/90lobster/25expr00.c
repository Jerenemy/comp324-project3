/*!tests!
 *
 * {
 *      "input":    ["1"],
 *      "exception":  "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    int x = get_int_s();

    print_int(x+1);

    return ;
}