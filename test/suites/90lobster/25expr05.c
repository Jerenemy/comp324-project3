/*!tests!
 *
 * {
 *      "input":    ["BetterNotPrintThis"],
 *      "exception":  "SecurityError"
 * }
 *
 */

#include "cminus.h"

void main() {
    int x = get_string_s();

    print_int(x);

    return ;
}