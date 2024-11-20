/*!tests!
 *
 * {
 *      "input":    ["1","true"],
 *      "exception":  "SecurityError"
 * }
 *
 * 
 * {
 *      "input":    ["1","false"],
 *      "output":   ["1"]
 * }
 * 
 */

#include "cminus.h"

/*
void f(x,y) {
    if (y) {
        print_int(x);
    }
    else print_int(x);
    return ;
}
*/

void main() {
    int x = get_int();
    bool y = get_bool_s();

    /*
    f(x,y);
    */

    return ;
}