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
 *       "exception":  "SecurityError"
 * }
 * 
 */

#include "cminus.h"


int f(int x,bool y) {
    if (y) {
        print_int(x);
    }
    else print_int(x);
    return 0;
}


void main() {
    int x = get_int();
    bool y = get_bool_s();

    
    f(x,y);
    

    return ;
}