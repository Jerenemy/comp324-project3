/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["0", "2"]
 * }
 *
 */

#include "cminus.h"

void main() {
    int x = 0 ;

    while (x == 0) {
        print_int(0) ;
        x = 1 ;
    }

    while (false) {
        print_int(1) ;
    }

    print_int(2) ;

    return ;
}
