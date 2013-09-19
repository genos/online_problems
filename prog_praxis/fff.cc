#include <iostream>
#include <vector>
#include "maybe.h"

using namespace maybe;

auto fff2(uintmax_t upper_bound) ->  Maybe<uintmax_t> {
    auto sieve = std::vector<uintmax_t>(upper_bound);
    // sieve
    for (uintmax_t p = 2; p < upper_bound; ++p) {
        if (!sieve[p]) {
            for (uintmax_t i = p; i < upper_bound; i += p) {
                sieve[i] += 1;
            }
        }
    }
    // search
    for (uintmax_t i = 4; i < upper_bound; ++i) {
        if (sieve[i - 3] == 4 && sieve[i - 2] == 4 &&
            sieve[i - 1] == 4 && sieve[i] == 4) {
            return Just(i - 3);
        }
    }
    return Nothing();
}

auto main(int argc, char *argv[]) -> int {
    auto upper_bound = uintmax_t(10000);
    if (argc > 1) {
        upper_bound = std::stol(argv[1]);
    }
    std::cout << fff2(upper_bound) << std::endl;
}
