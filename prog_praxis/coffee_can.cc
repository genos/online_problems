/*
I went a slightly different route, both with language (C++11) and simulation
technique. As for the problems, the second one tripped me up. Moreover, it
reminded me of a similar problem in Hofstadter's "Godel, Escher, Bach" that
also stumped me a bit. You'd think I would have learned my lesson.

[sourcecode lang="cpp"]
*/
#include <iostream>
#include <random>

auto coffee_can(uintmax_t black, uintmax_t white) -> char {
    if ((black + white) == 0) { return 'X'; }  // error
    auto d = std::uniform_real_distribution<double>{0.0, 1.0};
    auto e = std::default_random_engine{std::random_device{}()};
    auto ww = [&d, &e](uintmax_t b, uintmax_t w) -> double {
        return d(e) < ((w * (w - 1.0)) / ((w + b) * (w + b - 1.0)));
    };
    while ((black + white) > 1) {
        if (ww(black, white)) {
            ++black;
            white -= 2;
        } else {
            --black;
        }
    }
    return (black == 1 ? 'b' : 'w');
}

auto main(int argc, char *argv[]) -> int {
    if (argc != 3) {
        std::cerr << "usage: " << argv[0] << " <#black> <#white>" << std::endl;
        return EXIT_FAILURE;
    } else {
        auto black = uintmax_t(std::stol(argv[1])),
             white = uintmax_t(std::stol(argv[2]));
        std::cout << coffee_can(black, white) << std::endl;
    }
}
/*
[/sourcecode]
*/
