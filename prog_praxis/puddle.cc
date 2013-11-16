A C++11 version of the answer described on his blog (not feeling too
creative today myself):
[sourcecode lang="cpp"]
#include <algorithm>
#include <iostream> #include <vector>

template <typename UInt>
auto calculate_volume(const std::vector<UInt>& heights) -> UInt {
    auto lmax = UInt{0}, rmax = UInt{0}, volume = UInt{0};
    auto left = size_t{0}, right = heights.size() - 1;
    while (left < right) {
        lmax = std::max(lmax, heights[left]);
        rmax = std::max(rmax, heights[right]);
        if (lmax >= rmax) {
            volume += rmax - heights[right];
            --right;
        } else {
            volume += lmax - heights[left];
            ++left;
        }
    }
    return volume;
}

auto main() -> int {
    auto puddles = std::vector<std::vector<uintmax_t>>
        {{2, 5, 1, 2, 3, 4, 7, 7, 6},
         {2, 5, 1, 3, 1, 2, 1, 7, 7, 6},
         {2, 7, 2, 7, 4, 7, 1, 7, 3, 7},
         {6, 7, 7, 4, 3, 2, 1, 5, 2},
         {2, 5, 1, 2, 3, 4, 7, 7, 6, 2, 7, 1, 2, 3, 4, 5, 5, 4}};
    for (auto p: puddles) {
        std::cout << calculate_volume(p) << std::endl;
    }
    return EXIT_SUCCESS;
}
[/sourcecode]
