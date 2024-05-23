# Change any of the following NamedTuple values to true to
# enable testing different bonus tasks
tested_bonus_tasks = (rev = true, by = true, lt = true, multiple_matches = true)

import Base.Order

function binarysearch(v, x; by=identity, lt=isless, rev=false)
    o = Base.Order.ord(lt, by, rev)
    lo, hi = 0, length(v) + 1
    @inbounds while lo < hi - 1
        m = lo + ((hi - lo) >> 1)
        if Base.Order.lt(o, v[m], x)
            lo = m
        elseif Base.Order.lt(o, x, v[m])
            hi = m
        else
            a, b = m, m
            @inbounds while a > 1 && v[a - 1] == x
                a -= 1
            end
            @inbounds while b < length(v) && v[b + 1] == x
                b += 1
            end
            return a:b
        end
    end
    lo + 1:hi - 1
end
