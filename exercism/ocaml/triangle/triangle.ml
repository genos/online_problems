open Base

let ok a b c = a > 0 && b > 0 && c > 0 && a + b >= c && a + c >= b && b + c >= a
let is_equilateral a b c = ok a b c && equal a b && equal b c
let is_isosceles a b c = ok a b c && (equal a b || equal a c || equal b c)
let is_scalene a b c = ok a b c && a <> b && a <> c && b <> c
