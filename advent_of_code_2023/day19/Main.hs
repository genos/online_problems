{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.IO qualified as T

test :: Text
test =
    "px{a<2006:qkq,m>2090:A,rfg}\n\
    \pv{a>1716:R,A}\n\
    \lnx{m>1548:A,A}\n\
    \rfg{s<537:gd,x>2440:R,A}\n\
    \qs{s>3448:A,lnx}\n\
    \qkq{x<1416:A,crn}\n\
    \crn{x>2662:A,R}\n\
    \in{s<1351:px,qqz}\n\
    \qqz{s>2770:qs,m<1801:hdj,R}\n\
    \gd{a>3333:R,R}\n\
    \hdj{m>838:A,pv}\n\
    \\n\
    \{x=787,m=2655,a=1222,s=2876}\n\
    \{x=1679,m=44,a=2067,s=496}\n\
    \{x=2036,m=264,a=79,s=2244}\n\
    \{x=2461,m=1339,a=466,s=291}\n\
    \{x=2127,m=1623,a=2188,s=1013}"


main :: IO ()
main = T.putStrLn test
