{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
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

data Destination = Name Text | Accept | Reject
data Rule = Immediate Destination | Step {_predicate :: Part -> Bool, _destination :: Destination}
data Part = Part {_x :: Int, _m :: Int, _a :: Int, _s :: Int}

readSetup :: Text -> (Map Text [Rule], [Part])
readSetup = either (error "Bad parse") id . parseOnly ((,) <$> workflows <*> ("\n\n" *> parts <* endOfInput))
  where
    workflows = M.fromList <$> workflow `sepBy1'` "\n"
    parts = part `sepBy1'` "\n"
    workflow = (,) <$> takeTill (== '{') <*> ("{" *> ((rule `sepBy1'` ",") <* "}"))
    part = Part <$> ("{x=" *> decimal) <*> (",m=" *> decimal) <*> (",a=" *> decimal) <*> (",s=" *> decimal <* "}")
    rule = choice [Step <$> predicate <*> (":" *> destination), Immediate <$> destination]
    predicate = do
        cat <- choice [_x <$ "x", _m <$ "m", _a <$ "a", _s <$ "s"]
        lg <- choice [(<) <$ "<", (>) <$ ">"]
        n <- decimal
        pure $ \p -> lg (cat p) n
    destination = choice [Accept <$ "A", Reject <$ "R", Name <$> takeTill (not . isAlpha)]

part1 :: Map Text [Rule] -> [Part] -> Int
part1 flows = sum . fmap (\(Part x m a s) -> x + m + a + s) . filter accept
  where
    accept p = go (Name "in")
      where
        go Accept = True
        go Reject = False
        go (Name n) = go $ (head . mapMaybe push) (flows M.! n)
        push (Immediate d) = Just d
        push (Step f d) = if f p then Just d else Nothing

main :: IO ()
main = do
    print . uncurry part1 $ readSetup test
    input <- readSetup <$> T.readFile "input.txt"
    print $ uncurry part1 input
