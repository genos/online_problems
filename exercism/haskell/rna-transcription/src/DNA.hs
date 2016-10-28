module DNA
  (toRNA)
  where

toRNA :: String -> Maybe String
toRNA = mapM comp
  where comp 'A' = Just 'U'
        comp 'C' = Just 'G'
        comp 'G' = Just 'C'
        comp 'T' = Just 'A'
        comp _ = Nothing
