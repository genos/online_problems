module DNA (toRNA) where

toRNA :: Traversable t => t Char -> Maybe (t Char)
toRNA = traverse (`lookup` [('A', 'U'), ('C', 'G'), ('G', 'C'), ('T', 'A')])
