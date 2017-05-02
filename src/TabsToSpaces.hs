
defaultTabSize = 8

tabsToSpaces :: Int -> String -> String
tabsToSpaces tabSize line = tabsToSpaces' 0 line
  where
    tabsToSpaces' position ('\t':xs) = 
      let nSpaces = tabSize - (position `rem` tabSize)
          spaces = replicate nSpaces ' '
          newPosition = position + nSpaces
      in  spaces ++ tabsToSpaces' newPosition xs

    tabsToSpaces' position (x:xs) = x : tabsToSpaces' (position + 1) xs
    tabsToSpaces' position [] = []

main = interact (unlines . (map $ tabsToSpaces defaultTabSize)  . lines)

