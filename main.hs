import Parser
import Printer

import Text.Parsec (parse)

main = do
  cont <- readFile "test.lux"
  let (Right ast) = parse phrasingContents "test.lux" cont
  writeFile "test.hs" $ toHaskell ast
