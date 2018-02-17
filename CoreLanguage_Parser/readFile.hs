import System.IO
import CoreLanguageParser

main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp))

readF :: IO String
readF = do handle <- openFile "input.txt" ReadMode
           prog <- readloop handle
           hClose handle
           return prog

readloop handle = do ineof <- hIsEOF handle
                     if ineof then return []
                     else do x <- hGetLine handle
                             xs <- readloop handle
                             return (x ++ xs)

comp :: [(Program Name, Name)] -> Program Name
comp [] = error "no parse"
comp [(e, [])] = e
comp [(_, a)] = error("doesn't use all input:\n\n\t" ++ a ++ "\n")