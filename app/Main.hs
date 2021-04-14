module Main where

import System.IO
import Lib

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "What is your KIT username (uXXXX)?"
  username <- getLine
  putStrLn "Where is your password stored?"
  passfile <- getLine

  cookies <- establishSession username passfile
  body <- fetchContractView cookies
  extractAndPrintGrades body
