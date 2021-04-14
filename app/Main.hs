module Main where

import Control.Exception
import System.IO
import Lib

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "What is your KIT username (uXXXX)?"
  username <- getLine
  putStrLn "What is your password?"
  password <- withEcho False getLine

  cookies <- establishSession username password
  body <- fetchContractView cookies
  extractAndPrintGrades body

withEcho :: Bool -> IO a -> IO a
withEcho echo action =
    bracket (hGetEcho stdin)
            (hSetEcho stdin)
            (const $ hSetEcho stdin echo >> action)
