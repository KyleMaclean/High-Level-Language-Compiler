-- compile with: ghc Main.hs -o mtc -hide-all-packages -package base

module Main where

import Parser -- step 1: parse
import Type -- step 2: type check
import Code -- step 3: generate code
import Machine -- step 4: run code

import System.Environment
import Data.Char

main :: IO ()
main = do
  args <- getArgs
-- expect the one and only argument to be the name of the file to process
  let arg0 = head args
  let fileName = takeWhile (/='.') arg0
  let fileExtension = dropWhile (/='.') arg0
  src <- readFile arg0
  case fileExtension of
    ".tam" -> do
      -- save the stack from executing the file's parsed instructions
      stk <- execTAMT (parseTAM src)
      putStrLn ("stack > " ++ (show stk))
    ".mt" -> do
      -- convert the compiled code into a string of newline-separated instructions
      let tam = writeTAM $ compile src
      if tam /= "" then
        writeFile (fileName++".tam") tam
        else
          putStrLn (".tam file not generated")
    where
{-
phases in compiling [TAMInst] from the string contents of the .mt file:
	parseProg - generate an AST of the program
	astCheck - type check the AST and return it unchanged if successful
	generateCode - generate a variable environment and [TAMInst] from the AST
	snd - disregard the variable environment to just return the [TAMInst]
-}
      compile src = snd $ generateCode $ astCheck $ parseProg src