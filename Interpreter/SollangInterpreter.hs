module Main where

import ErrM

import System.Environment
import System.IO

import Control.Monad.Except
import Control.Monad.Trans.Except

import AbsSollang
import LexSollang
import ParSollang

import Interpreter
import TypeChecker

main :: IO ()
main = do
    args <- getArgs
    case args of {
        [] -> interpretFromStandardInput;
        [fileName] -> interpretFromFile fileName;
        otherwise -> hPutStrLn stderr "You can provide at most one source code file to interpret.";
    }
    return ()

interpretFromStandardInput :: IO ()
interpretFromStandardInput = do
  sourceCode <- hGetContents stdin
  interpretSourceCode sourceCode

interpretFromFile :: String -> IO ()
interpretFromFile fileName = do
    sourceCode <- readFile fileName
    interpretSourceCode sourceCode

interpretSourceCode :: String -> IO ()
interpretSourceCode sourceCode = case pProgram (myLexer sourceCode) of {
    (Ok transformedSourceCode) -> interpetProgram transformedSourceCode;
    (Bad errorString) -> hPutStrLn stderr errorString;
}

interpetProgram :: Program -> IO ()
interpetProgram program = do
  typeCheckResult <- runExceptT (checkTypes program)
  case typeCheckResult of {
    (Left e) -> hPutStrLn stderr e;
    Right () -> executeInterpreter program
  }

executeInterpreter :: Program -> IO ()
executeInterpreter program = do
  interpreterResult <- runExceptT (runInterpreter program)
  case interpreterResult of {
    (Left e) -> hPutStrLn stderr e;
    otherwise -> return ();
  }
