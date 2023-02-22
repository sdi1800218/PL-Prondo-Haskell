{-
    Usage: <Main> < input.txt
    TODO: Tests as per haskell-in-haskell
-}

module Main where

import Intensional
import Parser (programParser)
import Text.Pretty.Simple (pPrint)
import Transform
import Types
import Text.Parsec

main :: IO ()
main = do
    contents <- getContents

    case programParser contents of
        -- TODO
        Left err -> error "> Parse Error"
        Right fp ->
            do
                putStrLn "> Pretty-Print the MiniHaskell AST:\n"
                pPrint fp

                -- Print the Transformation Result
                putStrLn "\n\n> Pretty-Print the Intensional AST:\n"
                pPrint (transform fp)

                -- Print the Eval
                putStrLn "\n\n> Result is:"
                --case eval (transform fp) of
                --    INum n -> print n
                --    IBool b -> print b
                --    _ -> error "> Runtime Error"

-- Extract FP from a Left|Right
fromRight :: (Either ParseError FProgram) -> FProgram
fromRight e = either (error.show) id e