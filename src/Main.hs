{-
    Usage: <Main> < input.txt
    TODO: Tests as per haskell-in-haskell
-}

module Main where

import Intensional (eval)
import Parser (programParser)
import Text.Pretty.Simple (pPrint)
import Transform
import Types
import Text.Parsec

main :: IO ()
main = do
    -- https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:getContents
    contents <- getContents

    case programParser contents of
        -- Is this a tree
        Left err -> error "> Parse Error"
        Right fp ->
            do
                putStrLn "> Pretty-Print the MiniHaskell AST:\n"
                pPrint fp

                -- Print the Transformation Result
                putStrLn "\n\n> Pretty-Print the Intensional AST:\n"
                --pPrint (transform fp)

                -- Print the Eval
                putStrLn "\n\n> Result is:"
                -- If eval == isINum(n) then print else sad panic
                --case eval (transform fp) of
                --    INum n -> print n
                --    IBool b -> print b
                --    _ -> error "> Runtime Error"

-- Extract FP from a Left|Right
fromRight :: (Either ParseError FProgram) -> FProgram
fromRight e = either (error.show) id e