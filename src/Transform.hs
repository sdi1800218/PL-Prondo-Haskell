module Transform (transform, mutate, mutateFCall) where

import Types

type Symbol = (String, Int)

--transform :: FProgram -> IProgram
transform fp ip = do

    -- 1. Enumerate function calls
    let fpEnum = fcallEnum fp

    -- 2.

    return fpEnum

-- Enumerate all declared functions' calls
fcallEnum :: FProgram -> FProgram
fcallEnum fp = do

    let fdefs = (snd fp)

    -- 1. Get Function Symbols
    --let symbols = [fsymbols | it <- (scrapSymbols fdefs), fsymbols <- (fst it)]

    -- 2. Traverse and mutate FDefs FOR EACH function symbol in result FExpr
    let resultEnum = (mutate mutateFCall (fst fp))

    -- 3. Also for FExprs in the Fdefinitions
    let fdefsEnum =
            [
                fdefCast
                (
                    (fst3 it),
                    (snd3 it),
                    (mutate mutateFCall (trd3 it))
                )
                | it <- fdefs
            ]

    -- 4. Manual mutation return
    (programCast resultEnum fdefsEnum)

-- scrapSymbols: gets all the Function Definitions and returns
--                  their names and their parameter names
scrapSymbols :: [FDefinition] -> [(String, [String])]
scrapSymbols fp = [((fst3 it), (snd3 it)) | it <- fp]

-- TRAVERSE
mutate :: (FExpr -> FExpr) -> FExpr -> FExpr
mutate function fexpr =
    case fexpr of
    -- Abhore the state

    -- First the simple nodes
    FVar var                    -> (FVar var)
    FNum num                    -> (FNum num)
    FBool bull                  -> (FBool bull)

    -- Then the recursives
    FCall func arg              -> (FCall func (map function arg))

    FParens expr                -> (FParens (function expr))
    FIfThenElse cond aff neg    -> (FIfThenElse (function cond)
                                                (function aff)
                                                (function neg)
                                    )

    -- MUST BE A BETTER WAY
    --FUnaryOp op expr            -> (FUnaryOp op (map mutt expr))
    --FCompOp op expr1 expr2      -> (FCompOp op (map mutt expr1) (mutate expr2))
    FBinaryOp op expr1 expr2    -> (FBinaryOp op (function expr1) (function expr2))
    --FBooleanOp op expr1 expr2   -> (FBooleanOp op (map mutt expr1) (mutate expr2))

-- Wrapper that overrides the functionality of mother mutate for a specific case
mutateFCall :: FExpr -> FExpr
mutateFCall fexpr  = do
    case fexpr of
        FCall func arg -> (FCall (func++("HELLO")) (map (mutate mutateFCall) arg))
        fexpr          -> (mutate mutateFCall fexpr)

{---------------- Helpers ----------------}

fdefCast :: (String, [String], FExpr) -> FDefinition
fdefCast fdef = fdef

programCast :: FExpr -> [FDefinition] -> FProgram
programCast a b = (a, b)

-- type FDefinition = (String, [String], FExpr)
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- type FDefinition = (String, [String], FExpr)
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- type FDefinition = (String, [String], FExpr)
trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c