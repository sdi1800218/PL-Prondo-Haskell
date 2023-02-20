module Transform (transform, fcallEnum, fst3, snd3, trd3,
scrapSymbols, makeIP, fExprToIexpr, extractFCall, traverseFCall,
getActuals, paramParse) where

import Types
import Control.Monad.State
import Data.Char

type Symbol = (String, Int)

{-
instance Functor FExpr where
    fmap f (FVar var)   = f (FVar Var)
    fmap f (FNum num)   = f (FNum num)
    fmap f (FBool bull) = f (FBool bull)
    --fmap f () = Node (f x) (fmap f leftsub) (fmap f rightsub)
-}

-- TODO
transform :: FProgram -> IProgram
transform fp = do

    -- 1. Enumerate function calls
    let fpEnum = fcallEnum fp

    -- Let's suppose the above works
    -- 2. Construct initial Intensional Language Tree
    let ip = makeIP fpEnum

    -- 3. Get all symbols from the initial FP
    let symbols = scrapSymbols (snd fp)

    -- 4. Parse Actuals from the enum'ed FP
    let actuals = getActuals fpEnum symbols

    -- 5. Augment IProgram with them
    ip:actuals

-- Traverse one FDefinition at the time and scrap args
-- For every FExpr inside an FCall's args, extract the stuff into
getActuals :: [FDefinition] -> [(String, [String])] -> [IDefinition]
getActuals fpEnum []               = []
getActuals fpEnum (symbol:symbols) = do

    -- PRELUDE: * Every symbol is a function along with it's typical parameters' names
    --          * We want to create IActual objects

    -- 1. For every fdef we have, let's parse the actuals of the current symbol
    let roughActuals = do
        fdef <- fpEnum
        -- a. go through every fdef for the current function
        (paramParse (trd3 fdef) symbol)

    -- 2. Then merge the results
    -- ALTERNATIVE: make a recursive function that computes the structure properly.
    let currActuals = (mergeActuals roughActuals)

    -- 3. Recursively find the rest of the actuals for the other functions
    currActuals ++ (getActuals fpEnum symbols)


-- Given an FExpr and a Function along with it's typical parameters,
-- produce a list of [IExpr] per function parameter
paramParse :: FExpr -> (String, [String]) -> [IDefinition]
paramParse fexpr (func, params) = do
    -- 1. Get me all FCalls of the highest level
    let fcalls = extractFCall fexpr

    -- 2. So now that we have the fcalls of the expression,
    --      we want to recurse into them in order.
    --      Like: get all params from 1st, then from 2nd, etc..
    let actual = do
        fcall <- fcalls
        

-- For every symbol reduce all the duplicates into a single array
mergeActuals :: [IDefinition] -> [IDefinition]
mergeActuals [] = []
mergeActuals list = list
--TODO

-- Returns an array of IExpr per function call, in proper order
traverseFCall :: String -> FExpr -> [[IExpr]]
traverseFCall function fexpr =
    case fexpr of
        -- 1. If we match another FCall we save it's args
        -- and then recurse deeper into them to find possibly more FCalls
        FCall func args -> if (function == (extractFunc func))
                            then [(fExprToIexpr fexpr)]--(map fExprToIexpr args)
                                -- ++ (concat [traverseFCall arg | arg <- args ])
                            else
                                (concat [traverseFCall arg | arg <- args ])
        -- 2. Here we assume that an FCall will have only primitives as nodes.
        fexpr           -> [fExprToIexpr fexpr]

-- extractFCall: TODO
extractFCall :: FExpr -> [FExpr]
extractFCall fexpr =
    case fexpr of

    -- First the simple nodes
        FVar var                    -> []
        FNum num                    -> []
        FBool bull                  -> []

    -- Then the recursives
        FCall func args              -> args

        FParens expr                -> [] ++ (extractFCall expr)
        FIfThenElse cond aff neg    -> [] ++ (extractFCall cond)
                                          ++ (extractFCall aff)
                                          ++ (extractFCall neg)

        FUnaryOp op expr            -> [] ++ (extractFCall expr)
        FCompOp op expr1 expr2      -> [] ++ (extractFCall expr1) ++ (extractFCall expr2)
        FBinaryOp op expr1 expr2    -> [] ++ (extractFCall expr1) ++ (extractFCall expr2)
        FBooleanOp op expr1 expr2   -> [] ++ (extractFCall expr1) ++ (extractFCall expr2)

-- makeIP: Reconstruct an FProgram to an IProgram
makeIP :: [FDefinition] -> IProgram
makeIP fdefs = do
    fdef <- fdefs
    let fname = (fst3 fdef);
    let fexpr = (trd3 fdef);
    return (fname, (fExprToIexpr fexpr))

-- fExprToIexpr: faulty camel cased function that turns FExpr to IExpr
--                  special handling for ICall
fExprToIexpr :: FExpr -> IExpr
fExprToIexpr fexpr =
    case fexpr of

        -- 1. First the leaf expressions
        FVar var                    -> (IVar var)
        FNum num                    -> (INum num)
        FBool bull                  -> (IBool bull)

        -- 2. Then the recursive ones
        FCall func arg              -> (ICall (read (extractNum func) :: Int) (extractFunc func))

        FParens expr                -> (IParens (fExprToIexpr expr))
        FIfThenElse cond aff neg    -> (IIfThenElse (fExprToIexpr cond)
                                                (fExprToIexpr aff)
                                                (fExprToIexpr neg)
                                        )

        FUnaryOp op expr            -> (IUnaryOp op (fExprToIexpr expr))
        FCompOp op expr1 expr2      -> (ICompOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2))

        FBinaryOp op expr1 expr2    -> (IBinaryOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2))

        FBooleanOp op expr1 expr2   -> (IBooleanOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2))

-- Simple extractor that uses isDigit. Doesn't handle very special cases,
-- such as function named like "f1337"
extractNum :: String -> String
extractNum enumFunc = [num | num <- enumFunc, isDigit num]

extractFunc :: String -> String
extractFunc enumFunc = [func | func <- enumFunc, isLetter func]

-- Enumerate all declared functions' calls
fcallEnum :: FProgram -> [FDefinition]
fcallEnum fp = do

    -- 1. Save the FDefinitions somewhere special
    let fdefs = (snd fp)

    -- 2. Transform result to have homogenuity
    let fdefResult = ("result", [""], (fst fp))

    -- 3. Get Function Symbols
    let symbols = [(fst it) | it <- (scrapSymbols fdefs)]

    {- DEFUNKT
    -- 3. Flatten all FExpr in the FDefinitions
    let flatFdefs = [( (fst3 it), (snd3 it), (flattenFExpr $ trd3 it) ) | it <- fdefs]

    -- 4. Connect them and feed them to the enumerator
    let fpEnum = (enumerator (flatResult:flatFdefs) symbols)
    -}

    -- 4. For every symbol (function) we want to enumerate all its occurences
    --      inside the TOTAL of fdefinitions
    let totalFdefs = fdefResult:fdefs

    let muttTotalFdefs = (enumerate totalFdefs symbols)

    muttTotalFdefs

-- enumerate is a wrapper of enum
enumerate :: [FDefinition] -> [String] -> [FDefinition]
enumerate totalFdefs []               = totalFdefs
enumerate totalFdefs (symbol:symbols) = do

    -- 1. Make an array of FExpr
    let totalFExpr = [(trd3 fdef) | fdef <- totalFdefs]

    -- 2. Enumerate them
    let (muttFExprs, state) = (handleArrayFExpr totalFExpr (symbol, 0))

    -- 3. Reconstruct FDefinitions
    let len = (length muttFExprs) - 1 -- functional? what is functional?
    let reconstructFDefs = [(uno, dos, tres) | it <- [0 .. len],
                        let uno = (fst3 (totalFdefs !! it)),
                        let dos = (snd3 (totalFdefs !! it)),
                        let tres = (muttFExprs !! it)]

    -- 4. Recurse with feedback
    (enumerate reconstructFDefs symbols)

-- Helper for enum and enumerate that handles the passing of state between of
-- an array of FExpr that get processed sequentially.
handleArrayFExpr :: [FExpr] -> Symbol -> ([FExpr], Symbol)
handleArrayFExpr [] finalState                       = ([], finalState)
handleArrayFExpr (fexpr:fexprs) initialState    = do


        -- WE WANT
        -- TO RUN EACH FExpr AND THEN RUN THE NEXT WITH ITS NEW STATE

        -- 1. Run current
        let (muttfexpr, newState) = enum (fexpr, initialState)

        -- 2. Then move to the rest of the list
        ---- a. save state
        let (recurseList, newerState) = (handleArrayFExpr fexprs newState)
        let actualArgs = [muttfexpr] ++ recurseList

        (actualArgs, newerState)

-- scrapSymbols: gets all the Function Definitions and returns
--                  their names and their parameter names
scrapSymbols :: [FDefinition] -> [(String, [String])]
scrapSymbols fp = [((fst3 it), (snd3 it)) | it <- fp]

-- enum: is the ugliest function ever written; it's a disgusting fusion
--          of bad ideas and bad code. It is stateful, and stateless.
--          It iterates, it mutates and should not be modified because
--          nothing will work afterwards. It reminds of C code with
--          embedded assembly.
-- enum: Traverses the AST and annotates the symbol given with a number
enum :: (FExpr, Symbol) -> (FExpr, Symbol)
enum (fexpr, state) = do
    -- Hah
    let funcSymbol = (fst state)

    -- Let's pattern match and reconstruct the tree recursively
    case fexpr of

        -- 1. First the leaf nodes
        FVar var                    -> ((FVar var), state)
        FNum num                    -> ((FNum num), state)
        FBool bull                  -> ((FBool bull), state)

        -- 2. Then the hard one
        FCall func args  ->  if (func == funcSymbol)
                                then do
                                    -- Here begins the beautiful functional paradigm
                                    let count = (snd state);

                                    -- 1. Handle func mutation
                                    let muttFunc = func++(show count);
                                    let newCount = count + 1;
                                    let newState = (funcSymbol, newCount)

                                    -- 2. Recurse into the args
                                    let (actualArgs, newerState) = (handleArrayFExpr args newState)

                                    -- 3. Reconstruct
                                    ((FCall muttFunc actualArgs), newerState)
                                else do
                                    -- maybe this doesn't match, but its children..
                                    let (actualArgs, newerState) = (handleArrayFExpr args state)
                                    ((FCall func actualArgs), newerState)

        -- 3. Then the normal ones but with the special mutation lingo
        FParens expr            -> do
                                let exp = (enum (expr, state))
                                ((FParens (fst exp)), (snd exp))

        -- TODO
        {-FIfThenElse cond aff neg    -> (FIfThenElse (enum cond)
                                                    (enum aff)
                                                    (enum neg)
                                        )
        -}

        -- FUnaryOp op expr         -> (FUnaryOp op (map mutt expr))
        FCompOp op expr1 expr2   -> do
                                -- A. Relay work
                                let (left, right, newState) = android (expr1, expr2, state)

                                -- B. Reconstruct FExpr and combine with the state
                                ((FCompOp op left right), newState)

        FBinaryOp op expr1 expr2 -> do
                                -- A. Relay work
                                let (left, right, newState) = android (expr1, expr2, state)

                                -- B. Reconstruct FExpr and combine with the state
                                ((FBinaryOp op left right), newState)

        FBooleanOp op expr1 expr2 -> do
                                -- A. Relay work
                                let (left, right, newState) = android (expr1, expr2, state)

                                -- B. Reconstruct FExpr and combine with the state
                                ((FBooleanOp op left right), newState)

-- android: encapsulates stuff that would need to be repeated
android :: (FExpr, FExpr, Symbol) -> (FExpr, FExpr, Symbol)
android (expr1, expr2, state) = do

    -- A. Compute the left fexpr
    let left = (enum (expr1, state));
    let leftFExpr = (fst left)

    -- B. Recompute state
    let newState = (snd left);

    -- C. Now compute the right with the altered state
    let right = (enum (expr2, newState));
    let rightFExpr = (fst right)
    let newerState = (snd right)

    -- D. Ret-a-triple
    (leftFExpr, rightFExpr, newerState)

-- Flatten an FExpr from left to right, top to bottom into a list (inorder)
flattenFExpr :: FExpr -> [[FExpr]]
flattenFExpr fexpr = do
    case fexpr of
        -- First the simple nodes
        FVar var                    -> [[fexpr]]
        FNum num                    -> [[fexpr]]
        FBool bull                  -> [[fexpr]]

        -- Then the recursives
        FCall func arg              -> [[fexpr]] ++ (concat [flattenFExpr it | it <- arg])

        FParens expr                -> flattenFExpr expr
        FIfThenElse cond aff neg    -> [fexpr : [cond] ++ [aff] ++ [neg]]
                                                ++ (flattenFExpr cond)
                                                ++ (flattenFExpr aff)
                                                ++ (flattenFExpr neg)

        -- MUST BE A BETTER WAY
        FUnaryOp op expr            -> [fexpr : [expr]] ++ (flattenFExpr expr)
        FCompOp op expr1 expr2      -> [fexpr :  [expr1] ++ [expr2]] ++
                                        (flattenFExpr expr1) ++ (flattenFExpr expr2)

        FBinaryOp op expr1 expr2    -> [[fexpr]] ++
                                        (flattenFExpr expr1) ++ (flattenFExpr expr2)

        FBooleanOp op expr1 expr2   -> [[fexpr]] ++
                                        (flattenFExpr expr1) ++ (flattenFExpr expr2)


{- THINGS THAT NEVER WORKED

-- Just go through it
traverseAST :: FExpr -> FExpr
traverseAST fexpr =
    case fexpr of

    -- First the simple nodes
        FVar var                    -> (FVar var)
        FNum num                    -> (FNum num)
        FBool bull                  -> (FBool bull)

    -- Then the recursives
        FCall func arg              -> (FCall func (map traverseAST arg))

        FParens expr                -> (FParens (traverseAST expr))
        FIfThenElse cond aff neg    -> (FIfThenElse (traverseAST cond)
                                                (traverseAST aff)
                                                (traverseAST neg)
                                    )

    -- MUST BE A BETTER WAY
        --FUnaryOp op expr            -> (FUnaryOp op (map mutt expr))
        --FCompOp op expr1 expr2      -> (FCompOp op (map mutt expr1) (mutate expr2))
        FBinaryOp op expr1 expr2    -> (FBinaryOp op (traverseAST expr1) (traverseAST expr2))
        --FBooleanOp op expr1 expr2   -> (FBoolean

type FlatFDefinition = (String, [String], [FExpr])

flatfdefCast :: String -> [String] -> [FExpr] -> FlatFDefinition
flatfdefCast a b c = (a, b, c)

flatfdefCast2 :: (String, [String], [FExpr]) -> FlatFDefinition
flatfdefCast2 flatfdef = flatfdef

-- enumerator TODO
enumerator :: [FlatFDefinition] -> [String] -> [FlatFDefinition]
enumerator flatdefs []          = flatdefs
enumerator flatdefs (s:symbols) = do

    -- 1. Reconstruct the flatFdefinitions, mutating only the 3rd element
    let count = 0 -- TODO
    -- First Strike
    let newFdefs = do
        -- for each flat fdef
        fdef <- flatdefs

        -- Second Strike
        let tres = do

            -- for each fexpr in the flat fdef
            fdefExpr <- (trd3 fdef)
            count    <- [0,1..]
            -- Match the function symbol we are currently enumerating
            return $ if (match fdefExpr s)
                then (mutateFCall fdefExpr count)
                else fdefExpr

        return (flatfdefCast (fst3 fdef) (snd3 fdef) tres)
        {-
        [((fst3 fdef), (snd3 fdef),
                    [newfdef | fdefExpr <- (trd3 fdef),
                        if (match fdefExpr s) -- if fcall with curr symbol
                        then (mutateFCall fdefExpr (head count))
                        else fdefExpr
                    ]
                    )
                    | fdef <- flatFdefs]
        -}
    -- 3. Recurse to next symbol
    enumerator newFdefs symbols

-- Wrapper that overrides the functionality of mother mutate for a specific case
mutateFCall :: FExpr -> Int -> FExpr
mutateFCall fexpr num =
    case fexpr of
        FCall func arg -> (FCall (func++(show num)) arg)

-- Check if the given fexpr is an FCall with designated function symbol
match :: FExpr -> String -> Bool
match fexpr symbol =
    case fexpr of
        FCall func arg -> if func == symbol then True
                                            else False
        fexpr            -> False

-- TRAVERSE
traverse :: (FExpr -> FExpr) -> FExpr -> FExpr
traverse fun fexpr =
    case fexpr of

    -- First the simple nodes
    FVar var                    -> fun (FVar var)
    FNum num                    -> fun (FNum num)
    FBool bull                  -> fun (FBool bull)

    -- Then the recursives
    FCall func arg              -> fun (FCall (map fun arg))

    FParens expr                -> fun (FParens (fun expr))
    FIfThenElse cond aff neg    -> fun (FIfThenElse (fun cond) (fun aff) (fun neg))

    FUnaryOp op expr            -> fun (FUnaryOp op (fun expr))
    FCompOp op expr1 expr2      -> fun (FCompOp op expr1 expr2)
    FBinaryOp op expr1 expr2    -> fun (FBinaryOp op expr1 expr2)
    FBooleanOp op expr1 expr2   -> fun (FBooleanOp op expr1 expr2)


iter :: State Int Int -> Int Int
iter = do
    i <- get
    modify (+1)
    return i

plusOne :: Int -> Int
plusOne n = execState iter n
-}


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