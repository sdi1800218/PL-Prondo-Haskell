module Transform (
    transform,
    fcallEnum,
    trd3,
    scrapSymbols,
    makeIP,
    getActuals
) where

import Types
import Control.Monad.State -- not used
import Data.Char(isDigit, isLetter)   -- extract{Num, Func}
import Data.Map(fromListWith, toList) -- mergeIDefs()

-- My custom symbol == state type
type Symbol = (String, Int)

{------------------------------ Implementation ------------------------------}

-- TODO:
--      - Change name of Symbol type
--      - Handle Function names that have numbers

-- transform: Transform the given FProgram in the following phases:
--      1. Enumerate function calls, by turning "f" to "f#".
--      2. Constructs the easy part of the IProgram by flat conversion.
--      3. Gets all symbols and their params.
--      4. Extracts actuals for it's above param per function.
transform :: FProgram -> IProgram
transform fp = do

    -- 1. Enumerate function calls
    let fpEnum = fcallEnum fp

    -- 2. Construct initial Intensional Language Tree
    let ip = makeIP fpEnum

    -- 3. Get all symbols from the initial FP
    let symbols = scrapSymbols (snd fp)

    -- 4. Parse Actuals from the enum'ed FP
    let actuals = getActuals fpEnum symbols

    -- 5. Augment IProgram with them
    ip ++ actuals

-- Traverse one FDefinition at the time and scrap args
-- For every FExpr inside an FCall's args, extract the stuff into
getActuals :: [FDefinition] -> [(String, [String])] -> [IDefinition]
getActuals fpEnum []               = []
getActuals fpEnum (symbol:symbols) = do

    {-
    PRELUDE:
      * Every symbol is a function along with it's typical parameters' names
      * We want to create IActual objects
    -}

    -- 1. For every fdef we have,
    --      let's parse the actuals of the current symbol
    let roughActuals = do
        fdef <- fpEnum
        -- Go through every fdef for the current function
        (paramParse (trd3 fdef) symbol)

    -- 2. Then merge the results
    -- ALTERNATIVE: make a recursive function
    --              that computes the structure properly.
    let currActuals = (mergeIDefs roughActuals)

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
        -- a. map the fcalls
        fcall <- fcalls

        -- b. traverse each one
        -- the result is gonna be an array with the args of every function call
        -- i.e [[args0], [args1], [args2]]
        let dirty = traverseFCall func fcall

        -- c. Map the symbols with the indexes of this array
        let zippedUp = map (zip params) dirty

        -- d. Just scrap (param, IExpr) pairs
        let finalOne0 = do
            param <- params
            let actuals = do
                internal <- zippedUp
                [snd elem | elem <- internal, (fst elem) == param]

            -- Cast.
            return (param, (actualsCast actuals))

        -- Cast.
        map idefCast finalOne0

    -- 3. Return and pray.
    actual

-- mergeIDefs: For every symbol reduce all the duplicates into a single array
mergeIDefs :: [(String, IExpr)] -> [(String, IExpr)]
mergeIDefs []      = []
mergeIDefs idefs   = do
    let result = toList (fromListWith (mergeActuals) (map stripIActuals idefs))
    map recoverIActuals result

-- Helper for mergeIDefs().
mergeActuals :: [IExpr] -> [IExpr] -> [IExpr]
mergeActuals actuals1 actuals2 = (actuals2 ++ actuals1)

-- traverseFCall: Returns an array of [args] per function call
--                  This is the most valuable function, albeit ugly.
--                  It traverse an FCall and scraps all it's call arguments.
traverseFCall :: String -> FExpr -> [[IExpr]]
traverseFCall function fexpr =
    case fexpr of
        -- 1. Keep the args of a matched function in an array i.e [Xarg, Yarg]
        --     and then recurse into the args to find more stuff.
        FCall func args -> if (function == (extractFunc func))
                            -- get params
                            then [(map fExprToIexpr args)]
                                ++ concat ((map (traverseFCall function) args))
                            -- dont get, but check children
                            else
                                concat ((map (traverseFCall function) args))

        -- 2. Here we assume that an FCall will have only primitives as nodes.
        FVar var                    -> [[]]
        FNum num                    -> [[]]
        FBool bull                  -> [[]]

        -- 3. Avoid shenanigans from smart [people]
        FParens expr                -> (traverseFCall function expr)
        FIfThenElse cond aff neg    -> (traverseFCall function cond)
                                            ++ (traverseFCall function aff)
                                            ++ (traverseFCall function neg)

        FUnaryOp op expr            -> (traverseFCall function expr)
        FCompOp op expr1 expr2      -> (traverseFCall function expr1)
                                            ++ (traverseFCall function expr2)
        FBinaryOp op expr1 expr2    -> (traverseFCall function expr1)
                                            ++ (traverseFCall function expr2)
        FBooleanOp op expr1 expr2   -> (traverseFCall function expr1)
                                            ++ (traverseFCall function expr2)

-- extractFCall: fetches the highest level FCalls from the FExpr, into a list
extractFCall :: FExpr -> [FExpr]
extractFCall fexpr =
    case fexpr of

        -- Dont scrap now, we'll do later; in parallel bfs style
        FVar var                    -> []
        FNum num                    -> []
        FBool bull                  -> []

        -- Target.
        FCall func args             -> [fexpr]

        FParens expr                -> [] ++ (extractFCall expr)

        FIfThenElse cond aff neg    -> [] ++ (extractFCall cond)
                                          ++ (extractFCall aff)
                                          ++ (extractFCall neg)

        FUnaryOp op expr            -> [] ++ (extractFCall expr)

        FCompOp op expr1 expr2      -> [] ++ (extractFCall expr1)
                                          ++ (extractFCall expr2)

        FBinaryOp op expr1 expr2    -> [] ++ (extractFCall expr1)
                                          ++ (extractFCall expr2)

        FBooleanOp op expr1 expr2   -> [] ++ (extractFCall expr1)
                                          ++ (extractFCall expr2)

-- makeIP: Reconstruct an FProgram to an IProgram
--          Fetch the symbol of fdef and pair it with its converted fexpr.
makeIP :: [FDefinition] -> IProgram
makeIP fdefs = do
    fdef <- fdefs

    let fname = (fst3 fdef);
    let fexpr = (trd3 fdef);

    return (fname, (fExprToIexpr fexpr))

-- fExprToIexpr: faulsely camel cased function that turns FExpr to IExpr.
--              Special handling for ICall. Recursive hell.
fExprToIexpr :: FExpr -> IExpr
fExprToIexpr fexpr =
    case fexpr of

        -- 1. First the leaf expressions
        FVar var                    -> (IVar var)
        FNum num                    -> (INum num)
        FBool bull                  -> (IBool bull)

        -- 2. The precious.
        FCall func arg -> (ICall (read (extractNum func) :: Int)
                                (extractFunc func)
                            )

        -- 3. Then the recursive ones
        FParens expr                -> (IParens (fExprToIexpr expr))
        FIfThenElse cond aff neg    -> (IIfThenElse (fExprToIexpr cond)
                                                (fExprToIexpr aff)
                                                (fExprToIexpr neg)
                                        )

        FUnaryOp op expr            -> (IUnaryOp op (fExprToIexpr expr))
        FCompOp op expr1 expr2      -> (ICompOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2)
                                        )

        FBinaryOp op expr1 expr2    -> (IBinaryOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2)
                                        )

        FBooleanOp op expr1 expr2   -> (IBooleanOp op (fExprToIexpr expr1)
                                                    (fExprToIexpr expr2)
                                        )

-- fcallEnum: Transforms the FProgram into a linear of [FDefinition],
--              then it enumerates per function and params present,
--              and returns the result.
fcallEnum :: FProgram -> [FDefinition]
fcallEnum fp = do

    -- 1. Save the FDefinitions somewhere special
    let fdefs = (snd fp)

    -- 2. Transform result to have homogenuity
    let fdefResult = ("result", [""], (fst fp))

    -- 3. Get Function Symbols
    let symbols = [(fst it) | it <- (scrapSymbols fdefs)]

    -- 4. For every symbol (function) we want to enumerate all its occurences
    --      inside the TOTAL of fdefinitions
    let totalFdefs = fdefResult:fdefs

    let muttTotalFdefs = (enumerate totalFdefs symbols)

    -- 5. Return the mutation.
    muttTotalFdefs

-- enumerate: is a wrapper of enum, actually handleArrayFExpr.
--              It must pass the new state to the next computation.
enumerate :: [FDefinition] -> [String] -> [FDefinition]
enumerate totalFdefs []               = totalFdefs
enumerate totalFdefs (symbol:symbols) = do

    -- 1. Make an array of FExpr
    let totalFExpr = [(trd3 fdef) | fdef <- totalFdefs]

    -- 2. Enumerate them
    let (muttFExprs, state) = (handleArrayFExpr totalFExpr (symbol, 0))

    -- 3. Reconstruct FDefinitions
    let len = (length muttFExprs) - 1
    -- functional? what is functional?
    let reconstructFDefs = [(uno, dos, tres) | it <- [0 .. len],
                        let uno = (fst3 (totalFdefs !! it)),
                        let dos = (snd3 (totalFdefs !! it)),
                        let tres = (muttFExprs !! it)]

    -- 4. Recurse with feedback
    (enumerate reconstructFDefs symbols)

-- handleArrayFExpr: Helper for enum() and enumerate().
-- Handles the passing of state between an array of FExpr,
-- that get processed sequentially.
handleArrayFExpr :: [FExpr] -> Symbol -> ([FExpr], Symbol)
handleArrayFExpr [] finalState               = ([], finalState)
handleArrayFExpr (fexpr:fexprs) initialState = do

        -- 1. Run current
        let (muttfexpr, newState) = enum (fexpr, initialState)

        -- 2. Then move to the rest of the list
        ---- a. recurse and save state
        let (recurseList, newerState) = (handleArrayFExpr fexprs newState)
        ---- b. append new fexprs
        let actualArgs = [muttfexpr] ++ recurseList

        -- 3. Ret-a-pair
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
    -- 1. Fetch our symbol.
    let funcSymbol = (fst state)

    -- 2. Let's pattern match and reconstruct the tree recursively
    case fexpr of

        -- A. First the leaf nodes
        FVar var    -> ((FVar var), state)
        FNum num    -> ((FNum num), state)
        FBool bull  -> ((FBool bull), state)

        -- B. Then the precious one
        FCall func args  ->
            {- Here begins the beauty of the functional paradigm -}
            if (func == funcSymbol)
                then do
                    let count = (snd state);

                    -- a. Handle func mutation
                    let muttFunc = func++(show count);
                    let newCount = count + 1;
                    let newState = (funcSymbol, newCount)

                    -- b. Recurse into the args and save final state.
                    let (actualArgs, final) = (handleArrayFExpr args newState)

                    -- c. Reconstruct FCall
                    ((FCall muttFunc actualArgs), final)

                else do
                    -- a. Search the children for matches and save final state.
                    let (actualArgs, final) = (handleArrayFExpr args state)

                    -- b. Reconstruct FCall
                    ((FCall func actualArgs), final)

        -- C. Then the normal ones but with the special mutation lingo
        FParens expr             ->
            do
                let exp = (enum (expr, state))
                ((FParens (fst exp)), (snd exp))

        FIfThenElse cond aff neg ->
            do
                -- A. Relay work
                let (left,middle,right,newState) = android3(cond,aff,neg,state)

                -- B. Reconstruct FExpr and combine with the state
                ((FIfThenElse (left) (middle) (right)), newState)

        FUnaryOp op expr         ->
            do
                -- A. Relay work
                let exp = (enum (expr, state))

                -- B. Reconstruct FExpr and combine with the state
                ((FUnaryOp op (fst exp)), (snd exp))

        FCompOp op expr1 expr2   ->
            do
                -- A. Relay work
                let (left, right, newState) = android (expr1, expr2, state)

                -- B. Reconstruct FExpr and combine with the state
                ((FCompOp op left right), newState)

        FBinaryOp op expr1 expr2 ->
            do
                -- A. Relay work
                let (left, right, newState) = android (expr1, expr2, state)

                -- B. Reconstruct FExpr and combine with the state
                ((FBinaryOp op left right), newState)

        FBooleanOp op expr1 expr2 ->
            do
                -- A. Relay work
                let (left, right, newState) = android (expr1, expr2, state)

                -- B. Reconstruct FExpr and combine with the state
                ((FBooleanOp op left right), newState)

-- android3: encapsulates state altercations that would need to be repeated
android3 :: (FExpr, FExpr,FExpr, Symbol) -> (FExpr, FExpr, FExpr, Symbol)
android3 (expr1, expr2, expr3, state) = do

    -- A. Compute the left fexpr
    let left = (enum (expr1, state));
    let leftFExpr = (fst left)

    -- B. Recompute state
    let newState = (snd left);

    -- C. Now compute the middle with the altered state
    let middle = (enum (expr2, newState));
    let middleFExpr = (fst middle)

    -- D. Recompute or what
    let newerState = (snd middle)

    -- E. Now compute the right
    let right = (enum (expr3, newerState));
    let rightFExpr = (fst right)

    -- F. No state, no party
    let newestState = (snd right)

    -- H. Ret-a-quaple
    (leftFExpr, middleFExpr, rightFExpr, newestState)

-- android: encapsulates state altercations that would need to be repeated
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

        -- Then the special
        FCall func arg              -> [[fexpr]] ++
                                        (concat [flattenFExpr it | it <- arg])

        -- Then the recursives
        FParens expr                -> flattenFExpr expr
        FIfThenElse cond aff neg    -> [fexpr : [cond] ++ [aff] ++ [neg]]
                                        ++ (flattenFExpr cond)
                                        ++ (flattenFExpr aff)
                                        ++ (flattenFExpr neg)

        FUnaryOp op expr            -> [fexpr : [expr]]
                                        ++ (flattenFExpr expr)

        FCompOp op expr1 expr2      -> [fexpr :  [expr1] ++ [expr2]]
                                        ++ (flattenFExpr expr1)
                                        ++ (flattenFExpr expr2)

        FBinaryOp op expr1 expr2    -> [[fexpr]]
                                        ++ (flattenFExpr expr1)
                                        ++ (flattenFExpr expr2)

        FBooleanOp op expr1 expr2   -> [[fexpr]]
                                        ++ (flattenFExpr expr1)
                                        ++ (flattenFExpr expr2)

{-------------------------------- Helpers --------------------------------}
-- Mazepse an IExpr == IActuals [IExpr] object
recoverIActuals :: (String, [IExpr]) -> (String, IExpr)
recoverIActuals (var, actuals) = (var, (IActuals actuals))

-- Expand an IExpr == IActuals [IExpr] object
stripIActuals :: (String, IExpr) -> (String, [IExpr])
stripIActuals (var, actuals) = case actuals of
    IActuals expr -> (var, expr)

-- Simple extractor that uses isDigit. Doesn't handle very special cases,
-- such as functions named like "f1337"
extractNum :: String -> String
extractNum enumFunc = [num | num <- enumFunc, isDigit num]

-- Same ^^
extractFunc :: String -> String
extractFunc enumFunc = [func | func <- enumFunc, isLetter func]

-- Casts
fdefCast :: (String, [String], FExpr) -> FDefinition
fdefCast fdef = fdef

idefCast :: (String, IExpr) -> IDefinition
idefCast idef = idef

actualsCast :: [IExpr] -> IExpr
actualsCast exprs = (IActuals exprs)

programCast :: FExpr -> [FDefinition] -> FProgram
programCast a b = (a, b)

-- End of Casts

-- type FDefinition = (String, [String], FExpr)
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- type FDefinition = (String, [String], FExpr)
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- type FDefinition = (String, [String], FExpr)
trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c


{-------------------------------- Vault --------------------------------}

{- THINGS THAT NEVER WORKED

-- TODO
extractPrimitives :: FExpr -> [IExpr]
extractPrimitives fexpr =
    case fexpr of

    -- First the simple nodes
        FVar var                    -> [fExprToIexpr fexpr]
        FNum num                    -> [fExprToIexpr fexpr]
        FBool bull                  -> [fExprToIexpr fexpr]

    -- Then the recursives
        FCall func args              -> [fExprToIexpr fexpr]

        FParens expr                -> [] ++ (extractPrimitives expr)
        FIfThenElse cond aff neg    -> [] ++ (extractPrimitives cond)
                                          ++ (extractPrimitives aff)
                                          ++ (extractPrimitives neg)

        FUnaryOp op expr            -> [] ++ (extractPrimitives expr)
        FCompOp op expr1 expr2      -> [] ++ (extractPrimitives expr1)
                                            ++ (extractPrimitives expr2)
        FBinaryOp op expr1 expr2    -> [] ++ (extractPrimitives expr1)
                                            ++ (extractPrimitives expr2)
        FBooleanOp op expr1 expr2   -> [] ++ (extractPrimitives expr1)
                                            ++ (extractPrimitives expr2)


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