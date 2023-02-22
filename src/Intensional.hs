module Intensional (eval, evaluate1) where

import Types
import Debug.Trace

{------------------------------ Implementation ------------------------------}

-- eval: is a wrapper around evaluate1().
--      it feeds parameters properly into evaluate1()
--      to calculate the result of our program.
eval :: IProgram -> IExpr
eval ip = do
    -- 1. Map "result" expression
    let resultVar = snd (head ip)

    -- 2. Run it
    let resultEval = (evaluate1 resultVar ip [])

    -- 3. Check the result type and return
    case resultEval of
        Left num    -> (INum num)
        Right bull  -> (IBool bull)

        -- error case, return jibberish
        -- resultEval  -> (IActuals [])

-- evaluate1: Applies semantics and semntic rules to the IProgram
--            to recursively simplify expressions and derive primitives,
--            on which it operates.
evaluate1 :: IExpr -> [IDefinition] -> IEnv -> (Either Int Bool)
evaluate1 iexpr idefs env = do
    let x = trace("Call :: evaluate" ++ show iexpr) -- not used
    case iexpr of
        -- Primitives
        INum    num     -> Left num
        IBool   bull    -> Right bull

        {-
        We got no assignments,
        so a var can only be a typical parameter (aka idef -> IActuals entry)
        -}
        IVar    var     -> do
                            let switch = (lookMeUp var idefs)
                            (evaluate1 switch idefs env)

        -- Works according to semantics provided
        IActuals    iexprs       -> do
                                    let index = (head env)
                                    let newEnv = (drop 1 env)
                                    let selection = (iexprs!!index)
                                    (evaluate1 selection idefs newEnv)

        -- Lookup the symbol and augment the env, also due to semantics
        ICall       num symbol   -> do
                                    let switch = (lookMeUp symbol idefs)
                                    (evaluate1 switch idefs (num:env))


        {-
            This is a tricky one.
            First calculate the expression of the condition,
            then choose which expression to calculate.
            This way we save ourselves from dragons.
        -}
        IIfThenElse cond aff neg ->
            do
                -- cond1 :: Boolean
                let cond1   = (evaluate1 cond idefs env)
                let strip1  = case cond1 of
                                Left num    -> error (runtime 1)
                                Right bool  -> bool

                -- Check our cond and calc only the needed one
                if strip1
                    -- aff :: either
                    then (evaluate1 aff idefs env)
                    -- neg :: either
                    else (evaluate1 neg idefs env)

        -- Simply skip parens
        IParens expr        -> (evaluate1 expr idefs env)

        -- Another tricky one
        IUnaryOp op expr    ->
            do
                -- Eval the result
                let res = (evaluate1 expr idefs env)

                -- Then cast accordingly
                case op of
                    -- NOTE: For an Int to do Positive it's like the Id trans,
                    -- since: +(-2) == -2, +(2) == 2.
                    -- Int
                    Positive ->
                        case res of
                            Left num    -> Left num
                            Right bool  -> Right (error (runtime 2))
                    -- Int
                    Negative -> case res of
                                    Left num    -> Left (negate num)
                                    Right bool  -> Right (error (runtime 3))
                    -- Bool
                    Not      -> case res of
                                    Left num    -> Left (error (runtime 4))
                                    Right bool  -> Right (not bool)

        -- Computes expressions and then combines them according to the Op
        -- It's ugly, because it must work for different stuff.
        -- Eq and Neq are for both types, the rest only for Ints.
        ICompOp op expr1 expr2      ->
            do
                {- expr1 :: Either, expr2 :: Either -}
                let res1 = (evaluate1 expr1 idefs env)
                let res2 = (evaluate1 expr2 idefs env)

                case res1 of
                    Left num1    ->
                        case res2 of
                            -- Different types of operands.
                            Right bool2 -> (error (runtime 9))

                            Left num2   ->
                                -- We matched 2 Ints, all applicable.
                                case op of
                                    LtEq    -> Right (num1 <= num2)
                                    Lt      -> Right (num1 < num2)
                                    GtEq    -> Right (num1 >= num2)
                                    Gt      -> Right (num1 > num2)
                                    Eq      -> Right (num1 == num2)
                                    Neq     -> Right (num1 /= num2)

                    Right bool1  ->
                        case res2 of
                            -- Different types of operands.
                            Left num2  -> (error (runtime 9))

                            Right bool2 ->
                                -- We matched 2 Booleans, only Eq and Neq.
                                case op of
                                    Eq  -> Right (bool1 == bool2)
                                    Neq -> Right (bool1 /= bool2)

        -- Computes expressions and then combines them according to the Op
        IBinaryOp op expr1 expr2    ->
            do
                -- expr1 :: Int
                let res1 = (evaluate1 expr1 idefs env)
                let strip1  = case res1 of
                                    Left num    -> num
                                    Right bool  -> (error (runtime 6))
                -- expr2 :: Int
                let res2 = (evaluate1 expr2 idefs env)
                let strip2  = case res2 of
                                    Left num    -> num
                                    Right bool  -> (error (runtime 6))
                case op of
                        Plus    -> Left (strip1 + strip2)
                        Mult    -> Left (strip1 * strip2)
                        Minus   -> Left (strip1 - strip2)
                        Div     -> Left (handleDivision strip1 strip2)

        -- Computes expressions and then combines them according to the Op
        IBooleanOp op expr1 expr2   ->
            do
                -- expr1 :: Boolean
                let res1    = (evaluate1 expr1 idefs env)
                let strip1  = case res1 of
                                    Left num    -> (error (runtime 7))
                                    Right bool  -> bool
                -- expr2 :: Boolean
                let res2    = (evaluate1 expr2 idefs env)
                let strip2  = case res2 of
                                    Left num    -> (error (runtime 7))
                                    Right bool  -> bool
                case op of
                        And     -> Right (strip1 && strip2)
                        Or      -> Right (strip1 || strip2)

-- lookMeUp: Fetches symbol IExprs from the IDefs
lookMeUp :: String -> [IDefinition] -> IExpr
-- if it dont exist, we got problems.
lookMeUp symbol (idef:idefs) | symbol == (fst idef)   = (snd idef)
                             | otherwise              = (lookMeUp symbol idefs)

-- handleDivision: Throws exception if div by zero, floors down the result.
handleDivision :: Int -> Int -> Int
handleDivision a 0 = error (runtime 8)
handleDivision a b = (div a b)

-- Runtime Error Directives
runtime :: Int -> String
runtime num = do
    let prefix = "\n> Runtime Error: "
    case num of
        1 -> (prefix ++ "If Condition not Boolean")
        2 -> (prefix ++ "UnaryOp Positive with Boolean")
        3 -> (prefix ++ "UnaryOp Negative with Boolean")
        4 -> (prefix ++ "UnaryOp Not with Integer")
        5 -> (prefix ++ "CompOp Operand not Integer")
        6 -> (prefix ++ "BinaryOp Operand not Integer")
        7 -> (prefix ++ "BooleanOp Operand not Boolean")
        8 -> (prefix ++ "Division by zero")
        9 -> (prefix ++ "UnaryOp Operand Type Mismatch")

{------------------------------ Vault ------------------------------}

{- Initial implementation that first simplified expressions,
    and then calculated the expression of primitives.
   Failed because of recursive calls, such as fib_fact input.

-- Doc
calculate :: IExpr -> (Either Int Bool)
calculate simpleIP =
    case simpleIP of

        -- Them leaves
        INum num    -> Left num
        IBool bull  -> Right bull

        IParens expr    -> (calculate expr)

        -- TODO
        -- IUnaryOp
        -- IIfThenElse

        -- Works on Ints
        IBinaryOp op expr1 expr2    -> do
                                        let res1    = (calculate expr1)
                                        let strip1  = case res1 of
                                                        Left num    -> num
                                                        Right bool  -> error "oops"
                                        let res2    = (calculate expr2)
                                        let strip2  = case res2 of
                                                        Left num    -> num
                                                        Right bool  -> error "oops"
                                        case op of
                                            Plus    -> Left (strip1 + strip2)
                                            Mult    -> Left (strip1 * strip2)
                                            Minus   -> Left (strip1 - strip2)
                                            -- TODO: Div     -> Left (handleDivision strip1 strip2)

        -- Works on Booleans
        IBooleanOp op expr1 expr2   -> do
                                        let res1 = (calculate expr1)
                                        let strip1  = case res1 of
                                                        Left num    -> error "oops"
                                                        Right bool  -> bool
                                        let res2 = (calculate expr2)
                                        let strip2  = case res2 of
                                                        Left num    -> error "oops"
                                                        Right bool  -> bool
                                        case op of
                                            And     -> Right (strip1 && strip2)
                                            Or      -> Right (strip1 || strip2)

        -- Works on Ints, but returns Boolean
        ICompOp op expr1 expr2      -> do
                                        let res1 = (calculate expr1)
                                        let strip1  = case res1 of
                                                        Left num    -> num
                                                        Right bool  -> error "oops"
                                        let res2 = (calculate expr2)
                                        let strip2  = case res2 of
                                                        Left num    -> num
                                                        Right bool  -> error "oops"
                                        case op of
                                            LtEq     -> Right (strip1 <= strip2)
                                            Lt       -> Right (strip1 < strip2)
                                            GtEq     -> Right (strip1 >= strip2)
                                            Gt       -> Right (strip1 > strip2)
                                            Eq       -> Right (strip1 == strip2)
                                            Neq      -> Right (strip1 /= strip2)


-- TODO: Doc
evaluate :: IExpr -> [IDefinition] -> IEnv -> IExpr
evaluate iexpr idefs env = do
    let x = trace("Call :: evaluate" ++ show iexpr)
    case iexpr of
        INum    num     -> iexpr
        IBool   bull    -> iexpr
        -- We got no assignments,
        -- so a var can only be a typical parameter (aka idef -> IActuals entry)
        IVar    var     -> do
                            let switch = (lookMeUp var idefs)
                            (evaluate switch idefs env)

        IActuals    iexprs       -> do
                                    let index = (head env)
                                    let newEnv = (drop 1 env)
                                    let selection = (iexprs!!index)
                                    (evaluate selection idefs newEnv)

        ICall       num symbol   -> do -- Lookup the symbol and augment the env
                                    let switch = (lookMeUp symbol idefs)
                                    (evaluate switch idefs (num:env))

        IIfThenElse cond aff neg -> do
                                    let res1 = (evaluate cond idefs env)
                                    let res2 = (evaluate aff idefs env)
                                    let res3 = (evaluate neg idefs env)
                                    (IIfThenElse res1 res2 res3)

        IParens expr        -> (IParens (evaluate expr idefs env))
        IUnaryOp op expr    -> (IUnaryOp op (evaluate expr idefs env))

        ICompOp op expr1 expr2      -> do
                                    let res1 = (evaluate expr1 idefs env)
                                    let res2 = (evaluate expr2 idefs env)
                                    (ICompOp op res1 res2)

        IBinaryOp op expr1 expr2    -> do
                                    let res1 = (evaluate expr1 idefs env)
                                    let res2 = (evaluate expr2 idefs env)
                                    (IBinaryOp op res1 res2)

        IBooleanOp op expr1 expr2   -> do
                                    let res1 = (evaluate expr1 idefs env)
                                    let res2 = (evaluate expr2 idefs env)
                                    (IBooleanOp op res1 res2)

-- sameEither: Checks if two Either objects are the same Either
sameEither :: (Either Int Bool) -> (Either Int Bool) -> Bool
sameEither a b = case a of
                    Left num    -> case b of
                                    Left num    -> True
                                    Right bool  -> False
                    Right bool  -> case b of
                                    Left num    -> False
                                    Right bool  -> True

intEither :: (Either Int Bool) -> (Either Int Bool) -> Bool
intEither a b = case a of
                    Left num    -> case b of
                                    Left num    -> True
                                    Right bool  -> False
                    Right bool  -> False
-}