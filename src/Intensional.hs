module Intensional (eval, evaluate, calculate) where

import Types
import Debug.Trace

{-
Μηχανισμός Εκτέλεσης

Αφού μεταφραστεί το πηγαίο πρόγραμμα σε νοηματικό κώδικα, μπορεί πλέον να εκτελεστεί για να παράξει το
επιθυμητό αποτέλεσμα.

Για αυτό τον σκοπό, πρέπει αρχικά να ορίσουμε την σημασιολογία των τελεστών call και
actuals. Το ακόλουθο μοντέλο αποδεικνύεται ότι είναι επαρκές, για την αποτίμηση νοηματικών προγραμμάτων:

• Ο τελεστής calli επαυξάνει μία λίστα w, προσαρτώντας το i στην κεφαλή της:
    (calli a)(w) = a(i : w).

• Ο τελεστής actuals παίρνει τον αριθμοδείκτη i στην κεφαλή μίας λίστας,
    και τον χρησιμοποιεί για να επιλέξει το i-οστό της στοιχείο:
    (actuals(a0 , ..., an−1 ))(i : w) = (ai )(w).

Συνδυάζοντας τη σημασιολογία των call και actuals με τους κανόνες αποτίμησης εκφράσεων που φαίνονται
παρακάτω, μπορούμε πλέον να εκτελέσουμε ένα πρόγραμμα σε νοηματικό κώδικα.

1 EVAL(E1 BinaryOp E2, tags) = EVAL(E1, tags) BinaryOp EVAL(E2, tags)

2 EVAL(if B then E1 else E2, tags) = if EVAL(B, tags) then EVAL(E1, tags) else EVAL(E2, tags)

3 EVAL(constant, tags) = constant

4 EVAL((E1), tags) = EVAL(E1, tags)

5 EVAL(UnaryOp E, tags) = UnaryOp EVAL(E, tags)

-}

-- TODO: Doc
-- Hint: The valid return values are INum and IBool
eval :: IProgram -> IExpr
eval ip = do
    let resultVar = snd (head ip)
    let resultEval = (evaluate resultVar ip [])
    let res = calculate resultEval
    case res of
        Left num    -> (INum num)
        Right bull  -> (IBool bull)

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

-- TODO: Doc
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
                                            --Div     -> Left (handleDivision strip1 strip2)

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

lookMeUp :: String -> [IDefinition] -> IExpr
-- if it dont exist, we got problems.
lookMeUp symbol (idef:idefs) | symbol == (fst idef)   = (snd idef)
                             | otherwise              = (lookMeUp symbol idefs)