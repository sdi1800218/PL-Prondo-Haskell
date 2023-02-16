module Types where

{---------------- Functional Types ----------------}

-- AST representation of the source language (a small subset of Haskell).
-- [FDefinition] contains the AST representation of every other function definition.
type FProgram = (FExpr, [FDefinition])

-- FDefinition consists of the following triplet:
--      (Function Name, [Typical Parameters], AST Representation of Assigned Expression)
type FDefinition = (String, [String], FExpr)
-- Example: the function definition "foo(x, y) = x + y" is parsed into
--      ("foo", ["x", "y"], (FBinaryOp Plus (FVar "x") (FVar "y")).

-- FExpr contains the AST representation of the expression assigned in the "result" function.
data FExpr
        = FVar String
        | FNum Int
        | FBool Bool
        | FParens FExpr
        | FIfThenElse FExpr FExpr FExpr
        | FCall String [FExpr]
        | FCompOp OpCompare FExpr FExpr
        | FBinaryOp OpBinary FExpr FExpr
        | FBooleanOp OpBool FExpr FExpr
        | FUnaryOp OpUnary FExpr
        deriving (Eq, Show)

{---------------- Intensional Types ----------------}

-- AST representation of the intermediate (Intensional) language.
type IProgram = [IDefinition]

-- IDefinition consists of the pair:
--      (Function Name, AST Representation of Intensional Expression)
type IDefinition = (String, IExpr)
-- Example: the function definition "foo = x + y" is parsed into
--      ("foo", (IBinaryOp Plus (IVar "x") (IVar "y")).

-- IEnv represents the "tags" environment variable to be used by the Intensional evaluator.
type IEnv = [Int]

data IExpr
        = IVar String
        | INum Int
        | IBool Bool
        | IParens IExpr
        | IIfThenElse IExpr IExpr IExpr
        | ICall Int String
        | IActuals [IExpr] -- this is the only extra
        | ICompOp OpCompare IExpr IExpr
        | IBinaryOp OpBinary IExpr IExpr
        | IBooleanOp OpBool IExpr IExpr
        | IUnaryOp OpUnary IExpr
        deriving (Eq, Show)

{---------------- Common Types ----------------}

data OpBinary = Plus | Mult | Minus | Div
        deriving (Eq, Show)

data OpCompare = LtEq | Lt | GtEq | Gt | Eq | Neq
        deriving (Eq, Show)

data OpBool = And | Or
        deriving (Eq, Show)

data OpUnary = Positive | Negative | Not
        deriving (Eq, Show)
