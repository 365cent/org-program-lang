module A1_400298939 where

import Text.XHtml (base)
import Language.Haskell.TH (Lit(IntegerL))
--From https://en.wikibooks.org/wiki/Haskell/Control_structures I think that its better to use case instead of let, so that it can give VError

data Expr =
      EInt Integer
    | ECh Char
    | EBool Bool
    | EString String
    --
    | EAdd Expr Expr -- addition of integers
    | EMul Expr Expr -- multiplication of integers
    | ENeg Expr -- negation of integers
    | ECat Expr Expr -- concatenation of strings
    | ECons Char Expr -- adding a character at the start of a string
    | EAnd Expr Expr -- And of two bool eans
    | EXor Expr Expr -- Xor of two bool eans
    | EIf Expr Expr Expr -- if −- then -- else
    | EShowInt Expr -- render an integer as a string
    deriving (Eq, Show)

data Val =
      VInt Integer
    | VBool Bool
    | VString String
    | VError -- something went wrong
    deriving (Eq, Show)


evalExpr :: Expr -> Val
evalExpr (EInt i)      = VInt i         -- trun Integer into the VInt constructor.
evalExpr (ECh c)       = VString [c]    -- convert a char into a string only contain one charator.
evalExpr (EBool b)     = VBool b        -- trun a bool into the VBool constructor.
evalExpr (EString s)   = VString s      -- trun a string into the VString constructor.

evalExpr (EAdd a b)    = 
    case (evalExpr a, evalExpr b) of        -- evaluate the two evalExpr
        (VInt i, VInt j) -> VInt (i + j)    -- if both are integers, add them.
        _ -> VError                         -- otherwise, it is an error.

evalExpr (EMul a b)    = 
    case (evalExpr a, evalExpr b) of        -- evaluate the two evalExpr
        (VInt i, VInt j) -> VInt (i * j)    -- If both are integers, multiply them.
        _ -> VError                         -- otherwise, it is an error.

evalExpr (ENeg a)      =
    case evalExpr a of          -- evaluate the two evalExpr
        VInt i -> VInt (-i)     -- If it's an integer, negate it.
        _ -> VError             -- otherwise, it is an error.

evalExpr (ECat a b)    =
    case (evalExpr a, evalExpr b) of                -- evaluate the two evalExpr
        (VString i, VString j) -> VString (i ++ j)  -- If both are strings, concatenate them.
        _ -> VError                                 -- otherwise, it is an error.

evalExpr (ECons c e)   =
    case evalExpr e of                  -- evaluate the two evalExpr
        VString s -> VString (c : s)    -- If it's a string, prepend the character to it.
        _ -> VError                     -- otherwise, it is an error.

evalExpr (EAnd a b)    =
    case (evalExpr a, evalExpr b) of            -- evaluate the two evalExpr
        (VBool i, VBool j) -> VBool (i && j)    -- If both are booleans, compute And of Bool.
        _ -> VError                             -- otherwise, it is an error.

evalExpr (EXor a b)    =
    case (evalExpr a, evalExpr b) of            -- evaluate the two evalExpr
        (VBool i, VBool j) -> VBool (i /= j)    -- If both are booleans, compute Xor using inequality.
        _ -> VError                             -- otherwise, it is an error.

evalExpr (EIf a b c)   =
    case evalExpr a of
        VBool i -> 
            if i then evalExpr b        -- if the condition is True, evaluate the b expression.
            else evalExpr c             -- otherwise, evaluate the c expression.
        _ -> VError                     -- otherwise, it is an error.

evalExpr (EShowInt a)  =
    case evalExpr a of                  -- evaluate the evalExpr a
        VInt i -> VString $ show i      -- If it's an integer, convert it to a string.
        _ -> VError                     -- otherwise, it is an error.
