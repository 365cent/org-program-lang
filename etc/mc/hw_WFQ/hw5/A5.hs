{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-name-shadowing -Wno-unused-matches #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Main part comes from Skel.hs basically
module A5 where

import Control.Applicative ()
import Data.Functor
import Data.List
import Debug.Trace
import Data.Maybe

-- * Core Data Types
-- | Untyped expressions
data Expr
    = Var String
    -- ^ Variables
    | Lam String Expr | App Expr Expr
    -- ^ Lambda and application (See figure 9-1)
    | Unit
    -- ^ Unit (See 11.2 in TAPL)
    | Let String Expr Expr
    -- ^ Let-bindings (See 11.5 in TAPL)
    | Pair Expr Expr | Fst Expr | Snd Expr
    -- ^ Pairs (See 11.6 in TAPL)
    | TT | FF | If Expr Expr Expr
    -- ^ Booleans (See 8.2 in TAPL)
    | Zero | Succ Expr | Pred Expr | IsZero Expr
    -- ^ Natural Numbers (See 8.2 in TAPL)
    deriving (Show)

-- | Types.
data Type
  = TpVar String
  -- ^ Type variables
  | FnTp Type Type
  -- ^ Function types
  | UnitTp
  -- ^ Unit (See 11.2 in TAPL)
  | PairTp Type Type
  -- ^ Pair Types (See 11.6 in TAPL)
  | BoolTp
  -- ^ Booleans (See 8.2 in TAPL)
  | NatTp
  -- ^ Booleans (See 8.2 in TAPL)
  deriving (Eq, Show)

-- | Typed expressions
data TypedExpr
    = TVar String
    -- ^ Variables
    | TLam String Type TypedExpr | TApp TypedExpr TypedExpr
    -- ^ Lambda and application (See figure 9-1)
    | TUnit
    -- ^ Unit (See figure 11-2 in TAPL)
    | TLet String Scheme TypedExpr TypedExpr
    -- ^ Let-bindings (See figure 11-4 in TAPL)
    | TPair TypedExpr TypedExpr | TFst TypedExpr | TSnd TypedExpr
    -- ^ Pairs (See figure 11-5 in TAPL)
    | TTT | TFF | TIf TypedExpr TypedExpr TypedExpr
    -- ^ Booleans (See figure 8-2 in TAPL)
    | TZero | TSucc TypedExpr | TPred TypedExpr | TIsZero TypedExpr
    -- ^ Natural Numbers (See figure 8-2 in TAPL)
    deriving (Show)

data Scheme = Poly [String] Type | Mono Type
    deriving (Show)

-- | Contexts
type Ctx = [(String, Scheme)]

-- | Substitutions
type Substitution = [(String, Type)]

freeVars :: Expr -> [String]
freeVars (Var v) = [v]
freeVars (Lam v e) = freeVars e \\ [v]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Let v e1 e2) = freeVars e1 `union` (freeVars e2 \\ [v])
freeVars (Pair e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Fst e) = freeVars e
freeVars (Snd e) = freeVars e
freeVars (If e1 e2 e3) = freeVars e1 `union` freeVars e2 `union` freeVars e3
freeVars (Succ e) = freeVars e
freeVars (Pred e) = freeVars e
freeVars (IsZero e) = freeVars e
freeVars _ = []  -- For Unit, TT, FF, Zero

freeTpVars :: Type -> [String]
freeTpVars (TpVar v) = [v]
freeTpVars (FnTp t1 t2) = freeTpVars t1 `union` freeTpVars t2
freeTpVars _ = []  -- Base types have no type variables

freeSchemeVars :: Scheme -> [String]
freeSchemeVars (Mono t) = freeTpVars t
freeSchemeVars (Poly vars t) = freeTpVars t \\ vars

freeCtxVars :: Ctx -> [String]
freeCtxVars ctx = nub $ concatMap (freeSchemeVars . snd) ctx
-- * Substitution

composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 = [(v, substTp sub2 t) | (v, t) <- sub1] ++ sub2

composeSubs :: [Substitution] -> Substitution
composeSubs =  foldl' composeSub []

-- | Remove a set of variables from a substitution.
thinSub :: [String] -> Substitution -> Substitution
thinSub avoid sub = filter (\(v, _) -> notElem v avoid) sub

-- | Pick a name that isn't found in the provided list of names.
freshen :: String -> [String] -> String
freshen name avoid = head $ filter (`notElem` avoid) (map (++"'") (iterate (++"'") name))
                   
-- | Rename a term to not use the names in the provided list.
rename :: Expr -> [String] -> Expr
rename e avoid = go e []
  where
    go (Var v) renames = Var $ fromMaybe v (lookup v renames)
    go (Lam v e) renames = 
        let v' = freshen v avoid 
        in Lam v' (go e ((v, v') : renames))
    go (App e1 e2) renames = App (go e1 renames) (go e2 renames)
    -- ... Implement cases for Let, Pair, Fst, Snd, etc. ...
    go e _ = e  -- For Unit, TT, FF, Zero, Succ, Pred, IsZero
-- | 'subst e1 e2 x' is 'e1[e2/x]'
subst :: Expr -> Expr -> String -> Expr
subst e1 e2 x = case e1 of
    Var v -> 
        if v == x then e2 else Var v
    Lam v e -> 
        if v == x then Lam v e else Lam v (subst e e2 x)
    App e1' e2' -> 
        App (subst e1' e2 x) (subst e2' e2 x)
    Let v e1' e2' -> 
        Let v (subst e1' e2 x) (if v == x then e2' else subst e2' e2 x)
    Pair e1' e2' -> 
        Pair (subst e1' e2 x) (subst e2' e2 x)
    Fst e' -> 
        Fst (subst e' e2 x)
    Snd e' -> 
        Snd (subst e' e2 x)
    If e1' e2' e3' -> 
        If (subst e1' e2 x) (subst e2' e2 x) (subst e3' e2 x)
    Succ e' -> 
        Succ (subst e' e2 x)
    Pred e' -> 
        Pred (subst e' e2 x)
    IsZero e' -> 
        IsZero (subst e' e2 x)
    _ -> e1  -- For Unit, TT, FF, Zero

-- | Substitute for type variables in a type.
substTp :: Substitution -> Type -> Type
substTp sub (TpVar v) = fromMaybe (TpVar v) (lookup v sub)
substTp sub (FnTp t1 t2) = FnTp (substTp sub t1) (substTp sub t2)
substTp _ t = t  -- For UnitTp, PairTp, BoolTp, NatTp

-- | Substitute for type variables in a type scheme.
substScheme :: Substitution -> Scheme -> Scheme
substScheme sub (Mono t) = Mono (substTp sub t)
substScheme sub (Poly vars t) = Poly vars (substTp (thinSub vars sub) t)

-- | Substitute for type variables in a typed expression.
substTypedExpr :: Substitution -> TypedExpr -> TypedExpr
substTypedExpr sub te = case te of
    TVar v -> TVar v
    TLam v t e -> TLam v (substTp sub t) (substTypedExpr sub e)
    TApp e1 e2 -> TApp (substTypedExpr sub e1) (substTypedExpr sub e2)
    TLet v scheme e1 e2 -> TLet v (substScheme sub scheme) (substTypedExpr sub e1) (substTypedExpr sub e2)
    TPair e1 e2 -> TPair (substTypedExpr sub e1) (substTypedExpr sub e2)
    TFst e -> TFst (substTypedExpr sub e)
    TSnd e -> TSnd (substTypedExpr sub e)
    TIf e1 e2 e3 -> TIf (substTypedExpr sub e1) (substTypedExpr sub e2) (substTypedExpr sub e3)
    TSucc e -> TSucc (substTypedExpr sub e)
    TPred e -> TPred (substTypedExpr sub e)
    TIsZero e -> TIsZero (substTypedExpr sub e)
    TTT -> TTT
    TFF -> TFF
    TZero -> TZero
    TUnit -> TUnit


substCtx :: Substitution -> Ctx -> Ctx
substCtx sub ctx = map (\(var, scheme) -> (var, substScheme sub scheme)) ctx

-- * Type Inference and Erasure

generalize :: Ctx -> Type -> Scheme
generalize ctx tp = Poly (freeTpVars tp \\ ctxVars) tp
  where ctxVars = concatMap (freeSchemeVars . snd) ctx

instantiate :: Scheme -> Infer Type
instantiate (Mono t) = return t
instantiate (Poly vars t) = do
  freshVars <- mapM (const freshTpVar) vars
  let sub = zip vars freshVars
  return $ substTp sub t

-- | Helper function for construct the synth
extendCtx :: Ctx -> (String, Scheme) -> Ctx
extendCtx ctx (var, scheme) = (var, scheme) : ctx

synth :: Ctx -> Expr -> Infer (TypedExpr, Type, Substitution)
synth ctx expr = case expr of
    Var x -> 
        -- Handle variables
        case lookup x ctx of
            Just scheme -> do
                t <- instantiate scheme
                return (TVar x, t, [])
            Nothing -> typeError $ "Unbound variable: " ++ x

    Lam x e -> 
        -- Handle lambda abstractions
        do
            alpha <- freshTpVar
            let ctx' = extendCtx ctx (x, Mono alpha)
            (te, t, sub) <- synth ctx' e
            return (TLam x alpha te, FnTp (substTp sub alpha) t, sub)

    App e1 e2 -> 
        -- Handle function applications
        do
            (te1, t1, sub1) <- synth ctx e1
            (te2, t2, sub2) <- synth (substCtx sub1 ctx) e2
            beta <- freshTpVar
            sub3 <- unify (substTp sub2 t1) (FnTp t2 beta)
            let finalSub = composeSubs [sub3, sub2, sub1]
            return (TApp te1 te2, substTp finalSub beta, finalSub)

    Let x e1 e2 -> 
        -- Handle let-bindings
        do
            (te1, t1, sub1) <- synth ctx e1
            let scheme = generalize (substCtx sub1 ctx) t1
            let ctx' = extendCtx (substCtx sub1 ctx) (x, scheme)
            (te2, t2, sub2) <- synth ctx' e2
            let finalSub = composeSub sub2 sub1
            return (TLet x scheme te1 te2, t2, finalSub)

    -- Handle Unit
    Unit -> 
        return (TUnit, UnitTp, [])

    -- Handle Pairs
    Pair e1 e2 -> 
        do
            (te1, t1, sub1) <- synth ctx e1
            (te2, t2, sub2) <- synth (substCtx sub1 ctx) e2
            return (TPair te1 te2, PairTp t1 t2, composeSub sub1 sub2)

    -- Handle Fst (First element of a pair)
    Fst e -> 
        do
            (te, t, sub) <- synth ctx e
            alpha <- freshTpVar
            beta <- freshTpVar
            sub' <- unify t (PairTp alpha beta)
            return (TFst te, substTp sub' alpha, composeSub sub sub')

    -- Handle Snd (Second element of a pair)
    Snd e -> 
        do
            (te, t, sub) <- synth ctx e
            alpha <- freshTpVar
            beta <- freshTpVar
            sub' <- unify t (PairTp alpha beta)
            return (TSnd te, substTp sub' beta, composeSub sub sub')

    -- Handle Booleans (True and False)
    TT -> return (TTT, BoolTp, [])
    FF -> return (TFF, BoolTp, [])

    -- Handle If expressions
    If e1 e2 e3 -> 
        do
            (te1, t1, sub1) <- synth ctx e1
            (te2, t2, sub2) <- synth (substCtx sub1 ctx) e2
            (te3, t3, sub3) <- synth (substCtx sub2 ctx) e3
            sub4 <- unify t1 BoolTp
            sub5 <- unify t2 t3
            let finalSub = composeSubs [sub5, sub4, sub3, sub2, sub1]
            return (TIf te1 te2 te3, t2, finalSub)

    -- Handle Natural Numbers (Zero, Succ, Pred, IsZero)
    Zero -> return (TZero, NatTp, [])

    Succ e -> 
        do
            (te, t, sub) <- synth ctx e
            sub' <- unify t NatTp
            return (TSucc te, NatTp, composeSub sub sub')
    Pred e -> 
        do
            (te, t, sub) <- synth ctx e
            sub' <- unify t NatTp
            return (TPred te, NatTp, composeSub sub sub')
    IsZero e -> 
        do
            (te, t, sub) <- synth ctx e
            sub' <- unify t NatTp
            return (TIsZero te, BoolTp, composeSub sub sub')


-- Helper function to construct the unify
-- learned from the TuT unification.hs
unifyTVar :: String -> Type -> Infer Substitution
unifyTVar var t
  | t == TpVar var = 
      -- If the type variable is the same as the type, no substitution is needed.
      return []
  | var `elem` freeTpVars t = 
      -- Occurs check: A type variable cannot be unified with a type if it occurs in that type.
      typeError $ "Infinite type: " ++ var ++ " occurs in " ++ show t
  | otherwise = 
      -- Otherwise, we can unify the type variable with the type.
      return [(var, t)]


unify :: Type -> Type -> Infer Substitution
unify (TpVar alpha) tp = unifyTVar alpha tp
unify tp (TpVar beta) = unifyTVar beta tp
unify (FnTp atp1 atp2) (FnTp btp1 btp2) = do
    -- Unify the input types of the function types
    sub1 <- unify atp1 btp1
    -- Apply the first substitution to the output types before unifying them
    let atp2Substituted = substTp sub1 atp2
    let btp2Substituted = substTp sub1 btp2
    sub2 <- unify atp2Substituted btp2Substituted
    -- Compose the substitutions from both unification steps
    return $ composeSub sub2 sub1

unify NatTp NatTp = return []
unify BoolTp BoolTp = return []
unify UnitTp UnitTp = return []
unify (PairTp t1 t2) (PairTp t3 t4) = do
    sub1 <- unify t1 t3
    sub2 <- unify (substTp sub1 t2) (substTp sub1 t4)
    return $ composeSub sub2 sub1
unify _ _ = typeError "Cannot unify different types"


-- | Infer the type of an expression, returning
-- it's typed counterpart, along with it's type.
infer :: Ctx -> Expr -> Either String (TypedExpr, Type)
infer ctx e = execInfer (do
  (te, t, _) <- synth ctx e
  return (te, t))

-- | Erase a typed expression.
erase :: TypedExpr -> Expr
erase (TVar v) = Var v
erase (TLam v _ e) = Lam v (erase e)
erase (TApp e1 e2) = App (erase e1) (erase e2)
-- ... Implement cases for TLet, TPair, TFst, TSnd, etc. ...
erase _ = undefined  -- For TUnit, TTT, TFF, TZero, TSucc, TPred, TIsZero

step :: Expr -> Maybe Expr
step e = case e of
    -- Beta reduction for application (App) of a lambda (Lam) expression
    App (Lam x e1) e2 -> 
        Just (subst e1 e2 x) -- Substitute e2 for x in e1

    -- Conditional evaluation (If)
    If TT e1 e2 -> 
        Just e1
    If FF e1 e2 -> 
        Just e2
    If e1 e2 e3 -> 
        fmap (\e1' -> If e1' e2 e3) (step e1)

    -- Numeric operations (Succ, Pred, IsZero)
    Succ (Pred e) -> 
        Just e
    Pred (Succ e) -> 
        Just e
    IsZero Zero -> 
        Just TT
    IsZero (Succ _) -> 
        Just FF
    IsZero e -> 
        fmap IsZero (step e)

    _ -> Nothing  -- No step possible


eval :: Expr -> Expr
eval e = maybe e eval (step e)

-- * Inference Monad

newtype Infer a = Infer { runInfer :: [String] -> Either String (a, [String]) }
    deriving (Functor)

instance Applicative Infer where
    pure a = Infer $ \s -> Right (a, s)
    mf <*> ma = Infer $ \s -> do
        (f, s') <- runInfer mf s
        (a, s'') <- runInfer ma s'
        pure (f a, s'')

instance Monad Infer where
    return = pure
    mf >>= k = Infer $ \s -> do
        (a, s') <- runInfer mf s
        runInfer (k a) s'

-- | Generate a fresh type variable.
freshTpVar :: Infer Type
freshTpVar = Infer $ \names ->
  let name = head names
  in Right (TpVar name, tail names)

-- | Emit a type error.
typeError :: String -> Infer a
typeError err = Infer $ \_ -> Left err

-- | Infinite list of fresh names.
genNames :: [String]
genNames = map pure ['a' .. 'z'] ++ map (++ "'") genNames

execInfer :: Infer a -> Either String a
execInfer m = fst <$> runInfer m genNames

-- * Misc. Helpers

unionWith :: (Eq b) => (a -> [b]) -> [a] -> [b]
unionWith k = foldr (\x bs -> union (k x) bs) []

remove :: (Eq a) => a -> [(a,b)] -> [(a,b)]
remove a [] = []
remove a ((a', x):xs) | a == a' = remove a xs
                      | otherwise = (a', x) : remove a xs
