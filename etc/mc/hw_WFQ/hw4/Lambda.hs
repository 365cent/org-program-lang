{-# OPTIONS_GHC -Wall #-}
-- | A simple, small-step evaluator for the untyped lambda calculus.
module Lambda where

-- * Helper Functions

-- | Remove every occurance of 'x' from 'xs'.
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y:xs) | x == y = remove x xs
                | otherwise = y:remove x xs

-- * Lambda Calculus

-- | Expressions in the untyped lambda calculus.
data Expr
    = Var String
    -- ^ Variables: 'x'
    | Lam String Expr
    -- ^ Lambda Abstraction: 'λ x. e'
    | App Expr Expr
    -- ^ Application: 'e e'
    | Pair Expr Expr
    -- ^ Tuple Creation: 'pair e e'
    | Fst Expr
    -- ^ Projecting first element in pair: fst e
    | Snd Expr
    -- ^ Projecting second element in pair: snd e
    | Bool Bool
    -- ^ Boolean constants: True/False
    | And Expr Expr
    -- ^ Logical conjunction: e ⋀ e
    | If Expr Expr Expr
    -- ^ If-then-else: if e then e else e
    | Let String Expr Expr
    -- ^ Let bindings 'let x = e in e'


-- * Substitution
-- This is the engine of computation for lambda calculus
--
-- More subtle than it first appears!
-- Consider the example of:
--   (λ x. λ y. x) (λ z. y)
--
-- We need to handle these variable capturing issues somehow...
-- Our strategy is to rename everything to avoid capturing
-- entirely.
--
-- We also need to handle let-bindings; have to decide on
-- variable scoping! IE:
--  let x = x in x
--          ^--------[ Is this variable free or bound? ]
--
-- We will consider 'x' free in the previous example; in other words,
-- the variable of a let-binding is *not* bound in the binding, only in the bod.

-- | Compute the set of free variables of an expression.
freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Lam x e) =
    -- 'x' is bound in 'λ x. e', so we remove it from the free variable set.
    remove x (freeVars e)
freeVars (App e1 e2) =
    freeVars e1 ++ freeVars e2
freeVars (Pair e1 e2) =
    freeVars e1 ++ freeVars e2
freeVars (Fst e) =
    freeVars e
freeVars (Snd e) =
    freeVars e
freeVars (Bool _) =
    []
freeVars (And e1 e2) =
    freeVars e1 ++ freeVars e2
freeVars (If e1 e2 e3) =
    freeVars e1 ++ freeVars e2 ++ freeVars e3
freeVars (Let x e1 e2) =
    -- Recall that 'x' is not bound in 'e1' for let-bindings,
    -- so we do not remove it from the free variables of 'e1'.
    freeVars e1 ++ remove x (freeVars e2)

-- | Pick a name that isn't found in the provided list of names.
freshen :: String -> [String] -> String
freshen name avoid | name `elem` avoid = freshen (name ++ "'") avoid
                   | otherwise = name

-- | Rename a term to not use the names in the provided list.
rename :: Expr -> [String] -> Expr
rename e = go e []
  where
    -- Basic algorithm is to track what we have renamed variables
    -- to, and then freshen each bound variable.
    go :: Expr -> [(String, String)] -> [String] -> Expr
    go (Var x) rn _ =
        case lookup x rn of
          Just y -> Var y
          Nothing -> Var x
    go (Lam x e) rn avoid =
        -- Invent a new name for 'x', and then record
        -- it's new name.
        let x' = freshen x avoid in
        Lam x' (go e ((x, x') : rn) avoid)
    go (App e1 e2) rn avoid =
        App (go e1 rn avoid) (go e2 rn avoid)
    go (Pair e1 e2) rn avoid =
        Pair (go e1 rn avoid) (go e2 rn avoid)
    go (Fst e1) rn avoid = Fst (go e1 rn avoid)
    go (Snd e1) rn avoid = Snd (go e1 rn avoid)
    go b@(Bool _) _ _ = b
    go (And e1 e2) rn avoid = And (go e1 rn avoid) (go e2 rn avoid)
    go (If e1 e2 e3) rn avoid = If (go e1 rn avoid) (go e2 rn avoid) (go e3 rn avoid)
    go (Let x e1 e2) rn avoid =
      -- Invent a new name for 'x', and then record
      -- it's new name.
      let x' = freshen x avoid
      -- 'x' is not bound in 'e1', so we do not rename 'x' to
      -- it's new name in 'e1'.
      in Let x' (go e1 rn avoid) (go e2 ((x, x') : rn) avoid)

-- | Substitute 'x' for 'e1' in 'e2'.
subst :: String -> Expr -> Expr -> Expr
subst x e1 e2 =
    -- Remember that we need to avoid capturing variables!
    -- Our strategy for doing so is to avoid the problem entirely
    -- by renaming all the variables 'e1' to names that are not fresh
    -- in 'e2', eliminting the potential for capture.
    --
    -- Note that 'rename' also makes each name unique, which simplifies
    -- the 'Lam' case, as we do not need to worry about shadowing.
    substRenamed x e1 (rename e2 (freeVars e1))
  where
    substRenamed :: String -> Expr -> Expr -> Expr
    substRenamed x e1 (Var y) | x == y = e1 -- Perform the substitution when the vars are equal.
                              | otherwise = Var y
    substRenamed x e1 (Lam y e2) | x == y = Lam y e2 -- If 'x == y', then the lambda shadows, so we stop.
                                 | otherwise = Lam y (substRenamed x e1 e2)
    substRenamed x e1 (App e2 e3) =
        App (substRenamed x e1 e2) (substRenamed x e1 e3)
    substRenamed x e1 (Let v e2 e3) =
        -- 'v' is not bound in 'e2', so we substitute regardless
        -- of whether 'x == v'.
        let e2' = substRenamed x e1 e2 in
        -- If 'x == v', then the let binding shadows, so we do not
        -- substitute into the body.
        if x == v then
          Let v e2' e3
        else
          Let v e2' (substRenamed x e1 e3)

-- * Reduction
-- We are using a small-step strategy for evaluation, where we repeatedly
-- perform single reduction steps until we cannot step anymore.
--
-- This is not the only strategy for evaluation! It is somewhat inefficient,
-- but it does let us see every single reduction we take, in order.

-- | Evaluation strategy for CBV        
stepCBV :: Expr -> Maybe Expr
stepCBV = error "TODO: left as an exercise"

-- | Evaluation strategy for CBN
stepCBN :: Expr -> Maybe Expr
stepCBN (Var _) =
    -- Cannot reduce a lone variable.
    Nothing
stepCBN (Lam _ _) =
    -- Lambda expressions are normal forms in CBV.
    Nothing
stepCBN (App fn arg) =
    -- Reduce the LHS first
    case stepCBN fn of
      Just vfn ->
          -- We can make a reduction!
          Just (App vfn arg)
      Nothing ->
          -- Both the function and the argument are values; check to see if the
          -- function is a lambda.
          -- Note that we aren't reducing the argument!
          case fn of
            Lam x body ->
                -- If it is, then we can β-reduce.
                Just (subst x arg body)
            _ -> Nothing
stepCBN (Bool _) =
    -- Boolean literals cannot be reduced.
    Nothing
stepCBN (And (Bool True) e2) = Just e2
stepCBN (And (Bool False) e2) = Just (Bool False) -- ~ short circuiting
stepCBN (And e1 e2) = 
  case stepCBN e1 of
    Just e1' -> Just (And e1' e2)
    Nothing -> Nothing -- ill-typed or irreducible (e.g., a free variable)
stepCBN (Pair _ _) = Nothing
stepCBN (Fst (Pair e1 e2)) = Just e1
stepCBN (Snd (Pair e1 e2)) = Just e2
stepCBN (Fst e) = 
  case stepCBN e of
    (Just e') -> Just $ Fst e'
    Nothing -> Nothing
stepCBN (Snd e) = 
  case stepCBN e of
    (Just e') -> Just $ Snd e'
    Nothing -> Nothing
stepCBN (If (Bool True) e2 e3) = Just e2
stepCBN (If (Bool False) e2 e3) = Just e3
stepCBN (If e1 e2 e3) = 
  case stepCBN e1 of
    Just e1' -> Just (If e1' e2 e3)
    Nothing -> Nothing -- ill-typed or irreducible (e.g., a free variable)
stepCBN (Let v e1 e2) = Just (subst v e1 e2)


-- | Reduce an expression using a given reduction strategy.
reduceWith :: (Expr -> Maybe Expr) -> Expr -> [Expr]
reduceWith strat e =
    case strat e of
      Just e' -> e : reduceWith strat e'
      Nothing -> [e]

-- * Pretty Printing
-- ‼️ You do not need to understand this code ‼️

type Prec = (Int, Int)

nonassoc :: Int -> Prec
nonassoc p = (2*p, 2*p)

left :: Int -> Prec
left p = (2*p, 2*p+1)

right :: Int -> Prec
right p = (2*p+1, 2*p)

isolated :: Prec
isolated = (minBound, minBound)

leftOf :: Prec -> Prec
leftOf (l, _) = (minBound, l)

rightOf :: Prec -> Prec
rightOf (_, r) = (r, minBound)

app :: Prec
app = left 1

arrow :: Prec
arrow = right 0

delimited :: Prec
delimited = nonassoc 2

atom :: Prec
atom = nonassoc 3

parens :: Prec -> Prec -> String -> String
parens (l', r') (l, r) s =
    if l' >= l || r' >= r then
      "(" ++ s ++ ")"
    else
      s

classify :: Expr -> Prec
classify (Var _) = atom
classify (App _ _) = app
classify (Lam _ _) = arrow
classify (Pair _ _) = delimited
classify (Fst _) = app
classify (Snd _) = app
classify (Bool _) = atom
classify (And _ _) = app
classify (If _ _ _) = app
classify (Let _ _ _) = delimited

-- | This is in IO to avoid an annoying GHCi restriction
-- when printing unicode strings.
pretty :: Expr -> IO ()
pretty e = putStrLn $ go e isolated
  where
    go :: Expr -> Prec -> String
    go e prec =
        parens prec (classify e) $
        case e of
          Var x -> x
          Lam x e -> "\\" ++ x ++ ". " ++ go e (rightOf arrow)
          App e1 e2 -> go e1 (leftOf app) ++ " " ++ go e2 (rightOf app)
          Pair e1 e2 -> "(" ++ go e1 isolated ++ ", " ++ go e2 isolated
          Fst e -> "fst " ++ go e (rightOf app)
          Snd e -> "snd " ++ go e (rightOf app)
          And e1 e2 -> "and " ++ go e1 (rightOf app) ++ " " ++ go e2 (rightOf app)
          If e1 e2 e3 -> "if " ++ go e1 isolated ++ " then " ++ go e2 isolated ++ " else " ++ go e3 (rightOf app)
          Let x e1 e2 -> "let " ++ x ++ " = " ++ go e1 isolated ++ " in " ++ go e2 (rightOf app)

printReductions :: (Expr -> Maybe Expr) -> Expr -> IO ()
printReductions strat e = mapM_ pretty (reduceWith strat e)

ex1 :: Expr
ex1 = Lam "x" (Lam "y" (Var "z"))

ex2 :: Expr
ex2 = App ex1 (Var "z")

