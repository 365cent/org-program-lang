module A3.SKI where

-- * Question 1: Revenge of the Goblins and Gnomes
data SKI
    = S
    | K
    | I
    | App SKI SKI

    deriving (Eq, Show)
    
ski :: SKI -> Maybe SKI
ski (App (App (App S x) y) z) = Just $ App (App x z) (App y z)              -- Sxyz -> xz(yz)
ski (App (App K x) _)         = Just x                                      -- Kxy -> x
ski (App I x)                 = Just x                                      -- Ix -> x
ski (App x y)                 = case ski x of                               -- try to reduce left side
                                   Just reduced_x -> Just $ App reduced_x y
                                   Nothing -> case ski y of                 -- if fail, try right side
                                                 Just reduced_y -> Just $ App x reduced_y
                                                 Nothing -> Nothing
ski _                         = Nothing                                     -- No reduction
