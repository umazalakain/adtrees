module Language.ADTrees where

import Prelude hiding (or, and, min, max)

-- TODO: mention https://github.com/tomahawkins/fault-tree

type Name = String

data Faction = Proponent | Opponent
invert :: Faction -> Faction
invert Proponent = Opponent
invert Opponent = Proponent

-- Domain for a given faction
data Domain a b = MkDomain
    { get     :: a -> b
    , neutral :: b
    , or      :: b -> b -> b
    , and     :: b -> b -> b
    , counter :: b -> b
    }

-- Attack-defense domains
type ADDomain a b = Faction -> Domain a b

data Event a 
    = Basic Name a
    | Comment Name (Event a)
    | Counter (Event a)
    | And [Event a]
    | Or [Event a]
    deriving (Show, Eq)

aggregate :: ADDomain a b -> Faction -> Event a -> b
aggregate d p (Basic _ a) = get (d p) a
aggregate d p (Comment _ e) = aggregate d p e
aggregate d p (Counter e) = counter (d p) (aggregate d (invert p) e)
aggregate d p (And es) = undefined -- map (aggregate d p) es
aggregate d p (Or es) = undefined

cutsets :: Event a -> [Event a]
cutsets (Basic n a) = [Basic n a]
cutsets (Comment n e) = map (Comment n) (cutsets e)
cutsets (Counter e) = map Counter (cutsets e)
cutsets (And es) = map And (mapM cutsets es) -- cartesian product
cutsets (Or es) = concatMap cutsets es

-- EXAMPLES

probability :: (a -> Rational) -> ADDomain a Rational
probability f _ = MkDomain
    { get     = f
    , neutral = 0
    , or      = (+)
    , and     = (*)
    , counter = (1 -)
    }

data Difficulty = L | M | H

min :: Difficulty -> Difficulty -> Difficulty
min L _ = L
min _ L = L
min M _ = M
min _ M = M
min _ _ = H

invertDifficulty :: Difficulty -> Difficulty
invertDifficulty L = H
invertDifficulty M = M
invertDifficulty H = L

max :: Difficulty -> Difficulty -> Difficulty
max a b = min (invertDifficulty a) (invertDifficulty b)

difficulty :: (a -> Difficulty) -> ADDomain a Difficulty
difficulty f Proponent = MkDomain
    { get     = f
    , neutral = H
    , or      = min
    , and     = max
    , counter = invertDifficulty
    }
difficulty f Opponent = MkDomain
    { get     = f
    , neutral = L
    , or      = max
    , and     = min
    , counter = invertDifficulty
    }

-- TODO: Cost

example :: Event Rational
example = Comment "Enemy forges package" (Or [ 
    Comment "Enemy signs forged package" (
        Comment "Enemy has secret key" (
            Or [
                Basic "Enemy has stolen secret key" 0.2,
                Comment "Enemy has calculated secret key" (
                    Or [
                        Basic "Poor key generation allows secret key to be easily derived" 0.1,
                        Basic "Unknown vulnerability in hash algorithm is exploited" 0.1,
                        Comment "Enemy uses known attack on weak hash algorithm" (
                            And [
                                Basic "Enemy applies known attack to hash algorithm" 0.5,
                                Basic "Poor choice of hash algorithm" 0.1
                            ]
                        )
                    ]
                )
            ]
        )
    ),
    Comment "Enemy replaces genuine archive with malicious archive" (
        Comment "Enemy has constructed malicious archive with same hash as genuine archive" (
            Comment "Enemy has effectively broken hash algorithm" (
                Or [
                    Basic "Unknown vulnerability in hash algorithm is exploited" 0.1,
                    Comment "Enemy uses known attack on weak hash algorithm" (
                        And [
                            Basic "Enemy applies known attack to hash algorithm" 1.0,
                            Basic "Poor choice of hash algorithm" 0.1
                        ]
                    )
                ]
            )
        )
    )
    ])
