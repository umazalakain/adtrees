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
    { get   :: a -> b
    , plus  :: b -> b -> b
    , zero  :: b
    , times :: b -> b -> b
    , one   :: b
    , minus :: b -> b
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
aggregate d p (Counter e) = minus (d p) (aggregate d (invert p) e)
aggregate d p (And es) = foldr (times (d p) . aggregate d p) (one $ d p) es
aggregate d p (Or es) = foldr (plus (d p) . aggregate d p) (zero $ d p) es

cutsets :: Event a -> [Event a]
cutsets (Basic n a) = [Basic n a]
cutsets (Comment n e) = map (Comment n) (cutsets e)
cutsets (Counter e) = map Counter (cutsets e)
cutsets (And es) = map And (mapM cutsets es) -- cartesian product
cutsets (Or es) = concatMap cutsets es

-- EXAMPLES

probability :: (a -> Rational) -> ADDomain a Rational
probability f _ = MkDomain
    { get   = f
    , plus  = (+)
    , zero  = 0
    , times = (*)
    , one   = 1
    , minus = (1 -)
    }

data Difficulty = L | M | H deriving (Show, Eq)

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
    { get   = f
    , plus  = min
    , zero  = H
    , times = max
    , one   = L
    , minus = invertDifficulty
    }
difficulty f Opponent = MkDomain
    { get   = f
    , plus  = max
    , zero  = L
    , times = min
    , one   = H
    , minus = invertDifficulty
    }

-- TODO: Cost

data ExampleAttribute = MEA
    { getProbability :: Rational
    , getDifficulty  :: Difficulty
    }

example :: Event ExampleAttribute
example = Comment "Bank Account" (Or [
    Comment "ATM" (And [
        Comment "PIN" (Or [
            Basic "Eavesdrop" $ MEA 0.01 L,
            Comment "Find Note" (Counter (
                Basic "Memorize" $ MEA 0.80 L)),
            Basic "Force" $ MEA 0.01 H]),
        Basic "Card" $ MEA 0.1 H]),
    Comment "Online" (And [
        Comment "Password" (Or [
            Basic "Phishing" $ MEA 0.90 L,
            Basic "Key logger" $ MEA 0.20 M]),
        Basic "User name" $ MEA 0.90 L,
        Counter (
            Comment "2nd Auth Factor" (Or [
                Basic "Key Fobs" $ MEA 0.01 L,
                Basic "PIN Pad" $ MEA 0.01 L,
                Counter (
                    Comment "Malware" (Or [
                        Basic "Browser" $ MEA 0.20 M,
                        Basic "OS" $ MEA 0.20 M]))]))])])
