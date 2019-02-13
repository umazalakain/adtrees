module Language.ADTrees where

import Prelude hiding (or, and, min, max)
import Data.List (lookup)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- TODO: mention https://github.com/tomahawkins/fault-tree

type Name = String

data Player = A | D deriving (Show, Eq)

invert :: Player -> Player
invert A = D
invert D = A

-- Semantics for a given faction
data FSemantics a b = MkFSemantics
    { get   :: a -> b
    , plus  :: b -> b -> b
    , zero  :: b
    , times :: b -> b -> b
    , one   :: b
    , minus :: b -> b
    }

-- Attack-defense semantics
type Semantics a b = Player -> FSemantics a b

data Event a 
    = Basic Player Name a
    | And   Player Name [Event a]
    | Or    Player Name [Event a]
    deriving (Show, Eq)

-- TODO: add minus
aggregate :: Semantics a b -> Event a -> b
aggregate d (Basic p _ a)   = get (d p) a
aggregate d (And   p _ es)  = foldr (times (d p) . aggregate d) (one $ d p) es
aggregate d (Or    p _ es)  = foldr (plus (d p) . aggregate d) (zero $ d p) es

-- TODO: check this makes sense
cutsets :: Event a -> [Event a]
cutsets (Basic p n a) = [Basic p n a]
cutsets (And p n es) = map (And p n) (mapM cutsets es) -- cartesian product
cutsets (Or _ _ es) = concatMap cutsets es

flatten :: Event a -> [Event a]
flatten e@(Basic _ _ _) = [e]
flatten e@(And _ _ es)  = e : concatMap flatten es
flatten e@(Or _ _ es)   = e : concatMap flatten es

dot :: Eq a => Event a -> String
dot r = unlines
    [ "digraph {"
    , "\trankdir=BT"
    , unlines $ map node (flatten r)
    , unlines $ map edge (flatten r)
    , "}"
    ]
    where
        ids = [ (e', "event_" ++ show i) | (e', i) <- zip (flatten r) [0 :: Int ..] ]
        eventId e = fromJust $ lookup e ids

        -- TODO: add player colors
        -- TODO: add attributes
        -- TODO: add or/and fanciness
        node e@(Basic p n a) = printf "\t%s [label=\"%s\"]" (eventId e) n
        node e@(And p n es)  = printf "\t%s [label=\"AND %s\"]" (eventId e) n
        node e@(Or p n es)   = printf "\t%s [label=\"OR %s\"]" (eventId e) n

        edge e@(Basic p n a) = ""
        edge e@(And p n es)  = unlines [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- es ]
        edge e@(Or p n es)   = unlines [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- es ]
-- EXAMPLES

probability :: (a -> Rational) -> Semantics a Rational
probability f _ = MkFSemantics
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

difficulty :: (a -> Difficulty) -> Semantics a Difficulty
difficulty f A = MkFSemantics
    { get   = f
    , plus  = min
    , zero  = H
    , times = max
    , one   = L
    , minus = invertDifficulty
    }
difficulty f D = MkFSemantics
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
    } deriving (Show, Eq)

example :: Event ExampleAttribute
example = Or A "Bank Account" [
    And A "ATM" [
        Or A "PIN" [
            Basic A "Eavesdrop" $ MEA 0.01 L,
            And A "Find Note" [
                Basic D "Memorize" $ MEA 0.80 L],
            Basic A "Force" $ MEA 0.01 H],
        Basic A "Card" $ MEA 0.1 H],
    And A "Online" [
        Or A "Password" [
            Basic A "Phishing" $ MEA 0.90 L,
            Basic A "Key logger" $ MEA 0.20 M],
        Basic A "User name" $ MEA 0.90 L,
        Or D "2nd Auth Factor" [
            Basic D "Key Fobs" $ MEA 0.01 L,
            Basic D "PIN Pad" $ MEA 0.01 L,
            Or A "Malware" [
                Basic A "Browser" $ MEA 0.20 M,
                Basic A "OS" $ MEA 0.20 M]]]]
