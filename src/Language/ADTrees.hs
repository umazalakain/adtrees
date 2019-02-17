module Language.ADTrees
    ( Player(..)
    , ADTree(..)
    , Semantics
    , PSemantics(..)
    , aggregate
    , cutsets
    , flatten
    , dot
    , probability
    , difficulty
    ) where

import Data.List (lookup)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- --------- --
-- Structure --
-- --------- --

type Name = String

data Player = A | D deriving (Show, Eq)

data ADTree a 
    = Basic   Name a
    | And     Name [ADTree a]
    | Or      Name [ADTree a]
    | Counter Name (ADTree a) (ADTree a)
    deriving (Show, Eq)

switchPlayer :: Player -> Player
switchPlayer A = D
switchPlayer D = A

-- TODO: check this makes sense
cutsets :: ADTree a -> [ADTree a]
cutsets (Basic n a) = [Basic n a]
cutsets (And n es)  = map (And n) (mapM cutsets es) -- cartesian product
cutsets (Or _ es)   = concatMap cutsets es
cutsets (Counter n a d) = undefined

flatten :: ADTree a -> [ADTree a]
flatten c@Basic{}         = [c]
flatten c@(And _ cs)      = c : concatMap flatten cs
flatten c@(Or _ cs)       = c : concatMap flatten cs
flatten c@(Counter _ a d) = c : flatten a ++ flatten d

-- ------- --
-- Algebra --
-- ------- --

data PSemantics a = MkPSemantics
    { plus    :: a -> a -> a
    , zero    :: a
    , times   :: a -> a -> a
    , one     :: a
    , counter :: a -> a -> a
    }

type Semantics a = Player -> PSemantics a

aggregate :: Semantics a -> Player -> ADTree a -> a
aggregate _   _ (Basic _ a)     = a
aggregate sem p (Or _ cs)       = foldr (plus (sem p) . aggregate sem p) (zero $ sem p) cs
aggregate sem p (And _ cs)      = foldr (times (sem p) . aggregate sem p) (one $ sem p) cs
aggregate sem p (Counter _ a d) = counter (sem p) (aggregate sem p a) (aggregate sem (switchPlayer p) d)

-- --------- --
-- Rendering --
-- --------- --

dot :: (Eq a) => Player -> (a -> String) -> ADTree a -> String
dot pl fs r = unlines
    [ "digraph {"
    , "\trankdir=BT"
    , "\tnode [style=\"bold,rounded\"]"
    , nodes pl r
    , edges r
    , "}"
    ]
    where
        ids = [ (e', "node" ++ show i) | (e', i) <- zip (flatten r) [0 :: Int ..] ]
        eventId e = fromJust $ lookup e ids

        color A = "#ff0000"
        color D = "#00ff00"

        nodes p e@(Basic n a)     = printf "\t%s [label=<%s <br/> <FONT POINT-SIZE=\"10\">%s</FONT>>,color=\"%s\",shape=box]" (eventId e) n (fs a) (color p)
        nodes p e@(And n cs)      = unlines $ printf "\t%s [label=<%s <br/> AND>,color=\"%s\",shape=box]\n" (eventId e) n (color p) : map (nodes p) cs
        nodes p e@(Or n cs)       = unlines $ printf "\t%s [label=<%s <br/> OR>,color=\"%s\",shape=box]\n" (eventId e) n (color p) : map (nodes p) cs
        nodes p e@(Counter n a d) = printf "\t%s [label=<%s <br/> COUNTER>,color=\"%s\",shape=diamond]\n" (eventId e) n (color p) ++ nodes p a ++ nodes (switchPlayer p) d

        edges Basic{} = ""
        edges e@(And _ cs)  = unlines $ [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- cs ] ++ map edges cs
        edges e@(Or _ cs)   = unlines $ [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- cs ] ++ map edges cs
        edges e@(Counter _ a d) = printf "\t%s -> %s" (eventId a) (eventId e) ++ printf "\t%s -> %s" (eventId d) (eventId e) ++ edges a ++ edges d


-- ---------------- --
-- Example algebras --
-- ---------------- --

probability :: Semantics Rational
probability _ = MkPSemantics
    { plus    = (+)
    , zero    = 0
    , times   = (*)
    , one     = 1
    , counter = (-)
    }

difficulty :: Semantics Rational
difficulty A = MkPSemantics
    { plus    = min
    , zero    = 1
    , times   = max
    , one     = 0
    , counter = max
    }
difficulty D = MkPSemantics
    { plus    = max
    , zero    = 0
    , times   = min
    , one     = 1
    , counter = min
    }

-- TODO: Cost
