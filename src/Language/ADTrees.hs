{-# LANGUAGE DeriveFunctor #-}

module Language.ADTrees
    ( Player(..)
    , ADTree(..)
    , Semantics
    , PSemantics(..)
    , evaluate
    , cutsets
    , flatten
    , dot
    , probability
    , difficulty
    , cost
    , skill
    , timeParallel
    , timeSequence
    , satisfiability
    ) where

import Data.List (lookup, intercalate, words)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- --------- --
-- Structure --
-- --------- --

type Name = String

data ADTree a 
    = Basic   Name a
    | And     Name [ADTree a]
    | Or      Name [ADTree a]
    | Counter Name (ADTree a) (ADTree a)
    deriving (Show, Eq, Functor)

cutsets :: ADTree a -> [ADTree a]

cutsets (Basic n a) = [Basic n a]

-- One cutset from every children
-- Cartesian product of all combinations
cutsets (And n es) = map (And n) (mapM cutsets es)

-- Any cutset from any children
cutsets (Or n es) = map (Or n . (: [])) (concatMap cutsets es)

-- One cutset from the attacker, one from the deffender
-- Cartesian product of all combinations
cutsets (Counter n a d) = Counter n <$> cutsets a <*> cutsets d

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

data Player = A | D deriving (Show, Eq)

type Semantics a = Player -> PSemantics a

switchPlayer :: Player -> Player
switchPlayer A = D
switchPlayer D = A

evaluate :: Semantics a -> Player -> ADTree a -> a
evaluate _   _ (Basic _ a)     = a
evaluate sem p (Or _ cs)       = foldr (plus (sem p) . evaluate sem p) (zero $ sem p) cs
evaluate sem p (And _ cs)      = foldr (times (sem p) . evaluate sem p) (one $ sem p) cs
evaluate sem p (Counter _ a d) = counter (sem p) (evaluate sem p a) (evaluate sem (switchPlayer p) d)

-- --------- --
-- Rendering --
-- --------- --

dot :: (Eq a) => (a -> String) -> Player -> ADTree a -> String
dot fa pl r = unlines
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

        breaks = intercalate "<br/>" . words

        nodes p e@(Basic n a)     = printf "\t%s [label=<%s<br/><FONT POINT-SIZE=\"10\">%s</FONT>>,color=\"%s\",shape=box]" (eventId e) (breaks n) (fa a) (color p)
        nodes p e@(And n cs)      = unlines $ printf "\t%s [label=<%s>,color=\"%s\",shape=triangle]\n" (eventId e) (breaks n) (color p) : map (nodes p) cs
        nodes p e@(Or n cs)       = unlines $ printf "\t%s [label=<%s>,color=\"%s\",shape=invtriangle]\n" (eventId e) (breaks n) (color p) : map (nodes p) cs
        nodes p e@(Counter n a d) = printf "\t%s [label=<%s>,color=\"%s\",shape=diamond]\n" (eventId e) (breaks n) (color p) ++ nodes p a ++ nodes (switchPlayer p) d

        edges Basic{} = ""
        edges e@(And _ cs)  = unlines $ [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- cs ] ++ map edges cs
        edges e@(Or _ cs)   = unlines $ [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- cs ] ++ map edges cs
        edges e@(Counter _ a d) = printf "\t%s -> %s" (eventId a) (eventId e) ++ printf "\t%s -> %s" (eventId d) (eventId e) ++ edges a ++ edges d

-- ---------------- --
-- Example algebras --
-- ---------------- --

{-
   Probability of success, assuming all actions are mutually independent
-}
probability :: Semantics Rational
probability _ = MkPSemantics
    { plus    = \x y -> x + y - x * y
    , zero    = 0
    , times   = (*)
    , one     = 1
    , counter = (-)
    }

{-
   Difficulty for the attacker, assuming all attacker's actions are in place
-}
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

{-
   Severity for the defender, assuming all attacker's actions are in place
-}
severity :: Semantics Rational
severity _ = MkPSemantics
    { plus    = max
    , zero    = 0
    , times   = (+)
    , one     = 0
    , counter = (-)
    }

{-
   Minimal cost for the attacker, assuming that all attacker's actions are in
   place and that resources are not reused.
-}
cost :: Semantics Int
cost A = MkPSemantics
    { plus    = min
    , zero    = maxBound
    , times   = (+)
    , one     = 0
    , counter = (+)
    }
cost D = MkPSemantics
    { plus    = (+)
    , zero    = 0
    , times   = min
    , one     = maxBound
    , counter = min
    }

{-
   Minimal skill needed for the attacker, assuming that all attacker's actions
   are in place.
-}
skill :: Semantics Int
skill A = MkPSemantics
    { plus    = min
    , zero    = maxBound
    , times   = max
    , one     = minBound
    , counter = max
    }
skill D = MkPSemantics
    { plus    = max
    , zero    = minBound
    , times   = min
    , one     = minBound
    , counter = min
    }

{-
   Minimal time needed for the attacker, assuming that all attacker's actions
   are in place and that actions are executed in parallel.
-}
timeParallel :: Semantics Int
timeParallel A = MkPSemantics
    { plus    = min
    , zero    = maxBound
    , times   = max
    , one     = minBound
    , counter = max
    }
timeParallel D = MkPSemantics
    { plus    = max
    , zero    = minBound
    , times   = min
    , one     = minBound
    , counter = min
    }

{-
   Minimal time needed for the attacker, assuming that all attacker's actions
   are in place and that actions are executed in sequence.
-}
timeSequence :: Semantics Int
timeSequence A = MkPSemantics
    { plus    = min
    , zero    = maxBound
    , times   = (+)
    , one     = 0
    , counter = (+)
    }
timeSequence D = MkPSemantics
    { plus    = (+)
    , zero    = 0
    , times   = min
    , one     = minBound
    , counter = min
    }

{-
    Satisfiability of the scenario.
-}
satisfiability :: Semantics Bool
satisfiability _ = MkPSemantics
    { plus    = (||)
    , zero    = False
    , times   = (&&)
    , one     = True
    , counter = \x y -> x && not y
    }
