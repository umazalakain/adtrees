{-# LANGUAGE DeriveFunctor #-}

module Language.ADTrees
    ( Player(..)
    , ADTree(..)
    , Semantics
    , PSemantics(..)
    , isAttacker
    , isBasic
    , evaluate
    , cutsets
    , flatten
    , dot
    , probability
    , difficulty
    , severity
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

-- | All possible attack-defense interactions that can lead to the root goal to
-- be achieved.
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

data Player = A | D deriving (Show, Eq)

-- | Whether the given player is 'A'
isAttacker :: Player -> Bool
isAttacker A = True
isAttacker D = False

-- | Whether the given 'ADTree' is 'Basic'
isBasic :: ADTree a -> Bool
isBasic (Basic _ _) = True
isBasic _           = False

-- | Flatten the tree structure, filtering on the 'Player' of the nodes
flatten :: (Player -> Bool) -- ^ Filter nodes according to their 'Player'
        -> Player           -- ^ Player at the root node of the tree
        -> ADTree a
        -> [ADTree a]

flatten f p c = (if f p then (c :) else id) (doFlatten f p c)
    where
        doFlatten f p c@Basic{}         = []
        doFlatten f p c@(And _ cs)      = concatMap (flatten f p) cs
        doFlatten f p c@(Or _ cs)       = concatMap (flatten f p) cs
        doFlatten f p c@(Counter _ a d) = flatten f p a ++ flatten f (switchPlayer p) d

-- ------- --
-- Algebra --
-- ------- --

-- | Semantics for the algebra for a given player
data PSemantics a = MkPSemantics
    { plus    :: a -> a -> a -- ^ 'Or' operation
    , zero    :: a           -- ^ 'Or' default
    , times   :: a -> a -> a -- ^ 'And' operation
    , one     :: a           -- ^ 'And' default
    , counter :: a -> a -> a -- ^ 'Counter' operation
    }

-- | Semantics for the algebra of each player
type Semantics a = Player -> PSemantics a

-- | Switch players
switchPlayer :: Player -> Player
switchPlayer A = D
switchPlayer D = A

-- | Evaluate an 'ADTree' according to the specified 'Semantics'
evaluate :: Semantics a -- ^ Evaluation algebra
         -> Player      -- ^ Player at the root node
         -> ADTree a
         -> a

evaluate _   _ (Basic _ a)     = a
evaluate sem p (Or _ cs)       = foldr (plus (sem p) . evaluate sem p) (zero $ sem p) cs
evaluate sem p (And _ cs)      = foldr (times (sem p) . evaluate sem p) (one $ sem p) cs
evaluate sem p (Counter _ a d) = counter (sem p) (evaluate sem p a) (evaluate sem (switchPlayer p) d)

-- --------- --
-- Rendering --
-- --------- --

-- | Output the Graphviz representation for an 'ADTree'
dot :: (Eq a)
    => (a -> String) -- ^ Rendering function for 'Basic' event attributes
                     -- ^ Rendered using Graphviz's support for HTML tags
    -> Player        -- ^ Player at the root node
    -> ADTree a
    -> String

dot fa pl r = unlines
    [ "digraph {"
    , "\trankdir=BT"
    , "\tnode [style=\"bold,rounded\"]"
    , nodes pl r
    , edges r
    , "}"
    ]
    where
        ids = [ (e', "node" ++ show i) | (e', i) <- zip (flatten (const True) pl r) [0 :: Int ..] ]
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
