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

import Data.List (lookup, partition)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- --------- --
-- Structure --
-- --------- --

type Name = String

data Player = A | D deriving (Show, Eq)

data ADTree a 
    = Basic Player Name a
    | And   Player Name [ADTree a]
    | Or    Player Name [ADTree a]
    deriving (Show, Eq)

switchPlayer :: Player -> Player
switchPlayer A = D
switchPlayer D = A

getPlayer :: ADTree a -> Player
getPlayer (Basic p _ _) = p
getPlayer (And p _ _) = p
getPlayer (Or p _ _) = p

partitionPlayer :: [ADTree a] -> ([ADTree a], [ADTree a])
partitionPlayer = partition ((== A) . getPlayer)

-- TODO: check this makes sense
cutsets :: ADTree a -> [ADTree a]
cutsets (Basic p n a) = [Basic p n a]
cutsets (And p n es) = map (And p n) (mapM cutsets es) -- cartesian product
cutsets (Or _ _ es) = concatMap cutsets es

flatten :: ADTree a -> [ADTree a]
flatten e@Basic{}      = [e]
flatten e@(And _ _ es) = e : concatMap flatten es
flatten e@(Or _ _ es)  = e : concatMap flatten es

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

aggPlus :: Semantics a -> Player -> [ADTree a] -> a
aggPlus sem p = foldr (plus (sem p) . aggregate sem) (zero $ sem p)

aggTimes :: Semantics a -> Player -> [ADTree a] -> a
aggTimes sem p = foldr (times (sem p) . aggregate sem) (one $ sem p)

aggregate :: Semantics a -> ADTree a -> a
aggregate _   (Basic _ _ a) = a
aggregate sem (Or p _ cs)   = let (as, ds) = partitionPlayer cs
                               in counter (sem p) (aggPlus sem p as) (aggPlus sem (switchPlayer p) ds)
aggregate sem (And p _ cs)  = let (as, ds) = partitionPlayer cs
                               in counter (sem p) (aggTimes sem p as) (aggTimes sem (switchPlayer p) ds)

-- --------- --
-- Rendering --
-- --------- --

dot :: (Eq a) => (a -> String) -> ADTree a -> String
dot fs r = unlines
    [ "digraph {"
    , "\trankdir=BT"
    , "\tnode [style=\"bold,rounded\"]"
    , unlines $ map node (flatten r)
    , unlines $ map edge (flatten r)
    , "}"
    ]
    where
        ids = [ (e', "ADTree_" ++ show i) | (e', i) <- zip (flatten r) [0 :: Int ..] ]
        eventId e = fromJust $ lookup e ids

        color A = "#ff0000"
        color D = "#00ff00"

        -- TODO: add player colors
        -- TODO: add attributes
        -- TODO: add or/and fanciness
        node e@(Basic p n a) = printf "\t%s [label=<%s <br/> <FONT POINT-SIZE=\"10\">%s</FONT>>,color=\"%s\",shape=box]" (eventId e) n (fs a) (color p)
        node e@(And p n _)   = printf "\t%s [label=<%s <br/> AND>,color=\"%s\",shape=box]" (eventId e) n (color p)
        node e@(Or p n _)    = printf "\t%s [label=<%s <br/> OR>,color=\"%s\",shape=box]" (eventId e) n (color p)

        edge Basic{} = ""
        edge e@(And _ _ es)  = unlines [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- es ]
        edge e@(Or _ _ es)   = unlines [ printf "\t%s -> %s" (eventId e') (eventId e) | e' <- es ]


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
