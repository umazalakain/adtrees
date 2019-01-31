module Language.ADTrees where

import Data.List (sortOn)

-- TODO: mention https://github.com/tomahawkins/fault-tree

type Name = String
type Probability = Rational

data Event
    = Leaf Name Probability
    | Branch Name Event
    | Not Event
    | And [Event]
    | Or [Event]
    deriving (Show, Eq)

probability :: Event -> Probability
probability (Leaf _ p) = p
probability (Branch _ e) = probability e
probability (Not e) = 1 - probability e
probability (And es) = product (map probability es)
probability (Or es) = sum (map probability es)

cutsets :: Event -> [Event]
cutsets (Leaf n p) = [Leaf n p]
cutsets (Branch n e) = map (Branch n) (cutsets e)
cutsets (Not e) = map Not (cutsets e)
cutsets (And es) = map And (mapM cutsets es) -- cartesian product
cutsets (Or es) = concatMap cutsets es

byProbability :: [Event] -> [Event]
byProbability = sortOn probability

cutoffAcceptable :: Rational -> [Event] -> [Event]
cutoffAcceptable p = filter ((<= p) . probability)

example :: Event
example = Branch "Enemy forges package" (Or [ 
    Branch "Enemy signs forged package" (
        Branch "Enemy has secret key" (
            Or [
                Leaf "Enemy has stolen secret key" 0.2,
                Branch "Enemy has calculated secret key" (
                    Or [
                        Leaf "Poor key generation allows secret key to be easily derived" 0.1,
                        Leaf "Unknown vulnerability in hash algorithm is exploited" 0.1,
                        Branch "Enemy uses known attack on weak hash algorithm" (
                            And [
                                Leaf "Enemy applies known attack to hash algorithm" 0.5,
                                Leaf "Poor choice of hash algorithm" 0.1
                            ]
                        )
                    ]
                )
            ]
        )
    ),
    Branch "Enemy replaces genuine archive with malicious archive" (
        Branch "Enemy has constructed malicious archive with same hash as genuine archive" (
            Branch "Enemy has effectively broken hash algorithm" (
                Or [
                    Leaf "Unknown vulnerability in hash algorithm is exploited" 0.1,
                    Branch "Enemy uses known attack on weak hash algorithm" (
                        And [
                            Leaf "Enemy applies known attack to hash algorithm" 1.0,
                            Leaf "Poor choice of hash algorithm" 0.1
                        ]
                    )
                ]
            )
        )
    )
    ])
