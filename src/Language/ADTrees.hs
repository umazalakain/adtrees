module Language.ADTrees where

-- TODO: mention https://github.com/tomahawkins/fault-tree

type Name = String
data Player = Attacker | Defender

-- Domain for any given player
data Domain a b = MkDomain
    { get     :: a -> b
    , or      :: a -> b -> b
    , and     :: a -> b -> b
    , counter :: a -> b -> b
    }

-- Attack-defense domains
data ADDomain a b = MkADDomain (Domain a b) (Domain a b)

pd :: Player -> ADDomain a b -> Domain a b
pd Attacker (MkADDomain a _) = a
pd Defender (MkADDomain _ d) = d

data Event a 
    = Basic Name a
    | Comment Name (Event a)
    | Counter (Event a)
    | And [Event a]
    | Or [Event a]
    deriving (Show, Eq)

aggregate :: ADDomain a b -> Player -> Event a -> b
aggregate d p (Basic _ a) = get (pd p d) a
aggregate d p (Comment _ e) = aggregate d p e
aggregate d p (Counter e) = undefined
aggregate d p (And es) = undefined
aggregate d p (Or es) = undefined

cutsets :: Event a -> [Event a]
cutsets (Basic n p) = [Basic n p]
cutsets (Comment n e) = map (Comment n) (cutsets e)
cutsets (Counter e) = map Counter (cutsets e)
cutsets (And es) = map And (mapM cutsets es) -- cartesian product
cutsets (Or es) = concatMap cutsets es

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
