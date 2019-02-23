import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: ADTree (Rational, Rational)
example =
    Or "DNS"
        [ Or "data corruption"
            [ Or "repository corruption"
                [ Counter "outdated information"
                    ( Basic "(D)DoS on hidden master" (0.8, 0.8) )
                    ( Or ""
                        [ Basic "out-of-band replication" (0.6, 0.8)
                        , Basic "tuning of SOA expiration parameters" (0.2, 0.2)
                        ]
                    )
                , Or "modified information"
                    [ Counter ""
                        ( Basic "master compromised" (0.9, 1.0) )
                        ( Basic "harden master" (0.8, 0.6) )
                    , Counter ""
                        ( Basic "secondary compromised" (0.8, 0.8) )
                        ( Basic "establish SLA and OLA with secondary operator" (0.7, 0.5) )
                    , Counter ""
                        ( Basic "social engineering" (0.7, 0.7) )
                        ( Basic "secure procedures and education" (0.9, 0.5) )
                    ]
                , Or "domain name hijacking"
                    [ Basic "typosquatting" (0.1, 0.1)
                    , Basic "IDN abuse" (0.1, 0.1)
                    ]
                ]
            ]
        , Or "privacy"
            [ Basic "cache snooping" (0.4, 0.3)
            , Basic "NSEC walk" (0.3, 0.3)
            ]
        , Or "denial of service"
            [ Counter "system/application crash"
                ( Basic "specially crafted packet" (0.4, 0.7) )
                ( Basic "diversity OS and DNS server" (0.2, 0.5) )
            , Counter "resource starvation"
                ( Basic "(D)DoS attack" (0.6, 0.7) )
                ( Or ""
                    [ Basic "system and network overprovisioning" (0.7, 0.6)
                    , Basic "deploy anycast" (0.6, 0.8)
                    ]
                )
            ]
        ]

attrStr :: (Rational, Rational) -> String
attrStr (d, s) = printf "difficulty: %s<br/>severity: %s" (show d) (show s)

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot attrStr A example)
          pid <- runCommand $ printf "dot Gratio=\"compress\" -Gdpi=300 -Gsize=\"8.3,11.7!\" -Tpng -v \"%s\" -o\"%s\"" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
