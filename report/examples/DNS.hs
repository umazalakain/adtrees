import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: ADTree ()
example =
    Or "DNS"
        [ Or "data corruption"
            [ Or "repository corruption"
                [ Counter "outdated information"
                    ( Basic "(D)DoS on hidden master" () )
                    ( Or ""
                        [ Basic "out-of-band replication" ()
                        , Basic "tuning of SOA expiration parameters" ()
                        ]
                    )
                , Counter "modified information"
                    ( Or ""
                        [ Basic "master compromised" ()
                        , Basic "secondary compromised" ()
                        , Basic "social engineering" ()
                        ]
                    )
                    ( And ""
                        [ Basic "harden master" ()
                        , Basic "establish SLA and OLA with secondary operator" ()
                        , Basic "secure procedures and education" ()
                        ]
                    )
                , Or "domain name hijacking"
                    [ Basic "typosquatting" ()
                    , Basic "IDN abuse" ()
                    ]
                ]
            ]
        , Or "privacy"
            [ Basic "cache snooping" ()
            , Basic "NSEC walk" ()
            ]
        , Or "denial of service"
            [ Or "DNS servers"
                [ Counter "system/application crash"
                    ( Basic "specially crafted packet" () )
                    ( Basic "diversity OS and DNS server" () )
                , Counter "resource starvation"
                    ( Basic "(D)DoS attack" () )
                    ( Or ""
                        [ Basic "system and network overprovisioning" ()
                        , Basic "deploy unicast" ()
                        ]
                    )
                ]
            ]
        ]


main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot (const " ") A example)
          pid <- runCommand $ printf "dot Gratio=\"compress\" -Gdpi=300 -Gsize=\"8.3,11.7!\" -Tpng -v \"%s\" -o\"%s\"" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
