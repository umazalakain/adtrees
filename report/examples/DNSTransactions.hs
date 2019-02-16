import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: Event ()
example =
    Or A "DNS Transactions" [
        Or A "Query/Response Threats" [
            Or A "Forged response" [
                Or A "Compromised server" [
                    Basic A "OS compromised" (),
                    Basic A "Communication stack compromised" ()],
                Or A "Caching server compromised" [
                    Basic A "Packet interception" (),
                    Basic A "Query prediction" ()]],
            Or A "Removal of RRs" [],
            Or A "Incorrect expansion" []
        ],
        Or A "Zone Transfer Threats" [
            Or A "Denial of Service" [],
            Or A "Tampered transfer message" []
        ],
        Or A "Dynamic Updates Threats" [
            Or A "Unauthorized updates" [],
            Or A "Tampered update request" [],
            Or A "Replay attacks" []
        ],
        Or A "DNS NOTIFY Threats" [
            Or A "Spurious NOTIFY" []
        ]
    ]

        

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot (const " ") example)
          pid <- runCommand $ printf "dot -Tpng -v %s -o%s" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
