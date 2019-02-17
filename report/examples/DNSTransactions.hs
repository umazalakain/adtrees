import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: ADTree ()
example =
    Or "DNS Transactions" [
        Or "Query/Response Threats" [
            Or "Forged response" [
                Or "Compromised server" [
                    Basic "OS compromised" (),
                    Basic "Communication stack compromised" ()],
                Or "Caching server compromised" [
                    Basic "Packet interception" (),
                    Basic "Query prediction" ()]],
            Or "Removal of RRs" [],
            Or "Incorrect expansion" []
        ],
        Or "Zone Transfer Threats" [
            Or "Denial of Service" [],
            Or "Tampered transfer message" []
        ],
        Or "Dynamic Updates Threats" [
            Or "Unauthorized updates" [],
            Or "Tampered update request" [],
            Or "Replay attacks" []
        ],
        Or "DNS NOTIFY Threats" [
            Or "Spurious NOTIFY" []
        ]
    ]

        

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot A (const " ") example)
          pid <- runCommand $ printf "dot -Gsize=\"16.52,11.68\" -Gratio=\"fill\"  -Glandscape=false -Gsplines=ortho -Tpng -v \"%s\" -o\"%s\"" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
