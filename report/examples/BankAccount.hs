import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


fsExampleAttribute :: (Rational, Rational) -> String
fsExampleAttribute (p, d) = printf "probability: %s <br/> difficulty: %s" (show p) (show d)

example :: ADTree (Rational, Rational)
example = 
    Or "Bank Account" [
        And "ATM" [
            Or "PIN" [
                Basic "Eavesdrop" (0.01, 0.3),
                Counter "Get Note"
                    (Basic "Find Note" (0.40, 0.3))
                    (Basic "Memorize" (0.80, 0.1)),
                Basic "Force" (0.01, 0.9)],
            Basic "Card" (0.1, 0.7)],
        Counter "Online"
            (And "Login" [
                Or "Password" [
                    Basic "Phishing" (0.90, 0.4),
                    Basic "Key logger" (0.20, 0.6)],
                Basic "User name" (0.90, 0.1)])
            (Counter "2nd Auth Factor"
                (Or "Usage" [
                    Basic "Key Fobs" (0.01, 0.1),
                    Basic "PIN Pad" (0.01, 0.3)])
                (Or "Malware" [
                    Basic "Browser" (0.20, 0.5),
                    Basic "OS" (0.20, 0.4)]))]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot A fsExampleAttribute example)
          pid <- runCommand $ printf "dot -Tpng -v %s -o%s" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
