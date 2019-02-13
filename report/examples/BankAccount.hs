import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees

type ExampleAttribute = (Rational, Difficulty)

fsExampleAttribute :: ExampleAttribute -> String
fsExampleAttribute (p, d) = printf "probability: %s <br/> difficulty: %s" (show p) (show d)

example :: Event ExampleAttribute
example = 
    Or A "Bank Account" [
        And A "ATM" [
            Or A "PIN" [
                Basic A "Eavesdrop" (0.01, L),
                And A "Find Note" [
                    Basic D "Memorize" (0.80, L)],
                Basic A "Force" (0.01, H)],
            Basic A "Card" (0.1, H)],
        And A "Online" [
            Or A "Password" [
                Basic A "Phishing" (0.90, L),
                Basic A "Key logger" (0.20, M)],
            Basic A "User name" (0.90, L),
            Or D "2nd Auth Factor" [
                Basic D "Key Fobs" (0.01, L),
                Basic D "PIN Pad" (0.01, L),
                Or A "Malware" [
                    Basic A "Browser" (0.20, M),
                    Basic A "OS" (0.20, M)]]]]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot fsExampleAttribute example)
          pid <- runCommand $ printf "dot -Tpng -v %s -o%s" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
