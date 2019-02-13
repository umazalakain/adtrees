import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


fsExampleAttribute :: (Rational, Rational) -> String
fsExampleAttribute (p, d) = printf "probability: %s <br/> difficulty: %s" (show p) (show d)

example :: Event (Rational, Rational)
example = 
    Or A "Bank Account" [
        And A "ATM" [
            Or A "PIN" [
                Basic A "Eavesdrop" (0.01, 0),
                And A "Find Note" [
                    Basic D "Memorize" (0.80, 0)],
                Basic A "Force" (0.01, 1)],
            Basic A "Card" (0.1, 1)],
        And A "Online" [
            Or A "Password" [
                Basic A "Phishing" (0.90, 0),
                Basic A "Key logger" (0.20, 0.5)],
            Basic A "User name" (0.90, 0),
            Or D "2nd Auth Factor" [
                Basic D "Key Fobs" (0.01, 0),
                Basic D "PIN Pad" (0.01, 0),
                Or A "Malware" [
                    Basic A "Browser" (0.20, 0.5),
                    Basic A "OS" (0.20, 0.5)]]]]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot fsExampleAttribute example)
          pid <- runCommand $ printf "dot -Tpng -v %s -o%s" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
