import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees

data ExampleAttribute = MEA
    { getProbability :: Rational
    , getDifficulty  :: Difficulty
    } deriving (Eq)

fsExampleAttribute :: ExampleAttribute -> String
fsExampleAttribute a = printf "probability: %s <br/> difficulty: %s" (show $ getProbability a) (show $ getDifficulty a)

example :: Event ExampleAttribute
example = Or A "Bank Account" [
    And A "ATM" [
        Or A "PIN" [
            Basic A "Eavesdrop" $ MEA 0.01 L,
            And A "Find Note" [
                Basic D "Memorize" $ MEA 0.80 L],
            Basic A "Force" $ MEA 0.01 H],
        Basic A "Card" $ MEA 0.1 H],
    And A "Online" [
        Or A "Password" [
            Basic A "Phishing" $ MEA 0.90 L,
            Basic A "Key logger" $ MEA 0.20 M],
        Basic A "User name" $ MEA 0.90 L,
        Or D "2nd Auth Factor" [
            Basic D "Key Fobs" $ MEA 0.01 L,
            Basic D "PIN Pad" $ MEA 0.01 L,
            Or A "Malware" [
                Basic A "Browser" $ MEA 0.20 M,
                Basic A "OS" $ MEA 0.20 M]]]]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot fsExampleAttribute example)
          pid <- runCommand $ printf "dot -Tpng -v %s -o%s" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
