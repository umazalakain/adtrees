import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: ADTree ()
example =
    Or "Attack DNS hosting" [
        Or "Host platform threads" [
            Counter "OS"
                (Or "Attack OS" [
                    Basic "Vulnerate OS" (),
                    Basic "Corrupt system config" ()])
                (And "Protect OS" [
                    Basic "Patch OS" (),
                    Basic "Follow NIST's recomm" ()]),
            Counter "DNS server"
                (Or "Attack DNS server" [
                    Basic "Vulnerate DNS server" (),
                    Basic "Corrupt DNS config" ()])
                (And "Protect DNS server" [
                    Basic "Run latest" (),
                    Basic "Keep config updated" (),
                    Basic "Review vulnerabilities" (),
                    Basic "Turn off Version Query" (),
                    Basic "Run with basic privileges" (),
                    Basic "Run isolated" (),
                    Basic "Do not recurse" ()]),
            Basic "DoS" (),
            Basic "ARP spoofing" (),
            Basic "Attack system config" (),
            Basic "Attack DNS config" (),
            Basic "MiTM on LAN" ()],
        Or "DNS data threats" [
            Basic "Lame delegation" (),
            Basic "Zone drift and zone thrash" (),
            Basic "Leak information" ()]]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot (const " ") A example)
          pid <- runCommand $ printf "dot -Gratio=\"fill\" -Gsize=\"11.7,8.3!\" -Glandscape=true -Tpng -v \"%s\" -o\"%s\"" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
