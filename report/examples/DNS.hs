import Text.Printf (printf)
import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)

import Language.ADTrees


example :: ADTree ()
example =
    Or "attack DNS hosting" [
        Or "host platform threads" [
            Counter "OS"
                (Or "" [
                    Basic "vulnerate OS" (),
                    Basic "corrupt system config" ()])
                (And "" [
                    Basic "patch OS" (),
                    Basic "follow NIST's recomm" ()]),
            Counter "DNS server"
                (Or "" [
                    Basic "vulnerate DNS server" (),
                    Basic "corrupt DNS config" ()])
                (And "" [
                    Basic "run latest" (),
                    Basic "keep config updated" (),
                    Basic "review vulnerabilities" (),
                    Basic "turn off Version Query" (),
                    Basic "run with basic privileges" (),
                    Basic "run isolated" (),
                    Basic "do not recurse" ()]),
            Counter "availability"
                (Or "DoS" [
                    Basic "external DoS" (),
                    Basic "ARP spoofing" ()])
                (Basic "network and geographic dispersion" ()),
            Counter "integrity"
                (Basic "MiTM on LAN" ())
                (Basic "DNSSEC" ())],
        Counter "DNS data threats" 
            (Or "" [
                Basic "Lame delegation" (),
                Basic "Zone drift and zone thrash" (),
                Basic "Leak information" ()])
            (And "" [
                Basic "Appropriate refresh value" (),
                Basic "Appropriate retry value" (),
                Basic "Appropriate expire value" (),
                Basic "Appropriate TTL value" (),
                Basic "Review of TXT RRs" (),
                Basic "Integrity check zone file" ()])]

main :: IO ()
main = do args <- getArgs
          writeFile (args !! 0) (dot (const " ") A example)
          pid <- runCommand $ printf "dot -Ngroup=A -Gratio=\"compress\" -Gdpi=300 -Gsize=\"11.7,8.3!\" -Glandscape=true -Tpng -v \"%s\" -o\"%s\"" (args !! 0) (args !! 1)
          waitForProcess pid
          return ()
