import Quantum.QProcessor.Script.Parser
import Quantum.QProcessor.Script.Interpreter
import System.Environment
import System.IO
import Text.Parsec

main :: IO ()
main = getArgs >>= route

route :: [String] -> IO ()
route [] = getContents >>= runScript
route [filename] = readFile filename >>= runScript
route _ = printHelp

runScript :: String -> IO ()
runScript script = do
  let result = runScriptParser (parser <* eof) script
  either (hPrint stderr) interpretIO result

printHelp :: IO ()
printHelp = do
  pname <- getProgName
  hPutStrLn stderr $ "usage: " ++ pname ++ " [file]"
