module Console where

import Grid
import Data.Char (digitToInt)
import System.IO

isvalidCommand :: String -> Bool
isvalidCommand [i,sep,j] = i <= 'i' && i >= 'a' && sep == ',' && j <= '9' && j >= '1' 
isvalidCommand _ = False

commandToPos :: String -> Position
commandToPos [i,_,j] = (fromEnum i - fromEnum 'a' + 1, digitToInt j) 

startRepl :: Grid -> IO ()
startRepl b = do { putStrLn $ formatBoard b;
                   putStr "> ";
                   hFlush stdout;
                   inp <- getLine;
                   putChar '\n';
                   if isvalidCommand inp then
  let { p = commandToPos inp;
        content = b .!. p }
in case content of {
        Mine -> (putStrLn $ formatBoard $ setQ b p OMine) >> putStrLn "Boom! But game is not over ......";
        (Close n) -> startRepl (setQ b p $ Open n);
        _ -> startRepl b }
  else (putStrLn $ inp ++ " is not a valid command, try again!") >> startRepl b }
                   
