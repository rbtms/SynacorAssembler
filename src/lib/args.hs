module ArgumentParser where

import System.Environment
import System.Exit (exitSuccess)

data Arguments = Arguments {
    _src_path :: [Char]
  , _out_path :: [Char]
  , _v        :: Bool
} deriving Show

usage :: [Char]
usage = "\n\
\  Usage:\n\n\ 
\  --src : Path of the source file\n\
\  --out : Path where the compiled binary will be stored\n\
\  --v   : Verbose flag\n"

parse_args :: [String] -> Arguments -> IO Arguments
parse_args [] args = return args
parse_args (str:strs) args
  | str == "--src" = parse_args (tail strs) $ args { _src_path = head strs }
  | str == "--out" = parse_args (tail strs) $ args { _out_path = head strs }
  | str == "--v"   = parse_args strs        $ args { _v        = True }
  | otherwise      = do
    putStrLn usage
    exitSuccess

check_args :: Arguments -> IO Arguments
check_args args
  | _src_path args == "" || _out_path args == "" = do
    putStrLn usage
    exitSuccess
  | otherwise = return args


get_args :: IO Arguments
get_args = do
  args  <- getArgs
  (parse_args args empty_args) >>= check_args >>= return
  where empty_args = Arguments { _src_path = "", _out_path = "", _v = False }

