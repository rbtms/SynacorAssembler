module Main where

import Data.Word (Word16)
import Data.Char (isLower, isAlpha, isNumber, ord, toLower)
import Data.List (all)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary.Put as P
import qualified Data.List.Split as S
import System.IO
import Debug.Trace


-- |
-- | Datatypes
-- |

data ConstType = NumType Int | ChrType Char | StrType [Char]
  deriving Show

data Register = A | B | C | D | E | F | G | H
  deriving Show

data Token =
    T_Section     [Char]
  | T_Identifier  [Char]
  | T_Number      Int
  | T_Chr         Char
  | T_Register    Char
  | T_Tag         [Char]
  | T_Opcode      [Char]
  | T_Type        [Char]
  | T_Comma
  | T_Newline
  deriving (Eq, Show)

data TokenInfo = TokenInfo { t_line :: Int, t_token :: Token }
  deriving (Eq, Show)

data Section = Section { s_name :: [Char], s_line :: Int, s_tokens :: [TokenInfo] }
  deriving Show

data Const =
    C_Num { c_name :: [Char], c_num :: Int   }
  deriving (Eq, Show)

data Tag = Tag { _jmp_line :: Int, _name :: [Char] }
  deriving Show

-- data Opcode =
--     Halt
--   | Set 


-- |
-- | Types
-- |

type Identifier = [Char]

-- |
-- | Constants
-- |

_MAX_NUM :: Int
_MAX_NUM = 32768

_TOKEN_COMMENT :: Char
_TOKEN_COMMENT = ';'

_TOKEN_TYPES = ["CHR", "NUM"]

_TOKEN_OPCODES :: [String]
_TOKEN_OPCODES = [ "HALT", "SET", "PUSH", "POP", "EQ", "GT", "JMP", "JT", "JF"
                 , "ADD", "MULT", "MOD", "AND", "OR", "NOT", "RMEM", "WMEM"
                 , "CALL", "RET", "OUT", "IN", "NOOP" 
                 ]

_TOKEN_SECTIONS :: [String]
_TOKEN_SECTIONS = ["data", "code"]

-- |
-- | Boolean checking functions
-- |

-- is_opcode, is_identifier, is_chr, is_register :: [Char] -> Bool
-- is_number, is_section, is_tag, is_type        :: [Char] -> Bool

-- | Tokens

is_opcode     str = elem str _TOKEN_OPCODES
is_identifier str = isLower (head str)
  && all (\c -> isLower c || isNumber c) (tail str)
is_chr        str = length str == 3
  && head str == '\''
  && last str == '\''
  && is_identifier [str!!1]
is_register   str = length str == 1
  && elem (str!!0) "ABCDEFGH"
is_comma      str = str == ","
is_number     str = (isNumber . head $ str) || (head str == '-') && all isNumber (tail str)
is_section    str = head str == '.' && is_identifier (tail str)
is_tag        str = last str == ':' && is_identifier (init str)
is_type       str = elem str _TOKEN_TYPES

-- | Values

is_reg :: Int -> Bool
is_reg   n = n > 32767 && n < 32775
is_value :: Int -> Bool
is_value n = n >= 0 && n < 32776


to_int :: [Char] -> Int
to_int n = read n :: Int

-- |
-- | Error throwing
-- |

--throw_err :: [Char] -> Int -> ()
throw_err err line = error $ "Error: " ++ err ++ " at line " ++ (show line)

throw_invalid_token t = throw_err ("Invalid token: " ++ (show . t_token $ t)) (t_line t)


-- |
-- | First pass - Tokenizer
-- |

strip_comments :: String -> String
strip_comments [] = []
strip_comments (c:cs)
  | c == _TOKEN_COMMENT = []
  | otherwise           = c : strip_comments cs

tokenize :: [[TokenInfo]] -> [String] -> Int -> [TokenInfo]
tokenize acc [] _ = concat . reverse $ acc
tokenize acc (line:lines) line_n
  -- | Line is empty or with only comments
  | null line || null line' = tokenize acc lines (line_n+1)
  -- | Line has tokens
  | otherwise = tokenize ((tokens++[TokenInfo line_n T_Newline]):acc) lines (line_n+1)
        -- | Strip comments
  where line'     = strip_comments line
        -- | Split by space
        -- | -> Separate by comma
        -- | -> Map to tokens
        -- | -> TokenInfo
        -- | -> Concat into a stream of tokens
        tokens    = concat . map (map (TokenInfo line_n . to_token) . sep_comma) $ words line'
        sep_comma = S.split (S.dropBlanks $ S.oneOf ",")
        to_token str
          | is_section    str = T_Section    (tail str)
          | is_register   str = T_Register   (head str)
          | is_opcode     str = T_Opcode     str
          | is_type       str = T_Type       str
          | is_tag        str = T_Tag        (init str)
          | is_number     str = T_Number     (read str :: Int)
          | is_chr        str = T_Chr (str!!1)
          | is_comma      str = T_Comma
          | is_identifier str = T_Identifier str
          | otherwise         = throw_err ("Invalid token \"" ++ str ++ "\"") line_n

-- |
-- | Second pass - Sections
-- |

check_sections sections
  | not . has_section "data" $ sections = throw_err "Lacking data section" (-1)
  | not . has_section "code" $ sections = throw_err "Lacking code section" (-1)
  | length sections /= 2 = throw_err "Invalid number of sections (unimplemented)" (-1)
  | otherwise            = sections
  where has_section name sections = any (\s -> s_name s == name) $ sections

split_sections :: [TokenInfo] -> [Section]
split_sections = map to_section . split_sections'
  where split_sections' = S.split (S.dropBlanks $ S.keepDelimsL $ S.whenElt (is_section_token . t_token))
        to_section tokens = case t_token . head $ tokens of
          T_Section name -> Section {
                             s_name   = name
                           , s_line   = (t_line . head $ tokens)
                           -- | Drop following Newline
                           , s_tokens = drop 2 tokens
                         }
          _         -> throw_err "Token outside of section" (t_line . head $ tokens)
        is_section_token (T_Section _) = True
        is_section_token _             = False
  
-- |
-- | Third pass - Parse sections
-- |

parse_num :: Int -> Int
parse_num n
  | n >= 0 = mod n _MAX_NUM
  | n < 0  = parse_num $ n + _MAX_NUM

filter_tags :: [TokenInfo] -> [TokenInfo]
filter_tags = filter (is_tag . t_token)
  where is_tag (T_Tag name) = True
        is_tag _            = False

-- | .data

tag_name :: TokenInfo -> [Char]
tag_name t = case t_token t of
  T_Tag name -> name
  _          -> throw_err "Invalid tag in const checking" (t_line t)

check_consts :: [Const] -> [TokenInfo] -> [Const]
check_consts [] _ = []
check_consts (c:consts) tags
  | any (\c' -> c_name c == c_name c') consts =
    throw_err ("Redeclared constant: " ++ (c_name c)) (-1)
  | any (\t -> is_tag t c) tags =
    throw_err ("Ambiguous constant: " ++ (c_name c)) (-1)
  | otherwise = c : check_consts consts tags
  where is_tag t c = c_name c == tag_name t

check_entry_point:: [TokenInfo] -> [TokenInfo]
check_entry_point tags
  | not . any (\t -> tag_name t == "main") $ tags = throw_err "Entry point not defined" (-1)
  | otherwise = tags

check_tags :: [TokenInfo] -> [TokenInfo]
check_tags [] = []
check_tags (t:tags)
  | any (\t' -> tag_name t == tag_name t') tags = throw_err "Redeclarated tag" (t_line t)
  | otherwise   = t : check_tags tags

parse_s_data :: [TokenInfo] -> [Const]
parse_s_data ts = d_identifier ts
  where d_identifier []     = []
        d_identifier (t:ts) = case t_token t of
          T_Identifier t_name' -> d_type ts t_name'
          _                    -> throw_err "Invalid token" (t_line t)
        d_type (t:ts) t_name' = case t_token t of
          T_Type t_type' -> d_value ts t_name' t_type'
          _              -> throw_err "Invalid token" (t_line t)
        d_value (t:ts) t_name' t_type' = case t_token t of
          T_Number n -> if t_type' == "NUM"
            then (C_Num t_name' $ parse_num n) : d_newline ts
            else throw_err "Invalid constant type" (t_line t)
          T_Chr    c -> if t_type' == "CHR"
            then (C_Num t_name' $ ord c) : d_newline ts
            else throw_err "Invalid constant type" (t_line t)
          _          -> throw_err "Invalid token" (t_line t)
        d_newline (t:ts) = case t_token t of
          T_Newline -> d_identifier ts
          _         -> throw_err "Invalid token" (t_line t)

-- | .code

resolve_identifier :: [Const] -> [TokenInfo] -> [Char] -> Int -> Token
resolve_identifier const' tags name line_n
  | null c_matches && null tag_matches =
    throw_err ("Constant or tag not declared: " ++ name) line_n
  | null c_matches = T_Tag name
  | otherwise = case head c_matches of
    C_Num _ n  -> T_Number n
  where c_matches   = filter (\c -> c_name c == name) const'
        tag_matches = filter (\t -> tag_name t == name) tags

fix_s_code :: [Const] -> [TokenInfo] -> [TokenInfo] -> [TokenInfo]
fix_s_code _      _    [] = []
fix_s_code const' tags (t:ts) = t' : fix_s_code const' tags ts
  where resolve_reg r = 32671 + (ord . toLower $ r)
        t' = TokenInfo (t_line t) $ case t_token t of
          T_Number n        -> T_Number . parse_num $ n
          T_Chr c           -> T_Number . ord $ c
          T_Register r      -> T_Number . resolve_reg $ r
          T_Identifier name -> resolve_identifier const' tags name (t_line t)
          _                 -> t_token t

-- |
-- | Fourth pass - Tags
-- |
-- | (Opcode, Number of arguments)
instr_info :: [Char] -> (Int, [String])
instr_info op = case op of
  "HALT"   -> (0x00, [])
  "SET"    -> (0x01, ["REG", "VAL"])
  "PUSH"   -> (0x02, ["VAL"])
  "POP"    -> (0x03, ["REG"])
  "EQ"     -> (0x04, ["REG", "VAL", "VAL"])
  "GT"     -> (0x05, ["REG", "VAL", "VAL"])
  "JMP"    -> (0x06, ["TAG"])
  "JT"     -> (0x07, ["VAL", "TAG"])
  "JF"     -> (0x08, ["VAL", "TAG"])
  "ADD"    -> (0x09, ["REG", "VAL", "VAL"])
  "MULT"   -> (0x0A, ["REG", "VAL", "VAL"])
  "MOD"    -> (0x0B, ["REG", "VAL", "VAL"])
  "AND"    -> (0x0C, ["REG", "VAL", "VAL"])
  "OR"     -> (0x0D, ["REG", "VAL", "VAL"])
  "NOT"    -> (0x0E, ["REG", "VAL"])
  "RMEM"   -> (0x0F, ["REG", "VAL"])
  "WMEM"   -> (0x10, ["VAL", "REG"])
  "CALL"   -> (0x11, ["TAG"])
  "RET"    -> (0x12, [])
  "OUT"    -> (0x13, ["VAL"])
  "IN"     -> (0x14, ["REG"])
  "NOOP"   -> (0x15, [])
  _        -> throw_err "Invalid opcode" (-1)

{-
fourth_pass :: [TokenInfo] -> Int -> Tags
fourth_pass (t:ts) ptr = case t_token t of
  T_Tag    name -> (Tag ptr name) : fourth_pass ts ptr
  T_Opcode name -> case name of
    "HALT" -> 0x00 : f_newline ts
    "SET"  -> 0x01 : f_arg ts 2 (ptr+3)
  where f_arg ts arg_n ptr
          | arg_n == 1 = 
          | otherwise  =
        f_newline (t:ts) = case t of
          T_Newline -> fourth_pass ts ptr

-}
-- |
-- | Fifth pass - Opcode parsing
-- |

is_num n = n >= 0 && n < 32768

res_raw t tags = case t_token t of
  T_Number n -> n
  T_Tag name -> 0 --_jmp_line . head . filter (\tag -> _name tag == name) $ tags

fifth_pass :: [TokenInfo] -> [Tag] -> [Int]
fifth_pass [] _ = []
fifth_pass (t:ts) tags = case t_token t of
  T_Tag    name -> fifth_pass ts tags
  T_Opcode name -> if null args_t
    then f_newline ts args_t
    else op : f_arg ts args_t
    where (op, args_t) = instr_info name
  T_Newline     -> fifth_pass ts tags
  _             -> throw_invalid_token t
  where -- | Argument
        f_arg ts [] = throw_err "f_arg: No more tokens" (-1)
        f_arg (t:ts) (arg_t:args_t) = case arg_t of
          -- | 32767 < n < 32776
          "REG" -> if is_reg n then n : f_endarg ts args_t else throw_err "Not a register" line
          -- | Number literal or register
          "VAL" -> n : f_endarg ts args_t
          -- | No current restriction
          "TAG" -> n : f_endarg ts args_t
          where n = res_raw t tags
                line = t_line t
        -- | End of the argument
        f_endarg (t:ts) args_t = case t_token t of
          T_Comma   -> f_arg ts args_t
          T_Newline -> f_newline (t:ts) args_t
          _         -> throw_invalid_token t
        f_newline (t:ts) args_t = if null args_t
            -- | All arguments are valid
            then fifth_pass ts tags
            -- | There are arguments not resolved
            else throw_err "Not enough arguments" (t_line t)

main :: IO ()
main = do
  src <- openFile "src/test.txt" ReadMode >>= hGetContents
  putStrLn src

  -- | Tokenizer
  
  let tokens = tokenize [] (lines src) 1
  putStrLn "First pass - Tokenize"
  mapM_ putStrLn $ map (("    "++) . show) tokens

  -- | Section
  
  let sections = split_sections tokens
  putStrLn "\nSecond pass - Split into sections"
  mapM_ putStrLn $ map (\s ->
       (s_name s ++ " (Line " ++ (show . s_line $ s) ++ ")\n")
    ++ (concat . map (("    "++) . (++"\n") . show) . s_tokens $ s)
    ) sections

  -- | Section parsing
  
  let s_data = s_tokens . head . filter ((=="data") . s_name) $ sections
  let s_code = s_tokens . head . filter ((=="code") . s_name) $ sections
  let tags   = check_tags . check_entry_point . filter_tags $ s_code

  let consts  = check_consts (parse_s_data s_data) tags
  let s_code' = fix_s_code consts tags s_code

  putStrLn "\nThird pass - Section parsing"
  putStrLn "Consts"
  mapM_ print consts
  putStrLn "Tags"
  mapM_ print tags
  putStrLn "Fix .code"
  mapM_ putStr $ map (("    "++) . (++"\n") . show) $ s_code'
  -- mapM_ print s_code

  -- | Fourth pass

  -- | Fifth pass
  putStrLn "\nFifth pass - Bytecode"
  let bytecode = fifth_pass s_code' []
  print bytecode

  BS.writeFile "bin/test.bin" $ P.runPut $ mapM_ P.putWord16le $ map (\n -> fromIntegral n :: Word16) bytecode

