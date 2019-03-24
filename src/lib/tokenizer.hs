module Tokenizer where

import qualified Data.List.Split as S
import Data.Char (isLower, isAlpha, isNumber, ord, toLower)
import Debug.Trace

-- |
-- | Datatypes
-- |

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

-- |
-- | Constants
-- |

_TOKEN_REGISTERS :: [String]
_TOKEN_REGISTERS = ["A", "B", "C", "D", "E", "F", "G", "H"]

_TOKEN_COMMENT :: Char
_TOKEN_COMMENT = ';'

_TOKEN_TYPES = ["CHR", "NUM"]

_TOKEN_OPCODES :: [String]
_TOKEN_OPCODES = [ "HALT", "SET", "PUSH", "POP", "EQ", "GT", "JMP", "JT", "JF"
                 , "ADD", "MULT", "MOD", "AND", "OR", "NOT", "RMEM", "WMEM"
                 , "CALL", "RET", "OUT", "IN", "NOOP" 
                 ]

_TOKEN_SECTIONS :: [String]
_TOKEN_SECTIONS = [".data", ".code"]

_TOKEN_COMMA :: Char
_TOKEN_COMMA = ','

_TOKEN_EOL :: Char
_TOKEN_EOL = '\n'

-- |
-- | Boolean helpers
-- |

-- is_opcode, is_identifier, is_chr, is_register :: [Char] -> Bool
-- is_number, is_section, is_tag, is_type        :: [Char] -> Bool

is_opcode     str = elem str _TOKEN_OPCODES
is_identifier str = elem (head str) "_abcdefhijklmnopqrstuvwxyz"
  && all (\c -> c == '_' || isLower c || isNumber c) (tail str)
is_chr        str = length str == 3
  && head str == '\''
  && last str == '\''
is_escaped_chr str = length str == 4
  && head str == '\''
  && last str == '\''
  && str!!1   == '\\'
is_register   str = elem str _TOKEN_REGISTERS
is_comma          = (==[_TOKEN_COMMA])
is_number     str = (isNumber . head $ str) || (head str == '-') && all isNumber (tail str)
is_hex        str = length str > 2 && take 2 str == "0x" && all isNumber (drop 2 str)
is_section    str = elem str _TOKEN_SECTIONS
is_tag        str = last str == ':' && is_identifier (init str)
is_type       str = elem str _TOKEN_TYPES
is_newline        = (== [_TOKEN_EOL])

-- |
-- | Other helpers
-- |

unescape :: [Char] -> Char
unescape str = case str of
  "'\\n'" -> '\n'
  _       -> error $ "Escaped character not supported: " ++ str


-- |
-- | Tokenizer
-- |

strip_comments :: [Char] -> Bool -> [Char]
strip_comments [] _ = []
strip_comments (c:cs) is_comment
  | c == ';'   =     strip_comments cs True
  | c == '\n'  = c : strip_comments cs False
  | is_comment =     strip_comments cs is_comment
  | otherwise  = c : strip_comments cs is_comment

-- | Right now only parsing character quotes
split_tokens :: [(Int, String)] -> String -> Bool -> Int -> [(Int, [Char])]
split_tokens acc [] _ _ = reverse . map (\(l, cs) -> (l, reverse cs)) . filter (not . null . snd) $ acc
split_tokens acc (c:cs) is_quotes line_n
  | c == '\'' = split_tokens acc'        cs (not is_quotes) line_n
  | is_quotes = split_tokens acc'        cs is_quotes       line_n
  | c == '\n' = split_tokens newline_acc cs is_quotes      (line_n+1)
  | c == ' '  = split_tokens space_acc   cs is_quotes       line_n
  | otherwise = split_tokens acc'        cs is_quotes       line_n
  where (last_line, last_cs) = head acc
        acc'                 = (last_line, c:last_cs):(tail acc)
        newline_acc          = (line_n+1, ""):(line_n, "\n"):acc
        space_acc            = (line_n, ""):acc

tokenize :: [Char] -> [TokenInfo]
tokenize src = tokenize' tokens
  where src'   = strip_comments src False
        tokens = split_tokens [(0, [])] src' False 1

tokenize' :: [(Int, [Char])] -> [TokenInfo]
tokenize' = map (\t@(line_n, str) -> TokenInfo line_n $ to_token t) . concat . map sep_comma
    where sep_comma (line_n, str) = map (\str' -> (line_n, str')) . S.split (S.dropBlanks $ S.oneOf ",") $ str
          to_token (line_n, str)
            | is_section     str = T_Section    (tail str)
            | is_register    str = T_Register   (head str)
            | is_opcode      str = T_Opcode     str
            | is_type        str = T_Type       str
            | is_tag         str = T_Tag        (init str)
            | is_number      str = T_Number     (read str :: Int)
            -- | Dangerous? Resolving hex token as number
            | is_hex         str = T_Number     (read str :: Int)
            | is_chr         str = T_Chr        (str!!1)
            | is_escaped_chr str = T_Chr        (unescape str)
            | is_comma       str = T_Comma
            | is_identifier  str = T_Identifier str
            | is_newline     str = T_Newline
            | otherwise          = error ("Invalid token \"" ++ str ++ "\"") line_n

