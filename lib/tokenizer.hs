module Tokenizer where

import qualified Data.List.Split as S
import Data.Char (isLower, isAlpha, isNumber, ord, toLower)

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
-- | Boolean helpers
-- |

-- is_opcode, is_identifier, is_chr, is_register :: [Char] -> Bool
-- is_number, is_section, is_tag, is_type        :: [Char] -> Bool

is_opcode     str = elem str _TOKEN_OPCODES
is_identifier str = isLower (head str)
  && all (\c -> isLower c || isNumber c) (tail str)
is_chr        str = length str == 3
  && head str == '\''
  && last str == '\''
is_register   str = length str == 1
  && elem (str!!0) "ABCDEFGH"
is_comma      str = str == ","
is_number     str = (isNumber . head $ str) || (head str == '-') && all isNumber (tail str)
is_section    str = head str == '.' && is_identifier (tail str)
is_tag        str = last str == ':' && is_identifier (init str)
is_type       str = elem str _TOKEN_TYPES

-- |
-- | Tokenizer
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
          | otherwise         = error ("Invalid token \"" ++ str ++ "\"") line_n

