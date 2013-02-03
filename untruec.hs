import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe

main :: IO ()
main = do
  contents <- B.getContents
  B.putStr $ generate $ tokenize contents

-- Primitives of the language. Ret has no corresponding character and is added
-- at the end of every lambda. Puts is generated from string literals. There is
-- no opcode for push.
data Primitive = Ret | Add | Sub | Mul | Div | Neg | Eq | Gt | And | Or | Not |
                 Store | Fetch | Call | Dup | Drop | Swap | Rot | Pick | If |
                 While | Puts | Puti | Putc | Getc
  deriving (Show, Eq)

-- Lookup a primitive from its surface form (one character)
lookup_primitive :: Char -> Maybe Primitive
lookup_primitive c =
  lookup c [('+', Add), ('-', Sub), ('*', Mul), ('/', Div), ('_', Neg),
            ('=', Eq), ('>', Gt), ('&', And), ('|', Or), ('~', Not),
            (':', Store), (';', Fetch), ('!', Call), ('$', Dup), ('%', Drop),
            ('\\', Swap), ('@', Rot), ('`', Pick), ('?', If), ('#', While),
            ('.', Puti), (',', Putc), ('^', Getc)]

-- Lookup the character value of an opcode
opcode :: Primitive -> Char
opcode p =
  chr $ fromJust $
    lookup p [(Ret, 0x80), (Add, 0x81), (Sub, 0x82), (Mul, 0x83), (Div, 0x84),
              (Neg, 0x85), (Eq, 0x86), (Gt, 0x87), (And, 0x88), (Or, 0x89),
              (Not, 0x8a), (Store, 0x8b), (Fetch, 0x8c), (Call, 0x8d),
              (Dup, 0x8e), (Drop, 0x8f), (Swap, 0x90), (Rot, 0x91),
              (Pick, 0x92), (If, 0x93), (While, 0x94), (Puts, 0x95),
              (Puti, 0x96), (Putc, 0x97), (Getc, 0x98)]

-- The tokens: a primitive function (e.g. +, !, &c.), a literal string (between
-- double quotes; will produce Puts), a literal number (implicit push), a global
-- reference (a character between 'a' and 'z'), or a lambda expression which
-- consists of a list of child tokens and points back to its parent expression.
-- The main program itself is a lambda (i.e. a program p is read like [p]!)
data Token = Function Primitive | LiteralString C.ByteString |
             LiteralNumber Int | Global Char | Lambda [Token] (Maybe Token)
  deriving Show

-- Tokenizer states: reading a chunk (i.e. in the main program or a lambda),
-- a string token (starting with " and ending with a matching "), a number
-- (starting with a digit and ending with any non digit), a comment (starting
-- with { and ending with a matching }, keeping track of the nesting level), or
-- a quoted character (starting with a ' and ending with the next character)
data TokenizerState = Chunk | StringToken C.ByteString | NumberToken Int |
                      Comment Int | Quote

-- Tokenize the string into a Lambda token
-- We add space for the 26 global variables before the code, and a call to the
-- actual start of program, followed by a Ret which will halt the machine
tokenize :: B.ByteString -> Token
tokenize s =
  let globals = [LiteralNumber 0 | x <- ['a' .. 'z']]
      start = 6 + 4 * length globals
  in tokenize' Chunk
       (Lambda ([LiteralNumber start, Function Call, Function Ret] ++ globals)
       Nothing) (C.unpack s)

-- The tokenizer keeps track of its state and the current lambda being
-- constructed. It reads one character at a time, adding tokens to the current
-- lambda. Unknown characters (like whitespace) are simply skipped.
tokenize' :: TokenizerState -> Token -> String -> Token
tokenize' Chunk token@(Lambda _ Nothing) "" = token
tokenize' Chunk p ('[':xs) = tokenize' Chunk (Lambda [] (Just p)) xs
tokenize' Chunk (Lambda _ Nothing) (']':xs) = error "Unbalanced ]"
tokenize' Chunk (Lambda ts (Just (Lambda ts' p))) (']':xs) =
  tokenize' Chunk (Lambda (ts' ++ [Lambda ts Nothing]) p) xs
tokenize' Chunk p ('{':xs) = tokenize' (Comment 1) p xs
tokenize' Chunk p ('}':xs) = error "Unbalanced }"
tokenize' (Comment _) p "" = error "Unterminated comment"
tokenize' (Comment n) p ('{':xs) = tokenize' (Comment $ n + 1) p xs
tokenize' (Comment 1) p ('}':xs) = tokenize' Chunk p xs
tokenize' (Comment n) p ('}':xs) = tokenize' (Comment $ n - 1) p xs
tokenize' (Comment n) p (_:xs) = tokenize' (Comment n) p xs
tokenize' Chunk p ('"':xs) = tokenize' (StringToken C.empty) p xs
tokenize' (StringToken s) _ "" = error "Unterminated string"
tokenize' (StringToken s) (Lambda ts p) ('"':xs) =
  tokenize' Chunk (Lambda (ts ++ [LiteralString s]) p) xs
tokenize' (StringToken s) t (x:xs) = tokenize' (StringToken (C.snoc s x)) t xs
tokenize' Chunk t ('\'':xs) = tokenize' Quote t xs
tokenize' Quote _ "" = error "Unterminated quote"
tokenize' Quote (Lambda ts p) (x:xs) =
  tokenize' Chunk (Lambda (ts ++ [LiteralNumber $ ord x]) p) xs
tokenize' Chunk (Lambda ts p) (x:xs)
  | x >= '0' && x <= '9' =
      tokenize' (NumberToken $ (ord x - ord '0')) (Lambda ts p) xs
  | x >= 'a' && x <= 'z' =
      tokenize' Chunk (Lambda (ts ++ [Global x]) p) xs
  | otherwise = case lookup_primitive x of
      (Just op) -> tokenize' Chunk (Lambda (ts ++ [Function op]) p) xs
      Nothing -> tokenize' Chunk (Lambda ts p) xs
tokenize' (NumberToken n) (Lambda ts p) "" = Lambda (ts ++ [LiteralNumber n]) p
tokenize' (NumberToken n) (Lambda ts p) (x:xs)
  | x >= '0' && x <= '9' =
      tokenize' (NumberToken $ 10 * n + (ord x - ord '0')) (Lambda ts p) xs
  | otherwise = tokenize' Chunk (Lambda (ts ++ [LiteralNumber n]) p) (x:xs)

-- Generate code for the body of the top lambda expression
generate :: Token -> C.ByteString
generate (Lambda xs _) = generate' 0 0 xs

-- Generate code for a list of token. Keep track of the start location of that
-- chunk of code (m) and the current position in the byte string (n > m). 
-- Lambdas generate a new chunk that is added at the end of the code, while that
-- address is pushed to the stack at the location where the lambda was defined.
generate' :: Int -> Int -> [Token] -> C.ByteString
generate' _ _ [] = generate'' (Function Ret)
generate' m n ((Function op):xs) =
  C.append (generate'' $ Function op) (generate' m (n + 1) xs)
generate' m n ((LiteralString s):xs) = let s' = generate'' $ LiteralString s in
  C.append s' (generate' m (n + C.length s') xs)
generate' m n ((LiteralNumber n'):xs) =
  let n'' = generate'' $ LiteralNumber n'
  in C.append n'' (generate' m (n + C.length n'') xs)
generate' m n ((Global c):xs) =
  let c' = generate'' $ LiteralNumber $ 4 * (ord c - ord 'a') + 6
  in C.append c' (generate' m (n + C.length c') xs)
generate' m n ((Lambda ys _):xs) =
  let xs' = generate' m (n + 4) xs
      m' = n + 4 + C.length xs'
      ys' = generate' m' m' ys
  in C.append (generate'' $ LiteralNumber m') $ C.append xs' ys'

-- Helper function to generate the code for literals (primitives, numbers and
-- strings.) A string generates a Puts function, followed by the length of the
-- string, followed by the characters of the string (it is a special form that
-- does not use the stack)
generate'' :: Token -> C.ByteString
generate'' (Function op) = C.singleton $ opcode op
generate'' (LiteralString s) =
  C.append (generate'' $ Function Puts) $
    C.append (generate'' $ LiteralNumber $ C.length s) s
generate'' (LiteralNumber n) =
  C.pack $ map chr $
    (shift (n .&. 0x7f000000) (-24)) : (shift (n .&. 0xff0000) (-16)) :
    (shift (n .&. 0xff00) (-8)) : (n .&. 0xff) : []
