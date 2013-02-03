import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Data.Word

import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe

main = do
  contents <- B.getContents
  B.putStr $ generate $ tokenize contents

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

opcodes = [(Ret, 0x80), (Add, 0x81), (Sub, 0x82), (Mul, 0x83), (Div, 0x84),
           (Neg, 0x85), (Eq, 0x86), (Gt, 0x87), (And, 0x88), (Or, 0x89),
           (Not, 0x8a), (Store, 0x8b), (Fetch, 0x8c), (Call, 0x8d), (Dup, 0x8e),
           (Drop, 0x8f), (Swap, 0x90), (Rot, 0x91), (Pick, 0x92), (If, 0x93),
           (While, 0x94), (Puts, 0x95), (Puti, 0x96), (Putc, 0x97),
           (Getc, 0x98)]

opcode :: Primitive -> Char
opcode p = chr $ fromJust $ lookup p opcodes

data Token = Function Primitive | LiteralString C.ByteString |
             LiteralNumber Int | Global Char | Lambda [Token] (Maybe Token)
  deriving Show


data TokenizerState = Chunk | StringToken C.ByteString | NumberToken Int |
                      Comment | Quote

-- Tokenize the string into a Lambda token
tokenize :: B.ByteString -> Token
tokenize s =
  let globals = [LiteralNumber 0 | x <- ['a' .. 'z']]
      start = 6 + 4 * length globals
  in tokenize' Chunk
       (Lambda ([LiteralNumber start, Function Call, Function Ret] ++ globals)
       Nothing) (C.unpack s)

tokenize' :: TokenizerState -> Token -> String -> Token
tokenize' Chunk token@(Lambda _ Nothing) "" = token
tokenize' Chunk p ('[':xs) = tokenize' Chunk (Lambda [] (Just p)) xs
tokenize' Chunk (Lambda ts (Just (Lambda ts' p))) (']':xs) =
  tokenize' Chunk (Lambda (ts' ++ [Lambda ts Nothing]) p) xs
tokenize' Chunk p ('{':xs) = tokenize' Comment p xs
tokenize' Comment p ('}':xs) = tokenize' Chunk p xs
tokenize' Comment p (_:xs) = tokenize' Comment p xs
tokenize' Chunk p ('"':xs) = tokenize' (StringToken C.empty) p xs
tokenize' (StringToken s) (Lambda ts p) ('"':xs) =
  tokenize' Chunk (Lambda (ts ++ [LiteralString s]) p) xs
tokenize' (StringToken s) t (x:xs) = tokenize' (StringToken (C.snoc s x)) t xs
tokenize' Chunk t ('\'':xs) = tokenize' Quote t xs
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
  in C.append c' (generate' m (m + C.length c') xs)
generate' m n ((Lambda ys _):xs) =
  let xs' = generate' m (n + 4) xs
      m' = m + n + 4 + C.length xs'
      ys' = generate' m' m' ys
  in C.append (generate'' $ LiteralNumber m') $ C.append xs' ys'

generate'' :: Token -> C.ByteString
generate'' (Function op) = C.singleton $ opcode op
generate'' (LiteralString s) =
  C.append (generate'' $ Function Puts) $
    C.append (generate'' $ LiteralNumber $ C.length s) s
generate'' (LiteralNumber n) =
  C.pack $ map chr $
    (shift (n .&. 0x7f000000) (-24)) : (shift (n .&. 0xff0000) (-16)) :
    (shift (n .&. 0xff00) (-8)) : (n .&. 0xff) : []
