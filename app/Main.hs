module Main where

-- import Lib
-- import qualified Data.Either                   as E
-- import qualified Data.List                     as List
-- import           Data.Map.Strict               ((!?))
-- import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as May
-- import qualified Data.Text.Lazy                as TLazy
-- import qualified System.Console.Haskeline      as HLine
-- import qualified System.Environment            as Env
import           Data.Int
import qualified Data.Text.Lazy                as TLazy
import qualified Safe                          as Safe
import qualified System.Environment            as Env
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import qualified Text.Pretty.Simple            as PrettyS

data CLeaf = CReserved Reserved
           | CInt32    Int32
           | EOF
           deriving Show

data Reserved = Add
              | Sub
              | Mul
              | Div
              | Unknown
              deriving Show

data CTree = Branch [CTree]
           | Leaf   CLeaf
           deriving Show

makeSrc :: [String] -> CTree -> [String]
makeSrc accm (Branch []) = accm
makeSrc accm (Branch (tree:treeList)) =
  makeSrc generated $ Branch treeList
  where
    generated = makeSrc accm tree
makeSrc accm (Leaf (CReserved func)) = accm ++ generated
  where
    generated = stackExec (matchLLVMCommand $ func)
makeSrc accm (Leaf (CInt32 num)) = accm ++ push num
makeSrc accm (Leaf EOF) = accm

matchLLVMCommand :: Reserved -> [String]
matchLLVMCommand func =
  case func of
    Add -> "    add rax, rbx":"    push rax":[]
    Sub -> "    sub rax, rbx":"    push rax":[]
    Mul -> "    mul rbx":"    push rax":[]
    Div -> "    div rbx":"    push rax":[]

push :: Int32 -> [String]
push num
  = ("    push " ++ show num):"":[]

stackExec :: [String] -> [String]
stackExec func
  = "    pop rbx":"    pop rax":func ++ "":[]

symbol :: Parser Char
symbol = oneOf "+-*/"

mainParser, parseExpr, parseMul, parseTerm, parseNum :: Parser CTree

mainParser = spaces >> ((P.try $ do { eof; return $ Leaf EOF})
                        <|> parseExpr)

parseExpr = do
  mul0 <- spaces >> parseMul
  rest <- many (P.try $ do
                   arthmetic <- spaces >> matchReserved <$> many1 (oneOf "+-")
                   mul <- spaces >> parseMul
                   return $ mul:[arthmetic])
  return . Branch $ mul0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseMul = do
  mul0 <- parseTerm
  rest <- many (try $ do
                   arthmetic <- spaces >> matchReserved <$> many1 (oneOf "*/")
                   mul <- spaces >> parseTerm
                   return $ mul:[arthmetic])
  return . Branch $ mul0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseTerm =
  spaces >>
  (P.try parseNum
   <|> between (char '(') (char ')') parseExpr)

parseNum = Leaf . CInt32 . read <$> many1 digit

matchReserved :: String -> CTree
matchReserved x =
  Leaf . CReserved
  $ case x of
      "+" -> Add
      "-" -> Sub
      "*" -> Mul
      "/" -> Div
      _   -> Unknown

main :: IO ()
main = do
  input <- head <$> Env.getArgs
  case parse mainParser "" input of
    Right val -> do
      -- putStr . TLazy.unpack . PrettyS.pShow $ val
      mapM_ putStrLn
        $ code ++ "_exit:":"    mov rax, 1":"    pop rbx":"    int 0x80":[]
        where code = (makeSrc (-- ".intel_syntax noprefix":".global main":
                               "section .text":
                               "global _start":
                               "":
                               "_start:":[]) val)
    Left err  -> putStrLn $ show err
