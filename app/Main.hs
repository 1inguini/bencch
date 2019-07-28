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
-- import           Data.Int
-- import           Text.Parsec.Language               as Language
-- import           Text.Parsec.Token                  as Token
-- import           Text.ParserCombinators.Parsec.Expr as Expr
import qualified Data.Text.Lazy                as TLazy
import qualified Safe                          as Safe
import qualified System.Environment            as Env
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec
import qualified Text.Pretty.Simple            as PrettyS

data CLeaf = CReserved Reserved
           | Ident Char
           | LVar { offset   :: Integer
                  , isassign :: Bool}
           | CInt      Int
           | EOF
           deriving Show

data Reserved = Add
              | Sub
              | Mul
              | Div
              | Eq
              | NEq
              | GrT
              | GrE
              | LsT
              | LsE
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
makeSrc accm (Leaf (CInt num)) = accm ++ push num
makeSrc accm (Leaf (LVar theoffset True)) = accm ++ generated
  where
    generated = stackAssign theoffset
makeSrc accm (Leaf (LVar theoffset False)) = accm ++ generated
  where
    generated = stackLoadLval theoffset
makeSrc accm (Leaf EOF) = accm

matchLLVMCommand :: Reserved -> [String]
matchLLVMCommand func =
  case func of
    Add -> "    add     rax, rdi":[]
    Sub -> "    sub     rax, rdi":[]
    Mul -> "    imul    rdi"     :[]
    Div -> "    idiv    rdi"     :[]
    Eq  -> "    cmp     rax, rdi":"    sete    al" :"    movzx   rax, al":[]
    NEq -> "    cmp     rax, rdi":"    setne   al" :"    movzx   rax, al":[]
    GrT -> "    cmp     rdi, rax":"    setl    al" :"    movzx   rax, al":[]
    GrE -> "    cmp     rdi, rax":"    setle   al" :"    movzx   rax, al":[]
    LsT -> "    cmp     rax, rdi":"    setl    al" :"    movzx   rax, al":[]
    LsE -> "    cmp     rax, rdi":"    setle   al" :"    movzx   rax, al":[]

push :: Int -> [String]
push num =
  ("    push    " ++ show num):"":[]

stackExec :: [String] -> [String]
stackExec func =
  "    pop     rdi":"    pop     rax":func ++ "    push    rax":"":[]

stackAssign :: Integer -> [String]
stackAssign theoffset =
  (genLVal theoffset) ++
  "    pop     rdi":"    pop     rax":"    mov     [rdi], rax":"":[]

stackLoadLval :: Integer -> [String]
stackLoadLval theoffset =
  (genLVal theoffset) ++
  "    pop     rax":"    mov     rax, [rax]":"    push    rax":[]

genLVal :: Integer -> [String]
genLVal theoffset = "    mov     rax, rbp":("  sub rax, " ++ show theoffset):"    push    rax":[]

symbol :: Parser Char
symbol = oneOf "+-*/"

mainParser, parseStmt, parseExpr, parseAssign, parseEquality, parseRelational, parseMul, parseAdd, parseUnary, parseTerm, parseNum
  :: Parser CTree

mainParser = Branch <$> (spaces >>
                          ((P.try $ do { eof; return $ [Leaf EOF]})
                            <|> many parseStmt))

parseStmt = do
  expr <- spaces >> parseExpr
  _ <- spaces >> char ';'
  return expr

parseExpr = parseAssign

parseAssign = do
  inits <- many (P.try $ do
                   ident <- parseAssignLVar
                   _  <- spaces >> string "="
                   return $ ident)
  expr <- spaces >> parseEquality
  return . Branch $ expr:inits
  where
    parseAssignLVar = parseLVar True

parseEquality = do
  rel0 <- parseRelational
  rest <- many (P.try $ do
                   eq <- spaces >> matchReserved <$> ((P.try $ string "==") <|> string "!=")
                   rel <- spaces >> parseRelational
                   return $ rel:[eq])
  return . Branch $ rel0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseRelational = do
  add0 <- parseAdd
  rest <- many (P.try $ do
                   comp <- spaces >> matchReserved <$> ((P.try $ string "<=")
                                                         <|> (P.try $ string ">=")
                                                         <|> (P.try $ string ">")
                                                         <|> string "<")
                   add <- spaces >> parseAdd
                   return $ add:[comp])
  return . Branch $ add0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseAdd = do
  mul0 <- parseMul
  rest <- many (P.try $ do
                   arthmetic <- spaces >> matchReserved <$> many1 (oneOf "+-")
                   mul <- spaces >> parseMul
                   return $ mul:[arthmetic])
  return . Branch $ mul0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseMul = do
  una0 <- parseUnary
  rest <- many (P.try $ do
                   arthmetic <- spaces >> matchReserved <$> many1 (oneOf "*/")
                   una <- spaces >> parseUnary
                   return $ una:[arthmetic])
  return . Branch $ una0:(May.maybe ([]) (id) $ Safe.foldl1May (++) rest)

parseUnary = do
  op <- (P.try $ oneOf "+-") <|> (P.try $ lookAhead anyToken)
  spaces
  num <- parseTerm
  return $ case op of
             '-' -> Branch [(Leaf . CInt) 0, num, Leaf . CReserved $ Sub]
             '+' -> num
             _   -> num

parseTerm = P.try parseNum
            <|> (P.try $ parseLVar False)
            <|> (P.try $ between (char '(') (char ')') parseExpr)

parseNum = Leaf . CInt . read <$> many1 digit

parseLVar :: Bool -> Parser CTree
parseLVar isassn = do
  var <- letter
  return . Leaf $ LVar { offset = fromIntegral $ 8 * (1 + (length (takeWhile (var /=) ['a'..'z'])))
                       , isassign = isassn}
  -- (Leaf . Ident) var
  -- Branch [Leaf . CReserved . LVar $ 8 * (1 + (length (takeWhile (var /=) ['a'..'z']))), Leaf . CReserved $ Ident]

matchReserved :: String -> CTree
matchReserved x =
  Leaf . CReserved
  $ case x of
      "+"  -> Add
      "-"  -> Sub
      "*"  -> Mul
      "/"  -> Div
      "==" -> Eq
      "!=" -> NEq
      "<"  -> LsT
      "<=" -> LsE
      ">"  -> GrT
      ">=" -> GrE
      _    -> Unknown


codeGen :: CTree -> IO ()
codeGen val =
      mapM_ putStrLn
        $ code ++
        "":
        "; epiloge":
        "":
        "    pop     rax":
        "    mov     rbx, rax":
        "    mov     rsp, rbp":
        "    pop     rbp":
        "    ret":
        "":
        "":[]
        where code = (makeSrc ( "section .text"
                              : "global _start"
                              : ""
                              : "_start:"
                              : "    call    main"
                              : ""
                              : "_exit:"
                              : "    mov     rax, 1"
                              -- : "    pop     rbx"
                              : "    int     0x80"
                              : ""
                              : "main:"
                              : "; prologe"
                              : ""
                              : "    push    rbp"
                              : "    mov     rbp, rsp"
                              : "    sub     rsp, 208"
                              : ""
                              : ""
                              : "; generated code"
                              : "":[]) val)


main :: IO ()
main = do
  input <- head <$> Env.getArgs
  case parse mainParser "" input of
    Right val -> do
      -- putStr . TLazy.unpack . PrettyS.pShow $ val
      codeGen val
    Left err  -> putStrLn $ show err
