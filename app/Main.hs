module Main where

-- import Lib
-- import qualified Data.Either                   as E
-- import qualified Data.List                     as List
-- import           Data.Map.Strict               ((!?))
import           Data.Map.Strict               as Map
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
           | CInt      Int
           | LVar { offset   :: Offset
                  , isassign :: Bool}
           | Return
           | EOF
           deriving Show

type VarName = String
type Offset = Integer

type Locals = Map VarName Offset

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

mainParser :: Parser (Locals, CTree)
mainParser =  spaces >> (parseStatements empty)
  where
    parseStatements :: Locals -> Parser (Locals, CTree)
    parseStatements locals = do
      (l0, stmt) <- parseStmt locals
      (l1, stmts) <- spaces >> option (l0, Leaf EOF) (P.try $ parseStatements l0)
      return $ (l1, Branch $ stmt:stmts:[])

parseStmt, parseExpr, parseAssign, parseEquality, parseRelational, parseMul, parseAdd, parseUnary, parseTerm, parseNum
  :: Locals -> Parser (Locals, CTree)

parseStmt l =
  (P.try $ do{ expr <- spaces >> parseExpr l
             ; _    <- spaces >> char ';'
             ; return expr})
  <|> do{ _    <- spaces >> string "return"
        ; (lx, expr) <- spaces >> parseExpr l
        ; _    <- spaces >> char ';'
        ; return (lx, Branch $ expr:(Leaf Return):[])}

parseExpr l = parseAssign l

parseAssign l = do
  (lx, inits) <- option (l, []) (P.try $ spaces >> parseinits l)
  (ly, expr)  <- spaces >> parseEquality lx
  return $ (ly, Branch $ expr:inits)
  where
    parseAssignLVar lx = parseLVar lx True
    parseinits locals = do
      (lx, stmt)  <- spaces >> parseAssignLVar locals
      _           <- spaces >> (char '=' >> (notFollowedBy $ char '='))
      (ly, stmts) <- option (lx, []) (P.try $ spaces >> parseinits lx)
      return $ (ly, stmt:stmts)

parseEquality l = do
  (lx, rel0) <- parseRelational l
  (ly, rest) <- option (lx, []) (P.try $ parserest lx)
  return (ly, Branch $ rel0:rest)
  where
    parserest locals = do
      eq         <- spaces >> matchReserved <$> ((P.try $ string "==")
                                                 <|> string "!=")
      (lx, rel)  <- spaces >> parseRelational locals
      (ly, rest) <- option (lx, []) $ P.try $ spaces >> parserest lx
      return $ (ly, rel:eq:rest)

parseRelational l = do
  (lx, add0) <- parseAdd l
  (ly, rest) <- option (lx, []) (P.try $ parserest lx)
  return (ly, Branch $ add0:rest)
  where
    parserest locals = do
      comp <- spaces >> matchReserved <$> ((P.try $ string "<=")
                                            <|> (P.try $ string ">=")
                                            <|> (P.try $ string ">")
                                            <|> string "<")
      (lx, add)  <- spaces >> parseAdd locals
      (ly, rest) <- option (lx, []) $ P.try $ spaces >> parserest lx
      return $ (ly, add:comp:rest)

parseAdd l = do
  (lx, mul0) <- parseMul l
  (ly, rest) <- option (lx, []) $ P.try $ parserest lx
  return (ly, Branch $ mul0:rest)
  where
    parserest locals = do
      arthmetic  <- spaces >> matchReserved <$> many1 (oneOf "+-")
      (lx, mul)  <- spaces >> parseMul locals
      (ly, rest) <- option (lx, []) $ P.try $ spaces >> parserest lx
      return $ (ly, mul:arthmetic:rest)

parseMul l = do
  (lx, una0) <- parseUnary l
  (ly, rest) <- option (lx, []) $ P.try $ parserest lx
  return (ly, Branch $ una0:rest)
  where
    parserest locals = do
      arthmetic <- spaces >> matchReserved <$> many1 (oneOf "*/")
      (lx, una)  <- spaces >> parseUnary locals
      (ly, rest) <- option (lx, []) $ P.try $ spaces >> parserest lx
      return $ (ly, una:arthmetic:rest)

parseUnary l = do
  op        <- (P.try $ oneOf "+-") <|> (P.try $ lookAhead anyToken)
  (lx, num) <- spaces >> parseTerm l
  return $ case op of
             '-' -> (lx , Branch [(Leaf . CInt) 0, num, Leaf . CReserved $ Sub])
             '+' -> (lx, num)
             _   -> (lx, num)

parseTerm l = P.try $ parseNum l
            <|> (P.try $ parseLVar l False)
            <|> (P.try $ between (char '(') (char ')') $ parseExpr l)

parseNum l = do
  ctree <- Leaf . CInt . read <$> many1 digit
  return (l, ctree)

parseLVar :: Locals -> Bool -> Parser (Locals, CTree)
parseLVar locals isassn = do
  var <- many1 letter
  let
    theoffset = 8 + (Safe.maximumDef 0 (elems locals))
    lvar = maybe
           (Leaf $ LVar { offset = theoffset , isassign = isassn})
           (\x -> Leaf $ LVar { offset = x , isassign = isassn} )
           $ locals !? var
    in
    return (Map.insert var theoffset locals, lvar)
  -- return . Leaf $ LVar { offset = fromIntegral $ 8 * (1 + (length (takeWhile (var /=) ['a'..'z'])))
  --                      , isassign = isassn}
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


codeGen :: Locals -> CTree -> IO ()
codeGen locals val =
      mapM_ putStrLn
        $ code
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
                              :("    sub     rsp, " ++ show (Safe.maximumDef 0 (elems locals)))
                              : ""
                              : ""
                              : "; generated code"
                              : "":[]) val)

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
makeSrc accm (Leaf Return) = accm ++ generated
  where
    generated =
      "; epiloge":
      "":
      "    pop     rax":
      "    mov     rbx, rax":
      "    mov     rsp, rbp":
      "    pop     rbp":
      "    ret":"":[]
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
  "    pop     rax":"    mov     rax, [rax]":"    push    rax":"":[]

genLVal :: Integer -> [String]
genLVal theoffset = "    mov     rax, rbp":("    sub     rax, " ++ show theoffset):"    push    rax":[]

main :: IO ()
main = do
  input <- head <$> Env.getArgs
  case parse mainParser "" input of
    Right (l, val) -> do
      -- putStrLn . TLazy.unpack . PrettyS.pShow $ val
      codeGen l val
    Left err  -> putStrLn $ show err

debugmain :: String -> IO ()
debugmain input =
    case parse mainParser "" input of
    Right val -> do
      putStr . TLazy.unpack . PrettyS.pShow $ val
      -- codeGen val
    Left err  -> putStrLn $ show err
