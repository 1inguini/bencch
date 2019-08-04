{-# LANGUAGE DuplicateRecordFields #-}

module Main where

-- import Lib
-- import qualified Data.Either                   as E
import           Data.List                  as List
import           Data.Map.Strict            as Map
import           Data.Text.Lazy             as T
import qualified Data.Void                  as V
import           Prelude                    as P
import           Safe                       as Safe
import           System.Environment         as Env
import           Text.Megaparsec            as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import qualified Text.Megaparsec.Error      as MP.E
import qualified Text.Pretty.Simple         as PrettyS

type Parser = Parsec V.Void Text

data CNode = Branch [CNode]
           | Statement  CNode
           | CReserved  Reserved
           | CtrlStruct CtrlStruct
           | CInt       Int
           | Var       {mayassign :: Maybe CNode, var :: Text}
           | FunCall   {funcName :: Text, args :: [CNode]}
           | Void
           | EOF
           deriving  (Show, Eq)

data TopLevel = DeFun { funcName :: Text, args :: [CNode], definition :: CNode}
              deriving  (Show, Eq)

type VarName = String
type Offset = Integer
type Locals = Map.Map VarName Offset

data CtrlStruct = If   { cond :: CNode, stmt :: CNode, elst :: CNode }
                | Whl  { cond :: CNode, stmt :: CNode}
                | For  { init :: CNode, cond :: CNode, finexpr :: CNode, stmt :: CNode }
                | Ret
                deriving (Show, Eq)

data CtrlCond = Start
              | Mid
              | End
              deriving (Show, Eq)

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
              deriving (Show, Eq)


spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1
                (L.skipLineComment (pack "//"))
                (L.skipBlockComment (pack "/*") (pack "*/"))

symbol :: String -> Parser Text
symbol = (L.symbol spaceConsumer) . pack

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

identifier, semicolon, comma, colon, dot :: Parser Text
identifier =
  spaceConsumer >> rawident <* spaceConsumer
  where
    rawident = do
      head <- letterChar
      rest <- many (try letterChar <|> single '_')
      return . pack $ head:rest

semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

lexeme  = L.lexeme spaceConsumer
integer = lexeme L.decimal
-- signedInteger = L.signed spaceConsumer integer

mainParser :: Parser [TopLevel]
mainParser = pProgram -- <* eof

pProgram :: Parser [TopLevel]
pProgram = many pDeclare

pDeclare :: Parser TopLevel
pDeclare = pDeclareFunction

pDeclareFunction :: Parser TopLevel
pDeclareFunction = do
  funcName   <- identifier
  args       <- parens $ pInitialize `sepBy` comma
  definition <- blockStmt
  return DeFun {funcName = funcName, args = args, definition = definition}

pStatement, pExpression, pInitialize, pVar, pEquality, pRelational, pAdd, pMul, pTerm, pNum, pUnary, pFunCall
  :: Parser CNode

pStatement = choice
             [ returnStmt
             , whileStmt
             , forStmt
             , ifStmt
             , blockStmt
             , simpleStmt]

blockStmt, simpleStmt, returnStmt, ifStmt, forStmt, whileStmt :: Parser CNode

blockStmt = Branch <$> (try . braces $ many pStatement)

simpleStmt = pExpression <* semicolon

returnStmt = try $ symbol "return" >> simpleStmt

ifStmt = do
  cond <- try $ symbol "if" >> parens pExpression
  stmt <- pStatement
  elst <- option (Branch []) (symbol "else" >> pStatement)
  return $ CtrlStruct If { cond = cond, stmt = stmt, elst = elst }

forStmt = do
  init <- try $ symbol "for" >>
          parens (do init    <- option Void pExpression
                     _       <- semicolon
                     cond    <- option Void pExpression
                     _       <- semicolon
                     finexpr <- option Void pExpression
                     return For { init    = init
                                , cond    = cond
                                , finexpr = finexpr
                                , stmt    = Branch [] })
  stmt <- pStatement
  return $ CtrlStruct init { stmt = stmt }

whileStmt = do
  cond <- try $ symbol "while" >> parens pExpression
  stmt <- pStatement
  return $ CtrlStruct Whl {cond = cond, stmt = stmt}

pExpression = choice
              [ pAssign
              , pEquality ]

pAssign :: Parser CNode
pAssign = do
  inits      <- some (try $ pInitialize <* (symbol "="))
  definition <- option Void pEquality
  return . Branch $ (P.map (\x -> x {mayassign = Just definition}) inits)

pInitialize = pVar

pFunCall = try $ do
  func <- identifier
  args <- parens $ option [] (pExpression `sepBy` comma)
  return FunCall {funcName = func, args = args}

pEquality = do
  arg0 <- pRelational
  option (arg0) (do eq   <- choice $ (P.map symbol) ["==", "!="]
                    args <- pRelational
                    return FunCall {funcName = eq, args = [arg0, args]})

pRelational = do
  arg0 <- pAdd
  option (arg0) (do comp <- choice $ (P.map symbol) ["<=", ">=", ">", "<"]
                    args <- pAdd
                    return FunCall {funcName = comp, args = [arg0, args]})

pAdd = do
  arg0 <- pMul
  option (arg0) (do arth <- choice $ (P.map symbol) ["+", "-"]
                    args <- pMul
                    return FunCall {funcName = arth, args = [arg0, args]})

pMul = do
  arg0 <- pUnary
  option (arg0) (do arth <- choice $ (P.map symbol) ["*", "/"]
                    args <- pUnary
                    return FunCall {funcName = arth, args = [arg0, args]})

pUnary = genpUnary "-" (\x -> FunCall {funcName = (T.singleton '-'), args = [CInt 0, x]}) id pTerm
         <|> genpUnary "+" id id pTerm
  where
    genpUnary :: String -> (b -> c) -> (b -> c) -> Parser b -> Parser c
    genpUnary sym op0 op1 parg =
      (op0 <$> (try $ symbol sym >> parg))
      <|> op1 <$> parg

pTerm = choice [ pFunCall
               , pVar
               , pNum
               , parens pExpression]

pVar = do
  identifier <- try $ identifier
  return Var {mayassign = Nothing, var = identifier}

pNum = CInt <$> try integer

matchReserved :: Text -> CNode
matchReserved x = CReserved
  $ case unpack x of
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

-- codeGen :: Parsed -> IO ()
-- codeGen p =
--       mapM_ putStrLn
--         $ (filter (List.isPrefixOf "extern") code) ++ (filter (not . (List.isPrefixOf "extern")) code)
--         where
--           code = (makeSrc ( "section .text"
--                           : "global _start"
--                           : ""
--                           : "_start:"
--                           : "    call    main"
--                           : ""
--                           : "_exit:"
--                           : "    mov     rax, 1"
--                           -- : "    pop     rbx"
--                           : "    int     0x80"
--                           : ""
--                           : "; generated code"
--                           : "":[]) p)

-- makeSrc :: [String] -> Parsed -> [String]
-- makeSrc accm p@Parsed {ctree = Leaf (DeFun {funcName = func, definition = tree})} =
--   accm ++ generated
--   where
--     registers :: [String]
--     registers = foldl (++) [] $ List.genericTake argnum (map (reverse . push) ["rdi", "rsi", "rdx", "rcx", "r8", "r9"])
--     argnum = maxoffset `div` 8
--     maxoffset = Safe.maximumDef 8 (Map.elems (lvars p))
--     generated = (func ++ ":")
--                 :"; prologe"
--                 : "    push    rbp"
--                 : "    mov     rbp, rsp"
--                 :("    sub     rsp, " ++ show maxoffset):[]
--                 -- : registers
--                 -- ++ "":(foldl (++) [] $ map (stackAssign . show) $ drop 2 [-8,0..maxoffset])
--                 ++ "":makeSrc [] p {ctree = tree} ++ [""]
-- makeSrc accm Parsed {ctree = Branch []} = accm
-- makeSrc accm p@Parsed {ctree = Branch (tree:treeList)} =
--   makeSrc generated $ p {ctree = Branch treeList}
--   where
--     generated = makeSrc accm p {ctree = tree}
-- makeSrc accm p@Parsed {ctree = Leaf (Stmt tree)} =
--   makeSrc accm p {ctree = tree} ++ "; statement end":("    pop     rax"):"":[]
-- makeSrc accm Parsed {ctree = Leaf (CReserved func)} = accm ++ generated
--   where
--     generated = stackExec (matchCommand $ func)
-- makeSrc accm Parsed {ctree = Leaf (CtrlStruct ctrl)} = accm ++ generated
--   where
--     generated = matchCtrlStruct ctrl
-- makeSrc accm Parsed {ctree = Leaf (CInt num)} = accm ++ push (show num)
-- makeSrc accm p@Parsed {ctree = Leaf (Funcall {funcName = func, args = argsx})} =
--   -- ("extern    " ++ func):
--   extern:accm ++ generatedArgs ++ ("    call    " ++ func):"    push    rax":"":[]
--   -- ("extern    " ++ func):accm ++ ("    call    " ++ func):"    push    rax":[]
--   where
--     extern = if func `elem` (funcs p)
--              then []
--              else "extern    " ++ func
--     genargs (num, tree) = case num of
--                                 0 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rdi":"":[]
--                                 1 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rsi":"":[]
--                                 2 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rdx":"":[]
--                                 3 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rcx":"":[]
--                                 4 -> (makeSrc [] p {ctree = tree}) ++ "    pop     r8":"":[]
--                                 5 -> (makeSrc [] p {ctree = tree}) ++ "    pop     r9":"":[]
--                                 _ -> (makeSrc [] p {ctree = tree})
--     generatedArgs = Safe.foldr1Def [] (++) $ map genargs (reverse argsx)
-- makeSrc accm Parsed {ctree = Leaf (LVar theoffset True)} = accm ++ generated
--   where
--     generated = stackAssign (show theoffset)
-- makeSrc accm Parsed {ctree = Leaf (LVar theoffset False)} = accm ++ generated
--   where
--     generated = stackLoadLval (show theoffset)
-- makeSrc accm Parsed {ctree = Leaf EOF} = accm

-- matchCommand :: Reserved -> [String]
-- matchCommand func =
--   case func of
--     Add -> "    add     rax, rdi":[]
--     Sub -> "    sub     rax, rdi":[]
--     Mul -> "    imul    rdi"     :[]
--     Div -> "    idiv    rdi"     :[]
--     Eq  -> "    cmp     rax, rdi":"    sete    al" :"    movzx   rax, al":[]
--     NEq -> "    cmp     rax, rdi":"    setne   al" :"    movzx   rax, al":[]
--     GrT -> "    cmp     rdi, rax":"    setl    al" :"    movzx   rax, al":[]
--     GrE -> "    cmp     rdi, rax":"    setle   al" :"    movzx   rax, al":[]
--     LsT -> "    cmp     rax, rdi":"    setl    al" :"    movzx   rax, al":[]
--     LsE -> "    cmp     rax, rdi":"    setle   al" :"    movzx   rax, al":[]
--     Unknown -> []

-- matchCtrlStruct :: CtrlStruct -> [String]
-- matchCtrlStruct ctrl =
--   case ctrl of
--     If {cond = Start, nthLabel = lnum}  -> "    pop     rax":
--                                            "    cmp     rax, 0":
--                                            ("    je      Lelse" ++ show lnum):"":[]
--     If {cond = Mid, nthLabel = lnum}    -> ("    jmp     Lend" ++ show lnum):("Lelse" ++ show lnum ++ ":"):[]
--     If {cond = End, nthLabel = lnum}    -> ("Lend" ++ show lnum ++ ":"):"":[]

--     Whl {cond = Start, nthLabel = lnum} -> ("Lbegin" ++ show lnum ++ ":"):[]
--     Whl {cond = Mid, nthLabel = lnum}   -> "    pop     rax":
--                                            "    cmp     rax, 0":
--                                            ("    je      Lend" ++ show lnum):"":[]
--     Whl {cond = End, nthLabel = lnum}   -> ("    jmp     Lbegin" ++ show lnum):("Lend" ++ show lnum ++ ":"):[]

--     For {cond = Start, nthLabel = lnum} -> ("Lbegin" ++ show lnum ++ ":"):[]
--     For {cond = Mid, nthLabel = lnum}   ->  "    pop     rax":
--                                             "    cmp     rax, 0":
--                                            ("    je      Lend" ++ show lnum):"":[]
--     For {cond = End, nthLabel = lnum}   -> ("    jmp     Lbegin" ++ show lnum):("Lend" ++ show lnum ++ ":"):[]

--     Ret -> "; epiloge":
--            "":
--            "    pop     rax":
--            "    mov     rbx, rax":
--            "    mov     rsp, rbp":
--            "    pop     rbp":
--            "    ret":"":[]


-- push :: String -> [String]
-- push num =
--   ("    push    " ++ num):"":[]

-- stackExec :: [String] -> [String]
-- stackExec func =
--   "    pop     rdi":"    pop     rax":func ++ "    push    rax":"":[]

-- stackAssign :: String -> [String]
-- stackAssign theoffset =
--   (genLVal theoffset) ++
--   "    pop     rdi":"    pop     rax":"    mov     [rdi], rax":"    push    rax":"":[]

-- stackLoadLval :: String -> [String]
-- stackLoadLval theoffset =
--   (genLVal theoffset) ++
--   "    pop     rax":"    mov     rax, [rax]":"    push    rax":"":[]

-- genLVal :: String -> [String]
-- genLVal theoffset = "    mov     rax, rbp":("    sub     rax, " ++ theoffset):"    push    rax":[]

main :: IO ()
main = do
  input <- pack . List.head <$> Env.getArgs
  -- parseTest mainParser input
  case parse mainParser "" input of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow $ parsed
      -- codeGen parsed
    Left err  -> putStrLn $ MP.E.errorBundlePretty err

debugmain :: String -> IO ()
debugmain input =
    case parse mainParser "" (pack input) of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow $ parsed
      -- codeGen parsed
    Left err  -> putStrLn $ MP.E.errorBundlePretty err
