module Main where

-- import Lib
-- import qualified Data.Either                   as E
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
-- import qualified Data.Maybe                    as May
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

data CTree = Branch [CTree]
           | Leaf   CLeaf
           deriving (Show, Eq)

-- showCTree :: CTree -> String
-- showCTree (Branch (tree:trees)) = if Prelude.null trees
--   then show tree
--   else "(Branch " ++ (Safe.foldl1Def "" (\x y -> x ++ ", " ++ y )  $ Prelude.map show (tree:trees)) ++ ")"
-- showCTree (Branch []) = ""
-- showCTree (Leaf leaf)    = "(Leaf " ++ show leaf ++ ")"

-- instance Show CTree where show = showCTree

data CLeaf = CReserved  Reserved
           | CtrlStruct CtrlStruct
           | Stmt       CTree
           | CInt       Int
           | LVar       { offset   :: Offset
                        , isassign :: Bool}
           | DeFun      { funcName   :: String
                        , definition :: CTree}
           | Funcall    { funcName :: String
                        , args     :: [(Integer, CTree)]}
           | EOF
           deriving  (Show, Eq)

type VarName = String
type Offset = Integer

type Locals = Map.Map VarName Offset

data CtrlStruct = If   { cond :: CtrlCond, nthLabel :: Integer}
                | Whl  { cond :: CtrlCond, nthLabel :: Integer}
                | For  { cond :: CtrlCond, nthLabel :: Integer}
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

data Parsed = Parsed { stnum :: Integer
                     , lvars :: Locals
                     , funcs :: [String]
                     , ctree :: CTree}
            deriving (Show, Eq)

mainParser :: Parser Parsed
mainParser = parseProgram

parseProgram :: Parser Parsed
parseProgram =  spaces >> recur emptyparsed
  where
    emptyparsed = Parsed { stnum = 0
                         , lvars = Map.empty
                         , funcs = []
                         , ctree = Branch []}
    recur parsed = do
                defun  <- spaces >> parseDefun parsed
                defuns <- spaces >>
                  option (defun {ctree = Leaf EOF})
                  (P.try $ recur defun {stnum = 1 + (stnum defun)})
                return $ defuns {ctree = (Branch $ (ctree defun):(ctree defuns):[])}
-- (parseDefun $ Parsed 0 Map.empty (Branch []))
-- (P.try $ do
                          --     stmts <- parseStatements 0 Map.empty
                          --     return $ stmts)

parseDefun :: Parsed -> Parser Parsed
parseDefun parsed = do
  func    <- many1 letter
  _       <- spaces >> char '('
  (l0, _) <- option (l, Branch []) (P.try $ spaces >> parseLVar True l)
  l1      <- option (l0) (P.try $ getArgs 0 l0)
  _       <- spaces >> char ')'
  stmt    <- blockstmt parsed {stnum = 1 + (stnum parsed), lvars = l1}
  return stmt {funcs = func:(funcs stmt), ctree = Leaf DeFun {funcName = func, definition = (ctree stmt)}}
  where
    l = lvars parsed
    getArgs argnum lx = do
      _           <- spaces >> char ','
      (lx0, _) <- option (lx, Branch []) (P.try $ spaces >> parseLVar True lx)
      lx1 <- option lx0 (P.try $ spaces >> getArgs (argnum + 8) lx0)
      return $ (lx1)


parseStatements :: Parsed -> Parser Parsed
parseStatements p = do
  stmt  <- spaces >> parseStmt p {stnum = num + 1}
  stmts <- spaces >> option (stmt {ctree = Leaf EOF}) (P.try $ parseStatements stmt {stnum =  1 + (stnum stmt)})
  return $ stmts {ctree = (Branch $ (ctree stmt):(ctree stmts):[])}
    where
      num = stnum p


parseExpr, parseAssign, parseEquality, parseRelational, parseMul, parseAdd, parseUnary, parseTerm, parseNum, parseFuncall
  :: Locals -> Parser (Locals, CTree)

parseStmt :: Parsed -> Parser Parsed
parseStmt p =
  (P.try $ blockstmt p)
  <|> (P.try $ ifstmt p)
  <|> (P.try $ whilestmt p)
  <|> (P.try $ forstmt p)
  <|> (P.try $ returnstmt p)
  <|> simplestmt p
simplestmt, blockstmt, ifstmt, whilestmt, forstmt, returnstmt :: Parsed -> Parser Parsed

simplestmt p = do
  expr <- spaces >> parseExpr (lvars p)
  _    <- spaces >> char ';'
  return $ (\(l0, tree) -> (p { lvars = l0
                              , ctree = (Leaf . Stmt $ tree)})) expr

blockstmt p = do
  _ <- spaces >> char '{'
  stmts <- parseStatements p
  _ <- spaces >> char '}'
  return $ stmts {lvars = (lvars p)}

ifstmt p = do
  _                <- spaces >> string "if"
  _                <- spaces >> char '('
  (l0, thecond)    <- spaces >> parseExpr (lvars p)
  _                <- spaces >> char ')'
  stmt <- spaces >> parseStmt p {lvars = l0}
  elst <- option stmt (P.try $ elsestmt stmt)
  return $ stmt { ctree =
                  Branch
                  $ thecond
                  :(Leaf . CtrlStruct $ If {cond = Start, nthLabel = num})
                  :(ctree stmt)
                  :(Leaf . CtrlStruct $ If {cond = Mid, nthLabel = num})
                  :(ctree elst)
                  :(Leaf . CtrlStruct $ If {cond = End, nthLabel = num}):[]}
    where
      num = stnum p
      elsestmt px = do
        _ <- spaces >> string "else"
        stmt <- spaces >> parseStmt px
        return stmt

forstmt p = do
  _                <- spaces >> spaces >> string "for"
  _                <- spaces >> char '('
  (l0, initialize) <- spaces >> option (l, Branch []) (P.try $ parseExpr l)
  _                <- spaces >> char ';'
  (l1, thecond)    <- spaces >> option (l, Branch []) (P.try $ parseExpr l0)
  _                <- spaces >> char ';'
  (l2, theproc)    <- spaces >> option (l, Branch []) (P.try $ parseExpr l1)
  _                <- spaces >> char ')'
  stmt             <- spaces >> parseStmt p {lvars = l2}
  return (stmt {ctree = Branch
                $ initialize
                : (Leaf . CtrlStruct $ For {cond = Start, nthLabel = num})
                : thecond
                : (Leaf . CtrlStruct $ For {cond = Mid, nthLabel = num})
                : (ctree stmt)
                : theproc
                : (Leaf . CtrlStruct $ For {cond = End, nthLabel = num}):[]})
  where
    num = stnum p
    l = lvars p

whilestmt p = do
  _                <- spaces >> spaces >> string "while"
  _                <- spaces >> char '('
  (l0, thecond)    <- spaces >> parseExpr l
  _                <- spaces >> char ')'
  stmt             <- spaces >> parseStmt p {lvars = l0}
  return $ stmt {ctree =
                 Branch
                 $ (Leaf . CtrlStruct $ Whl {cond = Start, nthLabel = num})
                  :thecond
                 :(Leaf . CtrlStruct $ Whl {cond = Mid, nthLabel = num})
                 :(ctree stmt)
                 :(Leaf . CtrlStruct $ Whl {cond = End, nthLabel = num}):[]}
    where
      num = stnum p
      l = lvars p

returnstmt p = do
  _          <- spaces >> string "return"
  (l0, expr) <- spaces >> parseExpr l
  _          <- spaces >> char ';'
  return $ p { stnum = num
             , lvars = l0
             , ctree = Leaf . Stmt $ Branch $ expr:(Leaf . CtrlStruct $ Ret):[]}
    where
      num = stnum p
      l = lvars p


parseExpr l = parseAssign l

parseAssign l = do
  (lx, inits) <- option (l, []) (P.try $ spaces >> parseinits l)
  (ly, expr)  <- spaces >> parseEquality lx
  return $ (ly, if Prelude.null inits
                then expr
                else Branch $ expr:inits)
  where
    parseAssignLVar = parseLVar True
    parseinits locals = do
      (lx, stmt)  <- spaces >> parseAssignLVar locals
      _           <- spaces >> (char '=' >> (notFollowedBy $ char '='))
      (ly, stmts) <- option (lx, []) (P.try $ spaces >> parseinits lx)
      return $ (ly, stmt:stmts)

parseEquality l = do
  (lx, rel0) <- parseRelational l
  (ly, rest) <- option (lx, []) (P.try $ parserest lx)
  return (ly, if Prelude.null rest
              then rel0
              else Branch $ rel0:rest)
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
  return (ly, if Prelude.null rest
              then mul0
              else Branch $ mul0:rest)
  where
    parserest locals = do
      arthmetic  <- spaces >> matchReserved <$> many1 (oneOf "+-")
      (lx, mul)  <- spaces >> parseMul locals
      (ly, rest) <- option (lx, []) $ P.try $ spaces >> parserest lx
      return $ (ly, mul:arthmetic:rest)

parseMul l = do
  (lx, una0) <- parseUnary l
  (ly, rest) <- option (lx, []) $ P.try $ parserest lx
  return (ly, if Prelude.null rest
              then una0
              else Branch $ una0:rest)
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
              <|> (P.try $ parseFuncall l)
              <|> (P.try $ parseLVar False l)
              <|> (P.try $ between (char '(') (char ')') $ parseExpr l)

parseNum l = do
  ctree <- Leaf . CInt . read <$> many1 digit
  return (l, ctree)

parseFuncall l = do
  func <- many1 letter
  _ <- spaces >> char '('
  (l0, fstarg) <- option (l, Branch []) (P.try $ spaces >> parseExpr l)
  (l1, restargs) <- option (l, []) (P.try $ getArgs 1 l0)
  _ <- spaces >> char ')'
  return (l1, Leaf Funcall {funcName = func, args = (0, fstarg):restargs})
  where
    getArgs argnum lx = do
      _           <- spaces >> char ','
      (lx0, fstx) <- option (lx, Branch []) (P.try $ spaces >> parseExpr lx)
      (lx1, rest) <- option (lx, []) (P.try $ spaces >> getArgs (argnum + 1) lx0)
      return $ (lx1, (argnum, fstx):rest)

parseLVar :: Bool -> Locals -> Parser (Locals, CTree)
parseLVar isassn locals = do
  var <- many1 letter
  let
    theoffset = if isassn
                then 8 + (Safe.maximumDef 0 (Map.elems locals))
                else (Safe.maximumDef 0 (Map.elems locals))
    (l, lvar) = maybe
                (Map.insert var theoffset locals, Leaf $ LVar { offset = theoffset , isassign = isassn})
                (\x -> (locals, Leaf $ LVar { offset = x , isassign = isassn}))
                $ locals Map.!? var
    in
    return (l, lvar)
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

codeGen :: Parsed -> IO ()
codeGen p =
      mapM_ putStrLn
        $ (filter (List.isPrefixOf "extern") code) ++ (filter (not . (List.isPrefixOf "extern")) code)
        where
          code = (makeSrc ( "section .text"
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
                          : "; generated code"
                          : "":[]) p)

makeSrc :: [String] -> Parsed -> [String]
makeSrc accm p@Parsed {ctree = Leaf (DeFun {funcName = func, definition = tree})} = accm ++ generated
  where
    generated = (func ++ ":")
                :"; prologe"
                : ""
                : "    push    rbp"
                : "    mov     rbp, rsp"
                :("    sub     rsp, " ++ show (Safe.maximumDef 0 (Map.elems $ lvars p)))
                : "":makeSrc [] p {ctree = tree}
makeSrc accm Parsed {ctree = Branch []} = accm
makeSrc accm p@Parsed {ctree = Branch (tree:treeList)} =
  makeSrc generated $ p {ctree = Branch treeList}
  where
    generated = makeSrc accm p {ctree = tree}
makeSrc accm p@Parsed {ctree = Leaf (Stmt tree)} =
  makeSrc accm p {ctree = tree} ++ "; statement end":("    pop     rax"):"":[]
makeSrc accm Parsed {ctree = Leaf (CReserved func)} = accm ++ generated
  where
    generated = stackExec (matchCommand $ func)
makeSrc accm Parsed {ctree = Leaf (CtrlStruct ctrl)} = accm ++ generated
  where
    generated = matchCtrlStruct ctrl
makeSrc accm Parsed {ctree = Leaf (CInt num)} = accm ++ push num
makeSrc accm p@Parsed {ctree = Leaf (Funcall {funcName = func, args = argsx})} =
  -- ("extern    " ++ func):
  extern:accm ++ generatedArgs ++ ("    call    " ++ func):"    push    rax":"":[]
  -- ("extern    " ++ func):accm ++ ("    call    " ++ func):"    push    rax":[]
  where
    extern = if func `elem` (funcs p)
             then []
             else "extern    " ++ func
    genargs (num, tree) = case num of
                                0 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rdi":"":[]
                                1 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rsi":"":[]
                                2 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rdx":"":[]
                                3 -> (makeSrc [] p {ctree = tree}) ++ "    pop     rcx":"":[]
                                4 -> (makeSrc [] p {ctree = tree}) ++ "    pop     r8":"":[]
                                5 -> (makeSrc [] p {ctree = tree}) ++ "    pop     r9":"":[]
                                _ -> (makeSrc [] p {ctree = tree})
    generatedArgs = Safe.foldr1Def [] (++) $ map genargs (reverse argsx)
makeSrc accm Parsed {ctree = Leaf (LVar theoffset True)} = accm ++ generated
  where
    generated = stackAssign theoffset
makeSrc accm Parsed {ctree = Leaf (LVar theoffset False)} = accm ++ generated
  where
    generated = stackLoadLval theoffset
makeSrc accm Parsed {ctree = Leaf EOF} = accm

matchCommand :: Reserved -> [String]
matchCommand func =
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
    Unknown -> []

matchCtrlStruct :: CtrlStruct -> [String]
matchCtrlStruct ctrl =
  case ctrl of
    If {cond = Start, nthLabel = lnum}  -> "    pop     rax":
                                           "    cmp     rax, 0":
                                           ("    je      Lelse" ++ show lnum):"":[]
    If {cond = Mid, nthLabel = lnum}    -> ("    jmp     Lend" ++ show lnum):("Lelse" ++ show lnum ++ ":"):[]
    If {cond = End, nthLabel = lnum}    -> ("Lend" ++ show lnum ++ ":"):"":[]

    Whl {cond = Start, nthLabel = lnum} -> ("Lbegin" ++ show lnum ++ ":"):[]
    Whl {cond = Mid, nthLabel = lnum}   -> "    pop     rax":
                                           "    cmp     rax, 0":
                                           ("    je      Lend" ++ show lnum):"":[]
    Whl {cond = End, nthLabel = lnum}   -> ("    jmp     Lbegin" ++ show lnum):("Lend" ++ show lnum ++ ":"):[]

    For {cond = Start, nthLabel = lnum} -> ("Lbegin" ++ show lnum ++ ":"):[]
    For {cond = Mid, nthLabel = lnum}   ->  "    pop     rax":
                                            "    cmp     rax, 0":
                                           ("    je      Lend" ++ show lnum):"":[]
    For {cond = End, nthLabel = lnum}   -> ("    jmp     Lbegin" ++ show lnum):("Lend" ++ show lnum ++ ":"):[]

    Ret -> "; epiloge":
           "":
           "    pop     rax":
           "    mov     rbx, rax":
           "    mov     rsp, rbp":
           "    pop     rbp":
           "    ret":"":[]


push :: Int -> [String]
push num =
  ("    push    " ++ show num):"":[]

stackExec :: [String] -> [String]
stackExec func =
  "    pop     rdi":"    pop     rax":func ++ "    push    rax":"":[]

stackAssign :: Integer -> [String]
stackAssign theoffset =
  (genLVal theoffset) ++
  "    pop     rdi":"    pop     rax":"    mov     [rdi], rax":"    push    rax":"":[]

stackLoadLval :: Integer -> [String]
stackLoadLval theoffset =
  (genLVal theoffset) ++
  "    pop     rax":"    mov     rax, [rax]":"    push    rax":"":[]

genLVal :: Integer -> [String]
genLVal theoffset = "    mov     rax, rbp":("    sub     rax, " ++ show theoffset):"    push    rax":[]



asmMov :: String -> Int -> String
asmMov reg int = "    mov     " ++ reg ++ show int

main :: IO ()
main = do
  input <- head <$> Env.getArgs
  case parse mainParser "" input of
    Right parsed -> do
      -- putStrLn . TLazy.unpack . PrettyS.pShow $ parsed
      codeGen parsed
    Left err  -> putStrLn $ show err

debugmain :: String -> IO ()
debugmain input =
    case parse mainParser "" input of
    Right val -> do
      putStr . TLazy.unpack . PrettyS.pShow $ val
      -- codeGen val
    Left err  -> putStrLn $ show err
