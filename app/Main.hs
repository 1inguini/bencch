{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

-- import Lib
import qualified Control.Exception          as Ex
import           Control.Monad.State        as St
import           Data.Either                as E
import qualified Data.List                  as List
import           Data.Map.Strict            as Map
import           Data.Maybe                 as May
import           Data.Text.Lazy             as T
import           Data.Text.Lazy.Read        as T.R
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

tshow :: Show a => a -> Text
tshow = pack . show

type Parser = Parsec V.Void Text

data CNode = Branch [CNode]
           | Statement  CNode
           | CtrlStruct CtrlStruct
           | CInt      {unwrap :: Int}
           | Var       {mayassign :: Maybe CNode, var :: Text}
           | Funcall   {funcName :: Text, args :: [CNode]}
           | Void
           | EOF
           deriving  (Show, Eq)

data TopLevel = DeFun { funcName :: Text, args :: [CNode], definition :: [CNode] }
              deriving  (Show, Eq)

type VarName = Text
type Offset = Integer
type Variables = Map VarName (Offset, CNode)

data CtrlStruct = If  { cond :: CNode, stmt :: CNode, elst :: CNode }
                | Whl { cond :: CNode, stmt :: CNode}
                | For { init :: CNode, cond :: CNode, finexpr :: CNode, stmt :: CNode }
                | Ret CNode
                deriving (Show, Eq)

spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1
                (L.skipLineComment "//")
                (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = (L.symbol spaceConsumer)

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
mainParser = pProgram <* eof

pProgram :: Parser [TopLevel]
pProgram = many pDeclare

pDeclare :: Parser TopLevel
pDeclare = pDeclareFunction

pDeclareFunction :: Parser TopLevel
pDeclareFunction = do
  funcName            <- identifier
  args                <- parens $ pInitialize `sepBy` comma
  (Branch definition) <- blockStmt
  return DeFun {funcName = funcName, args = args, definition = definition}

pStatement, pExpression, pInitialize, pVar, pEquality, pRelational, pAdd, pMul, pTerm, pNum, pUnary, pFuncall
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

simpleStmt = Statement <$> pExpression <* semicolon

returnStmt = try $ symbol "return" >> (CtrlStruct . Ret <$> pExpression <* semicolon)

ifStmt = do
  cond <- try $ symbol "if" >> parens pExpression
  stmt <- pStatement
  elst <- option Void (symbol "else" >> pStatement)
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

pInitialize = (\x -> x {mayassign = Just Void}) <$> pVar

pFuncall = try $ do
  func <- identifier
  args <- parens $ option [] (pExpression `sepBy` comma)
  return Funcall {funcName = func, args = args}

pEquality = do
  arg0 <- pRelational
  option (arg0) (do eq   <- choice $ (P.map symbol) ["==", "!="]
                    args <- pRelational
                    return Funcall {funcName = eq, args = [arg0, args]})

pRelational = do
  arg0 <- pAdd
  option (arg0) (do comp <- choice $ (P.map symbol) ["<=", ">=", ">", "<"]
                    args <- pAdd
                    return Funcall {funcName = comp, args = [arg0, args]})

pAdd = do
  arg0 <- pMul
  option (arg0) (do arth <- choice $ (P.map symbol) ["+", "-"]
                    args <- pMul
                    return Funcall {funcName = arth, args = [arg0, args]})

pMul = do
  arg0 <- pUnary
  option (arg0) (do arth <- choice $ (P.map symbol) ["*", "/"]
                    args <- pUnary
                    return Funcall {funcName = arth, args = [arg0, args]})

pUnary = genpUnary "-" (\x -> Funcall {funcName = (T.singleton '-'), args = [CInt 0, x]}) id pTerm
         <|> genpUnary "+" id id pTerm
  where
    genpUnary :: Text -> (b -> c) -> (b -> c) -> Parser b -> Parser c
    genpUnary sym op0 op1 parg =
      (op0 <$> (try $ symbol sym >> parg))
      <|> op1 <$> parg

pTerm = choice [ pFuncall
               , pVar
               , pNum
               , parens pExpression]

pVar = do
  identifier <- try $ identifier
  return Var {mayassign = Nothing, var = identifier}

pNum = CInt <$> try integer

data CState = CState
            { stnum   :: Integer
            , funcs   :: [Text]
            , externs :: [Text]
            , vars    :: Variables
            , accm    :: [Text]
            , defName :: Text
            } deriving (Show, Eq)

data NasmGenError = VariableNotAssigned Text
                  | UnmatchedArgNum     {funcName :: Text, argsgot :: [CNode]}
                  deriving (Show, Eq)

type CStateOrError = Either NasmGenError CState

eitherModify :: (CState -> CState) -> St.State CStateOrError ()
eitherModify f = modify (fmap f)

program2nasm :: [TopLevel] -> [Text]
program2nasm tops =
  let
    toText :: CState -> [Text]
    toText (CState {accm = accm}) = accm
    genExtern :: CState -> [Text]
    genExtern (CState {externs = externs}) = P.map (\func -> "extern " <> func) externs
    eitherCState = (execState (mapM_ toplevel2nasm tops)
               $ Right CState { stnum   = 0
                              , funcs   = []
                              , externs = []
                              , vars    = Map.empty
                              , accm    = []
                              , defName = T.empty})
  in
  case eitherCState of
    Right cstate -> genExtern cstate ++
                    [ "section .text"
                    , "global _start"
                    , ""
                    , "_start:"
                    , "    call    main"
                    , ""
                    , "_exit:"
                    , "    mov     rax, 1"
                    , "    int     0x80"
                    , ""
                    , "; generated code"
                    , ""] ++
                    toText cstate
    Left err     -> [tshow err]


toplevel2nasm :: TopLevel -> St.State CStateOrError ()
toplevel2nasm DeFun {funcName = funcName, args = args, definition = definition} = do
  eitherModify $ \s -> s { defName = funcName
                         , funcs = funcName:(funcs s)
                         , accm = accm s ++
                                  [ funcName <> ":"
                                  , "; prologe"
                                  , "    push    rbp"
                                  , "    mov     rbp, rsp"
                                  , "    sub     rsp, 0", ""]}
  mapM_ cnode2nasm (args ++ definition)

unwrapEitherS :: (CState -> St.State CStateOrError ()) -> CStateOrError -> St.State CStateOrError ()
unwrapEitherS m (Right s)    = m s
unwrapEitherS _ err@(Left _) = put err

cnode2nasm :: CNode -> St.State CStateOrError ()
cnode2nasm (Branch []) = get >>= put
cnode2nasm (Branch (node:nodeList)) =
  cnode2nasm node >> cnode2nasm (Branch nodeList)
cnode2nasm (Statement node) = do
  cnode2nasm node
  eitherModify (\s -> s { stnum = stnum s + 1
                        , accm = accm s ++
                                 [ "; statement end"
                                 , "    pop     rax"
                                 , ""]})
cnode2nasm (CInt num) = eitherModify $ \s ->
  s {accm = accm s ++ (push . pack . show) num}

cnode2nasm (Var {var = var, mayassign = Nothing}) = modify $ \s -> case s of
  Right s -> maybe
             (Left . VariableNotAssigned $ var) (\(offset, _) -> Right s { accm = accm s ++ stackLoadLval offset})
             $ vars s !? var
  err     -> err

cnode2nasm (Var {var = var, mayassign = Just value}) = do
  cnode2nasm value
  eitherModify $ \s ->
    maybe
    (let newoffset = 8 + (maximumDef 0 $ P.map fst (elems $ vars s)) in
       s { vars = insert var (newoffset, value) $ vars s
         , accm = accm s ++ stackAssign newoffset})
    (\(offset, _) -> s { accm = accm s ++ stackAssign offset})
    $ vars s !? var

cnode2nasm (Funcall {funcName = funcName, args = args}) =
  case (funcName `elem` ["+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<="], P.length args) of
    (True, 2) -> do cnode2nasm (atDef Void args 0)
                    cnode2nasm (atDef Void args 1)
                    eitherModify $ \s -> s { accm = accm s ++ stackExec funcName }
    (True, _) -> modify $ \_ -> Left UnmatchedArgNum {funcName = funcName, argsgot = args }
    (False, _) -> get >>= unwrapEitherS
      (\s -> do
          if funcName `elem` funcs s
          then modify id
          else eitherModify $ \s -> s {externs = funcName:(externs s)}
          mapM_ (\(num, register) ->
                   case atMay args num of
                     Just arg -> do
                       cnode2nasm arg
                       eitherModify $ \s -> s {accm = accm s ++ ["    pop     " <> register, ""]}
                     Nothing  -> cnode2nasm Void)
            [ (0, "rdi")
            , (1, "rsi")
            , (2, "rdx")
            , (3, "rcx")
            , (4, "r8")
            , (5, "r9")]
          mapM_ cnode2nasm (P.reverse $ P.drop 6 args)
          eitherModify (\s -> s {accm = accm s ++ ["    call    " <> funcName, ""]}))

cnode2nasm (CtrlStruct (If {cond = cond, stmt = stmt, elst = Void})) =
  get >>= unwrapEitherS (\s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    eitherModify $ \s -> s {accm = accm s ++
                             [ "    pop     rax"
                             , "    cmp     rax, 0"
                             , "    je      Lend" <> lxxx,""]}
    cnode2nasm stmt
    eitherModify $ \s -> s {accm = accm s ++ ["Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (If {cond = cond, stmt = stmt, elst = elst})) =
  get >>= unwrapEitherS (\s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    eitherModify $ \s -> s {accm = accm s ++
                             [ "    pop     rax"
                             , "    cmp     rax, 0"
                             , "    je      Lelse" <> lxxx, ""]}
    cnode2nasm stmt
    eitherModify $ \s -> s {accm = accm s ++
                             [ "    jmp     Lend" <> lxxx
                             , "Lelse" <> lxxx <> ":"]}
    cnode2nasm elst
    eitherModify $ \s -> s {accm = accm s ++ ("Lend" <> lxxx <> ":"):[]})

cnode2nasm (CtrlStruct (Whl {cond = cond, stmt = stmt})) =
  get >>= unwrapEitherS
  (\s -> let lxxx = (tshow . stnum) s in do
      eitherModify $ \s -> s {accm = accm s ++ ("Lbegin" <> lxxx <> ":"):[]}
      cnode2nasm cond
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    pop     rax"
                                  , "    cmp     rax, 0"
                                  , "    je      Lend" <> lxxx, ""]}
      cnode2nasm stmt
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    jmp     Lbegin" <> lxxx
                                  , "Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (For {init = init, cond = cond, finexpr = finexpr, stmt = stmt})) =
  get >>= unwrapEitherS
  (\s -> let lxxx = (tshow . stnum) s in do
      cnode2nasm init
      eitherModify $ \s -> s {accm = accm s ++ ["Lbegin" <> lxxx <> ":"]}
      cnode2nasm cond
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    pop     rax"
                                  , "    cmp     rax, 0"
                                  , "    je      Lend" <> lxxx, ""]}
      cnode2nasm stmt
      cnode2nasm finexpr
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    jmp     Lbegin" <> lxxx
                                  , "Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (Ret node)) = do
  cnode2nasm node
  eitherModify $ \s -> s {accm = accm s ++
                                 ["; epiloge"
                                 , ""
                                 , "    pop     rax"
                                 , "    mov     rbx, rax"
                                 , "    mov     rsp, rbp"
                                 , "    pop     rbp"
                                 , "    ret", ""]}

cnode2nasm EOF = get >>= put
cnode2nasm Void = get >>= put

stackExec :: Text -> [Text]
stackExec func =
  "    pop     rdi":"    pop     rax":matchCommand func ++ "    push    rax":"":[]

matchCommand :: Text -> [Text]
matchCommand func =
  case func of
    "+"  -> "    add     rax, rdi":[]
    "-"  -> "    sub     rax, rdi":[]
    "*"  -> "    imul    rdi"     :[]
    "/"  -> "    idiv    rdi"     :[]
    "==" -> "    cmp     rax, rdi":"    sete    al" :"    movzx   rax, al":[]
    "!=" -> "    cmp     rax, rdi":"    setne   al" :"    movzx   rax, al":[]
    ">"  -> "    cmp     rdi, rax":"    setl    al" :"    movzx   rax, al":[]
    ">=" -> "    cmp     rdi, rax":"    setle   al" :"    movzx   rax, al":[]
    "<"  -> "    cmp     rax, rdi":"    setl    al" :"    movzx   rax, al":[]
    "<=" -> "    cmp     rax, rdi":"    setle   al" :"    movzx   rax, al":[]
    _    -> []

push :: Text -> [Text]
push num =
  (T.concat ["    push    ", num]):T.empty:[]

stackAssign :: (Integral a, Show a) => a -> [Text]
stackAssign theoffset =
  (genLVal theoffset) ++
  "    pop     rdi":"    pop     rax":"    mov     [rdi], rax":"    push    rax":"":[]

stackLoadLval :: (Integral a, Show a) => a -> [Text]
stackLoadLval theoffset =
  (genLVal theoffset) ++
  "    pop     rax":"    mov     rax, [rax]":"    push    rax":"":[]

genLVal :: (Integral a, Show a) => a -> [Text]
genLVal theoffset = "    mov     rax, rbp":("    sub     rax, " <> (tshow theoffset)):"    push    rax":[]

main :: IO ()
main = do
  input <- pack . List.head <$> Env.getArgs
  -- parseTest mainParser input
  case parse mainParser "" input of
    Right parsed -> do
      -- putStrLn . unpack . PrettyS.pShow $ parsed
      mapM_ (putStrLn . unpack) $ program2nasm parsed
    Left err  -> putStrLn $ MP.E.errorBundlePretty err

debugmain :: String -> IO ()
debugmain input =
    case parse mainParser "" (pack input) of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow $ parsed
      mapM_ (putStrLn . unpack) $ program2nasm parsed
    Left err  -> putStrLn $ MP.E.errorBundlePretty err
