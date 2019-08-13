{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

-- import Lib
import           Control.Monad.State        as St
import           Data.Either                as E
import qualified Data.List                  as List
import           Data.Map.Strict            as Map
import           Data.Maybe                 as May
import           Data.Text.Lazy             as T
import qualified Data.Void                  as V
import           Prelude                    as P
import           Safe                       as Safe
import           System.Environment         as Env
import           Text.Megaparsec            as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug      as MP.Debug
import qualified Text.Megaparsec.Error      as MP.E
import qualified Text.Pretty.Simple         as PrettyS

tshow :: Show a => a -> Text
tshow = pack . show

type Parser = Parsec V.Void Text

data CNode = Branch [CNode]
           | Expression  CNode
           | CtrlStruct CtrlStruct
           | CLong   {unwrap :: Integer}
           | DefVar  {var :: Text}
           | Var     {mayassign :: Maybe CNode, var :: Text}
           | Funcall {funcName :: Text, args :: [CNode]}
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
      rest <- many (try letterChar <|> try digitChar <|> single '_')
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
             , simpleStmt
             ]

blockStmt, simpleStmt, returnStmt, ifStmt, forStmt, whileStmt :: Parser CNode

blockStmt = Branch <$> (try . braces $ many pStatement)

simpleStmt = Expression <$> pExpression <* semicolon

returnStmt = try $ symbol "return" >> (CtrlStruct . Ret <$> simpleStmt)

ifStmt = do
  cond <- try $ symbol "if" >> parens pEquality
  stmt <- pStatement
  elst <- option Void (symbol "else" >> pStatement)
  return $ CtrlStruct If { cond = cond, stmt = stmt, elst = elst }

forStmt = do
  init <- try $ symbol "for" >>
          parens (do init    <- option Void pAssign
                     _       <- semicolon
                     cond    <- option Void pEquality
                     _       <- semicolon
                     finexpr <- option Void pAssign
                     return For { init    = init
                                , cond    = cond
                                , finexpr = finexpr
                                , stmt    = Branch [] })
  stmt <- pStatement
  return $ CtrlStruct init { stmt = stmt }

whileStmt = do
  cond <- try $ symbol "while" >> parens pEquality
  stmt <- pStatement
  return $ CtrlStruct Whl {cond = cond, stmt = stmt}

pExpression = choice
              [ pInitialize
              , pAssign
              , pEquality
              ]


pAssign :: Parser CNode
pAssign = do
  init       <- try $ pVar <* symbol "="
  definition <- option Void pExpression
  return init {mayassign = Just definition}

pInitialize = (\x -> DefVar {var = x}) <$> (symbol "long" >> identifier)

pFuncall = try $ do
  func <- identifier
  args <- parens $ option [] (pExpression `sepBy` comma)
  return Funcall {funcName = func, args = args}

pEquality = do
  arg0 <- pRelational
  option (arg0) (do eq   <- choice $ (P.map symbol) ["==", "!="]
                    args <- pExpression
                    return Funcall {funcName = eq, args = [arg0, args]})

pRelational = do
  arg0 <- pAdd
  option (arg0) (do comp <- choice $ (P.map symbol) ["<=", ">=", ">", "<"]
                    args <- pExpression
                    return Funcall {funcName = comp, args = [arg0, args]})

pAdd = do
  arg0 <- pMul
  option (arg0) (do arth <- choice $ (P.map symbol) ["+", "-"]
                    args <- pExpression
                    return Funcall {funcName = arth, args = [arg0, args]})

pMul = do
  arg0 <- pUnary
  option (arg0) (do arth <- choice $ (P.map symbol) ["*", "/"]
                    args <- pExpression
                    return Funcall {funcName = arth, args = [arg0, args]})

pUnary = choice $ (P.map genpUnary unarysAndParsers) ++ [pTerm]
  where
    genpUnary :: (Text, Parser CNode) -> Parser CNode
    genpUnary (sym, parser) =
      (\x -> Funcall {funcName = sym, args = [x]}) <$> (try $ symbol sym >> parser)

unarys :: [Text]
unarys = P.map fst unarysAndParsers

unarysAndParsers :: [(Text, Parser CNode)]
unarysAndParsers = [ ("+", pTerm)
                   , ("-", pTerm)
                   , ("*", pUnary)
                   , ("&", pUnary)
                   ]

pTerm = choice
        [ pFuncall
        , pVar
        , pNum
        , parens pExpression]

pVar = do
  identifier <- try $ identifier
  return Var {mayassign = Nothing, var = identifier}

pNum = CLong <$> try integer

data CState = CState
            { stnum   :: Integer
            , funcs   :: [Text]
            , externs :: [Text]
            , vars    :: Variables
            , accm    :: [Text]
            , defName :: Text
            } deriving (Show, Eq)

data NasmGenError = VariableNotDefined Text
                  | MultipleDeclaration Text
                  | UnmatchedArgNum     {funcName :: Text, argsgot :: [CNode]}
                  deriving (Show, Eq)

type CStateOrError = Either NasmGenError CState

eitherModify :: (CState -> CState) -> St.State CStateOrError ()
eitherModify f = modify (fmap f)

unwrapEitherS :: (CState -> St.State CStateOrError ()) -> CStateOrError -> St.State CStateOrError ()
unwrapEitherS m (Right s)    = m s
unwrapEitherS _ err@(Left _) = put err

sysVCallRegs :: [Text]
sysVCallRegs = [ "rdi", "rsi", "rdx", "rcx", "r8", "r9"]

biops :: [Text]
biops = ["+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<="]

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
                    , "    mov     rbx, rax"
                    , "    mov     rax, 1"
                    , "    int     0x80"
                    , ""
                    , "; generated code"
                    , ""] ++
                    toText cstate
    Left err     -> [tshow err]

toplevel2nasm :: TopLevel -> St.State CStateOrError ()
toplevel2nasm DeFun {funcName = funcName, args = args, definition = definition} = do
  eitherModify $ \s -> s { vars = Map.empty }
  mapM_ genLocals (args ++ definition)
  eitherModify $ \s -> s { defName = funcName
                         , funcs = funcName:(funcs s)
                         , accm = accm s ++
                                  [ funcName <> ":"
                                  , "; prologe"] }
  push "rbp"
  mov "rbp" "rsp"
  get >>= unwrapEitherS (\s -> do matchCommand "-" "rsp" (tshow (maximumDef 0 $ P.map fst (elems $ vars s)))
                                  eitherModify $ \s -> s {vars = Map.empty})
  nl
  assign6Args 0 (P.take (P.length args) sysVCallRegs)
  assignRestArgs (P.length sysVCallRegs) (P.drop (P.length sysVCallRegs) args)
  mapM_ cnode2nasm definition
  where
    assign6Args :: (Show a, Integral a) => a -> [Text] -> St.State CStateOrError ()
    assign6Args _ [] = modify id
    assign6Args index (reg:registers) = do
      eitherModify $ \s -> s { accm = accm s }
      push reg
      stackAssign (8 * (index + 1))
      cnode2nasm $ Expression Void
      assign6Args (succ index) registers

    assignRestArgs :: (Show a, Integral a) => a -> [b] -> St.State CStateOrError ()
    assignRestArgs _ [] = modify id
    assignRestArgs index (_:xs) = do
      push ("[rbp+" <> tshow (8 * (2 + index - (fromIntegral $ P.length sysVCallRegs))) <> "]")
      stackAssign (8 * (1 + index))
      cnode2nasm $ Expression Void
      assignRestArgs (succ index) xs

genLocals :: CNode -> St.State CStateOrError ()
genLocals DefVar {var = var} = do
  get >>= unwrapEitherS
    (\s ->
       maybe
       (let newoffset = 8 + (maximumDef 0 $ P.map fst (elems $ vars s)) in do
           eitherModify $ \s -> s { vars = insert var (newoffset, Void) $ vars s})
       (\_ -> put . Left . MultipleDeclaration $ var)
       $ vars s !? var)

genLocals (Branch []) = modify id
genLocals (Branch nodeList) =
  mapM_ genLocals nodeList
genLocals (Expression node) =
  genLocals node
genLocals (CtrlStruct (If {cond = cond, stmt = stmt, elst = Void})) =
  mapM_ genLocals [cond, stmt]
genLocals (CtrlStruct (If {cond = cond, stmt = stmt, elst = elst})) =
  mapM_ genLocals [cond, stmt, elst]
genLocals (CtrlStruct (Whl {cond = cond, stmt = stmt})) =
  mapM_ genLocals [cond, stmt]
genLocals (CtrlStruct (For {init = init, cond = cond, finexpr = finexpr, stmt = stmt})) =
  mapM_ genLocals [init, cond, stmt, finexpr]
genLocals (CtrlStruct (Ret node)) = do
  genLocals node
genLocals _ = modify id

cnode2nasm :: CNode -> St.State CStateOrError ()
cnode2nasm (Branch []) = get >>= put
cnode2nasm (Branch nodeList) =
  mapM_ cnode2nasm nodeList
cnode2nasm (Expression node) = do
  cnode2nasm node
  eitherModify $ \s -> s { stnum = stnum s + 1
                         , accm = accm s ++ [ "; statement end"]}
  pop "rax"
  nl

cnode2nasm (CLong num) = push (tshow num)

cnode2nasm DefVar {var = var} = do
  get >>= unwrapEitherS
    (\s ->
       maybe
       (let newoffset = 8 + (maximumDef 0 $ P.map fst (elems $ vars s)) in do
           push "0"
           stackAssign newoffset
           eitherModify $ \s -> s { vars = insert var (newoffset, Void) $ vars s})
       (\_ -> put . Left . MultipleDeclaration $ var)
       $ vars s !? var)

cnode2nasm Var {var = var, mayassign = Nothing} = do
  s <- get
  case s of
    Right s -> maybe
               (put . Left . VariableNotDefined $ var)
               (\(offset, _) -> stackLoadLval offset)
               $ vars s !? var
    err     -> put err

cnode2nasm Var {var = var, mayassign = Just value} = do
  cnode2nasm value
  s <- get
  case s of
    Right s -> maybe
               (put . Left . VariableNotDefined $ var)
               (\(offset, _) -> stackAssign offset)
               $ vars s !? var
    err     -> put err

cnode2nasm Funcall {funcName = funcName, args = args} =
  case ( funcName `elem`  biops
       , funcName `elem` unarys
       , P.length args) of
    (True, _, 2) -> stackExec funcName (atDef Void args 0) (atDef Void args 1)

    (_, True, 1) -> stackUnary funcName (atDef Void args 0)

    (True, _, _) -> modify $ \_ -> Left UnmatchedArgNum {funcName = funcName, argsgot = args }
    (_, True, _) -> modify $ \_ -> Left UnmatchedArgNum {funcName = funcName, argsgot = args }

    (False, _, _) -> get >>= unwrapEitherS
      (\s -> do
          if funcName `elem` funcs s
            then modify id
            else eitherModify $ \s -> s {externs = funcName:(externs s)}
          fst6args 0 sysVCallRegs
          mapM_ cnode2nasm (P.reverse $ P.drop (P.length sysVCallRegs) args)
          eitherModify (\s -> s {accm = accm s ++ [ "    call    " <> funcName]})
          push "rax"
          nl)
  where
    fst6args :: Integral a => a -> [Text] -> St.State CStateOrError ()
    fst6args 6 _ = modify id
    fst6args index registers =
      case atMay args (fromIntegral index) of
        Just arg -> do
          cnode2nasm arg
          pop (atDef "" registers $ fromIntegral index)
          nl
          fst6args (succ index) registers
        Nothing  -> modify id

cnode2nasm (CtrlStruct (If {cond = cond, stmt = stmt, elst = Void})) =
  get >>= unwrapEitherS (\s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    pop "rax"
    eitherModify $ \s -> s {accm = accm s ++
                             [ "    cmp     rax, 0"
                             , "    je      Lend" <> lxxx,""]}
    cnode2nasm stmt
    eitherModify $ \s -> s {accm = accm s ++ ["Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (If {cond = cond, stmt = stmt, elst = elst})) =
  get >>= unwrapEitherS (\s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    pop "rax"
    eitherModify $ \s -> s {accm = accm s ++
                             [ "    cmp     rax, 0"
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
      pop "rax"
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    cmp     rax, 0"
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
      pop "rax"
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    cmp     rax, 0"
                                  , "    je      Lend" <> lxxx]}
      nl
      cnode2nasm stmt
      cnode2nasm finexpr
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    jmp     Lbegin" <> lxxx
                                  , "Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (Ret node)) = do
  cnode2nasm node
  eitherModify $ \s -> s {accm = accm s ++ ["; epiloge"]}
  -- pop "rax"
  mov "rsp" "rbp"
  pop "rbp"
  eitherModify $ \s -> s {accm = accm s ++ ["    ret"]}
  nl

cnode2nasm EOF = get >>= put
cnode2nasm Void = get >>= put

stackExec :: Text -> CNode -> CNode -> St.State CStateOrError ()
stackExec func arg0 arg1 = do
  cnode2nasm arg0
  cnode2nasm arg1
  pop "r11"
  pop "rax"
  matchCommand func "rax" "r11"
  push "rax"
  nl

matchCommand :: Text -> Text -> Text -> St.State CStateOrError ()
matchCommand func arg0 arg1 = do
  eitherModify $ \s ->
    s {accm = accm s ++
              case func of
                "+"  -> [ "    add     " <> arg0 <> ", " <> arg1 ]
                "-"  -> [ "    sub     " <> arg0 <> ", " <> arg1 ]
                "*"  -> [ "    mov     rax, " <> arg0, "    imul    " <> arg1 ]
                "/"  -> [ "    mov     rax, " <> arg0, "    idiv    " <> arg1 ]
                "==" -> [ "    cmp     " <> arg0 <> ", " <> arg1, "    sete    al" , "    movzx   rax, al" ]
                "!=" -> [ "    cmp     " <> arg0 <> ", " <> arg1, "    setne   al" , "    movzx   rax, al" ]
                ">"  -> [ "    cmp     " <> arg1 <> ", " <> arg0, "    setl    al" , "    movzx   rax, al" ]
                ">=" -> [ "    cmp     " <> arg1 <> ", " <> arg0, "    setle   al" , "    movzx   rax, al" ]
                "<"  -> [ "    cmp     " <> arg0 <> ", " <> arg1, "    setl    al" , "    movzx   rax, al" ]
                "<=" -> [ "    cmp     " <> arg0 <> ", " <> arg1, "    setle   al" , "    movzx   rax, al" ]
                _    -> []}

stackUnary :: Text -> CNode -> St.State CStateOrError ()
stackUnary func arg =
            case func of
              "+" -> modify id
              "-" -> cnode2nasm arg >> pop "rax" >> nasmNeg "rax" >> push "rax"
              "*" -> cnode2nasm arg >> pop "rax" >> push "[rax]"
              "&" -> do s <- get
                        case (arg, s) of
                          (Var {var = var}, Right s) -> maybe
                                                        (put . Left . VariableNotDefined $ var)
                                                        (\(offset, _) -> do mov "rax" "rbp"
                                                                            matchCommand "-" "rax" (tshow offset)
                                                                            push "rax")
                                                        $ vars s !? var
                          (var, Right s)             -> (put . Left . VariableNotDefined $ tshow var)
                          (_, err@(Left _))          -> put err
              _   -> modify id
            >> nl

nasmNeg :: Text -> St.State CStateOrError ()
nasmNeg arg = eitherModify $ \s -> s {accm = accm s ++  [ "    neg     " <> arg ]}

stackAssign :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackAssign theoffset = do
  pop ("[rbp-" <> (tshow theoffset) <> "]")
  push ("[rbp-" <> (tshow theoffset) <> "]")
  nl

stackLoadLval :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackLoadLval theoffset = do
  push ("[rbp-" <> (tshow theoffset) <> "]")
  nl

-- genLVal :: (Integral a, Show a) => a -> St.State CStateOrError ()
-- genLVal theoffset = do
--   mov "rax" "rbp"
--   matchCommand "-" "rax" (tshow theoffset)

push :: Text -> St.State CStateOrError ()
push num = eitherModify $ \s ->
  s {accm = accm s ++ [ "    push    qword " <> num ]}

pop :: Text -> St.State CStateOrError ()
pop num = eitherModify $ \s ->
  s {accm = accm s ++ [ "    pop     qword " <> num ]}

mov :: Text -> Text -> St.State CStateOrError ()
mov dest src = eitherModify $ \s ->
  s {accm = accm s ++ [ "    mov     " <> dest <> ", qword " <> src]}

nl :: St.State CStateOrError ()
nl = eitherModify $ \s ->
  s {accm = accm s ++ [""]}

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
