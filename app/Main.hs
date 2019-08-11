{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

-- import Lib
import qualified Control.Exception          as Ex
import           Control.Monad.State        as St
import           Data.Either                as E
import qualified Data.IntMap.Strict         as IMap
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
           | CLong      {unwrap :: Integer}
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

simpleStmt = pExpression <* semicolon

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

pUnary = genpUnary "-" (\x -> Funcall {funcName = (T.singleton '-'), args = [CLong 0, x]}) id pTerm
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

pNum = CLong <$> try integer

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

unwrapEitherS :: (CState -> St.State CStateOrError ()) -> CStateOrError -> St.State CStateOrError ()
unwrapEitherS m (Right s)    = m s
unwrapEitherS _ err@(Left _) = put err

sysVCallRegs :: [Text]
sysVCallRegs = [ "rdi", "rsi", "rdx", "rcx", "r8", "r9"]

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
  get >>= unwrapEitherS (\s -> matchCommand "-" "rsp" (tshow (maximumDef 0 $ P.map fst (elems $ vars s))))
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
      assign6Args (succ index) registers

    assignRestArgs :: (Show a, Integral a) => a -> [b] -> St.State CStateOrError ()
    assignRestArgs _ [] = modify id
    assignRestArgs index (_:xs) = do
      mov "rax" ("qword [rbp+" <> tshow (8 * (2 + index - (fromIntegral $ P.length sysVCallRegs))) <> "]")
      push "rax"
      nl
      stackAssign (8 * (1 + index))
      assignRestArgs (succ index) xs

genLocals :: CNode -> St.State CStateOrError ()
genLocals Var {var = var, mayassign = Just _} = do
      eitherModify $ \s ->
          maybe
          (let newoffset = 8 + (maximumDef 0 $ P.map fst (elems $ vars s)) in
              s { vars = insert var (newoffset, Void) (vars s)})
          (\_ -> s)
          $ vars s !? var
genLocals (Branch nodeList) =
  mapM_ genLocals nodeList
genLocals (Statement node) =
  genLocals node
genLocals _ = modify id

cnode2nasm :: CNode -> St.State CStateOrError ()
cnode2nasm (Branch []) = get >>= put
cnode2nasm (Branch nodeList) =
  mapM_ cnode2nasm nodeList
cnode2nasm (Statement node) = do
  cnode2nasm node
  eitherModify $ \s -> s { stnum = stnum s + 1
                         , accm = accm s ++ [ "; statement end"]}
  pop "rax"
  nl

cnode2nasm (CLong num) = push (tshow num)

cnode2nasm Var {var = var, mayassign = Nothing} = do
  s <- get
  case s of
    Right s -> maybe
               (put . Left . VariableNotAssigned $ var) (\(offset, _) -> stackLoadLval offset)
               $ vars s !? var
    err     -> put err

cnode2nasm Var {var = var, mayassign = Just value} = do
  cnode2nasm value
  get >>= unwrapEitherS
    (\s ->
       maybe
       (let newoffset = 8 + (maximumDef 0 $ P.map fst (elems $ vars s)) in do
           eitherModify $ \s -> s { vars = insert var (newoffset, value) $ vars s}
           stackAssign newoffset )
       (\(offset, _) -> stackAssign offset)
       $ vars s !? var)

cnode2nasm Funcall {funcName = funcName, args = args} =
  case (funcName `elem` ["+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<="], P.length args) of
    (True, 2) -> do cnode2nasm (atDef Void args 0)
                    cnode2nasm (atDef Void args 1)
                    stackExec funcName
                    nl

    (True, _) -> modify $ \_ -> Left UnmatchedArgNum {funcName = funcName, argsgot = args }

    (False, _) -> get >>= unwrapEitherS
      (\s -> do
          if funcName `elem` funcs s
            then modify id
            else eitherModify $ \s -> s {externs = funcName:(externs s)}
          fst6args 0 sysVCallRegs
          mapM_ cnode2nasm (P.reverse $ P.drop (P.length sysVCallRegs) args)
          nl
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
          eitherModify $ \s -> s {accm = accm s ++ ["    pop     " <> (atDef "" registers $ fromIntegral index), ""]}
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
                                  , "    je      Lend" <> lxxx, ""]}
      cnode2nasm stmt
      cnode2nasm finexpr
      eitherModify $ \s -> s {accm = accm s ++
                                  [ "    jmp     Lbegin" <> lxxx
                                  , "Lend" <> lxxx <> ":"]})

cnode2nasm (CtrlStruct (Ret node)) = do
  cnode2nasm node
  eitherModify $ \s -> s {accm = accm s ++ ["; epiloge"]}
  pop "rax"
  mov "rsp" "rbp"
  pop "rbp"
  eitherModify $ \s -> s {accm = accm s ++ ["    ret"]}
  nl

cnode2nasm EOF = get >>= put
cnode2nasm Void = get >>= put

stackExec :: Text -> St.State CStateOrError ()
stackExec func = do
  pop "r11"
  pop "rax"
  matchCommand func "rax" "r11"
  push "rax"
  nl

matchCommand :: Text -> Text -> Text -> St.State CStateOrError ()
matchCommand func arg0 arg1 = eitherModify $ \s ->
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

stackAssign :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackAssign theoffset = do
  pop "rax"
  mov ("qword [rbp-" <> (tshow theoffset) <> "]") "rax"
  push "rax"
  nl

stackLoadLval :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackLoadLval theoffset = do
  mov "rax" ("qword [rbp-" <> (tshow theoffset) <> "]")
  push "rax"
  nl

-- genLVal :: (Integral a, Show a) => a -> St.State CStateOrError ()
-- genLVal theoffset = do
--   mov "rax" "rbp"
--   matchCommand "-" "rax" (tshow theoffset)

push :: Text -> St.State CStateOrError ()
push num = eitherModify $ \s ->
  s {accm = accm s ++ [ "    push    " <> num ]}

pop :: Text -> St.State CStateOrError ()
pop num = eitherModify $ \s ->
  s {accm = accm s ++ [ "    pop     " <> num ]}

mov :: Text -> Text -> St.State CStateOrError ()
mov dest src = eitherModify $ \s ->
  s {accm = accm s ++ [ "    mov     " <> dest <> ", " <> src]}

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
