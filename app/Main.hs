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
import qualified Data.Set                   as Set
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

data CNode = Branch     [CNode]
           | Value      CValue
           | DefVar     {defType :: CType, varName :: Text}
           | AssignVar  {var :: Lhs, val :: CNode}
           | LoadVar    {var :: Lhs}
           | Expression CNode
           | CtrlStruct CtrlStruct
           | Funcall    {funcName :: Text, args :: [CNode]}
           | Void
           | EOF
           deriving  (Show, Eq)

data Lhs = VarName Text
         | PtrTo   CNode
         -- | CNode   CNode
         deriving (Show, Eq)

data CValue = CValue {ctype :: CType, val :: Integer}
            deriving (Show, Eq)

data CType = CLong
           | Ptr     CType
           | Unknown
           deriving  (Show, Eq)

data TopLevel = DeFun { ctype :: CType, funcName :: Text, args :: [CNode], definition :: [CNode] }
              deriving  (Show, Eq)

type Offset = Integer
type Variables = Map Text (CType, Offset)

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
symbol = L.symbol spaceConsumer

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
      head <- try letterChar
      rest <- many $ choice [try letterChar, try digitChar, try $ single '_']
      let ident = pack $ head:rest in
        if ident `elem` reserved
        then fail "reserved word"
        else return ident

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
  ctypeName           <- spaceConsumer >> many letterChar
  funcName            <- identifier
  args                <- parens $ pDefVar `sepBy` comma
  (Branch definition) <- blockStmt
  return DeFun { ctype = case ctypeName of
                           "long" -> CLong
                           _      -> Unknown
               , funcName = funcName, args = args, definition = definition}

pStatement, pExpression, pDefVar, pDefLong, pLoadVar, pEquality, pRelational, pAdd, pMul, pTerm, pLong, pUnary, pFuncall
  :: Parser CNode

pStatement = choice
             [ returnStmt
             , defStmt
             , whileStmt
             , forStmt
             , ifStmt
             , blockStmt
             , simpleStmt
             ]

reserved :: [Text]
reserved = ["return", "long", "while", "for", "if", "else"]

returnStmt, defStmt, whileStmt, forStmt, ifStmt, blockStmt, simpleStmt :: Parser CNode

returnStmt = symbol "return" >> CtrlStruct . Ret <$> simpleStmt

defStmt = pDefVar <* semicolon

pDefVar = pDefLong

pDefLong = symbol "long" >>
           DefVar
           <$> pPtrTo CLong
           <*> identifier

pPtrTo :: CType -> Parser CType
pPtrTo ctype = choice [ symbol "*" >> Ptr <$> pPtrTo ctype
                      , return ctype]

whileStmt = do
  cond <- symbol "while" >> parens (Expression <$> pEquality)
  stmt <- pStatement
  return $ CtrlStruct Whl {cond = cond, stmt = stmt}

forStmt = do
  init <- symbol "for" >>
          parens (do init    <- option Void (Expression <$> pAssign)
                     _       <- semicolon
                     cond    <- option Void (Expression <$> pEquality)
                     _       <- semicolon
                     finexpr <- option Void (Expression <$> pAssign)
                     return For { init    = init
                                , cond    = cond
                                , finexpr = finexpr
                                , stmt    = Branch [] })
  stmt <- pStatement
  return $ CtrlStruct init { stmt = stmt }

ifStmt = do
  cond <- symbol "if" >> parens (Expression <$> pEquality)
  stmt <- pStatement
  elst <- option Void (symbol "else" >> pStatement)
  return $ CtrlStruct If { cond = cond, stmt = stmt, elst = elst }

blockStmt = Branch <$> (braces $ many pStatement)

simpleStmt = Expression <$> pExpression <* semicolon

pExpression = choice
              [ pAssign
              , pEquality
              ]

pAssign :: Parser CNode
pAssign = try $ -- do
          AssignVar
          <$> pLhs <* symbol "="
          <*> pExpression
  -- ctype <- pPtrTo Unknown
  -- var   <- identifier <* symbol "="
  -- val   <- pExpression
  -- return AssignVar {ctype = ctype, var = var, val = val}

pLhs :: Parser Lhs
pLhs = try $ choice
       [ try $ symbol "*" >> PtrTo <$> pUnary
       , VarName <$> identifier]
  -- where
  --   pPtr = genpUnary ("*", pTerm)

pEquality = do
  arg0 <- pRelational
  option (arg0) (do eq   <- choice $ symbol <$> ["==", "!="]
                    args <- pExpression
                    return Funcall {funcName = eq, args = [arg0, args]})

pRelational = do
  arg0 <- pAdd
  option (arg0) (do comp <- choice $ symbol <$> ["<=", ">=", ">", "<"]
                    args <- pExpression
                    return Funcall {funcName = comp, args = [arg0, args]})

pAdd = do
  arg0 <- pMul
  option (arg0) (do arth <- choice $ symbol <$> ["+", "-"]
                    args <- pExpression
                    return Funcall {funcName = arth, args = [arg0, args]})

pMul = do
  arg0 <- pUnary
  option (arg0) (do arth <- choice $ symbol <$> ["*", "/"]
                    args <- pExpression
                    return Funcall {funcName = arth, args = [arg0, args]})

pUnary = choice $ (genpUnary <$> unarysAndParsers) ++ [pTerm]

genpUnary :: (Text, Parser CNode) -> Parser CNode
genpUnary (sym, parser) =
  (\x -> Funcall {funcName = sym, args = [x]}) <$> (symbol sym >> parser)

unarys :: [Text]
unarys = fst <$> unarysAndParsers

unarysAndParsers :: [(Text, Parser CNode)]
unarysAndParsers = [ ("+", pTerm)
                   , ("-", pTerm)
                   , ("*", pUnary)
                   , ("&", pUnary)
                   ]

pTerm = choice
        [ pFuncall
        , pLoadVar
        , pLong
        , parens pExpression
        ]

pFuncall = try $ do
  func <- identifier
  args <- parens $ option [] (pExpression `sepBy` comma)
  return Funcall {funcName = func, args = args}

pLoadVar = LoadVar <$> pLhs

pLong = (\num -> Value CValue {ctype = CLong, val = num}) <$> try integer

data CState = CState
            { stnum   :: Integer
            , funcs   :: [Text]
            , externs :: [Text]
            , vars    :: Variables
            , accm    :: [Text]
            , defName :: Text
            } deriving (Show, Eq)

data NasmGenError = VariableNotDefined     Lhs
                  | MultipleDeclaration    Text
                  | VariableNotInitialized {var :: Lhs}
                  | VariableUnknownType    {var :: Lhs, mayval :: Maybe CNode}
                  | UnknownCValue          CValue
                  | UnmatchedType          {var :: Lhs, oldCtype, newCtype :: CType}
                  | UnmatchedArgNum        {funcName :: Text, argsgot :: [CNode]}
                  deriving (Show, Eq)

type CStateOrError = Either (CState, CNode, NasmGenError) CState

throwNasmGenError :: CNode -> NasmGenError -> St.State CStateOrError ()
throwNasmGenError cnode nasmGenError = unwrapGet $ \s -> (put . Left) (s, cnode, nasmGenError)

sysVCallRegs :: [Text]
sysVCallRegs = [ "rdi", "rsi", "rdx", "rcx", "r8", "r9"]

biops :: [Text]
biops = ["+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<="]

program2nasm :: [TopLevel] -> Either (CState, CNode, NasmGenError) [Text]
program2nasm tops =
  let
    toText :: CState -> [Text]
    toText (CState {accm = accm}) = accm
    genExtern :: CState -> [Text]
    genExtern (CState {externs = externs}) = (\func -> "extern " <> func) <$> externs
    eitherCState = execState (mapM_ toplevel2nasm tops)
                   $ Right CState { stnum   = 0
                                  , funcs   = []
                                  , externs = []
                                  , vars    = Map.empty
                                  , accm    = []
                                  , defName = T.empty }
  in
  case eitherCState of
    Right cstate -> Right $ genExtern cstate ++
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
    Left err     -> Left err

toplevel2nasm :: TopLevel -> St.State CStateOrError ()
toplevel2nasm DeFun {funcName = funcName, args = args, definition = definition} = do
  eitherModify $ \s -> s { vars = Map.empty }
  eitherModify $ \s -> s { defName = funcName
                         , funcs = funcName:(funcs s)
                         }
  appendAccm [ funcName <> ":"
             , "; prologe"]
  push "rbp"
  mov "rbp" "rsp"
  mapM_ genLocals (args ++ definition)
  unwrapGet $ \s -> do matchCommand "-" "rsp" (tshow (maximumDef 0 $ snd <$> (elems $ vars s)))
                       eitherModify $ \s -> s {vars = Map.empty}
  nl
  mapM_ genLocals (args ++ definition)
  assign6Args 0 (P.take (P.length args) sysVCallRegs)
  assignRestArgs (P.length sysVCallRegs) (P.drop (P.length sysVCallRegs) args)
  mapM_ cnode2nasm definition
  where
    assign6Args :: (Show a, Integral a) => a -> [Text] -> St.State CStateOrError ()
    assign6Args _ [] = modify id
    assign6Args index (reg:registers) = do
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
genLocals n@DefVar {defType = Ptr ctype} =
  genLocals n {defType = ctype}
genLocals n@DefVar {defType = ctype, varName = varName} =
  unwrapGet $ \s ->
    maybe
    (let newoffset = 8 + (maximumDef 0 $ snd <$> (elems $ vars s)) in do
        eitherModify $ \s -> s { vars = insert varName (ctype, newoffset) $ vars s})
    (\_ -> throwNasmGenError n $ MultipleDeclaration varName)
    $ vars s !? varName

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
cnode2nasm (Branch []) = modify id
cnode2nasm (Branch nodeList) =
  mapM_ cnode2nasm nodeList
cnode2nasm (Expression node) = do
  appendAccm [ "; statement start"]
  cnode2nasm node
  pop "rax"
  appendAccm [ "; statement end"]
  eitherModify $ \s -> s { stnum = stnum s + 1 }
  nl

cnode2nasm (Value CValue {ctype = CLong, val = num}) = push (tshow num)
cnode2nasm n@(Value CValue {ctype = Ptr ctype})      = stackUnary "*" n {defType = ctype}
cnode2nasm n@(Value cval@CValue {ctype = Unknown})   = throwNasmGenError n $ UnknownCValue cval

-- cnode2nasm n@DefVar {defType = Ptr ctype} = cnode2nasm n {defType = ctype}
cnode2nasm n@DefVar {defType = ctype, varName = varName} = do
  unwrapGet
    (\s ->
       maybe
       (let newoffset = 8 + (maximumDef 0 $ snd <$> (elems $ vars s)) in do
           eitherModify $ \s -> s { vars = insert varName (ctype, newoffset) $ vars s}
           appendAccm [ "    ; " <> tshow n ]
           nl)
       (\_ -> modify id -- throwNasmGenError n $ MultipleDeclaration varName
       )
       $ vars s !? varName)

cnode2nasm n@AssignVar {var = PtrTo cnode, val = val} =  do
  s <- get
  case s of
    Right s -> do cnode2nasm cnode
                  cnode2nasm val
                  pop "rcx"
                  pop "rax"
                  mov "[rax]" "rcx"
                  push "rcx"
                  nl
    err     -> put err

cnode2nasm n@AssignVar {var = varname@(VarName var), val = val} = do
  s <- get
  case s of
    Right s -> maybe
               (throwNasmGenError n $ VariableNotDefined varname)
               (\(ctype, offset) -> -- case ctype of
                                      -- Ptr ctype -> stackUnary "*" n {}
                                      -- _         -> cnode2nasm value
                   cnode2nasm val >> stackAssign offset)
               $ vars s !? var
    err     -> put err

-- cnode2nasm n@(LoadVar {var = PtrTo var}) = do
cnode2nasm n@(LoadVar {var = var@(VarName varName)}) = do
  s <- get
  case s of
    Right s -> maybe
               (throwNasmGenError n $ VariableNotDefined var)
               (\(_, offset) -> stackLoadLval offset)
               $ vars s !? varName
    err     -> put err

cnode2nasm n@Funcall {funcName = funcName, args = args} =
  case ( funcName `elem` biops
       , funcName `elem` unarys
       , P.length args) of
    (True, _, 2) -> stackExec funcName (atDef Void args 0) (atDef Void args 1)

    (_, True, 1) -> stackUnary funcName (atDef Void args 0)

    (True, _, _) -> throwNasmGenError n $ UnmatchedArgNum {funcName = funcName, argsgot = args }
    (_, True, _) -> throwNasmGenError n $ UnmatchedArgNum {funcName = funcName, argsgot = args }

    (False, _, _) -> unwrapGet $ \s -> do
      if funcName `elem` funcs s
        then modify id
        else eitherModify $ \s -> s {externs = funcName:(externs s)}
      fst6args 0 sysVCallRegs
      mapM_ cnode2nasm (P.reverse $ P.drop (P.length sysVCallRegs) args)
      appendAccm [ "    call    " <> funcName]
      push "rax"
      nl
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
  unwrapGet $ \s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    -- pop "rax"
    appendAccm [ "    cmp     rax, 0"
               , "    je      Lend" <> lxxx
               ,""]
    cnode2nasm stmt
    appendAccm [ "Lend" <> lxxx <> ":" ]

cnode2nasm (CtrlStruct (If {cond = cond, stmt = stmt, elst = elst})) =
  unwrapGet $ \s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm cond
    -- pop "rax"
    appendAccm [ "    cmp     rax, 0"
               , "    je      Lelse" <> lxxx ]
    nl
    cnode2nasm stmt
    appendAccm [ "    jmp     Lend" <> lxxx
               , "Lelse" <> lxxx <> ":" ]
    cnode2nasm elst
    appendAccm [ "Lend" <> lxxx <> ":" ]

cnode2nasm (CtrlStruct (Whl {cond = cond, stmt = stmt})) =
  unwrapGet $ \s ->
  let lxxx = (tshow . stnum) s in do
    eitherModify $ \s -> s {accm = accm s ++ ("Lbegin" <> lxxx <> ":"):[]}
    cnode2nasm cond
    -- pop "rax"
    appendAccm [ "    cmp     rax, 0"
               , "    je      Lend" <> lxxx ]
    nl
    cnode2nasm stmt
    appendAccm [ "    jmp     Lbegin" <> lxxx
               , "Lend" <> lxxx <> ":"]

cnode2nasm (CtrlStruct (For {init = init, cond = cond, finexpr = finexpr, stmt = stmt})) =
  unwrapGet $ \s ->
  let lxxx = (tshow . stnum) s in do
    cnode2nasm init
    eitherModify $ \s -> s {accm = accm s ++ ["Lbegin" <> lxxx <> ":"]}
    cnode2nasm cond
    -- pop "rax"
    appendAccm [ "    cmp     rax, 0"
               , "    je      Lend" <> lxxx]
    nl
    cnode2nasm stmt
    cnode2nasm finexpr
    appendAccm [ "    jmp     Lbegin" <> lxxx
               , "Lend" <> lxxx <> ":"]

cnode2nasm (CtrlStruct (Ret node)) = do
  cnode2nasm node
  appendAccm ["; epiloge"]
  -- pop "rax"
  mov "rsp" "rbp"
  pop "rbp"
  appendAccm ["    ret"]
  nl

cnode2nasm EOF  = modify id
cnode2nasm Void = modify id

eitherModify :: (CState -> CState) -> St.State CStateOrError ()
eitherModify f = modify (fmap f)

unwrapEitherS :: (CState -> St.State CStateOrError ()) -> CStateOrError -> St.State CStateOrError ()
unwrapEitherS m (Right s)    = m s
unwrapEitherS _ err@(Left _) = put err

unwrapGet :: (CState -> St.State CStateOrError ()) -> St.State CStateOrError ()
unwrapGet f = get >>= unwrapEitherS f

appendAccm :: [Text] -> St.State CStateOrError ()
appendAccm texts = eitherModify $ \s -> s {accm = accm s ++ texts}

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
matchCommand func arg0 arg1 =
  appendAccm $ case func of
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
                 _    -> []

stackUnary :: Text -> CNode -> St.State CStateOrError ()
stackUnary func arg =
            case func of
              "+" -> modify id
              "-" -> cnode2nasm arg >> pop "rax" >> nasmNeg "rax" >> push "rax"
              "*" -> cnode2nasm arg >> pop "rax" >> push "[rax]"
              "&" -> do s <- get
                        case (arg, s) of
                          -- (n@(LoadVar {ctype = Ptr ctype ,var = var}), Right s) ->
                          --   stackUnary "*" n {ctype = ctype}
                          (n@(LoadVar {var = var@(VarName varName)}), Right s) ->
                            maybe
                            (throwNasmGenError n $ VariableNotDefined var)
                            (\(ctype, offset) -> case ctype of
                                                   -- Ptr ctype -> stackUnary "*" n {}
                                                   ctype     -> leaOffset "rax" offset >> push "rax")
                            $ vars s !? varName

                          (n@(LoadVar {var = var}), Right s)  -> throwNasmGenError n $ VariableNotDefined var
                          (_, err@(Left _)) -> put err
              _   -> modify id
            >> nl

nasmNeg :: Text -> St.State CStateOrError ()
nasmNeg arg = appendAccm [ "    neg     " <> arg ]

stackAssign :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackAssign theoffset = do
  pop ("[rbp-" <> tshow theoffset <> "]")
  push ("[rbp-" <> tshow theoffset <> "]")
  nl

stackLoadLval :: (Integral a, Show a) => a -> St.State CStateOrError ()
stackLoadLval theoffset = do
  push ("[rbp-" <> tshow theoffset <> "]")
  nl

-- genLVal :: (Integral a, Show a) => a -> St.State CStateOrError ()
-- genLVal theoffset = do
--   mov "rax" "rbp"
--   matchCommand "-" "rax" (tshow theoffset)

push :: Text -> St.State CStateOrError ()
push num =
  appendAccm [ "    push    qword " <> num ]

pop :: Text -> St.State CStateOrError ()
pop num =
  appendAccm [ "    pop     qword " <> num ]

mov :: Text -> Text -> St.State CStateOrError ()
mov dest src =
  appendAccm [ "    mov     " <> dest <> ", qword " <> src]

lea :: Text -> Text -> St.State CStateOrError ()
lea dest src =
  appendAccm [ "    lea     " <> dest <> ", [" <> src <> "]"]

leaOffset :: (Integral a, Show a) => Text -> a -> St.State CStateOrError ()
leaOffset dest offset = lea dest ("rbp-" <> tshow offset)

nl :: St.State CStateOrError ()
nl = appendAccm [ "" ]

main :: IO ()
main = do
  input <- pack . List.head <$> Env.getArgs
  case parse mainParser "" input of
    Right parsed -> do
      -- putStrLn . unpack . PrettyS.pShow $ parsed
      case program2nasm parsed of
        Right nasm -> mapM_ (putStrLn . unpack) $ nasm
        Left err   -> putStrLn . unpack . PrettyS.pShow $ err
    Left err  -> putStrLn $ MP.E.errorBundlePretty err

debugmain :: String -> IO ()
debugmain input =
    case parse mainParser "" (pack input) of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow $ parsed
      -- case program2nasm parsed of
      --   Right nasm -> mapM_ (putStrLn . unpack) $ nasm
      --   Left err   -> putStrLn . unpack . PrettyS.pShow $ err
    Left err  -> putStrLn $ MP.E.errorBundlePretty err
