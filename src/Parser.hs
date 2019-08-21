{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parser (mainParser, unarysAndParsers) where

import           Definition

import           Control.Monad.State        as St
import           Data.Text.Lazy             as T
import           Prelude                    as P
import           Safe
import           Text.Megaparsec            as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug      as MP.Debug

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
-- identifier =
--   spaceConsumer >> rawident <* spaceConsumer
--   where
--     rawident = do
--       head <- try letterChar
--       rest <- many $ choice [try letterChar, try digitChar, try $ single '_']
--       let ident = pack $ head:rest in
--         if ident `elem` reserved
--         then fail "reserved word"
--         else return ident

identifier = lexeme $ check =<<
  (do x  <- letterChar
      xs <- many (try alphaNumChar <|> single '_')
      pure . pack $ x:xs)
  where
    check x = if x `elem` reserved
                then fail $ "keyword " <> show x <> " cannot be an identifier"
                else pure x

semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

lexeme  = L.lexeme spaceConsumer
integer = lexeme L.decimal
-- signedInteger = L.signed spaceConsumer integer

unarysAndParsers :: [(Text, Parser CNode)]
unarysAndParsers = [ ("+", pTerm)
                   , ("-", pTerm)
                   , ("*", pUnary)
                   , ("&", pUnary)
                   ]

mainParser :: Parser [TopLevel]
mainParser = pProgram <* eof

pProgram :: Parser [TopLevel]
pProgram = many pDeclare

pDeclare :: Parser TopLevel
pDeclare = pDeclareFunction

pDeclareFunction :: Parser TopLevel
pDeclareFunction = do
  ctypeName         <- lexeme $ many letterChar
  funcName          <- identifier
  args              <- parens $ pDefVar `sepBy` comma
  Branch definition <- blockStmt
  pure DeFun { ctype      = case ctypeName of
                              "long" -> Long
                              _      -> Unknown
             , funcName   = funcName
             , args       = args
             , definition = definition}

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
           <$> pPtrTo Long
           <*> identifier

pPtrTo :: CType -> Parser CType
pPtrTo ctype = choice [ symbol "*" >> Ptr <$> pPtrTo ctype
                      , pure ctype]

whileStmt = do
  cond <- symbol "while" >> parens (Expression <$> pEquality)
  stmt <- pStatement
  pure $ CtrlStruct Whl {cond = cond, stmt = stmt}

forStmt = do
  init <- symbol "for" >>
          parens (do init    <- option Void (Expression <$> pAssign)
                     _       <- semicolon
                     cond    <- option Void (Expression <$> pEquality)
                     _       <- semicolon
                     finexpr <- option Void (Expression <$> pAssign)
                     pure For { init    = init
                                , cond    = cond
                                , finexpr = finexpr
                                , stmt    = Branch [] })
  stmt <- pStatement
  pure $ CtrlStruct init { stmt = stmt }

ifStmt = do
  cond <- symbol "if" >> parens (Expression <$> pEquality)
  stmt <- pStatement
  elst <- option Void (symbol "else" >> pStatement)
  pure $ CtrlStruct If { cond = cond, stmt = stmt, elst = elst }

blockStmt = Branch <$> braces (many pStatement)

simpleStmt = Expression <$> pExpression <* semicolon

pExpression = choice
              [ pAssign
              , pEquality
              ]

pAssign :: Parser CNode
pAssign = try $ do
  ctype <- pPtrTo Unknown
  var   <- pLhs <* symbol "="
  val   <- pExpression
  pure AssignVar {ctype = ctype, var = var, val = val}

pLhs :: Parser Lhs
pLhs = try $ choice
       [ try $ symbol "*" >> PtrTo <$> pUnary
       , VarName <$> identifier]

pEquality = do
  fstArg <- pRelational
  argAndOps <- MP.many $ MP.try $
               (\op rightArg pointFreeLeftArg ->
                  Funcall { funcName = op
                          , args = [pointFreeLeftArg, rightArg]})
               <$> choice (symbol <$> ["==", "!="])
               <*> pRelational
  pure $ P.foldr (flip (.)) id argAndOps fstArg

pRelational = do
  fstArg <- pAdd
  argAndOps <- MP.many $ MP.try $
               (\op rightArg pointFreeLeftArg ->
                  Funcall { funcName = op
                          , args = [pointFreeLeftArg, rightArg]})
               <$> choice (symbol <$> ["<=", ">=", ">", "<"])
               <*> pAdd
  pure $ P.foldr (flip (.)) id argAndOps fstArg

pAdd = do
  fstArg <- pMul
  argAndOps <- MP.many $ MP.try $
               (\op rightArg pointFreeLeftArg ->
                  Funcall { funcName = op
                          , args = [pointFreeLeftArg, rightArg]})
               <$> choice (symbol <$> ["+", "-"])
               <*> pMul
  pure $ P.foldr (flip (.)) id argAndOps fstArg

pMul = do
  fstArg <- pUnary
  argAndOps <- MP.many $ MP.try $
               (\op rightArg pointFreeLeftArg ->
                  Funcall { funcName = op
                          , args = [pointFreeLeftArg, rightArg]})
               <$> choice (symbol <$> ["*", "/"])
               <*> pUnary
  pure $ P.foldr (flip (.)) id argAndOps fstArg

pUnary = choice $ (genpUnary <$> unarysAndParsers) ++ [pTerm]

genpUnary :: (Text, Parser CNode) -> Parser CNode
genpUnary (sym, parser) =
  (\x -> Funcall {funcName = sym, args = [x]}) <$> (symbol sym >> parser)

pTerm = choice
        [ pFuncall
        , pLoadVar
        , pLong
        , parens pExpression
        ]

pFuncall = try $ do
  func <- identifier
  args <- parens $ option [] (pExpression `sepBy` comma)
  pure Funcall {funcName = func, args = args}

pLoadVar = do
  var <- pLhs
  pure LoadVar {ctype = Unknown, var = var}

pLong = (\num -> Value CValue {ctype = Long, val = num}) <$> try integer
