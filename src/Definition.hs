{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Definition where

import           Control.Monad.State as St
import           Control.Monad.State as St
import           Data.Either         as E
import           Data.Either         as E
import           Data.Map.Strict     as Map
import           Data.Map.Strict     as Map
import           Data.Maybe          as May
import           Data.Maybe          as May
import           Data.Text.Lazy      as T
import           Data.Text.Lazy      as T
import qualified Data.Void           as V
import           Prelude             as P
import           Prelude             as P
import           Safe                as Safe
import           Text.Megaparsec     as MP

type Parser = Parsec V.Void Text

data CNode = Branch     [CNode]
           | Value      CValue
           | DefVar     { defType :: CType
                        , varName :: Text }
           | AssignVar  { ctype :: CType
                        , var   :: Lhs
                        , val   :: CNode }
           | LoadVar    { ctype :: CType
                        , var   :: Lhs }
           | Expression CNode
           | CtrlStruct CtrlStruct
           | Funcall    { funcName :: Text
                        , args     :: [CNode]}
           | Void
           | EOF
           deriving  (Show, Eq)

data Lhs = VarName Text
         | PtrTo   CNode
         deriving (Show, Eq)

data CValue = CValue { ctype :: CType
                     , val   :: Integer }
            deriving (Show, Eq)

data CType = Long
           | Ptr     CType
           | Unknown
           deriving  (Show, Eq)

data TopLevel = DeFun { ctype      :: CType
                      , funcName   :: Text
                      , args       :: [CNode]
                      , definition :: [CNode] }
              deriving  (Show, Eq)

type Offset = Integer
type Variables = Map Text (CType, Offset)

data CtrlStruct = If  { cond :: CNode
                      , stmt :: CNode
                      , elst :: CNode }

                | Whl { cond :: CNode
                      , stmt :: CNode}

                | For { init    :: CNode
                      , cond    :: CNode
                      , finexpr :: CNode
                      , stmt    :: CNode }

                | Ret CNode
                deriving (Show, Eq)


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

unarys :: [Text]
unarys = ["+", "-", "*", "&"]
