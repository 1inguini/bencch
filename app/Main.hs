{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           NasmGen
import           Parser

import           Control.Monad.State   as St
import           Data.Either           as E
import qualified Data.List             as List
import           Data.Text.Lazy        as T
import           Prelude               as P
import           System.Environment    as Env
import           Text.Megaparsec       as MP
import qualified Text.Megaparsec.Error as MP.E
import qualified Text.Pretty.Simple    as PrettyS

main :: IO ()
main = do
  input <- pack . List.head <$> Env.getArgs
  case parse mainParser "" input of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow $ parsed
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
