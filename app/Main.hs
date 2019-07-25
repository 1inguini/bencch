module Main where

-- import Lib
import qualified System.Environment as Env

main :: IO ()
main = do
  input <- head <$> Env.getArgs

  putStrLn "define i32 @main(i32, i8**) {"
  putStrLn $ "  ret i32 " ++ input
  putStrLn "}"
