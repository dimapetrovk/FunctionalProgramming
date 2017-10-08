{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "haskell" }

  putStrLn "Enter a name"
  name <- getLine
  putStrLn "author"
  author <- getLine
  putStrLn "annotation"
  annotation <- getLine
  putStrLn "version"
  version <- getLine
  putStrLn "category"
  category <- getLine
  putStrLn "condition"
  condition <- getLine
  execute conn "insert into words (word, definition) values (?, ?)" $ Software (Just name) (Just author) (Just annotation) (Just version) (Just category) (Just condition)