module Main
  ( FileObject(..)
  , dirSizes
  , main
  , size
  )
  where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (concatMap, filter, many, (:))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow, log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional, try)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (intDecimal)

data FileObject = Dir (Array FileObject) | File Int

derive instance genericFileObject :: Generic FileObject _

instance showFileObject :: Show FileObject where show x = genericShow x

isDir :: FileObject -> Boolean
isDir (Dir _) = true
isDir (File _) = false

size :: FileObject -> Int
size (Dir a) =  sum $ map size a
size (File f) = f

dirSizes :: FileObject -> Array Int
dirSizes (File _) = []
dirSizes f@(Dir fs) = (size f) : (concatMap dirSizes $ filter isDir fs)

cdParser :: Parser String FileObject
cdParser = defer \_ -> do
  _ <- string "$ cd "
  _dirName <- anyTill $ char '\n'
  files <- lsParser
  dirs <- many $ try cdParser
  _ <- optional $ string "$ cd ..\n"
  pure (Dir $ files <> dirs)

lsParser :: Parser String (Array FileObject)
lsParser = do
  _ <- string "$ ls\n"
  fs <- many $ fileParser <|> dirParser
  pure (filter (not isDir) fs)
  where
    fileParser = do
      fileSize <- intDecimal
      _ <- char ' '
      _fileName <- anyTill $ char '\n'
      pure (File fileSize)
    dirParser = do
      _ <- string "dir "
      _dirName <- anyTill $ char '\n'
      pure (Dir [])

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input cdParser of
    Right fileObj -> do
      log "Part 1"
      let smallEnough x = x <= 100000
      let smallDirs = filter smallEnough $ dirSizes fileObj
      log $ "Sum of small enough dir sizes: " <> (show $ sum smallDirs)
    Left msg -> logShow msg
