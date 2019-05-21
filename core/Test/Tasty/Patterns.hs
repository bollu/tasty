-- | Test patterns

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Test.Tasty.Patterns
  ( TestWhitelist(..)
  , parseExpr
  , parseTestPattern
  , noPattern
  , Path
  , exprMatches
  , testPatternMatches
  ) where

import Test.Tasty.Options
import Test.Tasty.Patterns.Types
import Test.Tasty.Patterns.Parser
import Test.Tasty.Patterns.Eval

import Data.Char
import Data.Typeable
import Options.Applicative hiding (Success)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

newtype TestWhitelist = TestWhitelist (Maybe Expr)
  deriving (Typeable, Show, Eq)

noPattern :: TestWhitelist
noPattern = TestWhitelist Nothing

instance IsOption TestWhitelist where
  defaultValue = noPattern
  parseValue = parseTestPattern
  optionName = return "pattern"
  optionHelp = return "Select only tests which satisfy a pattern or awk expression"
  optionCLParser = mkOptionCLParser (short 'p' <> metavar "PATTERN")

parseExpr :: String -> Maybe Expr
parseExpr s
  | all (\c -> isAlphaNum c || c `elem` "._- ") s =
    Just $ ERE s
  | otherwise = parseAwkExpr s

parseTestPattern :: String -> Maybe TestWhitelist
parseTestPattern s
  | null s = Just noPattern
  | otherwise = TestWhitelist . Just <$> parseExpr s

exprMatches :: Expr -> Path -> Bool
exprMatches e fields =
  case withFields fields $ asB =<< eval e of
    Left msg -> error msg
    Right b -> b

testPatternMatches :: TestWhitelist -> Path -> Bool
testPatternMatches pat fields =
  case pat of
    TestWhitelist Nothing -> True
    TestWhitelist (Just e) -> exprMatches e fields
