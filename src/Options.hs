module Options where

import System.Console.GetOpt

import Data.Maybe

data Cfg = Cfg {
  filename :: Maybe String
}

data Option

optDescrs :: [ OptDescr Option ]
optDescrs = []

parseArgs :: [String] -> Cfg
parseArgs args = Cfg {
    filename = listToMaybe positionals
  }
  where
    (optionals, positionals, errors) = getOpt Permute optDescrs args
