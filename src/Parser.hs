module Parser where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Language.C
import Language.C.Data.InputStream
import Language.C.Data.Name
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Syntax.AST
import Language.C.System.GCC

parseFile :: FilePath -> IO (Either ParseError CTranslUnit)
parseFile fn = do
  con <- readFile fn
  let pos = position 0 fn 0 0
      istr = inputStreamFromString $
        unlines $ filter (not . cppLine) $ lines con
      cppLine :: String -> Bool
      cppLine ('#':_) = True
      cppLine _ = False

      bitns = map builtinIdent 
        ["size_t", "size_okabe", "size_nushio"]

  return $ fmap fst $ execParser translUnitP istr pos bitns newNameSupply

parseFiles :: [FilePath] -> IO (Either ParseError [CTranslUnit])
parseFiles fns = do
  runEitherT $ mapM EitherT $ map parseFile fns

parseFile' :: FilePath -> String -> FilePath -> IO CTranslUnit
parseFile' gcc opts fn = do
  r <- parseCFile (newGCC gcc) Nothing (words opts) fn
  either (error . show) return $ r
