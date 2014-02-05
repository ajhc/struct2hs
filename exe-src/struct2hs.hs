module Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Typeable
import System.Environment
import Language.C.Analysis
import Language.C.Pretty
import Language.C.Data.Ident

import Parser

{--
Usage:
$ pwd
/home/kiwamu/src/netbsd-arafura-s1
$ struct2hs $HOME/src/netbsd-arafura-s1/obj/tooldir/bin/i486--netbsdelf-gcc "-Di386 -I$HOME/src/netbsd-arafura-s1/sys/arch/i386/compile/obj/GENERIC_HS -I$HOME/src/netbsd-arafura-s1/common/include -I$HOME/src/netbsd-arafura-s1/sys/arch -I$HOME/src/netbsd-arafura-s1/sys -nostdinc -I$HOME/src/netbsd-arafura-s1/metasepi/sys/hsbuild/cbits -I$HOME/src/netbsd-arafura-s1/metasepi/sys/hsbuild -I$HOME/src/netbsd-arafura-s1/metasepi/sys/hsdummy -DMAXUSERS=64 -D_KERNEL -D_KERNEL_OPT -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_CONC=_JHC_CONC_CUSTOM -D_JHC_USE_OWN_STDIO -D_JHC_STANDALONE=0 -I$HOME/src/netbsd-arafura-s1/common/lib/libc/quad -I$HOME/src/netbsd-arafura-s1/common/lib/libc/string -I$HOME/src/netbsd-arafura-s1/common/lib/libc/arch/i386/string -I$HOME/src/netbsd-arafura-s1/sys/dist/ipf -I$HOME/src/netbsd-arafura-s1/sys/external/isc/atheros_hal/dist -I$HOME/src/netbsd-arafura-s1/sys/external/isc/atheros_hal/ic -I$HOME/src/netbsd-arafura-s1/sys/external/bsd/drm/dist/bsd-core -I$HOME/src/netbsd-arafura-s1/sys/external/bsd/drm/dist/shared-core -I$HOME/src/netbsd-arafura-s1/common/include -I$HOME/src/netbsd-arafura-s1/sys/external/bsd/acpica/dist/include" sys/dev/pci/auich_extern_SNATCHED.h
--}

cToHsName :: String -> String
cToHsName = concat . fmap (\(x:xs) -> toUpper x:xs) . filter (not . null) . fmap (filter isAlphaNum) . groupBy (\a b -> isAlphaNum a && isAlphaNum b)

cToHsTypeName :: TypeName -> Maybe String
cToHsTypeName TyVoid                 = Just "()"
cToHsTypeName (TyIntegral TyBool)    = Just "Bool"
cToHsTypeName (TyIntegral TyChar)    = Just "Char"
cToHsTypeName (TyIntegral TySChar)   = Just "CSChar"
cToHsTypeName (TyIntegral TyUChar)   = Just "CUChar"
cToHsTypeName (TyIntegral TyShort)   = Just "CShort"
cToHsTypeName (TyIntegral TyUShort)  = Just "CUShort"
cToHsTypeName (TyIntegral TyInt)     = Just "Int"
cToHsTypeName (TyIntegral TyUInt)    = Just "CUInt"
cToHsTypeName (TyIntegral TyLong)    = Just "CLong"
cToHsTypeName (TyIntegral TyULong)   = Just "CULong"
cToHsTypeName (TyIntegral TyLLong)   = Just "CLLong"
cToHsTypeName (TyIntegral TyULLong)  = Just "CULLong"
cToHsTypeName (TyFloating TyFloat)   = Just "Float"
cToHsTypeName (TyFloating TyDouble)  = Just "Double"
cToHsTypeName (TyFloating TyLDouble) = Just "CLDouble"
cToHsTypeName (TyComp (CompTypeRef (NamedRef (Ident name _ _)) StructTag _)) = Just $ cToHsName name
cToHsTypeName _                      = Nothing

showParam :: ParamDecl -> String
showParam (ParamDecl (VarDecl _ _ t) _)         = cToHsType t
showParam (AbstractParamDecl (VarDecl _ _ t) _) = cToHsType t

findFunPtr :: Type -> [Type]
findFunPtr (PtrType t _ _) = findFunPtr t
findFunPtr t@(FunctionType _ _) = [t]
findFunPtr _ = []

stripFunPtr :: Type -> Maybe String
stripFunPtr (FunctionType (FunType rt para _) _) =
  let ret = fmap ("IO " ++) $ cToHsType' rt
      args = concat $ zipWith (++) (fmap showParam para) $ repeat " -> "
  in fmap (args ++) $ ret
stripFunPtr _ = Nothing

cToHsType :: Type -> String
cToHsType t = fromMaybe (show $ pretty t) $ cToHsType' t

cToHsType' :: Type -> Maybe String
cToHsType' (DirectType tn _ _) = cToHsTypeName tn
cToHsType' (PtrType t _ _) = fmap (\a -> "(Ptr " ++ a ++ ")") $ cToHsType' t
cToHsType' t@(FunctionType _ _) = fmap (\a -> "(FunPtr (" ++ a ++ "))") $ stripFunPtr t
cToHsType' (ArrayType t _ _ _) = cToHsType' t
cToHsType' _ = Nothing

showNewType :: String -> String
showNewType stn = let hsStn = cToHsName stn in
  "newtype {-# CTYPE \"struct " ++ stn ++ "\" #-} " ++ hsStn ++ " = " ++ hsStn ++ " ()"

showSizeOf :: String -> String
showSizeOf stn = let hsStn = cToHsName stn in
  "foreign import primitive \"const.sizeof(" ++ "struct " ++ stn ++
  ")\"\n  sizeOf_" ++ hsStn ++ " :: Int"

showFfiDynamic :: String -> (String, Type) -> String
showFfiDynamic stn (n, t) =
  let hsStn = cToHsName stn
      f s = "\nforeign import ccall \"dynamic\" call_" ++ hsStn ++ "_" ++ n ++ " ::\n" ++
            "  FunPtr (" ++ s  ++ ") -> " ++ s
  in concatMap f $ catMaybes $ fmap stripFunPtr $ findFunPtr t

showAccessor :: String -> (String, Type) -> String
showAccessor stn (n, t) = let hsStn = cToHsName stn in
  "p_" ++ hsStn ++ "_" ++ n ++ " :: Ptr " ++ hsStn ++ " -> IO (Ptr " ++ cToHsType t ++ ")\n" ++
  "p_" ++ hsStn ++ "_" ++ n ++ " p = return $ plusPtr p offsetOf_" ++ hsStn ++ "_" ++ n

showMember :: String -> (String, Type) -> String
showMember stn (n, t) = let hsStn = cToHsName stn in
  "foreign import primitive \"const.offsetof(" ++ "struct " ++ stn ++ ", " ++ n ++
  ")\"\n  offsetOf_" ++ hsStn ++ "_" ++ n ++ " :: Int\n" ++
  showAccessor stn (n, t) ++ showFfiDynamic stn (n, t)

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

onlyStruct :: TagDef -> Bool
onlyStruct (CompDef (CompType _ StructTag _ _ _)) = True
onlyStruct _ = False

validMembers :: [MemberDecl] -> [(String, Type)]
validMembers members = mapMaybe go members
  where go (MemberDecl (VarDecl (VarName (Ident name _ _) _) _ t) _ _) = Just (name, t)
        go _                                                           = Nothing

showForeignPrim :: TagDef -> String
showForeignPrim (CompDef (CompType (NamedRef (Ident name _ _)) StructTag members _ _)) =
  let ms = validMembers members
  in unlines $ showNewType name : showSizeOf name : map (showMember name) ms
showForeignPrim td = "*** Not showable struct: " ++ (show . pretty) td

main :: IO ()
main = do
  let usage = error "Usage: struct2hs GCC_PATH GCC_OPTIONS C_HEADER_PATH"
  args <- getArgs
  when (length args /= 3) usage
  let [gcc,opts,fn] = args
  ast <- parseFile' gcc opts fn
  (globals,_warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
  -- print _warnings
  let structs = filter onlyStruct $ Map.elems $ gTags globals
  mapM_ (putStrLn . showForeignPrim) structs
  return ()
