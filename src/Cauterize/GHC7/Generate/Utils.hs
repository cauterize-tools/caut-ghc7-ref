module Cauterize.GHC7.Generate.Utils
       ( createGuard
       , createPath
       , createDirIfNotExist
       , nameToHsName
       , nameToHsVar
       , identToHsName
       , unpackIdent
       , identToPrimName
       , identToHsVar
       ) where

import System.Directory
import System.FilePath.Posix
import Control.Monad
import qualified Cauterize.CommonTypes as CT
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = createDirIfNotExist out >> go

createPath :: [FilePath] -> IO FilePath
createPath = go ""
  where
    go p [] = return p
    go root (p:ps) = do
      let combined = root `combine` p
      createDirIfNotExist combined
      go combined ps

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist out = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else unless de $ createDirectory out

nameToHsName :: T.Text -> String
nameToHsName n = T.unpack $ T.concat $ map T.toTitle $ T.split (== '_') n

nameToHsVar :: T.Text -> String
nameToHsVar n =
  let f:rs = T.split (== '_') n
  in T.unpack $ T.concat $ f:(map T.toTitle rs)

identToHsName :: CT.Identifier -> String
identToHsName ident = fromMaybe (nameToHsName (CT.unIdentifier ident))
                                (identToPrimName ident)

identToHsVar :: CT.Identifier -> String
identToHsVar ident = nameToHsVar (CT.unIdentifier ident)

unpackIdent :: CT.Identifier -> String
unpackIdent ident = T.unpack (CT.unIdentifier ident)

identToPrimName :: CT.Identifier -> Maybe String
identToPrimName n =
  let n' = T.unpack $ CT.unIdentifier n
  in case n' of
       "u8"   -> Just "Word8"
       "u16"  -> Just "Word16"
       "u32"  -> Just "Word32"
       "u64"  -> Just "Word64"
       "s8"   -> Just "Int8"
       "s16"  -> Just "Int16"
       "s32"  -> Just "Int32"
       "s64"  -> Just "Int64"
       "f32"  -> Just "Float"
       "f64"  -> Just "Double"
       "bool" -> Just "Bool"
       _      -> Nothing
