module Program where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS
import Data.Time.LocalTime


data Program = Program { id :: Integer, name, annotation, version :: String, authorId, typeId, termsOfUseId :: Integer,
                            distUrl ::String } deriving (Show)

createProgram :: IConnection a => String ->String -> String -> Integer -> Integer -> Integer -> String -> a -> IO Bool
createProgram name annotation version authorId typeId termsOfUseId distUrl conn =
    withTransaction conn (createSc' name annotation version authorId typeId termsOfUseId distUrl)

createSc' name annotation version authorId typeId termsOfUseId distUrl conn = do
    changed <- run conn query [SqlString name, SqlString annotation, SqlString version, SqlInteger authorId, SqlInteger typeId, SqlInteger termsOfUseId, SqlString distUrl]
    return $ changed == 1
    where
        query = "insert into program (name, annotation, version, author_id, type_id, terms_of_use_id, dist_url)" ++
            " values (?, ?, ?, ?, ?, ?, ?)"

readProgram :: IConnection a => Integer -> a -> IO [Program]
readProgram id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select name, annotation, version, author_id, type_id, terms_of_use_id, dist_url from program where id = ?"
      toDataObject [SqlInteger uid, SqlByteString name, SqlByteString annotation, SqlByteString version,
               SqlInteger authorId, SqlInteger typeId, SqlInteger termsOfUseId, SqlByteString distUrl] =
        Program uid (BS.unpack name) (BS.unpack annotation) (BS.unpack version) authorId typeId termsOfUseId (BS.unpack distUrl)
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllProgram :: IConnection a => a -> IO [Program]
readAllProgram conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select name, annotation, version, author_id, type_id, terms_of_use_id, dist_url from program order by id"
    toDataObject [SqlInteger uid, SqlByteString name, SqlByteString annotation, SqlByteString version,
                        SqlInteger authorId, SqlInteger typeId, SqlInteger termsOfUseId, SqlByteString distUrl] =
      Program uid (BS.unpack name) (BS.unpack annotation) (BS.unpack version) authorId typeId termsOfUseId (BS.unpack distUrl)
    toDataObject x = error $ "Unexpected result: " ++ show x

updateProgram :: IConnection a => Program -> a -> IO Bool
updateProgram (Program uid name annotation version authorId typeId termsOfUseId distUrl) conn =
    withTransaction conn (updateSd' uid name annotation version authorId typeId termsOfUseId distUrl)

updateSd' uid name annotation version authorId typeId termsOfUseId distUrl conn = do
  changed <- run conn query
                 [SqlString name, SqlString annotation, SqlString version, SqlInteger authorId, SqlInteger typeId,
                                                 SqlInteger termsOfUseId, SqlString distUrl, SqlInteger uid]
  return $ changed == 1
  where
    query = "update program set name=?, annotation=?, version=?, author_id=?, type_id=?, terms_of_use=?, dist_url=? where id = ?"