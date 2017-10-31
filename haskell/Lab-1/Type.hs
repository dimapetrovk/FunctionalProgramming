module Type where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.LocalTime
import Data.Typeable
import qualified Data.ByteString.Char8 as BS

data ResType = ResType {id :: Integer, name, description :: String} deriving (Show)


createType :: IConnection a => String -> String -> a -> IO Bool
createType name description conn =
    withTransaction conn (createSc' name description)

createSc' name description conn = do
    changed <- run conn query [SqlString name, SqlString description]
    return $ changed == 1
    where
        query = "insert into type (name, description)" ++
            " values (?, ?)"

readType :: IConnection a => Integer -> a -> IO [ResType]
readType id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select id, name, description from type where id = ?"
      toDataObject [SqlInteger uid, SqlByteString name, SqlByteString description] = ResType uid (BS.unpack name) (BS.unpack description)
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllType :: IConnection a => a -> IO [ResType]
readAllType conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select id, name, description from type order by id"
    toDataObject [SqlInteger uid, SqlByteString name, SqlByteString description] = ResType uid (BS.unpack name) (BS.unpack description)
    toDataObject x = error $ "Unexpected result: " ++ show x


updateType ::IConnection a => ResType -> a -> IO Bool
updateType (ResType uid name description) conn = withTransaction conn (updateSd' uid name description)

updateSd' uid name description conn = do
  changed <- run conn query
                 [SqlString name, SqlString description, SqlInteger uid]
  return $ changed == 1
  where
    query = "update type set name=?, description=? where id = ?"