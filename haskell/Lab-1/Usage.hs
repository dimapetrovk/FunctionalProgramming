module Usage where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS

data Usage = Usage {id, userId, programId :: Integer, time :: LocalTime, action, info :: String} deriving (Show)

createUsage :: IConnection a => Integer -> Integer -> LocalTime -> String  ->  String-> a -> IO Bool
createUsage userId programId time action info conn =
    withTransaction conn (createSc' userId programId time action info)

createSc' userId programId time action info conn = do
    changed <- run conn query [SqlInteger userId, SqlInteger programId, SqlLocalTime time, SqlString action, SqlString info]
    return $ changed == 1
    where
        query = "insert into usage (user_id, program_id, time, action, info)" ++
            " values (?, ?, ?, ?, ?)"

readUsage :: IConnection a => Integer -> a -> IO [Usage]
readUsage id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select id, user_id, program_id, time, action, info from usage where id = ?"
      toDataObject [SqlInteger uid, SqlInteger userId, SqlInteger programId, SqlLocalTime time, SqlByteString action, SqlByteString info] =
        Usage uid userId programId time (BS.unpack action) (BS.unpack info)
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllUsage :: IConnection a => a -> IO [Usage]
readAllUsage conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select id, user_id, program_id, time, action, info  from usage order by id"
    toDataObject [SqlInteger uid, SqlInteger userId, SqlInteger programId, SqlLocalTime time, SqlByteString action, SqlByteString info] =
        Usage uid userId programId time (BS.unpack action) (BS.unpack info)
    toDataObject x = error $ "Unexpected result: " ++ show x

updateUsage :: IConnection a => Usage -> a -> IO Bool
updateUsage (Usage uid userId programId time action info) conn =
    withTransaction conn (updateSd' uid userId programId time action info)

updateSd' uid userId programId time action  info conn = do
  changed <- run conn query
                 [SqlInteger userId, SqlInteger programId, SqlLocalTime time, SqlString action, SqlString info, SqlInteger uid]
  return $ changed == 1
  where
    query = "update usage set user_id=?, program_id=?, time=?, action=?, info=? where id = ?"