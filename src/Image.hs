{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings,
             RecordWildCards #-}
module Image
  ( Image
  , ImageId (..)
  , createImage
  , getImage
  , getImageBlob
  ) where

import ClassyPrelude
import Constants
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import qualified Network.AWS.S3 as S3
import Snap.Snaplet.AWS
import Snap.Snaplet.PostgresqlSimple
import SnapUtil

newtype ImageId = ImageId {unImageId :: Int }

instance Show ImageId where
  show (ImageId i) = show i

instance FromParam ImageId where
  fromParam bs = ImageId <$> readMay (decodeUtf8 bs)

instance ToField ImageId where
  toField (ImageId i) = toField i

instance FromField ImageId where
  fromField f mbs = ImageId <$> fromField f mbs

data Image = Image
  { imgId :: !ImageId
  , imgContentType :: !Text
  , imgCreatedAt :: !UTCTime
  }

instance ToRow Image where
  toRow Image{..} = [ toField imgContentType ]

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field

createImage :: (Monad m, HasPostgres m) => Text -> LByteString -> m (Maybe ImageId)
createImage contentType contents = do
  now <- liftIO getCurrentTime
  let img = Image (ImageId 0) contentType now
  withTransaction $ do
    mId <- query "INSERT INTO images \
                 \(id, content_type, created_at) \
                 \values (default, ?, now()) \
                 \returning id" img
    case onlySingle mId of
      Nothing -> return Nothing
      Just iId -> liftIO $ do
        putFile dominoBucketName (S3.ObjectKey $ tshow iId) contentType contents
        return $ Just iId

getImage :: HasPostgres m => ImageId -> m (Maybe Image)
getImage imgId =
  single <$> query "select id, conent_type, created_at where id = (?)" (Only imgId)

getImageBlob :: MonadIO m => ImageId -> m LByteString
getImageBlob imgId = liftIO $ getFile dominoBucketName (S3.ObjectKey $ tshow imgId)

single :: [a]-> Maybe a
single [x] = Just x
single _ = Nothing

onlySingle :: [Only a] -> Maybe a
onlySingle [x] = Just $ fromOnly x
onlySingle _ = Nothing
