{-# LANGUAGE DeriveGeneric #-}

data Software = Software {
      id :: DBKey,
      name :: String,
      author :: String,
      annotation :: String,
      version :: String,
      category :: String,
      condition :: String
  } deriving (Show, Generic)

instance ToRow Software where
      toRow d = [toField (name d),
      toField (author d),
      toField (annotation d),
      toField (version d),
      toField (category d),
      toField (condition d)]

instance FromRow Software where
  fromRow = Software <$> field <*> field <*> field <*> field <*> field <*> field <*> field