-- {-# LANGUAGE DeriveDataTypeable #-}

-- import Text.JSON.Generic

-- data Address = Address
--     { house  :: Integer
--     , street :: String
--     , city   :: String
--     , state  :: String
--     , zip    :: Integer
--     } deriving (Show, Data, Typeable)

-- data Person = Person
--     { name    :: String
--     , age     :: Integer
--     , address :: Address
--     } deriving (Show, Data, Typeable)
    
-- main :: IO ()
-- main = do
--     jsonData <- readFile "SnakesLaddersGame/person.json"
--     let person = decodeJSON jsonData :: Person
--     print (person)
--     writeFile "SnakesLaddersGame/person2.json" (encodeJSON person)

{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: String
    , zip    :: Integer
    } deriving (Show, Generic)

instance FromJSON Address
instance ToJSON Address

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

-- Custom function to encode JSON with more newlines and indentation
encodeJSON' :: ToJSON a => a -> String
encodeJSON' = C.unpack . encodePretty
--   where
--     customConfig = defConfig { confIndent = Spaces 4 } -- Indent with 4 spaces

main :: IO ()
main = do
    -- Read JSON from file
    jsonData <- B.readFile "SnakesLaddersGame/person.json"

    -- Decode JSON
    let result = Data.Aeson.decode jsonData :: Maybe Person

    -- Handle decoding result
    case result of
        Just person -> do
            -- Print the decoded object
            print person
            
            -- Convert back to JSON (pretty format) and write to a new file
            writeFile "SnakesLaddersGame/person2.json" (encodeJSON' person)
        
        Nothing -> putStrLn "Error decoding JSON."
