{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Shlack where

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 ((.:), (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe                 (catMaybes, fromMaybe)
import qualified Data.Text                  as T
import qualified Network.HTTP.Simple        as Http


newtype Config = Config { configProfiles :: [Profile] } deriving Show


instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> Config <$> o .: "profiles"


data Profile = Profile
  { profileName    :: !T.Text
  , profileWebhook :: !String
  } deriving Show


instance Aeson.FromJSON Profile where
  parseJSON = Aeson.withObject "Profile" $ \o -> Profile
    <$> o .: "name"
    <*> o .: "webhook"


data Message = Message
  { messageHead :: !(Maybe T.Text)
  , messageBody :: !(Maybe T.Text)
  , messageFoot :: !(Maybe T.Text)
  } deriving Show


instance Aeson.ToJSON Message where
  toJSON (Message h b f) = Aeson.object
    [ "text" .= fromMaybe "You have a message!" (h <|> b)
    , "blocks" .= catMaybes [fmap hb h, fmap bb b, fmap fb f]
    ]
    where
      hb x = Aeson.object
        [ "type" .= ("header" :: T.Text), "text" .= Aeson.object
          [ "type" .= ("plain_text" :: T.Text)
          , "text" .= x
          , "emoji" .= True
          ]
        ]

      bb x = Aeson.object
        [ "type" .= ("section" :: T.Text), "text" .= Aeson.object
          [ "type" .= ("mrkdwn" :: T.Text)
          , "text" .= x
          ]
        ]

      fb x = Aeson.object
        [ "type" .= ("context" :: T.Text) , "elements" .= [ Aeson.object
            [ "type" .= ("mrkdwn" :: T.Text)
            , "text" .= x
            ]
          ]
        ]


shlack :: MonadIO m => Profile -> Message -> m (Either String ())
shlack p m = do
  request <- either (error . show) (pure . prepareRequest m) $ Http.parseRequest (profileWebhook p)
  response <- Http.httpLbs request
  let statusCode = Http.getResponseStatusCode response
  let responseBody = BLC.unpack $ Http.getResponseBody response
  pure $ case statusCode of
    200 -> Right ()
    400 -> Left $ "Invalid request: " <> responseBody
    500 -> Left $ "Remote Slack server error: " <> responseBody
    x   -> Left $ "Unknown Error with status code " <> show x <> " and message: " <> responseBody
  where
    prepareRequest j = Http.setRequestMethod "POST" . Http.setRequestBodyJSON j
