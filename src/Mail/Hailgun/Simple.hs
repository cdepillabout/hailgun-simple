{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Mail.Hailgun.Simple
Copyright   :  Dennis Gosnell 2016
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

module description
-}

module Mail.Hailgun.Simple
  ( HasHailgunContext(getHailgunContext)
  , HailgunContext
  , Email(..)
  , sendEmail
  , emailToHailgunMessage
  , MessageContent
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, reader)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Mail.Hailgun
       (Attachment, HailgunContext, HailgunErrorMessage,
        HailgunErrorResponse(..), HailgunMessage, HailgunSendResponse,
        MessageContent(..), MessageRecipients(..), hailgunMessage)
import qualified Mail.Hailgun as Hailgun
import Text.Email.Validate (EmailAddress, toByteString)

class HasHailgunContext r where
  getHailgunContext :: r -> HailgunContext

instance HasHailgunContext HailgunContext where
  getHailgunContext = id

-- | Datatype to represent possible errors with sending an email.
data EmailError
  = EmailErrorIncorrectEmailFormat HailgunErrorMessage
  -- ^ Email was in incorrect format.  Since we are creating emails by hand,
  -- this error should never occur.
  | EmailErrorSendError HailgunErrorResponse
  -- ^ Error from Mailgun when trying to send an email.
  deriving (Generic, Show, Typeable)

data Email = Email
  { emailSubject :: Text
  , emailBody :: MessageContent
  , emailReplyTo :: EmailAddress
  , emailRecipientsTo :: [EmailAddress]
  , emailRecipientsCC :: [EmailAddress]
  , emailRecipientsBCC :: [EmailAddress]
  , emailAttachments :: [Attachment]
  }

-- | Send an email.
sendEmail
    :: forall r m
     . ( HasHailgunContext r
       , MonadIO m
       , MonadReader r m
       )
    => Email
    -> m (Either EmailError HailgunSendResponse)
sendEmail = either (pure . Left) sendHailgunMessage . emailToHailgunMessage

emailToHailgunMessage :: Email -> Either EmailError HailgunMessage
emailToHailgunMessage Email { emailSubject
                            , emailBody
                            , emailReplyTo
                            , emailRecipientsTo
                            , emailRecipientsCC
                            , emailRecipientsBCC
                            , emailAttachments
                            } =
  let recipients =
        MessageRecipients
          (fmap toByteString emailRecipientsTo)
          (fmap toByteString emailRecipientsCC)
          (fmap toByteString emailRecipientsBCC)
      eitherHailgunMessage =
        hailgunMessage
          emailSubject
          emailBody
          (toByteString emailReplyTo)
          recipients
          emailAttachments
  in first EmailErrorIncorrectEmailFormat eitherHailgunMessage


-- adminLoginMsg
--     :: Protocol
--     -> Host
--     -> EmailAddress
--     -> LoginToken
--     -> Either HailgunErrorMessage HailgunMessage
-- adminLoginMsg protocol host adminEmail loginToken = do
--     let loginTokenText =
--             asText $ pack $ urlEncode $ unpack $ unLoginToken loginToken
--         subject = "Kucipong Admin Login"
--         content = TextOnly . encodeUtf8 $ textContent loginTokenText
--         replyTo = "no-reply@kucipong.com"
--         to = toByteString adminEmail
--         recipients = emptyMessageRecipients { recipientsTo = [ to ] }
--         attachements = []
--     hailgunMessage subject content replyTo recipients attachements
--   where
--     textContent :: Text -> Text
--     textContent loginTokenText =
--         [st|
-- This is an email from Kucipong.  You can use the following URL to
-- login as an admin:

-- #{protocol}://#{host}/admin/login/#{loginTokenText}
--         |]

-- | Generic method for sending an email.  It takes an 'Either'
-- 'HailgunErrorMessage' 'HailgunMessage'.
--
-- If the value is 'Left' 'HailgunErrorMessage', then throw an
-- 'EmailErrorIncorrectEmailFormat'.  Normally it will not be 'Left', because
-- we are creating the 'HailgunMessage' by hand.
--
-- If the value is 'Right' 'HailgunMessage', then call 'sendEmail'' with the
-- message. 'sendEmail'' may throw a 'HailgunErrorResponse'.  Rethrow this as
-- an 'EmailErrorSendError'.
sendHailgunMessage
    :: forall r m
     . ( HasHailgunContext r
       , MonadIO m
       , MonadReader r m
       )
    => HailgunMessage
    -> m (Either EmailError HailgunSendResponse)
sendHailgunMessage hailgunMsg = do
  hailgunContext <- reader getHailgunContext
  eitherSendResponse <- liftIO $ Hailgun.sendEmail hailgunContext hailgunMsg
  pure $ first EmailErrorSendError eitherSendResponse
