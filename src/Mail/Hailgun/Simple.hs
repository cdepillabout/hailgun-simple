{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Mail.Hailgun.Simple
Copyright   :  Dennis Gosnell 2016
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides a simple, easy-to-use wrapper around the
<https://hackage.haskell.org/package/hailgun hailgun> package. hailgun
is a package providing a way to send email using
<https://www.mailgun.com Mailgun>.

Here is a short example of how to use this package:

@
  \{\-\# LANGUAGE OverloadedStrings \#\-\}
  \{\-\# LANGUAGE QuasiQuotes \#\-\}

  module FooBar where

  import "Control.Monad.Reader" ('Control.Monad.Reader.ReaderT')
  import "Data.Text" ('Data.Text.Text')
  import "Data.Text.Encoding" ('Data.Text.Encoding.encodeUtf8')
  import "Text.Email.Validate" ('EmailAddress')
  import Text.Shakespeare.Text (sbt)
  import "Mail.Hailgun.Simple"
         ('MessageContent'('TextOnly'), 'Email'(..), 'EmailError',
          'HailgunContext', 'ResponseFromMailgun', 'sendEmail')

  -- This function will send a new user an email.
  sendEmailToNewUser
    :: 'Text' -- ^ user's name
    -> 'EmailAddress' -- ^ user's email address
    -> 'ReaderT' 'HailgunContext' 'IO' ('Either' 'EmailError' 'ResponseFromMailgun')
  sendEmailToNewUser name emailaddress = do
    let email = 'Email'
          { 'emailSubject' = "Thanks for signing up!"
          , 'emailBody' = 'TextOnly' $ 'encodeUtf8' body
          , 'emailReplyTo' = myEmailAddress
          , 'emailRecipientsTo' = [emailaddress]
          , 'emailRecipientsCC' = []
          , 'emailRecipientsBCC' = []
          , 'emailAttachments' = []
          }
    'sendEmail' email
    where
      body :: 'Text'
      body = [sbt|Hi #{name}!
                 |
                 |Thanks for signing up to our service!
                 |
                 |From your friends at foobar.com :-)|]

  myEmailAddress :: 'EmailAddress'
  myEmailAddress = undefined
@
-}

module Mail.Hailgun.Simple
  ( -- * Sending an 'Email'
    sendEmail
    -- * 'Email'
  , Email(..)
  , MessageContent(..)
    -- * Response
  , ResponseFromMailgun(..)
    -- * 'HailgunContext'
  , HailgunContext(..)
  , HasHailgunContext(getHailgunContext)
    -- * Errors
  , EmailError(..)
    -- * Lower-level calls
  , emailToHailgunMessage
  , sendHailgunMessage
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, reader)
import Data.Bifunctor (bimap, first)
import Data.Data (Data)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Mail.Hailgun
       (Attachment, HailgunContext, HailgunErrorResponse(..),
        HailgunMessage, HailgunSendResponse(..), MessageContent(..),
        MessageRecipients(..), hailgunMessage)
import qualified Mail.Hailgun as Hailgun
import Text.Email.Validate (EmailAddress, toByteString)

-- | This class provides one layer (or multiple layers) of indirection.  It
-- makes it possible to pass 'sendEmail' a generic configuration datatype
-- that contains a 'HailgunContext' instead of a 'HailgunContext' directly.
--
-- For instance, imagine you had a configuration datatype like this:
--
-- @
--   data Config = Config
--     { configDatabasePool :: Pool
--     , configHailgunContext :: 'HailgunContext'
--     }
-- @
--
-- You could create an instance of 'HasHailgunContext' for @Config@ like this:
--
-- @
--   instance 'HasHailgunContext' Config where
--     getHailgunContext :: Config -> 'HailgunContext'
--     getHailgunContext = configHailgunContext
-- @
--
-- Now, you can pass @Config@ to 'sendEmail' instead of a 'HailgunContext'
-- directly.
class HasHailgunContext r where
  getHailgunContext :: r -> HailgunContext

instance HasHailgunContext HailgunContext where
  getHailgunContext = id

-- | Datatype to represent possible errors with sending an email.
data EmailError
  = EmailErrorIncorrectEmailFormat Text
  -- ^ Email was in incorrect format.  Since we are creating emails by hand,
  -- this error should never occur.
  | EmailErrorSendError Text
  -- ^ Error from Mailgun when trying to send an email.
  deriving (Generic, Show, Typeable)

-- | Datatype representing an email to send.
data Email = Email
  { emailSubject :: Text
  , emailBody :: MessageContent
  , emailReplyTo :: EmailAddress
  , emailRecipientsTo :: [EmailAddress]
  , emailRecipientsCC :: [EmailAddress]
  , emailRecipientsBCC :: [EmailAddress]
  , emailAttachments :: [Attachment]
  }

-- | Response returned from Mailgun's servers.
data ResponseFromMailgun = ResponseFromMailgun
  { mailgunMessage :: Text  -- ^ Freeform message from Mailgun
  , mailgunId :: Text       -- ^ ID of the message accepted by Mailgun
  } deriving (Data, Generic, Show, Typeable)

-- | Send an 'Email'.
--
-- Returns an 'EmailErrorIncorrectEmailFormat' error if the format of the email
-- was not correct (for instance, if the email senders or receivers were
-- incorrect, or the attachments are specified incorrectly).  If you are
-- constructing an 'Email' by hand (and not programatically), this error will
-- indicate a programmer error.
--
-- Returns an 'EmailErrorSendError' if there was a problem with actually
-- sending the 'Email'.  This will usually be an error from the Mailgun
-- servers.
sendEmail
    :: forall r m
     . ( HasHailgunContext r
       , MonadIO m
       , MonadReader r m
       )
    => Email
    -> m (Either EmailError ResponseFromMailgun)
sendEmail = either (pure . Left) sendHailgunMessage . emailToHailgunMessage

-- | Wrapper around "Mail.Hailgun"'s 'hailgunMessage'.
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
  in first (EmailErrorIncorrectEmailFormat . pack) eitherHailgunMessage

-- | Wrapper around "Mail.Hailgun"'s 'Hailgun.sendEmail'.  Used by 'sendEmail'.
sendHailgunMessage
    :: forall r m
     . ( HasHailgunContext r
       , MonadIO m
       , MonadReader r m
       )
    => HailgunMessage
    -> m (Either EmailError ResponseFromMailgun)
sendHailgunMessage hailgunMsg = do
  hailgunContext <- reader getHailgunContext
  eitherSendResponse <- liftIO $ Hailgun.sendEmail hailgunContext hailgunMsg
  pure $
    bimap
      hailgunErrorResponseToEmailError
      hailgunSendResponseToResponseFromMailgun
      eitherSendResponse

hailgunErrorResponseToEmailError :: HailgunErrorResponse -> EmailError
hailgunErrorResponseToEmailError = EmailErrorSendError . pack . herMessage

hailgunSendResponseToResponseFromMailgun :: HailgunSendResponse
                                         -> ResponseFromMailgun
hailgunSendResponseToResponseFromMailgun HailgunSendResponse {hsrMessage, hsrId} =
  ResponseFromMailgun {mailgunMessage = pack hsrMessage, mailgunId = pack hsrId}
