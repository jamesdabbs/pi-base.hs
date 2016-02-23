{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Actions
  ( searchByText
  , searchByFormula
  , getUniverse
  , assertTrait
  , assertTheorem
  , sendLoginEmail
  , EmailAddress
  , Host
  , getUserByToken
  , showAuth
  , expireSession
  ) where

import Api.Base

import Control.Monad.State (runState)
import Control.Concurrent.MVar (readMVar, modifyMVar)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql (runSqlPool)
import Network.Mail.Mime (Mail, Address(..))
import Network.Mail.SMTP (sendMailWithLogin', simpleMail, htmlPart)

import Logic (search, provisional, assertTheorem', assertTrait')
import Models
import Universe (moveTheorem)

searchByText :: Text -> Action [Entity Space]
searchByText _ = return []

searchByFormula :: Formula PropertyId -> MatchMode -> Action [Entity Space]
searchByFormula f m = do
  u <- getUniverse
  let ids = search f m u
  runDB $ selectList [SpaceId <-. ids] []

getUniverse :: Action Universe
getUniverse = asks getUVar >>= liftIO . readMVar

assertTrait :: Trait -> Action (Entity Trait)
assertTrait t = commit (assertTrait' t) $ \u ps -> do
  _id <- insert t
  return (u, ps, Entity _id t)

assertTheorem :: Implication PropertyId -> Action (Entity Theorem)
assertTheorem i@(Implication ant con desc) = commit (assertTheorem' i) $ \u ps -> do
  let t = Theorem ant con desc []
  _id <- insert t
  let replace' p@(Proof' tr th as) = if th == provisional then (Proof' tr provisional as) else p
      ps' = map replace' ps
      u'  = moveTheorem provisional _id u
  return (u', ps', Entity _id t)

commit :: (State Universe Deductions)
       -> (Universe -> Deductions -> ReaderT SqlBackend IO (Universe, Deductions, Entity a))
       -> Action (Entity a)
commit modifications presave = do
  uvar <- asks getUVar
  pool <- asks getPool

  liftIO $ modifyMVar uvar $ \u -> do
    let (proofs, u') = runState modifications u

    -- FIXME: start transaction
    --        clean this waaaaay up
    flip runSqlPool pool $ do
      (u'', proofs', result) <- presave u' proofs
      forM_ proofs' $ \(Proof' trait thrm assumptions) -> do
        tid     <- insert trait
        proofId <- insert $ Proof tid thrm Nothing
        forM_ (S.toList assumptions) $ \prop -> do
          -- FIXME: n+1
          t <- getBy $ TraitSP (traitSpaceId trait) prop
          case t of
            Nothing -> error $ "Failed to find assumed trait: " ++ (show prop)
            Just (Entity _id _) -> insert $ Assumption proofId _id
          -- TODO: save struts and supporters as well
      return (u'', result)

type EmailAddress = Text
type Host = Text

sendMail :: Mail -> Action ()
sendMail m = do
  user <- asks smtpUsername
  pass <- asks smtpPassword
  liftIO $ sendMailWithLogin' "smtp.sendgrid.net" 587 user pass m

createSession :: UserId -> Action Text
createSession uid = do
  uuid <- liftIO $ UUID.nextRandom
  let token = T.pack $ UUID.toString uuid
  _ <- runDB $ insert $ Session uid Nothing Nothing token
  return token

sendLoginEmail :: Maybe EmailAddress -> Maybe Host -> Action ()
sendLoginEmail Nothing _ = return ()
sendLoginEmail (Just addr) mhost = do
  muser <- runDB . getBy . UniqueUser $ addr
  case muser of
    Nothing -> return ()
    Just (Entity uid user) -> do
      token <- createSession uid
      let host = fromMaybe "https://topology.jdabbs.com" mhost
          to = Address { addressName = userName user, addressEmail = userIdent user }
          from = Address { addressName = Just "Pi-Base", addressEmail = "login@topology.jdabbs.com" }
          href = host <> "/#token=" <> token
      sendMail $ simpleMail from [to] [] [] "Pi-Base Login"
        [ htmlPart . LT.pack $ "<a href=" ++ (show href) ++ ">Click to login</a>"
        ]

-- TODO: any concern of timing attacks? Lockout mechanism?
getUserByToken :: Maybe AuthToken -> Action (Entity User)
getUserByToken mtoken = case mtoken of
  Nothing  -> halt $ err401 { errBody = "No token provided" }
  Just tok -> do
    -- TODO: verify that we _do_ start with `bearer `
    msession <- runDB . getBy . UniqueToken . T.drop 7 . decodeUtf8 $ tok
    case msession of
      Nothing -> halt $ err403 { errBody = "Forbidden" }
      Just (Entity _ session) -> do
        let _id = sessionUserId session
        muser <- runDB $ get _id
        case muser of
          Nothing -> halt err404
          Just u  -> return $ Entity _id u

showAuth :: AuthenticatedAction (Entity User)
showAuth = return

expireSession :: AuthenticatedAction ()
expireSession _ = do
  liftIO $ putStrLn "TODO: need to be able to determine _which_ session to expire, if there are multiples"
