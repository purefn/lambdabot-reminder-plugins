{-# LANGUAGE TupleSections #-}
module Lambdabot.Plugin.Reminder (reminderPlugins, reminderPlugin) where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted as E (SomeException(..), catch)
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Attoparsec.Text
import Data.Foldable hiding (concat)
import Data.Maybe
import Data.Text (pack, unpack)
import Data.Time.Clock
import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.Plugin
import System.Cron hiding (command)
import System.Cron.Parser

data Reminder = Reminder
  { server :: String -- \ Nick = server and
  , target :: String -- / room, person, etc
  , cron :: String
  , message :: String
  } deriving (Show, Read)

type S = ([Reminder], Maybe (ThreadId, TVar [(Reminder, CronSchedule)]))

reminderPlugins :: [String]
reminderPlugins = ["reminder"]

reminderPlugin :: Module S
reminderPlugin = newModule
  { moduleCmds = return
      [ (command "reminder-add")
          { aliases = []
          , help = say "reminder-add <crontab> <message>. Add a reminder for the channel."
          , process = addReminder
          }
      , (command "reminder-list")
          { aliases = []
          , help = say "reminder-list. See all the reminders for the channel."
          , process = listReminders
          }
      ]
  , moduleDefState = return ([], Nothing)
  , moduleSerialize = Just serial
  , moduleInit = reminderInit
  }

serial :: Serial S
serial = Serial se de where
  se = serialize stdSerial . fst
  de = ((, Nothing) <$>) . deserialize stdSerial

reminderInit :: ModuleT S LB ()
reminderInit = withMS $ \(rs, _) wr -> (init' rs >>= wr) where
  init' rs = do
    v <- liftIO $ newTVarIO $ parse' rs
    tid <- fork $ cronLoop v
    return (rs, Just (tid, v))
  parse' rs = do
    r <- rs
    c <- maybeToList $ maybeResult $ parse cronSchedule $ pack $ cron r
    return (r, c)
  cronLoop v = forever $ catch (go v) handleErr
  go v = do
    liftIO sleep
    now <- liftIO getCurrentTime
    xs <- liftIO $ readTVarIO v
    forM_ (filter (\(_, c) -> scheduleMatches c now) xs) $ \(r, _) ->
      lift $ send $ privmsg (Nick (server r) (target r)) $ message r
  handleErr :: MonadIO m => SomeException -> m ()
  handleErr = liftIO . print
  sleep = do
    now <- getCurrentTime
    -- how many microseconds we have to wait until the next minute
    let secs = round (realToFrac $ utctDayTime now) `rem` 60
    threadDelay $ 1000000 * (60 - secs)

addReminder :: String -> Cmd (ModuleT S LB) ()
addReminder s = case parse cronScheduleLoose $ pack s of
  Fail _ _ e -> say ("Failed to parse cron entry: " ++ e)
  Partial _ -> say "Incomplete cron entry"
  Done msg c -> do
    nick <- getTarget
    let
      ce = concat
        [ show $ minute c
        , " "
        , show $ hour c
        , " "
        , show $ dayOfMonth c
        , " "
        , show $ month c
        , " "
        , show $ dayOfWeek c
        ]
      r = Reminder (nTag nick) (nName nick) ce (unpack msg)
    addReminder' r c
    say "Noted."

addReminder' :: Reminder -> CronSchedule -> Cmd (ModuleT S LB) ()
addReminder' r c = withMS $ \(rs, s) wr -> do
  wr (r : rs, s)
  forM_ s $ \(_, v) ->
    liftIO $ atomically $ modifyTVar v ((r, c) :)

listReminders :: String -> Cmd (ModuleT S LB) ()
listReminders _ = readMS >>= \(rs, _) -> do
  t <- getTarget
  let rs' = filter (\r -> server r == nTag t && target r == nName t) rs
  traverse_ (say . showPretty) rs'

showPretty :: Reminder -> String
showPretty r = concat [ cron r, " ", message r ]

