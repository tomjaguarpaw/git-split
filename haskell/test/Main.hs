module Main (Main.main) where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import GitSplit
import System.Directory (setCurrentDirectory)
import System.IO.Temp qualified

type CommitHash = String

-- FIXME: could do this more safely with an Eff wrapper implementing
-- MonadIO and MonadMask
withSystemTempDirectory ::
  (e1 :> es) =>
  IOE e1 ->
  String ->
  (FilePath -> Eff es r) ->
  Eff es r
withSystemTempDirectory io s k =
  withEffToIO_ io $ \effToIO -> do
    System.IO.Temp.withSystemTempDirectory s (effToIO . k)

withRepo ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  (FilePath -> Eff es r) ->
  Eff es r
withRepo io ex k = withSystemTempDirectory io "tmp-git" $ \dir -> do
  effIO io (setCurrentDirectory dir)
  rThrowIO io ex "git" ["init"]
  rThrowIO io ex "git" ["config", "user.name", "temp"]
  rThrowIO io ex "git" ["config", "user.email", "temp@example.com"]
  k dir

touchCommit ::
  (e1 <: es, e2 <: es) =>
  IOE e1 ->
  Exception String e2 ->
  Maybe CommitHash ->
  FilePath ->
  Eff es CommitHash
touchCommit io ex mParent fp = do
  effIO io (writeFile fp "")
  for_ @Maybe mParent $ \parent -> do
    rThrowIO io ex "git" ["checkout", parent]
  rThrowIO io ex "git" ["add", fp]
  rThrowIO io ex "git" ["commit", "-m", "Added " <> fp]
  hash <- rBindIO io ex "git" ["rev-parse", "HEAD"]
  pure (LBS.unpack hash)

main = runOrBail $ \io ex -> do
  withRepo io ex $ \fp -> do
    c1 <- touchCommit io ex Nothing "1"
    c2 <- touchCommit io ex (Just c1) "2"
    c3 <- touchCommit io ex (Just c2) "3"
    c4 <- touchCommit io ex (Just c3) "4"

    rThrowIO io ex "git" ["log", "--decorate", "--graph", "--all", "--oneline"]
