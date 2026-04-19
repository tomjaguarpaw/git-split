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

type TreeHash = String

type BlobHash = String

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

readTreeEmpty ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  Eff es ()
readTreeEmpty io ex = rThrowIO io ex "git" ["read-tree", "--empty"]

readTree ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  TreeHash ->
  Eff es ()
readTree io ex tree = rThrowIO io ex "git" ["read-tree", tree]

writeTree ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  Eff es TreeHash
writeTree io ex = do
  tree <- rBindIO io ex "git" ["write-tree"]
  pure (LBS.unpack tree)

commitTree ::
  (Foldable t, e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  TreeHash ->
  String ->
  t String ->
  Eff es CommitHash
commitTree io ex tree message parents = do
  let parentArgs = flip concatMap parents $ \parent ->
        ["-p", parent]
  let args = ["commit-tree", tree, "-m", message] ++ parentArgs

  commit <- rBindIO io ex "git" args

  pure (LBS.unpack commit)

makeEmptyBlob ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  Eff es BlobHash
makeEmptyBlob io ex = do
  blob <- rBindIO io ex "git" ["hash-object", "-w", "-t", "blob", "/dev/null"]
  pure (LBS.unpack blob)

addToIndex ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  String ->
  Eff es ()
addToIndex io ex blob path =
  rThrowIO
    io
    ex
    "git"
    ["update-index", "--add", "--cacheinfo", "100644", blob, path]

main :: IO ()
main = runOrBail $ \io ex -> do
  withRepo io ex $ \_ -> do
    emptyBlob <- makeEmptyBlob io ex
    readTreeEmpty io ex

    addToIndex io ex emptyBlob "1"
    t1 <- writeTree io ex
    c1 <- commitTree io ex t1 "Committed 1" []

    addToIndex io ex emptyBlob "2"
    t2 <- writeTree io ex
    c2 <- commitTree io ex t2 "Committed 2" [c1]

    addToIndex io ex emptyBlob "3"
    t3 <- writeTree io ex
    c3 <- commitTree io ex t3 "Committed 3" [c2]

    addToIndex io ex emptyBlob "4"
    t4 <- writeTree io ex
    c4 <- commitTree io ex t4 "Committed 4" [c3]

    addToIndex io ex emptyBlob "5a"
    t5a <- writeTree io ex
    c5a <- commitTree io ex t5a "Committed 5a" [c4]

    addToIndex io ex emptyBlob "5b"
    t5b <- writeTree io ex
    c5b <- commitTree io ex t5b "Committed 5b" [c4]

    addToIndex io ex emptyBlob "6"
    t6 <- writeTree io ex
    c6 <- commitTree io ex t6 "Committed 6" [c5a, c5b]

    effIO io (putStrLn "Done all commits")

    rThrowIO io ex "git" ["checkout", c6]
    rThrowIO io ex "git" ["log", "--patch"]
    rThrowIO io ex "git" ["log", "--decorate", "--graph", "--all", "--oneline"]
