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
  rThrowIO io ex "git" ["init", "--quiet"]
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

makeIndexContainOnly ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  String ->
  Eff es ()
makeIndexContainOnly io ex blob path = do
  readTreeEmpty io ex
  addToIndex io ex blob path

commitIndex ::
  (e1 :> es, e2 :> es, Foldable t) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  t CommitHash ->
  Eff es CommitHash
commitIndex io ex message parents = do
  t <- writeTree io ex
  commitTree io ex t message parents

commitWithJust ::
  (e1 :> es, e2 :> es, Foldable t) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  String ->
  t CommitHash ->
  Eff es CommitHash
commitWithJust io ex blob filepath parents = do
  makeIndexContainOnly io ex blob filepath
  commitIndex io ex ("Committed " <> filepath) parents

main :: IO ()
main = runOrBail $ \io ex -> do
  withRepo io ex $ \_ -> do
    emptyBlob <- makeEmptyBlob io ex

    c1 <- commitWithJust io ex emptyBlob "1" []
    c2 <- commitWithJust io ex emptyBlob "2" [c1]
    c3 <- commitWithJust io ex emptyBlob "3" [c2]
    c4 <- commitWithJust io ex emptyBlob "4" [c3]
    c5a <- commitWithJust io ex emptyBlob "5a" [c4]
    c5b <- commitWithJust io ex emptyBlob "5b" [c4]
    c6 <- commitWithJust io ex emptyBlob "6" [c5a, c5b]
    c7 <- commitWithJust io ex emptyBlob "7" [c6]

    rThrowIO io ex "git" ["reset", "--hard", "--quiet"]
    rThrowIO io ex "git" ["checkout", c7, "--quiet"]

    let withSplitRepoExpectToThrow c msg = do
          handle (\_ -> pure ()) $ \shouldThrow -> do
            withSplitRepo io shouldThrow c $ \_ -> do
              throw ex msg

          rThrowIO io ex "git" ["reset", "--hard", "--quiet"]

    for_ [c4, c5a, c5b, c6] $ \c -> do
      withSplitRepoExpectToThrow
        c
        "Should have bailed out due to merge commit"

    rThrowIO io ex "touch" ["newfile"]
    rThrowIO io ex "git" ["add", "-N", "newfile"]
    withSplitRepoExpectToThrow
      c7
      "Should have bailed out due to unstaged changes"

    rThrowIO io ex "touch" ["newfile"]
    rThrowIO io ex "git" ["add", "newfile"]
    withSplitRepoExpectToThrow
      c7
      "Should have bailed out due to changes in the staging area"
