module Main (main) where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Control.Monad
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.String
import System.Environment
import System.Process.Typed

trimTrailingNewlines :: LBS.ByteString -> LBS.ByteString
trimTrailingNewlines = LBS.dropWhileEnd (== '\n')

main :: IO ()
main = runEff_ $ \io -> handle (effIO io . putStrLn) $ \ex -> do
  let rBind s = do
        (exitCode, stdout) <- effIO io (readProcessStdout (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure (trimTrailingNewlines stdout)
  let echoN = effIO io . putStr
  let echo = effIO io . putStrLn

  (handler, combinedProvided) <-
    effIO io getArgs >>= \case
      [arg1, arg2] -> pure (arg1, arg2)
      _ -> throw ex "Expected two arguments"

  t@(branch, _, _) <- prepareToSplit io ex combinedProvided

  echo ("I'm going to drop you into your chosen handler: " <> handler)
  echoN "Please make any number of commits and then exit the handler with "
  echo "exit code 0"

  currentShort <- fmap LBS.unpack (rBind "git rev-parse --short HEAD")
  let branchOrCurrentShort =
        if not (null branch)
          then
            branch
          else currentShort

  effIO io (runProcess (fromString handler)) >>= \case
    ExitSuccess -> pure ()
    ExitFailure {} -> do
      afterFailedHandler <- rBind "git rev-parse --short HEAD"
      echo
        ( "The handler failed at "
            <> LBS.unpack afterFailedHandler
            <> ".  Returning to "
            <> branchOrCurrentShort
            <> "."
        )
      restore io ex t

  applySubsequentCommits io ex t

restore ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  (String, String, string) ->
  Eff es b
restore io ex (branch, current, _) = do
  let rThrow s = do
        exitCode <- effIO io (runProcess (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure ()
  rThrow "git reset --quiet --hard"
  let returnTo = if not (null branch) then branch else current
  rThrow ("git checkout --force --quiet \"" <> returnTo <> "\"")
  throw ex ""

prepareToSplit ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  Eff es (String, String, String)
prepareToSplit io ex combinedProvided = do
  let r s =
        fmap
          (trimTrailingNewlines . snd)
          (effIO io (readProcessStdout (fromString s)))
  let rThrow s = do
        exitCode <- effIO io (runProcess (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure ()
  let rBind s = do
        (exitCode, stdout) <- effIO io (readProcessStdout (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure (trimTrailingNewlines stdout)
  let echoN = effIO io . putStr
  let echo = effIO io . putStrLn

  branch <- fmap LBS.unpack (r "git symbolic-ref --quiet --short HEAD")
  current <- fmap LBS.unpack (rBind "git rev-parse HEAD")
  currentShort <- fmap LBS.unpack (rBind "git rev-parse --short HEAD")

  let branchOrCurrentShort =
        if not (null branch)
          then
            branch
          else currentShort

  combined <- fmap LBS.unpack (rBind ("git rev-parse " <> combinedProvided))
  combinedShort <- rBind ("git rev-parse --short " <> combinedProvided)

  let throwFailed s msg =
        effIO io (runProcess (fromString s)) >>= \case
          ExitFailure {} -> throw ex msg
          ExitSuccess -> pure ()

  throwFailed
    ("git merge-base --is-ancestor " <> combinedProvided <> " " <> current)
    (combinedProvided <> " is not an ancestor of " <> branchOrCurrentShort)

  throwFailed
    "git diff --quiet"
    ( "The repo has uncommitted changes.  "
        <> "Stash, commit or reset them and then try again."
    )

  throwFailed
    "git diff --cached --quiet"
    ( "The repo has changes in the staging area.  "
        <> "Stash, commit or reset them and then try again."
    )

  effIO
    io
    ( runProcess
        ( fromString ("git rev-parse --verify --quiet " <> combined <> "^2")
        )
    )
    >>= \case
      ExitSuccess ->
        throw ex (combinedProvided <> " is a merge commit.  Cannot split.")
      ExitFailure {} -> pure ()

  combinedParent <- rBind ("git rev-parse " <> combined <> "^")
  combinedParentShort <- rBind ("git rev-parse --short " <> combined <> "^")

  echoN "checkout..."
  rThrow ("git checkout --quiet " <> combined)
  echoN "reset..."
  rThrow ("git reset --quiet " <> LBS.unpack combinedParent)
  echo "done"

  echoN "You were on "
  if not (null branch)
    then echoN ("branch " <> branch <> " (" <> currentShort <> "). ")
    else echoN (currentShort <> ". ")

  echo "You wanted to split the commit"
  echo ""
  rThrow ("git show --no-patch --pretty=short " <> combined)
  echo ""
  echoN
    ( "I'm now on "
        <> LBS.unpack combinedShort
        <> "'s parent ("
        <> LBS.unpack combinedParentShort
        <> "). "
    )

  pure (branch, current, combined)

applySubsequentCommits ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  (String, String, String) ->
  Eff es ()
applySubsequentCommits io ex (branch, current, combined) = do
  let rThrow s = do
        exitCode <- effIO io (runProcess (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure ()
  let rBind s = do
        (exitCode, stdout) <- effIO io (readProcessStdout (fromString s))
        case exitCode of
          failure@(ExitFailure {}) -> throw ex (show failure)
          ExitSuccess -> pure (trimTrailingNewlines stdout)
  let echoN = effIO io . putStr
  let echo = effIO io . putStrLn

  afterHandler <- rBind "git rev-parse HEAD"
  afterHandlerShort <- rBind "git rev-parse --short HEAD"

  echoN "reset..."
  rThrow ("git reset --quiet --hard " <> LBS.unpack afterHandler)

  echoN "checkout..."
  rThrow ("git checkout --quiet --force " <> combined)

  echoN "reset..."
  rThrow ("git reset --quiet --soft " <> LBS.unpack afterHandler)

  combinedSubject <- rBind ("git diff-tree -s --pretty=%s " <> combined)
  combinedBody <- rBind ("git diff-tree -s --pretty=%b " <> combined)

  echoN "commit..."
  _ <-
    rThrow
      ( "git commit --allow-empty --quiet -m \""
          <> LBS.unpack combinedSubject
          <> "\" -m \""
          <> LBS.unpack combinedBody
          <> "\""
      )

  restOfCombined <- rBind "git rev-parse HEAD"
  -- Check 2 equality
  echoN "checking equality..."
  _ <-
    rThrow
      ( "git diff --exit-code "
          <> LBS.unpack restOfCombined
          <> " "
          <> combined
      )

  echoN "rebase..."
  _ <-
    rThrow
      ( "git rebase --quiet --onto "
          <> LBS.unpack restOfCombined
          <> " "
          <> combined
          <> " "
          <> current
      )

  finished <- rBind "git rev-parse HEAD"
  finishedShort <- rBind "git rev-parse --short HEAD"
  let branchOrFinishedShort =
        if not (null branch)
          then branch
          else
            LBS.unpack finishedShort

  -- Check 3 equality
  echoN "checking equality..."
  rThrow ("git diff --exit-code " <> LBS.unpack finished <> " " <> current)

  when (not (null branch)) $ do
    echoN "setting branch to history with split..."
    rThrow ("git push --quiet --force . HEAD:" <> branch)
    rThrow ("git checkout --quiet " <> branch)
    pure ()

  -- Check 3 e;quality, and we have it checked out
  echoN "checking equality..."
  rThrow ("git diff --exit-code HEAD " <> current)

  echo "done"
  echo ""
  echo "Splitting finished successfully!"
  echo ""
  if not (null branch)
    then
      echoN ("Your branch is " <> branch <> ".  ")
    else
      echoN "Your HEAD is detached.  "

  currentShort <- fmap LBS.unpack (rBind "git rev-parse --short HEAD")

  echo
    ( "It was previously "
        <> currentShort
        <> ".  It is now "
        <> LBS.unpack finishedShort
        <> "."
    )
  echo ""
  echo "You might want to do exactly one of the following"
  echo ""
  echo "* An interactive rebase to reword the remnants of the split"
  echo ""
  echo
    ( "  $ git rebase --interactive "
        <> LBS.unpack afterHandlerShort
        <> " "
        <> branchOrFinishedShort
    )
  echo ""
  echoN "* If you don't want the split after all, "
  echo "reset your branch to what it was before"
  echo ""
  echo ("  $ git reset --hard " <> currentShort)
