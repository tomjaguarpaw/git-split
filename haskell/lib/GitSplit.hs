module GitSplit where

import Bluefin.Compound (mapHandle)
import Bluefin.EarlyReturn (returnEarly, withEarlyReturn)
import Bluefin.Eff (Eff, runEff_, (:>))
import Bluefin.Exception (Exception, handle, throw)
import Bluefin.IO (IOE, effIO)
import Bluefin.State (evalState, get, put)
import Control.Monad (forever, unless, when)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import Data.String (IsString (fromString))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    proc,
    readProcessStdout,
    runProcess,
  )

runOrBail ::
  (forall e. IOE e -> Exception String e -> Eff e ()) ->
  IO ()
runOrBail k =
  runEff_ $ \io -> handle
    ( \msg -> effIO io $ do
        putStrLn msg
        exitFailure
    )
    $ \ex ->
      k (mapHandle io) (mapHandle ex)

main :: IO ()
main = runOrBail $ \io ex -> do
  effIO io getArgs >>= \case
    "interactive" : rest -> case rest of
      [handler, combinedProvided] ->
        interactive io ex handler combinedProvided
      _ -> throw ex "interactive expected handler and commit to split"
    "prepareToSplit" : rest -> case rest of
      [combinedProvided] ->
        prepareToSplitCli io ex combinedProvided
      _ -> throw ex "prepareToSplit expected commit to split"
    "applySubsequentCommits" : rest -> case rest of
      [branch, current, combined] ->
        applySubsequentCommits io ex (branch, current, combined)
      _ -> throw ex "applySubsequentCommits expected three more arguments"
    "restore" : rest -> case rest of
      [branch, current, combined] ->
        restore io ex (branch, current, combined)
      _ -> throw ex "restore expected three more arguments"
    _ -> throw ex "Expected interactive"

rBindIO ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  [String] ->
  Eff es LBS.ByteString
rBindIO io ex f s = do
  (exitCode, stdout) <- effIO io (readProcessStdout (proc f s))
  case exitCode of
    failure@(ExitFailure {}) -> throw ex (show failure)
    ExitSuccess -> pure (trimTrailingNewlines stdout)

rThrowExitCode ::
  (e1 :> es, e2 :> es) =>
  (ExitCode -> ex) ->
  IOE e1 ->
  Exception ex e2 ->
  String ->
  [String] ->
  Eff es ()
rThrowExitCode k io ex f s = do
  exitCode <- effIO io (runProcess (proc f s))
  case exitCode of
    failure@(ExitFailure {}) -> throw ex (k failure)
    ExitSuccess -> pure ()

rThrowIO ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  [String] ->
  Eff es ()
rThrowIO = rThrowExitCode show

isMerge ::
  (e1 :> es, e2 :> es) => IOE e1 -> Exception String e2 -> String -> Eff es Bool
isMerge io ex commit = do
  let s = ["rev-parse", "--verify", "--quiet", commit <> "^2"]
  -- rev-parse writes its result to stdout so by getting it and
  -- ignoring it we stop that being printed to our stdout
  (exitCode, _) <- effIO io (readProcessStdout (proc "git" s))
  case exitCode of
    ExitSuccess -> pure True
    ExitFailure 1 -> pure False
    ExitFailure e -> throw ex ("git rev-parse returned " <> show e)

trimTrailingNewlines :: LBS.ByteString -> LBS.ByteString
trimTrailingNewlines = LBS.dropWhileEnd (== '\n')

withSplitRepo ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  ((String, String, String) -> Eff es Bool) ->
  Eff es ()
withSplitRepo io ex combinedProvided handler = do
  let rBind = rBindIO io ex
  let echo = effIO io . putStrLn
  let short s = fmap LBS.unpack (rBind "git" ["rev-parse", "--short", s])

  t@(branch, current, _) <- prepareToSplit io ex combinedProvided

  currentShort <- short current

  let branchOrCurrentShort =
        if not (null branch)
          then
            branch
          else currentShort

  success <- handler t

  unless success $ do
    afterFailedHandler <- short "HEAD"
    echo
      ( "The handler failed at "
          <> afterFailedHandler
          <> ".  Returning to "
          <> branchOrCurrentShort
          <> "."
      )
    restore io ex t
    throw ex ""

  applySubsequentCommits io ex t

interactive ::
  (e2 :> es, e1 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  String ->
  Eff es ()
interactive io ex handler combinedProvided = do
  let echo = effIO io . putStrLn
  let echoN = effIO io . putStr

  withSplitRepo io ex combinedProvided $ \_ -> do
    echo ("I'm going to drop you into your chosen handler: " <> handler)
    echoN "Please make any number of commits and then exit the handler with "
    echoN "exit code 0. To abort and return to where you were, exit the handler "
    echo "with a non-zero exit code."

    r <- effIO io (runProcess (fromString handler))
    pure $ case r of
      ExitSuccess -> True
      ExitFailure {} -> False

restore ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  (String, String, string) ->
  Eff es ()
restore io ex (branch, current, _) = do
  let rThrow = rThrowIO io ex
  rThrow "git" ["reset", "--quiet", "--hard"]
  let returnTo = if not (null branch) then branch else current
  rThrow "git" ["checkout", "--force", "--quiet", returnTo]

prepareToSplitCli ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  Eff es ()
prepareToSplitCli io ex combinedProvided = do
  (branch, current, combined) <- prepareToSplit io ex combinedProvided
  let quotesIfNull s = if null s then "\"" <> s <> "\"" else s
  effIO io $ do
    putStrLn "To continue run:"
    putStrLn ""
    putStrLn
      ( unwords
          [ "    ",
            "git-split",
            "applySubsequentCommits",
            quotesIfNull branch,
            quotesIfNull current,
            quotesIfNull combined
          ]
      )
    putStrLn ""
    putStrLn "To abort run:"
    putStrLn ""
    putStrLn
      ( unwords
          [ "    ",
            "git-split",
            "restore",
            quotesIfNull branch,
            quotesIfNull current,
            quotesIfNull combined
          ]
      )

currentHead ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  Eff es [Char]
currentHead io ex = fmap LBS.unpack (rBindIO io ex "git" ["rev-parse", "HEAD"])

hasUnstagedChanges ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  Eff es Bool
hasUnstagedChanges io ex = do
  exitCode <- effIO io (runProcess (proc "git" ["diff", "--quiet"]))
  case exitCode of
    ExitSuccess -> pure False
    ExitFailure 1 -> pure True
    ExitFailure e -> throw ex ("git diff --quiet returned " <> show e)

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
          (effIO io (readProcessStdout (fromString (unwords s))))
  let rThrow = rThrowIO io ex
  let rBind = rBindIO io ex
  let echoN = effIO io . putStr
  let echo = effIO io . putStrLn
  let short s = fmap LBS.unpack (rBind "git" ["rev-parse", "--short", s])

  branch <- fmap LBS.unpack (r ["git", "symbolic-ref", "--quiet", "--short", "HEAD"])
  current <- currentHead io ex
  currentShort <- short current

  let branchOrCurrentShort =
        if not (null branch)
          then
            branch
          else currentShort

  combined <- fmap LBS.unpack (rBind "git" ["rev-parse", combinedProvided])
  combinedShort <- short combinedProvided

  let throwFailed f s msg = rThrowExitCode (const msg) io ex f s

  throwFailed
    "git"
    ["merge-base", "--is-ancestor", combinedProvided, current]
    (combinedProvided <> " is not an ancestor of " <> branchOrCurrentShort)

  hasUnstagedChanges_ <- hasUnstagedChanges io ex
  when hasUnstagedChanges_ $ do
    throw
      ex
      ( "The repo has unstaged changes.  "
          <> "Stash, commit or reset them and then try again."
      )

  throwFailed
    "git"
    ["diff", "--cached", "--quiet"]
    ( "The repo has changes in the staging area.  "
        <> "Stash, commit or reset them and then try again."
    )

  mMergeCommit <- containsMerges io ex current combined

  for_ @Maybe mMergeCommit $ \mergeCommit ->
    throw ex (mergeCommit <> " is a merge commit.  Cannot split.")

  combinedParent <- fmap LBS.unpack (rBind "git" ["rev-parse", combined <> "^"])
  combinedParentShort <- short combinedParent

  echoN "checkout..."
  rThrow "git" ["checkout", "--quiet", combined]
  echoN "reset..."
  rThrow "git" ["reset", "--quiet", combinedParent]
  echo "done"

  echoN "You were on "
  if not (null branch)
    then echoN ("branch " <> branch <> " (" <> currentShort <> "). ")
    else echoN (currentShort <> ". ")

  echo "You wanted to split the commit"
  echo ""
  rThrow "git" ["show", "--no-patch", "--pretty=short", combined]
  echo ""
  echoN
    ( "I'm now on "
        <> combinedShort
        <> "'s parent ("
        <> combinedParentShort
        <> ").  The changes of "
        <> combinedShort
        <> " are uncommitted in the repository. "
    )

  pure (branch, current, combined)

containsMerges ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  String ->
  String ->
  Eff es (Maybe [Char])
containsMerges io ex start end =
  withEarlyReturn $ \ret -> evalState start $ \commit -> forever $ do
    commit' <- get commit
    commitIsMerge <- isMerge io ex commit'

    when commitIsMerge $ do
      returnEarly ret (Just commit')

    if commit' == end
      then
        returnEarly ret Nothing
      else do
        next <- rBindIO io ex "git" ["rev-parse", commit' <> "^"]
        put commit (LBS.unpack next)

applySubsequentCommits ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  (String, String, String) ->
  Eff es ()
applySubsequentCommits io ex (branch, current, combined) = do
  let rThrow = rThrowIO io ex
  let rBind = rBindIO io ex
  let echoN = effIO io . putStr
  let echo = effIO io . putStrLn
  let short s = fmap LBS.unpack (rBind "git" ["rev-parse", "--short", s])

  afterHandler <- currentHead io ex
  afterHandlerShort <- short afterHandler

  echoN "reset..."
  rThrow "git" ["reset", "--quiet", "--hard", afterHandler]

  echoN "checkout..."
  rThrow "git" ["checkout", "--quiet", "--force", combined]

  echoN "reset..."
  rThrow "git" ["reset", "--quiet", "--soft", afterHandler]

  combinedSubject <- rBind "git" ["diff-tree", "-s", "--pretty=%s", combined]
  combinedBody <- rBind "git" ["diff-tree", "-s", "--pretty=%b", combined]

  echoN "commit..."
  _ <-
    rThrow
      "git"
      [ "commit",
        "--allow-empty",
        "--quiet",
        "-m",
        LBS.unpack combinedSubject,
        "-m",
        LBS.unpack combinedBody
      ]

  restOfCombined <- currentHead io ex
  -- Check 2 equality
  echoN "checking equality..."
  _ <-
    rThrow
      "git"
      [ "diff",
        "--exit-code",
        restOfCombined,
        combined
      ]

  echoN "rebase..."
  _ <-
    rThrow
      "git"
      [ "rebase",
        "--quiet",
        "--onto",
        restOfCombined,
        combined,
        current
      ]

  finished <- currentHead io ex
  finishedShort <- short finished
  let branchOrFinishedShort =
        if not (null branch) then branch else finishedShort

  -- Check 3 equality
  echoN "checking equality..."
  rThrow "git" ["diff", "--exit-code", finished, current]

  when (not (null branch)) $ do
    echoN "setting branch to history with split..."
    rThrow "git" ["push", "--quiet", "--force", ".", "HEAD:" <> branch]
    rThrow "git" ["checkout", "--quiet", branch]

  -- Check 3 equality, and we have it checked out
  echoN "checking equality..."
  rThrow "git" ["diff", "--exit-code", "HEAD", current]

  echo "done"
  echo ""
  echo "Splitting finished successfully!"
  echo ""
  if not (null branch)
    then
      echoN ("Your branch is " <> branch <> ".  ")
    else
      echoN "Your HEAD is detached.  "

  currentShort <- short current

  echo
    ( "It was previously "
        <> currentShort
        <> ".  It is now "
        <> finishedShort
        <> "."
    )
  echo ""
  echo "You might want to do exactly one of the following"
  echo ""
  echo "* An interactive rebase to reword the remnants of the split"
  echo ""
  echo
    ( "  $ git rebase --interactive "
        <> afterHandlerShort
        <> " "
        <> branchOrFinishedShort
    )
  echo ""
  echoN "* If you don't want the split after all, "
  echo "reset your branch to what it was before"
  echo ""
  echo ("  $ git reset --hard " <> currentShort)
