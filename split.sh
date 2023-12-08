set -e

# *3      CURRENT
# .  *3   FINISHED
# .  .
# .  .
# *2 .    COMBINED
# |  *2   REST_OF_COMBINED
# |  |
# |  |
# |  *    AFTER_HANDLER
# |  .
# |  .
# |  .
# |  *
# | /
# |/
# |
# |
# *1      COMBINED^

# Empty string if not a symbolic ref
BRANCH=$(git symbolic-ref --quiet --short HEAD || true)
CURRENT=$(git rev-parse HEAD)
CURRENT_SHORT=$(git rev-parse --short HEAD)
if [ -n "$BRANCH" ]; then
    BRANCH_OR_CURRENT_SHORT=$BRANCH
else
    BRANCH_OR_CURRENT_SHORT=$CURRENT_SHORT
fi
COMBINED_PROVIDED=$2
COMBINED=$(git rev-parse $COMBINED_PROVIDED)
COMBINED_SHORT=$(git rev-parse --short $COMBINED_PROVIDED)
HANDLER=$1

# This is the best way I know of to (print an error message and then
# exit) on failure when set -e is set
git merge-base --is-ancestor $COMBINED $CURRENT || (echo "$COMBINED_PROVIDED is not an ancestor of $BRANCH_OR_CURRENT_SHORT"; false) || exit 1
git diff --quiet || (echo "The repo has uncommitted changes.  Stash, commit or reset them and then try again."; false) || exit 1
# If it's a merge commit (i.e. it has a second parent)
git rev-parse --verify --quiet "$COMBINED^2" > /dev/null && echo "$COMBINED_PROVIDED is a merge commit.  Cannot split." && exit 1 || true

COMBINED_PARENT=$(git rev-parse $COMBINED^)
COMBINED_PARENT_SHORT=$(git rev-parse --short $COMBINED^)

echo -n checkout...
git checkout --quiet $COMBINED
echo -n reset...
git reset --quiet $COMBINED_PARENT
echo done

echo -n "You were on "
if [ -n "$BRANCH" ]; then
    echo -n "branch $BRANCH ($CURRENT_SHORT). "
else
    echo -n "$CURRENT_SHORT. "
fi
echo "You wanted to split the commit"
echo
git show --no-patch --pretty=short $COMBINED
echo
echo -n "I'm now on $COMBINED_SHORT's parent ($COMBINED_PARENT_SHORT). "
echo "I'm going to drop you into your chosen handler: $HANDLER"
echo -n "Please make any number of commits and then exit the handler with "
echo "exit code 0."

set +e
sh -c "$HANDLER"
if [ $? -ne 0 ]; then
    AFTER_FAILED_HANDLER=$(git rev-parse --short HEAD)
    git reset --quiet --hard
    echo "The handler failed at $AFTER_FAILED_HANDLER.  Returning to $BRANCH_OR_CURRENT_SHORT."
    if [ -n "$BRANCH" ]; then
	RETURN_TO="$BRANCH"
    else
	RETURN_TO="$CURRENT"
    fi
    git checkout --force --quiet "$RETURN_TO"
    exit $?
fi
set -e

AFTER_HANDLER=$(git rev-parse HEAD)
AFTER_HANDLER_SHORT=$(git rev-parse --short HEAD)

echo -n reset...
git reset --quiet --hard $AFTER_HANDLER

echo -n checkout...
git checkout --quiet --force $COMBINED

echo -n reset...
git reset --quiet --soft $AFTER_HANDLER

COMBINED_SUBJECT=$(git diff-tree -s --pretty=%s $COMBINED)
COMBINED_BODY=$(git diff-tree -s --pretty=%b $COMBINED)

echo -n commit...
git commit --allow-empty --quiet -m "$COMBINED_SUBJECT" -m "$COMBINED_BODY"

REST_OF_COMBINED=$(git rev-parse HEAD)
# Check 2 equality
echo -n checking equality...
git diff --exit-code $REST_OF_COMBINED $COMBINED

echo -n rebase...
git rebase --quiet --onto $REST_OF_COMBINED $COMBINED $CURRENT

FINISHED=$(git rev-parse HEAD)
FINISHED_SHORT=$(git rev-parse --short HEAD)
if [ -n "$BRANCH" ]; then
    BRANCH_OR_FINISHED_SHORT=$BRANCH
else
    BRANCH_OR_FINISHED_SHORT=$FINISHED_SHORT
fi
# Check 3 equality
echo -n checking equality...
git diff --exit-code $FINISHED $CURRENT

if [ -n "$BRANCH" ]; then
    echo -n setting branch to history with split...
    git push --quiet --force . HEAD:$BRANCH
    git checkout --quiet $BRANCH
fi
# Check 3 equality, and we have it checked out
echo -n checking equality...
git diff --exit-code HEAD $CURRENT

echo done
echo
echo "Splitting finished successfully!"
echo
if [ -n "$BRANCH" ]; then
    echo -n "Your branch is $BRANCH.  "
else
    echo -n "Your HEAD is detached.  "
fi
echo "It was previously $CURRENT_SHORT.  It is now $FINISHED_SHORT."
echo
echo "You might want to do exactly one of the following"
echo
echo "* An interactive rebase to reword the remnants of the split"
echo
echo "  $ git rebase --interactive $AFTER_HANDLER_SHORT $BRANCH_OR_FINISHED_SHORT"
echo
echo "* If you don't want the split after all, reset your branch to what it was before"
echo
echo "  $ git reset --hard $CURRENT_SHORT"
