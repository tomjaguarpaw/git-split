How to use git-split:

* Determine the commit you want to split, call its git identifier
  `<commit>`.

* Run `git-split prepareToSplit <commit>`.

* The command will print some information about what to do next and
  how to proceed subsequently. In more detail:

  * The repo is now on a detached `HEAD` at `<commit>`'s parent, with
    `<commit>`'s changes already applied to the working tree but
    unstaged.

  * Edit the files to the desired first intermediate state
    and commit, then repeat for any further commits.

  * The final commit does not have to bring the repo back to the same
    state as `<commit>`.

  * If you're happy with the commit(s) you've split off, run the
    `git-split applySubsequentCommits` command line you were given in
    the output of ``git-split prepareToSplit <commit>`.  Use that
    command verbatim.  Do not reconstruct it, as it embeds the
    original `HEAD` and  commit hashes that must match exactly.  Your
    branch will then return to what it was, with the exception of
    `<commit>` having been split.

  * To abort the process run the `git-split restore` command line you
    were given in the output of `git-split prepareToSplit <commit>`,
    again verbatim.  You will then be returned to the state you were
    in before you ran ``git-split `prepareToSplit <commit>`.

## What to look for when splitting

Each commit split off should be independently correct (for example, it
should compile and pass tests) and represent a single logical change.
Commits should either be behaviour-preserving refactorings, or
behaviour-changing but as small and focused as possible so it is
obvious what they do.  Here are patterns that commonly appear as
splittable sub-changes within a larger commit:

* **Two independent changes to different functions**:  If a commit
  changes two functions then it is often possible to make one of the
  changes in one commit and the other in a subsequent commit.

## Valid Haskell transformations

These are behaviour-preserving Haskell-specific rewrites that can be
split off as their own commits:

* **Eta-expansion/eta-reduction**: A commit that also eta-expands or
  eta-reduces a call site can have that step split out on its own, for
  example transforming

  ```.hs
  let f x = g x + h x
  in map f ys
  ```

  to

  ```.hs
  map (\x -> g x + h x) ys
  ```

  can be split into two separate commits, firstly eta expanding

  ```.hs
  let f x = g x + h x
  in map (\x -> f x) ys
  ```

  and then inlining to achieve the final result.

* **Splitting traversals**: A commit that breaks a data structure over
  which it traverses into two pieces can have the breaking of the
  structure into two and the separation of the traversals as two
  separate commits, for example

  ```.hs
  let l = [1 .. 10] <> [100 .. 200]
  in for_ l body
  ```

  can be rewritten as

  ```.hs
  do
    for_ [1 .. 10] body
    for_ [100 .. 200] body
  ```

  but to simplify the change an intermediate commit that inlines `l`
  can be split off:

  ```
  for_ ([1 .. 10] <> [100 .. 200])
  ```

* **Extracting plus simplification**: When extracting a new binding it
  is tempting to simplify the body, for example changing

  ```.hs
  calc x y = x + x + x - y * y
  ```

  to

  ```.hs
  triple x = 3 * x
  calc x y = triple x - y * y
  ```

  but for clarity the simplification of the body can be done before or
  after the extraction, for example, simplifying first to

  ```.hs
  calc x y = 3 * x - y * y
  ```
