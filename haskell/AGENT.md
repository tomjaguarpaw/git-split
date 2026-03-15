How to use git-split:

* Determine the commit you want to split, call its git identifier
  `<commit>`.

* Run `git-split prepareToSplit <commit>`.

* The command will print some information about what to do next and
  how to proceed subsequently, but in brief:

  * Make one or more commits that represent the splitting of
    `<commit>`.

  * If you're happy with the commits you've made, run the `git-split
    applySubsequentCommits` command line you were given in the output
    of ``git-split prepareToSplit <commit>`.  Your branch will then
    return to what it was, with the exception of the `<commit>` having
    been split.

  * To abort the process run the `git-split restore` command line you
    were given in the output of ``git-split prepareToSplit <commit>`.
    You will then be returned to the state you were in before you ran
    ``git-split `prepareToSplit <commit>`.
