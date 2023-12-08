# `git-split`

## What is `git-split`?

`git-split` is the name of this project, which provides a shell
script, called `split.sh`, for splitting `git` commits.

## Quickstart

Suppose you have, somewhere in your git history, this commit that
contains two logically separate changes and you want to split it into
two separate commits.  You can do that with `git-split`!

```diff
commit abc123...

  Add foobar() and be verbose

+def foobar():
+    x = foo()
+    bar(x, True)
+
 def baz():
-    quux(n=42, verbose=False)
+    quux(n=42, verbose=True)
```

To split it, simply run

```sh
$ sh .../split.sh bash abc123
```

`git-split` will drop you into a new `bash` shell, ready to split the
commit `abc123` into separate pieces.  For example, you might want to
use `git commit --patch --verbose` and commit just the addition of
`foobar()`.  You'll get a commit like the following.

```diff
commit def456...

  Add foobar()

+def foobar():
+    x = foo()
+    bar(x, True)
+
```

Then type `exit 0`.  `git-split` will commit the rest of `abc123` on
top and return you to your shell.  You'll get another commit, on top
of the one that added `foobar()`, like the following.

```diff
commit ghi789...

  Add foobar() and be verbose

 def baz():
-    quux(n=42, verbose=False)
+    quux(n=42, verbose=True)
```

This commit gets the commit message of `abc123` so you might want to
follow `git-split`'s suggestion about rewording the remnants of the
split.

If you make a mistake type `exit 1` and `git-split` will return you to
where you were when you started.

That's it!

## Usage summary

1. Run `sh .../split.sh bash <id-of-commit-to-split>`

   (where `.../` is the path to where you put `split.sh` on your file
   system, and you can use your favorite shell in place of `bash`, of
   course)

2. Commit the first part of the split

   (you might like to use `git commit --patch --verbose`)

3. Run `exit 0`

   (or if you get stuck you can run `exit 1` and the split will be
   cancelled, returning you to where you started)

## Questions and feedback

If you have any difficulty using `split.sh`, any questions or any
feedback, then [file an
issue](https://github.com/tomjaguarpaw/git-split/issues/new).
