Before moving to Google Code, I had two Mercurial repositories, one for the Emacs22 release and one for the Emacs23 release.  The vc package has changed in Emacs between versions 22 and 23, making it impractical to have a single package that supports both.

Google Code only supports one Mercurial repository, so I decided to put the two branches in the same repository.  Unfortunately, this has made the "default" branch useless.

There are two branches that are "valid"

  * `emacs-22` used for the Emacs22 release.  This branch is in maintenance mode: no new features, only bugfixes
  * `emacs-23` used for the Emacs23 release.  This is in active developement.

If you are browsing the code, make sure you select one of the above two branches, otherwise the default branch will be shown which is an older version.

If you have cloned the repository, make sure you switch to the branch you want using:

```
hg update -C emacs-23
```

or

```
hg update -C emacs-22
```