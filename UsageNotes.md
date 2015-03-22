The `vc-clearcase` is a back-end to the Emacs vc interface.  You access most of the ClearCase operations using the general Emacs version control commands which can be accessed through the "Tools/Version Control" menu. See the "Version Control" chapter in the Emacs manual for a documentation.

There are some extra commands added to support ClearCase specific operations, they are appended to the "Tools/Version Control" menu when the current buffer contains a file managed by ClearCase.

If you use ClearCase UCM, see also UcmSupport which presents the UCM specific commands in `vc-clearcase`.



# Installing vc-clearcase #

To install the package, follow these steps:

  * Copy this directory under the site-lisp directory of your GNU/Emacs     installation, or to any directory in your load path.

  * Byte compile vc-clearcase.el and ucm.el.  You can do it from inside Emacs, or from the command line:

> % emacs -batch -f batch-byte-compile vc-clearcase.el
> % emacs -batch -f batch-byte-compile ucm.el

  * Add the following line to your initialization file (~/.emacs.el):

> (load "vc-clearcase-auto")

The ClearCase backend should activate when you open a file inside a ClearCase view.

If you don't have access to the global site-lisp directory, you can create a local one into your home directory, for example "~/.emacs.d/lisp" and add it to the load path by adding this line to the init.el file:

> (setq load-path (append load-path '("~/.emacs.d/lisp")))

# Registering files #

A file inside a ClearCase view can be registered either by typing `C-x v v` or `C-x v i` in the file's buffer or by selecting the file in a `*vc-dir*` buffer and typing `C-x v i`.  From a `*vc-dir*` buffer you can register multiple files at once.  Note however that ClearCase does not have atomic commits, so each file is registered individually.   Registering multiple files is provided as a convenience function in Emacs.

Before registering a file, the directory in which it resides must already be registered with ClearCase.  This is handled automatically by `vc-clearcase`:  if it detects that it has to register a file and the directory is unregistered, it will register the directory first.  This mechanism works for any number of sub directories, so `vc-clearcase` will correctly register a file named "alpha/beta/gamma/delta.txt" when only the "alpha" is registered with ClearCase.

# Checking out a file #

In ClearCase, a file must be checked out before you can edit it.  You checkout a file by typing `C-x v v` on a checked in file.  ClearCase supports two types of checkouts: reserved and unreserved.  Any number of users can checkout a file, but only one reserved checkout must exist.  The user who made the reserved checkout can check in the file.  Users who made unreserved checkouts can only check in if there are no reserved checkouts of the file.

The variable `clearcase-checkout-policy` determines the checkout type that `vc-clearcase` will use.  It can have the following values:

  * heuristic -- a heuristic is used to determine the checkout model. By default it tries to do a reserved checkout, but if `vc-clearcase` determines that the unreserved checkout will fail it will do an unreserved checkout.
  * reserved -- always attempt to do a reserved checkout.  This might fail if someone else has the file checked out reserved or we don't checkout the latest revision on the branch.
  * unreserved -- always do an unreserved checkout.

A log message can also be specified on checkout.  If a comment is specified, it will be used as the default log message when you check in the file (`vc-clearcase` will allow editing the log message before check in).  The variable `clearcase-checkout-comment-type` controls how `vc-clearcase` prompts for the message at checkout.  It can have the following values:

  * normal -- a buffer will be used to enter a log message, just like for file check in
  * brief -- the log message will be read from the mini-buffer
  * none -- no log message will be used on checkout.

# Hijacking files #

In a ClearCase workspace, files are normaly read only. To edit a file, it must be checked out first, which will make it writable. However, you can make a file writable using a "chmod" command, than edit the file without checking it out. ClearCase will consider such a file hijacked. To hijack a file in Emacs toggle the readonly status of a buffer with `toggle-read-only' (C-x C-q), edit the file than save it (Emacs will ask if you want to write to a readonly file). vc-clearcase will detect this situation and mark the file as hijacked (the string "Cc:HIJACKED" will appear in the mode-line).

**NOTE** vc-clearcase maps the hijacked state to the `unlocked-changes' vc-state so Emacs will mention "unlocked changes" when asking questions about a hijacked file.

A hijacked file can be reverted to the original version, discarding any modifications, using `vc-revert-buffer` (`C-x v u`), or it can be checked out, keeping the modifications, using `vc-next-action` (`C-x v v`).

# Checkout/Check in directories #

ClearCase tracks directory versions just as it tracks file versions.  Before a directory is modified, it must be checked out.  Directory modifications are considered adding, removing or renaming a file in that directory.  `vc-clearcase` will automatically checkout a directory if it needs to modify it (when registering, renaming or removing a file) and it will check in the directory when the operation is complete.

When doing many file modifications in sequence, such as renaming several files one-by-one, this approach will create one directory revision for every file operation.  You can avoid this by manually checking out a directory using `M-x vc-clearcase-checkout-directory RET`, perform the file operations that check in the directory when you are finished using `M-x vc-clearcase-checkin-directory RET`.

# Print Log (change history) #

The `C-x v l` (`vc-print-log`) command will print the change log (history) for the current file.  By default only changes from the current branch are displayed.  With a prefix arg (`C-u C-x v l`) the entire file's history is printed.

# Displaying Diffs #

The `C-x v =` (`vc-diff`) command will print a diff between two revisions of a file.  `vc-clearcase` can use the internal ClearCase diff tool or the UNIX diff utility to display the diffs.  This is controlled by the `clearcase-use-external-diff` variable: when it is t, the UNIX diff utility will be used.

Using an external diff program is more useful, because the context or unified diff output is understood by `diff-mode`: you can jump to the source code from the diff output, revert diffs, partially apply them, etc.

If an external diff utility is not available, the internal ClearCase diff can be used.  However, the diffs output by ClearCase are not understood by Emacs, so all you can do is read them.