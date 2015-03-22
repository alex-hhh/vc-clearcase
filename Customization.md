You can change the behavior of vc-clearcase by setting different values for some of the variables defined in the package. You can bring up the customization page for vc-clearcase using:

```
M-x customize-group <RET> vc-clearcase <RET>
```

Each variable is documented in the Customize buffer, but below are descriptions for the variables which I consider most important.

# Using an external diff program for comparisons #

The default diff utility in ClearCase does not produce context or unified diffs, which are the diff types that Emacs can understand and manipulate. You can instruct vc-clearcase to use the 'diff package from Emacs for all the version comparisons by setting the `clearcase-use-external-diff' variable to 't.

This variable can also be edited via Customize.

# Comment on checkout #

ClearCase requires files to be checked out before editing them and also requires a comment to be supplied. The variable `clearcase-checkout-comment-type' controls how the comment is specified. It can be set to one of:

  * 'normal -- a buffer will be used to enter a comment, just like for file checkin,
  * 'brief -- the comment will be read from the minibuffer,
  * 'none -- no comment will be used on checkout.

This variable can also be edited via Customize.

# Applying labels #

In ClearCase a label exists independently from the files it is applied to. A label must be created first before it can be applied. The variable `clearcase-no-label-action' determines how vc-clearcase behaves when asked to apply a label that does not exist. There are three possible values for this variable:

  * 'error -- an error will be signalled when we are asked to apply a non existent label.
  * 'create -- the label will be created if it does not exist,
  * 'ask -- ask the user whether she wants to create the label or not.

This variable can also be edited via Customize.

# Removing empty branches #

When a checkout operation creates a new branch, the reverting the checkout (uncheckout) will leave the element on that branch with a version of 0 which is identical to the parent version. If you use lots of branches, the version tree will become littered with useless branches. These empty branches will also force ClearCase to perform merges on the file when not needed.

vc-clearcase will remove an empty branch when the `clearcase-rmbranch-on-revert-flag' flag is set to 't . A new checkout in the same view will recreate the branch.