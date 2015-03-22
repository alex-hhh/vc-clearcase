vc-clearcase offers some support for ClearCase UCM.  All current support is around UCM Activities because this is close to the file-oriented nature of the VCS operations in Emacs.  In particular, there is no support for UCM Project, Stream and Baseline management.

The UCM commands below are not bound to any keys.  Instead you need to invoke them using `M-x` (e.g `M-x ucm-browse-activity`).



# View and Set the current activity (and maybe create it) #

**ucm-show-current-activity** will display the name of the current activity in the message buffer.  When invoked with a prefix argument, the number of files, number of revisions and the number of checked out files will also be displayed.

**ucm-set-activity** will set an activity in the current view, replacing the previous one.  The user is prompted to select one of the activities in the stream.  Two special activity names are recognised `*NONE*`, which will unset the current activity and `*NEW-ACTIVITY*` which will prompt the user to create a new activity.

# Locking, Obsoleting and Unlocking activities #

**ucm-lock-activity** will lock an activity in the current stream.  After the activity is locked, no files can be checked out under that activity.  When invoked with a prefix argument (`C-u M-x ucm-lock-activity`), the activity will be marked obsolete.

**ucm-unlock-activity** will unlock an activity.  The user must select from one of the activites in the stream.  Obsolete activities are now allowed to be unlocked unless the command is invoked with a prefix argument (`C-u M-x ucm-unlock-activity`)

# Checking-in all files under an activity #

**ucm-checkin-activity** will prompt for an activity name and check-in all the files that are checked out as part of that activity.  In the log buffer that pops-up, you can type `C-x C-f` to find out the list of files that will be checked in.

**NOTE** If you are familiar with other VCS systems, this command might not check in all checked-out files: if you have files checked out under different activities, only the files of the specified activity will be checked in.

# Listing and Browsing the activities in a stream #

**ucm-list-activities** will pop-up a buffer (`*UCM Activity List*`) containing a list of activities in the current stream.  When invoked with a prefix argument (`C-u M-x ucm-list-activities`), obsolete activities will also be included in this list.  The following actions are available in the `*UCM Activity List*` buffer:

|Key|Action|
|:--|:-----|
|RET or click|invoke `ucm-browse-activity` on the activity (see below)|
|g |refresh the activities in the buffer|
|m |mark/unmark an activity|
|u |unlock the marked activties or the selected activity if none are marked|
|s |set the selected activity in the view (equivalent of `ucm-set-activity`)|

**ucm-browse-activity** will pop-up a buffer (`*UCM Activity Browser*`) displaying information about the selected activity.  This includes the file versions, any checked out files and the contributor activities for a deliver or rebase activity.  The following actions are available in the `*UCM Activity Browser*`) buffer:

Clicking or hitting RET on an item depends on the item:

  * on a directory name -- a dired buffer for the directory will pop-up
  * on a file name -- the file will be visited
  * on a revision or checkout -- a diff buffer will pop up, showing the differences of that revision and its parent.

|Key|Action|
|:--|:-----|
|g |refresh the `*UCM Activity Browser*` buffer|
|e |invoke an ediff session on the current revision and the previous one|
|v |visit the selected revision of the file|
|m |select/unselect a revisions.  On a file, all the files revisions are selected, on a directory, all the revisions of the files in that directory are selected|
|c |check-in the selected checkouts|
|r |revert the selected checkouts|
|t |transfer the selected revisions to another activity|
|O |optimise the display -- this means that consecutive revisions of a file will be collapsed into a single revision and diff operations will work on their parent.  This can simplify the display when there are a lot of revisions of a file within the same activity.|

**HINT:** `ucm-browse-activity` will reuse the `*UCM Activity Browser*` buffer if invoked for a different activity.  If you want to browse two activities at the same time, you can rename the first activity buffer to something else, using `M-x rename-buffer` before calling `ucm-browse-activity` again.  A similar technique can be used with the `*UCM Activity List*` buffer.