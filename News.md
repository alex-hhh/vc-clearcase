# June 4, 2013, version 3.7 has been released #

  * Fixes checkin for Emacs 23.3 ([Bug #16](https://code.google.com/p/vc-clearcase/issues/detail?id=16))
  * vc-dir lists missing and special selection files
  * new variable 'clearcase-use-fprop-cache', used to enable caching of ClearCase status when a file is closed. The file will be opened faster since the ClearCase status need not be retrieved. Enabling this option will create folders named ".vc-clearcase" in each ClearCase controlled directory containing the saved information. The folders can be safely removed.
  * Various other minor fixes, see ChangeLog for more details


# May 18, 2011, version 3.6 has been released #

Version 3.6 of the vc-clearcase package was released with the following changes:

  * revision completion is available for VC commands that prompt for revisions. The revision table is not read until TAB is pressed, as there is a small delay for reading it.
  * new function `ucm-delete-activity' for deleting UCM activities   you can only delete an activity if it is not locked and has no revisions associated with it.
  * Fix problem with checking out files under Emacs 23.3 ([Bug #11](https://code.google.com/p/vc-clearcase/issues/detail?id=11))
  * Faster diffs when an external diff program is used (`clearcase-use-external-diff` is t).  vc-clearcase will no longer load different file revisions in Emacs before diff-ing them.
  * Various compatibility fixes for Emacs 23.3, see ChangeLog for more details.


# Jan 20, 2011, version 3.5 has been released #

Version 3.5 of the vc-clearcase package was released with the following changes:

  * recognize missing and "special selection" files
  * the view tag and vob tag are displayed in **vc-dir** listings
  * the version tree browser can be started form a **vc-dir** buffer
  * more robust indentification of VOB tags for ClearCase installations where the VOBs are not in the default directory
  * various small bug-fixes, see ChangeLog for more details

# Jul 18, 2010, version 3.4 has been released #

Version 3.4 of the vc-clearcase package was released.  This is a bugfix release, containing the following fixes:

  * Clearcase 7 compatibility on Windows ([Bug #3](https://code.google.com/p/vc-clearcase/issues/detail?id=3))
  * Create versioned backups for long paths and versions ([Bug #4](https://code.google.com/p/vc-clearcase/issues/detail?id=4))
  * Better compatibility with Emacs vc when setting up diff buffers
  * When hijacking a file, it is now made writable on disk
  * Ability to remove a file which has checkouts in other branches
  * Re-wrote annotation display code to support a flexible date format