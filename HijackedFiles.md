In a ClearCase workspace, files are normaly read only. To edit a file, it must be checked out first, which will make it writable. However, you can make a file writable using a "chmod" command, than edit the file without checking it out. ClearCase will consider such a file hijacked.

To hijack a file in Emacs toggle the readonly status of a buffer with `toggle-read-only' (C-x C-q), edit the file than save it (Emacs will ask if you want to write to a readonly file). vc-clearcase will detect this situation and mark the file as hijacked (the string "Cc:HIJACKED" will appear in the mode-line).

vc-clearcase maps the hijacked state to the `unlocked-changes' vc-state. Emacs will mention "unlocked changes" when asking questions about a hijacked file.

A hijacked file can be reverted to the original version using `vc-revert-buffer` (`C-x v u`), or it can be checked out using `vc-next-action` (`C-x v v`).