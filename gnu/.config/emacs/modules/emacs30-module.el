;; -*- lexical-binding: t -*-

;; Stuff that's coming in Emacs 30

;; Może być spoko do zatrzymania buffera w jednym oknie
;; C-x w d - toggle-window-dedicated

;; Ciekawe czy będzie mulić, ale może poprawić wygląd linii które się zawijają:
;; visual-wrap-prefix-mode

;; Podświetlanie przy stepie w debuggerze:
;; (setopt gud-highlight-current-line t)

;; kill-matching-buffers-no-ask

;; Czasem by się przydało: Info-url-alist

;; Log-Edit buffers now display a tool bar.

;; Wow, nice:
;; diff-ignore-whitespace-hunk' can now be applied to all hunks.
;; When called with a non-nil prefix argument,

;; VERY nice:
;; New command 'diff-apply-buffer' bound to 'C-c RET a'.

;; Have to check it out
;; *** New command 'replace-regexp-as-diff'.
;; It reads a regexp to search for and a string to replace with, then
;; displays a buffer with replacements as diffs.  After reviewing the
;; changes in the output buffer you can apply the replacements as
;; a patch to the current file buffer.  There are also new commands
;; 'multi-file-replace-regexp-as-diff' that shows as diffs replacements
;; in a list of specified files, and 'dired-do-replace-regexp-as-diff'
;; that shows as diffs replacements in the marked files in Dired.


;; dired-filename-display-length - obcina długie pathnamy

;; *** New command 'dired-do-open'.
;; This command is bound to "Open" in the context menu

;; I wanted this:
;; *** New value 'historical' for user option 'completions-sort'.
;; When 'completions-sort' is set to 'historical', completion candidates
;; will be first sorted alphabetically, and then re-sorted by their order
;; in the minibuffer history, with more recent candidates appearing first.
(setopt completions-sort 'historical)

;; First time I'm hearing about Which-function-mode:
;; (setopt which-func-display 'header)

;; Connection method "kubernetes" supports now optional container name.
;; The host name for Kubernetes connections can be of kind [CONTAINER.]POD,
;; in order to specify a dedicated container.


;; Maybe this will make tramp faster?
;; (setopt tramp-use-connection-share t)

;; Need to check it out:
;;; (setopt flymake-show-diagnostics-at-end-of-line t)

;; Sounds very cool
;; *** New minor mode 'completion-preview-mode'.
;; This minor mode shows you symbol completion suggestions as you type,
;; using an inline preview.  New user options in the 'completion-preview'
;; customization group control exactly when Emacs displays this preview.


;; Add Tramp methods dockercp and podmancp
;; - May allow to copy files from bare bone containers with no sh?


;; *** New optional connection method "run0".

;; nowe makro z kosmosu
;; Richard Stallman 2024-08-02 Install cond*


;; chyba coś z długimi liniami w eww
;; Jim Porter 2024-08-04 Improve SHR/EWW support for 'visual-wrap-prefix-mode'


;; kopiuje tekst bez patchy
;; *** New command 'diff-kill-ring-save'


;; kiedyś może się przydać
;; Michael Albinus 2024-09-11 Allow to disable symbolic links check in Dired

;; Visuwesh 2024-09-09 Make the *grep* buffer editable


;; no nie wiem, przecież raczej wiem co klikam
;; Philip Kaludercic 2024-09-03 Allow 'kill-region' to kill the last word when there is no region


;; podświetlanie docstringów
;;  *** New user option 'java-ts-mode-enable-doxygen'.


;; mocne
;; *** New command 'diff-revert-and-kill-hunk' bound to C-c M-r.
;; Sean Whitton 2024-09-30 New command diff-delete-other-hunks
;; Dmitry Gutov 2024-10-08 Support file creation and deletion in diff-apply-hunk


;; może się przydać
;; Sean Whitton 2024-10-03 New M-~ entry in save-some-buffers-action-alist


;; o, szukałem
;; +** New function 'native-compile-directory'.

;; zobaczymy
;; *** Using 'e' from Log View mode to modify change comments now works for Git.
;; *** 'vc-clone' is now an interactive command.

;; Fix C-c C-d and C-c C-w during log-view-modify-change-comment
