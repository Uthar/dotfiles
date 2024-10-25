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
