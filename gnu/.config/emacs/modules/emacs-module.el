;; -*- lexical-binding: t -*-

;; This file makes changes to the default built-in variables.
;; Also initializes Emacs with useful built-in modes.

(setq

 ;; Extra information in completions.
 completions-detailed t

 ;; Don't revert window config when quitting minibuffer.
 read-minibuffer-restore-windows nil

 ;; Enable M-x from inside another M-x.
 enable-recursive-minibuffers t

 ;; Squash mode line when window is small.
 mode-line-compact 'long

 ;; Disable annoying local variable pop-ups.
 enable-dir-local-variables nil
 enable-local-variables :safe

 ;; Disable echo messages when using emacsclient.
 server-client-instructions nil

 ;; Ask for y/n rather than yes/no.
 use-short-answers t

 ;; Disable an impure config source.
 custom-file "~/.config/emacs/custom.el"

 ;; Enable dialog boxes.
 use-dialog-box t

 ;; Text editing sane defaults.
 require-final-newline t

 ;; Less jumpy scroll.
 scroll-margin 4
 scroll-conservatively 1000
 scroll-preserve-screen-position nil
 next-screen-context-lines 10

 ;; Może kiedyś włączę.
 ;; scroll-error-top-bottom t

 ;; Clear the scratch buffer.
 initial-scratch-message ""

 ;; Disable the startup screen.
 inhibit-startup-screen t

 ;; Create backup files afresh instead of moving the original file.
 backup-by-copying t

 ;; Don't do it, because it goes to tmp and changing it is too hairy.
 remote-file-name-inhibit-auto-save t

 ;; Disable session recovery.
 auto-save-list-file-prefix nil

 ;; Prevent filesystem trashing.
 create-lockfiles nil

 ;; Display line and column numbers in the modeline.
 column-number-mode t
 line-number-mode t

 ;; Faster keypress feedback.
 echo-keystrokes 0.05

 ;; More throughput for subprocesses.
 read-process-output-max (* 1024 1024)

 ;; Automatically select and reuse help windows
 help-window-select t
 help-window-keep-selected t

 ;; Don't display undo history overflow warnings
 warning-suppress-types '((undo discard-info))

 ;; Auto open dired when switching projects
 project-switch-commands 'project-dired

 ;; Don't visit uninteresting buffers when switching
 switch-to-prev-buffer-skip-regexp
 (regexp-opt '("*inferior-lisp*" "*slime-events*" "*direnv*"))

 ;; Display isearch match count in modeline
 isearch-lazy-count t

 ;; Pokazuj bieżący projekt w modeline
 ;; BUG Causes general slowness when browsing sbcl source code in /nix/store
 ;; Probably project.el is trying to find a project in this huge directory
 ;; project-mode-line t

 ;; Never attempt to write unsaved files when compiling, because this feature
 ;; doesn't work and asks for unrelated files.
 compilation-save-buffers-predicate (lambda () nil)

 ;; Disable default help, because I know about that already
 echo-keystrokes-help nil

 ;; Ukryj szum
 erc-hide-list '("JOIN" "PART" "QUIT")
 
 )

;; Space indentation
(setopt indent-tabs-mode nil)
(setopt tab-width 2)
(setq c-basic-offset 2)
(setq sh-basic-offset 2)

(setq-default fill-column 80)
(setq-default cursor-type 'bar)

;; Never try to save password on disk
(setq auth-source-save-behavior nil)

(add-hook 'after-init-hook
  (lambda ()

    ;; Use UTF-8 by default.
    (set-language-environment "UTF-8")

    ;; Musi być tutaj - z top levelu nie działa, bo coś nadpisuje.
    (setopt default-input-method "polish-slash")

    ;; Show the depth of the minibuffer.
    (minibuffer-depth-indicate-mode t)

    ;; Save some screen estate by disabling bulky widgets.
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

    ;; Remember where point was when visiting previous buffers.
    (save-place-mode t)

    ;; Highlight matching parenthesis.
    (show-paren-mode t)

    ;; Enable window configuration change tracking and undo.
    (winner-mode t)

    ;; Enable recent files tracking and visiting.
    (recentf-mode)

    ;; Remember minibuffer commands between sessions.
    (savehist-mode)

    ;; Enable context menu with right mouse click.
    (context-menu-mode)

    ;; Save performance when encountering very long lines.
    (global-so-long-mode)

    ;; Window movement keybindings
    ;; (windmove-default-keybindings)
    ;; (windmove-swap-states-default-keybindings)

    ;; Repeatable undo, other-window etc.
    (repeat-mode)

    ;; Override selected text on yank and insert
    (delete-selection-mode)

    ))

;; Add executable bit when shebang detected.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook 'display-fill-column-indicator-mode)
