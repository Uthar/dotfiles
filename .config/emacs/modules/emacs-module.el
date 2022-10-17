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
 enable-local-variables nil

 ;; Disable echo messages when using emacsclient.
 server-client-instructions nil

 ;; Ask for y/n rather than yes/no.
 use-short-answers t

 ;; Disable an impure config source.
 custom-file null-device

 ;; Disable dialog boxes.
 use-dialog-box nil

 ;; Text editing sane defaults.
 require-final-newline t

 ;; For some reason, these don't set with 'setq'. Works witn
 ;; 'custom-set-variable' instead.
 ;; indent-tabs-mode nil
 ;; tab-width 4

 ;; Less jumpy scroll.
 scroll-margin 4
 scroll-conservatively 1000
 next-screen-context-lines 28

 ;; Clear the scratch buffer.
 initial-scratch-message ""

 ;; Disable the startup screen.
 inhibit-startup-screen t

 ;; Keep previous backup files.
 version-control t
 delete-old-versions t
 kept-old-versions 6

 ;; Prevent filesystem trashing.
 backup-directory-alist
 `((".*" . ,(concat user-emacs-directory "backups/")))
 auto-save-file-name-transforms
 `((".*" ,(concat user-emacs-directory "auto-save/") t))
 create-lockfiles nil

 ;; Useful for jumping a number of lines with universal-argument.
 display-line-numbers-type 'relative

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

 )

(setq-default fill-column 80)
(setq-default cursor-type 'bar)

;; See comment above.
(custom-set-variables
 '(tab-width 4)
 '(indent-tabs-mode nil))

(add-hook 'after-init-hook
  (lambda ()

    ;; Use UTF-8 by default.
    (set-language-environment "UTF-8")

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
    (windmove-default-keybindings)
    (windmove-swap-states-default-keybindings)

    ;; Repeatable undo, other-window etc.
    (repeat-mode)

    ;; Nicer completion
    (fido-vertical-mode)

    ))

;; Add executable bit when shebang detected.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Only show line numbers in editing-heavy modes, where it's useful.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook 'display-fill-column-indicator-mode)
