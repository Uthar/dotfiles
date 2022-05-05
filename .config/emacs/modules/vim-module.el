;; -*- lexical-binding: t -*-

(setq

 ;; 'universal-argument' is bound to <f12> instead.
 ;; C-u C-d for scrolling is arguably nicer than C-v M-v, because it
 ;; doesn't require switching modifiers. Other than just being used to
 ;; it.
 evil-want-C-u-scroll t

 ;; Prevent clobbering kill ring when pasting over a region.
 evil-kill-on-visual-paste nil

 ;; Use Emacs 28 built in redo.
 ;;
 ;; Was using undo-tree in the past, but it had performance problems
 ;; such as long GC's when reaching the history limit, and reliability
 ;; problems where it would lose the undo history.
 evil-undo-system 'undo-redo

 ;; Want to keep as many Emacs keys as possible - prefer to use Vim
 ;; keys strictly for editing text.
 evil-want-keybinding nil

 ;; Arguably nice to be able to do C-e C-x C-e in SLIME.
 evil-move-beyond-eol t

 ;; Preserve Emacs defaults as much as possible.
 ;; Fixes annoying breakage of help, grep and other buffers.
 evil-motion-state-modes nil
 evil-insert-state-modes nil

 )

(with-eval-after-load 'evil
  ;; Restore Emacs keybindings that are shadowed by default.
  (evil-global-set-key 'motion (kbd "C-e") nil)
  (evil-global-set-key 'insert (kbd "C-e") nil)
  (evil-global-set-key 'insert (kbd "C-a") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'motion (kbd "C-f") nil)
  (evil-global-set-key 'motion (kbd "C-b") nil)
  (evil-global-set-key 'normal [remap yank-pop] nil)
  (evil-global-set-key 'normal (kbd "C-n") nil)
  (evil-global-set-key 'normal (kbd "C-p") nil)
  (evil-global-set-key 'insert (kbd "C-n") nil)
  (evil-global-set-key 'insert (kbd "C-p") nil)
  (evil-global-set-key 'normal (kbd "M-.") nil))

(add-to-list 'load-path (concat +vendor-dir+ "evil"))
(autoload 'evil-mode "evil")

;; NOTE(kasper): Shouldn't use evil-local-mode, as that is not what
;; Evil documentation says to do. Doing it anyway breaks C-[
;; ('evil-force-normal-state') in insert mode.
;;
;; Can still configure Evil to be only enabled in chosen major modes
;; via 'evil-default-state', 'evil-set-initial-state' and
;; 'evil-buffer-regexps'. See the relevant documentation for details.
    
(setq evil-default-state 'emacs)

(with-eval-after-load 'evil
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'org-mode 'normal)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-set-initial-state 'yaml-mode 'normal))

(add-hook 'after-init-hook 'evil-mode)
