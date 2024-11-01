;; -*- lexical-binding: t -*-

;; Uzupełnianie nazw plików.
(autoload 'comint-filename-completion "comint")
(add-to-list 'completion-at-point-functions 'comint-filename-completion)

(advice-add 'file-name-all-completions :filter-return
  (lambda (paths)
    (cl-remove-if (lambda (path) (member path '("./" "../"))) paths))
  '((name . kaspi/skip-uninteresting)))

;;;; Te trzy libki robią to co najpierw implementowałem sam a zawsze miało bugi.

;; Orderless - dobre gdy nie pamięta się kolejności poszczególnych części nazw.
(add-to-list 'load-path (concat +vendor-dir+ "orderless"))
(require 'orderless)
(add-to-list 'completion-styles 'orderless)
(add-to-list 'orderless-matching-styles #'orderless-initialism)
(add-to-list 'completion-category-overrides '(file (styles basic partial-completion)))

;; Corfu sprawia że uzupełnienia pod wskaźnikiem pokazują się tuż obok niego.
;; Dodatkowo pozwala na wyszukiwanie w stylu orderless.
(add-to-list 'load-path (concat +vendor-dir+ "corfu"))
(require 'corfu)
(setq corfu-quit-at-boundary nil)
(global-corfu-mode)

;; Vertico dodaje następujące:
;; - domyślnie sortuje podpowiedzi po historii ich wcześniejszego wybierania
;; - działa z orderless
;; - odświeża podpowiedzi tuż po wpisaniu nowego znaku, tym samym działa z kb
;;   makrami z czym wcześniej miałem problem dla minibuffer-completion-help
(add-to-list 'load-path (concat +vendor-dir+ "vertico"))
(require 'vertico)
(define-key vertico-map (kbd "C-j") 'vertico-exit-input)
(define-key vertico-map (kbd "M-?") 'minibuffer-completion-help)
(define-key vertico-map (kbd "M-<up>") 'minibuffer-previous-completion)
(define-key vertico-map (kbd "M-<down>") 'minibuffer-next-completion)
(define-key vertico-map (kbd "M-RET") 'minibuffer-choose-completion)
(define-key vertico-map (kbd "M-g M-c") 'switch-to-completions)

(setq completions-sort 'historical)

(vertico-mode)
