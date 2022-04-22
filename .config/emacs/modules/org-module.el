;; -*- lexical-binding: t -*-

(add-hook 'org-mode-hook
  (lambda ()

    ;; Enable vim in Org buffers ('evil-default-state' could be 'emacs).
    (evil-set-initial-state 'org-mode 'normal)

    ;; Evil mode breaks section expand in Org. This fixes it.
    (evil-local-set-key 'motion (kbd "<tab>") 'org-cycle))

  )
