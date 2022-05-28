;; -*- lexical-binding: t -*-

(with-eval-after-load 'nix-mode

  ;; Copy the hash of the unpacked tarball under point.
  (defun kaspi/nix-prefetch-tarball-at-point ()
    (interactive)
    (let ((hash (shell-command-to-string
                 (concat "nix-prefetch-url --unpack "
                         (ffap-string-at-point)
                         " 2> /dev/null"))))
      (kill-new (string-trim hash))
      (message "Copied %s to kill ring" (string-trim hash))))

  (define-key nix-mode-map (kbd "C-x n h") 'kaspi/nix-prefetch-tarball-at-point))

  
(add-to-list 'load-path (concat +vendor-dir+ "nix-mode"))

(autoload 'nix-mode "nix-mode")

;; 'nix-repl' is actually in nix-repl.el, but this is convenient to be
;; able to load the rest of the library by running 'nix-repl'
(autoload 'nix-repl "nix-mode" "" t)

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
