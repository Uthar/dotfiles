;; -*- lexical-binding: t -*-

;; This file enables some commands that Emacs disables by default,
;; thus preventing an interruption popup when these would be invoked.
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
