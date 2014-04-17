;;; clojure-setup.el --- Clojure settings
;;; Commentary:
;;; Code:

;; Eldco in clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide special buffers from appearing in buffer switching commands
(setq nrepl-hide-special-buffers t)

;; Prevent the auto-display of the REPL buffer in a separate window
;; after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Stop the error buffer from popping up while working in buffers
;; other than the REPL
(setq cider-popup-stacktraces nil)

;; Enable error buffer popping also in the REPL
(setq cider-repl-popup-stacktraces t)

;; To auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

(provide 'clojure-setup)
;;; clojure-setup.el ends here
