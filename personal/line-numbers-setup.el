;;; line-numbers-setup.el --- Settings for line numbers in buffers
;;; Commentary:
;;; Code:

;; Line numbers in the left margin
(message "Enable line number...")
(global-linum-mode 1)

(require 'hlinum)
(require 'linum-relative)
(setq linum-format 'dynamic)

;; Toggle line numbers
(global-set-key "\C-x/" 'linum-mode)

;; Toggle relative line numbers
(global-set-key "\C-xr/" 'linum-relative-toggle)

(provide 'line-numbers-setup)
;;; line-numbers-setup.el ends here
