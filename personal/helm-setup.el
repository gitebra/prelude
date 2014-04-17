;;; helm-setup.el --- My helm settings

;;; Commentary:

;;; Code:

(require 'helm-locate)
(require 'auto-complete-config)

(setq helm-locate-command "locate-with-mdfind %.0s %s")
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(provide 'helm-setup)
;;; helm-setup.el ends here
