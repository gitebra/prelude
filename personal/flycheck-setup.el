;;; flycheck-setup.el --- Flycheck settings
;;; Commentary:
;;; Code:

(message "Enable global flycheck mode...")
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'flycheck-setup)
;;; flycheck-setup.el ends here
