;;; yaml-setup.el --- YAML settings
;;; Commentary:
;;; Code:

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(message "sls-files (Salt Stack state files) are YAML-files")
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

(provide 'yaml-setup)
;;; yaml-setup.el ends here
