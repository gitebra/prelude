;;; auto-complete-setup.el --- Auto-complete settings
;;; Commentary:
;;; Code:

;; Get started

(message "Setup auto-complete...")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20131128.233/dict")
(ac-config-default)
(setq ac-ignore-case nil)

;; C headers
(message "Auto complete c-headers...")
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; etags
(message "Auto complete etags...")
(require 'ac-etags)
(custom-set-variables
 '(ac-etags-requires 1))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(eval-after-load 'etags
  '(progn
     (ac-etags-setup)))

(defun my/c-mode-common-hook ()
  "Auto complete etags."
  (add-to-list 'ac-sources 'ac-source-etags))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

;; Ruby

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; (eval-after-load 'inf-ruby '
;;   '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))


(provide 'auto-complete-setup)
;;; auto-complete-setup.el ends here
