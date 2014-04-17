;;; multi-term-setup.el --- multi-term settings
;;; Commentary:
;;; Code:

(when (require 'multi-term nil t)
  (local-set-key (kbd "<C-next>") 'multi-term-next)
  (local-set-key (kbd "<C-prior>") 'multi-term-prev)
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1)))
  (setq term-bind-key-alist
        (append term-bind-key-alist
                (list (cons "C-c C-j" 'term-line-mode)
                      (cons "C-c C-k" 'term-char-mode))))
  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/local/bin/bash"
        multi-term-program-switches "--login"))

(provide 'multi-term-setup)
;;; multi-term-setup.el ends here
