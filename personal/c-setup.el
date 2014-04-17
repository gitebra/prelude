;;; c-setup.el --- C settings
;;; Commentary:
;;; Code:

(message "Setup C support...")
(require 'cc-mode)

(message "Use subword mode if it's available...")
(defun my/subword-mode (&optional arg)
  "Enable 'subword-mode' if ARG is nil or unset."
  (if (boundp 'subword-mode)
      (subword-mode arg)
    (if (boundp 'c-subword-mode)
        (c-subword-mode arg))))

;; General cc-mode hooks
(message "Enable some cc-mode hooks'...")
(add-hook 'c-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (turn-on-auto-fill)
            (hide-ifdef-mode 1)
            (hs-minor-mode 1)
            (cwarn-mode 1)
            (my/subword-mode)))

(defun my/semi&comma-inden-func ()
  "Break."
  'stop)

;;;; Guess C-indention style
;;(autoload 'guess-style-set-variable "guess-style" nil t)
;;(autoload 'guess-style-guess-variable "guess-style")
;;(autoload 'guess-style-guess-all "guess-style" nil t)
;;(add-hook 'c-mode-common-hook 'guess-style-guess-all)

(message "Define Element C style...")
;; Element C-style
(defconst element-c-style
  '((c-hanging-comment-ender-p    . nil)
    (c-comment-continuation-stars . "*  ")
    (c-basic-offset               . 3)
    (c-comment-only-line-offset   . 0)
    (c-continued-brace-offset     . 3)
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist
                                      c-semi&comma-no-newlines-before-nonblanks))
    (c-hanging-braces-alist       . ((substatement-open after)
                                     (brace-list-open)
                                     (block-close)))
    (c-hanging-colons-alist       . ((member-init-intro before)
                                     (inher-intro)
                                     (case-label after)
                                     (label after)
                                     (access-label after)))
    (c-cleanup-list               . (scope-operator
                                     empty-defun-braces
                                     defun-close-semi))
    (c-offsets-alist              . ((arglist-close     . c-lineup-arglist)
                                     (topmost-intro     . 0)
                                     (cpp-define-intro  . 0)
                                     (inextern-lang     . 0)
                                     (substatement-open . 0)
                                     (case-label        . 3)
                                     (block-open        . 0)
                                     (knr-argdecl-intro . -)
                                     (brace-list-entry  . 0))))
  "Element C Programming Style.")

;; Decide which indentation style to use
(defun my/c-style-hook ()
  "Figure out which c-style to use."
  (message "Running my/c-style-hook.")
  (let ((buf-name (if (buffer-file-name)
                      (buffer-file-name)
                    "")))
    (cond ((or (string-match "/vobs/ose5/" buf-name)
               (string-match "/vobs/ose/delta/" buf-name))
           (c-set-style "stroustrup")
           (setq c-basic-offset 3)
           (setq tab-width 8
                 indent-tabs-mode nil)
           (message "Setting up OSE style"))
          ((or (string-match "/ele-[-_.[:alnum:]]+/" buf-name)
               (string-match "/ws-[-_.[:alnum:]]+/" buf-name))
           (c-set-style "element")
           (message "Setting up Element style"))
          ((or (string-match "/linx/" buf-name)
               (string-match "/umlinx" buf-name))
           (c-set-style "linux")
           (setq indent-tabs-mode t)
           (message "Setting up Linux style"))
          (t (message "Setting style failed")))))

(add-hook 'c-mode-common-hook 'my/c-style-hook)

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun my/c-mode-common-hook ()
  "C-styles I use."
  (c-add-style "element" element-c-style)
  (c-set-offset 'member-init-intro '++)
  (setq tab-width 8
        indent-tabs-mode nil)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (c-toggle-auto-hungry-state -1))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Recognise signal files and OSE  con files in font-lock
(setq auto-mode-alist
      (cons '("\\.sig\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.con\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.conf\\'" . c-mode) auto-mode-alist))

(provide 'c-setup)
;;; c-setup.el ends here
