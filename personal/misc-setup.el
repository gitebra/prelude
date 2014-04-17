;;; misc-setup.el --- <enter description here>
;;; Commentary:
;;; Code:

;; Macros to determine the environment.
;; First, emacs flavor
(defun aquamacs-p ()
  (string-match "Aquamacs" (version)))

(defun fsf-emacs-p ()
  (string-match "GNU" (version)))

(defun xemacs-p ()
  (string-match "XEmacs" (version)))

;; then which OS we're at
(defun linux-emacs-p ()
  (string-match "linux" (version)))

(defun mac-emacs-p ()
  (string-match "-apple-" (version)))

(defun solaris-emacs-p ()
  (string-match "solaris" (version)))

;; and last, version
(defun emacs-21-p ()
  (string-match "21" (version)))

(defun emacs-22-p ()
  (string-match "22" (version)))

(defun emacs-23-p ()
  (string-match "23" (version)))

(defun emacs-24-p ()
  (string-match "24" (version)))

;; Check correctness of predicates

;; CL provides assert
(require 'cl)

(assert (or (aquamacs-p) (fsf-emacs-p) (xemacs-p)))
(assert (or (linux-emacs-p) (mac-emacs-p) (solaris-emacs-p)))

;; Macros to determine emacs provider

(defmacro when-aquamacs (&rest body)
  (if (aquamacs-p)
      `(progn ,@body)))

(defmacro when-fsf-emacs (&rest body)
  (if (fsf-emacs-p)
      `(progn ,@body)))

(defmacro when-linux-emacs (&rest body)
  (if (linux-emacs-p)
      `(progn ,@body)))

(defmacro when-mac-emacs (&rest body)
  (if (mac-emacs-p)
      `(progn ,@body)))

(defmacro when-xemacs (&rest body)
  (if (xemacs-p)
      `(progn ,@body)))

;; personal/non-standard modules found here
(add-to-list 'load-path "~/.emacs.d/personal/modules")

;; Disable bell function
(message "Disable the bell...")
(setq ring-bell-function 'ignore)

;; Swedish
(when (>= emacs-major-version 22)
  (message "Swedish language environment...")
  (set-language-environment "Swedish"))

;; No scroll bar
(message "Disable scroll bars...")
(scroll-bar-mode -1)

;; Time in mode line
(message "Enable 24h time in mode-line...")
(setq display-time-day-and-date t)
(require 'time)
(setq display-time-24hr-format t)
(display-time-mode)

;; Resize help windows
(message "Enable resize help window...")
(require 'resize-help-window)
(help-window-resize-mode 1)

;; 78 column wide lines when Fill mode is on
(setq fill-column 78)

;; My mail address
(setq user-mail-address "jonas.johansson@enea.com")

;; Join lines leaving one space
(global-set-key "\C-x?" 'join-line)

;; Brings visual feedback to some operations by highlighting portions
;; relating to the operations.
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Treat undo history as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Hide autosave-files
;; (setq backup-directory-alist
;;       (cons
;;        '("." . "~/.backup") backup-directory-alist))

;; README is a text file
(setq auto-mode-alist
      (cons '("\\README\\'" . text-mode) auto-mode-alist))

(provide 'misc-setup)
;;; misc-setup.el ends here
