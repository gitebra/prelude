;;; console-setup.el --- Config for emacs when run in a terminal

;;; Code:

;; Change theme depending on whether emacs runs in a terminal or in a window
(defun p/pick-color-theme (frame)
  "Set theme if FRAME is a window."
  (select-frame frame)
  (if (window-system frame)
      (progn
        (message "Enable zenburn theme on window...")
        (enable-theme 'zenburn)
        (message "Set default font inconsolata..."))
    (progn
      (message "Disable theme on terminal...")
      (disable-theme 'zenburn))))

;; Disable global-hl-mode in a terminal.
(defun p/pick-hl-mode (frame)
  "Set global-hl-mode if FRAME is a window."
  (select-frame frame)
  (if (window-system frame)
     (progn
       (message "Enable global-hl-line mode...")
       (global-hl-line-mode 1))
    (progn
      (message "Disable global-hl-line mode...")
      (global-hl-line-mode -1))))

;; Set line number format when running in a terminal.
(defun p/set-term-line-number-format (frame)
  "Set line number format when FRAME is not a  window."
  (select-frame frame)
  (unless  (window-system frame)
    (message "Set line number format in terminals...")
    (setq linum-format "%5d ")))

(defun p/disable-menu-bar (frame)
  "Disable menu-bar if FRAME is a terminal."
  (select-frame frame)
  (unless (window-system frame)
    (message "Disable menu-bar...")
    (menu-bar-mode -1)))

;; Set face-font incosolata when emacs runs in a window.
(defun p/set-face-font-inconsolata (frame)
  "Set face font inconsolata if FRAME is a window."
  (select-frame frame)
  (if (window-system frame)
      (set-face-font
       'default
       "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")))

(add-hook 'after-make-frame-functions 'p/pick-color-theme)
(add-hook 'after-make-frame-functions 'p/pick-hl-mode)
(add-hook 'after-make-frame-functions 'p/set-term-line-number-format)
(add-hook 'after-make-frame-functions 'p/disable-menu-bar)
(add-hook 'after-make-frame-functions 'p/set-face-font-inconsolata)

;; If emacs is started as emacs -nw rather than emacs --daemon
(unless window-system
  (message "Disable theme on terminal...")
  (disable-theme 'zenburn)
  (message "Disable global-hl-line mode...")
  (global-hl-line-mode -1)
  (message "Disable menu-bar...")
  (menu-bar-mode -1)
  (message "Set line number format in terminals...")
  (setq linum-format "%5d "))

;; Better colors
;; (autoload 'color-theme-approximate-on "color-theme-approximate")
;; (color-theme-approximate-on)

;;; Commentary:

(provide 'console-setup)
;;; console-setup.el ends here
