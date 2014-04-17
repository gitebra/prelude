;;; resize-help-window --- automatic resizing of help windows.
;; Time-stamp: <1998-11-03 21:52:19 ecl>

;; Copyright (C) 1998 Emilio C. Lopes.

;; Author: Emilio Lopes <Emilio.Lopes@Physik.TU-Muenchen.DE> and
;;         Dave Lopes <fx@gnu.org>
;; Maintainer: Emilio Lopes <Emilio.Lopes@Physik.TU-Muenchen.DE>
;; Created: 09 Oct 1998
;; Version: 1.1
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an eletronic
;; mail to this program's maintainer or by writting to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; General
;; =======
;;
;; This library provides automatic vertical resizing of windows displaying
;; help buffers subject to a maximal user defined height.
;;
;; To toggle the resizing of help windows use `M-x help-window-resize-mode'.
;;
;; Please send bug reports, suggestions, improvements, etc. to the
;; author of this library (see e-mail address above), so that they can
;; be incorporated in future versions.
;;
;;
;; Installation
;; ============
;;
;; Put this file ("resize-help-window.el") in a directory listed in your
;; `load-path' and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;      (require 'resize-help-window)
;;      (help-window-resize-mode 1)

;;; Code:

(defcustom help-window-resize-mode nil
  "*Non-nil means resize the help window to fit its contents.
This also applies to `apropos' and `completion' windows."
  :type 'boolean
  :group 'help)

(defcustom help-window-max-height (lambda (bn) (/ (- (frame-height) 2) 2))
  "*Maximum height of a window displaying a help buffer.
This is the maximum window height (in text lines) which `show-help-buffer'
will give to a help buffer, if `help-window-resize-mode' is non-nil.
It can also be a function which will be called with the name of the buffer
to be displayed as argument and should return an integer number.

This variable also applies to `apropos' and `completion' windows."
  :type '(choice integer function)
  :group 'help)

(defun help-window-resize-mode (arg)
  "Toggle `help-window-resize-mode'.
With prefix argument ARG, turn the resizing of help windows on iff ARG
is positive."
  (interactive "P")
  (setq help-window-resize-mode
        (if (null arg)
            (not help-window-resize-mode)
          (> (prefix-numeric-value arg) 0))))

(defun show-help-buffer (buffer)
  "Display BUFFER in a window of appropriate size.
If `help-window-resize-mode' is non-nil, shrink the window displaying BUFFER
to be as small as possible to display its contents.
Will not give a window of size greater than `help-window-max-height'."
  (let* ((current-window (selected-window))
         (window (display-buffer buffer))
         (window-height (1- (window-height window)))
         (max-height (if (functionp help-window-max-height)
                         (funcall help-window-max-height (buffer-name buffer))
                       help-window-max-height)))
    ;; In Help Mode, this may add text (a "[back]" button) via
    ;; `help-mode-maybe', so run it now.
    (with-current-buffer buffer
      (run-hooks 'temp-buffer-show-hook))
    (setq minibuffer-scroll-window window)
    (select-window window)
    ;; set height to max-height unless this is the only window
    ;; in this frame...
    (unless (or  (not help-window-resize-mode) (one-window-p 'nomini))
      (enlarge-window (- max-height window-height))
      ;; ... and then shrink it if necessary:
      (shrink-window-if-larger-than-buffer))
    (select-window current-window)))

(setq temp-buffer-show-function 'show-help-buffer)

(provide 'resize-help-window)

;;; resize-help-window.el ends here
