;;; calendar-setup.el --- Calendar customizations
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar customization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'calendar)
(message "Calendar customizations...")

;; Weeks start on Monday
(setq calendar-week-start-day 1)

;; Don't resize the calendar window
(add-hook 'initial-calendar-window-hook
          (lambda ()
            (setq window-size-fixed t)))

;; Play with the calendar mode line
(require 'cal-iso)
(eval-after-load 'calendar
  (add-hook
   'calendar-load-hook
   (lambda ()
     (message "Setting my calendar mode line")
     (setq calendar-mode-line-format
           (append
            (butlast calendar-mode-line-format)
            (append (list
                     ""
                     '(let* ((d (calendar-absolute-from-gregorian date))
                             (iso-date (calendar-iso-from-absolute d)))
                        (format "Week %d"
                                (extract-calendar-month iso-date)))
                     '(let* ((year (extract-calendar-year date))
                             (d (calendar-day-number date))
                             (days-remaining
                              (- (calendar-day-number (list 12 31 year)) d)))
                        (format "%d/%d" d days-remaining))
                     "")
                    (last calendar-mode-line-format)))))))

;; My location, enables calendar to display sunrise and sunset
(defun set-location-boston ()
  "Set current location to Boston, MA."
  (interactive)
  (setq calendar-latitude 42.4
        calendar-longitude -71.1
        calendar-location-name "Boston, MA"))

(defun set-location-nashua ()
  "Set current location to Nashua, NH."
  (interactive)
  (setq calendar-latitude 42.8
        calendar-longitude -71.5
        calendar-location-name "Nashua, NH"))

(defun set-location-stockholm ()
  "Set current location to Stockholm, Sweden."
  (interactive)
  (setq calendar-latitude 59.2
        calendar-longitude 18.0
        calendar-location-name "Stockholm, Sweden"))

;; At the moment I'm in Nashua
(add-hook 'calendar-load-hook 'set-location-nashua)


(provide 'calendar-setup)
;;; calendar-setup.el ends here
