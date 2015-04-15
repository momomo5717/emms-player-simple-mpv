;;; emms-player-simple-mpv-control-functions.el --- functions to control mpv via emms-player-simple-mpv.el -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; URL: https://github.com/momomo5717/emms-player-simple-mpv

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides functions to control mpv via emms-player-simple-mpv.el.

;;; Code:
(require 'emms-player-simple-mpv)

;;;###autoload
(defun emms-player-simple-mpv-seek-to-% (per)
  "Seek to PER(percent position)."
  (interactive "nmpv seek to (%%) : ")
  (setq per (cond ((< per 0) 0) ((> per 100) 100) (t per)))
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "length")
   per
   (lambda (per ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
                (pos  (floor (* per data) 100))
                (h (floor pos 3600))
                (m (floor (- pos (* 3600 h)) 60))
                (s (- pos (* 60 (+ (* 60 h) m)))))
           (emms-player-simple-mpv-tq-enqueue
            (list "seek" per "absolute-percent")
            (format "mpv seek to %s(%%%%) : %02d:%02d:%02d" per h m s)
            (lambda (form ans-ls)
              (if (emms-player-simple-mpv-tq-success-p ans-ls)
                  (message form)
                (message "mpv seek to (%%) : error")))))
       (message "mpv seek to (%%) : error")))))

;;;###autoload
(defun emms-player-simple-mpv-volume-to (v)
  "Set volume to V."
  (interactive "nmpv volume to : ")
  (emms-player-simple-mpv-tq-enqueue
   (list "set_property" "volume" v)
   v
   (lambda (v ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (message "mpv volume : %s" v)
       (message "mpv volume : error")))))

;;;###autoload
(defun emms-player-simple-mpv-mute-on ()
  "Mute on."
  (emms-player-simple-mpv-tq-enqueue
   '("set_property_string" "mute" "yes")
   nil
   (emms-player-simple-mpv-tq-error-message "mpv mute on : %s")))

;;;###autoload
(defun emms-player-simple-mpv-mute-off ()
  "Mute off."
  (emms-player-simple-mpv-tq-enqueue
   '("set_property_string" "mute" "no")
   nil
   (emms-player-simple-mpv-tq-error-message "mpv mute off : %s")))

;;;###autoload
(defun emms-player-simple-mpv-mute ()
  "Cycle mut."
  (interactive)
  (emms-player-simple-mpv-tq-clear)
  (emms-player-simple-mpv-tq-enqueue
   '("cycle" "mute")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          '("get_property_string" "mute")
          nil
          (emms-player-simple-mpv-tq-data-message "mpv mute : %s"))
       (message "mpv mute : error")))))

;;;###autoload
(defun emms-player-simple-mpv-time-pos ()
  "Display position in current file."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "time-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
                (h (floor data 3600))
                (m (floor (- data (* 3600 h)) 60))
                (s (floor (- data (* 60 (+ (* 60 h) m))))))
           (message "mpv time position : %02d:%02d:%02d" h m s))
       (message "mpv time position : error")))))

;;;###autoload
(defun emms-player-simple-mpv-time-pos-% ()
  "Display position (0-100) in current file."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "percent-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (message "mpv time position (%%) : %.2f"
                  (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
       (message "mpv time position (%%) : error")))))

(defmacro emms-player-simple-mpv--playlist-change-1 (str)
  "Helper macro for `emms-player-simple-mpv--playlist-change'."
  (let ((n (if (string= str "next") 1  -1)))
    `(progn
       (emms-player-simple-mpv-tq-clear)
       (emms-player-simple-mpv-tq-enqueue
        '("get_property" "playlist-pos")
        nil
        (lambda (_ ans-ls)
          (if (emms-player-simple-mpv-tq-success-p ans-ls)
              (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
                     (form (format "mpv playlist_%s position %s : %%s"
                                   ,str (+ data ,n))))
                (emms-player-simple-mpv-tq-enqueue
                 '(,(format "playlist_%s" str))
                 form
                 (lambda (form ans-ls)
                   (if (emms-player-simple-mpv-tq-success-p ans-ls)
                       (message form "success")
                     (message form "error")))))
            (message ,(format "mpv playlist_%s : error" str))))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-next ()
  "Go to the next entry on the playlist."
  (interactive)
  (emms-player-simple-mpv--playlist-change-1 "next"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-prev ()
  "Go to the previous entry on the playlist."
  (interactive)
  (emms-player-simple-mpv--playlist-change-1 "prev"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-pos ()
  "Display current position on the playlist."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "playlist-count")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          '("get_property" "playlist-pos")
          (format "mpv playlist position : %%s  (total %s)"
                  (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
          (lambda (form ans-ls)
            (if (emms-player-simple-mpv-tq-success-p ans-ls)
                (message form (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
              (message "mpv playlist position : error"))))
       (message "mpv playlist position : error")))))

(defun emms-player-simple-mpv--speed-change-1 (v ans-ls)
  "Helper function for `emms-player-simple-mpv-speed-change'."
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((data (/ (round (* 100 (+ (emms-player-simple-mpv-tq-assq-v 'data ans-ls) v)))
                      100.0))
             (speed (cond
                     ((< data 0.01) 0.01)
                     ((> data 100) 100)
                     (t data))))
        (emms-player-simple-mpv-tq-enqueue
         (list "set_property" "speed" speed)
         speed
         (lambda (speed ans-ls)
           (if (emms-player-simple-mpv-tq-success-p ans-ls)
               (message "mpv speed : %s" speed)
             (message "mpv speed : error")))))
    (message "mpv speed : error")))

;;;###autoload
(defun emms-player-simple-mpv-speed-change (v)
  "Change speed by V."
  (emms-player-simple-mpv-tq-clear)
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" "speed")
   v 'emms-player-simple-mpv--speed-change-1))

;;;###autoload
(defun emms-player-simple-mpv-speed (v)
  "Change speed by V."  
  (interactive "nmpv speed : ")
  (emms-player-simple-mpv-speed-change v))

;;;###autoload
(defun emms-player-simple-mpv-speed-up ()
  "Speed up by `emms-player-simple-mpv-speed-change-amount'."
  (interactive)
  (emms-player-simple-mpv-speed-change
   emms-player-simple-mpv-speed-change-amount))

;;;###autoload
(defun emms-player-simple-mpv-speed-down ()
  "Speed down by `emms-player-simple-mpv-speed-change-amount'."
  (interactive)
  (emms-player-simple-mpv-speed-change
   (- emms-player-simple-mpv-speed-change-amount)))

;;;###autoload
(defun emms-player-simple-mpv-speed-to (v)
  "Set speed to V."
  (interactive "nmpv speed to : ")
  (emms-player-simple-mpv-tq-enqueue
   (list "set_property" "speed" v)
   v
   (lambda (v ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (message "mpv speed : %s" v)
       (message "mpv speed : error")))))

;;;###autoload
(defun emms-player-simple-mpv-ontop ()
  "Cycle ontop."
  (interactive)
  (emms-player-simple-mpv-tq-clear)
  (emms-player-simple-mpv-tq-enqueue
   '("cycle" "ontop")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-tq-enqueue
          '("get_property_string" "ontop")
          nil
          (emms-player-simple-mpv-tq-data-message "mpv ontop : %s"))
       (message "mpv ontop : error")))))

(provide 'emms-player-simple-mpv-control-functions)
;;; emms-player-simple-mpv-control-functions.el ends here
