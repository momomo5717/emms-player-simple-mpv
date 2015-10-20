;;; emms-player-simple-mpv-e.g.time-display.el --- A setting example of TQ event hooks -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; URL: https://github.com/momomo5717/

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

;; A setting example for `emms-playing-time-display'.
;;
;; (require 'emms-player-simple-mpv-e.g.time-display)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-mode-line)
(require 'emms-playing-time)

(defvar emms-mode-line-cycle) ; Suppres a warning message.

(defun emms-player-simple-mpv-reset-playing-time-display-timer (&optional speed)
  "Reset `emms-playing-time'.
SPEED is a mpv property of speed."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "time-pos")
   (float-time)
   (lambda (sent-time ans-ls)
     (let ((time (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
       (when (and (emms-player-simple-mpv-playing-p) (numberp time))
         (when emms-playing-time-display-timer
           (emms-cancel-timer emms-playing-time-display-timer)
           (setq emms-playing-time-display-timer nil))
         (unless emms-player-paused-p
           (setq time (+ time (- (float-time) sent-time))))
         (setq emms-playing-time (1- (floor time)))
         (let (emms-mode-line-cycle)
           (emms-playing-time-display))
         (force-mode-line-update t)
         (unless emms-player-paused-p
           (setq speed (or (and (numberp speed)
                                (<= 0.01 speed) (<= speed 100) speed)
                           (and (emms-player-simple-mpv-last-speed-available-p)
                                emms-player-simple-mpv-last-speed)))
           (setq emms-playing-time-display-timer
                 (if speed
                     (run-at-time (/ (- 1.0 (- time (ffloor time))) speed)
                                  (/ 1.0 speed)
                                  'emms-playing-time-display)
                   (run-at-time (- 1.0 (- time  (ffloor time)))
                                1.0
                                'emms-playing-time-display)))))))))

(add-hook 'emms-player-simple-mpv-tq-event-unpause-hook
          'emms-player-simple-mpv-reset-playing-time-display-timer)

(add-hook 'emms-player-simple-mpv-tq-event-playback-restart-hook
          'emms-player-simple-mpv-reset-playing-time-display-timer)

(add-hook 'emms-player-simple-mpv-tq-event-speed-functions
          'emms-player-simple-mpv-reset-playing-time-display-timer)

(provide 'emms-player-simple-mpv-e.g.time-display)
;;; emms-player-simple-mpv-e.g.time-display.el ends here
