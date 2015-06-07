;;; emms-player-simple-mpv.el --- An extension of emms-player-simple.el for mpv JSON IPC  -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms, mpv
;; Version: 0.1.6
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (emms "4.0"))
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

;; This is an extension of emms-player-simple.el for mpv JSON IPC.
;; It provides macro and functions to define a player of mpv.

;; Setup:
;;
;; (require 'emms-player-simple-mpv)
;;
;; ;; An example of setting
;; ;; `emms-player-mpv' is defined by `define-emms-simple-player-mpv'.
;; (define-emms-simple-player-mpv mpv '(file url streamlist playlist)
;;     (concat "\\`\\(http[s]?\\|mms\\)://\\|"
;;             (apply #'emms-player-simple-regexp
;;                    "pls"
;;                    emms-player-base-format-list))
;;     "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")
;;
;; (emms-player-simple-mpv-add-to-converters
;;  'emms-player-mpv "." '(playlist)
;;  (lambda (track-name) (format "--playlist=%s" track-name)))
;;
;; (add-to-list 'emms-player-list 'emms-player-mpv)
;;

;;
;; ;; If you use hydra.el ( https://github.com/abo-abo/hydra ),
;; ;; the following example could be useful for playing sound.
;; ;; This example emulates default key bindings of mpv player.
;; (require 'hydra)
;; (require 'emms-player-simple-mpv-control-functions)
;;
;; ;; (global-set-key (kbd "<f2> m") 'emms-player-simple-mpv-hydra/body)
;; (defhydra emms-player-simple-mpv-hydra
;;   (:foreign-keys warn :hint nil)
;;   "
;;   Keyboard Control for emms simple player of mpv
;; -------------------------------------------------------------
;;   _Q_        Quit emms-player-simple-mpv-hydra.
;;   ─────────────────────────────
;;   <left> and <right>
;;            Seek backward/forward 5 seconds.
;;   S-<left> and S-<right>
;;            Seek backward/forward 1 seconds.
;;   <down> and <up>
;;            Seek backward/forward 1 minute.
;;   S-<down> and S-<upt>
;;            Seek backward/forward 5 seconds.
;;   ─────────────────────────────
;;   \[ and \]  Decrease/increase current playback speed by 10 %%%%.
;;   \{ and \}  Halve/double current playback speed.
;;   <backspace>
;;            Reset playback speed to normal.
;;   ─────────────────────────────
;;   < and >  Go backward/forward in the playlist.
;;   <retrun> Go forward in the playlist.
;;   ─────────────────────────────
;;   p / SPC  Pause (pressing again unpauses).
;;   ─────────────────────────────
;;   q        Stop playing and quit.
;;   ─────────────────────────────
;;   / and *  Decrease/increase volume.
;;   9 and 0  Decrease/increase volume.
;;   ─────────────────────────────
;;   m        Mute sound.
;;   ─────────────────────────────
;;   f        Toggle fullscreen.
;;   ─────────────────────────────
;;   T        Toggle stay-on-top.
;; -------------------------------------------------------------
;;
;; "
;;   ("Q" nil)
;;   ("<left>"    (lambda () (interactive) (emms-seek -5)))
;;   ("S-<left>"  (lambda () (interactive) (emms-seek -1)))
;;   ("<down>"    (lambda () (interactive) (emms-seek -60)))
;;   ("S-<down>"  (lambda () (interactive) (emms-seek -5)))
;;   ("<right>"   (lambda () (interactive) (emms-seek 5)))
;;   ("S-<right>" (lambda () (interactive) (emms-seek 1)))
;;   ("<up>"      (lambda () (interactive) (emms-seek 60)))
;;   ("S-<up>"    (lambda () (interactive) (emms-seek 5)))
;;   ("["  emms-player-simple-mpv-speed-decrease)
;;   ("]"  emms-player-simple-mpv-speed-increase)
;;   ("{"  emms-player-simple-mpv-speed-halve)
;;   ("}"  emms-player-simple-mpv-speed-double)
;;   ("<backspace>" emms-player-simple-mpv-speed-normal)
;;   ("<" emms-player-simple-mpv-playlist-prev)
;;   (">" emms-player-simple-mpv-playlist-next)
;;   ("<return>" emms-player-simple-mpv-playlist-next)
;;   ("p" emms-pause)
;;   ("SPC" emms-pause)
;;   ("q" (lambda () (interactive)
;;          (when (y-or-n-p "Run emms-stop")
;;            (emms-stop))) :exit t)
;;   ("/" emms-volume-lower)
;;   ("*" emms-volume-raise)
;;   ("9" emms-volume-lower)
;;   ("0" emms-volume-raise)
;;   ("m" emms-player-simple-mpv-mute)
;;   ("f" emms-player-simple-mpv-fullscreen)
;;   ("T" emms-player-simple-mpv-ontop))
;;

;;; Code:

(require 'emms-player-simple)
(require 'emms-volume)
(require 'cl-lib)
(require 'json)
(require 'tq)
(require 'later-do)

(defconst emms-player-simple-mpv-version "0.1.6")

(defgroup emms-simple-player-mpv nil
  "An extension of emms-simple-player.el."
  :group 'emms-player
  :prefix "emms-simple-player-mpv-")

(defcustom emms-player-simple-mpv-use-volume-change-function-p t
  "If non-nil, `emms-player-simple-mpv-volume-change' is used as `emms-volume-change-function'."
  :group 'emms-simple-player-mpv
  :type 'boolean)

(defcustom emms-player-simple-mpv-use-start-tq-error-message-p t
  "If non-nil, display error message when failed to start tq process."
  :group 'emms-simple-player-mpv
  :type 'boolean)

(defvar emms-player-simple-mpv-default-volume-function emms-volume-change-function
  "Set emms-volume-change-function for buckup.")

;;;###autoload
(defmacro define-emms-simple-player-mpv (name types regex command &rest args)
  "Extension of `define-emms-simple-player' for mpv JSON IPC."
  (let ((group         (intern (format "emms-player-%s"              name)))
        (command-name  (intern (format "emms-player-%s-command-name" name)))
        (parameters    (intern (format "emms-player-%s-parameters"   name)))
        (player-name   (intern (format "emms-player-%s"              name)))
        (start         (intern (format "emms-player-%s-start"        name)))
        (stop          (intern (format "emms-player-%s-stop"         name)))
        (playablep     (intern (format "emms-player-%s-playable-p"   name))))
  `(progn
     (defgroup ,group nil
       ,(format "EMMS player for %s." command)
       :group 'emms-player
       :prefix ,(format "emms-player-%s-" name))
     (defcustom ,command-name ,command
       ,(format "*The command name of %s." command)
       :type  'string
       :group ',group)
     (defcustom ,parameters ',args
       ,(format "*The arguments to `%s'." command-name)
       :type  '(repeat string)
       :group ',group)
     (defcustom ,player-name (emms-player ',start ',stop ',playablep)
       "*A player for EMMS."
       :type '(cons symbol alist)
       :group ',group)
     (emms-player-set ,player-name 'regex   ,regex)
     (emms-player-set ,player-name 'pause   'emms-player-simple-mpv-pause)
     (emms-player-set ,player-name 'resume  'emms-player-simple-mpv-unpause)
     (emms-player-set ,player-name 'seek    'emms-player-simple-mpv-seek)
     (emms-player-set ,player-name 'seek-to 'emms-player-simple-mpv-seek-to)
     (emms-player-set ,player-name 'get-media-title
                      (lambda (track) (file-name-nondirectory (emms-track-name track))))
     (emms-player-set ,player-name 'mpv-track-name-converters '())
     (emms-player-set ,player-name 'mpv-start-process-function
                      'emms-player-simple-mpv-default-start-process)
     (defun ,start (track)
       "Start the player process."
       (emms-player-simple-mpv-start track
                                     ,player-name
                                     ,command-name
                                     ,parameters))
     (defun ,stop ()
       "Stop the player process."
       (emms-player-simple-stop))
     (defun ,playablep (track)
       "Return non-nil when we can play this track."
       (and (executable-find ,command-name)
            (memq (emms-track-type track) ,types)
            (string-match (emms-player-get ,player-name 'regex)
                          (emms-track-name track)))))))

;; Utility for tq

(defvar emms-player-simple-mpv--tq nil
  "TQ process.")

(defvar emms-player-simple-mpv-tq-process-name
  "emms-player-simple-mpv-tq-process")

(defvar emms-player-simple-mpv--tq-event-buffer-name
  " *emms-player-simple-mpv--tq-event*")

(defvar emms-player-simple-mpv--socket
  (make-temp-name
   (expand-file-name "mpv--socket" temporary-file-directory)))

(defun emms-player-simple-mpv--socket ()
  (setq emms-player-simple-mpv--socket
        (make-temp-name
         (expand-file-name "mpv--socket" temporary-file-directory))))

(defun emms-player-simple-mpv--tq-create ()
  (tq-create (make-network-process
              :name emms-player-simple-mpv-tq-process-name
              :family 'local
              :service emms-player-simple-mpv--socket)))

(defun emms-player-simple-mpv--tq-close ()
  (when emms-player-simple-mpv--tq
    (tq-close emms-player-simple-mpv--tq)
    (setq emms-player-simple-mpv--tq nil))
  (when (file-exists-p emms-player-simple-mpv--socket)
    (ignore-errors (delete-file emms-player-simple-mpv--socket)))
  (when (buffer-live-p (get-buffer emms-player-simple-mpv--tq-event-buffer-name))
    (kill-buffer emms-player-simple-mpv--tq-event-buffer-name)))

(add-hook 'emms-player-stopped-hook  'emms-player-simple-mpv--tq-close)
(add-hook 'emms-player-finished-hook 'emms-player-simple-mpv--tq-close)

(defun emms-player-simple-mpv--socket-filter (_proc string)
  (emms-player-simple-mpv--tq-filter emms-player-simple-mpv--tq string))

(defun emms-player-simple-mpv--tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data. See tq.el."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (emms-player-simple-mpv--tq-process-buffer tq)))))

(defun emms-player-simple-mpv--tq-process-buffer (tq)
  "Check TQ's buffer at the head of the queue.
See tq.el."
  (let ((buffer (tq-buffer tq))
        (buffer-tq-event
         (get-buffer-create emms-player-simple-mpv--tq-event-buffer-name)))
    (while (and (buffer-live-p buffer) (set-buffer buffer)
                (> (buffer-size) 0))
      (if (tq-queue-empty tq)
          (let ((buf buffer-tq-event))
            (copy-to-buffer buf (point-min) (point-max))
            (delete-region (point-min) (point))
            (ignore-errors (emms-player-simple-mpv--tq-event-action)))
        (goto-char (point-min))
        (let ((answer-ls (cl-loop with ls
                                  for obj = (ignore-errors (json-read))
                                  when (null obj) return (nreverse ls)
                                  do (push obj ls)))
              (fn (tq-queue-head-fn tq))
              (closure (tq-queue-head-closure tq)))
          (delete-region (point-min) (point-max))
          (tq-queue-pop tq)
          (ignore-errors (funcall fn closure answer-ls)))))))

(defun emms-player-simple-mpv--tq-event-action ()
  "Action for event response from mpv."
  (let ((buf (get-buffer emms-player-simple-mpv--tq-event-buffer-name))
        ans-ls)
    (when (buffer-live-p buf)
      (setq ans-ls
            (with-current-buffer buf
              (goto-char (point-min))
              (cl-loop with ls
                       for obj = (ignore-errors (json-read))
                       when (null obj) return (nreverse ls)
                       do (push obj ls))))
      (with-current-buffer buf (erase-buffer))
      (cl-loop for ans in ans-ls
               for event = (cdr (assq 'event ans))
               when event do
               (cond
                ((equal event "pause")
                 (setq emms-player-paused-p t)
                 (run-hooks 'emms-player-paused-hook))
                ((equal event "unpause")
                 (setq emms-player-paused-p nil)
                 (run-hooks 'emms-player-paused-hook))
                ((equal event "playback-restart")
                 (emms-player-simple-mpv-tq-enqueue
                  '("get_property" "pause")
                  nil
                  (lambda (_ ans-ls)
                    (when (emms-player-simple-mpv-tq-success-p ans-ls)
                      (let ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
                        (when data
                          (if (eq data t)
                              (setq emms-player-paused-p t)
                            (setq emms-player-paused-p nil))
                          (run-hooks 'emms-player-paused-hook))))))))))))

(defun emms-player-simple-mpv-playing-p ()
  "Return t when `emms-player-simple-mpv--tq' process is open."
  (let ((process (tq-process emms-player-simple-mpv--tq)))
    (when process
     (eq (process-status process) 'open))))

;;;###autoload
(defun emms-player-simple-mpv-tq-clear ()
  "Clear old messages if it remains in tq."
  (let ((tq emms-player-simple-mpv--tq))
    (while (not (tq-queue-empty tq))
      (tq-queue-pop tq))))

(defun emms-player-simple-mpv--tq-make-command (com &rest params)
  "Build JSON command from COM and PARAMS."
  (concat (json-encode `(("command" . (,com ,@params)))) "\n"))

;;;###autoload
(defun emms-player-simple-mpv-tq-enqueue
    (com-ls closure fn &optional delay-question)
  "Wrapper function of `tq-enqueue'."
  (when (emms-player-simple-mpv-playing-p)
    (tq-enqueue emms-player-simple-mpv--tq
                (apply #'emms-player-simple-mpv--tq-make-command com-ls)
                "" closure fn delay-question)))

(defun emms-player-simple-mpv-tq-success-p (ans)
  "Check command response from ANS."
  (let ((err-msg
         (if (atom (caar ans))
             (cdr (assq 'error ans)) ;; For decoded JSON obj
           (cl-loop for obj in ans   ;; For decoded JSON obj list
                    for msg = (cdr (assq 'error obj))
                    when msg return msg))))
    (and (stringp err-msg)
         (string= err-msg "success"))))

(defun emms-player-simple-mpv-tq-assq (key ans)
  "Return the association for KEY in ANS."
  (if (atom (caar ans))
      (assq key ans)        ;; For decoded JSON obj
    (cl-loop for obj in ans ;; For decoded JSON obj list
             for assoc = (assq key obj)
             when assoc return assoc)))

(defun emms-player-simple-mpv-tq-assq-v (key ans)
  "Return a value of the association for KEY in ANS."
  (cdr (emms-player-simple-mpv-tq-assq key ans)))

;;;###autoload
(cl-defun emms-player-simple-mpv-tq-data-message
    (form &key (fn #'identity) (err-form form))
  "Return function to display a data message by FORM.
FORM can include a format specification for data.
:FN takes data as an argument.
:ERR-FORM can include a format specification %s."
  (lambda (_ ans-ls)
    (if (emms-player-simple-mpv-tq-success-p ans-ls)
        (let ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
          (if data (message form (funcall fn data))
            (message "mpv : nothing data message")))
      (message err-form (emms-player-simple-mpv-tq-assq-v 'error ans-ls)))))

;;;###autoload
(defun emms-player-simple-mpv-tq-error-message (form)
  "Return function to display an error message by FORM.
FORM can include a format specification %s."
  (lambda (_ ans-ls)
    (let ((err (emms-player-simple-mpv-tq-assq-v 'error ans-ls)))
      (if err (message form err)
        (message "mpv : nothing error message")))))

;; Functions to start mpv

;;;###autoload
(defun emms-player-simple-mpv-add-to-converters (player regexp types fn &optional appendp)
  "Add a converter to PLAYER's mpv-track-name-converters like `add-to-list'.
Converter is  \(list REGEXP TYPES FN\).
If APPENDP is no-nil,add converter to last.
TYPES is type list or t.
FN takes track-name as an argument."
  (let ((converters (emms-player-get player 'mpv-track-name-converters))
        (converter (list regexp types fn)))
    (if (cl-find regexp converters :key #'car :test #'equal)
        converters
      (emms-player-set player 'mpv-track-name-converters
                       (if appendp
                           (nconc converters (list converter))
                         (cons converter converters))))))

;;;###autoload
(defun emms-player-simple-mpv-remove-converter (player regexp)
  "Remove the converter from PLAYER's mpv-track-name-converters which has REGEXP."
  (let ((converters (emms-player-get player 'mpv-track-name-converters)))
    (emms-player-set player 'mpv-track-name-converters
                     (cl-delete regexp converters :key #'car :test #'equal))))

(defun emms-player-simple-mpv--track-to-input-form (track track-name-converters)
  "Convert TRACK to mpv input form by TRACK-NAME-CONVERTERS."
  (let* ((track-name (emms-track-name track))
         (track-type (emms-track-type track))
         (converter
          (cl-loop for (regexp types fn) in track-name-converters
                   when (and (string-match-p regexp track-name)
                             (or (eq types t) (memq track-type types)))
                   return fn)))
    (if converter (funcall converter track-name) track-name)))

(defun emms-player-simple-mpv--start-tq-error-message (params input-form)
  "Error message when tq-process fails to start."
  (message "Failed to start mpv--tq. Check parameters or input form.\n%s%s\n%s%s"
           "    " (mapconcat #'identity  params " ") "    " input-form))

(defun emms-player-simple-mpv-default-start-process
    (cmdname params input-form _track)
  "Default function for mpv-start-process-function."
  (apply  #'start-process
          emms-player-simple-process-name
          nil
          cmdname
          `(,@params ,input-form)))

;;;###autoload
(defun emms-player-simple-mpv-start (track player cmdname params)
  "Emulate `emms-player-simple-start' but the first arg."
  (emms-player-simple-mpv--tq-close)
  (let* ((input-socket
          (format "--input-unix-socket=%s" (emms-player-simple-mpv--socket)))
         (input-form
          (emms-player-simple-mpv--track-to-input-form
           track (emms-player-get player 'mpv-track-name-converters)))
         (get-media-title (emms-player-get player 'get-media-title))
         (media-title
          (if get-media-title
              (format "--media-title=%s"
                      (funcall get-media-title track))
            ""))
         (process
          (funcall (emms-player-get player 'mpv-start-process-function)
                   cmdname `(,input-socket ,media-title ,@params)
                   input-form track)))
    (set-process-sentinel process 'emms-player-simple-sentinel)
    (emms-player-started player)
    (setq emms-player-paused-p t)
    (run-hooks 'emms-player-paused-hook)
    (while (and (eq (process-status process) 'run)
                (not (file-exists-p emms-player-simple-mpv--socket)))
      (sit-for 0.05))
    (condition-case err
        (setq emms-player-simple-mpv--tq (emms-player-simple-mpv--tq-create))
      (error (message "%s" (error-message-string err))
             (when emms-player-simple-mpv-use-start-tq-error-message-p
               (later-do 'emms-player-simple-mpv--start-tq-error-message
                         params input-form))))
    (when (tq-process emms-player-simple-mpv--tq)
      (set-process-filter (tq-process emms-player-simple-mpv--tq)
                          'emms-player-simple-mpv--socket-filter)
      (when emms-player-simple-mpv-use-volume-change-function-p
        (emms-player-simple-mpv--set-volume-change-function)))))

;; Functions to control mpv

(defmacro emms-player-simple-mpv--set_property-1 (command)
  "Helper macro emms-player-simple-mpv-set_property\(_string\)."
  `(emms-player-simple-mpv-tq-enqueue
    (list ,command property value)
    nil
    (lambda (_ ans-ls)
      (if (emms-player-simple-mpv-tq-success-p ans-ls)
          (message (format "mpv %s : %s" msg spec) (funcall fn value))
        (message "mpv %s : error" err-msg)))))

;;;###autoload
(cl-defun emms-player-simple-mpv-set_property
    (property value &key (spec "%s") (msg property) (err-msg property) (fn #'identity))
  "Set PROPERTY to VALUE.
:SPEC is a format specification for VALUE.
:MSG is displayed when command succeeds.
:ERR-MSG is displayed when command fails.
:FN takes VALUE as an argument."
  (emms-player-simple-mpv--set_property-1 "set_property"))

;;;###autoload
(cl-defun emms-player-simple-mpv-set_property_string
    (property value &key (spec "%s") (msg property) (err-msg property) (fn #'identity))
  "Set PROPERTY to VALUE.
:SPEC is a format specification for VALUE.
:MSG is displayed when command succeeds.
:ERR-MSG is displayed when command fails.
:FN takes VALUE as an argument."
  (emms-player-simple-mpv--set_property-1 "set_property_string"))

(defsubst emms-player-simple-mpv--time-string (sec)
  "SEC to \"%02h:%02m:%02s\"."
  (let* ((h (floor sec 3600))
         (m (floor (- sec (* 3600 h)) 60))
         (s (- sec (* 60 (+ (* 60 h) m)))))
    (format "%02d:%02d:%02d" h m s)))

;; pause

;;;###autoload
(defun emms-player-simple-mpv-pause ()
  "Pause."
  (emms-player-simple-mpv-set_property_string
   "pause" "yes" :spec "success"))

;;;###autoload
(defun emms-player-simple-mpv-unpause ()
  "Unpause."
  (emms-player-simple-mpv-set_property_string
   "pause" "no" :spec "success" :msg "unpause" :err-msg "unpause"))

;; seek

(defun emms-player-simple-mpv--seek-1 (als ans-ls)
  "Helper funcion for `emms-player-simple-mpv-seek'."
  ;; als  : ((sec . n0) (len . n1))
  ;; data : time-pos
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((sec (cdr (assq 'sec als)))
             (len (cdr (assq 'len als)))
             (data  (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
             (data+ (+ sec data))
             (next-sec (cond
                        ((< data+ 0) 0)
                        ((> data+ len) len)
                        (t data+)))
             (time (emms-player-simple-mpv--time-string next-sec)))
        (emms-player-simple-mpv-tq-enqueue
         (list "seek" next-sec "absolute")
         (format "mpv seek %s : %s" (if (>= sec 0) ">>" "<<") time)
         (lambda (form ans-ls)
           (if (emms-player-simple-mpv-tq-success-p ans-ls)
               (message form)
             (message "mpv seek : error")))))
    (message "mpv seek : error")))

(defun emms-player-simple-mpv--seek-2 (sec)
  "Helper funcion for `emms-player-simple-mpv-seek'.
For a track which does not have length property."
  (emms-player-simple-mpv-tq-enqueue
   (list "seek" sec "relative")
   nil
   (emms-player-simple-mpv-tq-error-message
    (format "mpv seek %s %+d : %%s" (if (>= sec 0) ">>" "<<") sec))))

;;;###autoload
(defun emms-player-simple-mpv-seek (sec)
  "Seek by SEC."
  (emms-player-simple-mpv-tq-clear)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "length")
   sec
   (lambda (sec ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
           (emms-player-simple-mpv-tq-enqueue
            '("get_property" "time-pos")
            `((sec . ,sec) (len . ,data))
            'emms-player-simple-mpv--seek-1))
       (emms-player-simple-mpv--seek-2 sec)))))

;;;###autoload
(defun emms-player-simple-mpv-seek-to (sec)
  "Seek to SEC."
  (interactive "nmpv seek to (sec) : ")
  (emms-player-simple-mpv-tq-enqueue
   (list "seek" sec "absolute")
   sec
   (lambda (sec ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (message "mpv seek to : %s" (emms-player-simple-mpv--time-string sec))
       (message "mpv seek to : error")))))

;; volume

(defun emms-player-simple-mpv--volume-change-1 (v ans-ls)
  "Set volume plus V in `emms-player-simple-mpv-volume-change'.
ANS-LS includes data value."
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
             (data+ (round (+ data v)))
             (vol (cond
                   ((< data+ 0) 0)
                   ((> data+ 100) 100)
                   (t data+))))
        (emms-player-simple-mpv-set_property "volume" vol))
    (message "mpv volume : error")))

;;;###autoload
(defun emms-player-simple-mpv-volume-change (v)
  "Change volume by V."
  (emms-player-simple-mpv-tq-clear)
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" "volume")
   v 'emms-player-simple-mpv--volume-change-1))

(defun emms-player-simple-mpv--set-default-volume-change-function ()
  "Set default volume change function to `emms-volume-change-function'."
  (let ((default-volume-function
          (get 'emms-player-simple-mpv-volume-change
               :default-volume-change-function)))
    (if  (null default-volume-function)
        (setq emms-volume-change-function emms-player-simple-mpv-default-volume-function)
      (setq emms-volume-change-function default-volume-function)
      (put 'emms-player-simple-mpv-volume-change
           :default-volume-change-function nil)))
  (remove-hook 'emms-player-stopped-hook
               'emms-player-simple-mpv--set-default-volume-change-function)
  (remove-hook 'emms-player-finished-hook
               'emms-player-simple-mpv--set-default-volume-change-function))

(defun emms-player-simple-mpv--set-volume-change-function ()
  "Set `emms-player-simple-mpv-volume-change' to `emms-volume-change-function'."
  (put 'emms-player-simple-mpv-volume-change
       :default-volume-change-function  emms-volume-change-function)
  (setq emms-volume-change-function 'emms-player-simple-mpv-volume-change)
  (add-hook 'emms-player-stopped-hook
            'emms-player-simple-mpv--set-default-volume-change-function)
  (add-hook 'emms-player-finished-hook
            'emms-player-simple-mpv--set-default-volume-change-function))

(provide 'emms-player-simple-mpv)
;;; emms-player-simple-mpv.el ends here
