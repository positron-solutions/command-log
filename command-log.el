;;; command-log.el --- Log user inputs & commands -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Positron Solutions
;; Copyright (C) 2013 Nic Ferrier
;; Copyright (C) 2012 Le Wang
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Michael Weber <xmichaelw@foldr.org>
;; Maintainer: Positron Solutions <contact@positron.solutions>
;; Keywords: help, docs
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.0"))
;; homepage: https://github.com/positron-solutons/command-log
;; Initial-version: <2004-10-07 11:41:28 michaelw>
;; Time-stamp: <2004-11-06 17:08:11 michaelw>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is an updated fork of `command-log-mode'.  Also see the
;; `keycast' and `keypression' packages.

;; This add-on can be used to demo Emacs to an audience.  When activated,
;; keystrokes get logged into a designated buffer, along with the command bound
;; to them.

;; To enable, use e.g.:
;;
;; (require 'command-log)
;;
;; To see the log buffer, call M-x `command-log-toggle'.

;; The key strokes in the log are decorated with ISO9601 timestamps on the
;; property `:time' so if you want to convert the log for screencasting purposes
;; you could use the time stamp as a key into the video beginning.

;;; Code:

(require 'autorevert)
(require 'cl-lib)
(require 'comint)
(require 'button)
(require 'simple)

(defconst command-log--mouse-event-regex
  (rx bol "mouse") "Used in `command-log--mouse-event-p'.")

(defgroup command-log nil
  "Customization for the command log."
  :prefix 'command-log
  :group 'convenience)

(defcustom command-log-window-size 40
  "The size of the command-log window."
  :group 'command-log
  :type 'integer)

(defcustom command-log-default-side 'right
  "Which side for use in `display-buffer-in-side-window'."
  :group 'command-log
  :type 'symbol
  :options '(right left top bottom))

(defcustom command-log-window-text-scale 0
  "The text scale of the command-log window.
+1,+2,... increase and -1,-2,... decrease the font size."
  :group 'command-log
  :type 'integer)

(defcustom command-log-log-command-indentation 11
  "Indentation of commands in command log buffer."
  :group 'command-log
  :type 'integer)
(defcustom command-log-text-format "\"%s\""
  "How to display text.
Only applies when `command-log-log-text' is non-nil."
  :group 'command-log
  :type 'string)

(defcustom command-log-text-space "␣"
  "How to draw spaces in text."
  :group 'command-log
  :type 'string)

(defface command-log-key-face
  '((t :inherit 'font-lock-keyword-face))
  "Face for keys in command log."
  :group 'command-log)

(defface command-log-command-face
  '((t :inherit font-lock-doc-markup-face))
  "Face for commands in command log."
  :group 'command-log)

(defface command-log-repeat-face
  '((t :inherit 'font-lock-doc-face))
  "Face for commands in command log."
  :group 'command-log)

(defface command-log-text-face
  '((t :inherit 'font-lock-string-face))
  "Face for text echo'ing in the command log."
  :group 'command-log)

(defcustom command-log-time-string "%Y-%m-%dT%H:%M:%S"
  "The string sent to `format-time-string' when command time is logged."
  :group 'command-log
  :type 'string)

(defcustom command-log-logging-shows-buffer t
  "Turning on logging shows the buffer if it's not visible."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-hiding-disables-logging t
  "Hiding the buffer deactivates logging modes."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-disabling-logging-kills-buffer t
  "Turning off all logging kills the buffer."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-log-globally t
  "Does turning on `command-log-mode' happen globally?"
  :group 'command-log
  :type 'boolean)

(defcustom command-log-filter-commands
  '(self-insert-command
    handle-switch-frame)
  "A list commands which should not be logged, despite logging being enabled.
Frequently used non-interesting commands (like cursor movements)
should be put here."
  :group 'command-log
  :type '(repeat (symbol :tag "command function name")))

(defcustom command-log-text nil
  "Log text as strings instead of `self-insert-commands'.
You may want to just except `self-insert-command' by adding it to
`command-log-filter-commands'."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-log-mouse nil
  "Log mouse events.
Toggling this is more conveneint than setting `command-log-ignored-commands'."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-merge-repeats t
  "Merge repetitions of the same command."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-logging-dir (locate-user-emacs-file
                                    "var/command-log-mode/")
  "Directory in which to store files containing logged commands."
  :group 'command-log
  :type 'directory)

(defcustom command-log-buffer-name "*command-log*"
  "Command log buffer name."
  :group 'command-log
  :type 'string)

(defvar command-log--command-repetitions 0
  "Count of how often the last keyboard commands has been repeated.")

(defvar command-log--last-keyboard-command nil
  "Last logged keyboard command.")

(defvar command-log--last-command-keys nil
  "Last key description for `this-command-keys'.")

(defvar command-log--recent-history-string ""
  "This string will hold recently typed text.")

(defvar command-log--show-all-commands nil
  "Override `command-log-filter-commands' and show all commands instead.")

(declare-function helpful-at-point "helpful" ())
(defun command-log--push-button ()
  "Open help for command at point.
Use `helpful' package if loaded."
  (interactive)
  (if (featurep 'helpful)
      (helpful-at-point)
    (describe-symbol (symbol-at-point))))

(defvar command-log-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap push-button] #'command-log--push-button)
    (define-key map "g" nil)
    ;; TODO check the definition of bury & quit
    (set-keymap-parent map (make-composed-keymap  button-buffer-map special-mode-map))
    map))

(define-derived-mode command-log-output-mode fundamental-mode
  'command-log-output-mode
  "Major mode for command log output buffers."
  :interactive nil)

;;;###autoload
(define-minor-mode command-log-mode
  "Toggle keyboard command logging."
  :init-value nil
  :lighter " command-log"
  :keymap nil
  (if command-log-mode
      (add-hook 'pre-command-hook
                #'command-log--log-command 'default-depth)
    (remove-hook 'pre-command-hook #'command-log--log-command)))

;;;###autoload
(define-globalized-minor-mode global-command-log-mode command-log-mode command-log-mode
  "Enables minor mode in all buffers, including minibuffer."
  :group 'command-log)

;;;###autoload
(defun command-log-toggle (&optional clear)
  "Display/hide the buffer and activate/deactivate command-log modes.

The following variables are used to configure this toggle:

`command-log-log-globally' controls the preference for
`command-log-mode` minor mode or `global-command-log-mode.
`command-log-open-log-turns-on-mode' will activate modes if
showing the log buffer.  `command-log-close-log-turns-off-mode'
will clean up modes if killing the log buffer.

Passing a prefix CLEAR will clear the buffer before display."
  (interactive "P")
  (if (command-log--buffer-visible-p)
      (progn
        (command-log--hide-buffer)
        (when command-log-hiding-disables-logging
          (if command-log-log-globally
              (global-command-log-mode nil)
            (command-log-mode nil))
          (when command-log-disabling-logging-kills-buffer
            (command-log--hide-buffer t))))
    (progn
      (if command-log-log-globally
          (global-command-log-mode t)
        (command-log-mode t))
      (when command-log-logging-shows-buffer
        (command-log--show-buffer clear)))))

;;;###autoload
(defun command-log-close-command-log-buffer (&optional kill)
  "Close the command log window.
Prefix argument will KILL buffer."
  (interactive "P")
  (command-log--hide-buffer kill))

;;;###autoload
(defun command-log-command-log-clear ()
  "Clear the command log buffer."
  (interactive)
  (let ((buffer (command-log--get-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

;;;###autoload
(defun command-log-toggle-show-all-commands (&optional arg)
  "Override `command-log-filter-commands' and show everything.
ARG can be passed for direct setting."
  (interactive)
  (setq command-log--show-all-commands
        (or arg (not command-log--show-all-commands)))
  (when-let ((buffer (command-log--get-buffer)))
    (with-current-buffer buffer
      (message
       (propertize
        (format "Show all commands: %s" command-log--show-all-commands)
        'face 'success)))))

;;;###autoload
(defun command-log-save-command-log ()
  "Save commands to today's log.
Clears the command log buffer after saving."
  (interactive)
  (let ((buffer (command-log--get-buffer)))
    (when buffer
      (with-current-buffer buffer
        (make-directory command-log-logging-dir :parents)
        (goto-char (point-min))
        (let ((now (format-time-string "%Y-%02m-%02d %02H:%02M:%02S"))
              (write-region-annotate-functions '(command-log--line-time)))
          (while (and (re-search-forward "^.*" nil t)
                      (not (eobp)))
            (append-to-file (line-beginning-position)
                            (1+ (line-end-position))
                            (concat command-log-logging-dir now))))
        (when (y-or-n-p "Erase buffer?")
          (erase-buffer))))))

(defun command-log--line-time (start _end)
  "Return time at START as [timestamp].
END is ignored"
  (save-excursion
    (goto-char start)
    (let ((time (get-text-property (point) :time)))
      (if time
          (list (cons start
                      (if time
                          (concat "[" (get-text-property (point) :time) "] ")
                        "")))))))

(defun command-log--get-buffer ()
  "Just get the configured command log buffer."
  (get-buffer command-log-buffer-name))

(defun command-log--buffer-visible-p ()
  "Is the buffer already open and visible?"
  (let ((buffer (command-log--get-buffer)))
    (and buffer (get-buffer-window buffer))))

(defun command-log--get-buffer-window-list ()
  "Get the buffer windows or return empty list."
  (let ((buffer (command-log--get-buffer)))
    (if buffer (get-buffer-window-list buffer)
      (list))))

(defun command-log--show-buffer (&optional clear)
  "Displays the command log buffer in a window.
CLEAR will clear the buffer if it exists before returning it."
  (let ((buffer (command-log--setup-buffer clear)))
    (unless (windowp (get-buffer-window buffer))
      ;; attempt to inhibit resize of side window
      (with-current-buffer buffer
        (setq-local window-size-fixed
                    (pcase command-log-default-side
                      ('left 'width)
                      ('right 'width)
                      ('top 'height)
                      ('bottom 'height))))
      (display-buffer-in-side-window
       buffer `((dedicated . nil) (side . ,command-log-default-side))))))

(defun command-log--setup-buffer (&optional clear)
  "Setup (and create) the command-log buffer.
CLEAR will clear the buffer if it exists before returning it."
  (let ((created (not (command-log--get-buffer)))
        (buffer (get-buffer-create command-log-buffer-name)))
    (set-buffer buffer)
    (if created
        (progn (command-log-output-mode)
               (text-scale-set command-log-window-text-scale))
      (when clear
        (erase-buffer)))
    buffer))

(defun command-log--hide-buffer (&optional kill)
  "Delete the buffer window, kill if prefix argument.
KILL will kill the buffer after deleting its window."
  (let ((buffer (get-buffer command-log-buffer-name)))
    (when buffer
      (dolist (win (get-buffer-window-list buffer) nil)
        (delete-window win))
      (when kill
        (kill-buffer buffer)))))

(defun command-log--push-history ()
  "Push the character entered into the buffer into the recent history."
  (setq command-log--recent-history-string
        (concat command-log--recent-history-string
                (key-description (this-command-keys)))))

(defun command-log--mouse-event-p (event)
  "Return t if EVENT is mouse event.
Emacs `mouse-event-p' reports nil for movement."
  (let ((event-type (event-basic-type event)))
    (when (symbolp event-type)
      (string-match-p command-log--mouse-event-regex
                      (symbol-name event-type)))))

(defun command-log--should-log-command-p (cmd event)
  "Determine whether keyboard command CMD should be logged.
EVENT is the last input event that triggered the command."
  (let ((mouse (command-log--mouse-event-p event))
        (text (eq cmd #'self-insert-command))
        (filtered (member cmd command-log-filter-commands))
        (in-log-buffer (eq 'command-log-output-mode
                           (buffer-local-value 'major-mode (current-buffer)))))
    (or command-log--show-all-commands
        (and (not in-log-buffer)
             (or (if command-log-log-mouse
                     (not filtered)
                   (and (not mouse)
                        (not filtered)))
                 (and command-log-log-text text))))))

(defun command-log--scroll-buffer-windows ()
  "Move `point' to end of windows containing log buffer."
  (when (command-log--buffer-visible-p)
    (let ((current (selected-window)))
      (dolist (win (command-log--get-buffer-window-list) nil)
        (select-window win)
        (goto-char (point-max)))
      (select-window current))))

(defun command-log--zap-recent-history (cmd)
  "Clear history if CMD is not in `self-insert-command'."
  (when (or command-log--show-all-commands
            (not (member cmd command-log-filter-commands))
            (not (eq cmd #'self-insert-command)))
    (setq command-log--recent-history-string "")))

(defun command-log--log-command (&optional cmd)
  "Log CMD to the command-log--buffer."
  (let ((deactivate-mark nil) ; do not deactivate mark in transient mark mode
        ;; Don't let random commands change `this-command' Emacs global
        ;; variables by creating local lexical variables with their values.
        (this-command this-command)
        (buffer (command-log--get-buffer))
        (cmd (or cmd this-command))
        (event last-command-event)
        (keys (key-description (this-command-keys))))
    (when (and buffer (command-log--should-log-command-p cmd event))
      (with-current-buffer buffer
        (goto-char (point-max))
        (cond ((and command-log-merge-repeats
                    (not (and command-log-log-text
                              (eq cmd #'self-insert-command)
                              (not command-log--show-all-commands)))
                    (and (eq cmd command-log--last-keyboard-command)
                         (string= keys command-log--last-command-keys)))
               (cl-incf command-log--command-repetitions)
               (save-match-data
                 (when (and (> command-log--command-repetitions 1)
                            (search-backward "[" (line-beginning-position -1) t))
                   (delete-region (point) (line-end-position))))
               (backward-char) ; skip over either ?\newline or ?\space before ?\[
               (insert (propertize (concat " ["
                                           (number-to-string (1+ command-log--command-repetitions))
                                           " times]")
                                   'face 'command-log-repeat-face)))
              ((and (and command-log-log-text (not command-log--show-all-commands))
                    (eq cmd #'self-insert-command))
               (when (eq command-log--last-keyboard-command #'self-insert-command)
                 (delete-char -1)
                 (delete-region (line-beginning-position) (line-end-position)))
               (setq command-log--recent-history-string
                     (concat command-log--recent-history-string (kbd keys)))
               (setq command-log--last-keyboard-command cmd)
               (setq command-log--last-command-keys keys)
               (insert (propertize
                        (format
                         command-log-text-format
                         (string-replace " " command-log-text-space
                                         command-log--recent-history-string))
                        'face 'command-log-text-face))
               (insert "\n"))
              (t
               (setq command-log--command-repetitions 0)
               (insert
                (propertize
                 keys
                 :time  (format-time-string command-log-time-string (current-time))
                 'face 'command-log-key-face))
               (when (>= (current-column) command-log-log-command-indentation)
                 (newline))
               (move-to-column command-log-log-command-indentation t)
               (insert
                (if (byte-code-function-p cmd)
                    (propertize "<bytecode>" 'face 'command-log-command-face)
                  (propertize (symbol-name cmd)
                              'face 'command-log-command-face
                              'button '(t)
                              'category 'default-button)))
               (newline)
               (setq command-log--last-command-keys keys)
               (setq command-log--last-keyboard-command cmd)))
        (when (> (count-lines (point-min) (point-max)) comint-max-line-length)
          (goto-char (point-min))
          (delete-line))
        (command-log--zap-recent-history cmd) ; could be inside condition expression
        (command-log--scroll-buffer-windows)))))

(defvar-local command-log--dribble-file nil
  "Clean up this dribble file.")

(defun command-log--dribble-cleanup ()
  "Delete `command-log--dribble-file'."
  (when (file-exists-p command-log--dribble-file)
    (delete-file command-log--dribble-file )))

;;;###autoload
(defun command-log-tail-dribble ()
  "Open a dribble file and tail the contents."
  (interactive)
  (let* ((dribble-buffer "*Dribble*")
         (buffer (get-buffer dribble-buffer)))
    (if buffer
        (kill-buffer buffer)
      (with-current-buffer (get-buffer-create dribble-buffer)
        (setq-local command-log--dribble-file
                    (make-temp-file "dribble"))
        (open-dribble-file command-log--dribble-file)
        (insert-file-contents command-log--dribble-file 'visit)
        (add-hook 'kill-buffer-hook #'command-log--dribble-cleanup
                  nil 'local)
        (auto-revert-tail-mode)
        (setq-local auto-revert-verbose nil
                    buffer-read-only t))
      (switch-to-buffer-other-window dribble-buffer))))

(defvar-local command-log--lossage-refresh-timer nil
  "Timer that refreshes the lossage buffer.")

(defun command-log--lossage-cleanup ()
  "Cancel the lossage refresh timer."
  (cancel-timer command-log--lossage-refresh-timer))

;;;###autoload
(defun command-log-tail-lossage ()
  "Lossage with automatic refreshing."
  (interactive)
  (view-lossage)
  (when-let* ((help-buffer (get-buffer "*Help*")))
    (with-current-buffer help-buffer
      ;; set up automatic updating unless it's already running.
      (unless (buffer-local-value
               'command-log--lossage-refresh-timer
               help-buffer)
        (let ((timer (timer-create)))
          (timer-set-function timer #'command-log--refresh-lossage
                              (list help-buffer))
          (timer-set-time timer (current-time) 1.0)
          (timer-activate timer)
          (setq-local command-log--lossage-refresh-timer timer)
          (add-hook 'kill-buffer-hook #'command-log--lossage-cleanup
                    nil 'local))))))

(provide 'command-log)
;;; command-log.el ends here
