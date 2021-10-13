;;; command-log-mode.el --- log keyboard commands to buffer -*- lexical-binding: t -*-

;; homepage: https://github.com/lewang/command-log-mode

;; Copyright (C) 2013 Nic Ferrier
;; Copyright (C) 2012 Le Wang
;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: help
;; Initial-version: <2004-10-07 11:41:28 michaelw>
;; Time-stamp: <2004-11-06 17:08:11 michaelw>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on can be used to demo Emacs to an audience.  When
;; activated, keystrokes get logged into a designated buffer, along
;; with the command bound to them.

;; To enable, use e.g.:
;;
;; (require 'command-log-mode)
;; (add-hook 'LaTeX-mode-hook 'command-log-mode)
;;
;; To see the log buffer, call M-x clm-open-command-log-buffer.

;; The key strokes in the log are decorated with ISO9601 timestamps on
;; the property `:time' so if you want to convert the log for
;; screencasting purposes you could use the time stamp as a key into
;; the video beginning.

;;; Code:

(require 'cl-lib)

(defgroup command-log nil
  "Customization for the command log."
   :prefix 'clm
   :group 'convenience)

(defcustom clm-window-size 40
  "The size of the command-log window."
  :group 'command-log
  :type 'integer)

(defcustom clm-window-text-scale 0
  "The text scale of the command-log window.

+1,+2,... increase and -1,-2,... decrease the font size."
  :group 'command-log
  :type 'integer)

(defcustom clm-log-command-indentation 11
  "Indentation of commands in command log buffer."
  :group 'command-log
  :type 'integer)

(defface clm-key-face
  '((t :inherit 'font-lock-keyword-face))
  "Face for keys in command log."
  :group 'command-log)

(defface clm-command-face
  '((t :inherit font-lock-function-name-face))
  "Face for commands in command log."
  :group 'command-log)

(defface clm-repeat-face
  '((t :inherit 'font-lock-doc-face))
  "Face for commands in command log."
  :group 'command-log)

(defcustom clm-time-string "%Y-%m-%dT%H:%M:%S"
  "The string sent to `format-time-string' when command time is logged."
  :group 'command-log
  :type 'string)

(defcustom clm-logging-shows-buffer t
  "Turning on logging shows the buffer if it's not visible."
  :group 'command-log
  :type 'boolean)

(defcustom clm-hiding-disables-logging t
  "Hiding the buffer deactivates logging modes."
  :group 'command-log
  :type 'boolean)

(defcustom clm-disabling-logging-kills-buffer t
  "Turning off all logging kills the buffer."
  :group 'command-log
  :type 'boolean)

(defcustom clm-log-globally t
  "Does turning on command-log-mode happen globally?"
  :group 'command-log
  :type 'boolean)

(defcustom clm-exceptions
  '(self-insert-command
    handle-switch-frame)
  "A list commands which should not be logged, despite logging being enabled.
Frequently used non-interesting commands (like cursor movements)
should be put here."
  :group 'command-log
  :type '(repeat symbol))

(defcustom clm-log-text t
  "A non-nil setting means text will be saved to the command log."
  :group 'command-log
  :type 'boolean)

(defcustom clm-merge-repeats t
  "Merge repetitions of the same command."
  :group 'command-log
  :type 'boolean)

(defcustom clm-logging-dir "~/.emacs.d/etc/command-log-mode/"
  "Directory in which to store files containing logged commands."
  :group 'command-log
  :type 'directory)

(defcustom clm-buffer-name " *command-log*"
  "Name for displaying command log."
  :group 'command-log
  :type 'string)

(defvar clm--command-repetitions 0
  "Count of how often the last keyboard commands has been repeated.")

(defvar clm--last-keyboard-command nil
  "Last logged keyboard command.")

(defvar clm--recent-history-string ""
  "This string will hold recently typed text.")

(define-minor-mode command-log-mode
  "Toggle keyboard command logging."
  :init-value nil
  :lighter " command-log"
  :keymap nil
  (if command-log-mode
      (add-hook 'pre-command-hook #'clm--log-command 'default-depth 'buffer-local)
    (remove-hook 'pre-command-hook #'clm--log-command 'buffer-local)))

(define-globalized-minor-mode global-command-log-mode command-log-mode command-log-mode
  "Enables minor mode in all buffers, including minibuffer."
  :group 'command-log)

;;;###autoload
(defun clm-toggle (&optional clear)
  "Display/hide the buffer and activate/deactivate command-log modes.

The following variables are used to configure this toggle:

`clm-log-globally' controls the preference for `command-log-mode`
minor mode or `global-command-log-mode.
`clm-open-log-turns-on-mode' will activate modes if showing the log buffer.
`clm-close-log-turns-off-mode' will clean up modes if killing the log buffer.

Passing a prefix CLEAR will clear the buffer before display."
  (interactive "P")
  (if (clm--buffer-visible-p)
      (progn
        (clm--hide-buffer)
        (when clm-hiding-disables-logging
          (if clm-log-globally
              (global-command-log-mode nil)
            (command-log-mode nil))
          (when clm-disabling-logging-kills-buffer
            (clm--hide-buffer t))))
    (progn
      (if clm-log-globally
          (global-command-log-mode t)
        (command-log-mode t))
      (when clm-logging-shows-buffer
        (clm--show-buffer clear)))))

;;;###autoload
(defun clm-close-command-log-buffer (&optional kill)
  "Close the command log window.
Prefix argument will KILL buffer."
  (interactive "P")
  (clm--hide-buffer kill))

;;;###autoload
(defun clm-command-log-clear ()
  "Clear the command log buffer."
  (interactive)
  (let ((buffer (clm--get-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

;;;###autoload
(defun clm-save-command-log ()
  "Save commands to today's log.
Clears the command log buffer after saving."
  (interactive)
  (let ((buffer (clm--get-buffer)))
    (when buffer
      (save-window-excursion
        (set-buffer buffer)
        (goto-char (point-min))
        (let ((now (format-time-string "%Y-%02m-%02d %02H:%02M:%02S"))
	      (write-region-annotate-functions '(clm--line-time)))
          (while (and (re-search-forward "^.*" nil t)
		      (not (eobp)))
	    (append-to-file (line-beginning-position) (1+ (line-end-position)) (concat clm-logging-dir now))))
        (erase-buffer)))))

(defun clm--line-time (start _end)
  "Return time at START as [timestamp].
END is ignored"
  (save-excursion
    (goto-char start)
    (let ((time (get-text-property (point) :time)))
      (if time
	  (list (cons start (if time
				(concat "[" (get-text-property (point) :time) "] ")
			      "")))))))

(defun clm--get-buffer ()
  "Just get the configured command log buffer."
  (get-buffer clm-buffer-name))

(defun clm--buffer-visible-p ()
  "Is the buffer already open and visible?"
  (let ((buffer (clm--get-buffer)))
    (and buffer (get-buffer-window buffer))))

(defun clm--get-buffer-window-list ()
  "Get the buffer windows or return empty list."
  (let ((buffer (clm--get-buffer)))
    (if buffer (get-buffer-window-list buffer)
      (list))))

(defun clm--show-buffer (&optional clear)
  "Displays the command log buffer in a window.
CLEAR will clear the buffer if it exists before returning it."
  (let ((buffer (clm--setup-buffer clear)))
    (let ((win (get-buffer-window buffer)))
      (unless (windowp win)
        (let ((new-win (split-window-horizontally
                        (- 0 clm-window-size))))
          (set-window-buffer new-win buffer)
          (set-window-dedicated-p new-win t)))
      buffer)))

(defun clm--setup-buffer (&optional clear)
  "Setup (and create) the command-log-mode buffer.
CLEAR will clear the buffer if it exists before returning it."
  (let ((created (not (clm--get-buffer)))
        (buffer (get-buffer-create clm-buffer-name)))
    (progn (if created
               (with-current-buffer buffer
                 (text-scale-set clm-window-text-scale))
             (when clear
               (with-current-buffer buffer
                 (erase-buffer))))
           buffer)))

(defun clm--hide-buffer (&optional kill)
  "Delete the buffer window, kill if prefix argument.
KILL will kill the buffer after deleting its window."
  (let ((buffer (get-buffer clm-buffer-name)))
    (when buffer
      (dolist (win (get-buffer-window-list buffer) nil)
        (delete-window win))
      (when kill
        (kill-buffer buffer)))))

(defun clm--push-history ()
  "Push the character entered into the buffer into the recent history."
  (setq clm--recent-history-string
	(concat clm--recent-history-string
		(key-description (this-command-keys)))))

(defun clm--should-log-command-p (cmd)
  "Determine whether keyboard command CMD should be logged."
  ;; TODO check pause
  ;; TODO check minibuffer is logging
  (not (member cmd clm-exceptions)))

(defun clm--scroll-buffer-windows ()
  "Move `point' to end of windows containing log buffer."
  (when (clm--buffer-visible-p)
    (dolist (win (clm--get-buffer-window-list) nil)
      (save-window-excursion
        (select-window win)
        (goto-char (point-max))))))

(defun clm--zap-recent-history (cmd)
  "Clear history if CMD is not exception or `self-insert-command'."
  (unless (or (member this-command
		      clm-exceptions)
	      (eq this-command #'self-insert-command))
    (setq clm--recent-history-string "")))

(defun clm--log-command (&optional cmd)
  "Log CMD to the clm--buffer."
  (let ((deactivate-mark nil) ; do not deactivate mark in transient mark mode
        ;; Don't let random commands change `this-command' Emacs global
        ;; variables by creating local lexical variables with their values.
        (this-command this-command)
        (buffer (clm--get-buffer))
        (cmd (or cmd this-command)))
    (when (and buffer (clm--should-log-command-p cmd))
      (with-current-buffer buffer
        (let ((current (current-buffer)))
          (goto-char (point-max))
          (cond ((and clm-merge-repeats (eq cmd clm--last-keyboard-command))
                 (cl-incf clm--command-repetitions)
                 (save-match-data
                   (when (and (> clm--command-repetitions 1)
                              (search-backward "[" (line-beginning-position -1) t))
                     (delete-region (point) (line-end-position))))
                 (backward-char) ; skip over either ?\newline or ?\space before ?\[
                 (insert (propertize (concat
                                      " ["
                                      (number-to-string (1+ clm--command-repetitions))
                                      " times]")
                                     'face 'clm-repeat-face)))
                 (t
        	  (when (and clm-log-text clm-merge-repeats)
                    ;; showing accumulated text with interleaved key presses isn't very useful
        	    (if (eq clm--last-keyboard-command 'self-insert-command)
        	        (insert (propertize
                                 (concat "[text: " clm--recent-history-string "]\n")
                                 'face 'clm-repeat-face))))
                  (setq clm--command-repetitions 0)
                  (insert
                   (propertize
                    (key-description (this-command-keys))
                    :time  (format-time-string clm-time-string (current-time))
                    'face 'clm-key-face))
                  (when (>= (current-column) clm-log-command-indentation)
                    (newline))
                  (move-to-column clm-log-command-indentation t)
                  (insert
                   (propertize
                    (if (byte-code-function-p cmd) "<bytecode>" (symbol-name cmd))
                    'face 'clm-command-face))
                  (newline)
                  (setq clm--last-keyboard-command cmd)))
          (clm--zap-recent-history cmd)
          (clm--scroll-buffer-windows))))))

(provide 'command-log-mode)
;;; command-log-mode.el ends here
