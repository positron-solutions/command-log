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

(eval-when-compile (require 'cl-lib))

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

(defcustom clm-auto-show t
  "Show the command-log window or frame automatically."
  :group 'command-log
  :type 'boolean)

(defcustom clm-key-binding-open-log nil
  "The key binding used to toggle the log window."
  :group 'command-log
  :type '(radio
          (const :tag "No key" nil)
          (key-sequence "C-c o"))) ;; this is not right though it works for kbd

(defcustom clm-open-log-turns-on-mode t
  "Does opening the command log turn on the mode?"
  :group 'command-log
  :type 'boolean)

(defcustom clm-close-log-turns-off-mode t
  "Does closing the command log turn off the mode?"
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

(defcustom clm-logging-dir "~/log/"
  "Directory in which to store files containing logged commands."
  :group 'command-log
  :type 'directory)

(defcustom clm-buffer-name " *command-log*"
  "Name for displaying command log."
  :group 'command-log
  :type 'string)

(defvar clm--command-log-buffer nil
  "Reference of the currenly used buffer to display logged commands.")

(defvar clm--recent-history-string ""
  "This string will hold recently typed text.")

(defvar clm--command-repetitions 0
  "Count of how often the last keyboard commands has been repeated.")

(defvar clm--last-keyboard-command nil
  "Last logged keyboard command.")

(defun clm--recent-history ()
  (setq clm--recent-history-string
	(concat clm--recent-history-string
		(buffer-substring-no-properties (- (point) 1) (point)))))

(defun clm--zap-recent-history ()
  (unless (or (member this-original-command
		      clm-log-command-exceptions)
	      (eq this-original-command #'self-insert-command))
    (setq clm--recent-history-string "")))

;;;###autoload
(define-minor-mode command-log-mode
  "Toggle keyboard command logging."
  :init-value nil
  :lighter " command-log"
  :keymap nil
  (if command-log-mode
      (when (and
             clm-auto-show
             (not (get-buffer-window clm--command-log-buffer)))
        (clm-open-command-log-buffer))
      ;; We can close the window though
      (clm-close-command-log-buffer)))

(define-global-minor-mode global-command-log-mode command-log-mode
  command-log-mode)

(defun clm--should-log-command-p (cmd &optional buffer)
  "Determine whether keyboard command CMD should be logged.

If non-nil, BUFFER specifies the buffer used to determine whether
CMD should be logged.  If BUFFER is nil, the current buffer is
assumed."
  (let ((val (if buffer
		 (buffer-local-value command-log-mode buffer)
	       command-log-mode)))
    (and (not (null val))
	 (null (member cmd clm-log-command-exceptions)))))

(defun clm-open-command-log-buffer (&optional arg)
  "Open or create a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared."
  (interactive "P")
  (with-current-buffer
      (setq clm--command-log-buffer
            (get-buffer-create clm-buffer-name))
    (text-scale-set clm-window-text-scale))
  (when arg
    (with-current-buffer clm--command-log-buffer
      (erase-buffer)))
  (let ((new-win (split-window-horizontally
                  (- 0 clm-window-size))))
    (set-window-buffer new-win clm--command-log-buffer)
    (set-window-dedicated-p new-win t)))

(defun clm-close-command-log-buffer ()
  "Close the command log window."
  (interactive)
  (with-current-buffer
      (setq clm--command-log-buffer
            (get-buffer-create clm-buffer-name))
    (let ((win (get-buffer-window (current-buffer))))
      (when (windowp win)
        (delete-window win)))))

;;;###autoload
(defun clm-toggle (&optional arg)
  "Display/hide the buffer and activate/deactivate command-log modes.

The following variables are used to configure this toggle:

`clm-log-globally' controls the preference for `command-log-mode`
minor mode or `global-command-log-mode.
`clm-open-log-turns-on-mode' will activate modes if showing the log buffer.
`clm-close-log-turns-off-mode' will clean up modes if killing the log buffer.

Passing a prefix ARG will clear the buffer before display."
  (interactive "P")
  (when (and clm-open-log-turns-on-mode
             (not command-log-mode))
    (if clm-log-globally
        (global-command-log-mode t)
      (command-log-mode t)))

  (with-current-buffer
      (setq clm--command-log-buffer
            (get-buffer-create clm-buffer-name))
    (let ((win (get-buffer-window (current-buffer))))
      (if (windowp win)
          (progn
            (when (and clm-close-log-turns-off-mode
                       (command-log-mode nil))
              (if clm-log-globally
                  (global-command-log-mode nil)
                (command-log-mode nil)))
            (clm-close-command-log-buffer))
        ;; Else open the window
        (clm-open-command-log-buffer arg)))))

(defun clm-scroll-buffer-window (buffer &optional move-fn)
  "Update `point' of windows containing BUFFER according to MOVE-FN.

If non-nil, MOVE-FN is called on every window which displays
BUFFER.  If nil, MOVE-FN defaults to scrolling to the bottom,
making the last line visible.

Scrolling up can be accomplished with:
\(clm-scroll-buffer-window
  buf (lambda () (goto-char (point-min))))"
  (let ((selected (selected-window))
	(point-mover (or move-fn
			 (function (lambda () (goto-char (point-max)))))))
    (walk-windows (function (lambda (window)
			      (when (eq (window-buffer window) buffer)
				(select-window window)
				(funcall point-mover)
				(select-window selected))))
		  nil t)))

(defmacro clm-with-command-log-buffer (&rest body)
  (declare (indent 0))
  `(when (and (not (null clm--command-log-buffer))
	      (buffer-name clm--command-log-buffer))
     (with-current-buffer clm--command-log-buffer
       ,@body)))

(defun clm--log-command (&optional cmd)
  "Log CMD to the clm--buffer."
  (let ((deactivate-mark nil) ; do not deactivate mark in transient mark mode
        ;; Don't let random commands change `this-command' and `last-command'
        ;; Emacs global variables by creating local lexical variables with
        ;; their values.
        (this-command this-command)
        (last-command last-command))
    (setq cmd (or cmd this-command))
    (when (clm--should-log-command-p cmd)
      (clm-with-command-log-buffer
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
                (t ;; (message "last cmd: %s cur: %s" last-command cmd)
                 ;; showing accumulated text with interleaved key presses isn't very useful
        	 (when (and clm-log-text clm-merge-repeats)
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
          (clm-scroll-buffer-window current))))))

(defun clm-command-log-clear ()
  "Clear the command log buffer."
  (interactive)
  (with-current-buffer clm--command-log-buffer
    (erase-buffer)))

(defun clm--line-time (start end)
  "Return time at START as [timestamp].
END is ignored"
  (save-excursion
    (goto-char start)
    (let ((time (get-text-property (point) :time)))
      (if time
	  (list (cons start (if time 
				(concat "[" (get-text-property (point) :time) "] ")
			      "")))))))

(defun clm-save-command-log ()
  "Save commands to today's log.
Clears the command log buffer after saving."
  (interactive)
  (save-window-excursion
    (set-buffer (get-buffer clm-buffer-name))
    (goto-char (point-min))
    (let ((now (format-time-string "%Y-%m-%d"))
	  (write-region-annotate-functions '(clm--line-time)))
      (while (and (re-search-forward "^.*" nil t)
		  (not (eobp)))
	(append-to-file (line-beginning-position) (1+ (line-end-position)) (concat clm-logging-dir now))))
    (clm-command-log-clear)))

;; TODO only hook when modes activated and unhook when deactivating
(add-hook 'post-self-insert-hook 'clm--recent-history)
(add-hook 'post-command-hook 'clm--zap-recent-history)
(add-hook 'pre-command-hook 'clm--log-command)

;; TODO use normal binding interface
(eval-after-load 'command-log-mode
  '(when clm-key-binding-open-log
    (global-set-key
     (kbd clm-key-binding-open-log)
     'clm-toggle)))

(provide 'command-log-mode)

;;; command-log-mode.el ends here
