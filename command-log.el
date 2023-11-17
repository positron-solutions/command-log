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

(defcustom command-log-default-side 'right
  "Which side for use in `display-buffer-in-side-window'."
  :group 'command-log
  :type 'symbol
  :options '(right left top bottom))

(define-obsolete-variable-alias 'command-log-window-text-scale
  'command-log-text-scale "0.2.0")

(define-obsolete-variable-alias 'command-log-mode-window-font-size
  'command-log-text-scale "0.2.0")

(defcustom command-log-text-scale 0
  "The text scale of the command-log output.
+1,+2,... increase and -1,-2,... decrease the font size."
  :group 'command-log
  :type 'integer)

(defcustom command-log-repeat-format " #%s"
  "How to display repeats."
  :group 'command-log
  :type 'string)

(defcustom command-log-keys-min-width 8
  "How wide to make keys.
One way to help indentation.  Default will align up to two chords
with a trailing space."
  :group 'command-log
  :type 'integer)

(defcustom command-log-text-format "\"%s\""
  "How to display text.
Only applies when `command-log-text' is non-nil."
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
  '((t :inherit font-lock-function-name-face))
  "Face for commands in command log."
  :group 'command-log)

(defface command-log-repeat-face
  '((t :inherit 'shadow))
  "Face for commands in command log."
  :group 'command-log)

(defface command-log-text-face
  '((t :inherit 'font-lock-string-face))
  "Face for text echo'ing in the command log."
  :group 'command-log)

(define-obsolete-variable-alias 'clm/time-string
  'command-log-time-string "0.2.0")

(defcustom command-log-time-string "%Y-%m-%dT%H:%M:%S"
  "Sent to `format-time-string' if time logging enabled."
  :group 'command-log
  :type 'string)

(defcustom command-log-max-log-lines 256
  "Set higher if you need to save long sessions."
  :group 'command-log
  :type 'string)

(define-obsolete-variable-alias 'command-log-mode-auto-show
  'command-log-enable-shows "0.2.0")

(defcustom command-log-enable-shows t
  "Turning on logging shows the buffer if it's not visible."
  :group 'command-log
  :type 'boolean)

(define-obsolete-variable-alias 'command-log-hiding-disables-logging
  'command-log-hide-disables "0.2.0")

(defcustom command-log-hide-disables t
  "Hiding the buffer deactivates logging modes."
  :group 'command-log
  :type 'boolean)

(define-obsolete-variable-alias 'command-log-disabling-logging-kills-buffer
  'command-log-disable-kills "0.2.0")

(defcustom command-log-disable-kills t
  "Turning off all logging kills the buffer."
  :group 'command-log
  :type 'boolean)

(define-obsolete-variable-alias 'command-log-log-globally
  'command-log-prefer-global "0.2.0")

(defcustom command-log-prefer-global t
  "When logging is enabled automatically, is it global?"
  :group 'command-log
  :type 'boolean)


(define-obsolete-variable-alias 'clm/log-command-exceptions*
  'command-log-ignored "0.2.0")

(define-obsolete-variable-alias 'command-log-filter-commands
  'command-log-ignored "0.2.0")

(defcustom command-log-ignored
  '(self-insert-command
    handle-switch-frame)
  "A list commands which should be ignored.
For `self-insert-command', logging text overrides this."
  :group 'command-log
  :type '(repeat (symbol :tag "command function name")))


(define-obsolete-variable-alias 'command-log-log-text 'command-log-text "0.2.0")

(define-obsolete-variable-alias 'clm/log-text 'command-log-text "0.2.0")

(defcustom command-log-text nil
  "Log text as strings instead of `self-insert-commands'.
You may want to just except `self-insert-command' by adding it to
`command-log-filter-commands'."
  :group 'command-log
  :type 'boolean)

(define-obsolete-variable-alias 'command-log-log-mouse 'command-log-muose
  "0.2.0")

(defcustom command-log-mouse nil
  "Log mouse events.
Toggling this is more conveneint than setting `command-log-ignored-commands'."
  :group 'command-log
  :type 'boolean)

(define-obsolete-variable-alias 'clm/log-repeat 'command-log-merge-repeats
  "0.2.0")

(defcustom command-log-merge-repeats t
  "Merge repetitions of the same command."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-pre-command-target 'this-command
  "Symbol to report during the pre-command.
When non-nil, the symbol will be read during the
`pre-command-hook'.  Normally we are interested in
`this-command', but special debugging circumstances may be
interested in `last-repeatable-command' or `this-real-command'
etc.  Any Lisp variable that is always bound will work, but could
break features such as text logging, which watch for
`self-insert-command' in the post-command.

Experience shows that many commands which delegate out to other
commands, such `ivy-done' or lispy commands, will have the effect
of resetting `this-command' and `this-real-command' both between
the pre & post command hook.  In the pre-command hook we see the
command that was called, but for commands such as \`M-x', it' is
potentially not interesting to see the pre-command `this-command'
value, such as `ivy-done', the result of pressing \`RET' in an
\`M-x' menu.

The following is a log output of all likely events you will want
to log during the pre and post command:

;; pre-command-hook:
;;    this-command: ivy-done
;;    real-this-command: ivy-done
;;    real-last-command: self-insert-command
;;    last-repeatable-command: self-insert-command
;;
;; post-command-hook:
;;   this-command: next-line
;;   real-this-command: next-line
;;   real-last-command: self-insert-command
;;   last-repeatable-command: self-insert-command

Note: `self-insert-command' was the actual command that preceded
pressing \`RET' to cause an `ivy-done' command.
`real-last-command' is not terribly interesting.  It does not
change between the pre & post command hooks in any observed
cases.

However, also note that `real-this-command' was modified between
pre & post-command hooks.  This contradicts the semantic meaning
and docstring of `real-this-command'.  Practically, we don't care
because we caught the change via the `pre-command-hook' rather
than blindly trusting the words of the `real-last-command'
docstring author.  We obtain a useful result, and that is what
matters.

\`M-x' is known to not update `this-command' in the case of
counsel's \`M-x' command.

When developing a package, you may be very interested in
different values pre and post command.  It is very easy to modify
the point that values are recorded to fit your use case.  See the
body of `command-log--log-pre-command' and
`command-log--log-post-command' and submit a patch if you find a
use case for making these support function calls for example.

See `command-log-merge-targets' information about output
formatting."
  :group 'command-log
  :type 'symbol)

(defcustom command-log-post-command-target 'this-command
  "Symbol to report during the post-command.
When non-nil, the symbol will be read during the
`post-command-hook'.  Normally we are interested in
`this-command', but special debugging circumstances may be
interested in `last-repeatable-command' or `this-real-command'
etc.  Any Lisp variable that is always bound will work.

See `command-log-pre-command-target' for more information."
  :group 'command-log
  :type 'symbol)

(defcustom command-log-merge-repeat-targets 'post-command
  "Controls merging behavior when targets match or not.
We can prefer the `'pre-command' or the `'post-command'.  Setting
to `'nil' or `'both' will not merge the values at all.  The
pre-command will be first in the output.

When `command-log-pre-command-target' and
`command-log--log-post-command' target are the same or different,
how should we behave?  Since the user did not press additional
keys, there are no inputs, we will only display keys once.  The
same is true for repeats, but the repeat counter will only count
a repeat if everything matches.  Since the pre-command comes
before the post-command, we always show the pre-command before
the post command when both are shown.

See `command-log-post-excepting-pre-commands' for information
about commands that will override the setting of `'post-command'."
  :group 'command-log
  :type 'symbol
  :options '(pre-command post-command both))

(defcustom command-log-post-excepting-pre-commands
  `(counsel-M-x
    execute-extended-command
    ,(command-remapping 'execute-extended-command)) ; whatever the user's remap is
  "A list of commands that do not update `this-command'.
We decline to print the post-command value when the pre-command
value of `this-command' is a member of this list.  It's tricky.
There isn't a great way to detect this.  The only known use is
however terribly common, `execute-extended-command' and it's
re-mappings."
  :group 'command-log
  :type '(repeat symbol))

(define-obsolete-variable-alias 'command-log-logging-dir
  'command-log-save-dir "0.2.0")

(define-obsolete-variable-alias 'clm/logging-dir 'command-log-save-dir "0.2.0")

(defcustom command-log-save-dir (locate-user-emacs-file
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

(defvar command-log--pre-command nil
  "Command after remapping.")

(defvar command-log--pre-command-keys nil
  "Command keys after remapping.")

(defvar command-log--last-pre-command nil
  "Last logged pre command.")

(defvar command-log--last-post-command nil
  "Last logged post command.")

(defvar command-log--last-command-keys nil
  "Last key description for `this-command-keys'.")

(defvar command-log--repeat-start-marker nil
  "Marker for updating the repeat counter.")

(defvar command-log--repeat-end-marker nil
  "Marker for updating the repeat counter.")

(defvar command-log--self-insert-start nil
  "Marker for updating sequences of `self-insert-command'.")

(defvar command-log--self-insert-end nil
  "Marker for updating sequences of `self-insert-command'.")

(defvar command-log--self-insert-string ""
  "This string will hold recently typed text.")

(defvar command-log--show-all-commands nil
  "Override `command-log-filter-commands' and show all commands instead.")

(defvar command-log--last-log-marker nil
  "Saving our location so we can update repeat commands.")

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
  (cond (command-log-mode
         (add-hook 'pre-command-hook
                   #'command-log--log-pre-command 'default-depth)
         (add-hook 'post-command-hook
                   #'command-log--log-post-command 'default-depth))
        (t
         (remove-hook 'pre-command-hook #'command-log--log-pre-command)
         (remove-hook 'post-command-hook #'command-log--log-post-command))))

;;;###autoload
(define-globalized-minor-mode global-command-log-mode command-log-mode command-log-mode
  "Enables minor mode in all buffers, including minibuffer."
  :group 'command-log)

;;;###autoload
(defun command-log-toggle (&optional clear)
  "Display/hide the buffer and activate/deactivate command-log modes.

The following variables are used to configure this toggle:

`command-log-prefer-global' controls the preference for
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
          (if command-log-prefer-global
              (global-command-log-mode nil)
            (command-log-mode nil))
          (when command-log-disabling-logging-kills-buffer
            (command-log--hide-buffer t))))
    (progn
      (if command-log-prefer-global
          (global-command-log-mode t)
        (command-log-mode t))
      (when command-log-enable-shows
        (command-log--show-buffer clear)))))

(declare-function 'clm/toggle-command-log-buffer "command-log-mode" ())
(declare-function 'clm/open-command-log-buffer "command-log-mode" ())
(define-obsolete-function-alias
  'clm/toggle-command-log-buffer #'command-log-toggle "0.2.0")
(define-obsolete-function-alias
  'clm/open-command-log-buffer #'command-log-toggle "0.2.0")

;;;###autoload
(defun command-log-close-buffer (&optional kill)
  "Close the command log window.
Prefix argument will KILL buffer."
  (interactive "P")
  (command-log--hide-buffer kill))

(declare-function 'clm/close-command-log-buffer "command-log-mode" ())
(define-obsolete-function-alias
  'clm/close-command-log-buffer #'command-log-close-buffer "0.2.0")
(define-obsolete-function-alias
  'command-log-close-command-log-buffer #'command-log-close-buffer "0.2.0")

;;;###autoload
(defun command-log-clear ()
  "Clear the command log buffer."
  (interactive)
  (let ((buffer (command-log--get-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

(declare-function 'clm/command-log-clear "command-log-mode" ())
(define-obsolete-function-alias 'clm/command-log-clear #'command-log-clear "0.2.0")

;;;###autoload
(defun command-log-toggle-show-all (&optional arg)
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

(define-obsolete-function-alias
  'command-log-toggle-show-all-commands #'command-log-toggle-show-all "0.2.0")

;;;###autoload
(defun command-log-save ()
  "Save commands to today's log.
Clears the command log buffer after saving."
  (interactive)
  (let ((buffer (command-log--get-buffer)))
    (when buffer
      (with-current-buffer buffer
        (make-directory command-log-save-dir :parents)
        (goto-char (point-min))
        (let ((now (format-time-string "%Y-%02m-%02d %02H:%02M:%02S"))
              (write-region-annotate-functions '(command-log--line-time)))
          (while (and (re-search-forward "^.*" nil t)
                      (not (eobp)))
            (append-to-file (line-beginning-position)
                            (1+ (line-end-position))
                            (concat command-log-save-dir now))))
        (when (y-or-n-p "Erase buffer?")
          (erase-buffer))))))

(declare-function 'clm/save-command-log "command-log-mode" ())
(define-obsolete-function-alias
  'command-log-save-command-log #'command-log-save "0.2.0")
(define-obsolete-function-alias 'clm/save-command-log #'command-log-save "0.2.0")

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
             (or (if command-log-mouse
                     (not filtered)
                   (and (not mouse)
                        (not filtered)))
                 (and command-log-text text))))))

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
    (setq command-log--self-insert-string "")))

(defun command-log--format-command (cmd)
  "Make CMD human pretty and clickable."
  (if (byte-code-function-p cmd)
      (propertize "<bytecode>" 'face 'command-log-command-face)
    (propertize (symbol-name cmd)
                'face 'command-log-command-face
                'button '(t)
                'category 'default-button)))

(defun command-log--log-pre-command ()
  "Record the pre-command state.
This enables us to differentiate commands that delegate out to other commands by
reading before the command and comparing the state during the post command
hook."
  (setq command-log--pre-command-keys (key-description (this-command-keys)))
  (setq command-log--pre-command (symbol-value command-log-pre-command-target)))

(defun command-log--log-post-command ()
  "Write the command information to the output."
  (let ((deactivate-mark nil) ; do not deactivate mark in transient mark mode
        (buffer (command-log--get-buffer))
        (pre-cmd command-log--pre-command)
        (post-cmd (symbol-value command-log-post-command-target))
        (event last-command-event)
        (keys command-log--pre-command-keys))
    ;; During updates, use `real-this-command' for logic in case the user
    ;; has selected some random value for the targets.
    (when (and buffer (command-log--should-log-command-p real-this-command event))
      (with-current-buffer buffer
        (goto-char (point-max))
        (cond ((and command-log-merge-repeats
                    (not (and command-log-text
                              (eq post-cmd #'self-insert-command)
                              (not command-log--show-all-commands)))
                    ;; must completely match
                    (and (eq pre-cmd command-log--last-pre-command)
                         (eq post-cmd command-log--last-post-command)
                         (string= keys command-log--last-command-keys)))
               ;; Either set up repeat or delete marked region of old repeat and
               ;; re-insert between markers.
               (cl-incf command-log--command-repetitions)
               (if (> command-log--command-repetitions 1)
                   (progn (delete-region command-log--repeat-start-marker
                                         command-log--repeat-end-marker)
                          (goto-char command-log--repeat-start-marker))
                 (backward-char)
                 (setq command-log--repeat-start-marker (point-marker)
                       command-log--repeat-end-marker (point-marker))
                 (set-marker-insertion-type command-log--repeat-end-marker t))
               (insert (propertize (format command-log-repeat-format
                                           (1+ command-log--command-repetitions))
                                   'face 'command-log-repeat-face)))

              ((and (and command-log-text (not command-log--show-all-commands))
                    (eq post-cmd #'self-insert-command))
               ;; TODO the only reason we can't log text and all commands
               ;; simultaneously is because of this condition statement.

               ;; Either set up string or delete marked region of old string and
               ;; re-insert between markers.
               (if (eq command-log--last-post-command #'self-insert-command)
                   (progn (delete-region command-log--self-insert-start
                                         command-log--self-insert-end)
                          (goto-char command-log--self-insert-start))
                 (setq command-log--self-insert-start (point-marker)
                       command-log--self-insert-end (point-marker))
                 (set-marker-insertion-type command-log--self-insert-end t))
               (setq command-log--self-insert-string
                     (concat command-log--self-insert-string (kbd keys)))
               (insert
                (propertize
                 (format
                  command-log-text-format
                  ;; 32 is space and I always wish I could write it more
                  ;; explicitly lol
                  (subst-char-in-string 32
                                        (string-to-char command-log-text-space)
                                        command-log--self-insert-string)
                  'face 'command-log-text-face)))
               (newline))
              (t
               (insert
                (propertize
                 keys
                 :time  (format-time-string command-log-time-string (current-time))
                 'face 'command-log-key-face))

               (when (< (length keys) command-log-keys-min-width)
                 (insert (make-string (- command-log-keys-min-width
                                         (length keys))
                                      32)))
               (when (>= (length keys) command-log-keys-min-width)
                 (insert 32))

               (if (and (not (eq pre-cmd post-cmd))
                        (not (member
                              pre-cmd
                              command-log-post-excepting-pre-commands))
                        (eq command-log-merge-repeat-targets 'post-command))
                   (insert (command-log--format-command post-cmd))
                 (insert (command-log--format-command pre-cmd)))
               (newline)
               (when (and (not (eq pre-cmd post-cmd))
                          (not (member
                                pre-cmd
                                command-log-post-excepting-pre-commands))
                          (not (member command-log-merge-repeat-targets
                                       '(pre-command post-command))))
                 (insert (make-string command-log-keys-min-width 32))
                 (insert (command-log--format-command post-cmd))
                 (newline))

               ;; non-string command.  unset string tracking.
               (setq command-log--self-insert-end nil
                     command-log--self-insert-start nil
                     command-log--self-insert-string nil)
               ;; non-repeat command.  unset repetition tracking.
               (setq command-log--command-repetitions 0
                     command-log--repeat-start-marker nil
                     command-log--repeat-end-marker nil)))

        ;; every non-skipped command updates the last command
        (setq command-log--last-post-command post-cmd
              command-log--last-pre-command pre-cmd
              command-log--last-command-keys keys)

        (when (> (count-lines (point-min) (point-max))
                 command-log-max-log-lines)
          (delete-region (goto-char (point-min))
                         (progn  (forward-line) (point))))
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

(provide 'command-log)
;;; command-log.el ends here
