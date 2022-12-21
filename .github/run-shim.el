;;; run-shim.el --- -*-no-byte-compile: t; lexical-binding: t -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  Psionik K <73710933+psionic-k@users.noreply.github.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package sets up load paths and then loads the test files and runs
;; commands depending on the command line arguments.
;;
;; Usage:
;;
;; Always get a fresh Emacs for your test runs.  It will reload features and
;; byte compile where necessary.  The Emacs provided by the nix develop shell
;; contains the dependencies declared in the flake.nix.
;;
;;   # Use .github#emacsGit instead for tests against upstream
;;   nix develop .github#
;;   "emacs" --quick --script .github/run-shim.el -- lint
;;   "emacs" --quick --script .github/run-shim.el -- test
;;
;; Note that this elisp script assumes that some packages are located in
;; specific locations.

;;; Code:

(defun run-shim-lint-package ()
  "Lint the files in the package directory."

  (require 'elisp-lint)
  ;; 100-character column limit for lints.  If it's good enough for Linux, it's
  ;; good enough for us.  https://lkml.org/lkml/2020/5/29/1038
  (setq-default fill-column 100)
  ;; Spaces
  (setq-default indent-tabs-mode nil)

  ;; `command-line-args-left has the same effect as passing command line arguments.
  (let ((command-line-args-left
         (append
          '(;; "--no-<check>
            ;; "--no-byte-compile"
            ;; "--no-checkdoc"
            ;; "--no-indent"
            ;; "--no-package-lint"
            ;; "--no-fill-column"
            ;; "--no-check-declare"
            )
          (seq-filter
           (lambda (s) (not (string-match-p ".*autoloads.*.el$" s)))
           (file-expand-wildcards "../*.el")))))

    (message "ARGS: %s" command-line-args-left)

    ;; (setq elisp-lint-ignored-validators nil
    ;;       elisp-lint-file-validators nil
    ;;       elisp-lint-buffer-validators nil
    ;;       elisp-lint-batch-files nil)

    (elisp-lint-files-batch)))

(defun run-shim ()
  "Execute a CI process based on CLI arguments."
  ;; This expression normalizes the behavior of --quick --load <file> and --script
  ;; <file> behavior.  If you don't do this, --script will see every argument
  ;; passed and the arguments from the Nix wrapper to set load paths.  You can use
  ;; this to pass extra options to your scripts in the github actions.
  (when (member (car argv) '("-l" "--"))
    (print "Normalizing arguments")
    (while (not (member (car argv) '("--" nil)))
      (print (format "Normalizing arguments, stripped: %s" (pop argv))))
    (pop argv))

  (message "original default directory: %s" default-directory)
  ;; Configure load paths
  (setq default-directory (if load-file-name (file-name-directory load-file-name)
                            default-directory))
  (let* ((lisp-dir (expand-file-name (concat default-directory "../"))))
    (print (format "package load path: %s" lisp-dir))
    (push lisp-dir load-path))

  ;; running manually may encounter stale .elc
  (setq load-prefer-newer t)

  ;; Consume the command argument and run one of the routines
  (setq command (pop argv)) ; nil-safe
  (cond ((string= command "test")
         (require 'command-log)
         (global-command-log-mode t))
        ((string= command "lint") (run-shim-lint-package))
        t (print "Command not recognized.  Use test, lint etc.")))

;; Only attempt to run when Emacs is loading with or --batch --no-x-resources,
;; which is implied by -Q.
(when (or noninteractive inhibit-x-resources)
  (run-shim))

(provide 'run-shim)
;;; run-shim.el ends here
