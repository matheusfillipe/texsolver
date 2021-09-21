;;; texsolver.el -*- lexical-binding: t; -*-
;; texsolver.el emacs functions for texsolver
;; Copyright (C) 2021 Matheus Fillipe
;;
;; Author: Matheus Fillipe <https://github.com/matheusfillipe>
;; Maintainer: Matheus Fillipe <matheusfillipeag@gmail.com>
;; Created: September 01, 2021
;; Modified: September 01, 2021
;; Version: 0.0.1
;; Keywords: tex cli elisp latex sympy solver equations math expressions calculus
;; Homepage: https://github.com/matheus/texsolver
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;; Code:

(defvar texsolver-cli-path "~/bin/texsolver.py")
(defvar texsolver-wolframalpha-key "")

(defun texsolver-suggest-text (text &optional extra-args)
  "Get suggestions for text"
  (or extra-args
      (setq extra-args ""))
  (if (file-exists-p texsolver-cli-path)
      (progn (message "Running...")
             (let ((output (shell-command-to-string (format "%s %s \"%s\"" (expand-file-name
                                                                            texsolver-cli-path)
                                                            extra-args texsolver-last-text))))
               (if (> (string-width output) 0)
                   (progn
                     (setq texsolver-list (split-string output "\n"))
                     (push "ALL" texsolver-list)
                     (setq texsolver-last-opt (completing-read "Select: " texsolver-list))
                     (end-of-line)
                     (if (string= texsolver-last-opt "ALL")
                         (progn (insert " \\\\")
                                (mapcar
                                 (lambda (elm)
                                   (newline-and-indent)
                                   (insert (format "%s \\\\" elm)))
                                 (split-string output "\n"))
                                ;;(string-join (split-string output "\n") " \\\\\n")
                                )
                       (progn (newline-and-indent)
                              (insert texsolver-last-opt))))
                 (message "No results..."))))
    (message (format "No file found at %s" texsolver-cli-path))))

(defun texsolver-suggest-region (start end)
  "Get suggestions for region"
  (interactive "r")
  (setq texsolver-last-text
        (buffer-substring-no-properties
         start
         end))
  (texsolver-suggest-text texsolver-last-text))

(defun texsolver-worlfram-query (query)
  "Users wolfram alpha backend. Requires texsolver-worlframalpha-key"
  (interactive "sWA Query: ")
  (setq texsolver-last-text query)
  (texsolver-suggest-text query (format "-wak %s" texsolver-wolframalpha-key)))

(defun texsolver-worlfram-region (start end)
  "Users wolfram alpha backend. Requires texsolver-worlframalpha-key"
  (interactive "r")
  (setq texsolver-last-text
        (buffer-substring-no-properties
         start
         end))
  (texsolver-suggest-text texsolver-last-text (format "-wak %s" texsolver-wolframalpha-key)))

(provide 'texsolver)
;;; texsolver.el ends here
