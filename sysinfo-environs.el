;;;; sysinfo-environ.el --- Display system information in various formats  -*- lexical-binding: t; -*-

;; System Information and Environs

;; Copyright (C) 2025 ......

;; Author: ADD ME <addme@email.net>
;; Maintainer: ADD ME <addme@email.net>
;; URL: https://addme.com
;; Package-Version: ADDME
;; Version: ADDME
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Created: March 2025
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; ADDME

;;; Installation:
;; ADDME

;;; Usage:
;; ADDME

;;; Advice:
;; None currently.

;;; Code:


;;;; Helper functions
(defun sysinfo-environs-newlines-string-into-line-list (longstring)
  "Splits a string into a lists of strings, counting
`\n' (newlines) as dividers."
 (split-string longstring "\n"))

(defun sysinfo-environs-list-of-string-equals-string-into-alist (strings &optional quoted)
  "Transform list of strings into an alist of `(label . value)'.

Takes a list of strings which are of the form:
NAME_MAYBE_WITH_UNDERSCORES=\"value\"

and makes it into a list of alists of the form:
`((LABEL1 . value1) (LABEL2 . value2) ....)'
"
  (mapcar
   (lambda (x) (let ((field (split-string x "=")))
                 (cons
                  (car field)
                  (if quoted 
                      (replace-regexp-in-string "\"" "" (cadr field))
                    (cadr field)))))
   strings))

(defun sysinfo-environs-file-by-line-into-list (file &optional n)
  "Return first N lines of FILE."
  (let ((lines (or n
                   (string-to-number
                    (shell-command-to-string (concat "wc -l " file))))))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (cl-loop repeat lines
               unless (eobp)
               collect (prog1 (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                         (forward-line 1))))))

;;;; Functions to create alists from various data sources

;;;;; /etc/os-release
(defun sysinfo-environs-parse-os-release ()
  "Parse the `/etc/os-release' file into an alist."
  (let ((os-release (sysinfo-environs-file-by-line-into-list "/etc/os-release")))
    (cons "*os-release info*"
          (sysinfo-environs-list-of-string-equals-string-into-alist os-release t))))

;;;;; uname -?
(defun sysinfo-environs-parse-uname-info ()
  "Create an alist from some `uname' outputs."
  (let ((params '(("KERNEL_SPECS" . "-v")
                  ("KERNEL_RELEASE" . "-r")
                  ("KERNEL_NAME" . "-s")
                  ("OPERATING_SYSTEM" . "-o")
                  ("SYSTEM_NAME" . "-n")))
        (uname-info '()))
    (cl-loop for (key . value) in params
             do
             (setq uname-info 
                  (cons
                   (cons key
                         (replace-regexp-in-string "\n$" ""
                                                   (shell-command-to-string
                                                    (concat "uname " value))))
                   uname-info)))
    (cons "*uname -?*" uname-info)))

;;;;; sysinfo from Emacs itself
(defun sysinfo-environs-emacs-known-sysinfo ()
  "Creates an alist from the few system variables Emacs knows."
  (let ((emacs-sysinfo
         `(("system-name" .
            ,(if (version< emacs-version "25.1")
                system-name
              (system-name)))
          ("system-type" . ,system-type)
          ("system-configuration" . ,system-configuration))))
    (cons "*emacs known sysinfo*" emacs-sysinfo)))

;;;;; Emacs information about itself
(defun sysinfo-environs-emacs-self-info ()
  "Creates an alist about things Emacs knows about itself."
  (let ((common-features
         '(emacs-version
           emacs-build-number
           system-configuration))
        (if-featurep
         '(motif
           gtk
           x-toolkit
           ns
           haiku
           ;; cairo
           ))
        ;; (bound-featurep
        ;;  '(x-toolkit-scroll-bars))
        (emacs-self-info nil))
    (dolist (feat common-features)
      (setq emacs-self-info
            (cons
             (cons (symbol-name feat)
                   (eval feat))
             emacs-self-info)))
    (setq emacs-self-info
          (cons
           (cons
            "toolkit"
           (cond ((featurep 'gtk)
                   (concat
                    "GTK-toolkit (GTK "
                    gtk-version-string
                    ")"))
                 ((featurep 'x-toolkit)
                  "Lucid/Athena-toolkit (X toolkit)")
                 ((featurep 'motif)
                  (concat
                   "Motif-toolkit ("
                   ;; note: maybe want to use more than just
                   ;;       this substring
                   ;;       [TODO: test on motif build]
                   (substring motif-version-string 4)
                   ")"))
                 ((featurep 'ns)
                  (concat
                   "NS-toolkit ("
                   ns-version-string
                   ")"))
                 ((featurep 'haiku)
                  (concat
                   "Haiku-toolkit ("
                   (haiku-get-version-string)
                   ")"))
                 (t
                  "none")))
           emacs-self-info))
    (if (featurep 'cairo)
        (setq emacs-self-info
              (cons 
               (cons
                "cairo-version"
                cairo-version-string)
               emacs-self-info)))
    (if (boundp 'x-toolkit-scroll-bars)
      (setq emacs-self-info
            (cons 
             (cons
              "scrollbars toolkit"
              (if (memq x-toolkit-scroll-bars '(xaw xaw3d))
                  (format "%s"
		          (capitalize
                           (symbol-name x-toolkit-scroll-bars)))
                "none"))
             emacs-self-info)))
    (if emacs-build-time
        (setq emacs-self-info
              (cons 
               (cons
                "emacs-build-time"
                (format-time-string emacs-build-time))
               emacs-self-info)))
    (cons "*emacs self info*" (nreverse emacs-self-info))))

;; (sysinfo-environs-emacs-self)



;;;;; Information about Emacs build
;; (defun sysinfo-environs-emacs-own-info ()
;;   "Creates an alist with some useful information about the current Emacs build."
;;   (let ((sco-raw (split-string system-configuration-options))
;;         (sco-processed ())
;;         (sco-state t)
;;         (about-emacs-raw (emacs-version))
;;         (native-comp-avail (native-comp-available-p))
;;         (native-comp-method (if native-comp-jit-compilation 'jit 'aot)))
;;     (dolist (item sco-raw)
;;       (cond
;;        ((eq sco-state t)
;;         )))))

;;;;; list of all sources
(defvar sysinfo-environs-dataset-alist
  '(("emacs-info" . (sysinfo-environs-emacs-known-sysinfo))
    ("emacs-self-info" . (sysinfo-environs-emacs-self-info))
    ("uname-info" . (sysinfo-environs-parse-uname-info))
    ("os-release-info" . (sysinfo-environs-parse-os-release)))
  "An alist of all of the datasources and some names for them.")

(defun sysinfo-environs-dataset-bare-list ()
  "Strips out the pretty names from `sysinfo-environs-dataset-alist'.
For use in helper functions and elsewhere."
  (let ((barelist nil))
    (dolist (dataset sysinfo-environs-dataset-alist)
      (setq barelist
            (append (list (eval (list (cadr dataset))))
                  barelist)))
    (nreverse barelist)))


;; (sysinfo-environs-emacs-known-sysinfo)
;; (sysinfo-environs-dataset-bare-list-quoted)

;;;; Main functions  for creating temp buffers with pretty org tables

;;;###autoload
(defun sysinfo-environs-look-up-field (&optional dataset fieldname)
  "Return the value of a sysinfo `FIELD' from a given `DATASET' source.

Can be used interactively, will prompt user for `DATASET',
listing from all possibilities from `sysinfo-environs-dataset-alist',
and then listing all possible `FIELD's for chosen data source."
  (interactive)
  (let* ((choices sysinfo-environs-dataset-alist)
         (message-p (null (and dataset fieldname)))
         (dataset
          (cdr 
           (if dataset
               ;; if dataset is passed as string
               (if (stringp dataset)
                   (eval
                    (cdr 
                     (assoc dataset choices)))
                 ;; otherwise dataset is just passed as function
                 dataset)
             ;; if no dataset passed, ask user which one
               (eval
                (cdr
                 (assoc
                  (completing-read
                   "Which dataset of info: "
                   (cl-loop for (key . _) in choices
                            collect key))
                  choices))))))
         (field
          ;; use `fieldname' if passed
          (or (and fieldname (assoc fieldname dataset))
              ;; otherwise ask user for field, presenting
              ;; the choices from `dataset'
              (assoc 
               (completing-read
                "Return value for field: "
                (cl-loop for (key . _) in dataset
                         collect key))
               dataset)))
         (value
          (cdr field)))
    ;; if called interactively, message user
    (if message-p
        ;; if the value isn't a string, make it one (appending a quote)
        (if (stringp value)
            (message value)
          (message (concat "'" (symbol-name value))))
      value)))

;; examples:
;; (sysinfo-environs-look-up-field (sysinfo-environs-emacs-known-sysinfo) "system-type")
;; (sysinfo-environs-look-up-field "emacs-info" "system-type")
;; (sysinfo-environs-look-up-field (sysinfo-environs-emacs-known-sysinfo) "system-type") 

(defun sysinfo-environs-sysinfo (datasets &optional titlename)
  "Main function for creating temp buffers with Org tables
containing system information.

Should be called with a list of one or more `DATASETS'
(see below interactive functions for examples), and an optional
`TITLENAME' for the temp buffer."
  (let ((temp-buff-name (or titlename "*System Information*"))
        (logo-name nil)
        (best-image nil))
    (get-buffer-create temp-buff-name)
    (with-current-buffer temp-buff-name
      (read-only-mode -1)
      (erase-buffer)
      (while datasets 
        (let* ((dataset (car datasets))
               (dtitle (car dataset))
               (sysinfo (cdr dataset))
               (sub-temp-buff
                (get-buffer-create (symbol-name (cl-gensym)))))
          (insert "|----|----|\n")
          (insert "| " dtitle "| |\n")
          (insert "|----|----|\n")
          (with-current-buffer sub-temp-buff
            (dolist (item sysinfo)
              (when (and (string= dtitle "*os-release info*")
                         (string= (car item) "LOGO"))
                (setq logo-name (cdr item)))
              (let* ((label (car item))
                     (value* (cdr item))
                     (value (cond ((stringp value*)
                                   value*)
                                  ((symbolp value*)
                                   (concat "'" (symbol-name value*)))
                                  ((numberp value*)
                                   (number-to-string value*)))))
                (insert (concat "=" label "=" " == " "~" value "~" "\n"))))
            (org-mode)
            (org-table-convert-region (point-min) (point-max) " == ")
            (org-table-align)
            (append-to-buffer temp-buff-name (point-min) (point-max))
            (kill-buffer (current-buffer)))
          (insert "|----|----|\n")
          (setq datasets (cdr datasets))))
      (org-mode)
      (org-table-align)
      (org-table-align) ;; need to do it twice for some reason
      (goto-char (point-max))
      (insert "\n")
      (read-only-mode 1)
      (switch-to-buffer-other-window temp-buff-name))
    (when logo-name
      (let ((xdg-dirs (list "/usr/share/icons" "/usr/local/share/icons" "/run/current-system/profile/share/icons"))
            (logo-search '()))
        (dolist (dir xdg-dirs)
          (when (file-directory-p dir)
            (setq logo-search
                  (if (executable-find "find")
                      (split-string
                       (shell-command-to-string (concat "find " dir " -iname \"*" logo-name "*\" -exec realpath {} \\;")) "\n")
                    (directory-files-recursively dir (concat logo-name ".*"))))))
        (dolist (betterimage
                 '(
                   "svg$"
                   "1024x1024" "512x512" "256x256" "128x128"
                   )
                 )
          (setq logo-search-temp logo-search)
          (while (and (= (length best-image) 0) logo-search-temp)
            (let ((imagehit (car logo-search-temp)))
              (when (string-match betterimage imagehit)
                (setq best-image imagehit))
              (setq logo-search-temp (cdr logo-search-temp)))))))
      (with-current-buffer temp-buff-name
        (read-only-mode -1)
        (goto-char (point-max))
        (when best-image 
          (insert (concat "[[" best-image "]]")))
        (org-link-preview)
        (read-only-mode 1)
        (goto-char (point-min)))))

;;;; Interactive functions

;;;###autoload
(defun sysinfo-environs-os-release-info ()
  "Interactive function to display `/etc/os-release' info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list 
    (sysinfo-environs-parse-os-release))
   "*OS Release Info*"))

;;;###autoload
(defun sysinfo-environs-os-uname-info-display ()
  "Interactive function to display `uname -?' info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list 
    (sysinfo-environs-parse-uname-info))
   "*OS uname Info*"))

;;;###autoload
(defun sysinfo-environs-os-release-and-uname-display ()
  "Interactive function to display all system information
from os-release and uname."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-parse-uname-info)
    (sysinfo-environs-parse-os-release))
   "*System Info*"))

;;;###autoload
(defun sysinfo-environs-emacs-self-info-display ()
  "Interactive function to display information about Emacs itself."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-emacs-self-info))
   "*Emacs Self Info*"))


;;;###autoload
(defun sysinfo-environs-full-sys-info-display ()
  "Interactive function to display all accessible system info."
  (interactive)
  (sysinfo-environs-sysinfo
   (sysinfo-environs-dataset-bare-list)
   "*System Info*"))


;; (sysinfo-environs-os-release-info)

;;; printenv things, probably not good
(defun sysinfo-environs-split-paths-with-newlines (input)
  (let ((new-alist nil))
    (dolist (item input)
      (setq new-alist
            (cons
             (cons
              (car item)
              (if (not (null (cdr item)))
                       (replace-regexp-in-string ":" ":\n"
                                                 (cdr item))
                (cdr item)))
             new-alist)))
    new-alist))

(defun sysinfo-environs-get-all-env ()
  (cons "*environment variables*"
        ;; (sysinfo-environs-split-paths-with-newlines
         (sysinfo-environs-list-of-string-equals-string-into-alist
          (sysinfo-environs-newlines-string-into-line-list
           (shell-command-to-string "printenv")))))

;; (sysinfo-environs-sysinfo (list (sysinfo-environs-get-all-env)q))

;;; other old bits
;; (defun sysinfo-environs-os-release-look-up-field (&optional field)
;;   "Return the value of an os-release field `FIELD'."
;;   (interactive)
;;   (sysinfo-environs-look-up-field
;;    (sysinfo-environs-parse-os-release)
;;    (when field
;;      field)))
