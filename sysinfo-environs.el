;;; sysinfo-environs.el --- Display system information in various formats  -*- lexical-binding: t; -*-

;; System Information and Environs

;; Copyright (C) 2025 ......

;; Author: ADD ME <addme@email.net>
;; Maintainer: ADD ME <addme@email.net>
;; URL: https://addme.com
;; Package-Version: 0.1
;; Version: 0.1
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
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)

(defgroup sysinfo-environs nil
  "Display system information in various formats."
  :group 'tools)

(defcustom sysinfo-environs-try-to-find-unspecified-logo t
  "Search for logo-name by `ID' even if `os-release' doesn't specify."
  :group 'sysinfo-environs
  :type 'boolean)

(defcustom sysinfo-environs-show-os-release-logo-image t
  "Show logo image if possible when `os-release' is pared."
  :group 'sysinfo-environs
  :type 'boolean)

(defcustom sysinfo-environs-neofetch-no-single-window nil
  "If set, don't single-window neofetch-info."
  :group 'sysinfo-environs
  :type 'boolean)

;;;; Helper functions
;;;###autoload
(defun sysinfo-environs-newlines-string-into-line-list (longstring)
  "Splits a string `LONGSTRING' into a lists of strings.
\=(`\n' (newlines) are counted as dividers.\=)"
 (split-string longstring "\n"))

;;;###autoload
(defun sysinfo-environs-list-of-string-equals-string-into-alist (strings &optional quoted)
  "Transform list of strings `STRINGS' into an alist of `(label . value)'.

Takes a list of strings which are of the form:
NAME_MAYBE_WITH_UNDERSCORES=\"value\"

and makes it into a list of alists of the form:
`((LABEL1 . value1) (LABEL2 . value2) ....)'

Optional argument `QUOTED' escapes double-quotes in STRINGS."
  (mapcar
   (lambda (x) (let ((field (split-string x "=")))
                 (cons
                  (car field)
                  (if quoted
                      (replace-regexp-in-string "\"" "" (cadr field))
                    (cadr field)))))
   strings))

;;;###autoload
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
;;;###autoload
(defun sysinfo-environs--find-logo-path (logo-name os-id-name)
  "Find a path to the distributions `LOGO-NAME' image file.

Assumes operating system ID to be `OS-ID-NAME'.
\=(Tries to use .svg or highest resolution .png.\=)"
  (let ((best-image nil)
        (xdg-dirs (list
                   "/usr/local/share/icons"
                   (concat "/usr/share/" os-id-name)
                   "/usr/share/pixmaps"
                   "/run/current-system/profile/share/icons"
                   "/usr/share/icons"))
        (image-matcher (list
                        "svg$"
                        "1024x1024"
                        "512x512"
                        "256x256"
                        "128x128"))
        (logo-search '()))
    (dolist (dir xdg-dirs)
      (when (file-directory-p dir)
        (let ((new-logo-hit
               (if (executable-find "find")
                    (split-string
                     (shell-command-to-string (concat "find " dir " -iname \"*" logo-name "*\" -exec realpath {} \\;")) "\n")
                  (directory-files-recursively dir (concat logo-name ".*")))))
          (when new-logo-hit
            (setq logo-search (append logo-search new-logo-hit ))))))
    (setq logo-search (cl-remove "" logo-search :test 'string=))
    (while (and (not best-image)
                image-matcher)
      (let ((logo-search-temp logo-search)
            (img-style (car image-matcher)))
        (while logo-search-temp
          (let ((imagehit (car logo-search-temp)))
            (when (string-match img-style imagehit)
              (setq best-image imagehit))
            (setq logo-search-temp (cdr logo-search-temp)))))
      (setq image-matcher (cdr image-matcher)))
    ;; (dolist (betterimage image-matcher)
    ;;   (setq logo-search-temp logo-search)
    ;;   (while (and (= (length best-image) 0) logo-search-temp)
    ;;     (let ((imagehit (car logo-search-temp)))
    ;;       (when (string-match betterimage imagehit)
    ;;         (setq best-image imagehit)
    ;;         (let* ((old-image-matcher image-matcher)
    ;;                (image-matcher nil)
    ;;                (searchdone nil))
    ;;           (while (and old-image-matcher
    ;;                       (not searchdone))
    ;;             (let ((top-list (car old-image-matcher)))
    ;;               (setq image-matcher (cons top-list image-matcher))
    ;;               (when (string= betterimage top-list)
    ;;                 (setq searchdone t))
    ;;               (setq old-image-matcher (cdr old-image-matcher))))))
    ;;       (setq logo-search-temp (cdr logo-search-temp)))))
    ;; (cons logo-search best-image)
    best-image))

;; Simulate os-release not found:
;;
(let* ((os-release-file nil)
       (os-release-file (or os-release-file
                                    "[file not found]"))
       (os-release nil)
       (os-release-fake
        (cons "*os-release info*"
              (cons
               (cons "`os-release' location" os-release-file)
               (when os-release
                 (sysinfo-environs-list-of-string-equals-string-into-alist
                  os-release t))))))
    (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-parse-uname-info)
    os-release-fake)
   "*Simulated System Info*")
    ;; (sysinfo-environs-full-sys-info)
    )

;;;###autoload
(defun sysinfo-environs-parse-os-release ()
  "Parse the `/etc/os-release' file into an alist."
  (let* ((os-release-file
          (cond ((file-exists-p "/etc/os-release")
                 "/etc/os-release")
                ((file-exists-p "/usr/lib/os-release")
                 "/usr/lib/os-release")
                (t nil)))
         (os-release-file (or os-release-file
                              "[file not found]"))
         (os-release-raw (when os-release-file
                       (sysinfo-environs-file-by-line-into-list
                        os-release-file)))
         (os-release
          (cons "*os-release info*"
                (cons
                 (cons "`os-release' location" os-release-file)
                 ;; TODO:
                 ;; maybe test for symlink and show pointer??
                 (if os-release-raw
                   (sysinfo-environs-list-of-string-equals-string-into-alist
                    os-release-raw t)
                   os-release-file))))
         (os-id-name (cdr (assoc "ID" (cdr os-release))))
         (logo-name (or
                     (cdr (assoc "LOGO" (cdr os-release)))
                     (if sysinfo-environs-try-to-find-unspecified-logo
                         os-id-name
                       nil))))
    ;; find logo image
    (let ((img-path (if logo-name 
                        (sysinfo-environs--find-logo-path
                         logo-name os-id-name)
                      nil)))
      (when logo-name
        (setq os-release (nreverse
                          (cons
                           (cons "LOGO-IMAGE-PATH"
                                 (if img-path
                                     img-path
                                   "[not found]"))
                           (nreverse os-release)
                           ;; (nreverse (cl-remove nil os-release :test 'equal))
                           ))))
      (cl-remove nil os-release :test 'equal))))

;; (cdr (assoc "LOGO" (cdr (sysinfo-environs-parse-os-release))))
;; (setq sysinfo-raws (sysinfo-environs-parse-os-release))
;; (sysinfo-environs--find-logo-path "endeavouros" "endeavouros")
;; (assoc "LOGO" (cdr sysinfo-raws))
;; (assoc "ID" (cdr sysinfo-raws))
;; (cdr (assoc "LOGO" (cdr sysinfo-raws)))
;; (cdr (assoc "ID" (cdr sysinfo-raws)))


;;;;; uname -?

;;;###autoload
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
    (cons "*uname -?*"
          (cl-remove nil uname-info :test 'equal))))

;;;;; sysinfo from Emacs itself

;;;###autoload
(defun sysinfo-environs-emacs-known-sysinfo ()
  "Create an alist from the few system variables Emacs knows."
  (let ((emacs-sysinfo
         `(("system-name" .
            ,(if (version< emacs-version "25.1")
                system-name
              (system-name)))
          ("system-type" . ,system-type)
          ("system-configuration" . ,system-configuration))))
    (cons "*emacs known sysinfo*" emacs-sysinfo)))

;;;;; Emacs information about itself

;;;###autoload
(defun sysinfo-environs-emacs-self-info ()
  "Create an alist about things Emacs knows about itself."
  (let ((common-features
         '(emacs-version
           emacs-build-number
           system-configuration))
        ;; (if-featurep
        ;;  '(motif
        ;;    gtk
        ;;    x-toolkit
        ;;    ns
        ;;    haiku
        ;;    ;; cairo
        ;;    ))
        ;; (bound-featurep
        ;;  '(x-toolkit-scroll-bars))
        (emacs-self-info nil))
    (dolist (feat common-features)
      (setq emacs-self-info
            (cons
             (cons (symbol-name feat)
                   (format "%s" (eval feat)))
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
              "scrollbars-toolkit"
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
                (format-time-string "%Y-%m-%d %H:%M:%S" emacs-build-time))
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

;;;;; $ENV
(defun sysinfo-environs-get-all-env ()
  "Unused function for $ENV things."
  (cons "*environment variables*"
        (cl-remove '("") 
                   (sysinfo-environs-list-of-string-equals-string-into-alist
                    (sysinfo-environs-newlines-string-into-line-list
                     (shell-command-to-string "printenv")))
                   :test 'equal)))

(defun sysinfo-environs-gui-env ()
  "Return likely GUI environment (i.e., WM/DE)."
  (let ((xdg-current-desktop-top
         (if (sysinfo-environs-look-up-field "env-vars-info" "XDG_CURRENT_DESKTOP")
           (car 
            (split-string  ;; in-case it's a :'ed PATH-type return
              (sysinfo-environs-look-up-field "env-vars-info" "XDG_CURRENT_DESKTOP")
              ":"))
           ""))
        (xdg-session-desktop
         (sysinfo-environs-look-up-field "env-vars-info" "XDG_SESSION_DESKTOP"))
        (session-desktop
         (sysinfo-environs-look-up-field "env-vars-info" "DESKTOP_SESSION")))
    (cond ((not (or (string-empty-p xdg-current-desktop-top)
                    (null xdg-current-desktop-top)))
           xdg-current-desktop-top)
          ((not (or (string-empty-p xdg-session-desktop)
                    (null xdg-session-desktop)))
           xdg-session-desktop)
          ((not (or (string-empty-p session-desktop)
                    (null session-desktop)))
           session-desktop)
          ;; check for wmctrl...
          (t nil))))

;; (setq my/wmde (sysinfo-environs-gui-env))
;; (setq sysinfo-env-vars (sysinfo-environs-get-all-env))

;;;;; /sys/devices/virtual/dmi/id
(defun sysinfo-environs-read-sys-devices-virtual-dmi-ids ()
  "Get accessible hardware info encoded in `/sys/devices/virtual/dmi/id'."
(let* ((virtual-device-ids-dir "/sys/devices/virtual/dmi/id")
       (virtual-device-ids
        (split-string (shell-command-to-string
                       (concat "ls " virtual-device-ids-dir))))
       (hardsysinfo nil))
  (dolist (item virtual-device-ids)
    (let* ((item-file (concat virtual-device-ids-dir "/" item))
           (item-content
            (if (and (f-file-p item-file) (file-readable-p item-file))
                (replace-regexp-in-string "\n" ""
                                          (f-read-text item-file))
              nil)))
      (when item-content
        (setq hardsysinfo
              (cons
               (cons item
                     item-content)
               hardsysinfo)))))
  (cons "*virtual dev info*"
   hardsysinfo)))

;; (setq hardtesty (sysinfo-environs-read-sys-devices-virtual-dmi-ids))

;;;;; network

;; (format "%X:%X:%X" 65152 0 4762)

;; TRY....
;; (let ((out ""))
;;   (dolist (val '(65152 0 0 0 4762 56831 65182 36611 0))
;;     (setq out (concat out
;;                (format "%X:" val))))
;;   out)

;; (let ((out nil))
;;   (dolist (val '(65152 0 0 0 4762 56831 65182 36611 0))
;;     (setq out (cons
;;                (format "%X" val)
;;                out)))
;;   (setq out (nreverse out))
;;   (dolist (seg out)
;;     ))
;; END TRY....

;; two or more 0000 blocks change to ::, but only one group (with most in row)


;; (network-interface-list)(("wlan0" . [65152 0 0 0 4762 56831 65182 36611 0]) ("lo" . [0 0 0 0 0 0 0 1 0]) ("wlan0" . [192 168 0 217 0]) ("lo" . [127 0 0 1 0]))

;; (let ((interfaces nil))
;;   (dolist (interface (network-interface-list))
;;     (let* ((name (car interface))
;;            (addr (cdr interface))
;;            (lo-p (if (string= name "lo") t nil))
;;            (ip4-p (if (< (length addr) 6) t nil))
;;            (ip (if ip4-p
;;                  (format "%s.%s.%s.%s"
;;                          (nth 0 addr)
;;                          (nth 1 addr)
;;                          (nth 2 addr)
;;                          (nth 3 addr))
;;                  (format "%X:%X:%X")))))
;;     (setq interfaces
;;           (cons
;;            (cons )
;;            interfaces))))

;;;;; list of all sources
(defvar sysinfo-environs-dataset-alist
  '(("emacs-info" . (sysinfo-environs-emacs-known-sysinfo))
    ("emacs-self-info" . (sysinfo-environs-emacs-self-info))
    ("uname-info" . (sysinfo-environs-parse-uname-info))
    ("os-release-info" . (sysinfo-environs-parse-os-release))
    ("env-vars-info" . (sysinfo-environs-get-all-env))
    ("sys-devices-virtual-dmi-info" . (sysinfo-environs-read-sys-devices-virtual-dmi-ids)))
  "An alist of all of the datasources and some names for them.")

(defun sysinfo-environs-dataset-bare-list ()
  "Strips out the pretty names from `sysinfo-environs-dataset-alist'.
For use in helper functions and elsewhere."
  (let ((barelist nil))
    (dolist (dataset sysinfo-environs-dataset-alist)
      (setq barelist
            (cons (eval (list (cadr dataset)))
                  barelist)))
    (nreverse 
     (cl-remove nil barelist :test 'equal))))


;; (sysinfo-environs-emacs-known-sysinfo)
;; (sysinfo-environs-dataset-bare-list)
;; (setq testy (sysinfo-environs-dataset-bare-list))
;; (sysinfo-environs-dataset-bare-list-quoted)

;;;; Main functions  for creating temp buffers with pretty org tables

;;;###autoload
(defun sysinfo-environs-look-up-field (&optional dataset fieldname)
  "Return the value of a sysinfo `FIELDNAME' from a given `DATASET' source.

Can be used interactively, will prompt user for `DATASET',
listing from all possibilities from `sysinfo-environs-dataset-alist',
and then listing all possible `FIELDNAME's for chosen data source."
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
             (when (called-interactively-p 'any)
               (eval
                (cdr
                 (assoc
                  (completing-read
                   "Which dataset of info: "
                   (cl-loop for (key . _) in choices
                            collect key))
                  choices)))))))
         (field
          ;; use `fieldname' if passed
          (or (and fieldname (assoc fieldname dataset))
              ;; otherwise ask user for field, presenting
              ;; the choices from `dataset'
              (and (called-interactively-p 'any)
                   (assoc
                    (completing-read
                     "Return value for field: "
                     (cl-loop for (key . _) in dataset
                              collect key))
                    dataset))))
         (value
          (when field
            (cdr field))))
    ;; if called interactively, message user
    (if field
        (if message-p
            ;; if the value isn't a string, make it one (appending a quote)
            (if (stringp value)
                (message value)
              (message (concat "'" (symbol-name value))))
          value))))

;; examples:
;; (sysinfo-environs-look-up-field (sysinfo-environs-emacs-known-sysinfo) "system-type")
;; (sysinfo-environs-look-up-field "emacs-info")
;; (sysinfo-environs-look-up-field (sysinfo-environs-emacs-known-sysinfo) "system-type")

;;; Main function

;;;###autoload
(defun sysinfo-environs-sysinfo (datasets &optional titlename show-logo)
  "Create temp buffers with Org tables containing system information.

Should be called with a list of one or more `DATASETS'
\=(see below interactive functions for examples\=), and an optional
`TITLENAME' for the temp buffer."
  (let ((temp-buff-name (or titlename "*System Information*"))
        (os-is-name nil))
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
      (switch-to-buffer-other-window temp-buff-name)
      (when (and (boundp 'visual-fill-column-mode)
                 visual-fill-column-mode)
        (visual-fill-column-mode -1)))
      (with-current-buffer temp-buff-name
        (read-only-mode -1)
        (goto-char (point-max))
        (when (and sysinfo-environs-show-os-release-logo-image
               show-logo)
          (let ((img-path (sysinfo-environs-look-up-field "os-release-info" "LOGO-IMAGE-PATH")))
            (when (and img-path
                       (not (string= img-path "[not found]")))
              (insert (concat "[[" img-path "]]"))))
          (org-link-preview))
        (read-only-mode 1)
        (goto-char (point-min)))))

;;;; uptime
(defun sysinfo-environ-sys-uptime ()
  "Get system uptime."
  (interactive)
  (let ((cat-proc-uptime
         (string-to-number
          (car 
           (split-string 
            (shell-command-to-string "cat /proc/uptime"))))))
    (format-seconds "%Y, %D, %H, %M, %z%S" cat-proc-uptime)))

;; (setq catup (sysinfo-environ-sys-uptime))

;;;; (n)E(o)fetch

;;;###autoload
(defun sysinfo-environs-neofetch-like (&optional no-logo titlename)
  "Interactive function sort of like (neo|fast)fetch."
  (interactive)
  (defalias 'lookup 'sysinfo-environs-look-up-field)
  (let* ((machine (lookup "sys-devices-virtual-dmi-info" "product_version"))
         (machine (if (string-empty-p machine)
                      (lookup "sys-devices-virtual-dmi-info" "product_family")
                    machine)))
    (let ((temp-buff-name (or titlename "*(n)E(o)fetch*"))
          (neofetch `(("OS" .
                       ,(concat
                         (lookup "os-release-info" "PRETTY_NAME")
                         " "
                         system-configuration))
                      ("Host" .
                       ,(concat
                         (lookup "sys-devices-virtual-dmi-info" "product_name")
                         " ("
                         machine ;; product_version or product_family
                         ")"))
                      ("Kernel" .
                       ,(concat
                        (lookup "uname-info" "KERNEL_NAME")
                        " "
                        (lookup "uname-info" "KERNEL_RELEASE")))
                      ("System Uptime" .
                       ,(sysinfo-environ-sys-uptime))
                      ("Emacs Uptime" .
                       ,(emacs-uptime))
                      ("WM/DE" .
                       ,(concat
                         (sysinfo-environs-gui-env)
                         " ("
                         (lookup "env-vars-info" "XDG_SESSION_TYPE")
                         ")"))
                      )))
      (get-buffer-create temp-buff-name)
      (with-current-buffer temp-buff-name
        (read-only-mode -1)
        (erase-buffer)
        (insert "|----|\n")
        (insert "|" " ~" user-login-name"~​" "@​=" (system-name)"=" "|\n")
        (insert "|----|\n")
        (dolist (item neofetch)
          (let ((field (car item))
                (val (cdr item)))
            ;; ....
            (insert "| =" field "=: " "~" val "~" "|\n")
            )
          )
        (insert "|----|\n")
        (org-mode)
        (goto-char (point-min))
        (unless (equal no-logo t)
          ;; (org-table-insert-column)
          ;; (next-line)
          ;; (next-line)
          (let
              ((img-path
                (sysinfo-environs-look-up-field "os-release-info" "LOGO-IMAGE-PATH")))
            (when (and img-path
                       (not (string= img-path "[not found]")))
              (insert "\n")
              (insert (concat "[[" img-path "]]"))
              (insert "\n\n")))
          (org-link-preview))
        (org-table-align)
        (org-table-align) ;; need to do it twice for some reason
        (goto-char (point-min))
        ;; (insert "\n\n")
        ;; (goto-char (point-max))
        ;; (insert "\n")
        (switch-to-buffer-other-window temp-buff-name)
        (when (and (boundp 'visual-fill-column-mode)
                   visual-fill-column-mode)
          (visual-fill-column-mode -1))
        (read-only-mode 1)
        (goto-char (point-min))
        (unless sysinfo-environs-neofetch-no-single-window
          (delete-other-windows))))))

;; (sysinfo-environs-neofetch-like)


;;;; Interactive convenience functions

;;;###autoload
(defun sysinfo-environs-os-release-info-display ()
  "Interactive function to display `/etc/os-release' info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-parse-os-release))
   "*OS Release Info*"
   t))

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
  "Interactive function to display info from os-release and uname."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-parse-uname-info)
    (sysinfo-environs-parse-os-release))
   "*System Info*"
   t))

;;;###autoload
(defun sysinfo-environs-emacs-self-info-display ()
  "Interactive function to display information about Emacs itself."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-emacs-self-info))
   "*Emacs Self Info*"))

;;;###autoload
(defun sysinfo-environs-env-display ()
  "Interactive function to display environment info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-get-all-env))
   "*ENV info*"))

;;;###autoload
(defun sysinfo-environs-virtual-dmi-info-display ()
  "Interactive function to display virtual dmi/dev info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-read-sys-devices-virtual-dmi-ids))
   "*virtual dmi info*"))



;;;###autoload
(defun sysinfo-environs-fullish-sys-info-display ()
  "Interactive function to display all accessible system info."
  (interactive)
  (sysinfo-environs-sysinfo
   (list
    (sysinfo-environs-emacs-known-sysinfo)
    (sysinfo-environs-emacs-self-info)
    (sysinfo-environs-parse-uname-info)
    (sysinfo-environs-parse-os-release)
    ;; (sysinfo-environs-read-sys-devices-virtual-dmi-ids)
    )
    "*System Info*"
    t))


;;;###autoload
(defun sysinfo-environs-fullest-sys-info-display ()
  "Interactive function to display all accessible system info."
  (interactive)
  (sysinfo-environs-sysinfo
   (sysinfo-environs-dataset-bare-list)
   "*System Info*"
   t))


;; (sysinfo-environs-os-release-info)

;;; printenv things, probably not good
;; (defun sysinfo-environs-split-paths-with-newlines (input)
;;   "Unused function for $ENV things from `INPUT'."
;;   (let ((new-alist nil))
;;     (dolist (item input)
;;       (setq new-alist
;;             (cons
;;              (cons
;;               (car item)
;;               (if (not (null (cdr item)))
;;                        (replace-regexp-in-string ":" ":\n"
;;                                                  (cdr item))
;;                 (cdr item)))
;;              new-alist)))
;;     new-alist))

(defun sysinfo-environs-get-all-env ()
  "Unused function for $ENV things."
  (cons "*environment variables*"
        (cl-remove '("") 
                   (sysinfo-environs-list-of-string-equals-string-into-alist
                    (sysinfo-environs-newlines-string-into-line-list
                     (shell-command-to-string "printenv")))
                   :test 'equal)))



;; wm/de:
;; check $XDG_CURRENT_DESKTOP (need to check if contains ":
;; if """, check $XDG_SESSION_DESKTOP
;; if "", check $DESKTOP_SESSION
;; if "", check if `wmctrl' exists, if so, `wmctrl -m' outputs, e.g.:
;; Name: EXWM
;; Class: N/A
;; PID: N/A
;; Window manager's "showing the desktop" mode: N/A

;; (sysinfo-environs-sysinfo (list (sysinfo-environs-get-all-env)))
;; (setq sysinf-environs-env (list (sysinfo-environs-get-all-env)))

;;; other old bits
;; (defun sysinfo-environs-os-release-look-up-field (&optional field)
;;   "Return the value of an os-release field `FIELD'."
;;   (interactive)
;;   (sysinfo-environs-look-up-field
;;    (sysinfo-environs-parse-os-release)
;;    (when field
;;      field)))

(provide 'sysinfo-environs)

;;; sysinfo-environs.el ends here
