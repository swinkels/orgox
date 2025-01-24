;;; orgox.el --- Convert org-mode file to one suited for ox-hugo  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pieter Swinkels

;; Author: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; Maintainer: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; URL: https://github.com/swinkels/orgox
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3") (f "0.20.0") (ox-hugo "0.12.2"))
;; Keywords: tools ox-hugo

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Convert org-mode file to one suited for ox-hugo
;;

;;; Code:

(provide 'orgox)

(defcustom orgox nil
  "Convert org-mode file to one suitable for publishing by ox-hugo."
  :group 'Tools)

(defcustom orgox-hugo-site-directory nil
  "Directory of ox-hugo site."
  :type 'directory :group 'orgox)

(defcustom orgox-content-org-directory nil
  "Directory of ox-hugo site to hold org-files to publish."
  :type 'directory :group 'orgox)

;;;; Public definitions

(defun orgox-publish-current-buffer ()
  (interactive)
  (let ((ox-hugo-file (orgox-current-buffer-to-ox-hugo)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (org-hugo-export-wim-to-md :all-subtrees))))

(defun orgox-current-buffer-to-ox-hugo ()
  (interactive)
  (write-as-ox-hugo-file (current-buffer) orgox-content-org-directory))

(defun orgox-sync-note-dir ()
  (unless orgox-hugo-site-directory
    (error "Hugo site directory is not configured (orgox-hugo-site-directory)"))

  (unless (f-directory-p orgox-hugo-site-directory)
    (error (format
            "Hugo site directory %s does not exist (orgox-hugo-site-directory)"
            orgox-hugo-site-directory)))

  (let* ((static-subdir-name org-hugo-default-static-subdirectory-for-externals)
         (static-dir
          (file-name-concat orgox-hugo-site-directory "static" static-subdir-name)))

    (orgox--sync-note-dir (current-buffer) static-dir)))

;;;; Private definitions orgox-current-buffer-to-ox-hugo

(defun write-as-ox-hugo-file (org-buffer dest-dir)
  (let* ((ox-hugo-file-name (get-ox-hugo-file-name (buffer-name org-buffer)))
         (ox-hugo-file (file-name-concat dest-dir ox-hugo-file-name)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (write-as-ox-hugo-buffer org-buffer (current-buffer))
      (save-buffer))
    ox-hugo-file))

(defun write-as-ox-hugo-buffer (org-buffer ox-hugo-buffer)
  (let ((org-buffer-name (buffer-name org-buffer)))
    (with-current-buffer ox-hugo-buffer
      (erase-buffer)
      (insert-buffer org-buffer)
      (beginning-of-buffer)
      (insert "#+HUGO_BASE_DIR: ../\n")
      (insert
       (format "#+HUGO_SECTION: %s\n" (extract-date-sections org-buffer-name)))
      (newline 1)

      ;; let the first headline provide the title
      (org-next-visible-heading 1)
      (org-set-property "EXPORT_FILE_NAME" (slug-for-current-heading))
      (org-set-property "EXPORT_DATE" (extract-date org-buffer-name)))))

(defun extract-date-sections (buffer-name)
  (let ((elements (extract-date-elements buffer-name)))
    (format "posts/%s/%s/%s" (nth 0 elements) (nth 1 elements) (nth 2 elements))))

(defun extract-date-elements (buffer-name)
  (if (string-match-p "^20[[:digit:]]\\{6\\}$" (file-name-sans-extension buffer-name))
      (list
       (substring buffer-name 0 4)
       (substring buffer-name 4 6)
       (substring buffer-name 6 8))
    (error (format "Unable to extract date elements from \"%s\"" buffer-name))))

(defun extract-date (buffer-name)
  (let ((elements (extract-date-elements buffer-name)))
    (format "%s-%s-%s" (nth 0 elements) (nth 1 elements) (nth 2 elements))))

(defun get-ox-hugo-file-name (org-buffer-name)
  (concat (file-name-sans-extension org-buffer-name) ".ox-hugo.org"))

(defun slug-for-current-heading ()
  (let ((heading (nth 4 (org-heading-components))))
    (string-replace " " "-" (downcase heading))))

;;;; Private definitions orgox-sync-note-dir

(defun orgox--sync-note-dir (org-buffer ox-hugo-externals-dir)
  (let* ((note-dir-name (file-name-sans-extension (buffer-name org-buffer)))
         (note-dir (file-name-concat (f-dirname (buffer-file-name)) note-dir-name)))
    (when (f-directory-p note-dir)
      (orgox--one-way-sync note-dir ox-hugo-externals-dir))))

(defun orgox--one-way-sync (src-dir dest-dir)
  ;; make sure the directory target directory exists otherwise rsync will fail
  (f-mkdir-full-path dest-dir)
  (call-process "rsync" nil "orgox-process" nil "-Cavz" src-dir dest-dir))


;;; orgox.el ends here
