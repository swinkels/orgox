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

;;;; Public definitions

(defcustom orgox nil
  "Convert org-mode file to one suitable for publishing by ox-hugo."
  :group 'Tools)

(defcustom orgox-hugo-site-directory nil
  "Directory of ox-hugo site."
  :type 'directory :group 'orgox)

(defcustom orgox-base-url-for-note-asset nil
  "URL to use for .org note assets that should not be converted.

ox-hugo converts each link to an org-mode file as if it were a
note file that is included in the static site. For a link to an
org-mode file that is not a note but a note asset, this results
in a broken link. To avoid that, orgox replaces a link to an
org-mode asset to an URL to the asset in the online note repo."
  :type 'string :group 'orgox)

(defun orgox-publish-current-buffer ()
  (interactive)
  (let ((ox-hugo-file (orgox--export-to-note-file)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (org-hugo-export-wim-to-md :all-subtrees))))

(defun orgox-export-note-current-buffer ()
  (interactive)
  (orgox-export-note-buffer (current-buffer)))

(defun orgox-export-note-file (note-file)
  (interactive)
  (orgox-export-note-buffer (find-file-noselect note-file)))

(defun orgox-export-note-buffer (note-buffer)
  (interactive)

  (unless orgox-hugo-site-directory
    (error "Hugo site directory is not configured"))

  (unless (f-directory-p orgox-hugo-site-directory)
    (error (format "Hugo site directory %s does not exist" orgox-hugo-site-directory)))

  (let ((content-dir (orgox--get-ox-hugo-content-dir)))
    (when (f-exists-p content-dir)
      (unless (and (f-directory-p content-dir) (f-writable-p content-dir))
        (error
         (format
          "Hugo subdirectory for org-mode content %s should be a writable directory"
          content-dir)))))

  (unless org-hugo-default-static-subdirectory-for-externals
    (error "Hugo subdirectory for static externals is not configured"))

  (let ((static-externals-dir (orgox--get-ox-hugo-static-externals-dir)))
    (when (f-exists-p static-externals-dir)
      (unless (and (f-directory-p static-externals-dir) (f-writable-p static-externals-dir))
        (error
         (format
          "Hugo subdirectory for static externals %s should be a writable directory"
          static-externals-dir)))))

  (let ((note-buffer-name (buffer-name note-buffer)))
    (unless (orgox--extract-date-elements note-buffer-name)
      (error (format "Unable to extract date elements from \"%s\"" note-buffer-name))))

  ;; create the directories to hold the org-mode content and the static files
  (f-mkdir-full-path (orgox--get-ox-hugo-content-dir))
  (f-mkdir-full-path (orgox--get-ox-hugo-static-externals-dir))

  (orgox--export-note-buffer note-buffer))

;;;; Implementation orgox--export-note-buffer

(defun orgox--export-note-buffer (note-buffer)
  (with-current-buffer note-buffer
    (orgox--export-to-note-file)
    (orgox--sync-note-dir)))

;;;; Implementation orgox--export-to-note-file

(defun orgox--export-to-note-file ()
  (let* ((note-buffer (current-buffer))
         (note-buffer-name (buffer-name note-buffer))
         (date-elements (orgox--extract-date-elements note-buffer-name))
         (ox-hugo-file-name (orgox--get-ox-hugo-file-name note-buffer-name))
         (ox-hugo-file (f-join (orgox--get-ox-hugo-content-dir) ox-hugo-file-name)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (erase-buffer)
      (insert-buffer note-buffer)

      (beginning-of-buffer)
      (insert "#+HUGO_BASE_DIR: ../\n")
      (insert (format "#+HUGO_SECTION: %s\n" (orgox--as-date-sections date-elements)))
      (insert (format "#+HUGO_SLUG: %s\n\n"
                      (save-excursion
                        ;; let the first headline provide the title
                        (beginning-of-buffer)
                        (org-next-visible-heading 1)
                        (orgox--slug-for-current-heading))))

      ;; let the first headline provide the title
      (beginning-of-buffer)
      (org-next-visible-heading 1)
      (org-set-property "EXPORT_FILE_NAME" (orgox--as-hugo-filename date-elements))
      (org-set-property "EXPORT_DATE" (orgox--as-date date-elements))

      (orgox--update-local-links)

      (save-buffer))))

(defun orgox--get-ox-hugo-file-name (note-buffer-name)
  (concat (f-no-ext note-buffer-name) ".ox-hugo.org"))

(defun orgox--get-ox-hugo-file (note-buffer-name)
  (f-join orgox-hugo-site-directory
          "static"
          org-hugo-default-static-subdirectory-for-externals
          (orgox--get-ox-hugo-file-name)))

(defun orgox--extract-date-elements (buffer-name)
  (if (string-match-p "^20[[:digit:]]\\{6\\}$" (f-no-ext buffer-name))
      (list
       (substring buffer-name 0 4)
       (substring buffer-name 4 6)
       (substring buffer-name 6 8))
    nil))

(defun orgox--as-date-sections (date-elements)
  (format "%s/%s/%s/%s"
          org-hugo-section
          (nth 0 date-elements)
          (nth 1 date-elements)
          (nth 2 date-elements)))

(defun orgox--as-date (date-elements)
  (format "%s-%s-%s"
          (nth 0 date-elements)
          (nth 1 date-elements)
          (nth 2 date-elements)))

(defun orgox--as-hugo-filename (date-elements)
  (format "%s%s%s.md"
          (nth 0 date-elements)
          (nth 1 date-elements)
          (nth 2 date-elements)))

(defun orgox--update-local-links ()
  "Update links to other notes and note files.

ox-hugo automatically converts links to other files. This works
nicely for other notes, but not for links to note directories and
their contents. This function updates these links in the current
buffer so they work in the exported side.

This funcion finds each link that is formatted like

   [[<link target>]

and replace the link target by one that is suitable for export.
The replacement can be equal to the original link target if
ox-hugo supports the latter out-of-the-box. For details, see
function `orgox-convert-link'.

The current function works and modifies the current buffer. It
preserves the location of point."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\\[\\[\\([^]]*\\)]" nil t)
      (let* ((link (match-string 1))
             (new-link (save-match-data (orgox--convert-link link))))
        (replace-match new-link t t nil 1)))))

(defun orgox--convert-link (link)
  "Return a version of the given link that is suitable for export."
  (if (string-match "^\\(file:\\)?\\(../\\)?\\(20[[:digit:]]\\{6\\}\\)$" link)
      (let* ((name (match-string 3 link))
             (date-elements (orgox--extract-date-elements name)))
        (format "%s/%s/%s/%s"
                orgox-base-url-for-note-asset
                (nth 0 date-elements)
                (nth 1 date-elements)
                name))
    (if (string-match "^\\(file:\\)?\\(../[[:digit:]]\\{2\\}/\\)?\\(20[[:digit:]]\\{6\\}\\)$" link)
        (let* ((name (match-string 3 link))
               (date-elements (orgox--extract-date-elements name)))
          (format "%s/%s/%s/%s"
                  orgox-base-url-for-note-asset
                  (nth 0 date-elements)
                  (nth 1 date-elements)
                  name))
      (if (string-match "^\\(file:\\)?\\(../../[[:digit:]]\\{4\\}/[[:digit:]]\\{2\\}/\\)?\\(20[[:digit:]]\\{6\\}\\)$" link)
          (let* ((name (match-string 3 link))
                 (date-elements (orgox--extract-date-elements name)))
            (format "%s/%s/%s/%s"
                    orgox-base-url-for-note-asset
                    (nth 0 date-elements)
                    (nth 1 date-elements)
                    name))
        (if (string-match "^\\(file:\\)?\\(../\\)?\\(20[[:digit:]]\\{6\\}\\)/\\(.*\\)$" link)
            (let* ((note-dir-name (match-string 3 link))
                   (note-dir-file (match-string 4 link))
                   (date-elements (orgox--extract-date-elements note-dir-name)))
              (if (string= (f-ext link) "org")
                  (format "%s/%s/%s/%s/%s"
                          orgox-base-url-for-note-asset
                          (nth 0 date-elements)
                          (nth 1 date-elements)
                          note-dir-name
                          note-dir-file)
                (format "/%s/%s" note-dir-name note-dir-file)))
          link)))))

(defun orgox--extract-slug (note-file)
  (with-current-buffer (find-file-noselect note-file)
    (org-next-visible-heading 1)
    (orgox--slug-for-current-heading)))

(defun orgox--slug-for-current-heading ()
  (when (looking-at org-complex-heading-regexp)
    (let ((heading (nth 4 (org-heading-components))))
      (string-replace " " "-" (downcase heading)))))

;;;; Implementation orgox--sync-note-dir

(defun orgox--sync-note-dir ()
  (let* ((note-dir-name (f-no-ext (buffer-file-name)))
         (note-dir (f-join (f-dirname (buffer-file-name)) note-dir-name)))
    (when (f-directory-p note-dir)
      (orgox--one-way-sync note-dir (orgox--get-ox-hugo-static-externals-dir)))))

(defun orgox--one-way-sync (src-dir dest-dir)
  ;; make sure the directory target directory exists otherwise rsync will fail
  (f-mkdir-full-path dest-dir)
  (call-process "rsync" nil "orgox-process" nil "-Cavz" src-dir dest-dir))

(defun orgox--get-ox-hugo-static-externals-dir ()
  (f-join orgox-hugo-site-directory "static"))

;;;; Shared

(defun orgox--get-ox-hugo-content-dir ()
  (f-join orgox-hugo-site-directory "content-org"))

;;; orgox.el ends here
