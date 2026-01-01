;;; orgox-notes.el --- Export Org note buffer to one suited for ox-hugo -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pieter Swinkels

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

;; This package provides functions to convert the org-mode files that the author
;; uses to write their notes, to files that are suitable for publishing with
;; ox-hugo.

;;; Code:

(require 'f)
(require 'orgox)

(defun orgox-notes-export-to-ox-hugo-buffer (note-buffer ox-hugo-buffer ox-hugo-base-dir ox-hugo-file-name)
  "Export NOTE-BUFFER to OX-HUGO-BUFFER.
This function exports NOTE-BUFFER, which contains a note, to
OX-HUGO-BUFFER, which will contain that same note but suitable for
export using ox-hugo.  As OX-HUGO-BUFFER needs to configure the
directory of the Hugo site and the file for that buffer, these values
are passed in as OX-HUGO-BASE-DIR and OX-HUGO-FILE-NAME respectively."
  (let* ((note-buffer-name (buffer-name note-buffer))
         (date-elements (orgox-notes-extract-date-elements note-buffer-name)))
    (with-current-buffer ox-hugo-buffer
      (erase-buffer)
      (insert-buffer-substring note-buffer)

      (goto-char (point-min))
      (insert (format "#+HUGO_BASE_DIR: %s\n" ox-hugo-base-dir))
      (insert (format "#+HUGO_SECTION: %s\n" (orgox-notes-extract-section-string date-elements)))
      (insert (format "#+HUGO_SLUG: %s\n\n"
                      (save-excursion
                        ;; let the first headline provide the title
                        (goto-char (point-min))
                        (org-next-visible-heading 1)
                        (orgox-notes-extract-slug))))

      ;; let the first headline provide the title
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-set-property "EXPORT_FILE_NAME" ox-hugo-file-name)
      (org-set-property "EXPORT_DATE" (orgox-notes-extract-date date-elements))

      (orgox--update-local-links))))

(defun orgox-notes-extract-date-elements (buffer-name)
  "Return the date elements from BUFFER-NAME.
If BUFFER-NAME has the format \"YYYYMMDD.org\", the date elements is the
list of strings (YYYY MM DD).  If BUFFER-NAME has another format, this
function returns nil."
  (if (string-match-p "^20[[:digit:]]\\{6\\}$" (f-no-ext buffer-name))
      (list
       (substring buffer-name 0 4)
       (substring buffer-name 4 6)
       (substring buffer-name 6 8))
    nil))

(defun orgox-notes-extract-section-string (date-elements)
  "Return the Hugo section string for DATE-ELEMENTS.
The string returned has format `org-hugo-section' concatenated with
\"/YYYY/MM/DD\".  `org-hugo-section' is the default section for Hugo
posts."
  (format "%s/%s/%s/%s"
          org-hugo-section
          (nth 0 date-elements)
          (nth 1 date-elements)
          (nth 2 date-elements)))

(defun orgox-notes-extract-date (date-elements)
  "Return the date string for DATE-ELEMENTS.
The string returned has format \"YYYY-MM-DD\"."
  (format "%s-%s-%s"
          (nth 0 date-elements)
          (nth 1 date-elements)
          (nth 2 date-elements)))

(defun orgox-notes-extract-slug ()
  "Return the slug from the current headline.
The slug is created by converting the headline to lowercase and replacing
spaces with hyphens."
  (when (looking-at org-complex-heading-regexp)
    (let ((heading (nth 4 (org-heading-components))))
      (string-replace " " "-" (downcase heading)))))

(defun orgox-notes-update-local-links ()
  "Update links to other notes and note files.

ox-hugo automatically converts links to other files.  This works nicely
for other notes, but not for links to note directories and their
contents.  This function updates these links in the current buffer so
they work in the exported side.

This funcion finds each link that is formatted like

   [[<link target>]

and replace the link target by one that is suitable for export.
The replacement can be equal to the original link target if
ox-hugo supports the latter out-of-the-box.  For details, see
function `orgox-convert-link'.

The current function works and modifies the current buffer.  It
preserves the location of point."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]*\\)]" nil t)
      (let* ((link (match-string 1))
             (new-link (save-match-data (orgox-notes--convert-link link))))
        (replace-match new-link t t nil 1)))))

(defun orgox-notes--convert-link (link)
  "Return a version of LINK that is suitable for export.
If LINK is a link to a note asset that is an Org file or to a note
directory, this function returns the link to its counterpart in the
online note repository.  If LINK is a link to a note asset that is not
an Org file, it returns a link to the asset in the static part of the
site.  In all other cases this function returns LINK."
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

(provide 'orgox-notes)

;;; orgox-notes.el ends here
