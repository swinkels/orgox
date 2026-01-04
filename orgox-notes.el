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

;;;; Public API

(defun orgox-notes-export (note-buffer ox-hugo-buffer config)
  "Export NOTE-BUFFER to OX-HUGO-BUFFER.
This function exports NOTE-BUFFER, which contains a note, to
OX-HUGO-BUFFER, which will contain that same note but suitable for
export using ox-hugo.  CONFIG is a property list that contains
information that OX-HUGO-BUFFER needs to configure for Hugo:

:hugo-base-dir      Directory of the (local) Hugo site repository.
:markdown-file-name Name of the markdown file ox-hugo needs to generate.
:notes-url          URL to the online Git repository."
  (pcase-let (((map :hugo-base-dir :markdown-file-name :notes-url) config))

    (let* ((note-file-name
            (file-name-nondirectory (buffer-file-name note-buffer)))
           (date-elements (orgox-notes-extract-date-elements note-file-name)))

      (unless date-elements
        (error "cannot extract date from file name `%s'" note-file-name))

      (orgox-notes-export-to-ox-hugo-buffer note-buffer ox-hugo-buffer config)
      (orgox-notes-update-local-links ox-hugo-buffer notes-url)
      (orgox-notes-copy-note-directory-to-static note-buffer hugo-base-dir))))

;;;; Developer API

(defun orgox-notes-export-to-ox-hugo-buffer (note-buffer ox-hugo-buffer config)
  "Export NOTE-BUFFER to OX-HUGO-BUFFER.
This function exports NOTE-BUFFER, which contains a note, to
OX-HUGO-BUFFER, which will contain that same note but suitable for
export using ox-hugo.  CONFIG is a property list that contains
information that OX-HUGO-BUFFER needs to configure for Hugo:

:hugo-base-dir      Directory of the (local) Hugo site repository.
:markdown-file-name Name of the markdown file ox-hugo needs to generate.
:notes-url          URL to the online Git repository."
  (pcase-let (((map :hugo-base-dir :markdown-file-name) config))
    (let* ((note-buffer-name (buffer-name note-buffer))
           (date-elements (orgox-notes-extract-date-elements note-buffer-name)))
      (with-current-buffer ox-hugo-buffer
        (erase-buffer)
        (insert-buffer-substring note-buffer)

        (goto-char (point-min))
        (insert (format "#+HUGO_BASE_DIR: %s\n" hugo-base-dir))
        (insert (format "#+HUGO_SECTION: %s\n" (orgox-notes-extract-section-string date-elements)))
        (insert (format "#+HUGO_SLUG: %s\n\n" (orgox-notes--extract-slug)))

        ;; let the first headline provide the title
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-set-property "EXPORT_FILE_NAME" markdown-file-name)
        (org-set-property "EXPORT_DATE" (orgox-notes-extract-date date-elements))))))

(defun orgox-notes-update-local-links (ox-hugo-buffer notes-url)
  "Update links to other notes and note files.
Ox-hugo automatically converts links to other files.  This works nicely
for other notes, but not for links to note directories and note assets.
This function updates these links in the current buffer so they work in
the exported side.

This function goes over each string in the current buffer that matches
[[.*]] and when necessary, replaces the link target, so the text between
the inner square brackets, by one that is suitable for export by
ox-hugo.  This can mean that it replaces the target by the URL to its
counterpart in the online notes repository, hence the need for
NOTES-URL.

This function works and modifies the current buffer.  It preserves the
location of point."
  (with-current-buffer ox-hugo-buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]]*\\)]" nil t)
        (let* ((link (match-string 1))
               (new-link (save-match-data (orgox-notes--convert-link link notes-url))))
          (replace-match new-link t t nil 1))))))

(defun orgox-notes-copy-note-directory-to-static (note-buffer hugo-base-dir)
  "Copy the note directory of NOTE-BUFFER to the static directory.
Hugo makes the content of HUGO-BASE-DIR/static available as static
files.  In the generated site, each link to a note directory or a
non-Org note asset links to a directory or file in this folder.

If the given note does not have a note directory, this function does
nothing."
  (let ((note-dir (f-no-ext (buffer-file-name note-file))))
    (when (f-directory-p note-dir)
      (orgox-notes--one-way-sync note-dir (f-join hugo-base-dir "static")))))

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

;;;; Private functions

(defun orgox-notes--extract-slug ()
  "Return the slug from the first headline.
The slug is created by converting the headline to lowercase and replacing
spaces with hyphens."
  (save-excursion
    ;; let the first headline provide the title
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (when (looking-at org-complex-heading-regexp)
      (let ((heading (nth 4 (org-heading-components))))
        (string-replace " " "-" (downcase heading))))))

(defun orgox-notes--convert-link (link-target notes-url)
  "Return a version of LINK that is suitable for export.
If LINK-TARGET links to a note asset that is an Org file or to a note
directory, this function returns the link to its counterpart in the
online notes repository, hence the need for NOTES-URL.

If LINK-TARGET is a link-target to a note asset that is not an Org file,
it returns a link to the asset in the static part of the site.

In all other cases this function returns LINK."
  (if (string-match "^\\(file:\\)?\\(../\\)?\\(20[[:digit:]]\\{6\\}\\)$" link-target)
      (let* ((name (match-string 3 link-target))
             (date-elements (orgox--extract-date-elements name)))
        (format "%s/%s/%s/%s"
                notes-url
                (nth 0 date-elements)
                (nth 1 date-elements)
                name))
    (if (string-match "^\\(file:\\)?\\(../[[:digit:]]\\{2\\}/\\)?\\(20[[:digit:]]\\{6\\}\\)$" link-target)
        (let* ((name (match-string 3 link-target))
               (date-elements (orgox--extract-date-elements name)))
          (format "%s/%s/%s/%s"
                  notes-url
                  (nth 0 date-elements)
                  (nth 1 date-elements)
                  name))
      (if (string-match "^\\(file:\\)?\\(../../[[:digit:]]\\{4\\}/[[:digit:]]\\{2\\}/\\)?\\(20[[:digit:]]\\{6\\}\\)$" link-target)
          (let* ((name (match-string 3 link-target))
                 (date-elements (orgox--extract-date-elements name)))
            (format "%s/%s/%s/%s"
                    notes-url
                    (nth 0 date-elements)
                    (nth 1 date-elements)
                    name))
        (if (string-match "^\\(file:\\)?\\(../\\)?\\(20[[:digit:]]\\{6\\}\\)/\\(.*\\)$" link-target)
            (let* ((note-dir-name (match-string 3 link-target))
                   (note-dir-file (match-string 4 link-target))
                   (date-elements (orgox--extract-date-elements note-dir-name)))
              (if (string= (f-ext link-target) "org")
                  (format "%s/%s/%s/%s/%s"
                          notes-url
                          (nth 0 date-elements)
                          (nth 1 date-elements)
                          note-dir-name
                          note-dir-file)
                (format "/%s/%s" note-dir-name note-dir-file)))
          link-target)))))

(defun orgox-notes--one-way-sync (src-dir dest-dir)
  "Sync SRC-DIR to DEST-DIR using rsync.
Make sure the destination directory exists; rsync will fail otherwise."
  (f-mkdir-full-path dest-dir)
  (call-process "rsync" nil "orgox-process" nil "-Cavz" src-dir dest-dir))

(provide 'orgox-notes)

;;; orgox-notes.el ends here
