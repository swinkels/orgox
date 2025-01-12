;;; orgox.el --- Convert org-mode file to one suited for ox-hugo  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pieter Swinkels

;; Author: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; Maintainer: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; URL:
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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

;; Happy coding! ;)

(provide 'orgox)

(defcustom orgox nil
  "Convert org-mode file to one suitable for publishing by ox-hugo."
  :group 'Tools)

(defcustom orgox-content-org-directory nil
  "Directory of ox-hugo site to hold org-files to publish."
  :type 'directory :group 'orgox)

(defun demote-subtree-at-line (line-number)
  (goto-line line-number)
  (org-demote-subtree))

(defun slug-for-current-heading ()
  (let ((heading (nth 4 (org-heading-components))))
    (string-replace " " "-" (downcase heading))))

(defun extract-date (buffer-name)
  (if (string-match-p "^20[[:digit:]]\\{6\\}$" (file-name-sans-extension buffer-name))
      (concat
       (substring buffer-name 0 4)
       "-"
       (substring buffer-name 4 6)
       "-"
       (substring buffer-name 6 8))
    "Unable to extract date"))

(defun get-ox-hugo-file-name (org-buffer-name)
  (concat (file-name-sans-extension org-buffer-name) ".ox-hugo.org"))

(defun write-as-ox-hugo-buffer (org-buffer ox-hugo-buffer)
  (with-current-buffer ox-hugo-buffer
    (erase-buffer)
    (insert-buffer org-buffer)
    (beginning-of-buffer)
    (insert "#+HUGO_BASE_DIR: ../")
    (newline 2)

    ;; let the first headline provide the title
    (org-next-visible-heading 1)
    (org-set-property "EXPORT_FILE_NAME" (slug-for-current-heading))
    (org-set-property "EXPORT_DATE" (extract-date (buffer-name org-buffer)))

    ;; make the rest of the buffer a subtree of the first headline
    ;;
    ;; To do so, we demote the headlines that are at the same level as the first
    ;; headline. Of course this only works if they are at the same level or
    ;; lower.
    ;; (let ((line-numbers))
    ;;   (while (org-get-next-sibling)
    ;;     (add-to-list 'line-numbers (line-number-at-pos) t))
    ;;   (mapc 'demote-subtree-at-line line-numbers))))
    ))

(defun write-as-ox-hugo-file (org-buffer dest-dir)
  (let* ((ox-hugo-file-name (get-ox-hugo-file-name (buffer-name org-buffer)))
         (ox-hugo-file (file-name-concat dest-dir ox-hugo-file-name)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (write-as-ox-hugo-buffer org-buffer (current-buffer))
      (save-buffer))
    ox-hugo-file))

(defun orgox-current-buffer-to-ox-hugo ()
  (interactive)
  (write-as-ox-hugo-file (current-buffer) orgox-content-org-directory))

(defun orgox-publish-current-buffer ()
  (interactive)
  (let ((ox-hugo-file (orgox-current-buffer-to-ox-hugo)))
    (with-current-buffer (find-file-noselect ox-hugo-file)
      (org-hugo-export-wim-to-md :all-subtrees))))

(with-current-buffer (get-buffer "20241224.org")
  (orgox-publish-current-buffer))

;;; orgox.el ends here
