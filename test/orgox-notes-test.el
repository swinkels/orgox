;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'ox-hugo)

(require 'orgox-notes)

;; tests for orgox-notes-export-to-ox-hugo-buffer

(ert-deftest orgox-notes-end-to-end ()
  ;; The ox-hugo buffer that `orgox-notes-export-to-ox-hugo-buffer' populates
  ;; has the same content as the note buffer, but also includes the front matter
  ;; for the Hugo site and the Org properties for the headline of the note.
  (with-tmp-dir
   tmp-dir
   (let ((note-buffer (find-file (f-join tmp-dir  "20260101.org")))
         (ox-hugo-buffer (find-file (f-join tmp-dir "20260101.ox-hugo.org"))))

     (with-current-buffer note-buffer
       (insert "* This is the first headline"))

     (orgox-notes-export-to-ox-hugo-buffer note-buffer
                                           ox-hugo-buffer
                                           "/path/to/Hugo/site/repo"
                                           "20260101.md")

     (let ((expected-ox-hugo-buffer-content "\
#+HUGO_BASE_DIR: /path/to/Hugo/site/repo
#+HUGO_SECTION: posts/2026/01/01
#+HUGO_SLUG: this-is-the-first-headline

* This is the first headline
:PROPERTIES:
:EXPORT_FILE_NAME: 20260101.md
:EXPORT_DATE: 2026-01-01
:END:
"))

       (should (string= (get-buffer-content ox-hugo-buffer)
                        expected-ox-hugo-buffer-content))))))
