;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'ox-hugo)

(require 'orgox-notes)

;; tests for orgox-notes-export-to-ox-hugo-buffer

(ert-deftest test-export-to-ox-hugo-buffer ()
  ;; Show that the ox-hugo buffer that `orgox-notes-export-to-ox-hugo-buffer'
  ;; populates has the same text as the note buffer but also includes the
  ;; front matter for the Hugo site and the Org properties for the headline of
  ;; the note.
  (with-tmp-dir
   tmp-dir
   (let ((note-buffer (find-file (f-join tmp-dir  "20260101.org")))
         (ox-hugo-buffer (find-file (f-join tmp-dir "20260101.ox-hugo.org"))))

     (with-current-buffer note-buffer
       (insert "* This is the first headline"))

     (orgox-notes-export-to-ox-hugo-buffer note-buffer
                                           ox-hugo-buffer
                                           (list :hugo-base-dir "/path/to/Hugo/site/repo"
                                                 :markdown-file-name "20260101.md"
                                                 :notes-url "https://notes/repo/tree/main"))

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

;; tests for orgox-notes-update-local-links

(ert-deftest test-update-of-local-links-to-other-notes()
  (with-tmp-dir
   tmp-dir
   (let ((ox-hugo-buffer (find-file (f-join tmp-dir "20260102.ox-hugo.org"))))

     (with-current-buffer ox-hugo-buffer
       (insert "\
Show how orgox-notes leaves Org links that contain a relative path to
what appears to be a note as-is:
- [[20250125.org]]
- [[file:20250125.org]]
- [[../2025/01/20250125.org]]
- [[file:../2025/01/20250125.org]]"))

     (orgox-notes-update-local-links ox-hugo-buffer "https://notes/repo/tree/main")

     (let ((expected-ox-hugo-buffer-content "\
Show how orgox-notes leaves Org links that contain a relative path to
what appears to be a note as-is:
- [[20250125.org]]
- [[file:20250125.org]]
- [[../2025/01/20250125.org]]
- [[file:../2025/01/20250125.org]]"))

       (should (string= (get-buffer-content ox-hugo-buffer)
                        expected-ox-hugo-buffer-content))))))

(ert-deftest test-update-of-local-links-to-note-directory()
  (with-tmp-dir
   tmp-dir
   (let ((ox-hugo-buffer (find-file (f-join tmp-dir "20260102.ox-hugo.org"))))

     (with-current-buffer ox-hugo-buffer
       (insert "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that isn't an Org file:
- [[20250125]]
- [[file:20250125]]
- [[../20250125]]
- [[file:../20250125]]"))

     (orgox-notes-update-local-links ox-hugo-buffer "https://notes/repo/tree/main")

     (let ((expected-ox-hugo-buffer-content "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that isn't an Org file:
- [[https://notes/repo/tree/main/2025/01/20250125]]
- [[https://notes/repo/tree/main/2025/01/20250125]]
- [[https://notes/repo/tree/main/2025/01/20250125]]
- [[https://notes/repo/tree/main/2025/01/20250125]]"))

       (should (string= (get-buffer-content ox-hugo-buffer)
                        expected-ox-hugo-buffer-content))))))

(ert-deftest test-update-of-local-links-to-non-Org-note-assets()
  (with-tmp-dir
   tmp-dir
   (let ((ox-hugo-buffer (find-file (f-join tmp-dir "20260102.ox-hugo.org"))))

     (with-current-buffer ox-hugo-buffer
       (insert "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that isn't an Org file:
- [[20250125/hello.txt]]
- [[file:20250125/hello.txt]]
- [[../20250125/hello.txt]]
- [[file:../20250125/hello.txt]]"))

     (orgox-notes-update-local-links ox-hugo-buffer "https://notes/repo/tree/main")

     (let ((expected-ox-hugo-buffer-content "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that isn't an Org file:
- [[/20250125/hello.txt]]
- [[/20250125/hello.txt]]
- [[/20250125/hello.txt]]
- [[/20250125/hello.txt]]"))

       (should (string= (get-buffer-content ox-hugo-buffer)
                        expected-ox-hugo-buffer-content))))))

(ert-deftest test-update-of-local-links-to-Org-note-assets()
  (with-tmp-dir
   tmp-dir
   (let ((ox-hugo-buffer (find-file (f-join tmp-dir "20260102.ox-hugo.org"))))

     (with-current-buffer ox-hugo-buffer
       (insert "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that is an Org file:
- [[20250125/hello.org]]
- [[file:20250125/hello.org]]
- [[../20250125/hello.org]]
- [[file:../20250125/hello.org]]"))

     (orgox-notes-update-local-links ox-hugo-buffer "https://notes/repo/tree/main")

     (let ((expected-ox-hugo-buffer-content "\
Show how orgox-notes updates Org links that contain a relative path to
what appears to be a note asset that is an Org file:
- [[https://notes/repo/tree/main/2025/01/20250125/hello.org]]
- [[https://notes/repo/tree/main/2025/01/20250125/hello.org]]
- [[https://notes/repo/tree/main/2025/01/20250125/hello.org]]
- [[https://notes/repo/tree/main/2025/01/20250125/hello.org]]"))

       (should (string= (get-buffer-content ox-hugo-buffer)
                        expected-ox-hugo-buffer-content))))))
