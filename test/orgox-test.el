;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'ox-hugo)
(require 'orgox)

(defvar test-directory (file-name-concat default-directory "test"))

(defun verify-error-string (buffer-name error-string)
  (let ((expected-error-string
         (format "Unable to extract date elements from \"%s\"" buffer-name)))
    (should (string= expected-error-string error-string))))

(ert-deftest test-orgox--extract-date-elements-from-suitable-buffer-name()
  ;; return the "year", "month" and "day number" from a buffer name whose base
  ;; name consists of 8 digits
  (let ((date-elements (orgox--extract-date-elements "20250111.org")))
    (should (= 3 (length date-elements)))
    (should (string= "2025" (nth 0 date-elements)))
    (should (string= "01" (nth 1 date-elements)))
    (should (string= "11" (nth 2 date-elements)))))

(ert-deftest test-orgox--extract-date-elements-from-unsuitable-buffer-names()
  ;; return nil when the buffer name whose base name does not consist of 8
  ;; digits
  (dolist (unsuitable-buffer-name (list "abcdefgh.org" "*Messages*"))
    (should-not (orgox--extract-date-elements unsuitable-buffer-name))))

(ert-deftest test-orgox-export-note-buffer-for-undefined-hugo-site-directory()
  (let ((orgox-hugo-site-directory nil))

    (with-temp-buffer

      (let* ((err (should-error (orgox-export-note-buffer (current-buffer))))
             (err-message (error-message-string err)))

        (should (string=  "Hugo site directory is not configured" err-message))))))

(ert-deftest test-orgox--export-note-buffer-for-missing-hugo-site-directory()
  (let ((orgox-hugo-site-directory "/tmp/512e31e3"))

    (with-temp-buffer

      (let* ((err (should-error (orgox-export-note-buffer (current-buffer))))
             (err-message (error-message-string err)))

        (should
         (string= "Hugo site directory /tmp/512e31e3 does not exist" err-message))))))

;;;; Tests for orgox--export-to-note-file

(ert-deftest test-orgox--export-to-note-file()
  (let ((note-file (file-name-concat test-directory "20250119.org"))
        (ox-hugo-note-file (file-name-concat temporary-file-directory "content-org" "20250119.ox-hugo.org")))

    ;; delete any pre-existing output file - this is a very poor-mans solution
    ;; to a teardown phase
    (when (file-exists-p ox-hugo-note-file)
      (delete-file ox-hugo-note-file))

    (with-current-buffer (find-file-noselect note-file)
      (let ((orgox-hugo-site-directory temporary-file-directory))
        (orgox--export-to-note-file)))

    (should (file-readable-p ox-hugo-note-file))

    (let ((expected-ox-hugo-note-file (file-name-concat test-directory "20250119.ox-hugo.org")))

      (should (string= (f-read expected-ox-hugo-note-file) (f-read ox-hugo-note-file))))))

;;;; Tests for orgox--sync-note-dir

(ert-deftest test-orgox--sync-note-dir()
  ;; delete any pre-existing output dir - this is a very poor-mans solution
  ;; to a teardown phase
  (when (file-directory-p (file-name-concat temporary-file-directory "static"))
    (delete-directory (file-name-concat temporary-file-directory "static") t))

  (let ((orgox-hugo-site-directory temporary-file-directory)
        (org-file (file-name-concat test-directory "20250119.org")))

    (with-current-buffer (find-file-noselect org-file)
      (orgox--sync-note-dir)))

  (let ((ox-hugo-note-dir
         (file-name-concat temporary-file-directory "static" "ox-hugo" "20250119")))

    (should (f-directory-p ox-hugo-note-dir))
    (should (f-exists-p (file-name-concat ox-hugo-note-dir "hello.txt")))))

(ert-deftest test-orgox--sync-note-dir-that-does-not-exist()

  (let ((orgox-hugo-site-directory temporary-file-directory)
        (org-file (file-name-concat test-directory "20250124.org")))

    (cl-letf (((symbol-function 'one-way-sync-dir)
               (lambda () (error "one-way-sync-dir should not been called"))))

      (with-current-buffer (find-file-noselect org-file)
        (orgox--sync-note-dir))))

  (let ((ox-hugo-note-dir
         (file-name-concat temporary-file-directory "static" "ox-hugo" "20250124")))

    (should-not (f-directory-p ox-hugo-note-dir))))

;;;; Tests for orgox--update-local-links

(ert-deftest test-orgox--update-local-links()
  (let ((note-file (file-name-concat test-directory "20250125.org")))

    (with-current-buffer (find-file-noselect note-file)
      (orgox--update-local-links "20250125.org")

      (let ((expected-ox-hugo-org-file
             (file-name-concat test-directory "20250125.ox-hugo.org")))

        (should (string= (f-read expected-ox-hugo-org-file) (buffer-string)))))))

;;; orgox-test.el ends here
