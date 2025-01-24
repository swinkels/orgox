;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'ox-hugo)
(require 'orgox)

(defvar test-directory (file-name-concat default-directory "test"))

(defun verify-error-string (buffer-name error-string)
  (let ((expected-error-string
         (format "Unable to extract date elements from \"%s\"" buffer-name)))
    (should (string= expected-error-string error-string))))

(ert-deftest test-extract-date()
  (should (string= (extract-date "20250111.org") "2025-01-11"))

  ;; the buffer name without extension should consist of 8 digits: if you pass
  ;; anything else, it will signal an error

  (dolist (buffer-name (list "abcdefgh.org" "*Messages*"))
    (let ((err  (should-error (extract-date buffer-name) :type 'error)))
      (verify-error-string buffer-name (error-message-string err)))))

(ert-deftest test-orgox-current-buffer-to-ox-hugo()
  (let ((org-file (file-name-concat test-directory "20250119.org"))
        (expected-ox-hugo-org-file (file-name-concat test-directory "20250119.ox-hugo.org"))
        (ox-hugo-org-file (file-name-concat temporary-file-directory "20250119.ox-hugo.org")))

    ;; delete any pre-existing output file - this is a very poor-mans solution
    ;; to a teardown phase
    (when (file-exists-p ox-hugo-org-file)
      (delete-file ox-hugo-org-file))

    (with-current-buffer (find-file-noselect org-file)
      (let ((orgox-content-org-directory temporary-file-directory))
        (orgox-current-buffer-to-ox-hugo)))

    (should (file-readable-p ox-hugo-org-file))
    (should (string= (f-read expected-ox-hugo-org-file) (f-read ox-hugo-org-file)))))

(ert-deftest test-orgox-sync-note-dir-to-ox-hugo()
  ;; delete any pre-existing output dir - this is a very poor-mans solution
  ;; to a teardown phase
  (when (file-directory-p (file-name-concat temporary-file-directory "static"))
    (delete-directory (file-name-concat temporary-file-directory "static") t))

  (let ((orgox-hugo-site-directory temporary-file-directory)
        (org-file (file-name-concat test-directory "20250119.org")))

    (with-current-buffer (find-file-noselect org-file)
      (orgox-sync-note-dir)))

  (let ((ox-hugo-note-dir
         (file-name-concat temporary-file-directory "static" "ox-hugo" "20250119")))

    (should (f-directory-p ox-hugo-note-dir))
    (should (f-exists-p (file-name-concat ox-hugo-note-dir "hello.txt")))))

(ert-deftest test-orgox-sync-note-dir-when-ox-hugo-site-dir-is-nil()
  (let ((orgox-hugo-site-directory nil)
        (expected-error-string
         "Hugo site directory is not configured (orgox-hugo-site-directory)"))

    (let ((err (should-error (orgox-sync-note-dir))))

      (should (string= expected-error-string (error-message-string err))))))

(ert-deftest test-orgox-sync-note-dir-when-ox-hugo-site-dir-does-not-exist()
  (let ((orgox-hugo-site-directory "/tmp/512e31e3-f774-4ac3-aaf6-65eb29ea7d47")
        (expected-error-string
         (concat
          "Hugo site directory /tmp/512e31e3-f774-4ac3-aaf6-65eb29ea7d47 does not "
          "exist (orgox-hugo-site-directory)")))

    (let ((err (should-error (orgox-sync-note-dir))))

      (should (string= expected-error-string (error-message-string err))))))

(ert-deftest test-orgox-sync-note-dir-that-does-not-exist()

  (let ((orgox-hugo-site-directory temporary-file-directory)
        (org-file (file-name-concat test-directory "20250124.org")))

    (cl-letf (((symbol-function 'one-way-sync-dir)
               (lambda () (error "one-way-sync-dir should not been called"))))

      (with-current-buffer (find-file-noselect org-file)
        (orgox-sync-note-dir))))

  (let ((ox-hugo-note-dir
         (file-name-concat temporary-file-directory "static" "ox-hugo" "20250124")))

    (should-not (f-directory-p ox-hugo-note-dir))))

;;; orgox-test.el ends here
