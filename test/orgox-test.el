;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'ox-hugo)
(require 'orgox)

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
  (with-temp-buffer
    (let ((orgox-hugo-site-directory nil))
      (let* ((err (should-error (orgox-export-note-buffer (current-buffer))))
             (err-message (error-message-string err)))
        (should (string= "Hugo site directory is not configured" err-message))))))

(ert-deftest test-orgox--export-note-buffer-for-missing-hugo-site-directory()
  (with-temp-buffer
    (let ((orgox-hugo-site-directory "/tmp/512e31e3"))
      (let* ((err (should-error (orgox-export-note-buffer (current-buffer))))
             (err-message (error-message-string err)))
        (should
         (string= "Hugo site directory /tmp/512e31e3 does not exist" err-message))))))

;;;; Tests for orgox--export-to-note-file

(ert-deftest test-orgox--export-to-note-file()
  (let ((note-file (get-test-file "20250119.org"))
        (expected-ox-hugo-note-file (get-test-file "20250119.ox-hugo.org")))
    (with-current-buffer (find-file-noselect note-file)
      (with-temp-hugo-site
       (progn
         (orgox--export-to-note-file)
         (let ((ox-hugo-note-file (get-ox-hugo-site-file
                                   "content-org/20250119.ox-hugo.org")))
           (should (file-readable-p ox-hugo-note-file))
           (should (string= (f-read expected-ox-hugo-note-file)
                            (f-read ox-hugo-note-file)))))))))

(ert-deftest test-orgox--export-to-note-file-for-heading-at-first-line()
  (let ((note-file (get-test-file "20250204.org"))
        (expected-ox-hugo-note-file (get-test-file "20250204.ox-hugo.org")))
    (with-current-buffer (find-file-noselect note-file)
      (with-temp-hugo-site
       (progn
         (orgox--export-to-note-file)
         (let ((ox-hugo-note-file (get-ox-hugo-site-file
                                   "content-org/20250204.ox-hugo.org")))
           (should (file-readable-p ox-hugo-note-file))
           (should (string= (f-read expected-ox-hugo-note-file)
                            (f-read ox-hugo-note-file)))))))))

;;;; Tests for orgox--sync-note-dir

(ert-deftest test-orgox--sync-note-dir()
  (let ((note-file (get-test-file "20250119.org")))
    (with-current-buffer (find-file-noselect note-file)
      (with-temp-hugo-site
       (progn
         (orgox--sync-note-dir)
         (should (f-exists-p
                  (get-ox-hugo-site-file "static/ox-hugo/20250119/hello.txt"))))))))

(ert-deftest test-orgox--sync-note-dir-that-does-not-exist()
  (let ((note-file (get-test-file "20250124.org")))
    (with-current-buffer (find-file-noselect note-file)
      (with-temp-hugo-site
       (progn
         (cl-letf (((symbol-function 'one-way-sync-dir)
                    (lambda () (error "one-way-sync-dir should not been called"))))
           (orgox--sync-note-dir))
         (should-not (f-exists-p (get-ox-hugo-site-file "static/ox-hugo/20250124"))))))))

;;;; Tests for orgox--update-local-links

(ert-deftest test-orgox--update-local-links()
  (let ((note-file (get-test-file "20250125.org"))
        (expected-ox-hugo-org-file (get-test-file "20250125.ox-hugo.org")))
    (with-current-buffer (find-file-noselect note-file)
      (orgox--update-local-links "20250125.org")
      (should (string= (f-read expected-ox-hugo-org-file) (buffer-string))))))

(ert-deftest test-orgox--convert-file-reference-for-non-note-file()
  ;; The next check shows that a relative link to a file in the note directory
  ;; becomes an absolute link to a static file of the site. Other parts of orgox
  ;; copy these files so they end up as static files in the site.
  (should
   (string= (orgox--convert-file-reference "20250119/hello.txt" (get-test-file "."))
            "/20250119/hello.txt"))

  ;; The next check shows that a link to a non-note file is handled similarly.
  ;; As orgox does nothing with these files, the link will be broken in the
  ;; site.
  (should
   (string= (orgox--convert-file-reference "orgox-test.el" (get-test-file "."))
            "/orgox-test.el")))

(ert-deftest test-orgox--convert-file-reference-for-existing-note-file()
  (should
   (string= (orgox--convert-file-reference "20250207.org" (get-test-file "."))
            "/posts/2025/02/07/1st-heading.md")))

(ert-deftest test-orgox--convert-file-reference-for-existing-note-file-without-heading()
  (should
   (string= (orgox--convert-file-reference "20250208.org" (get-test-file "."))
            "/posts/2025/02/08/no-title.md")))

;;;; Support

(defun get-test-file (relative-file)
  (f-join (f-join default-directory "test" relative-file)))

(defun get-ox-hugo-site-file (relative-file)
  (f-join orgox-hugo-site-directory relative-file))

(defmacro with-temp-hugo-site (body)
  `(let* ((root-tmp-dir temporary-file-directory)
          (orgox-hugo-site-directory (f-join root-tmp-dir (make-temp-name "orgox-"))))

     ;; safeguard that we will be using a new directory
     (unless (not (f-exists-p orgox-hugo-site-directory))
       (error (format "Proposed Hugo site directory %s already exists"
                      orgox-hugo-site-directory)))
     (f-mkdir-full-path orgox-hugo-site-directory)
     (f-mkdir-full-path (f-join orgox-hugo-site-directory "content-org"))

     (unwind-protect
         ,body
       (progn

         ;; safeguard that we will be deleting a temporary directory
         (unless (f-descendant-of-p orgox-hugo-site-directory root-tmp-dir)
           (error (format
                   "Hugo site directory to delete %s does not seem a temporary directory"
                   orgox-hugo-site-directory)))

         (f-delete orgox-hugo-site-directory t)))))

(ert-deftest test-with-temp-hugo-site()
  (let ((temp-hugo-site-dir nil))
    (with-temp-hugo-site
     (progn
       (should (f-descendant-of-p orgox-hugo-site-directory temporary-file-directory))
       (should (f-directory-p orgox-hugo-site-directory))
       (should (f-writable-p orgox-hugo-site-directory))
       (should (f-directory-p (f-join orgox-hugo-site-directory "content-org")))
       (should (f-writable-p (f-join orgox-hugo-site-directory "content-org")))
       (setq temp-hugo-site-dir orgox-hugo-site-directory)))
    (should-not (f-exists-p temp-hugo-site-dir))))

(ert-deftest test-with-temp-hugo-site-deletes-dir-in-case-of-error()
  (let* ((err (should-error (error-from-with-temp-hugo-site)))
         (temp-hugo-site-dir (error-message-string err)))
    (should (f-descendant-of-p temp-hugo-site-dir temporary-file-directory))
    (should-not (f-exists-p temp-hugo-site-dir))))

(defun error-from-with-temp-hugo-site()
  (with-temp-hugo-site
   (progn
     (error orgox-hugo-site-directory))))

;;; orgox-test.el ends here
