;;; -*- mode: emacs-lisp -*-

(package "orgox"
         "0.1.0"
         "Convert org-mode file to one suited for ox-hugo")

(website-url "https://github.com/swinkels/orgox")
(keywords "tools" "ox-hugo")

(package-file "orgox.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa-stable")

(depends-on "emacs" "26.3")
;; Elpa stores multiple versions of a package but requries a manual download if
;; you want anything but the latest version. Melpa Stable only stores the latest
;; tagged version and Melpa only the latest committed version. So even if you
;; pass a version number to `depends-on', you only get the latest.
(depends-on "f")
(depends-on "ox-hugo")
