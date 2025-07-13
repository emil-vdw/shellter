(package "shellter"
         "0.1.0"
         "Perspective-aware eshell session management")

(website-url "https://github.com/emil-vdw/shellter")
(keywords "convenience" "terminals" "perspective")

(package-file "shellter.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "26.1")
(depends-on "perspective" "2.8")

(development
 (depends-on "ert-runner")
 (depends-on "undercover")
 (depends-on "package-lint")
 (depends-on "elisp-lint"))

(setq network-security-level 'low)  ; For CI environments