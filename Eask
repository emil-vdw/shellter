(workspace "shellter-monorepo"
           "0.1.0"
           "Shellter monorepo workspace")

(website-url "https://github.com/emil-vdw/shellter")

(source "gnu")
(source "melpa")

;; Define the packages in the workspace
(package "packages/shellter")
(package "packages/shellter-perspective")
(package "packages/shellter-consult")

(development
 (depends-on "ert-runner")
 (depends-on "undercover")
 (depends-on "package-lint")
 (depends-on "elisp-lint"))

(setq network-security-level 'low)  ; For CI environments