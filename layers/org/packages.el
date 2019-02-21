;; packages.el --- sheda-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Sheda
;;
;; Author: Sheda <sheda@fsfe.org>
;; URL: https://github.com/shedatc/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sheda-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sheda-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sheda-org/pre-init-PACKAGE' and/or
;;   `sheda-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sheda-org-packages
  '(
    emacsql
    emacsql-sqlite
    gnuplot-mode
    helm-org-rifle
    org
    org-brain
    (org-urgency :location local)
    )
  "The list of Lisp packages required by the sheda-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun sheda-org/init-emacsql ()
  "Intialize the emacsql package."
  (use-package emacsql
    ))

(defun sheda-org/init-emacsql-sqlite ()
  "Intialize the emacsql-sqlite package."
  (use-package emacsql-sqlite
    ))

(defun sheda-org/init-gnuplot-mode ()
  "Intialize the gnuplot-mode package."
  (use-package gnuplot-mode
    :commands 'gnuplot-mode
    :init
    ;; Reference: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((gnuplot . t)))
    ))

(defun sheda-org/init-helm-org-rifle ()
  "Intialize the helm-org-rifle package."
  (use-package helm-org-rifle
    :commands 'helm-org-rifle
    :init
    (spacemacs/set-leader-keys
      "oo" 'helm-org-rifle)))

(defun sheda-org/pre-init-org ()
  "Pre-initialize the org package (org-mode)."

  (setq org-agenda-files           (list my-org-directory my-org-agenda-directory)
        org-directory              my-org-directory
        org-agenda-span            15
        org-capture-templates
        '(;; Projects:
          ("C" "Code Sonar")
          ("Ct" "Code Sonar Task"      entry (file+headline     "proj/codesonar/TODO.org" "Code Sonar")       (file "tpl/task.org"))
          ("Cr" "Code Sonar Reference" entry (file+olp+datetree "proj/codesonar/references.org" "Code Sonar") (file "tpl/reference.org"))
          ("F" "FWLimit")
          ("Ft" "FWLimit Task"         entry (file+headline     "proj/fwlimit/TODO.org" "FWLimit")            (file "tpl/task.org"))
          ("Fr" "FWLimit Reference"    entry (file+olp+datetree "proj/fwlimit/references.org" "FWLimit")      (file "tpl/reference.org"))
          ;; Other:
          ("m" "Maybe Task" entry (file+olp+datetree "maybe.org" "Maybe")           (file "tpl/maybe-task.org"))
          ("c" "Contact"    entry (file+olp+datetree "contacts.org")                (file "tpl/contact.org"))
          ("r" "Reference"  entry (file+olp+datetree "references.org" "References") (file "tpl/reference.org"))
          ("s" "Secret"     entry (file+olp+datetree "secrets.org")                 (file "tpl/secret.org")))
        org-refile-targets
        '((nil              :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-export-with-smart-quotes t
        org-export-backends          '(ascii html icalendar latex md org)
        org-archive-location         "attic/%s_archive::"
        org-id-search-archives       nil
        org-tag-alist                '((:startgrouptag)
                                       ("Context")
                                       (:grouptags)
                                       (:startgroup)
                                       ("@freetime" . ?f)
                                       ("@home"     . ?h)
                                       ("@project"  . ?p)
                                       ("@work"     . ?w)
                                       (:endgroup)
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("When @work")
                                       (:grouptags)
                                       (:startgroup)
                                       ("dev"      . ?d)
                                       ("irp"      . ?i)
                                       ("packager" . ?P)
                                       ("qa"       . ?q)
                                       ("tac"      . ?t)
                                       (:endgroup)
                                       (:endgrouptag)

                                       ;; Often used:
                                       ("emacs"     . ?m)
                                       )
        org-link-abbrev-alist
        (list (cons "wikipedia"                     "https://en.wikipedia.org/wiki/%s")
              (cons "man"                           "http://www.freebsd.org/cgi/man.cgi?query=%s")
              (cons "file"                          "file:///home/stephaner/ens/files/%s") ;; XXX How should I invoke concat and expand-file-name to build it from user-home-directory?
              (cons "freebsd-architecture-handbook" "https://www.freebsd.org/doc/en_US.ISO8859-1/books/arch-handbook/$1.html")
              (cons "freebsd-handbook"              "https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/$1.html")
              (cons "freebsd-wiki"                  "https://wiki.freebsd.org/")
              (cons "fxr"                           "http://fxr.watson.org/fxr/source/")
              (cons "freebsd-pr"                    "https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=")
              (cons "pix"                           (concat "file://" (expand-file-name "ens/pix/%s" user-home-directory)))
              ;; Work-related:
              (cons "bug"       "https://mantis.stormshield.eu/view.php?id=%s")
              (cons "commit"    "https://review-sns.stormshield.eu/%s")
              ;; (cons "fw-branch" 'sheda-org/branch-url)
              (cons "review"    "https://review-sns.stormshield.eu/D%s")
              (cons "wiki"      "https://wiki.stormshield.eu/pmwiki_labo/index.php?n=%s")
              )
        ;; org-use-speed-commands      t
        org-export-initial-scope       'buffer
        org-enforce-todo-dependencies  t
        org-fontify-whole-heading-line t
        org-lowest-priority            ?D
        org-default-priority           org-lowest-priority

        ;; #+SEQ_TODO: TODO(t) IN-PROGRESS(p) UNDER-REVIEW(r) WAIT-NIGHTLY(n) BLOCKED(b) | DONE(d) CANCELLED(c)
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "UNDER-REVIEW(r)" "COMPILING(c)" "WAIT-NIGHTLY(n)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(C)"))

        org-table-separator-space " " ;; XXX Break tables alignment when set to a propertized value with (space :width 1).
        org-hide-block-startup    t
        )

  (setq sheda-org/per-tag-scores '(("next" . 15.0)
                                   ("emacs" . 0.1))
   )

  )

(defun sheda-org/post-init-org ()
  "Post-initialize the org package (org-mode)."
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "f"  'org-fill-paragraph ;; KEY OVERRIDE
    "St" 'org-move-subtree-down
    "Ss" 'org-move-subtree-up
    "u"  'org-toggle-link-display)
  (spacemacs/set-leader-keys
    "ol" 'org-open-at-point-global
    "oi" 'org-id-get-create)

  (evil-define-key 'normal evil-org-mode-map
    "T" 'evil-join) ;; XXX Instead of org-todo or org-insert-todo-heading.

  ;; (add-to-list 'org-entities-user
  ;;              '("unamusedface" nil nil "&x1F612;" nil nil nil))
  (setq org-entities-user nil)
  (add-to-list 'org-entities-user '("unamusedface" nil nil "&#128530;" nil nil nil))
  (add-to-list 'org-entities-user '("grinningface" nil nil "&#128513;" nil nil nil))
  (add-to-list 'org-entities-user '("coffee"       nil nil "&#9749;"   nil nil nil))

  ;; (add-hook 'org-mode-hook 'aggressive-indent-mode) ;; XXX Re-enable only when indent in #+BEGIN_SRC blocks is OK.
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Ensure new entries get an ID.
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

  ;; Use org structures and tables in message mode.
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  ;; (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  )

(defun sheda-org/init-org-brain ()
  "Initialize the org-brain package."
  (use-package org-brain
    :if (configuration-layer/package-usedp 'org)
    :commands (org-brain-add-child
               org-brain-add-parent
               org-brain-add-resource
               org-brain-paste-resource
               org-brain-remove-child
               org-brain-remove-parent
               org-brain-visualize
               org-brain-visualize-mode)
    :init
    (setq org-brain-path      my-org-directory
          org-brain-data-file (expand-file-name "org-brain/data.el" spacemacs-cache-directory) ;; XXX Why can't I use no-littering-var-directory here?
          org-brain-show-text t)
    (spacemacs/declare-prefix "ob"  "org-brain")
    (spacemacs/declare-prefix "obc" "child")
    (spacemacs/declare-prefix "obf" "friendship")
    (spacemacs/declare-prefix "obp" "parent")
    (spacemacs/declare-prefix "obr" "resource")
    (spacemacs/set-leader-keys
      ;; Applications:
      "ab"   'org-brain-visualize
      ;; Buffers:
      "obb"  'sheda-org/switch-to-brain-buffer
      ;; Children:
      "obca" 'org-brain-add-child
      "obcr" 'org-brain-remove-child
      ;; Friends:
      "obfa" 'org-brain-add-friendship
      "obfr" 'org-brain-remove-friendship
      ;; Parents:
      "obpa" 'org-brain-add-parent
      "obpr" 'org-brain-remove-parent
      ;; Resources:
      "obra" 'org-brain-add-resource
      "obrp" 'org-brain-paste-resource)
    :config
    (evilified-state-evilify org-brain-visualize-mode org-brain-visualize-mode-map)))

(defun sheda-org/init-org-urgency ()
  "Initialize the org-urgency package."
  (use-package org-urgency
    :if (configuration-layer/package-usedp 'org-brain)
    :commands (org-urgency/show-tasks-by-urgency)
    :init
    (setq org-agenda-custom-commands '(("u" "All TODOs sorted by urgency"
                                        alltodo ""
                                        ((org-agenda-cmp-user-defined 'org-urgency/cmp-urgencies)
                                         (org-agenda-sorting-strategy '(user-defined-up))))))
    (spacemacs/set-leader-keys "ou" 'org-urgency/show-tasks-by-urgency)))

;;; packages.el ends here
