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
  (setq org-directory my-org-directory
        org-agenda-files
        (list my-org-directory
              (format "%s/agenda/"           my-org-directory)
              (format "%s/proj/art/"         my-org-directory)
              (format "%s/proj/broodwar/"    my-org-directory)
              (format "%s/proj/codesonar/"   my-org-directory)
              (format "%s/proj/fw/"          my-org-directory)
              (format "%s/proj/fwlimit/"     my-org-directory)
              (format "%s/proj/hacking/"     my-org-directory)
              (format "%s/proj/irp/"         my-org-directory)
              (format "%s/proj/org-urgency/" my-org-directory)
              (format "%s/proj/packager/"    my-org-directory)
              (format "%s/proj/wf/"          my-org-directory))
        org-capture-templates
        '(;; Projects:
          ("A" "Art")
          ("At" "Art Task"      entry (file+headline     "proj/art/TODO.org"       "Art Tasks") (file "tpl/task.org"))
          ("Ar" "Art Reference" entry (file+olp+datetree "proj/art/references.org" "Art References") (file "tpl/reference.org"))
          ("B" "Brood War")
          ("Bt" "Brood War Task"      entry (file+headline     "proj/broodwar/TODO.org"       "Brood War Tasks") (file "tpl/task.org"))
          ("Br" "Brood War Reference" entry (file+olp+datetree "proj/broodwar/references.org" "Brood War References") (file "tpl/reference.org"))
          ("C" "Code Sonar")
          ("Ct" "Code Sonar Task"      entry (file+headline     "proj/codesonar/TODO.org"       "Code Sonar Tasks") (file "tpl/task.org"))
          ("Cr" "Code Sonar Reference" entry (file+olp+datetree "proj/codesonar/references.org" "Code Sonar References") (file "tpl/reference.org"))
          ("F" "Firmware")
          ("Ft" "Firmware Task"      entry (file+headline     "proj/fw/TODO.org"       "Firmware Tasks") (file "tpl/task.org"))
          ("Fr" "Firmware Reference" entry (file+olp+datetree "proj/fw/references.org" "Firmware References") (file "tpl/reference.org"))
          ("H" "Hacking")
          ("Ht" "Hacking Task"      entry (file+olp          "proj/hacking/TODO.org"       "Hacking" "Misc") (file "tpl/task.org"))
          ("Hr" "Hacking Reference" entry (file+olp+datetree "proj/hacking/references.org" "Hacking") (file "tpl/reference.org"))
          ("I" "IRP")
          ("It" "IRP Task"      entry (file+headline     "proj/irp/TODO.org"       "IRP Tasks") (file "tpl/task.org"))
          ("Ir" "IRP Reference" entry (file+olp+datetree "proj/irp/references.org" "IRP References") (file "tpl/reference.org"))
          ("L" "FWLimit")
          ("Lt" "FWLimit Task"      entry (file+headline     "proj/fwlimit/TODO.org"       "FWLimit Tasks") (file "tpl/task.org"))
          ("Lr" "FWLimit Reference" entry (file+olp+datetree "proj/fwlimit/references.org" "FWLimit References") (file "tpl/reference.org"))
          ("P" "Packager")
          ("Pt" "Packager Task"      entry (file+headline     "proj/packager/TODO.org"       "Packager Tasks")      (file "tpl/task.org"))
          ("Pr" "Packager Reference" entry (file+olp+datetree "proj/packager/references.org" "Packager References") (file "tpl/reference.org"))
          ("U" "Org Urgency")
          ("Ut" "Org Urgency Task"      entry (file+headline     "proj/org-urgency/TODO.org"       "Org Urgency Tasks") (file "tpl/task.org"))
          ("Ur" "Org Urgency Reference" entry (file+olp+datetree "proj/org-urgency/references.org" "Org Urgency References") (file "tpl/reference.org"))
          ("W" "Weapon Factory")
          ("Wt" "Weapon Factory Task"      entry (file+headline     "proj/wf/TODO.org"       "Weapon Factory Tasks") (file "tpl/task.org"))
          ("Wr" "Weapon Factory Reference" entry (file+olp+datetree "proj/wf/references.org" "Weapon Factory References") (file "tpl/reference.org"))
          ;; Other:
          ("T" "Task"       entry (file+headline "TODO.org" "Tasks")                (file "tpl/task.org"))
          ("t" "Maybe Task" entry (file+headline "TODO.org" "Maybe")                (file "tpl/maybe-task.org"))
          ("r" "Reference"  entry (file+olp+datetree "references.org" "References") (file "tpl/reference.org"))
          ("c" "Contact"    entry (file+olp+datetree "contacts.org" "Contacts")     (file "tpl/contact.org"))
          ("s" "Secret"     entry (file+olp+datetree "secrets.org")                 (file "tpl/secret.org")))
        org-tag-alist
        '((:startgrouptag)
          ("Context")
          (:grouptags)
          (:startgroup)
          ("@freetime" . ?f)
          ("@home"     . ?h)
          ("@work"     . ?w)
          (:endgroup)
          (:endgrouptag)

          (:startgrouptag)
          ("Project")
          (:grouptags)
          (:startgroup)
          ("Art"            . ?A)
          ("BroodWar"       . ?B)
          ("CodeSonar"      . ?C)
          ("Firmware"       . ?F)
          ("FWLimit"        . ?L)
          ("Hacking"        . ?H)
          ("IRP"            . ?I)
          ("OrgUrgency"     . ?U)
          ("Packager"       . ?P)
          ("Weapon Factory" . ?W)
          (:endgroup)
          (:endgrouptag)

          ;; Often used:
          ("Emacs"   . ?m)
          ("FreeBSD" . ?b)
          ("Org"     . ?o)
          ("mu4e"    . ?u))
        org-agenda-span 15
        org-refile-targets
        '((nil              :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-export-with-smart-quotes t
        org-export-backends          '(ascii html icalendar latex md org)
        org-archive-location         "attic/%s_archive::"
        org-id-search-archives       nil
        org-link-abbrev-alist
        (list (cons "wikipedia" "https://en.wikipedia.org/wiki/%s")
              (cons "man"       "http://www.freebsd.org/cgi/man.cgi?query=%s")
              (cons "f"         "file://~/ens/f/%s")
              ;; Work-related:
              (cons "bug"       "https://mantis.stormshield.eu/view.php?id=%s")
              (cons "commit"    "https://review-sns.stormshield.eu/%s")
              (cons "review"    "https://review-sns.stormshield.eu/D%s")
              (cons "wiki"      "https://wiki.stormshield.eu/pmwiki_labo/index.php?n=%s"))
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

  (setq sheda-org/per-tag-scores '(("Emacs" . 0.1)
                                   ("next" . 15.0)
                                   ("Org" . 0.1)))
  )

(defun sheda-org/post-init-org ()
  "Post-initialize the org package (org-mode)."
  (require 'org-tempo) ;; See https://orgmode.org/manual/Structure-Templates.html.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "f"  'org-fill-paragraph ;; KEY OVERRIDE
    "St" 'org-move-subtree-down
    "Ss" 'org-move-subtree-up
    "u"  'org-toggle-link-display)
  (spacemacs/set-leader-keys
    "ol" 'org-open-at-point-global
    "oi" 'org-id-get-create)

  (evil-define-key 'normal evil-org-mode-map
    "T" 'evil-join) ;; Instead of org-todo or org-insert-todo-heading.

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
