;;; packages.el --- sheda-org layer packages file for Spacemacs.
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
    helm-org-rifle
    org
    org-brain
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

(defun sheda-org/init-helm-org-rifle ()
  "Intialize the helm-org-rifle package."
  (use-package helm-org-rifle
    :commands 'helm-org-rifle
    :init
    (spacemacs/set-leader-keys
      "oo" 'helm-org-rifle-agenda-files)))

(defun sheda-org/pre-init-org ()
  "Pre-initialize the org package (org-mode)."

  (setq org-agenda-files           (list my-org-directory my-org-agenda-directory)
        org-directory              my-org-directory
        org-agenda-span            15
        org-capture-templates
        '(
          ("j" "Journal Entry" entry
           (file+datetree "journal.org") (file "journal-entry.org-template"))
          ("c" "Contact"       entry
           (file+datetree "journal.org") (file "contact.org-template"))
          ("s" "Secret"       entry
           (file+datetree "secret.org") (file "secret.org-template"))
          )
        org-export-with-smart-quotes t
        org-export-backends          '(ascii html icalendar latex md org)
        org-archive-location         "attic/%s_archive::"
        org-id-search-archives       nil
        org-tag-alist                '((:startgrouptag)
                                       ("Context")
                                       (:grouptags)
                                       (:startgroup)
                                       ("@ens"  . ?e)
                                       ("@home" . ?h)
                                       ("@work" . ?w)
                                       (:endgroup)
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("When @work")
                                       (:grouptags)
                                       (:startgroup)
                                       ("irp"      . ?i)
                                       ("packager" . ?p)
                                       ("qa"       . ?q)
                                       ("tac"      . ?t)
                                       (:endgroup)
                                       (:endgrouptag)

                                       ;; Often used:
                                       ("contact"   . ?c)
                                       ("emacs"     . ?m)
                                       ("reference" . ?r)
                                       ("secret"    . ?s)
                                       )
        org-link-abbrev-alist
        '(("wikipedia"                     . "https://en.wikipedia.org/wiki/%s")
          ("man"                           . "http://www.freebsd.org/cgi/man.cgi?query=%s")
          ("file"                          . "file:///home/stephaner/ens/files/%s") ;; XXX How should I invoke concat and expand-file-name to build it from user-home-directory?
          ("freebsd-architecture-handbook" . "https://www.freebsd.org/doc/en_US.ISO8859-1/books/arch-handbook/$1.html")
          ("freebsd-handbook"              . "https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/$1.html")
          ("freebsd-wiki"                  . "https://wiki.freebsd.org/")
          ("fxr"                           . "http://fxr.watson.org/fxr/source/")
          ("freebsd-pr"                    . "https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=")
          ;; Work-related:
          ("bug"    . "https://mantis.stormshield.eu/view.php?id=%s")
          ("review" . "https://labo-sns.stormshield.eu/reviewboard/r/%s")
          ("wiki"   . "https://wiki.stormshield.eu/pmwiki_labo/index.php?n=%s")
          )
        ;; org-use-speed-commands      t
        org-export-initial-scope       'subtree
        org-enforce-todo-dependencies  t
        org-fontify-whole-heading-line t
        org-lowest-priority            ?D
        org-default-priority           org-lowest-priority

        ;; #+SEQ_TODO: TODO(t) IN-PROGRESS(p) UNDER-REVIEW(r) WAIT-NIGHTLY(n) | DONE(d) CANCELLED(c)
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "UNDER-REVIEW(r)" "WAIT-NIGHTLY(n)" "|" "DONE(d)" "CANCELLED(c)"))


        org-agenda-custom-commands '(("u" "All TODOs sorted by urgency"
                                            alltodo ""
                                            ((org-agenda-cmp-user-defined 'sheda-org/cmp-urgencies)
                                             (org-agenda-sorting-strategy '(user-defined-up)))))
        sheda-org/per-tag-scores '(("next" . 15.0)
                                   ("emacs" . 0.5))
        ))

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

  ;; (add-hook 'org-mode-hook 'aggressive-indent-mode) ;; XXX Re-enable only when indent in #+BEGIN_SRC blocks is OK.

  ;; Ensure new entries get an ID.
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

  ;; Use org structures and tables in message mode.
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
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
    ;; :defer t
    :init
    (setq org-brain-path      my-org-directory
          org-brain-data-file (expand-file-name "org-brain/data.el" spacemacs-cache-directory)) ;; XXX Why can't I use no-littering-var-directory here?
    (spacemacs/declare-prefix "ob"  "org-brain")
    (spacemacs/declare-prefix "obc" "child")
    (spacemacs/declare-prefix "obf" "friendship")
    (spacemacs/declare-prefix "obp" "parent")
    (spacemacs/declare-prefix "obr" "resource")
    (spacemacs/set-leader-keys
      "ab"   'org-brain-visualize
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
      "obrp" 'org-brain-paste-resource
      )
    ;; (eval-after-load 'evil
    ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
    :config
    (evilified-state-evilify org-brain-visualize-mode org-brain-visualize-mode-map)
  ))

;;; packages.el ends here
