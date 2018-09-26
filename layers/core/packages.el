;;; packages.el --- sheda-core layer packages file for Spacemacs.
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
;; added to `sheda-core-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sheda-core/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sheda-core/pre-init-PACKAGE' and/or
;;   `sheda-core/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sheda-core-packages
  '(
    atomic-chrome
    beacon
    doc-view
    evil
    ;; (flycheck-grammalecte :location local)
    helm
    ;; (i3 :location local)
    info+
    nyan-mode
    ))

(defun sheda-core/init-atomic-chrome ()
  "Initialize and configure the atomic-chrome package."
  (use-package atomic-chrome
    ;; :defer t
    :config
    (atomic-chrome-start-server)))

(defun sheda-core/init-beacon ()
  "Initialize and configure the beacon package."
  (use-package beacon
    ;; :defer t
    :config
    (setq beacon-color "#268bd2")
    (beacon-mode)))

(defun sheda-core/post-init-doc-view ()
  "Initialize and configure the doc-view package."
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (define-key doc-view-mode-map (kbd "t") 'doc-view-next-line)
              (define-key doc-view-mode-map (kbd "T") 'doc-view-next-page)
              (define-key doc-view-mode-map (kbd "s") 'doc-view-previous-line)
              (define-key doc-view-mode-map (kbd "S") 'doc-view-previous-page)))
  )

(defun sheda-core/post-init-evil ()
  "Post-initialize the evil package."
  (define-key evil-motion-state-map "'" 'evil-goto-mark)
  (define-key evil-motion-state-map "`" 'evil-goto-mark-line)
  (evil-leader/set-key "rv" 'evil-show-marks)) ;; POTENTIAL KEY OVERRIDE

(defun sheda-core/init-flycheck-grammalecte ()
  "Initialize the flycheck-grammalecte package."
  (use-package flycheck-grammalecte
    :load-path "~/ens/emacs/spacemacs-layers/sheda-200/el/flycheck-grammalecte"
    :init
    (sheda-core/message "event: init: flycheck-grammalecte")))

(defun sheda-core/post-init-helm ()
  "Post-initialize the helm package."
  (setq helm-adaptive-mode                         t
        helm-push-mark-mode                        t
        helm-net-prefer-curl                       t
        ;; helm-kill-ring-threshold                   1
        ;; helm-raise-command                         "wmctrl -xa %s"
        ;; helm-scroll-amount                         4
        ;; helm-idle-delay                            0.01
        ;; helm-input-idle-delay                      0.01
        ;; helm-ff-search-library-in-sexp             t
        ;; helm-default-external-file-browser         "thunar"
        ;; helm-pdfgrep-default-read-command          "evince --page-label=%p '%f'"
        ;; helm-ag-base-command                       "ack-grep --nocolor --nogroup"
        helm-ff-auto-update-initial-value          t
        ;; helm-grep-default-command                  "ack-grep -Hn --smart-case --no-group %e %p %f"
        ;; helm-grep-default-recurse-command          "ack-grep -H --smart-case --no-group %e %p %f"
        ;; helm-reuse-last-window-split-state         t
        ;; helm-always-two-windows                    t
        ;; helm-buffers-favorite-modes                (append helm-buffers-favorite-modes
        ;;                                                    '(picture-mode artist-mode))
        ;; helm-ls-git-status-command                 'magit-status-internal
        ;; helm-M-x-requires-pattern                  0
        ;; helm-dabbrev-cycle-threshold               5
        ;; helm-surfraw-duckduckgo-url                "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
        ;; helm-boring-file-regexp-list               '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
        ;; helm-buffer-skip-remote-checking           t
        helm-apropos-fuzzy-match                   t
        helm-M-x-fuzzy-match                       t
        helm-lisp-fuzzy-completion                 t
        helm-completion-in-region-fuzzy-match      t
        helm-move-to-line-cycle-in-source          t
        ido-use-virtual-buffers                    t             ; Needed in helm-buffers-list
        ;; helm-tramp-verbose                         6
        helm-buffers-fuzzy-matching                t
        ;; helm-locate-command                        "locate %s -e -A --regex %s"
        ;; helm-org-headings-fontify                  t
        ;; helm-autoresize-max-height                 80 ; it is %.
        ;; helm-autoresize-min-height                 20 ; it is %.
        ;; helm-buffers-to-resize-on-pa               '("*helm apropos*" "*helm ack-grep*"
        ;;                                              "*helm grep*" "*helm occur*" "*helm ag*"
        ;;                                              "*helm multi occur*" "*helm git-grep*"
        ;;                                              "*helm imenu*" "*helm imenu all*"
        ;;                                              "*helm gid*" "*helm semantic/imenu*")
        ;; fit-window-to-buffer-horizontally          1
        ;; helm-open-github-closed-issue-since        7
        ;; helm-search-suggest-action-wikipedia-url
        ;; "https://fr.wikipedia.org/wiki/Special:Search?search=%s"
        ;; helm-wikipedia-suggest-url
        ;; "https://fr.wikipedia.org/w/api.php?action=opensearch&search="
        ;; helm-wikipedia-summary-url
        ;; "https://fr.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page="
        )


  ;; Use the childframe if available.
  ;; (setq helm-display-function             'helm-display-buffer-in-own-frame
  ;;       helm-display-buffer-reuse-frame   t
  ;;       helm-use-undecorated-frame-option t)

  ;; Use ctsr bindings to navigate lines and sources.
  (add-hook 'helm-mode-hook
            (lambda ()
              (sheda-core/adjust-keys-for-helm helm-map)
              (with-eval-after-load 'helm-bookmark
                (sheda-core/adjust-keys-for-helm helm-bookmark-map)
                (sheda-core/adjust-keys-for-helm helm-bookmark-find-files-map))
              (with-eval-after-load 'helm-org-rifle
                (sheda-core/adjust-keys-for-helm helm-org-rifle-map)))))

(defun sheda-core/init-i3 ()
  "Initialize and configure the i3 package."

  (use-package i3
    ;; :defer t
    ;; :config
    ;; (beacon-mode)
    )

  (use-package i3-integration
    :after i3
    ;; :defer t
    ;; :config
    ;; (beacon-mode)
    )


  )

(defun sheda-core/post-init-info+ ()
  "Post-initialize the info+ package."
  (sheda-core/message "post-init: info+")
  (add-hook 'Info-mode-hook
            (lambda ()
              (define-key Info-mode-map (kbd "t") 'evil-next-line)
              (define-key Info-mode-map (kbd "s") 'evil-previous-line)))
  ;; XXX Unable to make it work :/
  ;; (kl|config info+
  ;;   :description
  ;;   "Remap `info+' (a.k.a., Info-mode) bindings."
  ;;   :loader
  ;;   (spacemacs|use-package-add-hook Info-mode :post-config BODY)
  ;;   :common
  ;;   (sheda-core/message "post-init: info+ / kl/correct-keys")
  ;;   (kl/correct-keys Info-mode-map
  ;;     "j"
  ;;     "k"))
  )

(defun sheda-core/init-nyan-mode ()
  "Initialize the nyan-mode package."
  (use-package nyan-mode
    :config
    (setq nyan-minimum-window-width 250)
    (nyan-mode)))
