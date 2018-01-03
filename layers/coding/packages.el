(defconst sheda-coding-packages
  '(
    (astyle :location local)
    cc-mode
    cperl-mode
    flycheck-clang-analyzer
    helm-perldoc
    magit
    (perl-pod-preview :location local)
    (perltidy :location local)
    smart-tabs-mode
    )
)

(defun sheda-coding/init-astyle ()
  "Initialize the astyle package."
  (use-package astyle
    :config
    (setq-default astyle-program "~/bin/astyle")
    (add-hook 'c-mode-hook
              (lambda ()
                (spacemacs/set-leader-keys-for-major-mode 'c-mode
                  "t" 'astyle-region)))))

(defun sheda-coding/post-init-cc-mode ()
  "Post-initialize the cc-mode package."

  (defconst work-c-style
    ;; Always indent c/c++ sources, never insert tabs
    '(
      (c-tab-always-indent        . t)
      ;; Offset for line only comments
      ;;(c-comment-only-line-offset . 0)
    ;;; Controls the insertion of newlines before and after braces.
      ;; default is (before after)
      (c-hanging-braces-alist     . (
                                        ; if ( ) {  }
                                        ;        ^  ^
                                     (substatement-open  . (before after))
                                     (substatement-close . (before after))
                                        ; void foo ( ) { .. }
                                        ;              ^    ^
                                     (defun-open . (before after))
                                     (defun-close . (before after))
                                        ;  { .. }
                                        ;  ^    ^
                                     (block-open  . (before after))
                                     (block-close . (before after))
                                        ; static char gni[] = { .. }
                                        ;                     ^    ^
                                     (brace-list-open  . (before after))
                                     (brace-list-close . (before after))
                                        ; case label: { .. }
                                        ;             ^    ^
                                     (statement-case-open  . (before after))
                                     (statement-case-close . (before after))
                                        ; extern "C" { .. }
                                        ;            ^    ^
                                     (extern-lang-open  . (before after))
                                     (extern-lang-close . (before after))
                                     )
                                  )
    ;;; Controls the insertion of newlines before and after certain colons.
      (c-hanging-colons-alist     . ((member-init-intro before)
                                     (inher-intro)
                                     (case-label after)
                                     (label after)
                                     (access-label after)))
    ;;; List of various C/C++/ObjC constructs to "clean up".
      (c-cleanup-list             . (scope-operator))
    ;;; Association list of syntactic element symbols and indentation offsets.
                                        ; +   c-basic-offset times 1
                                        ; -   c-basic-offset times -1
                                        ; ++  c-basic-offset times 2
                                        ; --  c-basic-offset times -2
                                        ; *   c-basic-offset times 0.5
                                        ; /   c-basic-offset times -0.5
      (c-offsets-alist            . (
                                     (comment-intro     . 0)
                                     (statement         . 0)
                                     (substatement-open . 0)
                                     (case-label        . +)
                                     (block-open        . 0)
                                        ;(label             . 0)
                                     (arglist-cont-nonempty . +)
                                     (statement-case-intro . 0)
                                     )
                                  )
                                        ; (c-echo-syntactic-information-p . t)
      )
    "Work C/C++ indentation style")

  (defun sheda-coding/cc-mode-hook ()
    (c-set-style "work")

    ;; Labels are flush to the left
    (c-set-offset 'label [0])

    (setq fill-column            120
          c-backslash-max-column 120     ; max column for backslash in macros
          c-basic-offset         4
          tab-width              4
          indent-tabs-mode       t)
    )
  ;; (c-add-style "work" work-c-style)
  ;; (add-hook 'c++-mode-hook #'my-cc-mode-hook)
  ;; (add-hook 'c-mode-hook   #'my-cc-mode-hook)
  )

(defun sheda-coding/init-cperl-mode ()
  "Initialize the cperl-mode package."
  (use-package cperl-mode
    :defer t))

(defun sheda-coding/post-init-cperl-mode ()
  "Post-initialize the cperl-mode package."

  ;; Add the gtags key bindings to «<leader> g».
  (spacemacs|define-jump-handlers perl-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'perl-mode) ;; XXX Why perl-mode instead of cperl-mode?

  (setq flycheck-perl-perlcritic-executable "/usr/bin/vendor_perl/perlcritic"
        tab-width                           4)

  (add-hook 'perl-mode-hook (lambda ()
                              (setq indent-tabs-mode t))))

(defun sheda-coding/init-flycheck-clang-analyzer ()
  "Initialize the flycheck-clang-analyzer package."
  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup)))

(defun sheda-coding/init-helm-perldoc ()
  (use-package helm-perldoc
    :commands 'helm-perldoc
    :init
    (spacemacs/set-leader-keys-for-major-mode 'perl-mode ;; XXX Why perl-mode instead of cperl-mode?
      "hd" 'helm-perldoc)
    :config
    (helm-perldoc:setup)))

(defun sheda-coding/post-init-magit ()
  "Post-initialize the magit package."
  (setq magit-blame-heading-format "%s | %a | %C"
        magit-diff-refine-hunk     'all)

  ;; ‘magit-blame-mode-map and ~magit-blob-mode-map’, and in the popup
  ;;  ‘t’     (‘magit-blame-toggle-headings’)
  (add-hook 'magit-blame-mode-hook
            (lambda ()
              (define-key magit-blame-mode-map (kbd "T") 'magit-blame-toggle-headings)
              (define-key magit-blame-mode-map (kbd "t") 'evil-next-visual-line))))

(defun sheda-coding/init-perl-pod-preview ()
  "Initialize the perl-pod-preview package."
  (use-package perl-pod-preview
    :if (configuration-layer/package-usedp 'cperl-mode)
    :commands 'perl-pod-preview
    :init
    (spacemacs/set-leader-keys-for-major-mode 'perl-mode ;; XXX Why perl-mode instead of cperl-mode?
      "hf" 'perl-pod-preview)))

(defun sheda-coding/init-perltidy ()
  "Initialize the perltidy package."
  (use-package perltidy
    :if (configuration-layer/package-usedp 'cperl-mode)
    :commands 'perltidy-region
    :init
    (spacemacs/set-leader-keys-for-major-mode 'perl-mode ;; XXX Why perl-mode instead of cperl-mode?
      "t" 'perltidy-region)
    (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
      "t" 'perltidy-region)))

(defun sheda-coding/init-smart-tabs-mode ()
  "Initialize and configure the smart-tabs-mode package."
  (use-package smart-tabs-mode
    ;; :if (configuration-layer/package-usedp 'cperl-mode)
    :init
    (smart-tabs-insinuate 'cperl 'c)))
