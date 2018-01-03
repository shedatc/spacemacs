(configuration-layer/declare-layer 'auto-completion)
(setq
 auto-completion-enable-snippets-in-popup   t
 auto-completion-enable-sort-by-usage       t
 auto-completion-enable-help-tooltip        nil
 auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
 )

(configuration-layer/declare-layer 'c-c++)

(configuration-layer/declare-layer 'emacs-lisp)

;; (configuration-layer/declare-layer 'go)

(configuration-layer/declare-layer 'git)
(setq git-gutter-use-fringe t)

(configuration-layer/declare-layer 'gtags)

(configuration-layer/declare-layer 'haskell)

(configuration-layer/declare-layer 'semantic)

(configuration-layer/declare-layer 'sheda-core)

;; (configuration-layer/declare-layer 'shell-scripts)

(configuration-layer/declare-layer 'syntax-checking)
(setq syntax-checking-enable-by-default t)

(configuration-layer/declare-layer 'version-control)
