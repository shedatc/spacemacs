;; Registers:
(spacemacs/set-leader-keys
  "rc"   'copy-to-register
  "rp"   'point-to-register)

;; Push:
(if (string= system-name "azathoth.stephaner.labo.int")
    (add-hook 'after-save-hook 'sheda-core/run-push))

(setq-default browse-url-browser-function   'browse-url-default-browser
              make-backup-file-name-function 'sheda-core/make-backup-file-name
              password-cache-expiry          nil)

(setq projectile-enable-caching t
      fsm-debug                 nil ;; No debug.
      vc-follow-symlinks        t   ;; VC follows the link and visits the real file, telling you about it in the echo area.
      )

;; Enable conf-unix-mode for *.conf, *rc and *.cmd.
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("rc\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'"  . conf-unix-mode))

;; Enable yaml-mode for .push/rc.
(add-to-list 'auto-mode-alist '("\\.push/rc\\'"  . yaml-mode))

;; Enable conf-unix-mode for file whose content start with a [.
(add-to-list 'magic-mode-alist '("\\["  . conf-unix-mode))

;; (spacemacs/toggle-aggressive-indent-on)
;; (spacemacs/toggle-automatic-symbol-highlight-on)
;;(spacemacs/toggle-centered-point-globally-on)
(blink-cursor-mode)
(global-prettify-symbols-mode)
