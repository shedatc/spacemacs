;; (defun sheda-coding/load-commit-template-into-current-buffer ()
;;   "Load the commit template into the current buffer overwriting its content."
;;   (interactive)
;;   (erase-buffer)
;;   (insert-file-contents "~/.commit-template.html"))

;; (defun sheda-coding/org-link-tag-to-code-view-url (tag)
;;   "Return the URL used to display the target code. The tag syntax is an follow (ABNF):
;; REPOSITORY , PATH [ , REVISION ]

;; E.g.,
;;  [[code:devel|trunk/project/vpackager/sbin/loop_pipeline.sh@83272][loop_pipeline.sh]]
;; "
;;   (let* ((tokens     (split-string tag "," t)))
;;     (if (< (length tokens) 2)
;;         (error "invalid tag syntax")
;;       (let* ((repository (elt tokens 0))
;;              (path       (elt tokens 1))
;;              (url        (concat "https://labo-sns.stormshield.eu/viewvc/viewvc.py/"
;;                                  repository
;;                                  "/"
;;                                  path
;;                                  "?view=markup")))
;;         (if (eq (length tokens) 3)
;;             (let* ((revision (elt tokens 2)))
;;               (concat url "&revision=" revision))
;;           url)))))

(defun sheda-coding/cc-mode-hook ()
  (c-set-style "work")

  ;; Labels are flush to the left
  (c-set-offset 'label [0])

  (setq fill-column            120
        c-backslash-max-column 120     ; max column for backslash in macros
        c-basic-offset         4
        tab-width              4
        indent-tabs-mode       t))
