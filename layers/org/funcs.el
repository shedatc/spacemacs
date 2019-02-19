(defun sheda-core/switch-to-work-org-buffer ()
  "Switch to the work.org buffer."
  (interactive)
  (sheda-core/switch-to-file-buffer "work.org" my-org-directory))

(defun sheda-core/switch-to-tac-org-buffer ()
  "Switch to the tac.org buffer."
  (interactive)
  (sheda-core/switch-to-file-buffer "tac.org" my-org-directory))

(defun sheda-core/switch-to-qa-org-buffer ()
  "Switch to the qa.org buffer."
  (interactive)
  (sheda-core/switch-to-file-buffer "qa.org" my-org-directory))

(defun sheda-org/switch-to-brain-buffer ()
  "Switch to the *org-brain* buffer."
  (interactive)
  (switch-to-buffer "*org-brain*"))

(defun sheda-org/branch-url (branch)
  "Return the URL to view the given BRANCH. Note that BRANCH is double-encoded."
  (format "https://review-sns.stormshield.eu/source/firmware/history/%s/" (url-hexify-string (url-hexify-string branch))))

(defun sheda-org/add-current-buffer-to-org-layout ()
  "Add the current buffer to the @Org layout."
  (let* ((b (current-buffer))
         (p (persp-get-by-name "@Org")))
    (when p
      (sheda-core/message "Adding buffer %S to layout %S..." b p)
      (persp-add-buffer b p))))

(defun sheda-org/add-all-org-buffers-to-org-layout ()
  "Look for any Org buffer and add it to the @Org layout."
  (let* ((p (persp-get-by-name "@Org")))
    (when p (mapcar (lambda (b)
                      "If buffer is an Org buffer, add it to the @Org layout."
                      (with-current-buffer b
                        (when (and (string= major-mode "org-mode")
                                   (not (persp-contain-buffer-p b p)))
                          (persp-add-buffer b p))))
                    (buffer-list)))))
