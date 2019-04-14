(defun sheda-org/switch-to-TODO-org-buffer ()
  "Switch to the TODO.org buffer."
  (interactive)
  ;; (sheda-core/switch-to-file-buffer "org/TODO.org" my-org-directory)
  (let* ((buffer (get-buffer "TODO.org<org>")))
    (if (null buffer)
        (find-file (expand-file-name "TODO.org" my-org-directory))
      (switch-to-buffer buffer))))

(defun sheda-org/switch-to-project-TODO-org-buffer (project)
  "Switch to the TODO.org buffer of the given PROJECT."
  (interactive "sProject Short Name: ")
  (let* ((buffer (get-buffer (format "TODO.org<%s>" project))))
    (if (null buffer)
        (find-file (expand-file-name (format "proj/%s/TODO.org" project) my-org-directory))
      (switch-to-buffer buffer))))

(defun sheda-org/switch-to-art-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Art project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "art"))

(defun sheda-org/switch-to-broodwar-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Brood War project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "broodwar"))

(defun sheda-org/switch-to-codesonar-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Code Sonar project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "codesonar"))

(defun sheda-org/switch-to-fwlimit-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the FWLimit project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "fwlimit"))

(defun sheda-org/switch-to-hacking-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Hacking project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "hacking"))

(defun sheda-org/switch-to-irp-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the IRP project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "irp"))

(defun sheda-org/switch-to-packager-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Packager project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "packager"))

(defun sheda-org/switch-to-org-urgency-TODO-org-buffer ()
  "Switch to the TODO.org buffer of the Org Urgency project."
  (interactive)
  (sheda-org/switch-to-project-TODO-org-buffer "org-urgency"))

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
