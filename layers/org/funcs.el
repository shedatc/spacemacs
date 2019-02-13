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
