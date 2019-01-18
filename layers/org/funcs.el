(defun sheda-org/switch-to-brain-buffer ()
  "Switch to the *org-brain* buffer."
  (interactive)
  (switch-to-buffer "*org-brain*"))

(defun sheda-org/branch-url (branch)
  "Return the URL to view the given BRANCH. Note that BRANCH is double-encoded."
  (format "https://review-sns.stormshield.eu/source/firmware/history/%s/" (url-hexify-string (url-hexify-string branch))))
