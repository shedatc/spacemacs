(spacemacs/declare-prefix "ot"  "org-todo")
(spacemacs/set-leader-keys
  "ott" 'sheda-org/switch-to-TODO-org-buffer
  "otA" 'sheda-org/switch-to-art-TODO-org-buffer
  "otB" 'sheda-org/switch-to-broodwar-TODO-org-buffer
  "otC" 'sheda-org/switch-to-codesonar-TODO-org-buffer
  "otF" 'sheda-org/switch-to-fwlimit-TODO-org-buffer
  "otH" 'sheda-org/switch-to-hacking-TODO-org-buffer
  "otI" 'sheda-org/switch-to-irp-TODO-org-buffer
  "otP" 'sheda-org/switch-to-packager-TODO-org-buffer
  "otU" 'sheda-org/switch-to-org-urgency-TODO-org-buffer
  )
