(setq
 tab-width 4
 compile-command "push -f"
 flycheck-clang-include-path
 (list
  (expand-file-name "lab/git/firmware/crossplatform/lib/"    user-home-directory)
  (expand-file-name "lab/git/firmware/crossplatform/src/ASQ" user-home-directory)
  (expand-file-name "lab/git/firmware/lib/"                  user-home-directory)
  ))
