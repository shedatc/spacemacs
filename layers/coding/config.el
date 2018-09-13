(setq tab-width       4
      compile-command "push -f"

      ;; C/C++
      flycheck-clang-include-path
      (list (expand-file-name "lab/sns/firmware/crossplatform/lib/"    user-home-directory)
            (expand-file-name "lab/sns/firmware/crossplatform/src/ASQ" user-home-directory)
            (expand-file-name "lab/sns/firmware/lib/"                  user-home-directory))

      ;; Perl
      flycheck-perl-perlcritic-executable "/usr/bin/vendor_perl/perlcritic"
      flycheck-perl-include-path (list (expand-file-name "lib/perl5"                       user-home-directory)
                                       (expand-file-name "lab/sns/qa/project/fwqualif/lib" user-home-directory)
                                       (expand-file-name "lab/sns/qacross/lib/libNetasq"   user-home-directory)
                                       (expand-file-name "lab/sns/qa/lib/libNetasq"        user-home-directory)
                                       (expand-file-name "lab/sns/qa/project/fwqualif"     user-home-directory)
                                       )

      ;; See also hl-todo-keyword-faces.
      hl-todo-regexp (rx bow
                         (or
                          "ASSERT"
                          "DEBUG"
                          "DONE"
                          "DONT"
                          "FAIL"
                          "FIXME"
                          "HACK"
                          "HOLD"
                          "KLUDGE"
                          "NEXT"
                          "NOTE"
                          "NOTREACHED"
                          "OKAY"
                          "PASS"
                          "PROG"
                          "THEM"
                          "TODO"
                          "XXX")
                         eow)
      )
