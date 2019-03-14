(setq smtpmail-smtp-server       "smtp.stormshield.eu"
      smtpmail-smtp-service      25
      message-send-mail-function 'message-send-mail-with-sendmail
      send-mail-function         'sendmail-send-it

      ;; XXX OpenPGP
      mml-secure-openpgp-encrypt-to-self  t
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-signers          (list (sheda-communication/openpgp-signer))

      ;; DEBUG
      gnutls-log-level         0 ;; Because we use STARTTLS.
      )
