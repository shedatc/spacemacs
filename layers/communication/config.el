(setq smtpmail-smtp-server       "smtp.stormshield.eu"
      smtpmail-smtp-service      25
      message-send-mail-function 'message-send-mail-with-sendmail
      send-mail-function         'sendmail-send-it

      ;; DEBUG
      gnutls-log-level         0 ;; Because we use STARTTLS.
      )
