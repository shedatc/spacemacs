;;; astyle.el --- Tidy C code

;;; Commentary:

;; Based on astyle.el 0.01 (22 Dec 2007)by Ye Wenbin <wenbinye@gmail.com>.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'astyle)

;;; Code:

(eval-when-compile
  (require 'cl))
(defvar astyle-program "astyle"
  "*Program name of astyle")
(defvar astyle-buffer-name "*astyle*"
  "*Name of the temporary astyle buffer.")
(defvar astyle-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'astyle-write)
    map)
  "*keymap for *astyle* buffer.")

(defvar astyle-last-buffer nil
  "Internal variable.")

(defmacro astyle-save-point (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-point (point)))
     ,@body
     (goto-char old-point)))

(defun astyle-region (beg end)
  "Tidy C code in the region."
  (interactive "r")
  (astyle-save-point
   (call-process-region beg end astyle-program t t)))

(defun astyle-buffer ()
  "Call astyle for whole buffer."
  (interactive)
  (astyle-region (point-min) (point-max)))

(defun astyle-subroutine ()
  "Call astyle for subroutine at point."
  (interactive)
  (astyle-region (progn (beginning-of-defun) (point))
                 (progn (end-of-defun) (point))))

;;;###autoload 
(defun astyle-dwim (arg)
  "Astyle Do What I Mean.
If with prefix argument, just show the result of astyle.
You can use C-x C-s to save the tidy result.
If region is active call astyle on the region. If inside
subroutine, call astyle on the subroutine, otherwise call
astyle for whole buffer."
  (interactive "P")
  (let ((buf (current-buffer))
        beg end)
    (cond ((and mark-active transient-mark-mode)
           (setq beg (region-beginning)
                 end (region-end)))
          ((save-excursion
             (beginning-of-defun)
             (when (looking-at "\\s-*sub\\s-+") ; XXX Need to be adapted.
               (setq beg (point)
                     end (progn (end-of-defun) (point))))))
          (t (setq beg (point-min)
                   end (point-max))))
    (when arg
      (set-buffer (get-buffer-create astyle-buffer-name))
      (erase-buffer)
      (insert (with-current-buffer buf
                (buffer-substring beg end)))
      (setq astyle-last-buffer (list buf beg end))
      (setq beg (point-min)
            end (point-max))
      (perl-mode)
      (use-local-map astyle-map)
      (pop-to-buffer (current-buffer))
      (message "Press C-x C-s to apply the tidy result."))
    (astyle-region beg end)))

(defun astyle-write ()
  (interactive)
  (if astyle-last-buffer
      (let ((buf (get-buffer astyle-buffer-name)))
        (if (buffer-live-p buf)
            (if (buffer-live-p (car astyle-last-buffer))
                (with-current-buffer (car astyle-last-buffer)
                  (goto-char (cadr astyle-last-buffer))
                  (astyle-save-point
                   (delete-region (cadr astyle-last-buffer)
                                  (nth 2 astyle-last-buffer)))
                  (insert-buffer-substring buf)
                  ;; don't write again
                  (setq astyle-last-buffer nil))
              (error "The astyle associated buffer does not exist!"))
          (error "No astyle buffer exists!")))
    (message "Do you forget to perform astyle or already write to the associated buffer")))

(provide 'astyle)
;;; astyle.el ends here
