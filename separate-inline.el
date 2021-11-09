;;; separate-inline.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(defcustom separate-inline-regexp-rules nil
  "List of rules with the form 
(TARGET-REGEXP APART-REGEXP . DELIMITER)."
  :type '(alist :key-type string :value-type string)
  :group 'format-inline)

(defvar separate-inline-mode-hook nil
  "Functions to be called for `separate-inline-mode'.")

(defun separate-inline-detect-newline-behavior (beg end len)
  "t if current change is newline"
  ;; \n ASCII eq 10
  (and (eq len 0) (not (eq beg end)) (eq (char-after beg) 10)))

(defun separate-inline-last-line (beg end len)
  (when (separate-inline-detect-newline-behavior beg end len)
    
    (save-excursion
      
      (goto-char beg)
      
      (save-restriction
        
        (narrow-to-region
         (line-beginning-position)
         (line-end-position))

        (mapc
         (lambda (r)

           (let ((rx-1
                  (concat
                   "\\("
                   (cadr r) "\\)\\("
                   (car r) "\\)\\("
                   (cadr r) "\\)"))
                 (rx-h
                  (concat
                   "\\(^"
                   (car r) "\\)\\("
                   (cadr r) "\\)"))
                 (rx-t
                  (concat
                   "\\("
                   (cadr r) "\\)\\("
                   (car r) "$\\)")))

             (goto-char (point-min))
             
             (while (search-forward-regexp rx-h nil t)

               (goto-char (match-end 1))
               (insert (cddr r)))

             (goto-char (point-min))
             
             (while (search-forward-regexp rx-t nil t)

               (goto-char (match-end 1))
               (insert (cddr r)))

             (goto-char (point-min))
             
             (while (search-forward-regexp rx-1 nil t)

               (goto-char (match-end 2))
               (insert (cddr r))
               (goto-char (match-beginning 2))
               (insert (cddr r)))
             ))

         separate-inline-regexp-rules)
        
        ))))

(defun separate-inline-use-default-rules-for-org-local ()
  "A tested rules for Chinese user to separate inline in org-mode.
org-mode 中文行内分隔规格"
  (make-local-variable 'separate-inline-regexp-rules)
  (setq separate-inline-regexp-rules
        '(("[\*\+\/\~\=\$\_]\\cc+[\*\+\/\~\=\$\_]"
           "\\cc" . "\x200B")
          ("[\*\+\/\~\=\$\_]*[0-9A-Za-z]+[\-0-9A-Za-z\*\+\/\~\=\$\_]*"
           "\\cc" . " "))))

;;;###autoload
(define-minor-mode separate-inline-mode
  "A minor mode that separate inline automatically."
  :init-value nil
  :global nil
  (if (not separate-inline-mode)
      (remove-hook 'after-change-functions 'separate-inline-last-line t)
    (progn
      (run-hooks 'separate-inline-mode-hooks)
      (add-hook 'after-change-functions 'separate-inline-last-line nil t))))

(provide 'separate-inline)
;;; separate-inline.el ends here
