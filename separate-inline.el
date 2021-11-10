;;; separate-inline.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:
(defvar separate-inline-only-by-newline nil
  " Default nil, separate-inline once cursor leave current line.
If no nil, separate-inline only after newline (press enter key).")

(defcustom separate-inline-regexp-rules nil
  "List of rules with the form 
(TARGET-REGEXP APART-REGEXP . DELIMITER)."
  :type '(alist :key-type string :value-type string)
  :group 'separate-inline)

(defvar separate-inline-mode-hook nil
  "Functions to be called for `separate-inline-mode'.")

(defvar separate-inline-last-beg nil
  "Holds the last line-beginning-position")

(defvar separate-inline-need -1
  "To control separate-inline")

(defun separate-inline-detect-newline-behavior (beg end len)
  "t if current change is newline"
  ;; \n ASCII eq 10
  (and (eq len 0) (not (eq beg end)) (eq (char-after beg) 10)))

(defun separate-inline-update-cursor-beg (beg)
  ;; update var only for local buffer
  (unless separate-inline-last-beg
    (make-local-variable 'separate-inline-last-beg))
  (or (eq beg separate-inline-last-beg)
      (setq separate-inline-last-beg beg)))

(defun separate-inline-detect-change (beg end len)
  "Run at after-change-functions to update `separate-inline-need'"
  
  (separate-inline-update-cursor-beg
   (line-beginning-position)) ;recode current change pos
  
  (when (eq -1 separate-inline-need)
    (make-local-variable 'separate-inline-need)
    (setq separate-inline-need t))
  (or separate-inline-need
      (setq separate-inline-need t)))

(defun separate-inline-meet-need ()
  "Run after `separate-inline-update'"
  (when (eq -1 separate-inline-need)
    (make-local-variable 'separate-inline-need)
    (setq separate-inline-need nil))
  (and separate-inline-need
       (setq separate-inline-need nil)))

(defun separate-inline-update (beg)
  "Separating targe line's inline format.
By given rules of `separate-inline-regexp-rules'"
  (save-excursion        
    (goto-char beg)      
    
    (save-restriction
      (narrow-to-region beg (line-end-position))
      
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
       
       separate-inline-regexp-rules)))
  (separate-inline-meet-need))

(defun separate-inline-last-line-by-newline (beg end len)
  "Trigger inline update by newline behavior"
  (save-excursion
    (goto-char beg)
    (and (separate-inline-detect-newline-behavior beg end len)
         (separate-inline-update (line-beginning-position)))))

(defun separate-inline-last-line-by-leaveline ()
  "Trigger inline update if cursor leave current line"
  (and separate-inline-need separate-inline-last-beg
       ;; last not in the same line
       (not (eq separate-inline-last-beg (line-beginning-position)))
       ;; then do inline update
       (separate-inline-update separate-inline-last-beg)))

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
      (progn
        (remove-hook 'after-change-functions
                     'separate-inline-last-line-by-newline t)
        (remove-hook 'after-change-functions
                     'separate-inline-detect-change t)
        (remove-hook 'post-command-hook
                     'separate-inline-last-line-by-leaveline t))
    (progn
      (run-hooks 'separate-inline-mode-hook)
      (if separate-inline-only-by-newline
          (add-hook 'after-change-functions
                    'separate-inline-last-line-by-newline nil t)
        (progn
          (add-hook 'after-change-functions
                    'separate-inline-detect-change nil t)
          (add-hook 'post-command-hook
                    'separate-inline-last-line-by-leaveline nil t))))))

(provide 'separate-inline)
;;; separate-inline.el ends here
