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

(defvar separate-inline-last-pos nil
  "Holds the postion from the last leave line run of post-command-hooks.")

(defvar separate-inline-need -1
  "To control separate-inline")

(defun separate-inline-detect-newline-behavior (beg end len)
  "t if current change is newline"
  ;; \n ASCII eq 10
  (and (eq len 0) (not (eq beg end)) (eq (char-after beg) 10)))

(defun separate-inline-detect-cursor-leaveline-behavior ()
  ;; update var only for local buffer
  (unless separate-inline-last-pos
    (make-local-variable 'separate-inline-last-pos)
    (setq separate-inline-last-pos (point))
    )
  (let ((last nil))
    (unless (eq (line-number-at-pos)
                (line-number-at-pos separate-inline-last-pos))
      (setq last separate-inline-last-pos)
      (setq separate-inline-last-pos (point)))    
    last))

(defun separate-inline-detect-change (beg end len)
  "Run at after-change-functions to update `separate-inline-needed'"
  (unless (eq -1 separate-inline-need)
    (make-local-variable 'separate-inline-need)
    (setq separate-inline-need t))
  (unless separate-inline-need
    (setq separate-inline-need t)))

(defun separate-inline-meet-need ()
  "Run after `separate-inline-update'"
  (unless (eq -1 separate-inline-need)
    (make-local-variable 'separate-inline-need)
    (setq separate-inline-need nil))
  (when separate-inline-need
    (setq separate-inline-need nil)))

(defun separate-inline-update (POS)
  "Separating targe line's inline format.
By given rules of `separate-inline-regexp-rules'"
  (save-excursion    
    
    (goto-char POS)      
    
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
       
       separate-inline-regexp-rules)))
  (separate-inline-meet-need))

(defun separate-inline-last-line-by-newline (beg end len)
  "Trigger inline update by newline behavior"
  (when (separate-inline-detect-newline-behavior beg end len)
    (separate-inline-update beg)))

(defun separate-inline-last-line-by-leaveline ()
  "Trigger inline update if cursor leave current line"
  (when separate-inline-need
    (let ((pos (separate-inline-detect-cursor-leaveline-behavior)))
      (when pos
        (separate-inline-update pos)))))

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
