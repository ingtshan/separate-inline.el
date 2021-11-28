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

(defvar separate-inline--last-beg nil
  "To control separate-inline, 
When inline need ,it holds the last line-beginning-position. 
if nil, it tells to init in this buffer.
if -1, it tells last need is meet
if -2, it tells separate-inline-update is running")

(defun separate-inline-detect-newline-behavior (beg end len)
  "t if current change is newline"
  ;; \n ASCII eq 10
  (and (eq len 0) (not (eq beg end)) (eq (char-after beg) 10)))

(defun separate-inline-change-state (beg)
  ;; update var only for local buffer
  (unless separate-inline--last-beg
    (make-local-variable 'separate-inline--last-beg))
  (or (eq beg separate-inline--last-beg)
      (setq separate-inline--last-beg beg)))

(defun separate-inline-detect-change (beg end len)
  "Run at after-change-functions"

  (unless (eq -2 separate-inline--last-beg) ;refuse -2
    (if (separate-inline-detect-newline-behavior beg end len)
        (save-excursion
          (goto-char beg)
          (separate-inline-change-state (line-beginning-position)))
      (separate-inline-change-state (line-beginning-position)))))

(defun separate-inline-detect-only-newline (beg end len)
  "Run at after-change-functions"

  (unless (eq -2 separate-inline--last-beg) ;refuse -2
    (when (separate-inline-detect-newline-behavior beg end len)
      (save-excursion
        (goto-char beg)
        (separate-inline-change-state (line-beginning-position))))))

(defun separate-inline-update (beg)
  "Separating targe line's inline format.
By given rules of `separate-inline-regexp-rules'"
  ;; tell others next change is me
  (separate-inline-change-state -2)
  (save-excursion        
    (goto-char beg)      
    
    ;; (save-restriction
    ;;   (narrow-to-region (point-min) (line-end-position))
    (let ((bound (line-end-position)))
      
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
                 "\\("
                 (car r) "\\)\\("
                 (cadr r) "\\)"))
               (rx-t
                (concat
                 "\\("
                 (cadr r) "\\)\\("
                 (car r) "\\)")))

           ;; (goto-char beg)

           (while (search-forward-regexp rx-1 bound t)

             (goto-char (match-end 2))
             (insert (cddr r))
             (goto-char (match-beginning 2))
             (insert (cddr r))
             (setq bound (+ 2 bound)))

           (goto-char beg)
           
           (while (search-forward-regexp rx-h bound t)

             (goto-char (match-end 1))
             (insert (cddr r))
             (setq bound (1+ bound)))

           (goto-char beg)
           
           (while (search-forward-regexp rx-t bound t)

             (goto-char (match-end 1))
             (insert (cddr r))
             (setq bound (1+ bound)))
           
           ))
       
       separate-inline-regexp-rules)))
  ;; tell others the need is meet
  (separate-inline-change-state -1))

(defun separate-inline-update-last-line ()
  "Trigger inline update if cursor leave current line"
  (and separate-inline--last-beg
       ;; last not in the same line
       (not (eq separate-inline--last-beg (line-beginning-position)))
       ;; then do inline update
       (separate-inline-update separate-inline--last-beg)))

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
                     'separate-inline-detect-only-newline t)
        (remove-hook 'after-change-functions
                     'separate-inline-detect-change t)
        (remove-hook 'post-command-hook
                     'separate-inline-update-last-line t))
    (progn
      (run-hooks 'separate-inline-mode-hook)
      (if separate-inline-only-by-newline
          (add-hook 'after-change-functions
                    'separate-inline-detect-only-newline nil t)        
        (add-hook 'after-change-functions
                  'separate-inline-detect-change nil t))
      (add-hook 'post-command-hook
                'separate-inline-update-last-line nil t))))

(provide 'separate-inline)
;;; separate-inline.el ends here
