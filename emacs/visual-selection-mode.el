;;; visual-selection-mode.el --- C-SPC Visual Mode for Emacs -*- lexical-binding: t; -*-

;; Makes C-SPC act like "v" in Vim with transient visual selection mode
;; Usage: C-SPC i( = select inside parentheses, C-SPC a( = select around parentheses
;; Supports: ()[]{}\"'<> with both opening and closing characters
;; C-SPC C-SPC = normal mark set/deactivate behavior

;;; Configuration
(defvar my/visual-mode-active nil
  "Whether visual selection mode is currently active.")

(defvar my/debug-mode nil
  "Enable debug messages for troubleshooting.")

(defvar my/last-command-was-enhanced-set-mark nil
  "Track if the last command was our enhanced set-mark-command.")

;;; Debug helpers
(defun my/debug (format-string &rest args)
  "Print debug message if debug mode is enabled."
  (when my/debug-mode
    (apply 'message (concat "[DEBUG] " format-string) args)))

(defun my/toggle-debug ()
  "Toggle debug mode on/off."
  (interactive)
  (setq my/debug-mode (not my/debug-mode))
  (message "Debug mode %s" (if my/debug-mode "enabled" "disabled")))

(defun my/show-buffer-info ()
  "Show debug info about current buffer position."
  (interactive)
  (let ((p (point)))
    (message "Point: %d, Char: %c, Before: %c, Line: %d, Col: %d" 
             p 
             (or (char-after) ?\ )
             (or (char-before) ?\ )
             (line-number-at-pos)
             (current-column))))

;;; Core bracket matching
(defun my/find-matching-paren (open-char close-char)
  "Find the innermost matching parentheses/brackets containing point.
Returns a cons cell (start . end) or nil if not found."
  (save-excursion
    (let ((orig-point (point))
          (best-start nil)
          (best-end nil)
          (best-distance nil))
      
      (my/debug "Searching for %c%c from point %d" open-char close-char orig-point)
      
      ;; Find all pairs of matching parentheses that contain our position
      (goto-char (point-min))
      (while (search-forward (char-to-string open-char) nil t)
        (let ((start-pos (1- (point))))
          (save-excursion
            ;; For each opening paren, find its matching closing paren
            (goto-char start-pos)
            (let ((paren-count 1)
                  (end-pos nil))
              (while (and (> paren-count 0) (< (point) (point-max)))
                (forward-char)
                (let ((current-char (char-after)))
                  (when current-char
                    (cond
                     ((eq current-char open-char)
                      (setq paren-count (1+ paren-count)))
                     ((eq current-char close-char)
                      (setq paren-count (1- paren-count))
                      (when (= paren-count 0)
                        (setq end-pos (point))))))))
              
              ;; Check if this pair contains our cursor and is better than previous
              (when (and end-pos 
                         (> orig-point start-pos)
                         (< orig-point end-pos))
                (let ((distance (- end-pos start-pos)))
                  (my/debug "Found containing pair: (%d,%d) distance=%d" start-pos end-pos distance)
                  (when (or (not best-distance) (< distance best-distance))
                    (setq best-start start-pos
                          best-end end-pos
                          best-distance distance)
                    (my/debug "New best pair: (%d,%d)" best-start best-end))))))))
      
      (my/debug "Final result: start=%s, end=%s" best-start best-end)
      (when (and best-start best-end)
        (cons best-start best-end)))))

;;; Quote matching
(defun my/find-matching-quotes (quote-char)
  "Find matching quotes around point."
  (save-excursion
    (let ((start-pos nil)
          (end-pos nil)
          (orig-point (point)))
      (my/debug "Looking for quotes %c from point %d" quote-char orig-point)
      
      ;; Search backward for opening quote
      (while (and (not start-pos) (> (point) (point-min)))
        (backward-char)
        (when (= (char-after) quote-char)
          (setq start-pos (point))
          (my/debug "Found opening quote at %d" start-pos)))
      
      ;; Search forward for closing quote
      (when start-pos
        (goto-char (1+ start-pos))
        (while (and (not end-pos) (< (point) (point-max)))
          (when (= (char-after) quote-char)
            (setq end-pos (1+ (point)))
            (my/debug "Found closing quote at %d" (1- end-pos)))
          (forward-char)))
      
      (my/debug "Quote search result: start=%s, end=%s" start-pos end-pos)
      (when (and start-pos end-pos)
        (cons start-pos end-pos)))))

;;; Selection functions
(defun my/mark-inside-parens (open-char close-char)
  "Mark inside parentheses/brackets (excluding the delimiters)."
  (my/debug "mark-inside-parens called with %c%c" open-char close-char)
  (let ((bounds (my/find-matching-paren open-char close-char)))
    (if bounds
        (let* ((start (car bounds))
               (end (cdr bounds))
               (inside-start (1+ start))
               (inside-end (1- end)))
          (my/debug "Marking inside from %d to %d" inside-start inside-end)
          (goto-char inside-start)
          (set-mark (1+ inside-end))
          (setq mark-active t)
          (my/exit-visual-mode)
          (message "Marked inside %c%c: '%s'" open-char close-char 
                  (buffer-substring inside-start (1+ inside-end))))
      (my/exit-visual-mode)
      (message "No matching %c%c found" open-char close-char))))

(defun my/mark-around-parens (open-char close-char)
  "Mark around parentheses/brackets (including the delimiters)."
  (my/debug "mark-around-parens called with %c%c" open-char close-char)
  (let ((bounds (my/find-matching-paren open-char close-char)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          (my/debug "Marking around from %d to %d" start end)
          (goto-char start)
          (set-mark (1+ end))
          (setq mark-active t)
          (my/exit-visual-mode)
          (message "Marked around %c%c: '%s'" open-char close-char 
                  (buffer-substring start (1+ end))))
      (my/exit-visual-mode)
      (message "No matching %c%c found" open-char close-char))))

(defun my/mark-inside-quotes (quote-char)
  "Mark inside quotes."
  (let ((bounds (my/find-matching-quotes quote-char)))
    (if bounds
        (let* ((start (car bounds))
               (end (cdr bounds))
               (inside-start (1+ start))
               (inside-end (1- end)))  ; end is already after the closing quote
          (goto-char inside-start)
          (set-mark inside-end)  ; Don't add 1 here since end is already past the quote
          (setq mark-active t)
          (my/exit-visual-mode)
          (message "Marked inside %c: '%s'" quote-char 
                  (buffer-substring inside-start inside-end)))
      (my/exit-visual-mode)
      (message "No matching %c found" quote-char))))

(defun my/mark-around-quotes (quote-char)
  "Mark around quotes."
  (let ((bounds (my/find-matching-quotes quote-char)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          (goto-char start)
          (set-mark end)
          (setq mark-active t)
          (my/exit-visual-mode)
          (message "Marked around %c: '%s'" quote-char 
                  (buffer-substring start end)))
      (my/exit-visual-mode)
      (message "No matching %c found" quote-char))))

;;; Visual mode control
(defun my/enter-visual-mode ()
  "Enter visual selection mode."
  (setq my/visual-mode-active t)
  (my/debug "Entered visual mode at point %d" (point))
  ;; Set the mark when entering visual mode, just like normal C-SPC
  (set-mark-command nil)
  (message "Visual mode: i(inside) a(around) + ()[]{}\"'<> | Any other key exits"))

(defun my/exit-visual-mode ()
  "Exit visual selection mode."
  (setq my/visual-mode-active nil)
  (setq my/last-command-was-enhanced-set-mark nil)
  (my/debug "Exited visual mode"))

(defun my/handle-unknown-key (key1 key2)
  "Handle unknown key sequence in visual mode."
  (my/debug "Unknown key sequence, exiting visual mode")
  (setq my/last-command-was-enhanced-set-mark nil)
  (my/exit-visual-mode)
  (set-mark-command nil)
  (setq unread-command-events (list key1 key2)))

;;; Key dispatch
(defun my/visual-mode-dispatch ()
  "Handle key input in visual mode."
  (let ((key1 (read-key)))
    (my/debug "First key in visual mode: %s" 
             (if (characterp key1) (char-to-string key1) "special-key"))
    (cond
     ;; Handle C-SPC pressed again
     ((eq key1 67108896)
      (my/debug "Detected second C-SPC, deactivating mark")
      (setq my/last-command-was-enhanced-set-mark nil)
      (my/exit-visual-mode)
      (when mark-active
        (deactivate-mark)
        (message "Mark deactivated")))
     
     ;; Inside selections
     ((and (characterp key1) (= key1 ?i))
      (let ((key2 (read-key)))
        (my/debug "Second key for inside: %s" 
                 (if (characterp key2) (char-to-string key2) "special-key"))
        (if (characterp key2)
            (cond
             ((or (= key2 ?\() (= key2 ?\))) (my/mark-inside-parens ?\( ?\)))
             ((or (= key2 ?\[) (= key2 ?\])) (my/mark-inside-parens ?\[ ?\]))
             ((or (= key2 ?\{) (= key2 ?\})) (my/mark-inside-parens ?\{ ?\}))
             ((or (= key2 ?\<) (= key2 ?\>)) (my/mark-inside-parens ?\< ?\>))
             ((= key2 ?\") (my/mark-inside-quotes ?\"))
             ((= key2 ?\') (my/mark-inside-quotes ?\'))
             (t (my/handle-unknown-key key1 key2)))
          (my/handle-unknown-key key1 key2))))
     
     ;; Around selections
     ((and (characterp key1) (= key1 ?a))
      (let ((key2 (read-key)))
        (my/debug "Second key for around: %s" 
                 (if (characterp key2) (char-to-string key2) "special-key"))
        (if (characterp key2)
            (cond
             ((or (= key2 ?\() (= key2 ?\))) (my/mark-around-parens ?\( ?\)))
             ((or (= key2 ?\[) (= key2 ?\])) (my/mark-around-parens ?\[ ?\]))
             ((or (= key2 ?\{) (= key2 ?\})) (my/mark-around-parens ?\{ ?\}))
             ((or (= key2 ?\<) (= key2 ?\>)) (my/mark-around-parens ?\< ?\>))
             ((= key2 ?\") (my/mark-around-quotes ?\"))
             ((= key2 ?\') (my/mark-around-quotes ?\'))
             (t (my/handle-unknown-key key1 key2)))
          (my/handle-unknown-key key1 key2))))
     
     ;; Any other key exits visual mode and performs normal C-SPC
     (t
      (my/debug "Unknown first key, exiting visual mode")
      (setq my/last-command-was-enhanced-set-mark nil)
      (my/exit-visual-mode)
      (set-mark-command nil)
      (when (and key1 (not (eq key1 ?\C-g)))
        (setq unread-command-events (list key1)))))))

;;; Main entry point
(defun my/enhanced-set-mark-command (arg)
  "Enhanced set-mark-command that enters visual mode when called without prefix.
With prefix argument, behaves like normal set-mark-command.
When called twice in a row, behaves like default C-SPC C-SPC (set mark then deactivate)."
  (interactive "P")
  (cond
   ;; With prefix, use normal set-mark behavior
   (arg
    (my/debug "C-SPC called with prefix arg, using normal behavior")
    (setq my/last-command-was-enhanced-set-mark nil)
    (set-mark-command arg))
   
   ;; Second C-SPC in a row - use default behavior (set mark then deactivate)
   (my/last-command-was-enhanced-set-mark
    (my/debug "Second C-SPC in a row, using default set-mark behavior")
    (setq my/last-command-was-enhanced-set-mark nil)
    (set-mark-command nil))
   
   ;; First C-SPC without prefix - enter visual mode
   (t
    (my/debug "First C-SPC without prefix, entering visual mode")
    (setq my/last-command-was-enhanced-set-mark t)
    (my/enter-visual-mode)
    (my/visual-mode-dispatch))))

;;; Key bindings
;; Replace the default C-SPC binding
(global-set-key (kbd "C-SPC") 'my/enhanced-set-mark-command)

;; Also bind to C-@ (traditional alternative for C-SPC)
(global-set-key (kbd "C-@") 'my/enhanced-set-mark-command)

;; Provide a way to access the original set-mark-command if needed
(global-set-key (kbd "C-c SPC") 'set-mark-command)

;; Debug helper bindings
(global-set-key (kbd "C-c d t") 'my/toggle-debug)
(global-set-key (kbd "C-c d i") 'my/show-buffer-info)

(provide 'visual-selection-mode)

;;; Usage:
;; C-SPC i( or i) → select inside parentheses
;; C-SPC a( or a) → select around parentheses  
;; C-SPC i[ or i] → select inside square brackets
;; C-SPC a[ or a] → select around square brackets
;; C-SPC i{ or i} → select inside curly braces
;; C-SPC a{ or a} → select around curly braces
;; C-SPC i< or i> → select inside angle brackets
;; C-SPC a< or a> → select around angle brackets
;; C-SPC i" → select inside double quotes
;; C-SPC a" → select around double quotes
;; C-SPC i' → select inside single quotes
;; C-SPC a' → select around single quotes
;; C-SPC C-SPC → normal mark set/deactivate behavior
;; C-u C-SPC → exchange point and mark (normal behavior)
;; C-c SPC → direct access to original set-mark-command

;;; visual-selection-mode.el ends here
