(require 'cl)
(require 'imenu)
(require 'counsel)
(require 'magit-git)
(require 'magit-process)
(require 'projectile)

(defun core-project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let ((projectile-require-project-root strict-p))
    (ignore-errors (projectile-project-root))))

(defun core*project-root (&rest _)
  "An advice function used to replace project-root-detection functions in other
libraries."
  (core-project-root))

;; Delete words
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; COPY / PASTE on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (memq window-system '(mac ns))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun bookmark-jump-or-find-file (bookmark)
  "Jump to BOOKMARK, but if it's a directory, start a 'find-file' from there."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark")))
  (if (file-directory-p (bookmark-get-filename bookmark))
      (let ((default-directory (bookmark-get-filename bookmark)))
        (counsel-find-file))
    (bookmark-jump bookmark)))

(defun beginning-of-buffer-record ()
  "Record the current position in buffer and move to beginning."
  (point-to-register 9)
  (beginning-of-buffer))

(defun end-of-buffer-record ()
  "Record the current position in buffer and move to end."
  (point-to-register 9)
  (end-of-buffer))

(defun go-back-to-point ()
  "Go back to the point before navigating to beginning or end of buffer."
  (interactive)
  (jump-to-register 9))

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun disable-neotree-hook (_unused)
  (linum-mode -1))

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun get-root ()
  (let ((current-project (projectile-project-name)))
    (if (not current-project)
	(return "")
      current-project)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun codenav-imenu-candidates ()
  "Get the candidates list from imenu."
  (let* ((items (imenu--make-index-alist))
	 (items (delete (assoc "*Rescan*" items) items)))
    items))

(defun codenav-flatten-candidates (candidates)
  "Flatten CANDIDATES of imenu list."
  (let (result)
    (dolist (candidate candidates result)
      (if (imenu--subalist-p candidate)
          (setq result (append result (codenav-flatten-candidates (cdr candidate))))
        (add-to-list 'result candidate)))
    result))

(defun codenav-sort-candidates (candidates)
  (sort candidates (lambda (a b) (< (cdr a) (cdr b)))))


(defun codenav-test-flatten-candidates ()
  (interactive)
  (let* ((imenu-candidates (codenav-imenu-candidates))
         (names-and-pos (codenav-sort-candidates (codenav-flatten-candidates imenu-candidates))))
    (let ((a (nth 0 imenu-candidates)))
      (message "First element")
      (message "%s" a)
      (message "%s" (nth 1 a)))
    (message "\n\nRESULT:%s\n\n" names-and-pos)))


(defun codenav-current-symbol (names-and-pos)
  "Figure out current definition by checking positions of NAMES-AND-POS against current position."
  (interactive)
  (let ((list-length (length names-and-pos))
        (current-pos (point))
        (current-index 0)
        (next-index 0))
    (dolist (symbol names-and-pos)
      ;; If we reaches the end, just return the last element
      ;; instead of returning index+1
      (setq next-index (if (< next-index (1- list-length))
                          (1+ current-index)
                         current-index))
      (let* ((current-symbol-pos (marker-position (cdr symbol)))
	     (next-symbol-pos (marker-position (cdr (nth next-index names-and-pos)))))
        (if (and (= current-index 0) (< current-pos current-symbol-pos))
            (return 0))
	(if (and (>= current-pos current-symbol-pos) (< current-pos next-symbol-pos))
	    (return current-index)))
      (setq current-index (1+ current-index)))
    ;; If last item, decrement index
    (if (eq current-index (length names-and-pos))
	(1- current-index)
      current-index)))


(defun codenav-next-definition ()
  "Navigate to next function/class definition."
  (interactive)
  (let* ((imenu-candidates (codenav-imenu-candidates))
         (names-and-pos (codenav-sort-candidates (codenav-flatten-candidates imenu-candidates)))
	 (current-symbol (codenav-current-symbol names-and-pos))
         (next-symbol-index (if (>= (1+ current-symbol) (length names-and-pos)) 0
                              (1+ current-symbol)))
	 (next-symbol (nth next-symbol-index names-and-pos)))
    (imenu next-symbol)))


(defun codenav-prev-definition ()
  "Navigate to previous function/class definition."
  (interactive)
  (let* ((imenu-candidates (codenav-imenu-candidates))
         (names-and-pos (codenav-sort-candidates (codenav-flatten-candidates imenu-candidates)))
	 (current-symbol (codenav-current-symbol names-and-pos))
         (prev-symbol-index (if (< (1- current-symbol) 0) (1- (length names-and-pos))
                              (1- current-symbol)))
	 (prev-symbol (nth prev-symbol-index names-and-pos)))
    (imenu prev-symbol)))



(provide 'core-functions)
