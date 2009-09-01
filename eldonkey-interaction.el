
;; Showing results to user.

(defun mld-show-files ()
  (interactive)
  (let ((buf (get-buffer-create "*mldonkey-files*"))
        (files (get 'mld-file-info 'mld-data)))
    (set-buffer buf)
    (erase-buffer)
    (while files
      (let* ((file-info-entry (car files))
             (file (cdr file-info-entry)))
          ;; We should convert string from utf8 to the default.
          (insert (mld-format-file file)))
      (setq files (cdr files)))
    (mldonkey-files-mode)
    (display-buffer buf)))

(defun mld-format-int64 (msg)
  (let ((low (bindat-get-field msg 'u64l))
        (high (bindat-get-field msg 'u64u)))
    (+ (* high 256 256) low)))

(defun mld-format-file (entry)
  "Format file info for inclusion in *mldonkey-files* buffer.

Take a file ENTRY as returned by bindat-unpack and format it for
insertion. Returns a string."
  (let ((file-num (bindat-get-field entry 'file-num))
        (file-names (bindat-get-field entry 'file-names))
        entry-string)
    (setq entry-string (format "File Number: %s\nFile name(s): %s\n"
                               file-num
                               (string-as-multibyte
                                (bindat-get-field
                                 (car file-names) 'name 'str-value))))
    (setq file-names (cdr file-names))
    (while file-names
      (let ((file (car file-names)))
        (setq entry-string (format "%s              %s\n"
                                   entry-string
                                   (string-as-multibyte
                                    (bindat-get-field
                                     file 'name 'str-value)))))
      (setq file-names (cdr file-names)))
    (propertize entry-string 'mld-file-num file-num)))

(defun mld-show-file (file-num)
  (let ((file (cdr (assoc file-num (get 'mld-file-info 'mld-data))))
        (buf (get-buffer-create "*mldonkey-file*")))
    (message "%s" file-num)
    (set-buffer buf)
    (erase-buffer)
    (insert (string-as-multibyte (mld-format-file file)))
    (insert (format
             "Network: %s. Size: %s.\n"
             (bindat-get-field file 'file-network)
             (mld-format-int64 (bindat-get-field file 'file-size))))
    (insert (format
             "File-downloaded: %s.\n"
             (mld-format-int64 (bindat-get-field file 'file-downloaded))))
    (insert (format
             "All sources: %s. Active sources: %s.\n"
             (bindat-get-field file 'file-all-sources)
             (bindat-get-field file 'file-active-sources)))
    (insert (format
             "Chunks: %s."
             (bindat-get-field file 'chunks 'str-value)))
    (display-buffer buf)))

(defun mld-show-file-at-point (point)
  (interactive "d")
  (mld-show-file (get-text-property point 'mld-file-num)))

(defun mld-show-searches ()
  (interactive)
  (mld-send-getsearches mldonkey-connection)
  (add-hook 'mld-after-search-hook 'mld-show-searches-1))

(defun mld-show-searches-1 ()
  (remove-hook 'mld-after-search-hook 'mld-show-searches-1)
  (let ((buf (get-buffer-create)))
    (set-buffer buf)
    (let ((searches (cdr
                     (assoc 'searches (get 'mld-get-search 'mld-data)))))
      (while searches
        (let ((search (car searches)))
          (insert
           (format "Search number: %s\nSearch string: %s\n"
                   (bindat-get-field search 'search-num)
                   (string-as-multibyte
                    (bindat-get-field search 'search-string 'str-value))))
          (insert
           (format "Max hits: %s\n Search type: %s\nNetwork: %s\n\n"
                   (bindat-get-field search 'search-max-hits)
                   (bindat-get-field search 'search-type)
                   (bindat-get-field search 'search-network))))
        (setq searches (cdr searches))))
    (pop-to-buffer buf)))
      

(defun mld-show-definesearches ()
  (interactive)
  (let ((buf (get-buffer-create "*mld-definesearches*")))
    (set-buffer buf)
    (erase-buffer)
    (let* ((searches (car (get 'mld-definesearches 'mld-data)))
           (len (bindat-get-field searches 'searches-len))
           (queries (cdr (bindat-get-field searches 'query-data))))
      (while queries
        (let ((query (car queries)))
          (insert (format "Query string: %s\n"
                          (bindat-get-field query 'query-str 'str-value)))
          (insert (format "Query: %s\n\n"
                          (bindat-get-field query 'query))))
        (setq queries (cdr queries)))
      (display-buffer buf))))

(defvar mldonkey-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (kbd "RET") 'mld-show-file-at-point)
    map))

(define-derived-mode mldonkey-files-mode text-mode "Mld-files"
  "Major mode for browsing mldonkey files."
  (toggle-truncate-lines 1))

(provide 'eldonkey-interaction)