 ;; Copyright (C) 2009 Tiago Saboga <tiagosaboga@gmail.com>
 ;; This program is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2 of the License, or
 ;; (at your option) any later version.

 ;; This program is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with this program; if not, write to the Free Software
 ;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'bindat)
(require 'eldonkey-specs)
(require 'eldonkey-interaction)

(defvar mldonkey-user "admin")
(defvar mldonkey-passwd "admin")
(defvar mldonkey-host "localhost")
(defvar mldonkey-port 4001)



;Auxiliary/debug functions
(defun mld-capture-packet-and-disconnect (process parameters)
  (let ((buf (get-buffer-create " *mld-debug*")))
    (set-buffer buf)
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "%s" parameters)))
  (mld-disconnect process))

(defun mld-disconnect (process)
  (delete-process process))

(defun mld-killer (&optional erase-buffer)
  (interactive)
  (let ((processes (process-list)))
    (while processes
      (let ((proc (car processes)))
        (when (string-match "mldonkey" (process-name proc))
          (delete-process proc)))
      (setq processes (cdr processes))))
  (when erase-buffer
    (save-excursion
      (let ((buf (get-buffer " *mldonkey*")))
        (when buf
          (set-buffer buf)
          (erase-buffer))))))

(defun mld-query-now (query)
  "Submit a query to the first running connection to
  mldonkey. The query should be a list in the form

(keywords \"\" \"PHRASE-TO-SEARCH\")

Other query types exist, but some are not tested and most do not
yet work. Ah, and you have no way of seeing the results for now."
  (interactive "xQuery: ")
  (let ((processes (process-list))
        found)
    (while processes
      (let ((proc (car processes)))
        (when (string-match "mldonkey" (process-name proc))
          (setq found t)
          (mld-send-search-query proc query)))
      (setq processes (if found
                          nil
                        (cdr processes))))
    (unless found
      (error "Mldonkey connection not found."))))


; Receive packets and pass them to the correct handlers.
(defun mld-listener (process data)
  "Registered listener for mldonkey process.  Listen to new
incoming data. Verifies what kind of data is coming, and if we
have a complete packet."
  (setq mld-reading-data (concat mld-reading-data (string-as-unibyte data)))
  (setq my-counter (+ my-counter 1))
  (let ((enough-data t))
    (while (and enough-data
                (>= (length mld-reading-data) 4))
      (let ((len (bindat-get-field
                  (bindat-unpack '((size u32r))
                                 (substring mld-reading-data 0 4))
                  'size)))
        (if (>= (length mld-reading-data) (+ len 4))
            (let ((msg (substring mld-reading-data 0 (+ len 4))))
              (setq mld-reading-data
                    (substring mld-reading-data (+ len 4) nil))
              (mld-dispatcher process msg))
          (setq enough-data nil))))))

(defun mld-dispatcher (process data)
  "Do what is to be done with each packet from mlDonkey core.

Identify which function is performed by DATA, received from
PROCESS, and send it to the handler mld-get-data"
  (let* ((pack (bindat-unpack mld-message-bindat-spec (string-to-unibyte data)))
         (function (bindat-get-field pack 'function))
         (parameters (bindat-get-field pack 'parameters))
         (function-name (cdr (assoc function mld-to-gui-messages-alist))))
    (mld-get-data function-name process parameters)))


; "get" functions. Receive data and store it somewhere for latter use.
(defun mld-get-data (name process parameters)
  "Receive data from mldonkey core and process it.

NAME is a string; a variable named mld-NAME-bindat-spec must be a
bindat structure to decode PARAMETERS, which were received in the
tcp connection known as PROCESS.

If the function mld-get-NAME exists, it will be called with three
arguments: process, unpacked message and previous value of
mld-NAME.

The unpacked data will be stored in a property of symbol
mld-NAME.  If the handler function exists, it must return the new
value for this property; if not, the new msg will be consed onto
the existing list.

If data cannot be unpacked, connection is terminated as not to
make emacs hanging."
  (condition-case nil
      (let* ((symbname (format "mld-%s" name))
             (symb (intern symbname)))
        (let ((initialized-list (get 'mld-data 'mld-initialized)))
          (unless (memq name initialized-list)
            (put symb 'mld-data nil)
            (put 'mld-data 'mld-initialized
                 (cons name initialized-list))))
        (let* ((spec (intern (format "mld-%s-bindat-spec" name)))
               (func (intern (format "mld-get-%s" name)))
               (msg (bindat-unpack (eval spec) parameters)))
          (mld-save-data symb process msg func)))
    (error
     (setq mld-debug-data `(,name . ,parameters))
     (mld-disconnect process)
     (message "Error unpacking: disconnecting from mldonkey core."))))
  ;(run-hooks mld-after-incoming-data-processed-hook)

(defun mld-save-data (symbol process msg func)
  (put symbol 'mld-data
       (let ((mld-data (get symbol 'mld-data)))
         (if (functionp func)
             (funcall func process msg mld-data)
           (cons msg mld-data)))))

(defun mld-get-coreprotocol (process msg mld-data)
      (mld-handshake process parameters))

;; (defun mld-get-definesearches (process msg mld-data)
;;   (let ((searchnum (bindat-get-field msg 'search-num)))
;;     (cons `(,searchnum . ,msg)
;;           (assq-delete-all searchnum mld-data))))

(defun mld-get-result-info (process msg mld-data)
  (let ((result-num (bindat-get-field msg 'result-num)))
    (cons `(,result-num . ,msg)
          (assq-delete-all result-num mld-data))))

(defun mld-get-file-info (process msg mld-data)
  (let ((filenum (bindat-get-field msg 'file-num)))
    (cons `(,filenum . ,msg)
          (assq-delete-all filenum mld-data))))

(defun mld-get-file-downloaded (process msg mld-data)
  (let ((filenum (bindat-get-field msg 'n)))
    (cons `(,filenum . ,msg)
          (assq-delete-all filenum mld-data))))

(defun mld-get-client-info (process msg mld-data)
  (let ((client-num (bindat-get-field msg 'client-num)))
    (cons `(,client-num . ,msg)
          (assq-delete-all client-num mld-data))))

(defun mld-get-client-state (process msg mld-data)
  (let* ((client-num (bindat-get-field msg 'client-num))
         (client-data (cdr (assoc client-num
                                  (get 'mld-client-info 'mld-data)))))
    (mld-save-data 'mld-client-info
                   process
                   (cons msg
                         (assq-delete-all 'client-state client-data))
                   'mld-get-client-info)))

(defun mld-get-client-friend (process msg mld-data)
  (let ((client-num (bindat-get-field msg 'client-num)))
    (cons `(,client-num . ,msg)
          (assq-delete-all client-num mld-data))))

(defun mld-get-network (process msg mld-data)
  (let ((network-netnum (bindat-get-field msg 'network-netnum)))
    (cons `(,network-netnum . ,msg)
          (assq-delete-all network-netnum mld-data))))

(defun mld-get-search (process msg mld-data)
  (cons msg mld-data))

(defun mld-get-badpassword (process msg mld-data)
      (message "Bad password. Disconnecting.")
      (mld-disconnect process))

(defun mld-handshake (process parameters)
  (let* ((msg (bindat-unpack mld-coreprotocol-bindat-spec parameters))
         (version (bindat-get-field
                   msg
                   'version)))
    ;(message "max-to-gui: %s" (bindat-get-field msg 'max-to-gui))
    ;(message "max-from-gui: %s" (bindat-get-field msg 'max-from-gui))
    (mld-send-version process)
    (mld-send-password process)
    (message "Connected to MLDonkey. Protocol Version: %s" version)
    msg))


;; Sending messages to the core

(defun mld-prepare-string (s)
  (let ((len (length s)))
    `((str-len . ,len)
      (str-value . ,s))))

(defun mld-send-password (process)
  (let ((msg (bindat-pack mld-password-bindat-spec
                           `((password . ,(mld-prepare-string mldonkey-passwd))
                             (username . ,(mld-prepare-string mldonkey-user))))))
    (mld-send-pack process 52 msg)))

(defun mld-send-getsearches (process)
  (mld-send-pack process 59 ""))

(defun mld-send-getsearch (process search-num)
  (mld-send-pack process 60
                 (bindat-pack mld-getsearch-bindat-spec
                              `((search-num . ,search-num)))))

(defun mld-make-query (query)
  (let* ((query-type (car query))
         (query-list `((query-type . ,(car (rassoc query-type query-types))))))
    (cond ((memq query-type
              '(and or hidden))
           (let ((queries (cdr query)))
             (setq query-list (append
                                      `((queries-len . ,(length queries)))
                                      query-list))
             (while queries
               (setq query-list (append
                                 `((queries .
                                       (,(mld-make-query (car queries)))))
                                 query-list))
               (setq queries (cdr queries)))))
          ((memq query-type '(keywords minsize maxsize format
                                        media mp3-artist
                                        mp3-title mp3-album
                                        mp3-bitrate))
           (setq query-list
                 (append
                  `((s2 . ,(nth 2 query))
                    (s2-len . ,(length (nth 2 query)))
                    (s1 . ,(nth 1 query))
                    (s1-len . ,(length (nth 1 query))))
                  query-list)))
          ((eq query-type 'andnot)
           (setq query-list
                 (append
                         `((query-n . (,(mld-make-query (nth 2 query))))
                           (query-y . (,(mld-make-query (nth 1 query)))))
                          query-list)))
          ((eq query-type 'module)
           (setq query-list
                 (append
                  `((query . (,(mld-make-query (nth 2 query))))
                    (module-string . ,(nth 1 query))
                    (module-string-len . ,(length (nth 1 query))))
                  query-list))))
    query-list))

(defun mld-send-search-query (process &optional query
                                      network-to-search
                                      type-of-search)
  "
network-to-search is?

type-of-search is 0 for local search, 1 for remote search and 2 for
subscribe search.

query should be a list like
 (and
  (keywords word1 word2)
  (minsize str1 str2)
  (or
   (keywords str1 str2)
   (keywords str1 str2)))

  The 'functions' keyword, minsize, maxsize, format, media,
  mp3-artist, mp3-title, mp3-album and mp3-bitrate take two
  strings as arguments, while and, or and hidden take other
  functions, andnot takes exactly two functions and module takes
  a string and a function.

"
  (unless network-to-search
    (setq network-to-search 4))
  (unless type-of-search
    (setq type-of-search 1))
  (let* ((query-packet
          (mld-make-query query))
         (msg
          (bindat-pack
           mld-search-query-bindat-spec
           ;mld-buf-search-bindat-spec
           `((search-network . ,network-to-search)
             (search-type . ,type-of-search)
             (search-max-hits . 1000)
             (search-query . ,query-packet)
             (search-num . 0) ; what should I put here?
             ))))
    (mld-send-pack process 42 msg)))

(defun mld-send-version (process)
  (let ((msg (bindat-pack mld-guiprotocol-bindat-spec
                           '((version . 41)))))
    ;; (message "Message to send: %s" msg)
    (mld-send-pack process 0 msg)))

(defun mld-send-pack (process func msg)
  ;;(message "Message is %s" (type-of msg))
  ;;(message "Message is array? %s" (arrayp msg))
  (let* ((len (length msg))
         (pack (bindat-pack mld-message-bindat-spec
                            `((size . ,(+ len 2))
                              (function . ,func)
                              (parameters . ,(format "%s" msg))))))
    ;; (message "Sending pack: %s" pack)
    (process-send-string process pack)))


;; Entry point

(defun mldonkey-connect ()
  (interactive)
  (mld-killer t)

  ; debug vars
  (setq my-counter 0)
  (setq my-funclist ())
  (setq my-msglist ())
  (setq my-filelist ())

  (put 'mld-data 'mld-initialized ())
  (setq mld-reading-data "")
  (setq mldonkey-connection
        (make-network-process
         :name "mldonkey"
         :filter 'mld-listener
         :filter-multibyte nil
         :host mldonkey-host
         :service mldonkey-port)))

(provide 'eldonkey)
