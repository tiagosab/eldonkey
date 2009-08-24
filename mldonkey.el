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

(defvar mldonkey-user "admin")
(defvar mldonkey-passwd "admin")

(setq my-counter 0)

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

(defun mld-listener (process data)
  "Listen to new incoming data. Verifies what kind of data is
coming, and if we have a complete packet."
  (setq my-counter (+ my-counter 1))
  (let ((buf (get-buffer-create " *mldonkey*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (let ((beg (point-marker)))
        (end-of-buffer)
        (insert data)
        (goto-char beg)
        (if (>= (point-max) (+ beg 4))
            (let ((len (bindat-get-field
                        (bindat-unpack '((size u32r))
                                       (buffer-substring-no-properties
                                        beg (+ beg 4)))
                                       'size)))
              (when (>= (point-max) (+ beg len 4))
                (let ((msg (buffer-substring-no-properties beg (+ beg len 4))))
                  ;(message "Message is multibyte: %s" (multibyte-string-p msg))
                  (mld-dispatcher process msg))
                (forward-char (+ len 4))))
          ;(message "No way.")
          )))))

(defun mld-dispatcher (process data)
  "Unpack and identify each packet."
  (let* ((pack (bindat-unpack mld-message-bindat-spec (string-to-unibyte data)))
         (function (bindat-get-field pack 'function))
         (parameters (bindat-get-field pack 'parameters)))
    ;; (message "pack: %s" pack)
    ;; (message "length: %s" (bindat-length mld-message-bindat-spec pack))
    ;; (message "size: %s" (bindat-get-field pack :size))
    ;; (message "parameters: %s" parameters)
    ;; (message "Data: %s" data)
    ;; (message "uniData: %s" (string-to-unibyte data))
    ;; (message "Func: %s" function)
    ; function numbers are defined in file guiEncoding.ml
    (setq my-funclist (cons function my-funclist))
    (cond
     ((eq function 0)  ;"CoreProtocol")
      (mld-handshake process parameters))
     ((eq function 1)  ;"Options_info")
      (mld-get-options-info process parameters))
     ((eq function 3)  ; "DefineSearches
      )
     ((eq function 4)  ;"Result_info")
      )
     ((eq function 5)  ;"Search_result"
      )
     ((eq function 6)  ;"Search_waiting"
      )
     ((eq function 52)  ;"File_info" is 52 from proto 14 up
      )
     ((eq function 46) ; "File_downloaded" ***
      )
     ((eq function 10) ; File_add_source
      )
     ((eq function 12)  ; Server_user
      )
     ((eq function 13) ;"Server_state")
      )
     ((eq function 26) ;"Server_info")
      )
     ((eq function 15) ; "Client_info" ***
      )
     ((eq function 16) ; "Client_state" ***
      )
     ((eq function 17) ; Client_friend
      )
     ((eq function 18) ; Client_file
      )
     ((eq function 19) ;"Console")
      )
     ((eq function 20) ; Network_info
      )
     ((eq function 21) ; User_info
      )
     ((eq function 31) ; Room_info
      )
     ((eq function 23) ; Room_message
      )
     ((eq function 24) ; Room_add_user
      )
     ((eq function 27) ; MessageFromClient
      )
     ((eq function 47) ; BadPassword
      (message "Bad password. Disconnecting.")
      (mld-disconnect process)
      )
     ((eq function 53) ; DownloadFiles
      )
     ((eq function 54) ; DownloadedFiles
      )
     ((eq function 28) ; Connected Servers
      )
     ((eq function 49) ; "Client_stats" ***
      )
     ((eq function 32) ; Room_remove_user
      )
     ((eq function 48) ; Shared_file_info
      )
     ((eq function 34) ; Shared_file_upload
      )
     ((eq function 35) ; Shared_file_unshared
      )
     ((eq function 36) ; Add_section_option
      )
     ((eq function 38) ; Add_plugin_option
      )
     ((eq function 50) ; "File_remove_source" ***
      )
     ((eq function 9) ; File_update_availability
      )
     ((eq function 51) ; CleanTables
      )
     ((eq function 55) ; Uploaders list
      )
     ((eq function 56) ; Pending list
      )
     ((eq function 57) ; Search
      )
     ((eq function 58) ; Version
      )
     ((eq function 59) ; Stats
     (t
      (message "Unrecognized function number %s" function))
      ))))

(defun mld-disconnect (process)
  (delete-process process))

(defun mld-handshake (process parameters)
  (let* ((msg (bindat-unpack mld-coreprotocol-bindat-spec parameters))
         (version (bindat-get-field
                   msg
                   'version)))
    (message "version: %s" version)
    (message "max-to-gui: %s" (bindat-get-field msg 'max-to-gui))
    (message "max-from-gui: %s" (bindat-get-field msg 'max-from-gui))
    (setq mld-protocol-version version)
    (mld-send-version process)
    (mld-send-password process)))

(defun mld-get-options-info (process parameters)
  (let ((msg (bindat-unpack mld-options-info-bindat-spec parameters))
        (buf (get-buffer-create "*mldonkey-options*")))
    (save-excursion
      (set-buffer buf)
      (end-of-buffer)
      (insert (format "%s\n" (bindat-get-field msg 'options))))))

(defun mld-send-password (process)
  (let ((msg (bindat-pack mld-password-bindat-spec
                           `((passwordlen . ,(length mldonkey-passwd))
                             (password . ,mldonkey-passwd)
                             (usernamelen . ,(length mldonkey-user))
                             (username . ,mldonkey-user)))))
    (mld-send-pack process 52 msg)))

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

(defun mldonkey-connect ()
  (interactive)
  (mld-killer t)
  (setq my-counter 0)
  (setq my-funclist ())
  (setq mldonkey-connection
        (make-network-process
         :name "mldonkey"
         :filter 'mld-listener
         :filter-multibyte nil
         :host "localhost"
         :service 4001)))

;; Messages from server

(setq mld-coreprotocol-bindat-spec
      '((version   u32r)
        (max-to-gui u32r)
        (max-from-gui u32r)))
(setq mld-options-info-bindat-spec
      '((listlen       u16r)
        (options       repeat (listlen)
                       (namelen      u16r)
                       (name         str (namelen))
                       (valuelen     u16r)
                       (value        str (valuelen)))))
(setq mld-result-info-bindat-spec
  '((id           long)
    (network      long)
    (filenameslen int)
    (filenames    repeat (filenameslen)
                   (filenamelen    int)
                   (filename       (filenamelen)))
    (hash         16)
    (size         long)
    (formatlen    int)
    (format       str (formatlen))
    (typelen      int)
    (type         str (typelen))
    (taglistlen   int)
    (taglist      repeat (taglistlen)
                   (tagnamelen     int)
                   (tagname        str (tagnamelen))
                   (tagtype        byte)
                   (tagvalue       union
                                    (cond ((or (eq last 0)
                                               (eq last 1))
                                           "long")
                                          ((eq last 2)
                                           "str"
                                          ((eq last 3)
                                           "ip")))
                                    ("long" (tagvalue    long))
                                    ("str"  (tagvaluelen int)
                                            (tagvalue (tagvaluelen)))
                                    ("ip"   (tagvalue    ip))))
    (commentlen   int)
    (comment      str (commentlen))
    (done         byte)))

     ; Tag value (Long if type is 0 or 1, String if type is 2, Ip if type is 3)



(setq mld-server-info-bindat-spec nil)

(setq mld-message-bindat-spec
  '((size      u32r)
    (function  u16r)
    (parameters str (eval (- (bindat-get-field struct 'size) 2)))))
                 ;; union (cond
                 ;;        ((eq last 0) "CoreProtocol")
                 ;;        ((eq last 1) "Options_info")
                 ;;        ((eq last 4) "Result_info")
                 ;;        ((eq last 7) "File_info")
                 ;;        ((eq last 13) "Server_state")
                 ;;        ((eq last 19) "Console")
                 ;;        ((eq last 26) "Server_info"))
                 ;; ("CoreProtocol" mld-coreprotocol-bindat-spec)
                 ;; ("Options_info" mld-options-info-bindat-spec)
                 ;; ("Result_info" mld-result-info-bindat-spec)
                 ;; ("File_info" mld-file-info-bindat-spec)
                 ;; ("Server_state" mld-server-state-bindat-spec)
                 ;; ("Console" mld-console-bindat-spec)
                 ;; ("Server_info" mld-server-info-bindat-spec))))

;; Messages from gui

(setq mld-guiprotocol-bindat-spec
  '((version   u32r)))
(setq mld-password-bindat-spec
  '(
    (passwordlen  u16r)
    (password     str (passwordlen))
    (usernamelen  u16r)
    (username     str (usernamelen))
    ))

(provide 'mldonkey)
