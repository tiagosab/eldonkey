
;; Data Structures
;; Unless noted otherwise, they come from src/daemon/common/guiEncoding.ml

;; data structs
; int -> u32r
; int16 -> u16r
; int64 -> 2 * u32r - imp.
; int64_2 -> int64 - imp.
; int64_28 -> int64 - imp.
; buf_float_date -> int
; list -> u16r (len), repeat count of items
; list2 (same as list?)
; array (same as list)
; string -> u16r, str of length
; stringbin (same as string)
; hostname -> string, u8 (country code), imp.
; ip2 -> complex, imp.
; uid -> string, imp.
; float -> string
; floatdate -> u32r
; int_date -> u32r
; query -> complex struct
; tag -> complex
; host_state -> complex, imp.
; client_type -> u8
; bool -> u8
; bool_option -> u8
; result -> complex
; user -> complex
; room_state -> u8
; file_state -> complex
; room -> complex
; message -> complex
; mp3 -> complex
; ogg, theora, vorbis* -> complex
; format -> complex
; kind -> complex
; partial_file - complex
; file_field - complex
; magic_string - string
; sub_files - complex
; file_comments - complex
; file - complex, implemented
; addr - complex
; server - complex
; client -complex
; network - complex
; search_type - u8
; search - complex
; shared_info - complex
; stat_info - complex
; stat_info_list - complex

;; It would be great to make a spec for buf-list, but how would the
;; spec know what the contents of the list are?
;(setq mld-buf-list-bindat-spec)
;(setq mld-buf-list2-bindat-spec)
;(setq mld-buf-array-bindat-spec)

(setq query-types
      '((0 . and)
        (1 . or)
        (2 . andnot)
        (3 . module)
        (4 . keywords)
        (5 . minsize)
        (6 . maxsize)
        (7 . format)
        (8 . media)
        (9 . mp3-artist)
        (10 . mp3-title)
        (11 . mp3-album)
        (12 . mp3-bitrate)
        (13 . hidden)))

; buf_list
; buf_list2
; buf_array

(setq mld-buf-string-bindat-spec
      '((str-len u16r)
        (str-value str (eval last))))

; buf_string_bin = buf_string

(setq mld-buf-hostname-bindat-spec
      '((hostname-len u16r)
        (hostname str (eval last))
        (country-code u8)))

(setq mld-buf-ip2-bindat-spec
     '((ip-addr ip)
       (country-code u8)))

(setq mld-buf-uid-bindat-spec
      '((uid-len u16r)
        (uid-value str (eval last))))

; buf_float = buf_string
; buf_float_date = buf_string
; buf_int_date = u32r

; The next two specs come from other files (bigEndian or littleEndian
; or anyEndian.ml)
(setq mld-buf-int64-bindat-spec
      '((u64l u32r)
        (u64u u32r)))

(setq mld-buf-int64-32-bindat-spec
     '((num u32r)
       (union (eval (if (eq last 4294967295) :more))
              (:more (numnum u32r)))))

(setq mld-buf-int64-2-bindat-spec
      '((struct mld-buf-int64-bindat-spec)))

(setq mld-buf-int64-28-bindat-spec
     '((struct mld-buf-int64-bindat-spec)))

(setq mld-buf-query-bindat-spec
      '((query-type u8)
        (union
         (eval
          (cond
           ((eq last 0) :and)
           ((eq last 1) :or)
           ((eq last 2) :andnot)
           ((eq last 3) :module)
           ((eq last 4) :keywords)
           ((eq last 5) :minsize)
           ((eq last 6) :maxsize)
           ((eq last 7) :format)
           ((eq last 8) :media)
           ((eq last 9) :mp3-artist)
           ((eq last 10) :mp3-title)
           ((eq last 11) :mp3-album)
           ((eq last 12) :mp3-bitrate)
           ((eq last 13) :hidden)))
         ((memq tag
                '(:and :or :hidden))
          (queries-len u16r)
          (queries repeat (queries-len)
                   (struct mld-buf-query-bindat-spec)))
         (:andnot (query-y struct mld-buf-query-bindat-spec)
                  (query-n struct mld-buf-query-bindat-spec))
         (:module (module-string-len u16r)
                  (module-string str (module-string-len))
                  (query struct mld-buf-query-bindat-spec))
         ((memq tag
                '(:keywords :minsize :maxsize :format
                            :media :mp3-artist :mp3-title
                            :mp3-album :mp3-bitrate))
          (s1-len u16r)
          (s1 str (s1-len))
          (s2-len u16r)
          (s2 str (s2-len)))
         )))

(setq mld-buf-tag-bindat-spec
      '((tag-str-len u16r)
        (tag-str str (eval last))
        (tag-value-type u8)
        (tag-value union (eval last)
                   (0 (struct mld-buf-int64-32-bindat-spec))
                   (1 (struct mld-buf-int64-32-bindat-spec))
                   (2 (struct mld-buf-string-bindat-spec))
                   (3 (struct mld-buf-ip2-bindat-spec))
                   (4 (n u16r))
                   (5 (n u8))
                   (6 (pair repeat 2
                            (struct mld-buf-int64-32-bindat-spec))))))

(setq mld-host-state-types
      '((0 . notconnected)
        (1 . connecting)
        (2 . connected-initiating)
        (3 . connected-downloading)
        (4 . connected-1)
        (10 . connected-2)
        (5 . connected-n)
        (6 . newhost)
        (7 . removedhost)
        (8 . blacklistedhost)
        (9 . notconnected-n)))

(setq mld-buf-host-state-bindat-spec
     '((state u8)
       (union (eval (cdr (assoc last mld-host-state-types)))
              (connected-downloading
               (n u32r))
              (connected-n
               (n u32r))
              (notconnected-n
               (n u32r)))))
               
(setq mld-buf-client-type-bindat-spec
      '((type-client u8)))
; 1 friend; 2 contact

; buf_bool u8
; buf_bool_option u8

(setq mld-buf-result-bindat-spec
      '((result-num u32r)
        (result-source-network u32r)
        (result-names-len u16r)
        (result-names repeat (eval last)
                (struct mld-buf-string-bindat-spec))
        (result-uids-len u16r)
        (result-uids repeat (eval last)
                     (struct mld-buf-uid-bindat-spec))
        (result-size struct mld-buf-int64-2-bindat-spec)
        (result-format struct mld-buf-string-bindat-spec)
        (result-type struct mld-buf-string-bindat-spec)
        (result-tags-len u16r)
        (result-tags repeat (eval last)
                     (struct mld-buf-tag-bindat-spec))
        (result-comment struct mld-buf-string-bindat-spec)
        (result-done u8)
        (result-time u32r)))

(setq mld-buf-user-bindat-spec
      '((user-num u32r)
        (md4 str 16)
        (user-name struct mld-buf-string-bindat-spec)
        (user-ip struct mld-buf-ip2-bindat-spec)
        (user-port u16r)
        (tags-length u16r)
        (tags repeat (eval last)
              (struct mld-buf-tag-bindat-spec))
        (user-server u16r)))

; buf_room_state u8 (0 opened, 1 closed, 2 paused)

(setq mld-buf-file-state-alist
      '((0 . file-downloading)
        (1 . file-paused)
        (2 . file-downloaded)
        (3 . file-shared)
        (4 . file-cancelled)
        (5 . file-new)
        (6 . file-aborted)
        (7 . file-queued)))

(setq mld-buf-file-state-bindat-spec
      '((file-state u8)
        (file-aborted-details union
                              (eval
                               (cond ((eq last 1) :abort)))
                              (:abort
                               (aborted-details-len u16r)
                               (aborted-details str
                                                (aborted-details-len))))))

(setq mld-buf-room-bindat-spec
      '((room-num u16r)
        (room-network u16r)
        (room-name struct mld-buf-string-bindat-spec)
        (room-state u8)))

(setq mld-buf-message-bindat-spec
      '((msg-type u8)
        (msg-contents union
                      (eval
                       (cond
                        ((eq last 0) :server-message)
                        ((eq last 1) :public-message)
                        ((eq last 2) :private-message)))
                      (:server-message
                       (msg struct mld-buf-string-bindat-spec))
                      (:public-message
                       (n u32r)
                       (msg struct mld-buf-string-bindat-spec))
                      (:private-message
                       (n u32r)
                       (msg struct mld-buf-string-bindat-spec)))))

(setq mld-buf-mp3-bindat-spec
      '((title-len u16r)
        (title str (title-len))
        (artist-len u16r)
        (artist str (artist-len))
        (album-len u16r)
        (album str (album-len))
        (year-len u16r)
        (year str (year-len))
        (comment-len u16r)
        (comment str (comment-len))
        (tracknum u32r)
        (genre u32r)))

(setq mld-buf-ogg-stream-type
      '((stream-type u8)))
;; 0 video, 1 audio, 2 index, 3 text, 4 vorbis, 5 theora

(setq mld-buf-vorbis-bitrate-bindat-spec
      '((bitrate-type u8) ; 0 max, 1 nominal, 2 min
        (bitrate struct mld-buf-string-bindat-spec)))

(setq mld-buf-theora-cs-bindat-spec
      '((cs u8)))
;; 0 undefined, 1 Rec470M, 2 Rec470BG

(setq mld-buf-ogg-stream-tag-bindat-spec
      '((ogg-stream tag union
                    (eval
                     (cond ((eq last 0) "codec")
                           ((eq last 1) "bits-per-sample")
                           ((eq last 2) "duration")
                           ((eq last 3) "has-subtitle")
                           ((eq last 4) "has-index")
                           ((eq last 5) "audio-channels")
                           ((eq last 6) "audio-sample-rate")
                           ((eq last 7) "audio-blockalign")
                           ((eq last 8) "audio-avgbytespersec")
                           ((eq last 9) "vorbis-version")
                           ((eq last 10) "vorbis-sample-rate")
                           ((eq last 11) "vorbis-bitrates")
                           ((eq last 12) "vorbis-blocksize-0")
                           ((eq last 13) "vorbis-blocksize-1")
                           ((eq last 14) "video-width")
                           ((eq last 15) "video-heigth")
                           ((eq last 16) "video-sample-rate")
                           ((eq last 17) "aspect-ratio")
                           ((eq last 18) "theora-cs")
                           ((eq last 19) "theora-quality")
                           ((eq last 20) "theora-avgbytespersec")))
                    ("codec"
                     (codec-len u16r)
                     (codec str (codec-len)))
                    ("bits-per-sample"
                     (bits-per-sample u16r))
                    ("duration"
                     (duration u16r))
                    ("has-subtitle"
                     nil)
                    ("has-index"
                     nil)
                    ("audio-channels"
                     (audio-channels u16r))
                    ("audio-sample-rate"
                     (audio-sample-rate-len u16r)
                     (audio-sample-rate str
                                        (audio-sample-rate-len)))
                    ("audio-blockalign"
                     (audio-blockalign u16r))
                    ("audio-avgbytespersec"
                     (avgbps u16r)
                     (audio-avgbytespersec str (avgbps)))
                    ("vorbis-version"
                     (vorbis-version-len u16r)
                     (vorbis-version str (vorbis-version-len)))
                    ("vorbis-sample-rate"
                     (vorbis-sample-rate-len u16r)
                     (vorbis-sample-rate str (vorbis-sample-rate-len)))
                    ("vorbis-bitrates"
                     (vorbis-bitrates-len u16r)
                     (vorbis-bitrates repeat (vorbis-bitrates-len)
                                 (struct mld-buf-vorbis-bitrate-bindat-spec)))
                    ("vorbis-blocksize-0"
                     (vorbis-blocksize-0 u16r))
                    ("vorbis-blocksize-1"
                     (vorbis-blocksize-1 u16r))
                    ("video-width"
                     (video-width-len u16r)
                     (video-width str (video-width-len)))
                    ("video-heigth"
                     (video-heigth-len u16r)
                     (video-heigth str (video-heigth-len)))
                    ("video-sample-rate"
                     (video-sample-rate-len u16r)
                     (video-sample-rate str (video-sample-rate-len)))
                    ("aspect-ratio"
                     (aspect-ratio-len u16r)
                     (aspect-ration str (aspect-ration-len)))
                    ("theora-cs"
                     (struct mld-buf-theora-cs-bindat-spec))
                    ("theora-quality"
                     (theora-quality u16r))
                    ("theora-avgbytespersec"
                     (theora-avgbytespersec u16r)))))

(setq mld-buf-ogg-bindat-spec
      '((ogg-infos-len u16r)
        (ogg-infos repeat (ogg-infos)
                   (struct mld-buf-ogg-stream-type-bindat-spec))
        (ogg-stream-tags-len u16r)
        (ogg-stream-tags repeat (ogg-stream-tags-len)
                         (struct mld-buf-ogg-stream-tag-bindat-spec))))

(setq mld-buf-format-bindat-spec
      '((file-format u8)
        (file-format-details union
                             (eval
                              (cond ((eq last 0) "unknown")
                                    ((eq last 1) "s1s2") ;??
                                    ((eq last 2) "avi")
                                    ((eq last 3) "mp3")
                                    ((eq last 4) "ogg")
                                    (t "wrong")))
                             ("unknown" nil)
                             ("s1s2"
                              (s1-len u16r)
                              (s1 str (s1-len))
                              (s2-len u16r)
                              (s2 str (s2-len)))
                             ("avi"
                              (codec-len u16r)
                              (codec str (codec-len))
                              (width u32r)
                              (heigth u32r)
                              (fps u32r)
                              (rate u32r))
                             ("mp3"
                              (struct mld-buf-mp3-bindat-spec))
                             ("ogg"
                              (struct mld-buf-ogg-bindat-spec)))))

(setq mld-buf-kind-bindat-spec
      '((kind u8)
        (kind-details union (eval
                             (cond ((eq last 0) :known-location)
                                   ((eq last 1) :indirect-location)))
                      (:known-location
                       (struct mld-buf-ip2-bindat-spec)
                       (port u16r))
                      (:indirect-location
                       (struct mld-buf-hostname-bindat-spec)
                       (md4 str 16)
                       (struct mld-buf-ip2-bindat-spec)
                       (port u16r)))))

;; (setq mld-buf-partial-file-bindat-spec)
;; unused

;; (setq mld-buf-file-field-bindat-spec)
;; unused

;; (setq mld-buf-magic-string-bindat-spec
;;       '((struct mld-buf-string-bindat-spec)))

(setq mld-buf-sub-files-bindat-spec
      '((sub-files-len u16r)
        (sub-files repeat (eval last)
                   (name struct mld-buf-string-bindat-spec)
                   (size struct mld-buf-int64-bindat-spec)
                   (magic struct mld-buf-string-bindat-spec))))

(setq mld-buf-file-comments-bindat-spec
      '((comments-len u16r)
        (comments repeat (eval last)
                  (struct mld-buf-ip2-bindat-spec)
                  (name struct mld-buf-string-bindat-spec)
                  (rating u8)
                  (comment struct mld-buf-string-bindat-spec))))

(setq mld-buf-file-bindat-spec
      '((file-num u32r)
        (file-network u32r)
        (file-names-len u16r)
        (file-names repeat (file-names-len)
                    (name struct mld-buf-string-bindat-spec))
        (md4        str 16) ;; buf_md4
                       ;;- assuming 16 as per src/utils/lib/md4.ml
        (file-size struct mld-buf-int64-2-bindat-spec) ;; buf_int64_2)
        (file-downloaded struct mld-buf-int64-2-bindat-spec) ;; buf_int64_2)
        (file-all-sources u32r)
        (file-active-sources u32r)
        (file-state struct mld-buf-file-state-bindat-spec)
        (chunks struct mld-buf-string-bindat-spec)
        (net-avail-len u16r)
        (network-availability repeat (net-avail-len)
                              (network-avail-network u32r)
                              (network-avail struct mld-buf-string-bindat-spec))
        (file-dl-rate struct mld-buf-string-bindat-spec)
        (file-chunks-age-len u16r)
        (file-chunks-age repeat (file-chunks-age-len)
                         (age u32r))
        (file-age u32r)
        (file-format struct mld-buf-format-bindat-spec)
        (file-name struct mld-buf-string-bindat-spec)
        (file-last-seen u32r)
        (file-priority u32r)
        (file-comment struct mld-buf-string-bindat-spec)
        (file-uids-len u16r)
        (file-uids repeat (file-uids-len)
                   (struct mld-buf-string-bindat-spec))
        (sub-files-len u16r)
        (sub-files repeat (sub-files-len)
                   (struct mld-buf-sub-files-bindat-spec))
        (magic struct mld-buf-string-bindat-spec)
        (struct mld-buf-file-comments-bindat-spec)
        (file-user struct mld-buf-string-bindat-spec)
        (file-group struct mld-buf-string-bindat-spec)))

(setq mld-buf-addr-bindat-spec
     '((type u8)
       (addr union (eval
                    (cond ((eq last 0) :ip)
                          ((eq last 1) :hostname)))
             (:ip (struct mld-buf-ip2-bindat-spec))
             (:hostname (u16r)
                        (hostname str (eval last))))
       (is-blocked u8)))

(setq mld-buf-server-bindat-spec
      '((server-num u32r)
        (server-network u32r)
        (server-addr struct mld-buf-addr-bindat-spec)
        (server-port u16r)
        (server-score u32r)
        (server-tags-len u16r)
        (server-tags repeat (eval last)
                     (struct mld-buf-tag-bindat-spec))
        (server-nusers struct mld-buf-int64-28-bindat-spec)
        (server-nfiles struct mld-buf-int64-28-bindat-spec)
        (host-state struct mld-buf-host-state-bindat-spec)
        (server-name struct mld-buf-string-bindat-spec)
        (server-description struct mld-buf-string-bindat-spec)
        (server-preferred u8)
        (server-version struct mld-buf-string-bindat-spec)
        (server-max-users struct mld-buf-int64-bindat-spec)
        (server-lowid-users struct mld-buf-int64-bindat-spec)
        (server-soft-limit struct mld-buf-int64-bindat-spec)
        (server-hard-limit struct mld-buf-int64-bindat-spec)
        (server-ping u32r)))

(setq mld-buf-client-bindat-spec
      '((client-num u32r)
        (client-network u32r)
        (client-kind struct mld-buf-kind-bindat-spec)
        (client-state struct mld-buf-host-state-bindat-spec)
        (struct mld-buf-client-type-bindat-spec)
        (client-tags-len u16r)
        (client-tags repeat (eval last)
                     (struct mld-buf-tag-bindat-spec))
        (client-name struct mld-buf-string-bindat-spec)
        (client-rating u32r)
        (client-software struct mld-buf-string-bindat-spec)
        (client-session-downloaded struct mld-buf-int64-bindat-spec)
        (client-session-uploaded struct mld-buf-int64-bindat-spec)
        (client-upload struct mld-buf-string-bindat-spec)
        (client-connect-time u32r)
        (client-emule-mod struct mld-buf-string-bindat-spec)
        (client-release struct mld-buf-string-bindat-spec)
        (client-sui-verified u8)))

(setq mld-buf-network-bindat-spec
      '((network-netnum u32r)
        (network-netname struct mld-buf-string-bindat-spec)
        (network-enabled u8)
        (network-config-filename struct mld-buf-string-bindat-spec)
        (network-uploaded struct mld-buf-int64-bindat-spec)
        (network-downloaded struct mld-buf-int64-bindat-spec)
        (network-connected-servers u32r)
        (network-details-len u16r)
        (network-details repeat (eval last)
                         (detail u16r))))
                         ; hasservers 0, hasrooms 1,
                         ; hasmultinet 2, virtnetwork 3
                         ; hassearch 4, haschat 5
                         ; hassupernodes 6, hasupload 7
                         ; hasstats -1, unknownflag -1

(setq mld-buf-search-type-bindat-spec
      '((search-type u8)))

(setq mld-buf-search--query-bindat-spec
      '((search-num u32r)
        (search-query struct mld-buf-query-bindat-spec)
        (search-max-hits u32r)
        (struct mld-buf-search-type-bindat-spec)
        (search-network u32r)))

(setq mld-buf-search--string-bindat-spec
      '((search-num u32r)
        (search-string struct mld-buf-string-bindat-spec)
        (search-max-hits u32r)
        (struct mld-buf-search-type-bindat-spec)
        (search-network u32r)))

(setq mld-buf-shared-info-bindat-spec
      '((shared-num u32r)
        (shared-network u32r)
        (shared-filename struct mld-buf-string-bindat-spec)
        (shared-size struct mld-buf-int64-2-bindat-spec)
        (shared-uploaded struct mld-buf-int64-bindat-spec)
        (shared-requests u32r)
        (shared-uids-len u16r)
        (shared-uids repeat (eval last)
                     (struct mld-buf-uid-bindat-spec))
        (shared-sub-files struct mld-buf-sub-files-bindat-spec)
        (shared-magic struct mld-buf-string-bindat-spec)))

(setq mld-buf-stat-info-bindat-spec
      '((string-long struct mld-buf-string-bindat-spec)
        (string-short struct mld-buf-string-bindat-spec)
        (seen u32r)
        (banned u32r)
        (filerequest u32r)
        (download struct mld-buf-int64-bindat-spec)
        (upload struct mld-buf-int64-bindat-spec)))

(setq mld-buf-stat-info-list-bindat-spec
      '((s struct mld-buf-string-bindat-spec)
        (i u32r)
        (stat-infos-len u16r)
        (stat-infos repeat (eval last)
                    (struct mld-buf-stat-info-bindat-spec))))


;; Messages from core

(setq mld-to-gui-messages-alist
      '((0 . "coreprotocol")
        (1 . "options-info")
        (3 . "definesearches")
        (4 . "result-info")
        (5 . "search-result")
        (6 . "search-waiting")
        (52 . "file-info")
        (46 . "file-downloaded")
        (10 . "file-add-source")
        (12 . "server-user")
        (13 . "server-state")
        (26 . "server-info")
        (15 . "client-info")
        (16 . "client-state")
        (17 . "client-friend")
        (18 . "client-file")
        (19 . "console")
        (20 . "network-info")
        (21 . "user-info")
        (31 . "room-info")
        (23 . "room-message")
        (24 . "room-add-user")
        (27 . "messagefromclient")
        (47 . "badpassword")
        (53 . "downloadfiles")
        (54 . "downloadedfiles")
        (28 . "connected")
        (49 . "client-stats")
        (32 . "room-remove-user")
        (48 . "shared-file-info")
        (34 . "shared-file-upload")
        (35 . "shared-file-unshared")
        (36 . "add-section-option")
        (38 . "add-plugin-option")
        (50 . "file-remove-source")
        (9 . "file-update-availability")
        (51 . "cleantables")
        (55 . "uploaders")
        (56 . "pending")
        (57 . "search")
        (58 . "version")
        (59 . "stats")))

(setq mld-coreprotocol-bindat-spec
      '((version   u32r)
        (max-to-gui u32r)
        (max-from-gui u32r)))

(setq mld-options-info-bindat-spec
      '((listlen       u16r)
        (options       repeat (listlen)
                       (name struct mld-buf-string-bindat-spec)
                       (value struct mld-buf-string-bindat-spec))))

(setq mld-definesearches-bindat-spec
      '((searches-len u16r)
        (query-data repeat (eval last)
                    (query-str struct mld-buf-string-bindat-spec)
                    (query struct mld-buf-query-bindat-spec))))

(setq mld-result-info-bindat-spec
      '((struct mld-buf-result-bindat-spec)))

(setq mld-search-result-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-search-waiting-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-file-info-bindat-spec
      '((struct mld-buf-file-bindat-spec)))

(setq mld-file-downloaded-bindat-spec
      '((n u32r)
        (size u32r)
        (u16r)
        (float str (eval last))
        (last-seen u32r)))

(setq mld-file-add-source-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-server-busy-bindat-spec
      '((n1 u32r)
        (n2 struct mld-buf-int64-28-bindat-spec)
        (n3 struct mld-buf-int64-28-bindat-spec)))

(setq mld-server-user-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-server-state-bindat-spec
      '((n u32r)
        (host-state struct mld-buf-host-state-bindat-spec)))

(setq mld-server-info-bindat-spec
      '((struct mld-buf-server-bindat-spec)))

(setq mld-client-info-bindat-spec
      '((struct mld-buf-client-bindat-spec)))

(setq mld-client-state-bindat-spec
      '((client-num u32r)
        (host-state struct mld-buf-host-state-bindat-spec)))

(setq mld-client-friend-bindat-spec
      '((client-num u32r)
        (client-type struct mld-buf-client-type-bindat-spec)))

(setq mld-client-file-bindat-spec
      '((n1 u32r)
        (file-string struct mld-buf-string-bindat-spec)
        (n2 u32r)))

(setq mld-console-bindat-spec
      '((struct mld-buf-string-bindat-spec)))

(setq mld-network-info-bindat-spec
      '((struct mld-buf-network-bindat-spec)))

(setq mld-user-info-bindat-spec
      '((struct mld-buf-user-bindat-spec)))

(setq mld-room-info-bindat-spec
      '((struct mld-buf-room-bindat-spec)))

(setq mld-room-message-bindat-spec
      '((n u32r)
        (room-message struct mld-buf-message-bindat-spec)))

(setq mld-room-add-user-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-messagefromclient-bindat-spec
      '((num u32r)
        (msg struct mld-buf-string-bindat-spec)))

; (setq mld-badpassword-bindat-spec)
; empty message

(setq mld-downloadfiles-bindat-spec
      '((list-len u32r)
        (files repeat (list-len)
               (struct mld-buf-file-bindat-spec))))

(setq mld-downloadedfiles-bindat-spec
      '((list-len u32r)
        (files repeat (list-len)
               (struct mld-buf-file-bindat-spec))))

(setq mld-connected-servers-bindat-spec
      '((list-len u32r)
        (servers repeat (list-len)
                 (struct mld-buf-server-bindat-spec))))

(setq mld-client-stats-bindat-spec
      '((upload-counter struct mld-buf-int64-bindat-spec)
        (download-counter struct mld-buf-int64-bindat-spec)
        (shared-counter struct mld-buf-int64-bindat-spec)
        (nshared-files u32r)
        (tcp-upload-rate u32r)
        (tcp-download-rate u32r)
        (udp-upload-rate u32r)
        (udp-download-rate u32r)
        (ndownloading-files u32r)
        (ndownloaded-files u32r)
        (connected-networks-len u16r)
        (connected-networks repeat (eval last)
              (n u32r)
              (ns u32r))))

(setq mld-room-remove-user-bindat-spec
      '((room u32r)
        (user u32r)))

(setq mld-shared-file-info-bindat-spec
      '((struct mld-buf-shared-info-bindat-spec)))

(setq mld-shared-file-upload-bindat-spec
      '((num u32r)
        (upload struct mld-buf-int64-bindat-spec)
        (requests u32r)))

(setq mld-shared-file-unshared-bindat-spec
      '((num u32r)))

(setq mld-add-section-option-bindat-spec
      '((section struct mld-buf-string-bindat-spec)
        (desc struct mld-buf-string-bindat-spec)
        (option-name struct mld-buf-string-bindat-spec)
        (option-type struct mld-buf-string-bindat-spec)
        (optioh-help struct mld-buf-string-bindat-spec)
        (option-value struct mld-buf-string-bindat-spec)
        (option-default struct mld-buf-string-bindat-spec)
        (option-advanced u8)))

(setq mld-add-plugin-option-bindat-spec
      '((section struct mld-buf-string-bindat-spec)
        (desc  struct mld-buf-string-bindat-spec)
        (option-name struct mld-buf-string-bindat-spec)
        (option-type struct mld-buf-string-bindat-spec)
        (optioh-help struct mld-buf-string-bindat-spec)
        (option-value struct mld-buf-string-bindat-spec)
        (option-default struct mld-buf-string-bindat-spec)
        (option-advanced u8)))

(setq mld-file-remove-source-bindat-spec
      '((n1 u32r)
        (n2 u32r)))

(setq mld-file-update-availability-bindat-spec
      '((file-num u32r)
        (client-num u32r)
        (avail struct mld-buf-string-bindat-spec)))

(setq mld-cleantables-bindat-spec
      '((clients-len u16r)
        (clients repeat (eval last)
                 (client-num u32r))
        (servers-len u16r)
        (servers repeat (eval last)
                 (server-num u32r))))

(setq mld-uploaders-bindat-spec
      '((uploaders-len u16r)
        (uploaders repeat (eval last)
                   (n u32r))))

(setq mld-pending-bindat-spec
      '((pending-len u16r)
        (pending repeat (eval last)
                   (n u32r))))

(setq mld-search-bindat-spec
      '((struct mld-buf-search--string-bindat-spec)))

(setq mld-version-bindat-spec
      '((struct mld-buf-string-bindat-spec)))

; These are not used.
(setq mld-gift-server-attach-bindat-spec)
(setq mld-gift-server-stats-bindat-spec)

(setq mld-stats-bindat-spec
      '((num u32r)
        (stat-infos-len u16r)
        (stat-infos repeat (eval last)
                    (struct mld-buf-stat-info-list-bindat-spec))))


;; Messages from gui

(setq mld-search-query-bindat-spec
      '((struct mld-buf-search-bindat-spec)))

(setq mld-guiprotocol-bindat-spec
      '((version   u32r)))

(setq mld-getsearch-bindat-spec
      '((search-num u32r)))

(setq mld-password-bindat-spec
      '((password struct mld-buf-string-bindat-spec)
        (username struct mld-buf-string-bindat-spec)))


;; General structure of messages

(setq mld-message-bindat-spec
  '((size      u32r)
    (function  u16r)
    (parameters str (eval (- (bindat-get-field struct 'size) 2)))))

(provide 'eldonkey-specs)