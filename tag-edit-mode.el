;;; tag-edit-mode.el --- major mode for editing file tags  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 modula t.

;; Author: modula t. <defaultxr at pm dot me>
;; Homepage: https://github.com/defaultxr/tag-edit-mode
;; Keywords: files, multimedia, tools
;; Version: 0.5
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tag-Edit-Mode is a major mode to edit file tags (id3, vorbis, etc).
;; Quickly and easily edit tags with the power of Emacs!

;;; Code:

;; FIX: handle in-buffer errors when a tag can't be read (i.e. "-- failed to get tags ..."). should be handled by font-lock and by the tag-write functions
;; FIX: option to always write to a separate file (and another option to write to a separate file, then trash the originals before renaming the new file to the previous version's name)
;; FIX: allow the user to specify backend preference (kid3-cli should be preferred by default). possibly warn when falling back to a non-preferred backend?
;; FIX: backends should have the following information:
;; - supported filetypes/tags
;; FIX: possible future backends:
;; - Emms's metadata library(?)
;; FIX: flac files have their tag names in all caps; tag-edit-mode doesn't detect this.
;; FIX: implement imagemagick "identify -verbose image.png" to get metadata for images. exiftool might work too?
;; FIX: maybe test with ERT (Emacs' unit test framework)?
;; FIX: mark when a tag has unwritten changes
;; FIX: also mark when a file has unwritten changes
;; FIX: support viewing/setting images to tags
;; FIX: better support for files with more than one "tag set"; i.e. support reading and writing both id3v1 and id3v2.
;; FIX: also allow the user to specify behavior when writing files with more than one tag set. options should be: write only id3v2 and remove id3v1, write both id3v2 and v1, and ignore id3v1. possibly others?
;; ;; FIX: make a function to go to a specified tag for the file under point
;; Emacs has exif-field and exif-tag-alist to get EXIF tags, but it doesn't allow them to be set.

;;; requirements

(require 'cl-lib)

;;; customization

(defgroup tag-edit nil
  "Tag-Edit-Mode."
  :group 'external
  :prefix "tag-edit-")

(defcustom tag-edit-standard-tags (list "file" "title" "artist" "album" "comment" "date" "track" "genre" "album_artist")
  "Ordered list of tag names that should always be shown for files."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-remove-tags nil ; FIX: implement
  "List of tags that should not be shown for files, and will be
removed from them if the file's tags are written."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-hidden-tags nil ; FIX: implement
  "List of tags that should not be shown for files, but whose value
will be preserved if the file's tags are written."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-ignore-files-function nil
  "Function that determines whether a file should be ignored when
`tag-edit' is called on a directory."
  :type '(or null function)
  :group 'tag-edit)

(defcustom tag-edit-debug-log-path nil
  "Where debug logs from external programs (such as ffmpeg) should
be written, or nil to avoid writing logs."
  :type '(or null file)
  :group 'tag-edit)

(defcustom tag-edit-audio-player "mpv"
  "The default audio player to use for the `tag-edit-preview-file'
command."
  :type '(or string (file :must-match t))
  :group 'tag-edit)

(defcustom tag-edit-external-editor "kid3"
  "The default external tag editor to use for the
`tag-edit-open-file-in-external-editor' command."
  :type '(or string (file :must-match t))
  :group 'tag-edit)

(defcustom tag-edit-pulse-on-save t
  "Whether to pulse the file after saving its tags."
  :type 'boolean
  :group 'tag-edit)

;;; utility

(defvar tag-edit-tag-name-regexp "^\\([^:]+\\): "
  "The regular expression matching the names of tags in the buffer.")

(defun tag-edit-file-tags (file)
  "Get an alist mapping the names of all tags detected in FILE to their values."
  (funcall (tag-edit-backend-read-function) file))

(defun tag-edit-file-at-point ()
  "Get the filename of the file under point."
  (cl-second (assoc "file" (tag-edit-tags-at-point))))

(defun tag-edit-file-at-point-number ()
  "Get the index of the file under point."
  (1- (count-matches "^file: " (point-min) (point))))

(defvar-local tag-edit-files-original-tags nil
  "The hash table mapping the index of the file in the current
buffer to its original tags.")

(defun tag-edit-file-original-tags (&optional file)
  (let ((file (or file (tag-edit-file-at-point-number))))
    (if (integerp file)
        (gethash file tag-edit-files-original-tags)
      ;; FIX
      )))

(defun tag-edit-ui-element (start end &optional ro-message additional-properties)
  "Write a tag-edit interface element to the current buffer, at point."
  (let ((intangible (cl-getf additional-properties additional-properties (gensym)))
        (props `(read-only ,(or ro-message "Cannot edit tag-edit template") ,@additional-properties)))
    (set-text-properties start (+ start 1) (cl-list* 'cursor-intangible intangible props))
    (set-text-properties (+ start 1) (- end 1) (cl-list* 'cursor-intangible intangible props))
    (set-text-properties (- end 1) end (cl-list* 'rear-nonsticky t props))))

(defun tag-edit-buffer-insert-tag (name value)
  (let ((ro-msg "Cannot edit tag-edit template"))
    (let ((start (point)))
      (insert (concat name ": "))
      (tag-edit-ui-element start (point) ro-msg))
    (insert (or value ""))
    (let ((start (point)))
      (insert "\n")
      (tag-edit-ui-element start (point) ro-msg))))

(defun tag-edit-buffer-insert-file (file index)
  "Write the template for FILE in the current buffer, storing its
data at INDEX in the buffer's data."
  (let ((file (expand-file-name file))
        (inhibit-read-only t)
        (file-tags (tag-edit-file-tags file)))
    (unless file-tags
      (insert "-- failed to get tags for " file ":\n")
      (setq file-tags (list (list "file" file))))
    (puthash index file-tags tag-edit-files-original-tags)
    (dolist (tag tag-edit-standard-tags)
      (tag-edit-buffer-insert-tag tag (cl-second (assoc tag file-tags))))
    (dolist (tag file-tags)
      (unless (cl-find (cl-first tag) tag-edit-standard-tags :test #'string=)
        (tag-edit-buffer-insert-tag (cl-first tag) (cl-second tag))))
    (insert "\n")))

(defun tag-edit-buffer-insert-files (files)
  "Write the templates for FILES in the current buffer."
  (unless tag-edit-files-original-tags
    (setq tag-edit-files-original-tags (make-hash-table)))
  (let ((num 0))
    (dolist (file files)
      (tag-edit-buffer-insert-file file num)
      (setq num (1+ num))))
  (tag-edit-update-header))

(defun tag-edit-tags-at-point-region ()
  "Get the start and end position of the tags for the file at point."
  (list (save-excursion
          (end-of-line)
          (search-backward-regexp "^file: ")
          (point))
        (save-excursion
          (- (or (and (search-forward "\n\n" nil t)
                      (point))
                 (point-max))
             2))))

(defun tag-edit-tags-at-point ()
  "Get the alist of tags for the file at point."
  (save-restriction
    (save-excursion
      (apply #'narrow-to-region (tag-edit-tags-at-point-region))
      (goto-char (point-min))
      (let (result)
        (while (search-forward-regexp (concat tag-edit-tag-name-regexp "\\(.+\\)$") nil t)
          (push (list (match-string-no-properties 1)
                      (match-string-no-properties 2))
                result))
        result))))

(defvar-local tag-edit-unsaved-files nil
  "List of files in the tag-edit-mode buffer with unsaved changes.")

(defun tag-edit-after-change (beginning end length)
  "Function called after a change is made to a tag-edit-mode buffer."
  (ignore beginning end length)
  ;; FIX:
  ;; - compare text of tags in buffer to the original tags. if they aren't the same, add the filename to tag-edit-unsaved-files. if they are the same, ensure it is not in tag-edit-unsaved-files.
  (when (eql major-mode 'tag-edit-mode)
    (gethash (tag-edit-file-at-point-number) tag-edit-files-original-tags))
  ;; - update header (?)
  ;; (set-buffer-modified-p (> (length tag-edit-unsaved-files) 0))
  )

;;; kid3-cli

(defun tag-edit--hash-table-alist (hash-table)
  "Convert a hash table to an alist."
  (let (result)
    (maphash (lambda (key value)
               (push (list key value) result))
             hash-table)
    result))

(defun tag-edit-read-file-tags-with-kid3-cli (file)
  "Get an alist mapping the names of all tags detected in FILE to
their values using kid3-cli."
  (let ((file (expand-file-name file)))
    (with-temp-buffer
      ;; FIX: check if kid3-cli exits with a non-zero status?
      (call-process "kid3-cli" nil (current-buffer) nil "-c" "select" file "-c" "{\"method\":\"get\"}")
      (goto-char (point-min))
      (when-let* ((json (json-parse-buffer))
                  (tagged-file (gethash "taggedFile" (gethash "result" json)))
                  (tag (or (gethash "tag2" tagged-file)
                           (gethash "tag1" tagged-file)))
                  (frames (gethash "frames" tag)))
        (cons (list "file" file)
              (if (hash-table-p frames)
                  (tag-edit--hash-table-alist frames)
                (append (cl-map 'vector (lambda (frame)
                                          (list (gethash "name" frame) (gethash "value" frame)))
                                frames)
                        nil)))))))

(defun tag-edit-write-file-tags-with-kid3-cli (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if
OUTPUT-FILE is unspecified) using kid3-cli. Only tags specified
in TAGS are changed. A tag is removed if its value in TAGS is
empty or nil.

See also: `tag-edit-write-file-tags'"
  (let ((file (expand-file-name file))
        (output-file (when output-file (expand-file-name output-file))))
    (when output-file
      (copy-file file output-file))
    (call-process "kid3-cli" nil "*kid3-cli-output*" nil "-c" "select" (or output-file file)
                  (cl-loop for tag in tags
                           unless (string= (cl-first tag) "file")
                           append (list "-c" (concat "set " (cl-first tag) " '" (s-replace "'" "\\'" (cl-second tag)) "'"))))))

;;; ffmpeg

(defun tag-edit-read-file-tags-with-ffmpeg-ffprobe (file)
  "Get an alist mapping the names of all tags detected in FILE to
their values using ffprobe (ffmpeg)."
  (let ((file (expand-file-name file)))
    (with-temp-buffer
      (call-process "ffprobe" nil (current-buffer) nil "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" file)
      (goto-char (point-min))
      (when-let* ((data (json-parse-buffer))
                  (format (gethash "format" data))
                  (tags (gethash "tags" format)))
        (cl-list* (list "file" file)
                  (mapcar (lambda (tag)
                            (list tag (gethash tag tags)))
                          (hash-table-keys tags)))))))

(defun tag-edit-read-file-tags-with-ffmpeg-ffmetadata (file)
  (let* ((file (expand-file-name file))
         (ffmetadata-file (make-temp-file "tag-edit-mode-ffmetadata-tmp-" nil ".txt"))
         result)
    (call-process "ffmpeg" nil nil nil "-y" "-i" file "-f" "ffmetadata" ffmetadata-file)
    (with-temp-buffer
      (insert-file-contents-literally ffmetadata-file)
      (while (/= (point-max) (point))
        (forward-line)
        (let ((line (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                                    (save-excursion (end-of-line) (point)))))
          ;; FIX: what does ffmetadata do when tag values contain newlines?
          (when (s-contains-p "=" line)
            (push (s-split-up-to "=" line 1) result))))
      (delete-file ffmetadata-file)
      result)))

(defun tag-edit-read-file-tags-with-ffmpeg (file)
  "Get an alist mapping the names of all tags detected in FILE to
their values using ffmpeg."
  (tag-edit-read-file-tags-with-ffmpeg-ffprobe file))

(defun tag-edit-write-file-tags-with-ffmpeg-args (file tags &optional output-file) ; FIX: ensure that nil and empty tags are removed
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if
OUTPUT-FILE is unspecified) with ffmpeg using its -metadata
argument. Only tags specified in TAGS are changed. A tag is
removed if its value in TAGS is empty or nil.

See also: `tag-edit-write-file-tags-with-ffmpeg-ffmetadata'"
  (let* ((file (expand-file-name file))
         (output-file (or output-file file))
         (replace-p (string= file output-file))
         (temp-file-name (concat (file-name-directory output-file) ".out-" (file-name-nondirectory output-file))))
    (apply #'call-process "ffmpeg" nil tag-edit-debug-log-path nil
           `(;; "-v" "quiet" ; verbosity
             "-i" ,file ; input file
             "-map" "0" ; do not alter the media content
             "-y" ; overwrite existing file if one exists
             "-codec" "copy" ; prevent unnecessary reencoding
             ;; "-write_id3v2" "1" ; may sometimes be needed if ffmpeg can't guess the tag type
             ,@(cl-loop for tag in tags
                        unless (string= (cl-first tag) "file")
                        append (list "-metadata" (concat (cl-first tag) "=" (cl-second tag))))
             ,(if replace-p
                  temp-file-name
                output-file)))
    (when replace-p
      (rename-file temp-file-name output-file t))))

(defun tag-edit-write-ffmpeg-metadata-txt (tags filename)
  "Write the TAGS alist to FILENAME in ffmpeg's ffmetadata format."
  (with-temp-buffer
    (insert ";FFMETADATA1\n")
    (dolist (tag tags)
      (unless (string= "file" (car tag))
        (insert (cl-first tag) "=" (cl-second tag) "\n")))
    (write-file filename)))

(defun tag-edit-write-file-tags-with-ffmpeg-ffmetadata (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if
OUTPUT-FILE is unspecified) with ffmpeg using its -metadata
argument. Only tags specified in TAGS are changed. A tag is
removed if its value in TAGS is empty or nil.

See also: `tag-edit-write-file-tags-with-ffmpeg-args'"
  (ignore output-file) ; FIX?
  (let ((ffmetadata-file (make-temp-file "tag-edit-mode-ffmetadata-tmp-" nil ".txt")))
    ffmetadata-file
    (tag-edit-write-ffmpeg-metadata-txt tags ffmetadata-file)
    (call-process "ffmpeg" "-i" file "-f" "ffmetadata" ffmetadata-file)
    (delete-file ffmetadata-file)))

(defun tag-edit-write-file-tags-with-ffmpeg (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if
OUTPUT-FILE is unspecified) with ffmpeg using its -metadata
argument. Only tags specified in TAGS are changed. A tag is
removed if its value in TAGS is empty or nil."
  (tag-edit-write-file-tags-with-ffmpeg-args file tags output-file))

(defun tag-edit-write-file-tags-with-ffmetadata (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if
OUTPUT-FILE is unspecified) with ffmpeg using its \"ffmetadata\"
text format. Existing tags are kept, and only those specified in
TAGS are changed. A tag is removed if its value in TAGS is empty.

See also: `tag-edit-write-file-tags-with-ffmpeg-args'"
  (let ((metadata-file "/tmp/metadata-out.txt"))
    (tag-edit-write-ffmpeg-metadata-txt tags metadata-file)
    (call-process "ffmpeg" nil (current-buffer) nil
                  "-v" "quiet" ; verbosity
                  "-i" file ; input file
                  "-map" "0" ; do not alter the media content
                  "-y" ; overwrite existing file if one exists
                  "-codec" "copy" ; prevent unnecessary reencoding
                  ;; "-write_id3v2" "1" ; may sometimes be needed if ffmpeg can't guess the tag type
                  ;; "-metadata"
                  "-f" "ffmetadata"
                  "-i" metadata-file
                  (or output-file file))))

;; other possible backends:
;; - id3.el - https://github.com/larsmagne/id3.el
;; - tag.el - https://www.emacswiki.org/emacs/FileTagUpdate

;;; backend handling

(defvar tag-edit-backends (list 'kid3-cli 'ffmpeg)
  "List of backends supported by tag-edit-mode.")

(defvar tag-edit--backend nil
  "The active backend. If nil, attempt to auto-detect a backend, and
error if none could be found. You shouldn't use this variable
directly; instead, call `tag-edit-backend'.")

(defun tag-edit-detect-backend ()
  "Set `tag-edit-backend' to the first backend detected to be
available. Backends are searched in order of preference;
currently hardcoded to kid3-cli, then ffmpeg.

Users should not need to call this manually unless they install a
new backend, as tag-edit-mode will automatically run this
function the first time a backend is needed."
  (setq tag-edit--backend (cond ((executable-find "kid3-cli") 'kid3-cli)
                                ((executable-find "ffmpeg") 'ffmpeg)
                                (t 'none))))

(defun tag-edit-backend (&optional backend)
  "Get tag-edit-mode's current active backend. If BACKEND is
provided and it is a valid backend, just return it."
  (cond (backend
         (if (member backend tag-edit-backends)
             backend
           (error "No known backend with name %s." backend)))
        (tag-edit--backend tag-edit--backend)
        (t
         (let ((backend (tag-edit-detect-backend)))
           (or backend
               (error "Tag-edit-mode couldn't find any available backends. Try installing kid3-cli or ffmpeg and then call `tag-edit-detect-backend'."))))))

(defun tag-edit-backend-read-function (&optional backend)
  "Get the function used to read the tags of a file. If BACKEND is
specified, use that backend for this invocation."
  (let ((backend (tag-edit-backend backend)))
    (intern (concat "tag-edit-read-file-tags-with-" (symbol-name backend)))))

(defun tag-edit-backend-write-function (&optional backend)
  "Get the function used to write the tags of a file. If BACKEND is
specified, use that backend for this invocation."
  (let ((backend (tag-edit-backend backend)))
    (intern (concat "tag-edit-write-file-tags-with-" (symbol-name backend)))))

;;; main interactive commands

(defun tag-edit-write-file-tags ()
  "Write the tags for the file at point."
  (interactive)
  (let* ((region (tag-edit-tags-at-point-region))
         (tags (tag-edit-tags-at-point))
         (file (tag-edit-file-at-point)))
    (tag-edit-write-file-tags-with-ffmpeg-args file tags)
    (when tag-edit-pulse-on-save
      (pulse-momentary-highlight-region (cl-first region) (cl-second region)))))

(defun tag-edit-goto-file (file)
  "Move point to FILE in the current buffer."
  (goto-char (point-min))
  (if (search-forward (concat "file: " file) nil t)
      (beginning-of-line)
    (user-error "Could not find file %s in the current buffer." file)))

(defun tag-edit-goto-file-number (n)
  "Move point to the Nth file in the current buffer."
  (if-let ((tags (gethash n tag-edit-files-original-tags)))
      (tag-edit-goto-file (cl-second (assoc "file" tags)))
    (user-error "No file at index %d" n)))

(defun tag-edit-current-index ()
  "Get the index of the file under point."
  (when-let ((matches (count-matches "^file: " (point-min) (point))))
    (1- matches)))

(defun tag-edit-write-all-file-tags () ; FIX: may have an off-by-one error here; the first file doesn't seem to get written
  "Write the tags for all files in the current buffer."
  (interactive)
  (save-excursion
    (let* ((tag-edit-pulse-on-save nil)
           (keys (sort (hash-table-keys tag-edit-files-original-tags) #'<))
           (num-keys (length keys)))
      (dolist (index keys)
        (message "Writing tag for file %d of %d" index num-keys)
        (tag-edit-goto-file-number index)
        (unless (tag-edit-tags-equivalent (tag-edit-tags-at-point) ; FIX: define tag-edit-tags-equivalent
                                          (gethash (tag-edit-file-at-point-number) tag-edit-files-original-tags))
          (tag-edit-write-file-tags)))))
  (set-buffer-modified-p nil)
  (setf tag-edit-unsaved-files nil))

(defun tag-edit-revert-file-tags ()
  "Revert the tags of the file under point to the ones currently in the file.

See also: `tag-edit-revert-all-file-tags'"
  (interactive)
  (let* ((inhibit-read-only t)
         (region (tag-edit-tags-at-point-region))
         (file (tag-edit-file-at-point))
         (index (tag-edit-current-index)))
    (goto-char (cl-first region))
    (delete-region (cl-first region) (+ 2 (cl-second region)))
    (tag-edit-buffer-insert-file file index)))

(defun tag-edit-revert-all-file-tags ()
  "Revert the tags of all files in the buffer to the ones currently in their files.

See also: `tag-edit-revert-file-tags'"
  (interactive)
  (goto-char (point-min))
  (while (tag-edit-next-file)
    (tag-edit-revert-file-tags)))

(defun tag-edit-toggle-file-visibility () ; FIX: implement
  "Toggle the visibility of the tags of the file at point."
  (interactive)
  (user-error "`tag-edit-toggle-file-visibility' is not yet implemented."))

(defun tag-edit-next-field (&optional n)
  "Go to the Nth next field in the buffer."
  (interactive "p")
  (search-forward-regexp tag-edit-tag-name-regexp nil t (or n 1)))

(defun tag-edit-previous-field (&optional n)
  "Go to the Nth previous field in the buffer."
  (interactive "p")
  (tag-edit-next-field (- (or n 1))))

(defun tag-edit-next-file (&optional n)
  "Move forward N files.

See also: `tag-edit-previous-file'"
  (interactive "p")
  (search-forward-regexp "^file: " nil nil (or n 1)))

(defun tag-edit-previous-file (&optional n)
  "Move backward N files.

See also: `tag-edit-next-file'"
  (interactive "p")
  (beginning-of-line)
  (tag-edit-next-file (- (or n 1)))
  (tag-edit-next-field))

(defvar tag-edit-preview-file-process nil
  "The process of the audio preview started by `tag-edit-preview-file'.")

(defun tag-edit-stop-preview ()
  "Stop the audio preview started by `tag-edit-preview-file'."
  (interactive)
  (kill-process tag-edit-preview-file-process)
  (setf tag-edit-preview-file-process nil))

(defun tag-edit-preview-file (&optional file player)
  "Play the file under point in an audio player. If a preview for
this file is already playing, stop the preview."
  (interactive)
  (let* ((file (or file (tag-edit-file-at-point)))
         (player (or player tag-edit-audio-player))
         (should-play (or (null tag-edit-preview-file-process)
                          (not (equal (process-get tag-edit-preview-file-process 'filename) file))
                          (not (process-live-p tag-edit-preview-file-process)))))
    (when (and tag-edit-preview-file-process
               (process-live-p tag-edit-preview-file-process))
      (tag-edit-stop-preview))
    (when should-play
      (setf tag-edit-preview-file-process (start-process "tag-edit-file-preview" nil player file))
      (process-put tag-edit-preview-file-process 'filename file)
      (set-process-sentinel tag-edit-preview-file-process
                            (lambda (process event)
                              (ignore process)
                              (when (string= "finished\n" event)
                                (setf tag-edit-preview-file-process nil))))
      (message "Previewing %s. Call `tag-edit-stop-preview' or press C-c C-t on this file again to stop." file))))

(defun tag-edit-open-file-in-external-editor (&optional file editor)
  "Open FILE (or the file under point if not specified) in EDITOR,
or `tag-edit-external-editor'."
  (interactive)
  (start-process "tag-edit-external-editor" nil (or editor tag-edit-external-editor) (or file (tag-edit-file-at-point))))

(defun tag-edit-files (files &optional recursive-p)
  "Open a buffer to edit the tags of FILES. If a file is a
directory, its containing files are added. If RECURSIVE-P is
true, also add the contents of any directories found within those
directories, and so on."
  (interactive "f")
  (cl-flet ((remap-dirs-to-contents (files)
              "Replace each directory in FILES with a list of its contents, and
return a list containing the new list as its first element and
the number of directory replacements done as its second."
              (let ((replacements 0))
                (list
                 (mapcan (lambda (file)
                           (if (file-directory-p file)
                               (progn
                                 (setq replacements (1+ replacements))
                                 (directory-files file t "^[^.]"))
                             (list file)))
                         files)
                 replacements))))
    (let* ((buffer (generate-new-buffer "*tag-edit*")) ; FIX: more descriptive buffer name?
           (files (ensure-list files))
           (files (car (remap-dirs-to-contents files)))
           (files (if recursive-p
                      (let ((found 1))
                        (while (> found 0)
                          (setq found 0)
                          (let ((result (remap-dirs-to-contents files)))
                            (setq found (cl-second result)
                                  files (cl-first result))))
                        files)
                    files))
           (files (if tag-edit-ignore-files-function
                      (cl-remove-if tag-edit-ignore-files-function files)
                    files)))
      (switch-to-buffer buffer)
      (tag-edit-mode)
      (tag-edit-buffer-insert-files files)
      (goto-char (point-min)))))

(defun tag-edit-file (file)
  "Open a buffer to edit the tags of FILE."
  (interactive "f")
  (tag-edit-files (list file)))

(defun tag-edit-directory (&optional directory-or-file)
  "Edit the tags of the files in DIRECTORY-OR-FILE. If a file is
specified, edit the tags of all files in its directory, placing
the point on the specified one."
  (interactive "D")
  (let ((directory-p (file-directory-p directory-or-file)))
    (tag-edit-files (if directory-p
                        directory-or-file
                      (file-name-directory directory-or-file)))
    (unless directory-p
      (tag-edit-goto-file directory-or-file))))

(defun tag-edit (&optional files)
  "Edit the tags of FILES. If no files are provided, attempt to \"do
what you mean\"; editing either the marked files or file at point
if we're in a dired buffer, or prompting the user for a file if
we're not."
  (interactive)
  (if files
      (tag-edit-files files)
    (if (eql 'dired-mode major-mode)
        (tag-edit-dired)
      (call-interactively 'tag-edit-files))))

;;; dired extensions

(defun tag-edit-dired-marked ()
  "Run `tag-edit' on the files marked in this dired buffer.

See also: `tag-edit-dired-file-at-point', `tag-edit-dired', `tag-edit'"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (zerop (length files))
      (error "No files marked."))
    (tag-edit files)))

(defun tag-edit-dired-file-at-point ()
  "Run `tag-edit' on the file at point in this dired buffer.

See also: `tag-edit-dired-marked', `tag-edit-dired', `tag-edit'"
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (tag-edit (list file))
    (error "No file at point.")))

(defun tag-edit-dired ()
  "Edit the tags of the files marked in this dired buffer, or if
none are marked, edit the tags of the file at point.

See also: `tag-edit-dired-marked', `tag-edit-dired-file-at-point', `tag-edit'"
  (interactive)
  (if (dired-get-marked-files)
      (tag-edit-dired-marked)
    (if (dired-file-name-at-point)
        (tag-edit-dired-file-at-point)
      (error "No marked files, nor any file at point."))))

;;; header

(defun tag-edit-update-header () ; FIX: show number of files with unsaved tags?
  "Update the header line of the tag-edit buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count (count-matches "^file: ")))
      (setf header-line-format (concat "Editing tags of " (number-to-string count) " file" (when (/= count 1) "s")))))
  (force-mode-line-update))

;;; font-lock

(defface tag-edit-mode-tag-name-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face used to highlight the nick of the user acting or speaking."
  :group 'tag-edit)

(defvar tag-edit-mode-font-lock
  `((,tag-edit-tag-name-regexp 1 'tag-edit-mode-tag-name-face)))

;;; keymap

(defvar tag-edit-mode-map
  (let ((map (make-sparse-keymap "Tag-Edit-Mode")))
    (define-key map (kbd "C-c C-c") 'tag-edit-write-file-tags)
    (define-key map (kbd "C-x C-s") 'tag-edit-write-all-file-tags)
    (define-key map (kbd "C-c C-k") 'tag-edit-revert-file-tags)
    (define-key map (kbd "C-c C-M-k") 'tag-edit-revert-file-tags)

    (define-key map (kbd "TAB") 'tag-edit-toggle-file-visibility)
    (define-key map (kbd "M-n") 'tag-edit-next-field)
    (define-key map (kbd "M-p") 'tag-edit-previous-field)
    (define-key map (kbd "C-c C-n") 'tag-edit-next-file)
    (define-key map (kbd "C-c C-p") 'tag-edit-previous-file)

    (define-key map (kbd "C-c C-t") 'tag-edit-preview-file)
    (define-key map (kbd "C-c C-o") 'tag-edit-open-file-in-external-editor)
    map)
  "Keymap for `tag-edit-mode'.")

;;; define the mode

;;;###autoload
(define-derived-mode tag-edit-mode fundamental-mode "Tag-Edit"
  "Major mode for editing file tags."
  (use-local-map tag-edit-mode-map)
  ;; (add-to-list 'after-change-functions 'tag-edit-after-change-function) ; FIX: update header and set-buffer-modified-p
  (run-hooks 'tag-edit-mode-hook)
  (cursor-intangible-mode 1)
  (setq font-lock-defaults '(tag-edit-mode-font-lock))
  (tag-edit-update-header))

(provide 'tag-edit-mode)
;;; tag-edit-mode.el ends here
