;;; tag-edit.el --- major mode for editing file tags  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 modula t.

;; Author: modula t. <defaultxr at gmail dot com>
;; Homepage: https://github.com/defaultxr/tag-edit-mode
;; Keywords: files, multimedia, tools
;; Version: 0.1
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

;; 

;;; Code:

(defgroup tag-edit nil
  "Tag-Edit-Mode."
  :group 'external
  :prefix "tag-edit-")

;; FIX: this should be symbols
(defcustom tag-edit-standard-tags (list "file" "title" "artist" "album" "comment" "date" "track" "genre" "album_artist")
  "Ordered list of tag names that should always be shown for files."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-remove-tags nil ; FIX
  "List of tags that should not be shown for files, and will be removed from them if the file's tags are written."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-hidden-tags nil ; FIX
  "List of tags that should not be shown for files, but whose value will be preserved if the file's tags are written."
  :type '(list)
  :group 'tag-edit)

(defcustom tag-edit-ignore-files-function nil
  "Function that determines whether a file should be ignored when `tag-edit' is called on a directory."
  :type '(or null function)
  :group 'tag-edit)

(defcustom tag-edit-debug-log-path nil
  "Where debug logs from external programs (such as ffmpeg) should be written, or nil to avoid writing logs."
  :type '(or null file)
  :group 'tag-edit)

(defun tag-edit-file-tags-ffprobe (file)
  "Get an alist mapping the names of all tags detected in FILE to their values using ffprobe."
  (with-temp-buffer
    (call-process "ffprobe" nil (current-buffer) nil "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" file)
    (goto-char (point-min))
    (when-let* ((data (json-parse-buffer))
                (format (gethash "format" data))
                (tags (gethash "tags" format)))
      (list* (list "file" file)
             (cl-loop for tag in (hash-table-keys tags)
                      collect (list tag (gethash tag tags)))))))

(defun tag-edit-file-tags (file)
  "Get an alist mapping the names of all tags detected in FILE to their values."
  (tag-edit-file-tags-ffprobe file))

(defvar tag-edit-files-original-tags nil
  "The hash table mapping the index of the file in the current buffer to its original tags.")
(make-variable-buffer-local 'tag-edit-files-original-tags)

(defun tag-edit-ui-element (start end &optional ro-message additional-properties)
  "Write a tag-edit interface element to the current buffer, at point."
  (let ((intangible (getf additional-properties additional-properties (gensym)))
        (props `(read-only ,(or ro-message "Cannot edit tag-edit template") ,@additional-properties)))
    (set-text-properties start (+ start 1) (list* 'cursor-intangible intangible props))
    (set-text-properties (+ start 1) (- end 1) (list* 'cursor-intangible intangible props))
    (set-text-properties (- end 1) end (list* 'rear-nonsticky t props))))

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
  "Write the template for FILE in the current buffer, storing its data at INDEX in the buffer's data."
  (let ((file (expand-file-name file))
        (inhibit-read-only t)
        (file-tags (tag-edit-file-tags file)))
    (unless file-tags
      (insert "-- failed to get tags for " file ":\n")
      (setq file-tags (list (list "file" file))))
    (puthash index file-tags tag-edit-files-original-tags)
    (dolist (tag tag-edit-standard-tags)
      (tag-edit-buffer-insert-tag tag (second (assoc tag file-tags))))
    (dolist (tag file-tags)
      (unless (find (first tag) tag-edit-standard-tags :test #'string=)
        (tag-edit-buffer-insert-tag (first tag) (second tag))))
    (insert "\n")))

(defun tag-edit-buffer-insert-files (files)
  "Write the templates for FILES in the current buffer."
  (unless tag-edit-files-original-tags
    (setq-local tag-edit-files-original-tags (make-hash-table)))
  (let ((num 0))
    (dolist (file files)
      (tag-edit-buffer-insert-file file num)
      (setq num (1+ num))))
  (tag-edit-update-header))

(defun tag-edit-tags-at-point-region ()
  "Get the start and end position of the tags for the file at point."
  (list (save-excursion
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
      (let (res)
        (while (search-forward-regexp "^\\([^:]+?\\): \\(.+\\)$" nil t)
          (push (list (match-string-no-properties 1)
                      (match-string-no-properties 2))
                res))
        res))))

;;; ffmpeg

(defun tag-edit-write-file-tags-via-ffmpeg-args (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if OUTPUT-FILE is unspecified) with ffmpeg using its -metadata argument. Existing tags are kept, and only those specified in TAGS are changed. A tag is removed if its value in TAGS is empty.

See also: `tag-edit-write-file-tags-via-ffmetadata'"
  (let* ((output-file (or output-file file))
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
                        unless (string= (first tag) "file")
                        append (list "-metadata" (concat (first tag) "=" (second tag))))
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
        (insert (first tag) "=" (second tag) "\n")))
    (write-file filename)))

(defun tag-edit-write-file-tags-via-ffmetadata (file tags &optional output-file)
  "Write TAGS of FILE to OUTPUT-FILE (or just update FILE if OUTPUT-FILE is unspecified) with ffmpeg using its \"ffmetadata\" text format. Existing tags are kept, and only those specified in TAGS are changed. A tag is removed if its value in TAGS is empty.

See also: `tag-edit-write-file-tags-via-ffmpeg-args'"
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

;;; id3.el
;; https://github.com/larsmagne/id3.el

;;; tag.el
;; https://www.emacswiki.org/emacs/FileTagUpdate

;;; kid3-cli

;;; 

(defun tag-edit-write-file-tags ()
  "Write the tags for the file at point."
  (interactive)
  (let* ((region (tag-edit-tags-at-point-region))
         (tags (tag-edit-tags-at-point))
         (file (second (assoc "file" tags))))
    (tag-edit-write-file-tags-via-ffmpeg-args file tags)
    (pulse-momentary-highlight-region (first region) (second region))))

(defun tag-edit-goto-file (file)
  "Move point to FILE in the current buffer."
  (goto-point (point-min))
  (search-forward (concat "file: " (second (assoc "file" tags)))))

(defun tag-edit-goto-index (index)
  "Move point to the file at INDEX in the current buffer."
  (if-let ((tags (gethash index tag-edit-files-original-tags)))
      (tag-edit-goto-file (second (assoc "file" tags)))
    (error "No file at index %d" index)))

(defun tag-edit-write-all-file-tags ()
  "Write the tags for all files in the current buffer."
  (save-excursion
    (maphash (lambda (index value)
               (tag-edit-goto-file (second (assoc "file" value)))
               (tag-edit-write-file-tags))
             tag-edit-files-original-tags)))

;; FIX: should it just "dwim" if we're in a dired buffer?
(defun tag-edit-files (files &optional recursive-p)
  "Open a buffer to edit the tags of FILES. If a file is a directory, its containing files are added. If RECURSIVE-P is true, also add the contents of any directories found within those directories, and so on."
  (interactive "f")
  (cl-flet ((remap-dirs-to-contents (files)
                                    "Replace each directory in FILES with a list of its contents, and return a list containing the new list as its first element and the number of directory replacements done as its second."
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
    (let* ((buffer (generate-new-buffer "*tag-edit*"))
           (files (ensure-list files))
           (files (car (remap-dirs-to-contents files)))
           (files (if recursive-p
                      (let ((found 1))
                        (while (> found 0)
                          (setq found 0)
                          (let ((res (remap-dirs-to-contents files)))
                            (setq found (second res)
                                  files (first res))))
                        files)
                    files))
           (files (if tag-edit-ignore-files-function
                      (cl-remove-if tag-edit-ignore-files-function files)
                    files)))
      (switch-to-buffer buffer)
      (tag-edit-mode)
      (tag-edit-buffer-insert-files files)
      (goto-char (point-min)))))

(defun tag-edit (&optional files)
  "Edit the tags of FILES. If no files are provided, attempt to \"do what you mean\"; editing either the marked files or file at point if we're in a dired buffer, or prompting the user for a file if we're not."
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
  "Edit the tags of the files marked in this dired buffer, or if none are marked, edit the tags of the file at point.

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

;; (font-lock-add-keywords 'tag-edit-mode '(("^[^:]+?:" . 'font-lock-constant-face)))
;; keyword constant string builtin function-name

;;; keymap

(defvar tag-edit-mode-map
  (let ((map (make-sparse-keymap "Tag-Edit-Mode")))
    (define-key map (kbd "C-c C-c") 'tag-edit-write-file-tags)
    (define-key map (kbd "C-x C-s") 'tag-edit-write-all-file-tags)
    (define-key map (kbd "C-c C-k") 'tag-edit-revert-file-tags) ; FIX
    
    (define-key map (kbd "TAB") 'tag-edit-toggle-file-visibility) ; FIX
    (define-key map (kbd "M-n") 'tag-edit-next-field) ; FIX
    (define-key map (kbd "M-p") 'tag-edit-previous-field) ; FIX
    (define-key map (kbd "C-c C-n") 'tag-edit-next-file)
    (define-key map (kbd "C-c C-p") 'tag-edit-previous-file)

    (define-key map (kbd "C-c C-t") 'tag-edit-preview-file) ; FIX
    (define-key map (kbd "C-c C-o") 'tag-edit-open-file-in-external-editor) ; FIX
    map)
  "Keymap for `tag-edit-mode'.")

;;; define the mode

;;;###autoload
(define-derived-mode tag-edit-mode emacs-lisp-mode "Tag-Edit"
  "Major mode for editing file tags."
  (use-local-map tag-edit-mode-map)
  ;; (add-to-list 'after-change-functions 'tag-edit-after-change-function)
  (run-hooks 'tag-edit-mode-hook)
  (cursor-intangible-mode 1)
  (tag-edit-update-header))

(provide 'tag-edit)
;;; tag-edit.el ends here
