;;; g-trans.el --- google translate inside Emacs

;; Copyright (C) 2023 by Sergey Shevtsov

;; Author: Sergey Shevtsov <m3xan1k at duck.com>
;; Keywords: translation, dictionary
;; Version: 0.2

;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; g-trans.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; g-trans.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with g-trans.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Google translate inside Emacs

;;; Code:

(require 'url)
(require 'json)

(defconst g-trans--base-url "https://translate.googleapis.com/translate_a/single")
(defvar g-trans-default-source-lang "ru")
(defvar g-trans-default-target-lang "en")

;; Alist to query string
;; --------------------------------------------
(defun g-trans--params-to-query-string (params)
  "Encode alist to query string.
PARAMS - alist - key-value pairs of GET params."
  (concat "?"
	  (mapconcat
	   (lambda (pair)
	     (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))
	   params
	   "&")))

;; Send http request
;; ---------------------------------------------------------
(defun g-trans--make-request (query source-lang target-lang)
  "Make request to googleapi.
QUERY - string - word or phrase to translate.
SOURCE-LANG - string - source language code(iso3166-1-alpha-2).
TARGET-LANG - string - target language code(iso3166-1-alpha-2).
There's no official documentation about which languages are available."
  (let* ((url-request-method "GET")
         (params `((sl . ,source-lang)
                   (tl . ,target-lang)
		   (client . "gtx")
		   (dt . "t")
		   (q . ,query)))
         (url (concat g-trans--base-url
                      (g-trans--params-to-query-string params))))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char url-http-end-of-headers)
      (let ((data (json-read)))
	(kill-buffer)
	data))))

;; Parse json response
;; ----------------------------------------
(defun g-trans--get-translate-result (data)
  "DATA - json response from googleapi.
There is no documentation.
But successful json-response from googleapi looks smth like
[[[translated_result, ...]]]
so just recursively get this result"
  (when (not (list data)) "No results")
  (if (stringp data)
      (print data)
      (g-trans--get-translate-result (aref data 0))))

;; Main function
;; ---------------------------------------------------------------
(defun g-trans-translate (query &optional source-lang target-lang)
  "Translates given query from source-lang to target-lang.
QUERY - string - word or phrase to translate.
SOURCE-LANG - string - source language code.
TARGET-LANG - string - target language code.

There's no official documentation about which
languages are available.

If source-lang/target-lang not provided,
then global defaults are used."
  (interactive (list
		(read-from-minibuffer "Query: ")
		(read-from-minibuffer
		 (format "Source lang[%s]: " g-trans-default-source-lang))
		(read-from-minibuffer
		 (format "Target lang[%s]: " g-trans-default-target-lang))))
  (let ((source-lang (if (string= source-lang "")
			 g-trans-default-source-lang
		       source-lang))
	(target-lang (if (string= target-lang "")
			 g-trans-default-target-lang
		       target-lang)))
    (g-trans--get-translate-result
     (g-trans--make-request query source-lang target-lang))))

(provide 'g-trans)
;;; g-trans.el ends here
