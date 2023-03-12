;;; g-trans.el --- google translate inside Emacs

;; Copyright (C) 2023 by Sergey Shevtsov

;; Author: Sergey Shevtsov <m3xan1k at duck.com>
;; Keywords: translation, dictionary
;; Version: 0.1

;; Package-Requires: ((emacs "27.1") (request "0.3.3"))

;; This file is NOT part of GNU Emacs.

;; g-trans.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Google translate inside Emacs

;;; Code:

(require 'request)

;; Magic URL
(defconst g-trans-base-url "https://translate.googleapis.com/translate_a/single")

(defvar g-trans-default-source-lang "ru")
(defvar g-trans-default-target-lang "en")

;; Request to google
;; ---------------------------------------------------------
(defun g-trans--make-request (query source-lang target-lang)
  "Make request to googleapi.
QUERY - string - word or phrase to translate.
SOURCE-LANG - string - source language code
like 'en' for english or 'es' for spanish.
TARGET-LANG - string - target language code.
There's no official documentation about which
languages are available."
  (request
    g-trans-base-url
    :params `(("sl" . ,source-lang)
               ("tl" . ,target-lang)
               ("q" . ,query)
               ("client" . "gtx")
               ("dt" . "t"))
    :parser 'json-read
    :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message (format "%s" (g-trans--get-translate-result data)))))))

;; Parse json response
;; ----------------------------------------
(defun g-trans--get-translate-result (data)
  "DATA - json response from googleapi.
There is no documentation.
But successful json-response from googleapi looks smth like
[[[translated_result, ...]]]
so just recursively get this result"
  (if (not (list data))
    "No results"
    (if (stringp data)
      data
      (g-trans--get-translate-result (aref data 0)))))

;; Main function
;; ---------------------------------------------------------------
(defun g-trans-translate (query &optional source-lang target-lang)
  "Translates given query from source-lang to target-lang.
QUERY - string - word or phrase to translate.
SOURCE-LANG - string - source language code
like 'en' for english or 'es' for spanish.
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
    (g-trans--make-request query source-lang target-lang)))

(provide 'g-trans)
;;; g-trans.el ends here
