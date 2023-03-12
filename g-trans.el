;;; g-trans.el — google translate inside emacs

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

;; g-trans.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with g-trans.el.
;; If not, see <https://www.gnu.org/licenses/>.

(require 'request)

;; Magic URL
(defconst g-trans-base-url "https://translate.googleapis.com/translate_a/single")

;; Main function
;; ---------------------------------------------------------------
(defun g-trans-translate (query &optional source-lang target-lang)
  "Translates given query from source-lang to target-lang.
If source-lang/target-lang not provided, then
g-trans-default-source-lang/g-trans-default-target-lang
global settings are used, if they are not provieded either,
then default values are ru/en."
  (when (not (boundp 'g-trans-default-source-lang))
    (setq g-trans-default-source-lang "ru"))
  (when (not (boundp 'g-trans-default-target-lang))
    (setq g-trans-default-target-lang "en"))
  (interactive (list
                 (read-from-minibuffer "Query: ")
                 (read-from-minibuffer
                   (format "Source lang[%s]: " g-trans-default-source-lang))
                 (read-from-minibuffer
                   (format "Target lang[%s]: " g-trans-default-target-lang))))
  (when (string= source-lang "")
    (set 'source-lang g-trans-default-source-lang))
  (when (string= target-lang "")
    (set 'target-lang g-trans-default-target-lang))
  (g-trans--make-request query source-lang target-lang))

;; Request to google
;; ---------------------------------------------------------
(defun g-trans--make-request (query source-lang target-lang)
  "Make request to googleapi.
client=gtx and dt=t are necessary 'magic' params"
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
  "There is no documentation.
But successful json-response from googleapi looks smth like
[[[translated_result, ...]]]
so just recursively get this result"
  (if (not (list data))
    "No results"
    (if (stringp data)
      data
      (g-trans--get-translate-result (aref data 0)))))

(provide 'g-trans)
;;; g-trans.el ends here
