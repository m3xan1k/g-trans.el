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
(require 'cl-lib)

(defconst g-trans--base-url "https://translate.googleapis.com/translate_a/single")

(defun g-trans--params->query-string (params)
  (concat "?" (mapconcat
               (lambda (pair)
                 (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))
               params
               "&")))

;; (cl-assert (string=
;;             (g-trans--params->query-string '((a . "1") (b . "2") (c . "3")))
;;             "?a=1&b=2&c=3"))

(defun g-trans--send-request (query)
  (let* ((url-request-method "GET")
         (params `((sl . "en")
                   (tl . "ru")
		   (client . "gtx")
		   (dt . "t")
		   (q . ,query)))
         (url (concat g-trans--base-url
                      (g-trans--params->query-string params))))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char url-http-end-of-headers)
      (let ((data (json-read)))
	(kill-buffer)
	data))))

(defun g-trans--get-translate-result (data)
  (when (not (list data)) "No results")
  (if (stringp data)
      data
      (g-trans--get-translate-result (aref data 0))))

(provide 'g-trans)
;;; g-trans.el ends here
