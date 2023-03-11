(require 'request)

(setq g-trans-base-url "https://translate.googleapis.com/translate_a/single")

(defun g-trans-translate (query &optional source-lang target-lang)
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
  (g-trans-make-request query source-lang target-lang))

(defun g-trans-make-request (query source-lang target-lang)
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
                 (message (format "%s" (g-trans-get-translate-result data)))))))

(defun g-trans-get-translate-result (data)
  (if (not (list data))
    "No results"
    (if (stringp data)
      data
      (g-trans-get-translate-result (aref data 0)))))
