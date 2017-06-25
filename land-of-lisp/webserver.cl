;;;; Tiny webserver

;;; EXAMPLE HEADERS

;;; 1.
;; GET /lolcats.html HTTP/1.1
;; Host: localhost:8080
;; User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
;; Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8 Accept-Language: en-us,en;q=0.5
;; Accept-Encoding: gzip,deflate
;; Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
;; Keep-Alive: 300
;; Connection: keep-alive

;;; 2.
;; POST /login.html HTTP/1.1
;; Host: www.mywebsite.com
;; User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
;; Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
;; Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
;; Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 300
;; Connection: keep-alive
;; Content-Length: 39
;;;

;; Decode HTML escape characters
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\Space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;; Decode a list of parameters
(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

;; Parse the first line of the request header.
;; Looks something like `GET /lolcats.html HTTP/1.1`
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

;; Parse the rest of the headers
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

;; Parse request body
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

;; Serve :fire:
(defun serve (request-handler)
  (let ((socket (socket-server 3030)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))


;; Simple request handler
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name? <input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))
