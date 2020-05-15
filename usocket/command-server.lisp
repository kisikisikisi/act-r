;; ACT-R Model: Distraction Sub-system
;; command-server.lisp
;; Last updated: 20/02/2020
;; Connect to Chrome extension to receiving a new command for the model

(define-condition stop-server (condition) ())

(defun make-server-socket (port)
  (usocket:socket-listen "localhost" port :reuseaddress t))

(defun dispose (server)
  (format t "close...~%")
  (usocket:socket-close server))

(defun accept-client (server)
  (prog1 (usocket:socket-accept server)
    (format t "accept a client!~%")))

(defun trim-input (input)
;  (string-trim " ^M" input))
  (string-trim '(#\Space #\newline #\tab #\e #\Cr #\Lf) input))

(defun echo-input (client-stream input)
  (format t " echo to client!~%")
  (format t "~A~%" (trim-input input))
  (format client-stream "~A~%" (trim-input input))
  (force-output client-stream))

(defun input= (input expected)
  (string= (trim-input input) expected))

(defun handle-client (client)
  (with-open-stream (stream (usocket:socket-stream client))
      (loop for input = (read-line stream nil nil)
         while input
         if (input= input "stop-server") do
           (error 'stop-server)
         else do
           (if (input= input "run-next")
           (echo-input stream (run-next 1))
           (echo-input stream input)))
      t))

(defun server-start (&key (port 80))
  (let ((server-sock (make-server-socket port)))
    (handler-case
        (loop while (handle-client (accept-client server-sock)))
      (stop-server () (format t "exit...~%"))
      (condition () (format t "unexpected exit!~%")))
    (dispose server-sock)))
