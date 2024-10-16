#lang racket

(require racket/tcp)
(require net/url)

(define server-port null)
(define server-ip null)
(define listener null) ;; Listener is set when server is initialized

;; Keeps track of connected clients
(define connected-clients-out empty)
(define connected-clients-in empty)
(define queue-clients-out empty)
(define queue-clients-in empty)

(define queue-client-threads empty) ;; List to track threads for queued clients; needs this to abe able to terminate them
(define client-threads empty) ;; List to track threads for active clients; needs this to abe able to terminate them

;; FUNCTION DEFINITION FOR handle-client-start
;; Port String -> Void
;; Purpose: Consumes an output stream and a player ("X" or "O"),
;;          and writes that player to the output stream; Used at
;;          the beginning of the game to initialize a singluar player
;; Stub: (define (handle-client-start 0 "X") void)

(define (handle-client-start out player)   
  (printf "Setting player: ~a\n" player)
  (write player out)
  (flush-output out)
  (printf "Set Player to: ~a\n" player))

;; FUNCTION DEFINITION FOR handle-client
;; Port Port -> Void
;; Purpose: Handles a singlular client's connection, sending and receiving moves from a client to the other client;
;;          each handle-client per client is called in a single thread
;; Stub: (define (handle-client 0 0) void)

(define (handle-client in out)
  (with-handlers ([exn:fail? (λ (exn)
                               (printf "Handle-Client Error: ~a\n" (exn-message exn))
                               ;; Kill the thread associated with an active client
                               (define active-thread-message (format "Killing active thread: ~a" current-thread))
                               (printf "~a\n" active-thread-message)
                               (kill-thread (current-thread))
                               (set! client-threads (filter (λ (thread)
                                                              (not (equal? thread (current-thread))))
                                                            client-threads)))]) 
                               
    (let loop ()
      (define msg (read in))
      (if (eof-object? msg)
          (begin
            (printf "EOF received, clearing server.\n")
            (clear-server)) 
          (begin
            (printf "Received Move: ~a\n" msg)
            (broadcast-message msg out)
            (printf "Sent Move: ~a\n" msg)
            (loop))))))  ;; Continue listening for messages

;; FUNCTION DEFINITION FOR handle-queued-client
;; Port Port -> Void
;; Purpose: Handles a singlular client's connection in the queue,
;;          removes one if it disconnects
;; Stub: (define (handle-queued-client 0 0) void)

(define (handle-queued-client in out)
  (with-handlers ([exn:fail? (λ (exn)
                               (printf "Handle-Queued-Client Error: ~a\n" (exn-message exn)))])
    (define msg (read in))
    (printf "Message from Queued Client: ~a" msg)
    (define queue-length (length queue-clients-out)) ;; For printing
    (if (eof-object? msg) ;; Check if EOF is received, happens when client disconnects
        (begin
          (set! queue-clients-in (filter (λ (c) (not (equal? c in))) queue-clients-in))
          (set! queue-clients-out (filter (λ (c) (not (equal? c out))) queue-clients-out))
          (set! queue-length (length queue-clients-out))
          (printf "EOF received, removing client from queue. ~a" queue-length)
          (printf " client(s) still in the queue.\n"))
        void)))

;; FUNCTION DEFINITION FOR update-queued-clients-thread
;; Void -> Void
;; Purpose: Runs in a seperate thread; detects if there is a change in the length of the queued clients,
;;          and sends a message to all queued clients that visually updates their position
;; Stub: (define (update-queued-clients-thread) void)

(define (update-queued-clients-thread)
  (local [(define pos 1)
          (define queue-length-before (length queue-clients-out))]
    (let loop ()
      (when (not (= (length queue-clients-out) queue-length-before)) ;; If there is a change in the length of the queued clients
        (for-each
         (λ (each-out)
           (write (string-append "Queue Position: " (number->string pos)) each-out) ;; Send the message to the queued clients
           (set! pos (+ 1 pos))
           (flush-output each-out))
         queue-clients-out)
        (set! pos 1)
        (set! queue-length-before (length queue-clients-out)))
      (loop))))

;; FUNCTION DEFINITION FOR accept-loop
;; Void -> Void
;; Purpose: Handles incoming client connections to the tcp listener on port 5656
;;          - accept-loop-queue: When the connected clients has a length of less than 2 AND there are queued clients able to join
;;                               -> Moves a queued client into the connected clients list
;;          - accept-loop-handler: When the connected clients has a length of less than 2 AND there are no queued clients able to join
;;                               -> Listens for a client to join and moves them into the connected clients list
;;          - add-to-queue: When the connected clients has a length of 2
;;                               -> Listens for a client to join and moves them into the queued clients list
;; Stub: (define (accept-loop) void)

(define (accept-loop)
  (local [
          (define (accept-loop-queue)

            ;; Moves first client in queue to play
            (define in (first queue-clients-in))
            (define out (first queue-clients-out))
            (set! queue-clients-in (rest queue-clients-in)) 
            (set! queue-clients-out (rest queue-clients-out))

            (write "Connecting..." out)
            (flush-output out)

            (define queue-length (length queue-clients-out)) ;; For printing
            (printf "A client from the queue has been moved into play. ~a" queue-length)
            (printf " client(s) still in the queue.\n")

            ;; Kill the thread associated with the queued client
            (define active-thread-message (format "Killing queue thread: ~a" (first queue-client-threads)))
            (printf "~a\n" active-thread-message)
            (kill-thread (first queue-client-threads))
            (set! queue-client-threads (rest queue-client-threads))

            (accept-loop-handler in out))

          (define (accept-loop-handler in out)
            ;; Add the client to the list of connected clients
            (set! connected-clients-out (append connected-clients-out (list out))) ;; Adds client output port to list
            (set! connected-clients-in (append connected-clients-in (list in))) ;; Adds client input port to list
      
            ;; Print out client connection status
            (printf "Client connected. Total clients: ~a\n" (length connected-clients-out))
            (cond [(= (length connected-clients-out) 1) 
                   (printf "Waiting for second connection...\n")]
                  [(= (length connected-clients-out) 2)
                   (printf "Setting players...\n")
                   (handle-client-start (list-ref connected-clients-out 0) "X")
                   (handle-client-start (list-ref connected-clients-out 1) "O")]) 
      
            ;; Start a new thread to handle this client and track it
            (define thread-id (thread (λ () (handle-client in out))))
            (set! client-threads (append client-threads (list thread-id))))

          (define (add-to-queue in-queue out-queue)
            (set! queue-clients-out (append queue-clients-out (list out-queue)))
            (set! queue-clients-in (append queue-clients-in (list in-queue)))
            (define queue-length (length queue-clients-out)) ;; For printing
            ;(define msg (string-append "You have been placed in the queue for the server on port " (number->string server-port) ".\nYou are number " (number->string queue-length) " in the queue"))
            ;(write msg out-queue)
            ;(flush-output out-queue)
              
            ;; Start a new thread to handle this queued client and track it
            (define thread-id (thread (λ () (handle-queued-client in-queue out-queue))))
            (set! queue-client-threads (append queue-client-threads (list thread-id)))
    
            (printf "A client has been queued: ~a" queue-length)
            (printf " Clients in the queue.\n"))]
    
    (cond 
      [(and (< (length connected-clients-out) 2) (> (length queue-clients-out) 0)) (accept-loop-queue)]
      [(tcp-listener? listener)
       (define-values (in out) (tcp-accept listener)) 
       (cond [(and (< (length connected-clients-out) 2) (= (length queue-clients-out) 0)) (accept-loop-handler in out)]
             [(= (length connected-clients-out) 2) (add-to-queue in out)])])

    (accept-loop)))

;; FUNCTION DEFINITION FOR broadcast-message
;; String Port -> Void
;; Purpose: Broadcasts a message to all of the clients' out streams in
;;          connected-clients-out except excluded-out
;; Stub: (define (broadcast-message "" 0) void)

(define (broadcast-message msg excluded-out)
  (for-each
   (λ (each-out)
     (when (not (equal? each-out excluded-out))  ;; Check if the output stream is not the excluded one
       (write msg each-out) ;; Send the message to the client
       (flush-output each-out)))
   connected-clients-out))

;; FUNCTION DEFINITION FOR clear-server
;; Void -> Void
;; Purpose: Clears all the server's information including the connected-clients' ports;
;;          is called when the game is over and a client clicks "r", causing an EOF
;; Stub: (define (clear-server) void)

(define (clear-server) 
  (with-handlers ([exn:fail? (λ (exn) (printf "Clear-Server Error: ~a\n" (exn-message exn)))])
    (define num-clients (length connected-clients-out)) ;; For printing
    (define num-clients-queue (length queue-clients-out)) ;; For printing

    (broadcast-message "DC" null)
    
    ;; Remove the specific client from the list
    (for-each close-input-port connected-clients-in)
    (for-each close-output-port connected-clients-out)
    
    (set! connected-clients-in empty)
    (set! connected-clients-out empty)
    (define print-msg (string-append "Client disconnected. Disconnected "
                                     (number->string num-clients)
                                     " clients.\n"
                                     (number->string num-clients-queue)
                                     " client(s) in the queue.\n"))
    (printf print-msg)
    (accept-loop)))

;; FUNCTION DEFINITION FOR start-server
;; Void -> Void
;; Purpose: Starts a server on port 5656 for clients to connect to;
;;          adds 2 clients to the game, then starts adding to the queue
;; Stub: (define (start-server) void)

(define (start-server)
  (with-handlers ([exn:fail? (λ (exn)
                               (printf "Please enter a valid IP/port. Try again. Error Message: ~a\n" (exn-message exn))
                               (start-server))])
    (local [(define (get-server-ip)
              (flush-output) ; Ensure the prompt is displayed before waiting for input
              (define server-ip-response (read-line (current-input-port) 'any))
              (cond [(string=? server-ip-response "Localhost") (set! server-ip "127.0.0.1")]
                    [(string=? server-ip-response "Public") (set! server-ip (get-local-ip))]
                    [else (begin (printf "Invalid response; try again: ") (get-server-ip))]))

            (define (get-local-ip)
              (printf "Enter your Local IP: ")
              (flush-output) ; Ensure the prompt is displayed before waiting for input
              (define server-ip-response (read-line (current-input-port) 'any))
              server-ip-response)

            (define (get-server-port)
              (flush-output) ; Ensure the prompt is displayed before waiting for input
              (define server-port-response (string->number (read-line (current-input-port) 'any)))
              (set! server-port server-port-response))]

      (printf "Do you want the server hosted on your Local IP (publically accessible through your Public IP) or on Localhost (only accessible from your machine)?\nEnter Public or Localhost (Case matters): ")
      (get-server-ip)
      (printf "Enter Port on ~a you want to use: " server-ip)
      (get-server-port)

      (thread (λ () (update-queued-clients-thread)))
    
      (set! listener (tcp-listen server-port 4 #f server-ip))  ;; The server will listen on all IP addresses that are assigned to the host device
      ;; Any client that knows the server's IP address can connect to it
      (printf "Server started on port ~a\n" server-port)
      (accept-loop))))

(printf
"              -- WELCOME TO THE TIC TAC TOE CLIENT --\n
If you are hosting the server on your Local IP, it will be publically\n
accessable through your Public IP. However, Port Forwarding is needed\n
to allow this. Once you have Port Forwarded a port on your router,\n
you will have to enter your Local IP. The Local IP of your machine can\n
be found next to IPv4 when the command ipconfig is run on Command Prompt.\n
Lastly, enter in the port that has been forwarded.\n\n
On the client side, to connect to the server, all you have to do is\n
enter the server's PUBLIC IP ADDRESS
(not local, can be found anywhere online ex. https://whatismyipaddress.com)
along with the port that you ran the server on.\n
--------------------------------------------------------------------------\n\n")
(start-server) ;; Starts the server

;; --------------------------------------------------------
;                    FUNCTION ORDER
;; --------------------------------------------------------

;; FUNCTION DEFINITION FOR handle-client-start

;; FUNCTION DEFINITION FOR handle-client

;; FUNCTION DEFINITION FOR handle-queued-client

;; FUNCTION DEFINITION FOR accept-loop

;; FUNCTION DEFINITION FOR broadcast-message

;; FUNCTION DEFINITION FOR clear-server

;; FUNCTION DEFINITION FOR start-server
