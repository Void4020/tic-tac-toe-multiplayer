#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)
(require racket/tcp)
;; -------------------------------------------- CONSTANTS --------------------------------------------

(define SIZE 900) ;; can range from [300,900]
(define SIZE-SCALED (- SIZE (/ SIZE 4))) ;; To make the grid a little smaller than the MTS; bounding box is 1/8 on each side
(define MTS (empty-scene SIZE SIZE "black"))

(define blue-x (make-color 66 135 245))
(define red-o (make-color 250 160 160))

(define gray-opaque (make-color 255 255 255 100))
(define bt-blue (make-color 0 122 255))
(define bt-blue-opaque (make-color 0 122 255 200))
(define bt-gray (make-color 158 158 158))
(define bt-gray-opaque (make-color 158 158 158 200))
(define bt-green (make-color 76 175 80))
(define bt-green-opaque (make-color 76 175 80 200))

(define text-color "white")

(define online-play-button-color bt-blue)
(define local-play-button-color bt-green)
(define computer-play-button-color bt-gray)

(define vline (rectangle (/ SIZE-SCALED 25) SIZE-SCALED "solid" "white"))
(define hline (rectangle SIZE-SCALED (/ SIZE-SCALED 25) "solid" "white"))

(define GRID (place-image hline (/ SIZE 2) (* (/ SIZE 8) 5)
                          (place-image hline (/ SIZE 2) (* (/ SIZE 8) 3)
                                       (place-image vline (* (/ SIZE 8) 3) (/ SIZE 2)
                                                    (place-image vline (* (/ SIZE 8) 5) (/ SIZE 2) MTS)))))
(define alpha-beta-pruning? true) ;; toggle true or false whether or not you want alpha beta pruning
(define game-mode "start-screen")
;; Local - Two people on the same computer
;; Online - Two people online
;; Computer - One person versus the computer (Different difficulties)

(define difficulty-level 1)
;; Difficulty levels for the computer
;; 0 - Random
;; 1 - Easy Minimax (1 step ahead)
;; 2 - Medium Minimax (2 steps ahead)
;; [3-9] - Hard Minimax ([3-9] steps ahead)
;; - 9 is the hardest and the greatest depth possible

(define X 1)  ;; Player move
(define O 0)  ;; Computer move
(define B 2)  ;; Blank square

(define BLANK (list B B B 
                    B B B 
                    B B B))

; GAME OVER TEXTS
(define draw-text (overlay (text/font "Draw." (floor (/ SIZE 10)) "gray" "Gill Sans" 'swiss 'normal 'bold #f) (rectangle SIZE-SCALED (/ SIZE 10) "solid" gray-opaque)))
(define xwin-text (overlay (text/font "X WINS!" (floor (/ SIZE 10)) "blue" "Gill Sans" 'swiss 'normal 'bold #f) (rectangle SIZE-SCALED (/ SIZE 10) "solid" gray-opaque)))
(define owin-text (overlay (text/font "O WINS!" (floor (/ SIZE 10)) "red" "Gill Sans" 'swiss 'normal 'bold #f) (rectangle SIZE-SCALED (/ SIZE 10) "solid" gray-opaque)))

;; BUTTON COORDS
(define button-width (/ SIZE 3))
(define button-height (/ SIZE 12))
(define button-x (/ SIZE 2))

(define computer-button-center-y (- SIZE (+ (/ SIZE 5) (/ SIZE 10))))
(define local-button-center-y (- SIZE (/ SIZE 5)))
(define online-button-center-y (- SIZE (+ (/ SIZE 5) (* 2 (/ SIZE 10)))))

;; WIN-ORDERS TO DETERMINE WIN

(define COLS (list (list 0 3 6)
                   (list 1 4 7)
                   (list 2 5 8)))

(define ROWS (list (list 0 1 2)
                   (list 3 4 5)
                   (list 6 7 8)))

(define DIAGS (list (list 0 4 8)
                    (list 2 4 6)))

(define WIN-ORDERS (append ROWS COLS DIAGS))
;; ---------------------------------------------------------------------------------------------------
;; DATA DEFINITIONS 

;; ws is a (make-ws board Boolean Natural)
(define-struct ws (board turn depth))

;; interp.
;;  - board is a board (list of 9 naturals)
;;  - turn is:
;;     - either O or X (0 or 1) depending on who's turn it is,
;;     - B (2) if the game is over (draw)
;;     - 3 if the game is over (X wins)
;;     - 4 if the game is over (O wins)
;;     - -1 during the start screen
;;  - depth is a natural, signifying how many moves ahead 
;; Template:
#;
(define (fn-for-ws ws)
  (...
   (... ws-board ws)
   (... ws-turn ws)
   (... ws-depth ws)))

;; Board is a list of 9 Naturals.
;; Interp. a tic tac toe board. positions 0-2 are row 1, 3-5 are row 2, and 6-8 are row 3. 
;; each position is a 0, 1, or 2. 0 is an O, 1 is an X, and 2 is a B.
;; Template:
#;
(define (fn-for-board board)
  (cond
    [(empty? board) ...]
    [else
     (... first board)
     (fn-for-board (rest board))]))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTION DEFINITIONS 

;; FUNCTION DEFINITION FOR get-win-text
;; WorldState -> Image
;; Purpose: Consumes a world state, outputs the win text based on the turn number (see ws turn interp.)
;; Stub: (define (get-win-text MT-ws) empty-image)

(define (get-win-text ws)
  (cond [(= (ws-turn ws) 2) draw-text]   ;; 2 (draw)
        [(= (ws-turn ws) 3) xwin-text]   ;; 3 (X win)
        [(= (ws-turn ws) 4) owin-text]   ;; 4 (O win)
        [else empty-image]))             ;; 0 or 1 (player or computer move)))                   

;; FUNCTION DEFINITION FOR place-letter
;; Board Naturl Natural -> Board
;; Purpose: Given a board, the position to place a letter as a natural, and the value of the letter that should be placed,
;; return the changed board
;; Stub: (define (place-letter BLANK 0 0) board)

(define (place-letter board pos value)
  (append (take board pos)
          (list value)
          (drop board (add1 pos))))

;; FUNCTION DEFINITION FOR board-place
;; WorldState Natural -> WorldState
;; Purpose: Consumes a world state and an index, and places a letter in the correct list index
;; Stub: (define (board-place MT-ws 0) MT-ws)

(define (board-place ws index)
  (if (and (not (negative? index)) (= (list-ref (ws-board ws) index) B))
      (make-ws 
       (place-letter (ws-board ws) index (ws-turn ws))
       (if (= (ws-turn ws) O) X O)
       (ws-depth ws))
      ws))

;; FUNCTION DEFINITION FOR get-restart-text
;; WorldState -> Image
;; Purpose: Consumes a world state, produces an emtpy image if the game is still going
;; and produces "Press r to restart" if the game is over
;; Stub: (define (get-restart-text MT-ws) empty-image)

(define (get-restart-text ws)
  (cond [(> (ws-turn ws) 1) (text/font "Press 'r' to go home" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)]
        [other-client-dc? (text/font "Returning home..." (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)]
        [else empty-image]))

(define connecting-text "Connecting...")

(define (get-turn-text ws) 
  (cond
    [other-client-dc? empty-image]
    [(or (= -10 player-turn) (= -10 opponent-turn)) (text/font connecting-text (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)]
    [(= (ws-turn ws) player-turn) (text/font "Your Turn!" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)]
    [(= (ws-turn ws) opponent-turn) (text/font "Opponent's Turn!" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)]
    [else empty-image]))
  
;; FUNCTION DEFINITION FOR draw-letters
;; Board Natural -> Image
;; Purpose: Consumes a board and a natural (accumulator) and draws the letters onto the tic tac toe board
;; Stub: (define (draw-letters empty 0) empty-image)

(define (draw-letters board)
  (local [(define (draw-letters-acc board acc)  ;; acc is the list position; starts at 0
            (local [(define quarter (/ SIZE 4))     
                    (define (letter-select n)   ;; given the number in the board, returns what it should be as a string
                      (if (= n X)
                          (text/font "X" (floor (/ SIZE 5)) blue-x "Gill Sans" 'swiss 'normal 'bold #f)
                          (text/font "O" (floor (/ SIZE 5)) red-o "Gill Sans" 'swiss 'normal 'bold #f)))]
              (cond [(empty? board) GRID]
                    [(= (first board) B) (draw-letters-acc (rest board) (+ acc 1))]
                    [else (place-image (letter-select (first board))
                                       (+ quarter (* quarter (modulo acc 3)))
                                       (+ quarter (* quarter (floor (/ acc 3))))
                                       (draw-letters-acc (rest board) (+ acc 1)))])))]
    (draw-letters-acc board 0)))

;; FUNCTION DEFINITION FOR mouse-pos-to-index
;; Natural Natural -> Natural or -1
;; Purpose: Consumes a mouse x and mouse y in eighths, and returns the correct index [0,9] that
;; index corresponds to (-1 if out of bounds)
;-----------------------
;; out of bounds ([0,1] or [8,9]) -> -1
;; [2,3] [2,3] -> 0
;; [4,5] [2,3] -> 1
;; [6,7] [2,3] -> 2

;; [2,3] [4,5] -> 3
;; [4,5] [4,5] -> 4
;; [6,7] [4,5] -> 5

;; [2,3] [6,7] -> 6
;; [4,5] [6,7] -> 7
;; [6,7] [6,7] -> 8
;-----------------------
;; Stub: (define (mouse-pos-to-index 0 0) 0)

(define (mouse-pos-to-index x y) ;; takes in mouse x and mouse y in eighths
  (local [(define (xy-to-index x y-index)
            (cond [(or (= x 2) (= x 3)) y-index]
                  [(or (= x 4) (= x 5)) (+ 1 y-index)]
                  [(or (= x 6) (= x 7)) (+ 2 y-index)]   
                  [else -1]))]                             ; out of bounds
    (cond [(or (= y 2) (= y 3)) (xy-to-index x 0)]
          [(or (= y 4) (= y 5)) (xy-to-index x 3)]
          [(or (= y 6) (= y 7)) (xy-to-index x 6)]
          [else -1])))                                     ; out of bounds

;; FUNCTION DEFINITION FOR computer-turn
;; WorldState -> WorldState
;; Purpose: Consumes a world state and returns a world state where the computer has moved
;; depending on difficulty level
;; Stub: (define (computer-turn MT-ws) MT-ws)

(define (computer-turn ws)
  (cond [(= (ws-depth ws) 0) (random-computer-move ws)]
        [alpha-beta-pruning? (minimax-abp ws)]
        [else (minimax ws)]))

;; FUNCTION DEFINITION FOR random-computer-move
;; WorldState -> WorldState
;; Purpose: Consumes a world state and returns a world state where the computer has moved [diff = 0]
;; Stub: (define (random-computer-move MT-ws) MT-ws)

(define (random-computer-move ws)
  (local [(define random-pos (random 9))]
    (cond [(= (list-ref (ws-board ws) random-pos) B) ;; if blank
           (board-place ws random-pos)]
          [else (random-computer-move ws)])))

;; FUNCTION DEFINITION FOR minimax
;; WorldState -> WorldState
;; Purpose: Consumes a world state and returns a world state where the computer has moved [diff = 1 -> 9]
;; using the minimax algorithm to determine the best move
;; Stub: (define (minimax MT-ws) MT-ws)

(define (minimax ws)
  (local [
          (define (next-moves ws) ;; generates a list of WorldStates, each WorldState is a possible move for the current player's turn
            (local [(define (make-move-at index) 
                      (make-ws (place-letter (ws-board ws) index (ws-turn ws))
                               (if (= (ws-turn ws) X) O X)
                               (ws-depth ws)))
                    (define (indexes-of-blanks board)
                      (filter (λ (i) (= (list-ref board i) B)) (list 0 1 2 3 4 5 6 7 8)))]
              (map make-move-at (indexes-of-blanks (ws-board ws)))))
          
          (define (search ws depth depth-acc)
            (local [
                    (define (tree-end ws) ;; returns true or false based on whether the game is over or the depth is reached
                      (cond [(or (> (game-over-state ws) 1) (= (ws-depth ws) depth-acc)) true] ; game over or depth reached
                            [else false]))                                         ; 0 or 1 (player or computer move)))
          
                    (define (utility ws) ;; returns the utility value of the WorldState when the simultaed game is over
                      (cond [(= (game-over-state ws) 4) 1]  ;o wins
                            [(= (game-over-state ws) 3) -1] ;x wins
                            [(= (game-over-state ws) 2) 0]  ;draw
                            [(= (ws-depth ws) depth-acc) (/ (random 100) 100)]))] ;depth reached
      
              (cond [(tree-end ws) (utility ws)]
                    [(= (ws-turn ws) X) (apply min (map (λ (each-ws) (search each-ws depth (+ depth-acc 1))) (next-moves ws)))]     ;find best player move
                    [(= (ws-turn ws) O) (apply max (map (λ (each-ws) (search each-ws depth (+ depth-acc 1))) (next-moves ws)))])))] ;find best computer move

    (argmax (λ (each-ws) (search each-ws (ws-depth each-ws) 1)) (next-moves ws))))

;; FUNCTION DEFINITION FOR minimax-abp
;; WorldState -> WorldState
;; Purpose: Consumes a world state and returns a world state where the computer has moved [diff = 1 -> 9]
;; using the minimax alpha beta pruining algorithm to determine the best move
;; Stub: (define (minimax-abp MT-ws) MT-ws)

;; NOTE: Without alpha-beta pruning, the first move on difficulty nine takes about 8 seconds to complete. With alpha-beta pruning, that first move takes around 1-2 seconds.

(define (minimax-abp ws)
  (local [
          (define (next-moves ws)
            (local [(define (make-move-at index) 
                      (make-ws (place-letter (ws-board ws) index (ws-turn ws))
                               (if (= (ws-turn ws) X) O X)
                               (ws-depth ws)))
                    (define (indexes-of-blanks board)
                      (filter (λ (i) (= (list-ref board i) B)) (list 0 1 2 3 4 5 6 7 8)))]
              (map make-move-at (indexes-of-blanks (ws-board ws)))))
          
          (define (search ws depth depth-acc alpha beta)
            (local [
                    (define (tree-end ws)
                      (cond [(or (> (game-over-state ws) 1) (= (ws-depth ws) depth-acc)) true]
                            [else false]))
          
                    (define (utility ws)
                      (cond [(= (game-over-state ws) 4) 1]  ;o wins
                            [(= (game-over-state ws) 3) -1] ;x wins
                            [(= (game-over-state ws) 2) 0]  ;draw
                            [(= (ws-depth ws) depth-acc) (/ (random 100) 100)])) ;depth reached

                    (define (minimize moves alpha beta) ;evalutes children and returns the MIN score
                      (cond [(empty? moves) beta]
                            [else
                             (local [(define score (search (first moves) depth (+ depth-acc 1) alpha beta))]
                               (if (<= beta alpha)
                                   alpha  ; prune, knows O will avoid the branch anyways
                                   (minimize (rest moves) alpha (min beta score))))]))

                    (define (maximize moves alpha beta) ;evalutes children and returns the MAX score
                      (cond [(empty? moves) alpha]
                            [else
                             (local [(define score (search (first moves) depth (+ depth-acc 1) alpha beta))]
                               (if (>= alpha beta)
                                   beta  ; prune, knows X will avoid the branch anyways
                                   (maximize (rest moves) (max alpha score) beta)))]))]
              
              (cond 
                [(tree-end ws) (utility ws)]
                [(= (ws-turn ws) X) (minimize (next-moves ws) alpha beta)]
                [(= (ws-turn ws) O) (maximize (next-moves ws) alpha beta)])))]
    
    (argmax (λ (each-ws) (search each-ws (ws-depth each-ws) 1 -1000000 1000000)) (next-moves ws))))

;; FUNCTION DEFINITION FOR wins?
;; Board Turn -> Boolean
;; Purpose: Consumes a board and a turn and returns true if that the current player has won
;; else returns false
;; Stub: (define (wins? empty O) false)

(define (wins? board turn)
  (local [(define (get-positions orders)                 ; gets each value of the order positions
            (map (λ (pos) (list-ref board pos)) orders))
          (define (check-order orders)                   ; checking if in each of the win-orders, there is all of the current turn's letter
            (andmap (λ (val) (= val turn))
                    (get-positions orders)))]
    (ormap check-order WIN-ORDERS)))

;; FUNCTION DEFINITION FOR game-over-state
;; WorldState -> Natural
;; Purpose: Consumes a world state and returns 0 if still playing, 2 if draw, 3 if X win, and 4 if O win;
;; used for the handle-tick function to keep checking if there is a winner
;; Stub: (define (game-over-state MT-ws) 0)

(define (game-over-state ws)
  (cond [(not (string=? game-mode "start-screen"))
         (cond [(wins? (ws-board ws) X) (begin (set! game-over? true) 3)]
               [(wins? (ws-board ws) O) (begin (set! game-over? true) 4)]
               [(not (member B (ws-board ws))) 2]
               [else 0])]))

;; FUNCTION DEFINITION FOR main
;; WorldState -> WorldState
;; Purpose: Consumes a starting world state and calls the big-bang function on it
;; Stub: (define (main MT-ws) MT-ws)

(define (main ws) 
  (big-bang ws
    (on-tick handle-tick)
    (to-draw render-board)
    (on-mouse handle-mouse)
    (on-key handle-key)))

;; FUNCTION DEFINITION FOR handle-tick
;; WorldState -> WorldState
;; Purpose: Consumes a world state and checks each tick if the game is over; if it is,
;; it correctly updates the world state's turn
;; Stub: (define (handle-tick MT-ws) MT-ws)

(define (handle-tick ws)
  (local [(define check-game-state (game-over-state ws))
          
          (define (handle-tick-computer ws check-game-state)
            (cond [(> check-game-state O) (make-ws (ws-board ws) check-game-state (ws-depth ws))] ; game over (game-state > 0), update game-state
                  [(= (ws-turn ws) O) (computer-turn ws)]  ; computer's turn
                  [else ws]))  ; player's turn

          (define (handle-tick-local ws check-game-state)
            (cond [(> check-game-state O) (make-ws (ws-board ws) check-game-state (ws-depth ws))] ; game over (game-state > 0), update game-state
                  [else ws]))  ; either player's turn

          (define (handle-tick-online ws check-game-state)
            (cond
              [(> check-game-state O) (make-ws (ws-board ws) check-game-state (ws-depth ws))] ; game over (game-state > 0), update game-state
              [(and (= (ws-turn ws) opponent-turn) (> index-received -1)) (board-place ws index-received)] ; received an update from opponent client, update board
              [else ws]))]  ; Player's turn
          
    (cond
      [(string=? game-mode "start-screen") ws]
      [(string=? game-mode "computer") (handle-tick-computer ws check-game-state)]
      [(string=? game-mode "local") (handle-tick-local ws check-game-state)]
      [(string=? game-mode "online") (handle-tick-online ws check-game-state)])))

;; FUNCTION DEFINITION FOR handle-mouse
;; WorldState Natural Natural MouseEvent -> WorldState
;; Purpose: Consumes a world state, x and y mouse position, and mouse event,
;; and computes and calls a function to place a letter in the correct list index when the mouse button is clicked
;; Stub: (define (handle-mouse ws x y event) MT-ws)

(define (handle-mouse ws x y event)
  (local [(define (mouse-pos-in-eighths n) ;; x or y coordinate -> coordinate but in 8ths of the screen
            (floor (+ 1 (/ n (/ SIZE 8)))))
          (define gridx (mouse-pos-in-eighths x))
          (define gridy (mouse-pos-in-eighths y))

          (define (handle-mouse-computer event gridx gridy ws)
            (cond [(and (mouse=? event "button-down") (= (ws-turn ws) X))
                   (board-place ws (mouse-pos-to-index gridx gridy))]
                  [else ws]))

          (define (handle-mouse-local event gridx gridy ws)
            (cond [(and (mouse=? event "button-down") (or (= (ws-turn ws) X) (= (ws-turn ws) O)))
                   (board-place ws (mouse-pos-to-index gridx gridy))]
                  [else ws]))

          (define (handle-mouse-online event gridx gridy ws)
            (local [(define index (mouse-pos-to-index gridx gridy))
                    (define index-valid? (and (> index -1) (= (list-ref (ws-board ws) index) B)))]
              (cond [(and (mouse=? event "button-down") (= (ws-turn ws) player-turn) index-valid?)
                     (begin (set! index-to-send index) (board-place ws index))]
                    [else ws])))]
    
    (cond
      [(string=? game-mode "start-screen") (handle-mouse-start-screen event x y ws)]
      [(string=? game-mode "computer") (handle-mouse-computer event gridx gridy ws)]
      [(string=? game-mode "local") (handle-mouse-local event gridx gridy ws)]
      [(string=? game-mode "online") (handle-mouse-online event gridx gridy ws)]))) 

(define (handle-mouse-start-screen event x y ws)
  (local [(define (mouse-within-online-play)  
            (and
             (> x (- button-x (/ button-width 2)))
             (< x (+ button-x (/ button-width 2)))
             (> y (- online-button-center-y (/ button-height 2)))
             (< y (+ online-button-center-y (/ button-height 2)))))

          (define (mouse-within-computer-play)  
            (and
             (> x (- button-x (/ button-width 2)))
             (< x (+ button-x (/ button-width 2)))
             (> y (- computer-button-center-y (/ button-height 2)))
             (< y (+ computer-button-center-y (/ button-height 2)))))

          (define (mouse-within-local-play)  
            (and
             (> x (- button-x (/ button-width 2)))
             (< x (+ button-x (/ button-width 2)))
             (> y (- local-button-center-y (/ button-height 2)))
             (< y (+ local-button-center-y (/ button-height 2)))))]
  
    (cond [(and (mouse=? event "button-down") (mouse-within-online-play)) (begin (set! game-mode "online") (printf "Entering Online Play...\n") (start-online-game) (make-ws BLANK X difficulty-level))]
          [(and (mouse=? event "button-down") (mouse-within-computer-play)) (begin (set! game-mode "computer") (printf "Entering Computer Play...\n") (make-ws BLANK X difficulty-level))]
          [(and (mouse=? event "button-down") (mouse-within-local-play)) (begin (set! game-mode "local") (printf "Entering Local Play...\n") (make-ws BLANK X difficulty-level))]
          [(mouse-within-online-play) (begin (set! online-play-button-color bt-blue-opaque) (set! computer-play-button-color bt-gray) (set! local-play-button-color bt-green))]
          [(mouse-within-computer-play) (begin (set! online-play-button-color bt-blue) (set! computer-play-button-color bt-gray-opaque) (set! local-play-button-color bt-green))]
          [(mouse-within-local-play) (begin (set! online-play-button-color bt-blue) (set! computer-play-button-color bt-gray) (set! local-play-button-color bt-green-opaque))]     
          [else (begin (set! online-play-button-color bt-blue) (set! computer-play-button-color bt-gray) (set! local-play-button-color bt-green) ws)]))) 

;; FUNCTION DEFINITION FOR handle-key
;; WorldState key -> WorldState
;; Purpose: Consumes a world state and a key, and
;; Stub: (define (handle-key MT-ws "") MT-ws)

(define (handle-key ws key)
  (local [(define key->num (string->number key))
          
          (define (handle-key-computer ws key key->num) 
            (cond 
              [(and (= (ws-turn ws) X) (number? key->num)) (make-ws (ws-board ws) (ws-turn ws) key->num)] ; Prevents difficulty change from happening when its not the players turn
              [(and (key=? key "r") (> (ws-turn ws) 1)) (begin (set! game-mode "start-screen") (make-ws BLANK 5 difficulty-level))] ; Clicking r when the game is over goes home
              [else ws]))

          (define (handle-key-local ws key)
            (cond 
              [(and (key=? key "r") (> (ws-turn ws) 1)) (begin (set! game-mode "start-screen") (make-ws BLANK 5 difficulty-level))] ; Clicking r when the game is over goes home
              [else ws]))

          (define (handle-key-online ws key)
            (cond 
              [(and (key=? key "r") (> (ws-turn ws) 1)) (begin (disconnect-client) (set! game-mode "start-screen") (make-ws BLANK 5 difficulty-level))] ; Clicking r when the game is over goes home
              [else ws]))]
    (cond
      [(key=? key "escape")
       (if (and (null? global-in) (false? global-out)) ;; If the computer isn't connected to a server
           (exit)
           (begin (disconnect-client) (exit)))]
      [(string=? game-mode "computer") (handle-key-computer ws key key->num)]
      [(string=? game-mode "local") (handle-key-local ws key)]
      [(string=? game-mode "online") (handle-key-online ws key)])))

;; FUNCTION DEFINITION FOR render-board
;; WorldState -> Image
;; Purpose: Consumes a world state, and renders all components to the screen
;; Stub: (define (render-board ws) empty-image)

(define (render-board ws)
  (local [(define local-play-button (overlay
                                     (text/font "- Local -" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                     (rectangle (/ SIZE 3) (/ SIZE 12) "solid" local-play-button-color)))

          (define computer-play-button (overlay
                                        (text/font "- Computer -" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                        (rectangle (/ SIZE 3) (/ SIZE 12) "solid" computer-play-button-color)))

          (define online-play-button (overlay
                                      (text/font "- Online -" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                      (rectangle (/ SIZE 3) (/ SIZE 12) "solid" online-play-button-color)))

          (define (render-start-screen)
            (place-image online-play-button (/ SIZE 2) (- SIZE (+ (/ SIZE 5) (* 2 (/ SIZE 10))))
                         (place-image computer-play-button (/ SIZE 2) (- SIZE (+ (/ SIZE 5) (/ SIZE 10))) 
                                      (place-image local-play-button (/ SIZE 2) (- SIZE (/ SIZE 5)) 
                                                   (place-image (text/font "Tic Tac Toe" (floor (/ SIZE 10)) text-color "Gill Sans" 'swiss 'normal 'bold #f) (/ SIZE 2) (- (/ SIZE 2) (/ SIZE 10)) MTS)))))

          (define (render-computer-game ws)
            (place-image (get-restart-text ws) (/ SIZE 2) (- SIZE (/ SIZE 16))
                         (place-image (text/font (string-append "Difficulty: " (number->string (ws-depth ws))) (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                      (/ SIZE 2)
                                      (/ SIZE 16)
                                      (place-image (get-win-text ws)
                                                   (/ SIZE 2)
                                                   (/ SIZE 2)
                                                   (draw-letters (ws-board ws))))))

          (define (render-local-game ws)
            (place-image (get-restart-text ws) (/ SIZE 2) (- SIZE (/ SIZE 16))
                         (place-image (text/font "Local Play" (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                      (/ SIZE 2)
                                      (/ SIZE 16)
                                      (place-image (get-win-text ws)
                                                   (/ SIZE 2)
                                                   (/ SIZE 2)
                                                   (draw-letters (ws-board ws))))))

          (define (render-online-game ws)
            (local [(define player-text "null")]
              (cond
                [(= player-turn X) (set! player-text "X")]
                [(= player-turn O) (set! player-text "O")])
    
              (place-image (get-turn-text ws) (/ SIZE 2) (- SIZE (/ SIZE 16))
                           (place-image (get-restart-text ws) (/ SIZE 2) (- SIZE (/ SIZE 16))
                                        (place-image (text/font (string-append "Online Play - You are " player-text) (floor (/ SIZE 20)) text-color "Gill Sans" 'swiss 'normal 'bold #f)
                                                     (/ SIZE 2)
                                                     (/ SIZE 16)
                                                     (place-image (get-win-text ws)
                                                                  (/ SIZE 2)
                                                                  (/ SIZE 2)
                                                                  (draw-letters (ws-board ws))))))))]
    (cond
      [(string=? game-mode "start-screen") (render-start-screen)]
      [(string=? game-mode "computer") (render-computer-game ws)]
      [(string=? game-mode "local") (render-local-game ws)]
      [(string=? game-mode "online") (render-online-game ws)])))

;; --------------------------------------------------------
;; Client

(define current-turn X)
(define player-turn -10) ;; Player starts out as -10, whoever connects first is deemed player X
(define opponent-turn -10)
(define start? false)
(define index-to-send -1) ;; On valid player-turn move, updates index-to-send to be that index if it is positive, then sets it back to -1
(define index-received -1) ;; On receiving of opponent move, updates index-received to be that index, then sets it back to -1
(define server-ip null)
(define server-port null)
(define global-in null)
(define global-out null)
(define other-client-dc? false)
(define game-over? false)

(define send-receive-loop-thread null)
(define receive-message-thread null)

;; FUNCTION DEFINITION FOR reset-game-state
;; Void -> Void
;; Purpose: Resets all the server's details to clear for a new game
;; Stub: (define (reset-game-state) void)

(define (reset-game-state)
  (set! index-to-send -1)
  (set! index-received -1)
  (set! start? false)
  (set! current-turn X)
  (set! player-turn -10)
  (set! opponent-turn -10)
  (set! server-ip null)
  (set! server-port null)
  (set! other-client-dc? false)
  (set! text-color "white")
  (set! game-over? false)

  (printf "Killing threads ~a and ~a\n" send-receive-loop-thread receive-message-thread)
  (kill-thread send-receive-loop-thread)
  (kill-thread receive-message-thread)
  
  (set! send-receive-loop-thread null)
  (set! receive-message-thread null))

;; FUNCTION DEFINITION FOR set-server-details
;; Void -> Void
;; Purpose: Sets all the server's details to in preparation for a new game
;; Stub: (define (set-server-details) void)

(define (set-server-details)
  (with-handlers ([exn:fail? (λ (exn)
                               (printf "Please enter a valid IP/port. Try again. Error Message: ~a\n" (exn-message exn))
                               (set-server-details))])
    (local [(define (set-server-ip)
              (printf "Enter server's IP address (Public IP or Localhost IP): ")
              (flush-output) ; Ensure the prompt is displayed before waiting for input
              (set! server-ip (read-line (current-input-port) 'any)))

            (define (set-server-port)
              (printf "Enter server port: ")
              (flush-output) ; Ensure the prompt is displayed before waiting for input
              (set! server-port (string->number (read-line (current-input-port) 'any))))]
    
      ;; Connects to the server
      (set-server-ip)
      (set-server-port)
      (set!-values (global-in global-out) (tcp-connect server-ip server-port)))))

;; FUNCTION DEFINITION FOR send-receive-loop
;; Void -> Void
;; Purpose: Defines the send & receive loop for a client connected to a server; called in a thread
;; Stub: (define (send-receive-loop) void)

(define (send-receive-loop) 
  (local [
          ;; Prompts user for a move, waits until they made a move, then sends it to the server
          (define (prompt-user)
            (printf "Your Move!\n")
              
            (let loop () ;; Waits until turn
              (unless (> index-to-send -1)
                (loop)))
              
            (send-message index-to-send) ;; Sends move to the server
            (set! index-to-send -1)
            (printf "turn: ~a\n" opponent-turn)
            (set! current-turn opponent-turn))

          ;; Sends an index of the player's move to the server
          (define (send-message index)
            (with-handlers ([exn:fail? (λ (exn)
                                         (printf "Send-Message Error: ~a\n" (exn-message exn)))])
              (define msg "")
              (cond [(= current-turn X) (set! msg (string-append "X moved to index: " (number->string index)))]  ;; Sends the value of msg to the server using the output stream (X turn)
                    [(= current-turn O) (set! msg (string-append "O moved to index: " (number->string index)))]) ;; Sends the value of msg to the server using the output stream (O turn)

              (write msg global-out)
              (printf "Sent move: ~a to output port: ~a\n" msg global-out)
              (flush-output global-out)))

          ;; Reads a message from the server
          (define (receive-message)
            (with-handlers ([exn:fail? (λ (exn)
                                         (printf "Disconnection Error: ~a\n" (exn-message exn)))])
              (let loop ()
                (define msg (read global-in))
                (unless (eof-object? msg)  ;; Only process if not EOF
                  (cond
                    ;; Only sends the player home if the other client disconnects mid-game
                    [(and (string=? msg "DC") (not game-over?)) (begin (set! other-client-dc? true)
                                                                       (printf "Other client disconnected -- returning home...\n")
                                                                       (sleep 1)
                                                                       (disconnect-client)
                                                                       (set! game-mode "start-screen")
                                                                       (make-ws BLANK 5 difficulty-level))]
                    [else 
                     (define index (string->number (substring msg (- (string-length msg) 1) (string-length msg))))
                     (handle-index msg index)]))
                (loop))))
          
          ;; Handles an incoming message
          (define (handle-index msg index)
            (printf "Received Move: ~a\n" msg)
            (printf "turn: ~a\n" player-turn)
            (set! index-received index)
            (set! current-turn player-turn))

          ;; Receives player setting message
          (define (receive-setting-message)
            (define assigned-player (read global-in))
            (if (or (string=? assigned-player "X") (string=? assigned-player "O"))
                (cond [(string=? assigned-player "X") (set-players X)]
                      [(string=? assigned-player "O") (set-players O)])
                ;; If doesn't read "X" or "O", read a Queue Position
                (begin (set! connecting-text assigned-player) ;; Only displays a part of the message
                       (printf (string-append assigned-player "\n"))
                       (receive-setting-message))))

          ;; Sets player-turn and opponent-turn
          (define (set-players assigned-player)
            (set! player-turn assigned-player)
            (cond [(= assigned-player X) (begin (set! opponent-turn O) (set! text-color blue-x) (printf "Set this player to X\n"))]
                  [(= assigned-player O) (begin (set! opponent-turn X) (set! text-color red-o) (printf "Set this player to O\n"))]))] 

    ;; The client send & receive loop
    (define (client-loop)
      
      (if start?
          (receive-setting-message)
          void)
        
      (if (and start? (= player-turn X))         ;; Runs only for the first player; makes sure player 1 (X) goes first
          (begin
            (set! start? false) 
            (prompt-user))
          (set! start? false))

      (printf "waiting for message...\n")        ;; Repeats the loop to send more moves

      (set! receive-message-thread
            (thread (λ () (receive-message))))   ;; Starts a thread to handle receiving messages

      (let loop ()                               ;; Waits until turn
        (unless (= current-turn player-turn)
          (loop)))

      (prompt-user)                              ;; Prompts user; sends message
      
      (client-loop))                             ;; Recurses

    (client-loop))) ;; Trampoline Call

;; FUNCTION DEFINITION FOR start-online-game
;; Void -> Void
;; Purpose: Starts an online game by setting server details and running the send-receive-loop in a thread
;; Stub: (define (start-online-game) void)

(define (start-online-game)
  (set! start? true)
  (set-server-details)
  (printf "Connected to IP: ~a\n" server-ip)
  (set! send-receive-loop-thread (thread (λ () (send-receive-loop))))) ;; Starting sent-receive loop on a thread so game runs asynchronously

;; FUNCTION DEFINITION FOR disconnect-client
;; Void -> Void
;; Purpose: Disconnects the client from the server by resetting the game state and closing its I/O ports
;; Stub: (define (disconnect-client) void)

(define (disconnect-client)
  (reset-game-state)
  ;; Close the input and output ports
  (close-input-port global-in)
  (close-output-port global-out)
  (printf "Disconnected from server\n"))

;; --------------------------------------------------------
  
(define START (make-ws BLANK -1 difficulty-level))
(main START) ;; Forming the game window

;; --------------------------------------------------------
;                    FUNCTION ORDER
;; --------------------------------------------------------

;; FUNCTION DEFINITION FOR get-win-text

;; FUNCTION DEFINITION FOR board-place

;; FUNCTION DEFINITION FOR get-restart-text

;; FUNCTION DEFINITION FOR draw-letters

;; FUNCTION DEFINITION FOR mouse-pos-to-index

;; FUNCTION DEFINITION FOR computer-turn

;; FUNCTION DEFINITION FOR random-computer-move

;; FUNCTION DEFINITION FOR minimax

;; FUNCTION DEFINITION FOR wins?

;; FUNCTION DEFINITION FOR game-over-state

;; FUNCTION DEFINITION FOR main

;; FUNCTION DEFINITION FOR handle-tick

;; FUNCTION DEFINITION FOR handle-mouse

;; FUNCTION DEFINITION FOR handle-key

;; FUNCTION DEFINITION FOR render-board

;; -- CLIENT CODE --

;; FUNCTION DEFINITION FOR reset-game-state

;; FUNCTION DEFINITION FOR set-server-details

;; FUNCTION DEFINITION FOR send-receive-loop

;; FUNCTION DEFINITION FOR start-online-game

;; FUNCTION DEFINITION FOR disconnect-client