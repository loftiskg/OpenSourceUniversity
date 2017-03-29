;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname final_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; The Easiest Game Ever!  (make this more specific)

;; =================
;; Constants:
(define HEIGHT 500)
(define WIDTH 500)
(define PLAYER_ICON (rectangle 10 10 "solid" "green"))
(define COIN_IMG (ellipse 10 10 "solid" "yellow"))
(define MTS (empty-scene WIDTH HEIGHT))
(define SCORE_X_POS (- WIDTH (/ WIDTH 8)))
(define SCORE_Y_POS (+ HEIGHT 10))
(define WINNING_TEXT (text "You Win!" 36 "black"))
(define WINNING_TEXT_X_POS (/ WIDTH 2))
(define WINNING_TEXT_Y_POS (/ HEIGHT 2))
; c1 pos
(define C1_X_POS (* WIDTH .25))
(define C1_Y_POS (* HEIGHT .25))
; c2 pos
(define C2_X_POS (* WIDTH .75))
(define C2_Y_POS (* HEIGHT .25))
; c3 pos
(define C3_X_POS (* WIDTH .25))
(define C3_Y_POS (* HEIGHT .75))
; c4 pos
(define C4_X_POS (* WIDTH .75))
(define C4_Y_POS (* HEIGHT .75))

;; =================
;; Data definitions:

(define-struct player (x y score))
;; Player is (make_player Natural[0,WIDTH] Natural[0,HEIGHT] Natural[0,4])

(define P1 (make-player (/ WIDTH 2) (/ HEIGHT 2) 2)) ;player in the middle with score of 2

(define (fn-for-player p)
  (... (player-x p)        ; Natural[0,WIDTH]
       (player-y p)        ; Natural[0,HEIGHT]
       (player-score p)))  ; Natural[0,4]

;; Template Rules Used
;; - compound: composed of 3 components


(define-struct coin(x y available?))
;; coin is (make-coin Natural[0,WIDTH] Natural[0,HEIGHT], Boolean)
;; interp.
;;        - x is the x posistion of the coin
;;        - y is the y position of the coin
;;        - available? is a boolean, true if the coin should render on the screen, else do not render

(define C1 (make-coin (/ WIDTH 2) (/ HEIGHT 2) true))   ; coin render in the middle of the screen
(define C2 (make-coin (/ WIDTH 2) (/ HEIGHT 2) false))  ; coin does not render

(define (fn-for-coin c)
  (... (coin-x c)              ; Natural[0,WIDTH]
       (coin-y c)              ; Natural[0,HEIGHT]
       (coin-available? c)))   ; Boolean

;; Template Rules Used:
;; - compound data: composed of 3 parts

(define-struct game (p c1 c2 c3 c4 win?))

;; game is (make-game player coin, coin, coin, coin)
;; interp.
;; - p is the player
;; - c1 is the first coin
;; - c2 is the second coin
;; - c3 is the third coin
;; - c4 is the fourth coin

(define G1 (make-game (make-player (/ WIDTH 2) (/ HEIGHT 2) 0)
                      (make-coin C1_X_POS C1_Y_POS true)
                      (make-coin C2_X_POS C2_Y_POS true)
                      (make-coin C3_X_POS C3_Y_POS true)
                      (make-coin C4_X_POS C4_Y_POS true)
                      false))

(define G2 (make-game (make-player (/ WIDTH 2) (/ HEIGHT 2) 1)
                      (make-coin C1_X_POS C1_Y_POS false)
                      (make-coin C2_X_POS C2_Y_POS true)
                      (make-coin C3_X_POS C3_Y_POS true)
                      (make-coin C4_X_POS C4_Y_POS true)
                      false))
(define G3 (make-game (make-player (/ WIDTH 2) (/ HEIGHT 2) 2)
                      (make-coin C1_X_POS C1_Y_POS false)
                      (make-coin C2_X_POS C2_Y_POS true)
                      (make-coin C3_X_POS C3_Y_POS false)
                      (make-coin C4_X_POS C4_Y_POS true)
                      false))
(define G4 (make-game (make-player (/ WIDTH 2) (/ HEIGHT 2) 3)
                      (make-coin C1_X_POS C1_Y_POS false)
                      (make-coin C2_X_POS C2_Y_POS false)
                      (make-coin C3_X_POS C3_Y_POS false)
                      (make-coin C4_X_POS C4_Y_POS true)
                      false))
(define G5 (make-game (make-player (/ WIDTH 2) (/ HEIGHT 2) 4)
                      (make-coin C1_X_POS C1_Y_POS false)
                      (make-coin C2_X_POS C2_Y_POS false)
                      (make-coin C3_X_POS C3_Y_POS false)
                      (make-coin C4_X_POS C4_Y_POS false)
                      true))
;; =================
;; Functions:

;; game -> game
;; start the world with ...

(define (main g)
  (big-bang g                     ; player
            (to-draw   render)    ; player -> Image
            (on-key    move)))    ;player KeyEvent -> player

;; game -> game
;; produce the next ...
;; !!!
(define (move g ke)
  (cond [(key=? "up" ke ) (make-game (move-vertically (game-p g) -5)
                                           (game-c1 g)
                                           (game-c2 g)
                                           (game-c3 g)
                                           (game-c4 g)
                                           (game-win? g))]
        [(key=? "down" ke ) (make-game (move-vertically (game-p g) 5)
                                           (game-c1 g)
                                           (game-c2 g)
                                           (game-c3 g)
                                           (game-c4 g)
                                           (game-win? g))]
        [(key=? "right" ke ) (make-game (move-horizontal (game-p g) 5)
                                           (game-c1 g)
                                           (game-c2 g)
                                           (game-c3 g)
                                           (game-c4 g)
                                           (game-win? g))]
        [(key=? "left" ke ) (make-game (move-horizontal (game-p g) -5)
                                           (game-c1 g)
                                           (game-c2 g)
                                           (game-c3 g)
                                           (game-c4 g)
                                           (game-win? g))]
        [else g]))
;; player -> player
;; moves player horizonatally (chages x value)
(define (move-horizontal p dx)
      (make-player (modulo (+ (player-x p) dx) WIDTH) (player-y p) (player-score p)))

;; player -> player
;; moves player vertically(changes y value)
(define (move-vertically p dy)
      (make-player (player-x p) (modulo (+ (player-y p) dy) HEIGHT) (player-score p)))

;; Game -> Image
;; render player and coins 
;; !!!
(define (render g)
  (place-image PLAYER_ICON (player-x (game-p g)) (player-y (game-p g))
               (place-image COIN_IMG (coin-x (game-c1 g)) (coin-y (game-c1 g))
                            (place-image COIN_IMG (coin-x (game-c2 g)) (coin-y (game-c2 g))
                                         (place-image COIN_IMG (coin-x (game-c3 g)) (coin-y (game-c3 g))
                                                      (place-image COIN_IMG (coin-x (game-c4 g)) (coin-y (game-c4 g))
                                                                   MTS))))))