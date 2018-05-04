;; dizzy-invaders.rkt
;; Dizzy Invaders
;; Watch the Dizzian Spacecraft bounce off the walls
;;
(require 2htdp/image)
(require 2htdp/universe)

;; load a sprite set to be used
;; see the text in the graphics for acknowledgements
(define SPRITE-SET
  .
  )

;; constants

; from the water balloon project
(define WIDTH  600)
(define HEIGHT 300)
(define INVADER-SCALE 4)

(define CTR-Y (/ HEIGHT 2))

(define LINEAR-SPEED 2)

;(define-struct ALPHA-COLOR (r g b))
;
;(make-ALPHA-COLOR 0 8 255)

(define MTS (rectangle WIDTH HEIGHT "solid" "black"))

(define INVADER-IMAGE-1 (scale INVADER-SCALE (crop 20 0 14 12 SPRITE-SET)))
(define INVADER-IMAGE-2 (scale INVADER-SCALE (crop 35 0 14 12 SPRITE-SET)))

;; ============
;; Data Definitions

(define-struct invader-state (x y a d))
;; InvaderState is (make-invader-state Number Number)
;; interp. The state of an invader falling from the top of the screen
;; and bouncing off the left and right edges.
;; When the invader reaches the bottom of the screen, start again at the top.
;; Alternate between two images for a little bit of animation.
;;         x is the x-coordinate in pixels->Integer
;;         y is the y-coordinate in pixels->Integer
;;         a is the animation image -> Image
;;         d is the direction of travel -> Integer
(define IS1 (make-invader-state 10 0 INVADER-IMAGE-1 1))
(define IS2 (make-invader-state 30 15 INVADER-IMAGE-2 -1))

(define (fn-for-invader-state invader-state)
  (... (invader-state-x invader-state)
       (invader-state-y  invader-state)
       (invader-state-a invader-state)
       (invader-state-d invader-state)))

;; Template rules used:
;; - compound: 4 fields

;; ===========
;; Functions

;; InvaderState -> InvaderState
;; run the animation, starting with initial balloon state invader-state.
;; Start with (main (make-invader-state 0 0 INVADER-IMAGE-1 1))
;; <no tests for main functions>
(define (main invader-state)
  (big-bang invader-state
            (on-tick next-invader-state)
            (to-draw render-invader-state)
            ))

;; InvaderState -> InvaderState
;; advance invader-state by LINEAR-SPEED and change ANIMATION
(check-expect (next-invader-state (make-invader-state 1 12 INVADER-IMAGE-1 1))
              (make-invader-state (+ 1 LINEAR-SPEED) (+ 1 LINEAR-SPEED) INVADER-IMAGE-2 1))

(check-expect (next-invader-state (make-invader-state 1 12 INVADER-IMAGE-2 1))
              (make-invader-state (+ 1 LINEAR-SPEED) (+ 1 LINEAR-SPEED) INVADER-IMAGE-1 1))

(check-expect (next-invader-state (make-invader-state 0 HEIGHT INVADER-IMAGE-2 1))
              (make-invader-state (+ 0 LINEAR-SPEED) 0 INVADER-IMAGE-1 1))

;(define (next-invader-state invader-state)
;  (make-invader-state 3 3 INVADER-IMAGE-2 1)) ; stub
(define (next-invader-state invader-state)
  (make-invader-state (+ (* LINEAR-SPEED (change-marching-direction invader-state)) (invader-state-x invader-state))
                      (calculate-next-height invader-state)
                      (alternate-invader-image invader-state)
                      (change-marching-direction invader-state)))
                      
;; InvaderState->Image
;; alternate-invader-image
;; take one invader image and return the other(INVADER-IMAGE-1 <-> INVADER-IMAGE-2)
(check-expect (alternate-invader-image (make-invader-state 1 12 INVADER-IMAGE-1 1)) INVADER-IMAGE-2)
(check-expect (alternate-invader-image (make-invader-state 1 12 INVADER-IMAGE-2 1)) INVADER-IMAGE-1)

;(define (alternate-invader-image invader-state)
;  INVADER-IMAGE-1) ; stub

(define (alternate-invader-image invader-state)
  (cond [(equal? (invader-state-a invader-state) INVADER-IMAGE-1) INVADER-IMAGE-2]
        [else INVADER-IMAGE-1]))

;; InvaderState->Int
;; calculate-next-height
;; If the next position of the invader >= HEIGHT, set invader-state-y to 0 else (+ 1 LINEAR-SPEED)
(check-expect
 (calculate-next-height
  (make-invader-state 0 0 INVADER-IMAGE-1 1)) (+ 1 LINEAR-SPEED))

(check-expect
 (calculate-next-height
  (make-invader-state 0 HEIGHT INVADER-IMAGE-1 1)) 0)

;(define (calculate-next-height invader-state)
;  (cond [true 0]
;        [else 1])) ; stub

(define (calculate-next-height invader-state)
  (cond [(>= (invader-state-y invader-state) HEIGHT) 0]
        [else (+ (invader-state-y invader-state) (+ 1 LINEAR-SPEED))])) 

;; InvaderState->Int
;; change-marching-direction
;; if the sprite hits the right limit invader-state-x >= WIDTH, change the multiplier to -1
;; else if the sprite left limit invader-state-x <= 0, change the multiplier back to 1
;;
(check-expect (change-marching-direction (make-invader-state WIDTH 0 INVADER-IMAGE-1 1)) -1)
(check-expect (change-marching-direction (make-invader-state 0 0 INVADER-IMAGE-1 -1)) 1)
(check-expect (change-marching-direction (make-invader-state (/ WIDTH 2) 0 INVADER-IMAGE-1 1)) 1)

; stub
;(define (change-marching-direction invader-state)
;  1)

(define (change-marching-direction invader-state)
  (cond [(>= (invader-state-x invader-state) WIDTH) -1]
        [(<= (invader-state-x invader-state) 0) 1]
        [ else (invader-state-d invader-state)]))
  



(define (render-invader-state invader-state)
  (place-image
   (invader-state-a invader-state)
   (invader-state-x invader-state)
   (invader-state-y invader-state)
   MTS
   ))

(define (reset-invader-state invader-state)
  ...)


