;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define BLANK (rectangle WIDTH HEIGHT 0 "white"))
(define LAST-PICTURE (overlay (text "GAME OVER" 40 "BLACK") BLANK))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-TURNR (- WIDTH (/ (image-width INVADER) 2)))
(define INVADER-TURNL (/ (image-width INVADER) 2))

(define TANK-TURNR (- WIDTH (/ (image-width TANK) 2)))
(define TANK-TURNL (/ (image-width TANK) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of :
;; -empty
;; (cons Invader ListOfInvader)
;; interp. List of invaders

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I2 LOI1 ))
#;
(define (fn-for-loi loi)
  (cond [empty? (...)]
        [else
         (... (fn-for-invaders (first loi))
              (fn-for-loi (rest loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of :
;; -empty
;; (cons Missile ListOfMissile)
;; interp. List of Missiles

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M2 LOM1 ))
#;
(define (fn-for-lom lom)
  (cond [(empty?) (...)]
        [else
         (... (fn-for-missile (first lom)
                              (fn-for-lom (rest  lom))))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =========================================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                ; Game
    (on-tick   advance-game) ; Game -> Game
    (to-draw   render-game)  ; Game -> Image
    (on-key    handle-key)   ; KeyEvent -> Game
    (stop-when last-world? last-picture))) ; Game -> Boolean



;; ===========================================
;; on-tick functions:

;; Game -> Game
;; produce the next game state

;(define (advance-game s) s)


(define (advance-game s)
  (make-game (add-invader (advance-invaders(remove-invaders (game-invaders s) (game-missiles s))))
             (advance-missiles (remove-missiles (game-missiles s)(game-invaders s)))
             (advance-tank     (game-tank s))))


;; ListOfInvaders -> ListOfInvaders
;; move the invaders per clock tick
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders LOI1)
              (cons (make-invader (+ 150 (* INVADER-X-SPEED 12))
                                  (+ 100 (* INVADER-Y-SPEED (abs 12))) 12) empty))

;(define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; move Invader to new postion; bounce if invader hits the side wall 
(check-expect (advance-invader (make-invader 10 10 10)) (make-invader (+ 10 (* INVADER-X-SPEED 10))
                                                                      (+ 10 (* INVADER-Y-SPEED (abs 10)))
                                                                      10))

(check-expect (advance-invader (make-invader 5  50 -7)) (make-invader INVADER-TURNL (+ 50 (* INVADER-Y-SPEED (abs -7))) 7))
(check-expect (advance-invader (make-invader INVADER-TURNL  50 -7)) (make-invader INVADER-TURNL 60.5 7))

(check-expect (advance-invader (make-invader (- INVADER-TURNR 5)  50 7)) (make-invader INVADER-TURNR (+ 50 (* INVADER-Y-SPEED 7)) -7))
(check-expect (advance-invader (make-invader INVADER-TURNR        50 7)) (make-invader INVADER-TURNR 60.5 -7))

;(define (advance-invader i) i) ;stub

(define (advance-invader i)
  (cond [(> (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) INVADER-TURNR)
         (make-invader INVADER-TURNR
                       (+ (* INVADER-Y-SPEED (abs (invader-dx i)))
                          (invader-y i))
                       (- (invader-dx i)))]
        [(< (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) INVADER-TURNL)
         (make-invader INVADER-TURNL
                       (+ (* INVADER-Y-SPEED (abs (invader-dx i)))
                          (invader-y i))
                       (- (invader-dx i)))]
        [else
         (make-invader (+ (* INVADER-X-SPEED (invader-dx i))
                          (invader-x i))
                       (+ (* INVADER-Y-SPEED (abs (invader-dx i)))
                          (invader-y i))
                       (invader-dx i))]))
        

;; ListOfInvader -> ListOfInvader
;; Randomly add 1 invader at random postion
(check-random (add-invader empty) (if (> INVADE-RATE (random 5000))
                                      (cons (make-invader (random WIDTH) 10 1) empty)
                                      empty))
(check-random (add-invader LOI2) (if (> INVADE-RATE (random 5000))
                                     (cons (make-invader (random WIDTH) 10 1) LOI2)
                                     LOI2))
;(define (add-invader loi) loi)

(define (add-invader loi)
  (if (> INVADE-RATE (random 5000))
      (cons (make-invader (random WIDTH) 10 1) loi)
      loi))


;; ListofInvaders ListofMissiles-> ListofInvaders
;; produce a list invaders not hit by missiles

(check-expect (remove-invaders empty empty) empty)
(check-expect (remove-invaders (list (make-invader 50 50 1))
                               (list (make-missile 50 50)))
              empty)
(check-expect (remove-invaders (list (make-invader 40 50 1))
                               (list (make-missile 60 50)))
              (list (make-invader 40 50 1)))

(check-expect (remove-invaders (list (make-invader 50 50 1))
                               (list (make-missile 50 60)))
              empty)

;(define (remove-invaders loi lom) loi)

(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-hit? (first loi) lom)
             (remove-invaders (rest loi) lom)
             (cons (first loi) (remove-invaders (rest loi) lom)))]))
              
;; Invader ListofMissile -> Boolean
;; true if any missile hit the invader
(check-expect (invader-hit? (make-invader 50 50 1) empty) false)
(check-expect (invader-hit? (make-invader 50 50 1) (list (make-missile 20 20) (make-missile 30 50))) false)
(check-expect (invader-hit? (make-invader 50 35 1) (list (make-missile 20 20) (make-missile 30 30))) false)
(check-expect (invader-hit? (make-invader 10 10 1) (list (make-missile 10 15) (make-missile 50 50))) true)

;(define (invader-hit? i lom) false)

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (or (hit? i (first lom))
             (invader-hit? i (rest  lom)))]))

;; Invader Missile -> Boolean
;; true if the Missile hit the Invader
(check-expect (hit? (make-invader 40 50 1) (make-missile 45 45)) true )
(check-expect (hit? (make-invader 40 50 1) (make-missile 10 45)) false)
(check-expect (hit? (make-invader 40 50 1) (make-missile 45 10)) false)
(check-expect (hit? (make-invader 40 50 1) (make-missile 35 40)) true )

;(define (hit? i m) false)

(define (hit? i m)
  (and
   (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE) 
   (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; ListOfMissiles -> ListOfMissiles
;; move the Missiles per clock tick
(check-expect (advance-missiles empty) empty)

(check-expect (advance-missiles (cons (make-missile 50 150) empty)) (cons (make-missile 50 (- 150 MISSILE-SPEED)) empty))
              
(check-expect (advance-missiles (cons (make-missile 20 60) (cons (make-missile 50 150) empty)))
              (cons (make-missile 20 (- 60 MISSILE-SPEED))
                    (cons (make-missile 50 (- 150 MISSILE-SPEED)) empty)))

;(define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest  lom)))]))


;; Missile -> Missile
;; move the missle MISSILE-SPEED in y dir
(check-expect (advance-missile (make-missile 50 150)) (make-missile 50 (- 150 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 50 60 )) (make-missile 50 (- 60  MISSILE-SPEED)))

;(define (advance-missile m) m)
(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; ListofMissiles ListofInvaders -> ListofMissiles
;; produce a list invaders not hit by missiles

(check-expect (remove-missiles empty empty) empty)
(check-expect (remove-missiles (list (make-missile 50 50))
                               (list (make-invader 50 50 1)))
              empty)

(check-expect (remove-missiles (list (make-missile 60 50))
                               (list (make-invader 40 50 1)))
              (list (make-missile 60 50)))

(check-expect (remove-missiles (list (make-missile 50 60))
                               (list (make-invader 50 50 1)))
              empty)

;(define (remove-missiles lom loi) lom) ;stub

(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (missile-hit? (first lom) loi)
             (remove-missiles (rest lom) loi)
             (cons (first lom) (remove-missiles (rest lom) loi)))]))


;; Missile ListofInvader -> Boolean
;; true if any missile hit the invader
(check-expect (missile-hit? (make-missile 50 50) empty) false)
(check-expect (missile-hit? (make-missile 50 50) (list (make-invader 20 20 1) (make-invader 30 50 1))) false)
(check-expect (missile-hit? (make-missile 50 35) (list (make-invader 20 20 1) (make-invader 30 30 -1))) false)
(check-expect (missile-hit? (make-missile 10 10) (list (make-invader 10 15 -1) (make-invader 50 50 1))) true)

;(define (missile-hit? m loi) false)

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (or (hit? (first loi) m)
             (missile-hit? m (rest  loi)))]))


;; Tank -> Tank
;; move the Tank per clock tick
(check-expect (advance-tank (make-tank 50  1))
              (make-tank 52 1))

(check-expect (advance-tank (make-tank 0 -1))
              (make-tank TANK-TURNL -1))

(check-expect (advance-tank (make-tank TANK-TURNR 1))
              (make-tank TANK-TURNR  1))
;(define (advance-tank t) t) ;stub

(define (advance-tank t)
  (cond [(> (+ (* (tank-dir t) TANK-SPEED) (tank-x t)) TANK-TURNR)
         (make-tank TANK-TURNR (tank-dir t))]
        [(< (+ (* (tank-dir t) TANK-SPEED) (tank-x t)) TANK-TURNL)
         (make-tank TANK-TURNL (tank-dir t))]
        [else
         (make-tank (+ (* (tank-dir t) TANK-SPEED) (tank-x t))
                    (tank-dir t))]))
; ======================================================
; Rendering Functions:

;; Game -> Image
;; render current game state

;(define (render-game s) empty-image)

(define (render-game s)
  (overlay (render-invaders (game-invaders s))
           (render-missiles (game-missiles s))
           (render-tank     (game-tank s))
           BACKGROUND))

;; ListOfInvaders -> Image
;; render images for all invaders
(check-expect (render-invaders empty) BLANK)
(check-expect (render-invaders  LOI1)
              (place-image INVADER 150 100 BLANK))
(check-expect (render-invaders LOI2)
              (place-image INVADER 150 HEIGHT (render-invaders  LOI1)))

;(define (render-invaders loi) empty-image)

(define (render-invaders loi)
  (cond [(empty? loi) BLANK]
        [else
         (overlay (render-invader (first loi))
                  (render-invaders (rest loi)))])) 


;; Invader -> Image
;; place Invader on a blank iage
(check-expect (render-invader(make-invader 150 250 10)) (place-image INVADER 150 250 BLANK))
                      
;(define (render-invader i) empty-image)
(define (render-invader i)
  (if (not (invader-out? i))
      (place-image INVADER (invader-x i) (invader-y i) BLANK)
      empty-image))


;; Invader -> Boolean
;; produce true if the Invader is out of screen (y coord > height)
(check-expect (invader-out? (make-invader 10 10     10)) false)
(check-expect (invader-out? (make-invader 10 HEIGHT 10)) false)
(check-expect (invader-out? (make-invader 10 501    10)) true)

;(define (invader-out? i) false)

(define (invader-out? i)
  (> (invader-y i) HEIGHT))


;; ListOfMissiles -> Image
;; render images for all missiles
(check-expect (render-missiles empty) BLANK)
(check-expect (render-missiles LOM1)
              (place-image MISSILE 150 300 BLANK))

;(define (render-missiles lom) empty-image)
(define (render-missiles lom)
  (cond [(empty? lom) BLANK]
        [else
         (overlay (render-missile (first lom))
                  (render-missiles (rest  lom)))]))


;; Missile -> image
;; render a Missile in appropriate place
(check-expect (render-missile (make-missile 200 100))
              (place-image MISSILE 200 100 BLANK))
;(define (render-missile m) empty-image)
(define (render-missile m)
  (cond [(not (missile-out? m))
         (place-image MISSILE (missile-x m) (missile-y m) BLANK)]
        [else
         empty-image]))

;; Missile -> Boolean
;; produce true if the missle is out of screen (missile y coord. < 0)
(check-expect (missile-out? (make-missile 50  50)) false)
(check-expect (missile-out? (make-missile 50   0)) false)
(check-expect (missile-out? (make-missile 50 -10)) true )

;(define (out? m) false)

(define (missile-out? m)
  (< (missile-y m) 0))



;; Tank -> Image
;; render image of the tank
(check-expect (render-tank (make-tank 150 1))
              (place-image TANK 150 (- HEIGHT TANK-HEIGHT/2) BLANK))
 
;(define (render-tank t) empty-image)

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BLANK))

; ===============================================
;; on-key functions;

;; KeyEvent -> Game

(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (image-height TANK)))                                                       (game-missiles g))
                    (game-tank g))]
        [(key=? ke "left")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x(game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x(game-tank g)) 1))]
        [else g]))


; ================================================
;; Stop when Functions :

;; Game -> Boolean
;; True if one of the invaders reaches the bottom

(check-expect (last-world? G3) false)
(check-expect (last-world? (make-game (list (make-invader 150 (+ 1 HEIGHT) 1)) empty T1)) true)
               
;(define (last-world? g) false)

(define (last-world? s)
  (cond [(empty? (game-invaders s)) false]
        [else
         (or (invader-out? (first (game-invaders s)))
             (last-world? (make-game (rest (game-invaders s)) (game-missiles s) (game-tank s))))]))

(define (last-picture g)
  (overlay LAST-PICTURE (render-game g)))
  