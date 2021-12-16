;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: computergobrrrr
;;;
;;; Description:
;;;   brrrrrrrrrr
;;;   Each pixel is 1 meter
;;;

(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))
(define fps 10)
(define xv 10)
(define elas -0.2)
(define depth 5)
(define pi 3.141592653589793238462643383279502884)

(define (dot x y theta color)
  (pixel (* x (sin (* theta pi))) (+ y (/ (* x (cos (* theta pi))) depth)) color) ;; convert theta to rad
)

(define (update_yv x y yv c theta)
  (cond
    ((< yv 0)
      (cond
        ((and (< y 300) (<= x (* 100 (cos (* (+ theta 0.2) pi)))))
          (* yv elas)
        )
        ((and (< y 100) (<= x (* 200 (cos (* (- theta 0.4) pi)))))
          (* yv elas)
        )
        ((and (< y -100) (<= x (* 300 (cos (* theta pi)))))
          (* yv elas)
        )
        ((and (< y -300) (<= x (* 400 (cos (* (+ theta 0.7) pi)))))
          (* yv elas)
        )
        (else ;No bounce, yv stays the same
          (dot x y theta c)
          (- yv (/ 9.81 fps))
        ) 
      )
    )
    (else
      (dot x y theta c)
      (- yv (/ 9.81 fps))
    )
  )
)

; x => x coordinate
; y => y coordinate
; yv => y velocity
; xs => x starting coordinate
; ys => y starting coordinate
; theta => theta of current particle
(define (simulate_frame x y yv xs ys theta)
  (define x (+ x (/ xv fps))) ;Updating x position
  (define y (+ y (/ yv fps))) ;Updating y position
  (cond
    ((and (>= y -600) (<= x 500)) ;checks if the particle is visible in image
      (define c
        (rgb ;Determine color based on x, y, and yv (creates gradient)
          (max (min 1 (- 1 (/ x 450))) 0)
          (max (min 1 (- 1 (/ (abs yv) 200))) 0)
          (max (min 1 (- 1 (/ (+ y 500) 1000))) 0)
        )
      )
      (simulate_frame x y (update_yv x y yv c theta) xs ys theta))
    ((<= (abs theta) 1) ;This stops recursing when finished rendering particles around cylinder
      (cond ;This will check how "simulate_frame" should recurse
        ((<= ys 400) ;Finished rendering all pixels for this theta, start with new theta
          ;This will "flip-flop" the particles we render from + to -
          ;This is because the function renders the particles from back to front
          ;As the particles in the front should appear on top
          ;Note that theta = 0 is pointing directly away from the viewer
          (cond
            ((<= theta 0)
              (define theta (+ (* theta -1) 0.002)))
            (else
              (define theta (* theta -1)))
          )
          (simulate_frame 0 450 0 0 450 theta)
        )
        (else ;Render new particle with same theta, but lower starting y value
          (define start_y (- ys 10))
          (simulate_frame 0 start_y 0 0 start_y theta)
        )
      )
    )
  )
)

(define (draw)
  (hideturtle)
  (bgcolor "#191919")
  (simulate_frame 0 450 0 0 450 0.002)
)

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)