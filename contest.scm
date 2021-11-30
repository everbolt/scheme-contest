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

(define (dot x y theta color)
  (pixel (* x (sin (* theta 3.141592653589793238462643383279502884))) (+ y (/ (* x (cos (* theta 3.141592653589793238462643383279502884))) 5)) color) ;; convert theta to rad
)

(define (bounce x y yv c t)
  (cond
    ((< yv 0)
      (cond
        ((and (<= x 100) (< y 300)) ;First circle is 100 radius, y 300
          (* yv -0.5))
        ((and (and (< (abs x) 200) (> (abs x) 100)) (< y 100)) ;Second circle is 200 radius, y 100
          (* yv -0.5))
        (else ;No bounce, yv stays the same
          (dot x y t c)
          (+ yv -0.981)
        ) 
      )
    )
    (else
      (dot x y t c)
      (+ yv -0.981)
    )
  )
)

; cf => current frame
; x => x coordinate
; y => y coordinate
; yv => y velocity
; xs => x starting coordinate
; ys => y starting coordinate
; c => current color
; seed => current seed for rng
; t => theta of current ball
(define (frame cf x y yv xs ys seed t)
  (define x (+ x 1)) ;Updating x
  (define y (+ y (/ yv 10))) ;Updating y
  (cond
    ((and (>= y -500) (<= x 500))
      (define c
        (rgb 
          (- 1 (/ x 350))
          (min 1 (- 1 (/ (abs yv) 200)))
          (- 1 (/ (+ y 500) 1000))
        )
      )
      (frame (+ cf 1) x y (bounce x y yv c t) xs ys seed t))
    ((<= (abs t) 1) ;New ball
      (cond ;DETERMINE IF FINISHED THIS COLUMN
        ((<= ys 400) ;Finished column
          (cond
            ((<= t 0)
              (define t (+ (* t -1) 0.002)))
            (else
              (define t (* t -1)))
          )
          (frame 0 0 450 0 0 450 seed t)
        )
        (else
          (define start_y (- ys 10))
          (frame 0 0 start_y 0 0 start_y seed t)
        )
      )
    )
  )
)

(define (platforms t y radius)
  (goto (* radius (sin (* t 3.141592653589793238462643383279502884))) (+ y (/ (* radius (cos (* t 3.141592653589793238462643383279502884))) 5)))
  (cond
    ((>= t 2)
      (end_fill)
    )
    (else
      (platforms (+ t 0.01) y radius)
    )
  )
)

(define (draw)
  (hideturtle)
  (pu)
  (color "#ff9999")
  (bgcolor "#191919")

  (begin_fill)
  (platforms 0 100 200)
  (begin_fill)
  (platforms 0 300 100)

  (frame 0 0 450 0 0 450 1827394 0.002)
)

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)