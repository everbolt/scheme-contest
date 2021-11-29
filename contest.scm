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
(define fps 100) ;Frames per second
(define gravity -9.81) ;y_acc due to gravity
(define elasticity -0.8)
(define colors (cons "black" (cons "red" (cons "blue" (cons "green" (cons "orange" (cons "yellow" (cons "purple" (cons "red" nil)))))))))
(define (frame cf x y x_vel y_vel x_acc y_acc bounce colors x_start y_start)
  (setposition x y)
  ;(pd)
  (pixel x y (car colors))
  ;(pu)
  (if (< (abs (modulo x 100)) 0.01) ;Displaying info
    (display x y cf (newline))
  )
  (define x (+ x (/ x_vel fps)))
  (define y (+ y (/ y_vel fps)))
  (cond ;Bounce
    ((and (and (< x 100) (<= y 0)) (< y_vel 0))
      (define y_vel (* y_vel bounce)))
    (else
      (define y_vel (+ y_vel (/ y_acc fps))))
  )
  
  (cond
    ((and (>= y -500) (<= x 500))
      (frame (+ cf 1) x y x_vel y_vel x_acc y_acc bounce colors x_start y_start))
    ((> y_start 300)
      (color (car (cdr colors)))
      (frame 0 -500 (- y_start 25) x_vel 0 0 gravity bounce (cdr colors) x_start (- y_start 25)))
  )
)

(define (draw)
  (hideturtle)
  (setposition -500 0)
  (rt 90)
  (fd 600)
  (pu)
  (frame 0 -500 500 10 0 0 gravity elasticity colors -500 500)
  (exitonclick)
)

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)