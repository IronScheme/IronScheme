(import (rnrs))

(library (runge-kutta)
	(export 
		integrate-system
		head 
		tail)
	(import (rnrs base))
	
	(define integrate-system
		(lambda (system-derivative initial-state h)
			(let ((next (runge-kutta-4 system-derivative h)))
				(letrec ((states
					(cons initial-state
						(lambda ()
							(map-streams next states)))))
					states))))
					
	
	(define runge-kutta-4
		(lambda (f h)
			(let ((*h (scale-vector h))
						(*2 (scale-vector 2))
						(*1/2 (scale-vector (/ 1 2)))
						(*1/6 (scale-vector (/ 1 6))))
				(lambda (y)
				;; y is a system state
					(let* ((k0 (*h (f y)))
								 (k1 (*h (f (add-vectors y (*1/2 k0)))))
								 (k2 (*h (f (add-vectors y (*1/2 k1)))))
								 (k3 (*h (f (add-vectors y k2)))))
						(add-vectors y
							(*1/6 (add-vectors k0
							(*2 k1)
							(*2 k2)
							k3))))))))
							
							
	(define elementwise
		(lambda (f)
			(lambda vectors
				(generate-vector
					(vector-length (car vectors))
						(lambda (i)
							(apply f
								(map (lambda (v) (vector-ref v i))
											vectors)))))))
											
	(define generate-vector
		(lambda (size proc)
			(let ((ans (make-vector size)))
				(letrec ((loop
					(lambda (i)
						(cond ((= i size) ans)
						(else
							(vector-set! ans i (proc i))
						(loop (+ i 1)))))))
					(loop 0)))))
					
	(define add-vectors (elementwise +))
	
	(define scale-vector
		(lambda (s)
			(elementwise (lambda (x) (* x s)))))
	
	(define map-streams
		(lambda (f s)
			(cons (f (head s))
				(lambda () (map-streams f (tail s))))))
				
	(define head car)
	
	(define tail
		(lambda (stream) ((cdr stream))))
)

(import (runge-kutta))
	
(define damped-oscillator
	(lambda (R L C)
		(lambda (state)
			(let ((Vc (vector-ref state 0))
						(Il (vector-ref state 1)))
				(vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
					(/ Vc L))))))
					
(define the-states
	(integrate-system
		(damped-oscillator 10000 1000 .001)
			'#(1 0)
			.01))
			
(letrec ((loop (lambda (s)
					(newline)
					(write (head s))
					(loop (tail s)))))
	(loop the-states))
	