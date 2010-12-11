(import 
  (ironscheme)
  (ironscheme clr)
  (ironscheme threading))

(clr-reference bwapi-clr)
(clr-using BWAPI)

(define (init) 
  (clr-static-call bwapi BWAPI_init))

(define (client) 
  (clr-static-prop-get bwapi BWAPIClient))
  
(define (->list ienum)
  (clr-static-call IronScheme.Runtime.Cons FromList ienum))
  
; this behaves like C#/F# now	       
(define (broodwar)
  (let ((broodwar (clr-static-prop-get bwapi Broodwar)))
    (assert (not (null? broodwar)))
    broodwar)  )
  
(define (self)
  (let ((self (clr-call Game self (broodwar))))
    (assert (not (null? self)))
    self))
  
(define (update)
  (clr-call Client update (client))
  (for-each handle-event (get-events)))

(define (send args)
  (clr-call Game sendText (broodwar) args))
 
(define (enable-flag flag)
  (clr-call Game enableFlag (broodwar)
    (case flag
      [(complete-map-info) 0]
      [(user-input) 1]
      [(max) 2])))

(define (connect)
  (define (connect)
    (clr-call Client connect (client)))
  (display "\nTrying to connect ..")
  (let loop () 
    (unless (connect)
      (display ".")
      (thread-sleep 1000)
      (loop)))
	(display "\n"))
	
(define (wait-for-match)
  (display "Waiting for a match ...")
  (let wait () 
    (unless (clr-call Game isInGame (broodwar))
      (update)
      (wait)))
  (display "\nMatch Started !\n"))
  
  
; Player

(define (get-units)
  (->list (clr-call Player getUnits (self))))

; Minerals 
  
(define (get-minerals)
  (->list (clr-call Game getMinerals (broodwar))))

; Race  

(define (get-worker race)
  (clr-call Race getWorker race))
  
(define (get-self-race)
  (clr-call Player getRace (self)))
  
(define (race-zerg)
  (clr-static-prop-get bwapi Races_Zerg))  

; Unit type

(define (unittype-race ut)
  (clr-call UnitType getRace ut))
  
(define (unittype-worker? ut)
  (clr-call UnitType isWorker ut))

(define (unittype-resource-depot? ut)
  (clr-call UnitType isResourceDepot ut))
  
(define (unittypes-zerg-drone)  
  (clr-static-prop-get bwapi UnitTypes_Zerg_Drone))    

; Unit    
  
(define (unit-gettype unit)
  (clr-call Unit getType unit))

(define (get-race unit)
  (unittype-race (unit-gettype unit)))
  
(define (worker? unit)
  (unittype-worker? (unit-gettype unit)))
  
(define (resource-depot? unit)
  (unittype-resource-depot? (unit-gettype unit)))
  
(define (get-distance from to)
  (clr-call Unit getDistance from  (clr-cast Unit to)))
  
(define (right-click unit target)  
  (clr-call Unit rightClick unit (clr-cast Unit target)))

(define (morph unit ut)
  (clr-call Unit morph unit ut))

(define (get-larva unit)
  (->list (clr-call Unit getLarva unit)))  
  
(define (train unit worker)
 (clr-call Unit train unit worker))   
  
; Events  

(define (get-events)
  (->list (clr-call Game getEvents (broodwar))))
  
(define (event-type e)
  (clr-prop-get Event type e))
  
(define (event-text e)
  (clr-prop-get Event text e))

(define (event-unit e)
  (clr-prop-get Event unit e))

(define (event-player e)
  (clr-prop-get Event player e)) 
  
(define (trace-event e)
  (printf "event: ~a\n"  (event-type e)))   
  
(define (handle-event e)
  (case (event-type e)
    [(matchframe)     #f]
    [(matchstart)     (trace-event e)(match-start e)]
    [(matchend)       (trace-event e)]
    [(menuframe)      #f]
    [(nukedetect)     (trace-event e)]
    [(playerleft)     (trace-event e)]
    [(savegame)       (trace-event e)]
    [(sendtext)       (trace-event e)]
    [(receivetext)    (trace-event e)]
    [(unitcreate)     (trace-event e)]
    [(unitdestroy)    (trace-event e)]
    [(unitdiscover)   (trace-event e)]
    [(unitevade)      (trace-event e)]
    [(unithide)       (trace-event e)]
    [(unitshow)       (trace-event e)]
    [(morph)          (trace-event e)]
    [(unitrenegade)   (trace-event e)]
    [else #f]))
    
; Event impl

(define (match-start e)
  (define units (get-units))
  (define workers (filter worker? units))
  (define minerals (get-minerals))
  (define resource-depots (filter resource-depot? units))
  
  (for-each 
    (lambda (i)
      (let ((closest-mineral (fold-left 
                               (lambda (closest m)
                                 (if (or (null? closest) 
                                         (< (get-distance i m) (get-distance i closest)))
                                     m
                                     closest))
                               '() ; null seed
                               minerals)))
         (unless (null? closest-mineral)
           (right-click i closest-mineral))))
    workers)

  (for-each
    (lambda (i)
      (if (eq? (get-race i) (race-zerg))
          (for-each 
            (lambda (l)
              (morph l (unittypes-zerg-drone)))
            (get-larva i))
          (train i (get-worker (get-self-race)))))
    resource-depots))      

(init)
(connect)
(wait-for-match)

(send "Hello world!")

(enable-flag 'user-input)
;(enable-flag 'complete-map-info)

(let game-loop ()
  (update)
  (game-loop))

