(import 
  (ironscheme)
  (ironscheme clr)
  (ironscheme clr reflection)
  (ironscheme threading))

(clr-reference bwapi-clr)
(clr-using BWAPI)


(trace-define-syntax bw-import
  (lambda (x)
    (define (symbol-append s1 . args)
      (string->symbol 
        (apply string-append 
                (symbol->string s1)
                (map symbol->string args))))
    (syntax-case x ()
      [(_ type) 
        (let* ((tn (string->symbol 
                     (string-append 
                       "BWAPI." 
                       (symbol->string (syntax->datum #'type)))))
               (t  (get-clr-type tn))
               (m  (type-members t '(public instance static declaredonly))))
          (define (member-name->symbol m)
            (string->symbol (member-name m)))
          (define (member-name->syntax m)
            (datum->syntax #'type (member-name->symbol m)))
          (define (member-name->syntaxname m)
            (datum->syntax 
              #'type 
              (symbol-append 
                (syntax->datum #'type) ':: (member-name->symbol m))))
          (with-syntax ((tn       (datum->syntax #'type tn))
                        ((sn ...) (map member-name->syntaxname m)) 
                        ((mn ...) (map member-name->syntax m)))
            #'(begin
                (define-syntax sn
                  (syntax-rules ()
                    [(_ args (... ...)) (clr-call tn mn args (... ...))])) 
                ... )))])))


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
    broodwar))
  
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

; Game

(define (can-build-here? unit pos type)
  (clr-call Game canBuildHere (broodwar) unit pos type))  
  
; Player

(define (get-units)
  (->list (clr-call Player getUnits (self))))
  
(define (supply-total)
  (clr-call Player supplyTotal (self)))  

(define (supply-used)
  (clr-call Player supplyUsed (self)))  

; Minerals 
  
(define (get-minerals)
  (->list (clr-call Game getMinerals (broodwar))))
  
(define (find-closest-mineral unit)
  (fold-left 
    (lambda (closest m)
       (if (or (null? closest) 
               (< (get-distance unit m) (get-distance unit closest)))
           m
           closest))
     '() ; null seed
     (get-minerals)))

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
 
(define (idle? unit)
  (clr-call Unit isIdle unit))  
  
(define (build-supply-depot unit)
  (printf "sd needed\n"))
  
  
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
    [(MatchFrame)     (match-frame e)]
    [(MatchStart)     (match-start e)]
    [(MatchEnd)       (trace-event e)]
    [(MenuFrame)      #f]
    [(NukeDetect)     (trace-event e)]
    [(PlayerLeft)     (trace-event e)]
    [(SaveGame)       (trace-event e)]
    [(SendText)       (trace-event e)]
    [(ReceiveText)    (trace-event e)]
    [(UnitCreate)     (trace-event e)]
    [(UnitDestroy)    (trace-event e)]
    [(UnitDiscover)   (trace-event e)]
    [(UnitEvade)      (trace-event e)]
    [(UnitHide)       (trace-event e)]
    [(UnitShow)       (trace-event e)]
    [(Morph)          (trace-event e)]
    [(UnitRenegade)   (trace-event e)]
    [else #f]))
    
; Event impl

(define (send-idle-workers-to-work units)
  (define workers (filter worker? units))
  (define idle-workers (filter idle? workers))
  (define minerals (get-minerals))
  
  (for-each 
    (lambda (i)
      (let ((closest-mineral (find-closest-mineral i)))
         (unless (null? closest-mineral)
           (right-click i closest-mineral))))
    idle-workers)
  
  ; shitty logic
  (if (fx<? (supply-total) (fx+ (supply-used) 3))
    (build-supply-depot 
      (car (if (null? idle-workers) workers idle-workers)))))
    
(define (send-idle-depots-to-work units)
  (define resource-depots (filter idle? (filter resource-depot? units)))

  (for-each
    (lambda (i)
      (if (eq? (get-race i) (race-zerg))
          (for-each 
            (lambda (l)
              (morph l (unittypes-zerg-drone)))
            (get-larva i))
          (train i (get-worker (get-self-race)))))
    resource-depots))      

(define (match-start e)
  (define units (get-units))
  (send-idle-workers-to-work units)
  (send-idle-depots-to-work units))
    
(define (match-frame e)
  (define units (get-units))
  (send-idle-workers-to-work units)
  (send-idle-depots-to-work units))    


(init)
(connect)
(wait-for-match)

(send "Hello world!")

(enable-flag 'user-input)
;(enable-flag 'complete-map-info)

(let game-loop ()
  (update)
  (game-loop))

