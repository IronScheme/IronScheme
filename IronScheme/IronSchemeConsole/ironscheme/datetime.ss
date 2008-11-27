(library (ironscheme datetime)
  (export
    now
    today
    time-of-day
    datetime?
    timespan?
    year
    month
    day
    hour
    minute
    second
    millisecond
    days
    hours
    minutes
    seconds
    milliseconds
    total-days
    total-hours
    total-minutes
    total-seconds
    total-milliseconds
    ticks
    day-of-year
    day-of-week
    make-datetime
    make-timespan
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using system)

  (define (datetime? obj)
    (clr-is datetime obj))   
    
  (define (timespan? obj)
    (clr-is timespan obj))     
    
  (define make-datetime
    (case-lambda
      [(ticks)                                  (clr-new datetime ticks)]
      [(year month day)                         (clr-new datetime year month day)]
      [(year month day hour minute second)      (clr-new datetime year month day hour minute second)]    
      [(year month day hour minute second ms)   (clr-new datetime year month day hour minute second (clr-cast int32 ms))]))    
      
  (define make-timespan
    (case-lambda
      [(ticks)                                  (clr-new timespan ticks)]
      [(hours minutes seconds)                  (clr-new timespan hours minutes seconds)]
      [(days hours minutes seconds)             (clr-new timespan days hours minutes seconds)]    
      [(days hours minutes seconds ms)          (clr-new timespan days hours minutes seconds ms)]))    
      
  
  (define (now)
    (clr-static-prop-get datetime now))   

  (define (today)
    (clr-static-prop-get datetime today))   

  (define/contract (time-of-day dt:datetime)
    (clr-prop-get datetime timeofday dt))

  (define/contract (day-of-year dt:datetime)
    (clr-prop-get datetime dayofyear dt))

  (define/contract (day-of-week dt:datetime)
    (clr-prop-get datetime dayofweek dt))
  
  (define/contract (year dt:datetime)
    (clr-prop-get datetime year dt))

  (define/contract (month dt:datetime)
    (clr-prop-get datetime month dt))

  (define/contract (day dt:datetime)
    (clr-prop-get datetime day dt))

  (define/contract (hour dt:datetime)
    (clr-prop-get datetime hour dt))

  (define/contract (minute dt:datetime)
    (clr-prop-get datetime minute dt))

  (define/contract (second dt:datetime)
    (clr-prop-get datetime second dt))

  (define/contract (millisecond dt:datetime)
    (clr-prop-get datetime millisecond dt))

  (define (ticks date/timespan)
    (cond
      ((datetime? date/timespan)    (clr-prop-get datetime ticks date/timespan))
      ((timespan? date/timespan)    (clr-prop-get timespan ticks date/timespan))
      (else
        (assertion-violation 'ticks "not a datetime or timespan" date/timespan))))
    
  (define/contract (days ts:timespan)
    (clr-prop-get timespan days ts))

  (define/contract (hours ts:timespan)
    (clr-prop-get timespan hours ts))

  (define/contract (minutes ts:timespan)
    (clr-prop-get timespan minutes ts))

  (define/contract (seconds ts:timespan)
    (clr-prop-get timespan seconds ts))

  (define/contract (milliseconds ts:timespan)
    (clr-prop-get timespan milliseconds ts))

  (define/contract (total-days ts:timespan)
    (clr-prop-get timespan totaldays ts))

  (define/contract (total-hours ts:timespan)
    (clr-prop-get timespan totalhours ts))

  (define/contract (total-minutes ts:timespan)
    (clr-prop-get timespan totalminutes ts))

  (define/contract (total-seconds ts:timespan)
    (clr-prop-get timespan totalseconds ts))

  (define/contract (total-milliseconds ts:timespan)
    (clr-prop-get timespan totalmilliseconds ts))

)