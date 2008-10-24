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

  (define (time-of-day date)
    (clr-prop-get datetime timeofday date))

  (define (day-of-year date)
    (clr-prop-get datetime dayofyear date))

  (define (day-of-week date)
    (clr-prop-get datetime dayofweek date))
  
  (define (year date)
    (clr-prop-get datetime year date))

  (define (month date)
    (clr-prop-get datetime month date))

  (define (day date)
    (clr-prop-get datetime day date))

  (define (hour date)
    (clr-prop-get datetime hour date))

  (define (minute date)
    (clr-prop-get datetime minute date))

  (define (second date)
    (clr-prop-get datetime second date))

  (define (millisecond date)
    (clr-prop-get datetime millisecond date))

  (define (ticks date/timespan)
    (cond
      ((datetime? date/timespan)    (clr-prop-get datetime ticks date/timespan))
      ((timespan? date/timespan)    (clr-prop-get timespan ticks date/timespan))
      (else
        (assertion-violation 'ticks "not a datetime or timespan" date/timespan))))
    
  (define (days timespan)
    (clr-prop-get timespan days timespan))

  (define (hours timespan)
    (clr-prop-get timespan hours timespan))

  (define (minutes timespan)
    (clr-prop-get timespan minutes timespan))

  (define (seconds timespan)
    (clr-prop-get timespan seconds timespan))

  (define (milliseconds timespan)
    (clr-prop-get timespan milliseconds timespan))

  (define (total-days timespan)
    (clr-prop-get timespan totaldays timespan))

  (define (total-hours timespan)
    (clr-prop-get timespan totalhours timespan))

  (define (total-minutes timespan)
    (clr-prop-get timespan totalminutes timespan))

  (define (total-seconds timespan)
    (clr-prop-get timespan totalseconds timespan))

  (define (total-milliseconds timespan)
    (clr-prop-get timespan totalmilliseconds timespan))

)