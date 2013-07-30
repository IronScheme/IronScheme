#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme datetime)
  (export
    now
    utc-now
    datetime->utc
    datetime->local
    today
    difference
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
    date
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
    make-utc-datetime
    make-timespan
    
    stopwatch?
    make-stopwatch
    stopwatch-start
    stopwatch-stop
    stopwatch-elapsed
    
    measure)
  (import 
    (ironscheme)
    (ironscheme syntax shorthand)
    (ironscheme typed)
    (ironscheme unsafe)
    (ironscheme contracts)
    (ironscheme clr))
    
  (define (datetime? obj)
    (clr-is DateTime obj))   
    
  (define (timespan? obj)
    (clr-is TimeSpan obj))     
    
  (define/contract make-utc-datetime
    (case-lambda
      [(ticks)
        (clr-new DateTime ticks 'utc)]
      [(year:fixnum month:fixnum day:fixnum)
        (clr-new DateTime year month day 0 0 0 (clr-cast DateTimeKind 'utc))]
      [(year:fixnum month:fixnum day:fixnum hour:fixnum minute:fixnum second:fixnum)
        (clr-new DateTime year month day hour minute second (clr-cast DateTimeKind 'utc))]    
      [(year:fixnum month:fixnum day:fixnum hour:fixnum minute:fixnum second:fixnum ms:fixnum)
        (clr-new DateTime year month day hour minute second (clr-cast Int32 ms) (clr-cast DateTimeKind 'utc))]))    
    
  (define/contract make-datetime
    (case-lambda
      [(ticks)                                  
        (clr-new DateTime ticks)]
      [(year:fixnum month:fixnum day:fixnum)
        (clr-new DateTime year month day)]
      [(year:fixnum month:fixnum day:fixnum hour:fixnum minute:fixnum second:fixnum)
        (clr-new DateTime year month day hour minute second)]    
      [(year:fixnum month:fixnum day:fixnum hour:fixnum minute:fixnum second:fixnum ms:fixnum)   
        (clr-new DateTime year month day hour minute second (clr-cast Int32 ms))]))    
      
  (define/contract make-timespan
    (case-lambda
      [(ticks)                                  
        (clr-new TimeSpan ticks)]
      [(hours:fixnum minutes:fixnum seconds:fixnum)
        (clr-new TimeSpan hours minutes seconds)]
      [(days:fixnum hours:fixnum minutes:fixnum seconds:fixnum)
        (clr-new TimeSpan days hours minutes seconds)]    
      [(days:fixnum hours:fixnum minutes:fixnum seconds:fixnum ms:fixnum)
        (clr-new TimeSpan days hours minutes seconds ms)]))    
  
  (define (now)
    (clr-static-prop-get DateTime Now))   

  (define (utc-now)
    (clr-static-prop-get DateTime UtcNow))   

  (define (today)
    (clr-static-prop-get DateTime Today))
    
  (define/contract (datetime->utc dt:datetime)    
    (clr-call DateTime ToUniversalTime dt))
    
  (define/contract (datetime->local dt:datetime)    
    (clr-call DateTime ToLocalTime dt))    
    
  (define/contract (difference dt1:datetime dt2:datetime)
    (clr-static-call DateTime (op_Subtraction DateTime DateTime) dt1 dt2))

  (define/contract (time-of-day dt:datetime)
    (clr-prop-get DateTime TimeOfDay dt))

  (define/contract (day-of-year dt:datetime)
    (clr-prop-get DateTime DayOfYear dt))

  (define/contract (day-of-week dt:datetime)
    (clr-prop-get DateTime DayOfWeek dt))
  
  (define/contract (year dt:datetime)
    (clr-prop-get DateTime Year dt))

  (define/contract (month dt:datetime)
    (clr-prop-get DateTime Month dt))

  (define/contract (day dt:datetime)
    (clr-prop-get DateTime Day dt))

  (define/contract (hour dt:datetime)
    (clr-prop-get DateTime Hour dt))

  (define/contract (minute dt:datetime)
    (clr-prop-get DateTime Minute dt))

  (define/contract (second dt:datetime)
    (clr-prop-get DateTime Second dt))

  (define/contract (millisecond dt:datetime)
    (clr-prop-get DateTime Millisecond dt))
    
  (define/contract (date dt:datetime)
    (clr-prop-get DateTime Date dt))
    
  (define/contract (datetime-kind dt:datetime)
    (clr-prop-get DateTime Kind dt))

  (define (ticks date/timespan)
    (cond
      ((datetime? date/timespan)    (clr-prop-get DateTime Ticks date/timespan))
      ((timespan? date/timespan)    (clr-prop-get TimeSpan Ticks date/timespan))
      (else
        (assertion-violation 'ticks "not a datetime or timespan" date/timespan))))
    
  (define/contract (days ts:timespan)
    (clr-prop-get TimeSpan Days ts))

  (define/contract (hours ts:timespan)
    (clr-prop-get TimeSpan Hours ts))

  (define/contract (minutes ts:timespan)
    (clr-prop-get TimeSpan Minutes ts))

  (define/contract (seconds ts:timespan)
    (clr-prop-get TimeSpan Seconds ts))

  (define/contract (milliseconds ts:timespan)
    (clr-prop-get TimeSpan Milliseconds ts))

  (define/contract (total-days ts:timespan)
    (clr-prop-get TimeSpan TotalDays ts))

  (define/contract (total-hours ts:timespan)
    (clr-prop-get TimeSpan TotalHours ts))

  (define/contract (total-minutes ts:timespan)
    (clr-prop-get TimeSpan TotalMinutes ts))

  (define/contract (total-seconds ts:timespan)
    (clr-prop-get TimeSpan TotalSeconds ts))

  (define/contract (total-milliseconds ts:timespan)
    (clr-prop-get TimeSpan TotalMilliseconds ts))
    
  (clr-using System.Diagnostics) 
  
  (define (make-stopwatch)
    (clr-new Stopwatch))
    
  (define (stopwatch? o)
    (clr-is Stopwatch o))
    
  (define/contract (stopwatch-start sw:stopwatch)
    (clr-call Stopwatch Start sw))
    
  (define/contract (stopwatch-stop sw:stopwatch)
    (clr-call Stopwatch Stop sw))
    
  (define/contract (stopwatch-elapsed sw:stopwatch)
    (clr-prop-get Stopwatch Elapsed sw))
    
  (define/contract (stopwatch-elapsed-milliseconds sw:stopwatch)
    (clr-static-call Convert ToInt32 (clr-prop-get Stopwatch ElapsedMilliseconds sw)))

  (define-syntax-case (measure iters expr)
    (with-syntax ([str (format "~a" (syntax->datum #'expr))])
      #'(let ((sw (make-stopwatch)))
         (stopwatch-start sw)
         (let: f ((i : fixnum 0))
           (cond 
            [($fx=? i iters)
               (stopwatch-stop sw)
               (fprintf (current-error-port)
                        "~a: ~ams\n" 
                        str 
                        (round (/ (stopwatch-elapsed-milliseconds sw) 
                                  iters)))]
            [else
              expr
              (f ($fx+ i 1))]))))))