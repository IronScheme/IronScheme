#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using System)

  (define (datetime? obj)
    (clr-is DateTime obj))   
    
  (define (timespan? obj)
    (clr-is TimeSpan obj))     
    
  (define make-utc-datetime
    (case-lambda
      [(ticks)                                  (clr-new DateTime ticks 'utc)]
      [(year month day)                         (clr-new DateTime year month day 0 0 0 (clr-cast DateTimeKind 'utc))]
      [(year month day hour minute second)      (clr-new DateTime year month day hour minute second (clr-cast DateTimeKind 'utc))]    
      [(year month day hour minute second ms)   (clr-new DateTime year month day hour minute second (clr-cast int32 ms) (clr-cast DateTimeKind 'utc))]))    
    
  (define make-datetime
    (case-lambda
      [(ticks)                                  (clr-new datetime ticks)]
      [(year month day)                         (clr-new datetime year month day)]
      [(year month day hour minute second)      (clr-new datetime year month day hour minute second)]    
      [(year month day hour minute second ms)   (clr-new datetime year month day hour minute second (clr-cast int32 ms))]))    
      
  (define make-timespan
    (case-lambda
      [(ticks)                                  (clr-new TimeSpan ticks)]
      [(hours minutes seconds)                  (clr-new TimeSpan hours minutes seconds)]
      [(days hours minutes seconds)             (clr-new TimeSpan days hours minutes seconds)]    
      [(days hours minutes seconds ms)          (clr-new TimeSpan days hours minutes seconds ms)]))    
      
  
  (define (now)
    (clr-static-prop-get DateTime now))   

  (define (utc-now)
    (clr-static-prop-get DateTime UtcNow))   

  (define (today)
    (clr-static-prop-get DateTime today))
    
  (define/contract (datetime->utc dt:datetime)    
    (clr-call DateTime ToUniversalTime dt))
    
  (define/contract (datetime->local dt:datetime)    
    (clr-call DateTime ToLocalTime dt))    
    
  (define/contract (difference dt1:datetime dt2:datetime)
    (clr-static-call DateTime "op_Subtraction(DateTime,DateTime)" dt1 dt2))

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

)