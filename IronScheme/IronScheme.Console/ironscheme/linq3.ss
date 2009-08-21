#!r6rs
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

(library (ironscheme linq3)
  (export

    )

  (import 
    (rnrs))
    
  ;(define-record-type expression (fields type))
      
  
  (define-record-type provider (fields executer))
      
  (define-record-type queryable (fields provider))
  
  (define (executer src)
    (let ((disp (provider-executer (queryable-provider src))))
      (lambda (name . args)
        (apply disp name src args))))
     
  (define (list-select src selector)
    (with-syntax ((src src)(selector selector))
      #'(map selector src)))     
    
  (define (list-execute cmd src . args)
    (case cmd
      [(select)
        (apply list-select src args)]
      [else
        (assertion-violation 'list-execute "not done" cmd)]))  
        
  (define-syntax define-interface 
    (lambda (x)
      (syntax-case x ()
        [(_ name (arg ...) ...)
          #'(define-syntax name
              (lambda (x)
                (syntax-case x ()
                  [(_ src arg ...)
                    ((executor #'src) #'name #'arg ...)] ...)))])))

  (define-interface aggregate
    (func)
    (seed func)
    (seed func selector))
    
  (define-interface all
    (predicate))
    
  (define-interface any
    ()
    (selector))
    
  (define-interface average
    ()
    (selector))
    
  (define-interface concat
    (src2))
    
  (define-interface contains
    (item)
    (item comparer))
    
  (define-interface count
    ()
    (predicate))
    
    

  (define-interface select
    (selector))
  
  ;public static TSource Aggregate<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, TSource, TSource>> func);
  ;public static TAccumulate Aggregate<TSource, TAccumulate>(this IQueryable<TSource> source, TAccumulate seed, Expression<Func<TAccumulate, TSource, TAccumulate>> func);
  ;public static TResult Aggregate<TSource, TAccumulate, TResult>(this IQueryable<TSource> source, TAccumulate seed, Expression<Func<TAccumulate, TSource, TAccumulate>> func, Expression<Func<TAccumulate, TResult>> selector);
  ;public static bool All<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static bool Any<TSource>(this IQueryable<TSource> source);
  ;public static bool Any<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TElement> AsQueryable<TElement>(this IEnumerable<TElement> source);
  ;public static IQueryable AsQueryable(this IEnumerable source);
  ;public static decimal? Average(this IQueryable<decimal?> source);
  ;public static double? Average(this IQueryable<double?> source);
  ;public static double Average(this IQueryable<double> source);
  ;public static double? Average(this IQueryable<int?> source);
  ;public static decimal Average(this IQueryable<decimal> source);
  ;public static double? Average(this IQueryable<long?> source);
  ;public static double Average(this IQueryable<int> source);
  ;public static double Average(this IQueryable<long> source);
  ;public static float? Average(this IQueryable<float?> source);
  ;public static float Average(this IQueryable<float> source);
  ;public static double? Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int?>> selector);
  ;public static double? Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, long?>> selector);
  ;public static decimal Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, decimal>> selector);
  ;public static double? Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, double?>> selector);
  ;public static double Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, double>> selector);
  ;public static decimal? Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, decimal?>> selector);
  ;public static float? Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, float?>> selector);
  ;public static double Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int>> selector);
  ;public static double Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, long>> selector);
  ;public static float Average<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, float>> selector);
  ;public static IQueryable<TResult> Cast<TResult>(this IQueryable source);
  ;public static IQueryable<TSource> Concat<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2);
  ;public static bool Contains<TSource>(this IQueryable<TSource> source, TSource item);
  ;public static bool Contains<TSource>(this IQueryable<TSource> source, TSource item, IEqualityComparer<TSource> comparer);
  ;public static int Count<TSource>(this IQueryable<TSource> source);
  ;public static int Count<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TSource> DefaultIfEmpty<TSource>(this IQueryable<TSource> source);
  ;public static IQueryable<TSource> DefaultIfEmpty<TSource>(this IQueryable<TSource> source, TSource defaultValue);
  ;public static IQueryable<TSource> Distinct<TSource>(this IQueryable<TSource> source);
  ;public static IQueryable<TSource> Distinct<TSource>(this IQueryable<TSource> source, IEqualityComparer<TSource> comparer);
  ;public static TSource ElementAt<TSource>(this IQueryable<TSource> source, int index);
  ;public static TSource ElementAtOrDefault<TSource>(this IQueryable<TSource> source, int index);
  ;public static IQueryable<TSource> Except<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2);
  ;public static IQueryable<TSource> Except<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2, IEqualityComparer<TSource> comparer);
  ;public static TSource First<TSource>(this IQueryable<TSource> source);
  ;public static TSource First<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static TSource FirstOrDefault<TSource>(this IQueryable<TSource> source);
  ;public static TSource FirstOrDefault<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;private static Expression GetSourceExpression<TSource>(IEnumerable<TSource> source);
  ;public static IQueryable<IGrouping<TKey, TSource>> GroupBy<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector);
  ;public static IQueryable<IGrouping<TKey, TSource>> GroupBy<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, IEqualityComparer<TKey> comparer);
  ;public static IQueryable<IGrouping<TKey, TElement>> GroupBy<TSource, TKey, TElement>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TSource, TElement>> elementSelector);
  ;public static IQueryable<TResult> GroupBy<TSource, TKey, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TKey, IEnumerable<TSource>, TResult>> resultSelector);
  ;public static IQueryable<IGrouping<TKey, TElement>> GroupBy<TSource, TKey, TElement>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TSource, TElement>> elementSelector, IEqualityComparer<TKey> comparer);
  ;public static IQueryable<TResult> GroupBy<TSource, TKey, TElement, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TSource, TElement>> elementSelector, Expression<Func<TKey, IEnumerable<TElement>, TResult>> resultSelector);
  ;public static IQueryable<TResult> GroupBy<TSource, TKey, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TKey, IEnumerable<TSource>, TResult>> resultSelector, IEqualityComparer<TKey> comparer);
  ;public static IQueryable<TResult> GroupBy<TSource, TKey, TElement, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, Expression<Func<TSource, TElement>> elementSelector, Expression<Func<TKey, IEnumerable<TElement>, TResult>> resultSelector, IEqualityComparer<TKey> comparer);
  ;public static IQueryable<TResult> GroupJoin<TOuter, TInner, TKey, TResult>(this IQueryable<TOuter> outer, IEnumerable<TInner> inner, Expression<Func<TOuter, TKey>> outerKeySelector, Expression<Func<TInner, TKey>> innerKeySelector, Expression<Func<TOuter, IEnumerable<TInner>, TResult>> resultSelector);
  ;public static IQueryable<TResult> GroupJoin<TOuter, TInner, TKey, TResult>(this IQueryable<TOuter> outer, IEnumerable<TInner> inner, Expression<Func<TOuter, TKey>> outerKeySelector, Expression<Func<TInner, TKey>> innerKeySelector, Expression<Func<TOuter, IEnumerable<TInner>, TResult>> resultSelector, IEqualityComparer<TKey> comparer);
  ;public static IQueryable<TSource> Intersect<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2);
  ;public static IQueryable<TSource> Intersect<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2, IEqualityComparer<TSource> comparer);
  ;public static IQueryable<TResult> Join<TOuter, TInner, TKey, TResult>(this IQueryable<TOuter> outer, IEnumerable<TInner> inner, Expression<Func<TOuter, TKey>> outerKeySelector, Expression<Func<TInner, TKey>> innerKeySelector, Expression<Func<TOuter, TInner, TResult>> resultSelector);
  ;public static IQueryable<TResult> Join<TOuter, TInner, TKey, TResult>(this IQueryable<TOuter> outer, IEnumerable<TInner> inner, Expression<Func<TOuter, TKey>> outerKeySelector, Expression<Func<TInner, TKey>> innerKeySelector, Expression<Func<TOuter, TInner, TResult>> resultSelector, IEqualityComparer<TKey> comparer);
  ;public static TSource Last<TSource>(this IQueryable<TSource> source);
  ;public static TSource Last<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static TSource LastOrDefault<TSource>(this IQueryable<TSource> source);
  ;public static TSource LastOrDefault<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static long LongCount<TSource>(this IQueryable<TSource> source);
  ;public static long LongCount<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static TSource Max<TSource>(this IQueryable<TSource> source);
  ;public static TResult Max<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TResult>> selector);
  ;public static TSource Min<TSource>(this IQueryable<TSource> source);
  ;public static TResult Min<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TResult>> selector);
  ;public static IQueryable<TResult> OfType<TResult>(this IQueryable source);
  ;public static IOrderedQueryable<TSource> OrderBy<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector);
  ;public static IOrderedQueryable<TSource> OrderBy<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, IComparer<TKey> comparer);
  ;public static IOrderedQueryable<TSource> OrderByDescending<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector);
  ;public static IOrderedQueryable<TSource> OrderByDescending<TSource, TKey>(this IQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, IComparer<TKey> comparer);
  ;public static IQueryable<TSource> Reverse<TSource>(this IQueryable<TSource> source);
  ;public static IQueryable<TResult> Select<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, TResult>> selector);
  ;public static IQueryable<TResult> Select<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, int, TResult>> selector);
  ;public static IQueryable<TResult> SelectMany<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, IEnumerable<TResult>>> selector);
  ;public static IQueryable<TResult> SelectMany<TSource, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, int, IEnumerable<TResult>>> selector);
  ;public static IQueryable<TResult> SelectMany<TSource, TCollection, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, IEnumerable<TCollection>>> collectionSelector, Expression<Func<TSource, TCollection, TResult>> resultSelector);
  ;public static IQueryable<TResult> SelectMany<TSource, TCollection, TResult>(this IQueryable<TSource> source, Expression<Func<TSource, int, IEnumerable<TCollection>>> collectionSelector, Expression<Func<TSource, TCollection, TResult>> resultSelector);
  ;public static bool SequenceEqual<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2);
  ;public static bool SequenceEqual<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2, IEqualityComparer<TSource> comparer);
  ;public static TSource Single<TSource>(this IQueryable<TSource> source);
  ;public static TSource Single<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static TSource SingleOrDefault<TSource>(this IQueryable<TSource> source);
  ;public static TSource SingleOrDefault<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TSource> Skip<TSource>(this IQueryable<TSource> source, int count);
  ;public static IQueryable<TSource> SkipWhile<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TSource> SkipWhile<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int, bool>> predicate);
  ;public static decimal? Sum(this IQueryable<decimal?> source);
  ;public static double? Sum(this IQueryable<double?> source);
  ;public static int Sum(this IQueryable<int> source);
  ;public static long Sum(this IQueryable<long> source);
  ;public static int? Sum(this IQueryable<int?> source);
  ;public static double Sum(this IQueryable<double> source);
  ;public static long? Sum(this IQueryable<long?> source);
  ;public static decimal Sum(this IQueryable<decimal> source);
  ;public static float? Sum(this IQueryable<float?> source);
  ;public static float Sum(this IQueryable<float> source);
  ;public static decimal Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, decimal>> selector);
  ;public static double Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, double>> selector);
  ;public static int Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int>> selector);
  ;public static long Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, long>> selector);
  ;public static decimal? Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, decimal?>> selector);
  ;public static double? Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, double?>> selector);
  ;public static int? Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int?>> selector);
  ;public static long? Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, long?>> selector);
  ;public static float? Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, float?>> selector);
  ;public static float Sum<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, float>> selector);
  ;public static IQueryable<TSource> Take<TSource>(this IQueryable<TSource> source, int count);
  ;public static IQueryable<TSource> TakeWhile<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TSource> TakeWhile<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int, bool>> predicate);
  ;public static IOrderedQueryable<TSource> ThenBy<TSource, TKey>(this IOrderedQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector);
  ;public static IOrderedQueryable<TSource> ThenBy<TSource, TKey>(this IOrderedQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, IComparer<TKey> comparer);
  ;public static IOrderedQueryable<TSource> ThenByDescending<TSource, TKey>(this IOrderedQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector);
  ;public static IOrderedQueryable<TSource> ThenByDescending<TSource, TKey>(this IOrderedQueryable<TSource> source, Expression<Func<TSource, TKey>> keySelector, IComparer<TKey> comparer);
  ;public static IQueryable<TSource> Union<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2);
  ;public static IQueryable<TSource> Union<TSource>(this IQueryable<TSource> source1, IEnumerable<TSource> source2, IEqualityComparer<TSource> comparer);
  ;public static IQueryable<TSource> Where<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, bool>> predicate);
  ;public static IQueryable<TSource> Where<TSource>(this IQueryable<TSource> source, Expression<Func<TSource, int, bool>> predicate);

      
)     

#|
LINQ for R6RS Scheme
====================

See below code for examples and tests.


The following grammar is as per the C# spec [1]. 

Grammar notes:
- The optional type token is not present and has been removed from the grammar.
- IDENTIFIER is any Scheme identifier.
- Other uppercased or single quoted 'tokens' are translated to lowercased Scheme syntax, with exceptions.
- expression is any Scheme expression.
- boolean_expression is Scheme expression returning a boolean value.
- * denotes 0 or more.
- ? denotes optional.
- Differences in C++ style comments.
- Starts with query_expression.

query_expression
  : from_clause query_body
  ;
  
from_clause
  : FROM IDENTIFIER IN expression
  ;
  
query_body
  : query_body_clause* select_or_group_clause query_continuation?
  ;
  
query_body_clause
  : from_clause
  | let_clause
  | where_clause
  | join_clause
  | join_into_clause
  | orderby_clause
  ;
  
let_clause
  : LET IDENTIFIER '=' expression
  ;
  
where_clause
  : WHERE boolean_expression
  ;
  
join_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression 
  ;
  
join_into_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression INTO IDENTIFIER
  ;
  
orderby_clause
  : ORDERBY orderings
  ;

// ',' is replaced by THEN  
orderings
  : ordering
  | orderings ',' ordering
  ;
  
ordering
  : expression ordering_direction?
  ;

ordering_direction
  : ASCENDING
  | DESCENDING
  ;
  
select_or_group_clause
  : select_clause
  | group_clause
  ;
  
select_clause
  : SELECT expression
  ;
  
group_clause
  : GROUP expression BY expression
  ;

query_continuation
  : INTO IDENTIFIER query_body 
  ;

Iterator design:

http://csharpindepth.com/Articles/Chapter6/IteratorBlockImplementation.aspx
http://csharpindepth.com/Articles/Chapter11/StreamingAndIterators.aspx  

Transform process:

http://bartdesmet.net/blogs/bart/archive/2008/08/30/c-3-0-query-expression-translation-cheat-sheet.aspx


