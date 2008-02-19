#|


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

|#

(library (ironscheme linq)
  (export
    from)
  (import 
    (rnrs)
    (ironscheme clr))
 
  (define-syntax from
    (lambda (x)
      (syntax-case x (in)
        [(_ id in expr)    #'(cons 'id expr)])))
)