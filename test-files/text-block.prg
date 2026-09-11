* TEXT ... ENDTEXT. The body is raw output, so none of it should be parsed as code -- the lines
* below deliberately contain things that would not parse on their own.
TEXT TO lcSql NOSHOW
  select cust_id, total
    from orders
   where total > 100
ENDTEXT

TEXT TO lcHtml ADDITIVE TEXTMERGE
  <p>IF this parsed as code, ENDIF would be missing.</p>
  <p>100% unbalanced ( parens ) and "quotes
ENDTEXT

TEXT
  A block with no options at all.
ENDTEXT
