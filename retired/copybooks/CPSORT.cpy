      *    *************************************************************
      *    
      *    I asked Copilot for help as I was losing the first record.
      *    I did not realise I was iterating beyond the indexed bounds
      *    which meant that 0 was possible and this was being added to
      *    to the table.  Copilot also suggested a fix to stop self 
      *    referencing the first element and this is what it came up 
      *    with.  I kept it separate from my code as it is not my work.
      *
      *    *************************************************************

       COPILOT-SORT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= SCT-IDXC - 1
             PERFORM VARYING J FROM 1 BY 1 UNTIL J >= SCT-IDXC - I
               IF SCTI-CODE(J) > SCTI-CODE(J + 1)
                 PERFORM COPILOT-SWAP-RECORD
               END-IF
             END-PERFORM
           END-PERFORM
       .

       COPILOT-SWAP-RECORD.
           MOVE SHOPPING-CART-TABLE-INDEXED(J) TO TEMP-CART
           MOVE SHOPPING-CART-TABLE-INDEXED(J + 1) TO
                SHOPPING-CART-TABLE-INDEXED(J)
           MOVE TEMP-CART TO SHOPPING-CART-TABLE-INDEXED(J + 1)
       .
