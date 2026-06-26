      *    *************************************************************
      *
      *    Functions and methods to perform Bubble sort
      *    This is based loosely on the W3 School Python version
      *
      *    *************************************************************

       SORT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL SCT-IDXC
             PERFORM VARYING J FROM I BY 1 UNTIL J EQUAL SCT-IDXC
               IF SCTI-CODE(I) > SCTI-CODE(J)
                 PERFORM SWAP-RECORD
               END-IF
             END-PERFORM
           END-PERFORM
       .

      *    Swap the records around using a temporary table.
       SWAP-RECORD.
           MOVE SHOPPING-CART-TABLE-INDEXED(I) TO TEMP-CART
           MOVE SHOPPING-CART-TABLE-INDEXED(J) TO
                SHOPPING-CART-TABLE-INDEXED(I)
           MOVE TEMP-CART TO SHOPPING-CART-TABLE-INDEXED(J)
       .

