      *    *************************************************************
      *
      *    Functions and methods to query to query the quantity
      *    and to validate the response from the user
      *
      *    *************************************************************

       QUERY-QUANTITY.
           MOVE 'N' TO WS-RESP-OK
           MOVE SPACES TO WS-QUANT-RESP
           MOVE 0 TO WS-QUANT-NUM

           PERFORM UNTIL WS-RESP-OK EQUAL 'Y'
             DISPLAY "HOW MANY ITEMS TO DISPLAY? (1-29): "
               WITH NO ADVANCING
               ACCEPT WS-QUANT-RESP
               COMPUTE WS-QUANT-NUM = FUNCTION NUMVAL(WS-QUANT-RESP)
               PERFORM VALIDATE-QUANTITY
           END-PERFORM.
      
      *    *************************************************************
      *
      *    Validate that the quantity entered is within the predefined
      *    range, and if it is not notify the user
      *
      *    *************************************************************

       PROCESS-QUANTITY.
           MOVE 'N' TO WS-RESP-OK
           
           COMPUTE WS-QUANT-NUM = FUNCTION NUMVAL(WS-QUANT-RESP)
           PERFORM VALIDATE-QUANTITY
       .

      *    Ensure that the quantity is between the ranges of 1 and 29
       VALIDATE-QUANTITY.
           EVALUATE TRUE
            WHEN WS-QUANT-NUM GREATER 0 AND LESS 30
              MOVE 'Y' TO WS-RESP-OK
            WHEN OTHER
              MOVE "N" TO WS-RESP-OK
              IF WS-SILENT EQUAL 'N' THEN
                DISPLAY "INVALID QUANTITY BETWEEN 1 AND 29 INCLUSIVELY."
              END-IF
           END-EVALUATE.
