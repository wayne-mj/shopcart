      *    *************************************************************
      *
      *    Functions and methods to query to query the product code
      *    and to validate the response from the user
      *
      *    *************************************************************   

       QUERY-PRODUCT-CODE.
           MOVE 'N' TO WS-RESP-OK
           MOVE 0 TO WS-PRODUCT-NUM
           MOVE SPACES TO WS-PRODUCT-RESP

           PERFORM UNTIL WS-RESP-OK EQUAL 'Y'
             DISPLAY "SELECT A PRODUCT (1-40): "
               WITH NO ADVANCING
             ACCEPT WS-PRODUCT-RESP
             COMPUTE WS-PRODUCT-NUM = FUNCTION NUMVAL (WS-PRODUCT-RESP)
             PERFORM VALIDATE-PRODUCT-CODE       
           END-PERFORM
           MOVE "N" TO WS-RESP-OK
           MOVE SPACES TO WS-PRODUCT-RESP.

      *    *************************************************************
      *
      *    Validate the product code entered is within the predefined
      *    range, otherwise notify the user with an appropriate message
      *
      *    *************************************************************

       PROCESS-PRODUCT-CODE.
           MOVE 'N' TO WS-RESP-OK
           
           COMPUTE WS-PRODUCT-NUM = FUNCTION NUMVAL(WS-PRODUCT-RESP)
           PERFORM VALIDATE-PRODUCT-CODE
           IF WS-RESP-OK EQUAL "Y" THEN
             PERFORM SEARCH-PRODUCT-CODE
           END-IF
       .
      
      *    Ensure that the product code is between the ranges of
      *    1 and 40
       VALIDATE-PRODUCT-CODE.
           EVALUATE TRUE
             WHEN WS-PRODUCT-NUM GREATER 0 AND WS-PRODUCT-NUM LESS 41
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
               MOVE "N" TO WS-RESP-OK
               IF WS-SILENT EQUAL 'N'
                DISPLAY "INVALID INPUT: '1-40' ONLY."
               END-IF
           END-EVALUATE.

      *    *************************************************************
      *
      *    Execute the search of the table for the desired product.
      *    COBOL has a dedicated SEARCH function, but as the table is 
      *    not using an actual INDEX which would require the use of SET
      *    rather than MOVE/ADD PERFORM loops are used instead.
      *
      *    *************************************************************

       SEARCH-PRODUCT-CODE.
           SET HWC-IDX TO 1
           SEARCH PRODUCT-CATALOGUE-TABLE
             AT END
               DISPLAY "ITEM NOT FOUND"
             WHEN PCT-CODE(HWC-IDX) EQUAL WS-PRODUCT-NUM
               MOVE PCT-CODE(HWC-IDX) TO WS-PRODUCT-CODE
               MOVE PCT-PRODUCT(HWC-IDX) TO WS-PRODUCT-DESC
               MOVE PCT-PRICE(HWC-IDX) TO WS-PRODUCT-PRICE
           END-SEARCH
           .
