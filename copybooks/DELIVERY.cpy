      *    *************************************************************
      *
      *    Functions and methods to query to query the delivery method
      *    and to validate the response from the user
      *
      *    *************************************************************

       QUERY-DELIVERY-METHOD.
           MOVE "N" TO WS-RESP-OK
           MOVE SPACES TO WS-DELIVERY

           PERFORM UNTIL WS-RESP-OK EQUAL "Y"
             DISPLAY "DELIVERY METHOD? (DELIVERY/PICK-UP): "
               WITH NO ADVANCING
             ACCEPT WS-DELIVERY
             PERFORM VALIDATE-DELIVERY-METHOD
             PERFORM PROCESS-DELIVERY-METHOD
           END-PERFORM
           MOVE "N" TO WS-RESP-OK.
       
      *    *************************************************************
      *
      *    Validate the delivery methods that the user has entered,
      *    if they have entered in an invalid choice, notify them
      *
      *    *************************************************************

       PROCESS-DELIVERY.
           MOVE 'N' TO WS-RESP-OK
           PERFORM VALIDATE-DELIVERY-METHOD
      *    PERFORM PROCESS-DELIVERY-METHOD
       .

      *    Validate that the appropriate delivery method is chosen
       VALIDATE-DELIVERY-METHOD.
           EVALUATE WS-DELIVERY
             WHEN WS-DEL
               MOVE "Y" TO WS-RESP-OK
             WHEN WS-PU
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
               IF WS-SILENT EQUAL "N" THEN
                 DISPLAY "INVALID DELIVERY METHOD. " 
                         "CHOOSE 'DELIVERY' OR 'PICK-UP'."
               END-IF
           END-EVALUATE.

      *    This is used to determine the what type of delivery method
      *    was used for calculations
       PROCESS-DELIVERY-METHOD.
           MOVE 0 TO WS-DELIVERY-NUM
           IF WS-DELIVERY EQUAL WS-DEL THEN
             MOVE 1 TO WS-DELIVERY-NUM
           IF WS-DELIVERY EQUAL WS-PU THEN
             MOVE 2 TO WS-DELIVERY-NUM
           END-IF.
           