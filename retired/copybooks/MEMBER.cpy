      *    *************************************************************
      *
      *    Functions and methods to query if the customer is a member
      *    and to validate the response from the user
      *
      *    *************************************************************       

       QUERY-IS-MEMBER.
           MOVE SPACE TO WS-MEMBER-RESP
           MOVE "N" TO WS-RESP-OK
           
           PERFORM UNTIL WS-RESP-OK = 'Y'
             DISPLAY "IS THE CUSTOMER A MEMBER? (YES/NO/END): "
               WITH NO ADVANCING
             ACCEPT WS-MEMBER-RESP
             PERFORM VALIDATE-MEMBER
           END-PERFORM
           MOVE "N" TO WS-RESP-OK.
      
      *    *************************************************************
      *
      *    Validate that the entered response is correct and notify the
      *    user with a response if it is not
      *
      *    *************************************************************

       PROCESS-IS-MEMBER.
           MOVE 'N' TO WS-RESP-OK
           
           PERFORM VALIDATE-MEMBER
       .

      *    Validate whether the response is valid or not
       VALIDATE-MEMBER.
           EVALUATE WS-MEMBER-RESP
             WHEN "YES"
               MOVE 'Y' TO WS-RESP-OK
             WHEN "NO"
               MOVE 'Y' TO WS-RESP-OK
             WHEN "END"
               MOVE 'Y' TO WS-RESP-OK
             WHEN OTHER
               MOVE 'N' TO WS-RESP-OK
               IF WS-SILENT EQUAL "N"
                 DISPLAY "INVALID INPUT: 'YES/NO/END' ONLY."
               END-IF
           END-EVALUATE.
