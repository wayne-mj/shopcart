       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHOP-CART.
           AUTHOR "Wayne Jackson".
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * Define the file name and set it as a sequential file with each
      * line being beneath the previous.
       FILE-CONTROL.
           SELECT CSV-PRODUCT-FILE 
             ASSIGN TO "product.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CSV-SHOPPING-CART-FILE
             ASSIGN TO "shop-cart.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

      *    SELECT CSV-PRODUCT-DB
      *      ASSIGN TO "product.dat"
      *      ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *    *************************************************************
      *
      *    Variables and other related items for files
      *
      *    *************************************************************
       FILE SECTION.
      * File descriptor for CSV file
       FD  CSV-PRODUCT-FILE.
      * Each line should be no longer that 80 characters long
       01 CSV-PRODUCT-RECORD PIC X(80).
       
      * File descriptor for shop order CSV file
       FD  CSV-SHOPPING-CART-FILE.
      * Each line should be no longer that 80 characters long
       01  CSV-SHOPPING-CART-RECORD PIC X(80).
            
      *    *************************************************************
      *
      *    Working storage variables
      *
      *    ************************************************************* 

       WORKING-STORAGE SECTION.
      *    *************************************************************
      *
      *    END OF FILE marker(s)
      *
      *    *************************************************************

       01  WS-EOF01    PIC X(1) VALUE 'N'.
       01  WS-EOF02    PIC X(1) VALUE 'N'.

      *    *************************************************************
      *
      *    Constants and other such shenanigans.
      *
      *    *************************************************************
           
       01  WS-GAP      PIC X(4) VALUE SPACES.
      *01  WS-GAP      PIC X(4) VALUE "....".
       01  WS-COLS     PIC 9.
       01  WS-MAX      PIC 9(4) VALUE 9999.
       01  I           PIC 9(4) VALUE 0.
       01  J           PIC 9(4) VALUE 0.

       01  WS-DELIVERY-METHODS.
           05 WS-DEL   PIC X(8) VALUE "DELIVERY".
           05 WS-PU    PIC X(7) VALUE "PICK-UP".

      *    *************************************************************
      *    
      *    Required variables
      *
      *    *************************************************************
       01  WS-REQUIRED-VARIABLES.
           05 WS-CART-LINE      PIC 9(7)    VALUE 0.
           05 WS-DISP-ERR       PIC X(1)    VALUE "N".
           05 WS-DISP-MSG       PIC X(15)   VALUE SPACES.
           05 WS-RESP-OK        PIC X(1)    VALUE 'N'.
           05 WS-SILENT         PIC X(1)    VALUE 'N'.
           05 WS-MEMBER-RESP    PIC X(3)    VALUE SPACES.
           05 WS-PRODUCT-RESP   PIC X(2)    VALUE SPACES.
           05 WS-PRODUCT-NUM    PIC 9(2)    VALUE 0.
           05 WS-PRODUCT-CODE   PIC 9(4)    VALUE 0.
           05 WS-PRODUCT-DESC   PIC X(35)   VALUE SPACES.
           05 WS-PRODUCT-PRICE  PIC 9(5)V99 VALUE 0.
           05 WS-QUANT-RESP     PIC X(2)    VALUE SPACES.
           05 WS-QUANT-NUM      PIC 9(2)    VALUE 0.
           05 WS-DELIVERY       PIC X(15)   VALUE SPACES.
           05 WS-DELIVERY-NUM   PIC 9       VALUE 0.
           05 WS-SHIP-FEE       PIC 9(5)V99 VALUE 0.
           05 WS-COST           PIC 9(5)V99 VALUE 0.
           05 WS-TOTAL-COST     PIC 9(5)V99 VALUE 0.
           05 WS-FOUND          PIC X(1)    VALUE 'N'.
           05 WS-REPORT-Q       PIC 9(4).
           05 WS-REPORT-C       PIC 9(5)V99.
           05 WS-ERRORS         PIC 9(4)    VALUE 0.

      *    *************************************************************
      *
      *    Homeware City Storage variables
      *
      *    *************************************************************
       01  HOMEWARECITY-STORAGE.
           05 HWC-INDEX  PIC 9(10).
           05 HWC-CODE   PIC 9(2).
           05 SCT-INDEX  PIC 9(10).
           05 SCT-COUNT  PIC 9(10).
           05 SCT-IDXC   PIC 9(4).
           05 SCR-IDXC   PIC 9(4).
      *    *************************************************************
      *
      *    Data structures for the tables
      *
      *    *************************************************************
           05 PRODUCT-CATALOGUE-TABLE OCCURS 40 TIMES.
             10 PCT-CODE            PIC 9(4).
             10 PCT-PRODUCT         PIC X(35).
             10 PCT-PRICE           PIC 9(5)V99.
           
           05 PRODUCT-CATALOGUE-DISPLAY.
             10 PCD-CODE            PIC Z(4).
             10 PCD-PRODUCT         PIC X(35).
             10 PCD-PRICE           PIC Z(5).99.
           
           05 PRODUCT-CATALOGUE-HEADERS.
             10 PCH-CODE    PIC X(4)    VALUE "CODE".
             10 PCH-PROD    PIC X(35)   VALUE "PRODUCT NAME".
             10 PCH-PRICE   PIC X(8)    VALUE "$  PRICE".
           
      *    05 SHOPPING-CART-TABLE OCCURS 1000 TIMES.
      *      10 SCT-MEMBER    PIC X(3).
      *      10 SCT-CODE      PIC 9(4).
      *      10 SCT-PRODUCT   PIC X(35).
      *      10 SCT-PRICE     PIC 9(5)V9(2).
      *      10 SCT-QUANTITY  PIC 9(2).
      *      10 SCT-METHOD    PIC X(15).
      *      10 SCT-FEE       PIC 9(5)V99.
      *      10 SCT-COST      PIC 9(5)V99.
           
           05 SHOPTING-CART-TABLE-HEADERS.
             10 SCTH-MEMBER    PIC X(10)    VALUE "MEMBER".
             10 SCTH-CODE      PIC X(4)     VALUE "CODE".
             10 SCTH-PRODUCT   PIC X(35)    VALUE "PRODUCT".
             10 SCTH-PRICE     PIC X(8)     VALUE "$  PRICE".
             10 SCTH-QUANTITY  PIC X(8)     VALUE "QUANTITY".
             10 SCTH-METHOD    PIC X(15)    VALUE "SHIPPING METHOD".
             10 SCTH-FEE       PIC X(12)    VALUE "SHIPPING FEE".
             10 SCTH-COST      PIC X(8)     VALUE "$   COST".

           05 SHOPPING-CART-TABLE-INDEXED OCCURS 9999 TIMES
               ASCENDING KEY IS SCTI-CODE
               INDEXED BY SCT-IDX.
             10 SCTI-MEMBER    PIC X(3).
             10 SCTI-CODE      PIC 9(4).
             10 SCTI-PRODUCT   PIC X(35).
             10 SCTI-PRICE     PIC 9(5)V9(2).
             10 SCTI-QUANTITY  PIC 9(2).
             10 SCTI-METHOD    PIC X(15).
             10 SCTI-FEE       PIC 9(5)V99.
             10 SCTI-COST      PIC 9(5)V99.

           05 SHOPPING-CART-REPORT-INDEX OCCURS 9999 TIMES
               INDEXED BY SCR-IDX.
             10 SCRI-CODE      PIC 9(4).
             10 SCRI-PRODUCT   PIC X(35).
             10 SCRI-PRICE     PIC 9(5)V99.
             10 SCRI-QUANTITY  PIC 9(4).
             10 SCRI-COST      PIC 9(5)V99.
           
           05 TEMP-CART.
             10 FILLER    PIC X(3).
             10 FILLER    PIC 9(4).
             10 FILLER    PIC X(35).
             10 FILLER    PIC 9(5)V9(2).
             10 FILLER    PIC 9(2).
             10 FILLER    PIC X(15).
             10 FILLER    PIC 9(5)V99.
             10 FILLER    PIC 9(5)V99.  
           
           05 SHOPPING-CART-DISPLAY.
             10 SDC-MEMBER    PIC X(10).
             10 SCD-CODE      PIC Z(4).
             10 SCD-PRODUCT   PIC X(35).
             10 SCD-PRICE     PIC Z(5).99.
             10 SCD-QUANTITY  PIC Z(8).
             10 SCD-METHOD    PIC X(15).
             10 SCD-FEE       PIC Z(9).99.
             10 SCD-COST      PIC Z(5).99.
             10 SCD-TOTAL     PIC Z(5).99.
      
      *    *************************************************************
      *
      *    Main body of code
      *
      *    *************************************************************
       
       PROCEDURE DIVISION.
      *    REQUIRED FUNCTION
           PERFORM BUILD-CATALOGUE-TABLE
      *    REQUIRED FUNCTION

      *    PERFORM QUERY-USER-VERSION
           PERFORM QUERY-NON-INTERACTIVE-VERSION

      *    PERFORM BUILD-CATALOGUE-TABLE
      *    PERFORM PROCESS-SHOPPING-CART
      *    
      *    PERFORM SORT-TABLE
      *    PERFORM GENERATE-SHIPPING-REPORT
      *    PERFORM DISPLAY-SHIPPING-REPORT
      **    PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED
      *    DISPLAY " "
      *    DISPLAY "THERE WERE: " WS-ERRORS " ERRORS DETECTED"

           STOP RUN.
       
      *    *************************************************************
      *
      *    User interactive version of the code
      *
      *    *************************************************************
      
       QUERY-USER-VERSION.
      *    Commented out as this need to be done regardless of 
      *    automation of manual input
           MOVE 1 TO SCT-INDEX
           SET SCT-IDX TO 1

           PERFORM UNTIL WS-MEMBER-RESP EQUAL "END"
             PERFORM QUERY-IS-MEMBER
             IF WS-MEMBER-RESP NOT EQUAL "END"
               PERFORM DISPLAY-CATALOGUE
               PERFORM QUERY-PRODUCT-CODE
               PERFORM SEARCH-PRODUCT-CODE
               PERFORM QUERY-QUANTITY
               PERFORM QUERY-DELIVERY-METHOD
               PERFORM CALCULATE-SHIP-FEE
               PERFORM CALCULATE-COST

               PERFORM CONSOLIDATE-DATA-TO-TABLE-INDEXED
             END-IF
           END-PERFORM.
           
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED.
           DISPLAY " ... "
           PERFORM SORT-TABLE
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED
      *    PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE.
           .
       
       QUERY-NON-INTERACTIVE-VERSION.
           PERFORM PROCESS-SHOPPING-CART
           
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED
           
           DISPLAY " "

           PERFORM SORT-TABLE
           PERFORM GENERATE-SHIPPING-REPORT
           PERFORM DISPLAY-SHIPPING-REPORT

           DISPLAY " "
           DISPLAY "THERE WERE: " WS-ERRORS " ERRORS DETECTED"
       .
      *    *************************************************************
      *
      *    Consolidate the data into the data
      *    Increment the index
      *    Check how many records have been recorded so far and notify
      *    the user if closing in on the WS-MAX.
      *
      *    *************************************************************

      *CONSOLIDATE-DATA-TO-TABLE.
      *    MOVE WS-MEMBER-RESP TO SCT-MEMBER(SCT-INDEX)
      *    MOVE WS-PRODUCT-CODE TO SCT-CODE(SCT-INDEX)
      *    MOVE WS-PRODUCT-DESC TO SCT-PRODUCT(SCT-INDEX)
      *    MOVE WS-PRODUCT-PRICE TO SCT-PRICE(SCT-INDEX)
      *    MOVE WS-QUANT-NUM TO SCT-QUANTITY(SCT-INDEX)
      *    MOVE WS-DELIVERY TO SCT-METHOD(SCT-INDEX)
      *    MOVE WS-SHIP-FEE TO SCT-FEE(SCT-INDEX)
      *    MOVE WS-COST TO SCT-COST(SCT-INDEX)
      *
      *    ADD 1 TO SCT-INDEX
      *    MOVE SCT-INDEX TO SCT-COUNT
      *    IF SCT-INDEX EQUAL 9000 THEN
      *      DISPLAY "*** WARNING: " SCT-INDEX 
      *              " RECORDS OF " WS-MAX " ***"
      *    END-IF
      *.

       CONSOLIDATE-DATA-TO-TABLE-INDEXED.
           MOVE WS-MEMBER-RESP TO SCTI-MEMBER(SCT-IDX)
           MOVE WS-PRODUCT-CODE TO SCTI-CODE(SCT-IDX) 
           MOVE WS-PRODUCT-DESC TO SCTI-PRODUCT(SCT-IDX)
           MOVE WS-PRODUCT-PRICE TO SCTI-PRICE(SCT-IDX)
           MOVE WS-QUANT-NUM TO SCTI-QUANTITY(SCT-IDX)
           MOVE WS-DELIVERY TO SCTI-METHOD(SCT-IDX)
           MOVE WS-SHIP-FEE TO SCTI-FEE(SCT-IDX)
           MOVE WS-COST TO SCTI-COST(SCT-IDX)
           
           SET SCT-IDX UP BY 1
           MOVE SCT-IDX TO SCT-IDXC

           IF SCT-IDXC EQUAL 9000 THEN
             DISPLAY "*** WARNING: " SCT-IDXC 
                     " RECORDS OF " WS-MAX " ***"
           END-IF
       .
      
      *    *************************************************************
      *
      *    Display the consolidated data
      *
      *    *************************************************************

      *DISPLAY-CONSOLIDATED-DATA-TABLE.
      *    MOVE 1 TO SCT-INDEX
      *
      *    PERFORM UNTIL SCT-INDEX EQUAL SCT-COUNT
      *      DISPLAY SCT-MEMBER(SCT-INDEX) WS-GAP
      *              SCT-CODE(SCT-INDEX) WS-GAP
      *              SCT-PRODUCT(SCT-INDEX) WS-GAP
      *              SCT-PRICE(SCT-INDEX) WS-GAP
      *              SCT-QUANTITY(SCT-INDEX) WS-GAP
      *              SCT-METHOD(SCT-INDEX) WS-GAP
      *              SCT-FEE(SCT-INDEX) WS-GAP
      *              SCT-COST(SCT-INDEX)
      *      ADD 1 TO SCT-INDEX
      *    END-PERFORM.
       
       DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED.
           MOVE 0 TO WS-TOTAL-COST
           DISPLAY SCTH-MEMBER WS-GAP
                     SCTH-CODE WS-GAP
                     SCTH-PRODUCT WS-GAP
                     SCTH-PRICE WS-GAP
                     SCTH-QUANTITY WS-GAP
                     SCTH-METHOD WS-GAP
                     SCTH-FEE WS-GAP
                     SCTH-COST

           PERFORM VARYING SCT-IDX FROM 1 BY 1 UNTIL SCT-IDX 
                   EQUAL SCT-IDXC
             MOVE SCTI-MEMBER(SCT-IDX) TO SDC-MEMBER
             MOVE SCTI-CODE(SCT-IDX) TO SCD-CODE
             MOVE SCTI-PRODUCT(SCT-IDX) TO SCD-PRODUCT
             MOVE SCTI-PRICE(SCT-IDX) TO SCD-PRICE
             MOVE SCTI-QUANTITY(SCT-IDX) TO SCD-QUANTITY
             MOVE SCTI-METHOD(SCT-IDX) TO SCD-METHOD
             MOVE SCTI-FEE(SCT-IDX) TO SCD-FEE
             MOVE SCTI-COST(SCT-IDX) TO SCD-COST
             COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + SCTI-COST(SCT-IDX)

             DISPLAY SDC-MEMBER WS-GAP
                     SCD-CODE WS-GAP
                     SCD-PRODUCT WS-GAP
                     SCD-PRICE WS-GAP
                     SCD-QUANTITY WS-GAP
                     SCD-METHOD WS-GAP
                     SCD-FEE WS-GAP
                     SCD-COST
           END-PERFORM
           
           DISPLAY " "
           MOVE WS-TOTAL-COST TO SCD-TOTAL
           DISPLAY "TOTAL: $" SCD-TOTAL
           .

       GENERATE-SHIPPING-REPORT.
           MOVE 1 TO SCR-IDXC
           MOVE "N" TO WS-FOUND

           PERFORM VARYING SCT-IDX FROM 1 BY 1
                   UNTIL SCT-IDX EQUAL SCT-IDXC
             PERFORM VARYING SCR-IDX FROM 1 BY 1 
                     UNTIL SCR-IDX EQUAL SCR-IDXC
               IF SCRI-CODE(SCR-IDX) EQUAL SCTI-CODE(SCT-IDX) THEN
                 COMPUTE WS-REPORT-Q = SCRI-QUANTITY(SCR-IDX) +
                                       SCTI-QUANTITY(SCT-IDX)
                 COMPUTE WS-REPORT-C = SCRI-COST(SCR-IDX) +
                                       SCTI-COST(SCT-IDX)
                 MOVE WS-REPORT-Q TO SCRI-QUANTITY(SCR-IDX)
                 MOVE WS-REPORT-C TO SCRI-COST(SCR-IDX)

                 MOVE "Y" TO WS-FOUND
                 EXIT PERFORM
               END-IF
             END-PERFORM

             IF WS-FOUND EQUAL "N" THEN
               MOVE SCTI-CODE(SCT-IDX) TO SCRI-CODE(SCR-IDX)
               MOVE SCTI-PRODUCT(SCT-IDX) TO SCRI-PRODUCT(SCR-IDX)
               MOVE SCTI-PRICE(SCT-IDX) TO SCRI-PRICE(SCR-IDX)
               MOVE SCTI-QUANTITY(SCT-IDX) TO SCRI-QUANTITY(SCR-IDX)
               MOVE SCTI-COST(SCT-IDX) TO SCRI-COST(SCR-IDX)
               ADD 1 TO SCR-IDXC
             END-IF
             MOVE "N" TO WS-FOUND
           END-PERFORM
       .

       GENERATE-SHIPPING-REPORT-ORG.
           MOVE 1 TO SCR-IDXC

           PERFORM VARYING SCT-IDX FROM 1 BY 1 
                   UNTIL SCT-IDX EQUAL SCT-IDXC
             MOVE "N" TO WS-FOUND
             
             PERFORM VARYING SCR-IDX FROM 1 BY 1 
                     UNTIL SCR-IDX EQUAL SCR-IDXC
                     OR WS-FOUND EQUAL "Y"
             
               IF SCRI-CODE(SCR-IDX) EQUAL SCTI-CODE(SCT-IDX) THEN
                 COMPUTE SCRI-QUANTITY(SCR-IDX) =
                         SCRI-QUANTITY(SCR-IDX) + SCTI-QUANTITY(SCT-IDX)
                 COMPUTE SCRI-COST(SCR-IDX) = 
                         SCRI-COST(SCR-IDX) + SCTI-COST(SCT-IDX)
                 MOVE "Y" TO WS-FOUND
               END-IF
             END-PERFORM

             IF WS-FOUND EQUAL "N" THEN
               MOVE SCTI-CODE(SCT-IDX) TO SCRI-CODE(SCR-IDX)
               MOVE SCTI-PRODUCT(SCT-IDX) TO SCRI-PRODUCT(SCR-IDX)
               MOVE SCTI-PRICE(SCT-IDX) TO SCRI-PRICE(SCR-IDX)
               MOVE SCTI-QUANTITY(SCT-IDX) TO SCRI-QUANTITY(SCR-IDX)
               MOVE SCTI-COST(SCT-IDX) TO SCRI-COST(SCR-IDX)
               ADD 1 TO SCR-IDXC
             END-IF
           END-PERFORM.

       DISPLAY-SHIPPING-REPORT.
           SET SCR-IDX TO 1

           PERFORM VARYING SCR-IDX FROM 1 BY 1 
                                           UNTIL SCR-IDX EQUAL SCR-IDXC
             DISPLAY SCRI-CODE(SCR-IDX) WS-GAP
                     SCRI-PRODUCT(SCR-IDX) WS-GAP
                     SCRI-PRICE(SCR-IDX) WS-GAP
                     SCRI-QUANTITY(SCR-IDX) WS-GAP
                     SCRI-COST(SCR-IDX)
           END-PERFORM
       .
      *    *************************************************************
      *
      *    Build the product catalogue database from the product list and
      *    the product price list
      *
      *    *************************************************************

       BUILD-CATALOGUE-TABLE.
           OPEN INPUT CSV-PRODUCT-FILE.
           MOVE 1 TO HWC-INDEX
           PERFORM UNTIL WS-EOF01 EQUAL 'Y'
             READ CSV-PRODUCT-FILE
               AT END MOVE 'Y' TO WS-EOF01
               NOT AT END
                 MOVE HWC-INDEX TO HWC-CODE
                 MOVE HWC-CODE TO PCT-CODE(HWC-INDEX)
                 UNSTRING CSV-PRODUCT-RECORD
                   DELIMITED BY ','
                   INTO 
                     PCT-PRODUCT(HWC-INDEX)
                     PCT-PRICE(HWC-INDEX)
                 ADD 1 TO HWC-INDEX
             END-READ         
           END-PERFORM.
           CLOSE CSV-PRODUCT-FILE
           MOVE 'N' TO WS-EOF01.

      *    *************************************************************
      *
      *    Automate shopping cart processing
      *
      *    ************************************************************* 

       PROCESS-SHOPPING-CART.
           MOVE "N" TO WS-EOF01
           MOVE "Y" TO WS-SILENT
           OPEN INPUT CSV-SHOPPING-CART-FILE.
           PERFORM UNTIL WS-EOF01 EQUAL 'Y'
             READ CSV-SHOPPING-CART-FILE
               AT END MOVE 'Y' TO WS-EOF01
               NOT AT END
                 UNSTRING CSV-SHOPPING-CART-RECORD
                   DELIMITED BY "," INTO
                     WS-MEMBER-RESP
                     WS-PRODUCT-RESP
                     WS-QUANT-RESP
                     WS-DELIVERY

      *    All of this needs to succeed for the next part to proceed
      *    Any part fails, then it cannot proceed as an invalid choice
      *    has occurred and need to be addressed.
                 ADD 1 TO WS-CART-LINE
                 PERFORM PROCESS-IS-MEMBER
                 IF WS-RESP-OK EQUAL "Y" THEN
                   PERFORM PROCESS-PRODUCT-CODE
                   MOVE 'N' TO WS-DISP-ERR
                   IF WS-RESP-OK EQUAL "Y" THEN
                     PERFORM PROCESS-QUANTITY
                     MOVE 'N' TO WS-DISP-ERR
                     IF WS-RESP-OK EQUAL "Y" THEN
                       PERFORM PROCESS-DELIVERY
                       MOVE 'N' TO WS-DISP-ERR
                       IF WS-RESP-OK EQUAL "Y" THEN
                         PERFORM CALCULATE-SHIP-FEE
                         PERFORM CALCULATE-COST
                         PERFORM CONSOLIDATE-DATA-TO-TABLE-INDEXED
                         MOVE 'N' TO WS-DISP-ERR
                       ELSE
                         MOVE 'Y' TO WS-DISP-ERR
                         MOVE "DELIVERY" TO WS-DISP-MSG
                       END-IF
                     ELSE
                       MOVE 'Y' TO WS-DISP-ERR
                       MOVE "QUANTITY" TO WS-DISP-MSG
                     END-IF
                   ELSE
                     MOVE 'Y' TO WS-DISP-ERR
                     MOVE "CODE" TO WS-DISP-MSG
                   END-IF
                 ELSE
                   MOVE 'Y' TO WS-DISP-ERR
                   MOVE "MEMBER" TO WS-DISP-MSG
                 END-IF
      *          MOVE 'Y' TO WS-RESP-OK
                 IF WS-DISP-ERR EQUAL "Y" THEN
                   ADD 1 TO WS-ERRORS
                   DISPLAY "FIX ENTRY ON LINE: " WS-CART-LINE WS-GAP
                           "ERROR: " WS-DISP-MSG
                 END-IF
             END-READ
           END-PERFORM
           CLOSE CSV-SHOPPING-CART-FILE
           MOVE 'N' TO WS-EOF01
       .

      *    *************************************************************
      *
      *    Display the catalogue formatted in two columns with 
      *    headers and decorators
      *
      *    *************************************************************

       DISPLAY-CATALOGUE.
           PERFORM DISPLAY-CATALOGUE-HEADERS
           MOVE 1 TO HWC-INDEX
           MOVE 0 TO WS-COLS

           PERFORM UNTIL HWC-INDEX IS GREATER THAN 40
             MOVE PCT-CODE(HWC-INDEX) TO PCD-CODE
             MOVE PCT-PRODUCT(HWC-INDEX) TO PCD-PRODUCT
             MOVE PCT-PRICE(HWC-INDEX) TO PCD-PRICE
             IF WS-COLS EQUAL 0 THEN
               DISPLAY PCD-CODE WS-GAP
                       PCD-PRODUCT WS-GAP
                       PCD-PRICE WS-GAP
                       WITH NO ADVANCING
             ELSE
               DISPLAY PCD-CODE WS-GAP
                       PCD-PRODUCT WS-GAP
                       PCD-PRICE WS-GAP           
             END-IF
             
             ADD 1 TO WS-COLS
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF

             ADD 1 TO HWC-INDEX
           END-PERFORM.

      *    *************************************************************
      *
      *    Generate the headers for the product catalogue table
      *
      *    *************************************************************

       DISPLAY-CATALOGUE-HEADERS.
           MOVE 0 TO WS-COLS
           PERFORM UNTIL WS-COLS EQUAL 2
             IF WS-COLS EQUAL 0 THEN
               DISPLAY 
                 PCH-CODE WS-GAP
                 PCH-PROD WS-GAP
                 PCH-PRICE WS-GAP
                 WITH NO ADVANCING 
             ELSE
               DISPLAY
                 PCH-CODE WS-GAP
                 PCH-PROD WS-GAP
                 PCH-PRICE WS-GAP
             END-IF
             ADD 1 TO WS-COLS
           END-PERFORM
           MOVE 0 TO WS-COLS
           
           PERFORM UNTIL WS-COLS EQUAL 2
             IF WS-COLS EQUAL 0 THEN
               DISPLAY 
                 "====" WS-GAP
                 "===================================" WS-GAP
                 "========" WS-GAP
                 WITH NO ADVANCING 
             ELSE
               DISPLAY
                 "====" WS-GAP
                 "===================================" WS-GAP
                 "========" WS-GAP
             END-IF
             ADD 1 TO WS-COLS
           END-PERFORM
           MOVE 0 TO WS-COLS.

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
           MOVE 1 TO HWC-INDEX
           
           PERFORM UNTIL HWC-INDEX GREATER 40
             IF PCT-CODE(HWC-INDEX) EQUAL WS-PRODUCT-NUM THEN
               MOVE PCT-CODE(HWC-INDEX) TO WS-PRODUCT-CODE
               MOVE PCT-PRODUCT(HWC-INDEX) TO WS-PRODUCT-DESC
               MOVE PCT-PRICE(HWC-INDEX) TO WS-PRODUCT-PRICE
               EXIT PERFORM
             ELSE
               ADD 1 TO HWC-INDEX
             END-IF
           END-PERFORM
           MOVE 1 TO HWC-INDEX.

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
      *        IF WS-RESP-OK EQUAL 'Y' THEN
      *          MOVE WS-QUANT-NUM TO SCT-QUANTITY
      *        END-IF
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
      *      IF WS-RESP-OK EQUAL "Y"
      *        MOVE WS-DELIVERY TO SCT-METHOD
      *      END-IF
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
           PERFORM PROCESS-DELIVERY-METHOD
       .

       VALIDATE-DELIVERY-METHOD.
           EVALUATE WS-DELIVERY
      *      WHEN "DELIVERY"
             WHEN WS-DEL
               MOVE "Y" TO WS-RESP-OK
               MOVE 1 TO WS-DELIVERY-NUM
      *      WHEN "PICK-UP"
             WHEN WS-PU
               MOVE "Y" TO WS-RESP-OK
               MOVE 2 TO WS-DELIVERY-NUM
             WHEN OTHER
               IF WS-SILENT EQUAL "N" THEN
                 DISPLAY "INVALID DELIVERY METHOD. " 
                         "CHOOSE 'DELIVERY' OR 'PICK-UP'."
               END-IF
           END-EVALUATE.

       PROCESS-DELIVERY-METHOD.
           MOVE 0 TO WS-DELIVERY-NUM
           IF WS-DELIVERY EQUAL WS-DEL THEN
             MOVE 1 TO WS-DELIVERY-NUM
           IF WS-DELIVERY EQUAL WS-PU THEN
             MOVE 2 TO WS-DELIVERY-NUM
           END-IF.

      *    *************************************************************
      *
      *    Functions and methods to calculate shipping and costs
      *
      *    *************************************************************

       CALCULATE-SHIP-FEE.
           MOVE 0 TO WS-SHIP-FEE
           
           IF WS-DELIVERY-NUM EQUAL 1 THEN
             IF WS-QUANT-NUM GREATER THAN 1 THEN
               COMPUTE WS-SHIP-FEE = 2.00 + 
                       (( WS-QUANT-NUM - 1 ) * 1.60)
             ELSE
               MOVE 2.00 TO WS-SHIP-FEE
             END-IF
           END-IF.

       CALCULATE-COST.
           MOVE 0 TO WS-COST

           COMPUTE WS-COST = (WS-QUANT-NUM * WS-PRODUCT-PRICE) + 
                              WS-SHIP-FEE

           IF WS-MEMBER-RESP EQUAL "YES" THEN
             COMPUTE WS-COST = WS-COST * (90 / 100)
           END-IF.

      *    *************************************************************
      *
      *    Functions and methods to perform Bubble sort
      *    This is based loosely on the W3 School Python version
      *
      *    *************************************************************

       SORT-TABLE.
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > SCT-IDXC - 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL SCT-IDXC
      *      PERFORM VARYING J FROM I BY 1 UNTIL J > SCT-IDXC - 1
             PERFORM VARYING J FROM I BY 1 UNTIL J EQUAL SCT-IDXC
               IF SCTI-CODE(I) > SCTI-CODE(J)
                 PERFORM SWAP-RECORD
               END-IF
             END-PERFORM
           END-PERFORM
       .

       SWAP-RECORD.
           MOVE SHOPPING-CART-TABLE-INDEXED(I) TO TEMP-CART
           MOVE SHOPPING-CART-TABLE-INDEXED(J) TO
                SHOPPING-CART-TABLE-INDEXED(I)
           MOVE TEMP-CART TO SHOPPING-CART-TABLE-INDEXED(J)
       .

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
       

       
