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
       01  WS-MAX      PIC 9(4) VALUE 1000.

       01  WS-DELIVERY-METHODS.
           05 WS-DEL   PIC X(8) VALUE "DELIVERY".
           05 WS-PU    PIC X(7) VALUE "PICK-UP".

      *    *************************************************************
      *    
      *    Required variables
      *
      *    *************************************************************
       01  WS-REQUIRED-VARIABLES.
           05 WS-RESP-OK        PIC X(1)    VALUE 'N'.
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
      *    05 SCT-IDX    PIC 9(4).
           05 SCT-IDXC   PIC 9(4).
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
           
           05 SHOPPING-CART-TABLE OCCURS 1000 TIMES.
             10 SCT-MEMBER    PIC X(3).
             10 SCT-CODE      PIC 9(4).
             10 SCT-PRODUCT   PIC X(35).
             10 SCT-PRICE     PIC 9(5)V9(2).
             10 SCT-QUANTITY  PIC 9(2).
             10 SCT-METHOD    PIC X(15).
             10 SCT-FEE       PIC 9(5)V99.
             10 SCT-COST      PIC 9(5)V99.
           
           05 SHOPPING-CART-TABLE-INDEXED OCCURS 1000 TIMES
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
           
           05 SHOPPING-CART-DISPLAY.
             10 SDC-MEMBER    PIC X(3).
             10 SCD-CODE      PIC Z(4).
             10 SCD-PRODUCT   PIC X(35).
             10 SCD-PRICE     PIC Z(5).99.
             10 SCD-QUANTITY  PIC Z(2).
             10 SCD-METHOD    PIC X(15).
             10 SCD-FEE       PIC Z(5).99.
             10 SCD-COST      PIC Z(5).99.
      
      *    *************************************************************
      *
      *    Main body of code
      *
      *    *************************************************************
       
       PROCEDURE DIVISION.
           PERFORM QUERY-USER-VERSION
      *    PERFORM BUILD-CATALOGUE-TABLE
      *    
      *    PERFORM UNTIL WS-MEMBER-RESP EQUAL "END"
      *      PERFORM QUERY-IS-MEMBER
      *      IF WS-MEMBER-RESP NOT EQUAL "END"
      *        DISPLAY WS-MEMBER-RESP
      *      END-IF
      *    END-PERFORM

           STOP RUN.
       
      *    *************************************************************
      *
      *    User interactive version of the code
      *
      *    *************************************************************
      
       QUERY-USER-VERSION.
           PERFORM BUILD-CATALOGUE-TABLE
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
      *        DISPLAY WS-MEMBER-RESP WS-GAP
      *                WS-PRODUCT-CODE WS-GAP
      *                WS-PRODUCT-DESC WS-GAP
      *                WS-PRODUCT-PRICE WS-GAP
      *                WS-QUANT-NUM WS-GAP
      *                WS-DELIVERY WS-GAP
      *                WS-SHIP-FEE WS-GAP
      *                WS-COST WS-GAP
             END-IF
           END-PERFORM.
           
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED.
           DISPLAY " ... "
           PERFORM SORT-TABLE
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED
      *    PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE.
           .

      *    *************************************************************
      *
      *    Consolidate the data into the data
      *    Increment the index
      *    Check how many records have been recorded so far and notify
      *    the user if closing in on the WS-MAX.
      *
      *    *************************************************************

       CONSOLIDATE-DATA-TO-TABLE.
           MOVE WS-MEMBER-RESP TO SCT-MEMBER(SCT-INDEX)
           MOVE WS-PRODUCT-CODE TO SCT-CODE(SCT-INDEX)
           MOVE WS-PRODUCT-DESC TO SCT-PRODUCT(SCT-INDEX)
           MOVE WS-PRODUCT-PRICE TO SCT-PRICE(SCT-INDEX)
           MOVE WS-QUANT-NUM TO SCT-QUANTITY(SCT-INDEX)
           MOVE WS-DELIVERY TO SCT-METHOD(SCT-INDEX)
           MOVE WS-SHIP-FEE TO SCT-FEE(SCT-INDEX)
           MOVE WS-COST TO SCT-COST(SCT-INDEX)

           ADD 1 TO SCT-INDEX
           MOVE SCT-INDEX TO SCT-COUNT
           IF SCT-INDEX EQUAL 900 THEN
             DISPLAY "WARNING: " SCT-INDEX " RECORDS OF " WS-MAX
           END-IF
       .

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

           IF SCT-IDXC EQUAL 900 THEN
             DISPLAY "WARNING: " SCT-IDXC " RECORDS OF " WS-MAX
           END-IF
       .
      
      *    *************************************************************
      *
      *    Display the consolidated data
      *
      *    *************************************************************

       DISPLAY-CONSOLIDATED-DATA-TABLE.
           MOVE 1 TO SCT-INDEX

           PERFORM UNTIL SCT-INDEX EQUAL SCT-COUNT
             DISPLAY SCT-MEMBER(SCT-INDEX) WS-GAP
                     SCT-CODE(SCT-INDEX) WS-GAP
                     SCT-PRODUCT(SCT-INDEX) WS-GAP
                     SCT-PRICE(SCT-INDEX) WS-GAP
                     SCT-QUANTITY(SCT-INDEX) WS-GAP
                     SCT-METHOD(SCT-INDEX) WS-GAP
                     SCT-FEE(SCT-INDEX) WS-GAP
                     SCT-COST(SCT-INDEX)
             ADD 1 TO SCT-INDEX
           END-PERFORM.
       
       DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED.
           PERFORM VARYING SCT-IDX FROM 1 BY 1 UNTIL SCT-IDX 
                   EQUAL SCT-IDXC
           DISPLAY SCTI-MEMBER(SCT-IDX) WS-GAP
                   SCTI-CODE(SCT-IDX) WS-GAP
                   SCTI-PRODUCT(SCT-IDX) WS-GAP
                   SCTI-PRICE(SCT-IDX) WS-GAP
                   SCTI-QUANTITY(SCT-IDX) WS-GAP
                   SCTI-METHOD(SCT-IDX) WS-GAP
                   SCTI-FEE(SCT-IDX) WS-GAP
                   SCTI-COST(SCT-IDX)
           END-PERFORM.

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

       VALIDATE-MEMBER.
           EVALUATE WS-MEMBER-RESP
             WHEN "YES"
               MOVE 'Y' TO WS-RESP-OK
             WHEN "NO"
               MOVE 'Y' TO WS-RESP-OK
             WHEN "END"
               MOVE 'Y' TO WS-RESP-OK
             WHEN OTHER
              DISPLAY "INVALID INPUT: 'YES/NO/END' ONLY."
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

       VALIDATE-PRODUCT-CODE.
           EVALUATE TRUE
             WHEN WS-PRODUCT-NUM GREATER 0 AND WS-PRODUCT-NUM LESS 41
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
              DISPLAY "INVALID INPUT: '1-40' ONLY."
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

       VALIDATE-QUANTITY.
           EVALUATE TRUE
            WHEN WS-QUANT-NUM GREATER 0 AND LESS 30
              MOVE 'Y' TO WS-RESP-OK
            WHEN OTHER
              DISPLAY "INVALID QUANTITY BETWEEN 1 AND 29 INCLUSIVELY."
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
              DISPLAY "INVALID DELIVERY METHOD. " 
                      "CHOOSE 'DELIVERY' OR 'PICK-UP'."
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

       SORT-TABLE.
           SORT SHOPPING-CART-TABLE-INDEXED ON ASCENDING KEY SCTI-CODE
       .

       
