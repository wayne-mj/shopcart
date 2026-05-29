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

      *    *************************************************************
      *    
      *    Required variables
      *
      *    *************************************************************
       01   WS-REQUIRED-VARIABLES.
           05 WS-RESP-OK        PIC X(1) VALUE 'N'.
           05 WS-MEMBER-RESP    PIC X(3) VALUE SPACE.
           05 WS-PRODUCT-RESP   PIC X(2).
           05 WS-PRODUCT-NUM    PIC 9(2).
           
      *    *************************************************************
      *
      *    Homeware City Storage variables
      *
      *    *************************************************************
       01  HOMEWARECITY-STORAGE.
           05 HWC-INDEX  PIC 9(10).
           05 HWC-CODE   PIC 9(2).
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
           
           05 SHOPPING-CART-TABLE.
             10 SCT-MEMBER    PIC X(3).
             10 SCT-CODE      PIC 9(4).
             10 SCT-PRODUCT   PIC X(35).
             10 SCT-PRICE     PIC 9(5)V9(2).
             10 SCT-QUANTITY  PIC 9(2).
             10 SCT-METHOD    PIC X(15).
             10 SCT-FEE       PIC 9(5)V99.
             10 SCT-COST      PIC 9(5)V99.
      *    *************************************************************
      *
      *    Main body of code
      *
      *    *************************************************************
       PROCEDURE DIVISION.
           PERFORM BUILD-CATALOGUE-TABLE

      *    PERFORM UNTIL WS-MEMBER-RESP EQUAL "END"
      *      PERFORM QUERY-IS-MEMBER
      *    END-PERFORM

           STOP RUN.
      
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
           END-PERFORM
           .

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
           MOVE 0 TO WS-COLS
           .

      *    *************************************************************
      *
      *    Functions and methods to query if the customer is a member
      *    and to validate the response from the user
      *
      *    *************************************************************       

       QUERY-IS-MEMBER.
           MOVE SPACE TO WS-MEMBER-RESP
           
           PERFORM UNTIL WS-RESP-OK = 'Y'
             DISPLAY "IS THE CUSTOMER A MEMBER? (YES/NO/END): "
               WITH NO ADVANCING
             ACCEPT WS-MEMBER-RESP
             PERFORM VALIDATE-MEMBER
           END-PERFORM
           MOVE "N" TO WS-RESP-OK
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
              DISPLAY "INVALID INPUT: 'YES/NO/END' ONLY."
           END-EVALUATE
       .
      
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
             COMPUTE WS-PRODUCT-NUM = FUNCTION NUMVAL (WS-MEMBER-RESP)
             PERFORM VALIDATE-PRODUCT-CODE       
           END-PERFORM
           MOVE "N" TO WS-RESP-OK
           MOVE SPACES TO WS-PRODUCT-RESP
           .

       VALIDATE-PRODUCT-CODE.
           EVALUATE TRUE
             WHEN WS-PRODUCT-NUM GREATER 0 AND WS-PRODUCT-NUM LESS 41
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
              DISPLAY "INVALID INPUT: '1-40' ONLY."
           END-EVALUATE
       .

       SEARCH-PRODUCT-CODE.
           MOVE 1 TO HWC-INDEX
           
           PERFORM UNTIL HWC-INDEX GREATER 40
             IF PCT-CODE(HWC-INDEX) EQUAL WS-PRODUCT-NUM THEN
               MOVE PCT-CODE(HWC-INDEX) TO SCT-CODE
               MOVE PCT-PRODUCT(HWC-INDEX) TO SCT-PRODUCT
               MOVE PCT-PRICE(HWC-INDEX) TO SCT-PRICE
               EXIT PERFORM
             ELSE
               ADD 1 TO HWC-INDEX
             END-IF
           END-PERFORM
           MOVE 1 TO HWC-INDEX
       .
