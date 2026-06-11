       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * Define the file name and set it as a sequential file with each
      * line being beneath the previous.
       FILE-CONTROL.
           SELECT CSV-PRODUCT-FILE 
             ASSIGN TO "../product.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRODUCT-DB
             ASSIGN TO "product.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
      * File descriptor for CSV file
       FD  CSV-PRODUCT-FILE.
      * Each line should be no longer that 80 characters long
       01 CSV-PRODUCT-RECORD PIC X(80).
       
       FD  PRODUCT-DB.
       01  PRODUCT-DB-RECORD.
           05 DB-CODE  PIC 9(4).
           05 FILLER   PIC X(4).
           05 DB-PROD  PIC X(35).
           05 FILLER   PIC X(4).
           05 DB-PRICE PIC Z(5).99.
       
       WORKING-STORAGE SECTION.
       01  PCT-IDXC          PIC 9(4).
       01  SCTI-IDXC         PIC 9(4).
       01  WS-EOF            PIC X(1) VALUE "N".
       01  WS-GAP            PIC X(4) VALUE SPACES.
       01  WS-SEARCH         PIC 9(4).
       01  WS-RET            PIC 9.
       01  WS-COLS           PIC 9.

       01  HOMEWARECITY-STORAGE.
           05 PRODUCT-CATALOGUE-TABLE OCCURS 40 TIMES
                                      ASCENDING KEY IS PCT-CODE
                                      INDEXED BY PCT-IDX.
             10 PCT-CODE     PIC 9(4).
             10 PCT-PRODUCT  PIC X(35).
             10 PCT-PRICE    PIC 9(5)V99.
           
           05 PRODUCT-CATALOGUE-HEADERS.
             10 PCH-CODE     PIC X(4)  VALUE "CODE".
             10 PCH-PRODUCT  PIC X(35) VALUE "PRODUCT".
             10 PCH-PRICE    PIC X(7)  VALUE "$ PRICE".

           05 PRODUCT-CATALOGUE-DISPLAY.
             10 PCD-CODE     PIC Z(4).
             10 FILLER       PIC X(4).
             10 PCD-PRODUCT  PIC X(35).
             10 FILLER       PIC X(4).
             10 PCD-PRICE    PIC Z(4).99.

           05 SHOPPING-CART-TABLE-INDEX OCCURS 9999 TIMES
                                        ASCENDING KEY IS SCTI-CODE
                                        INDEXED BY SCTI-IDX.
             10 SCTI-MEMBER    PIC X(3).
             10 SCTI-CODE      PIC 9(4).
             10 SCTI-PRODUCT   PIC X(35).
             10 SCTI-PRICE     PIC 9(5)V99.
             10 SCTI-QUANTITY  PIC 9(2).
             10 SCTI-METHOD    PIC X(15).
             10 SCTI-FEE       PIC 9(5)V99.
             10 SCTI-COST      PIC 9(5)V99.
           
           05 SHOPPING-CART-TABLE-HEADERS.
             10 SCTH-MEMBER    PIC X(10)    VALUE "MEMBER".
             10 SCTH-CODE      PIC X(4)     VALUE "CODE".
             10 SCTH-PRODUCT   PIC X(35)    VALUE "PRODUCT".
             10 SCTH-PRICE     PIC X(7)     VALUE "$ PRICE".
             10 SCTH-QUANTITY  PIC X(8)     VALUE "QUANTITY".
             10 SCTH-METHOD    PIC X(15)    VALUE "SHIPPING METHOD".
             10 SCTH-FEE       PIC X(12)    VALUE "SHIPPING FEE".
             10 SCTH-COST      PIC X(7)     VALUE "$  COST".
           
           05 SHOPPING-CART-TABLE-DISPLAY.
             10 SCTD-MEMBER    PIC X(10).
             10 SCTD-CODE      PIC Z(4).
             10 SCTD-PRODUCT   PIC X(35).
             10 SCTD-PRICE     PIC Z(4).99.
             10 SCTD-QUANTITY  PIC Z(8).
             10 SCTD-METHOD    PIC X(15).
             10 SCTD-FEE       PIC Z(9).99.
             10 SCTD-COST      PIC Z(4).99.
             10 SCTD-TOTAL     PIC Z(5).99.

       PROCEDURE DIVISION.
           DISPLAY "STARTING"
           PERFORM BUILD-CATALOGUE-TABLE
           PERFORM DISPLAY-CATALOGUE
           PERFORM WRITE-PROD-DB
           MOVE 41 TO WS-SEARCH

           PERFORM SEARCH-CATALOGUE
           DISPLAY "WS-RET: " WS-RET
           DISPLAY "ENDING"
           STOP RUN.

       BUILD-CATALOGUE-TABLE.
           SET PCT-IDX TO 1
           MOVE "N" TO WS-EOF

           OPEN INPUT CSV-PRODUCT-FILE.
           PERFORM UNTIL WS-EOF EQUAL "Y"
             READ CSV-PRODUCT-FILE
             AT END MOVE "Y" TO WS-EOF
             NOT AT END
               MOVE PCT-IDX TO PCT-IDXC
               MOVE PCT-IDXC TO PCT-CODE(PCT-IDX)
               UNSTRING CSV-PRODUCT-RECORD DELIMITED BY ","
               INTO
                 PCT-PRODUCT(PCT-IDX)
                 PCT-PRICE(PCT-IDX)
               SET PCT-IDX UP BY 1
             END-READ
           END-PERFORM
           CLOSE CSV-PRODUCT-FILE
       .

       DISPLAY-CATALOGUE.
           SET PCT-IDX TO 1
           PERFORM DISPLAY-CATALOGUE-HEADERS
           MOVE 0 TO WS-COLS
           PERFORM VARYING PCT-IDX 
                                   FROM 1 BY 1 
                                   UNTIL PCT-IDX GREATER PCT-IDXC
             MOVE PCT-CODE(PCT-IDX) TO PCD-CODE
             MOVE PCT-PRODUCT(PCT-IDX) TO PCD-PRODUCT
             MOVE PCT-PRICE(PCT-IDX) TO PCD-PRICE
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
           END-PERFORM
       .

       DISPLAY-CATALOGUE-HEADERS.
           MOVE 0 TO WS-COLS
           PERFORM UNTIL WS-COLS EQUAL 2
             IF WS-COLS EQUAL 0 THEN
               DISPLAY PCH-CODE WS-GAP
                       PCH-PRODUCT WS-GAP
                       PCH-PRICE WS-GAP
                       WITH NO ADVANCING
             ELSE
               DISPLAY PCH-CODE WS-GAP
                       PCH-PRODUCT WS-GAP
                       PCH-PRICE
             END-IF
             ADD 1 TO WS-COLS
           END-PERFORM
       .
       
       SEARCH-CATALOGUE.
           SET PCT-IDX TO 1
           SEARCH PRODUCT-CATALOGUE-TABLE
             AT END
               MOVE 4 TO WS-RET
             WHEN PCT-CODE(PCT-IDX) = WS-SEARCH
               DISPLAY " "
               DISPLAY "SEARCH RESULTS: "
               DISPLAY PCT-CODE(PCT-IDX) WS-GAP
                       PCT-PRODUCT(PCT-IDX) WS-GAP
                       PCT-PRICE(PCT-IDX)
               MOVE 0 TO WS-RET
           END-SEARCH
       .

       WRITE-PROD-DB.
           SET PCT-IDX TO 1
           OPEN OUTPUT PRODUCT-DB.
             
             PERFORM VARYING PCT-IDX FROM 1 BY 1 UNTIL PCT-IDX
                                                 GREATER PCT-IDXC
               
               MOVE PCT-CODE(PCT-IDX) TO PCD-CODE
               MOVE PCT-PRODUCT(PCT-IDX) TO PCD-PRODUCT
               MOVE PCT-PRICE(PCT-IDX) TO PCD-PRICE
               WRITE PRODUCT-DB-RECORD FROM 
                 PRODUCT-CATALOGUE-DISPLAY
             END-PERFORM

           CLOSE PRODUCT-DB
       .

