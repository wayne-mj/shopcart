      * COBOL rewrite of the Python code of the 3rd assessment
      * Because many of the features of modern programming languages
      * are not present in COBOL this is going to be a challenge.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     SHOP-CART.
       AUTHOR.         WAYNE JACKSON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * Define the file name and set it as a sequential file with each
      * line being beneath the previous.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "product.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * File descriptor for CSV file
       FD  CSV-FILE.
      * Each line should be no longer that 80 characters long
       01 CSV-RECORD PIC X(80).
       
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Read from CSV File 
       01 WS-EOF PIC X(1) VALUE 'N'.
      ******************************************************************
      * Gap between columns
       01 WS-GAP       PIC X(4) VALUE SPACES.
      * Counter for the Shopping Cart List
       01 WS-CART      PIC 9(4) VALUE 0.
      * Max shopping cart size
       01 WS-MAX-CART  PIC 9(4) VALUE 9999.
      * Column count for table view of catalogue   
       01 WS-COLS      PIC 9(1) VALUE 0.
       
      * Data structures for catalogue including temporary counter
       01 HOMEWARECITY-STORAGE.
      * This is the index for the array/table
      * HWC-SC-CODE: Shop Catalogue Code
           05 HWC-SC-CODE   PIC 9(2) VALUE 0.

      * This is the array/table for the catalogue.  It does not work the
      * the same as other programming languages.
           05 SHOP-CATALOGUE OCCURS 40 TIMES.
             10 SC-CODE PIC 9(2).
             10 SC-PRODUCT PIC X(35).
             10 SC-PRICE PIC 9(3)V99.
      * Display variants of the catalogue     
           05 SHOP-CAT-DISP.
             10 SCD-DISP-CODE PIC Z(4).
             10 SCD-DISP-PROD PIC X(35).
             10 SCD-DISP-PRICE PIC Z(4).99.
      * Variables for the catalogue headers   
           05 CATALOGUE-HEADERS.
             10 CH-CODE PIC X(4) VALUE "CODE".
             10 CH-PRODUCT PIC X(35) VALUE "PRODUCT NAME".
             10 CH-PRICE PIC X(7) VALUE "$ PRICE".
      * Variables for the shopping cart list header    
           05 SHOPPING-CART-HEADERS.
             10 SH-MEMBER   PIC X(10)   VALUE "MEMBERSHIP".
             10 SH-CODE     PIC X(4)    VALUE "CODE".
             10 SH-PRODUCT  PIC X(35)   VALUE "PRODUCT".
             10 SH-PRICE    PIC X(7)    VALUE "$ PRICE".
             10 SH-QUANT    PIC X(8)    VALUE "QUANTITY".
             10 SH-SHIP     PIC X(15)   VALUE "SHIPPING METHOD".
             10 SH-FEE      PIC X(12)   VALUE "SHIPPING FEE".
             10 SH-COST     PIC X(7)    VALUE "COST $".
      * Variables to display the shopping cart
           05 SHOPPING-CART-DISPLAY.
             10 SHD-MEMBER   PIC X(10).
             10 SHD-CODE     PIC Z(4).
             10 SHD-PRODUCT  PIC X(35).
             10 SHD-PRICE    PIC Z(4).99.
             10 SHD-QUANT    PIC Z(8).
             10 SHD-SHIP     PIC X(15).
             10 SHD-FEE      PIC Z(9).99.
             10 SHD-COST     PIC Z(4).99.
      * Table for the shopping cart list
           05 SHOPPING-CART-LIST OCCURS 1 TO 9999 TIMES
                DEPENDING ON WS-CART
                INDEXED BY CART-INDEX.
             10 SCL-MEMBER   PIC X(10).
             10 SCL-CODE     PIC X(4).
             10 SCL-PRODUCT  PIC X(35).
             10 SCL-PRICE    PIC X(7).
             10 SCL-QUANT    PIC X(8).
             10 SCL-SHIP     PIC X(15).
             10 SCL-FEE      PIC X(12).
             10 SCL-COST     PIC X(7).

       PROCEDURE DIVISION.
      * Build the catalogue from the CSV File
           PERFORM BUILD-CAT
           PERFORM DISPLAY-CAT
           STOP RUN.

      * Build catalogue from CSV file
       BUILD-CAT.
           OPEN INPUT CSV-FILE.
           PERFORM UNTIL WS-EOF='Y'
             READ CSV-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 ADD 1 TO HWC-SC-CODE
                 MOVE HWC-SC-CODE TO SC-CODE(HWC-SC-CODE)
                 UNSTRING CSV-RECORD
                   DELIMITED BY ','
                   INTO     
                     SC-PRODUCT(HWC-SC-CODE)
                     SC-PRICE(HWC-SC-CODE)
             END-READ
           END-PERFORM
           CLOSE CSV-FILE.
       END-BUILD-CAT.

      * Testing that the catalogue has been created and can be displayed
       TEST-DISPLAY-CAT.
      * Reset the counter to 0 to
           MOVE 0 TO HWC-SC-CODE
      * Iterate through table   
           PERFORM UNTIL HWC-SC-CODE IS EQUAL 40
             ADD 1 TO HWC-SC-CODE
             DISPLAY SC-CODE(HWC-SC-CODE) " "
                     SC-PRODUCT(HWC-SC-CODE) " "
                     SC-PRICE(HWC-SC-CODE)
           END-PERFORM.
       END-TEST-DISPLAY-CAT.

      * Display the catalogue in two alternating columns
      * This uses the display catalogue rather than the processing
      * catalogue.  This is going to get confusing rather quickly hence
      * why databases would be more suited for this type of thing
       DISPLAY-CAT.
           PERFORM DISPLAY-CAT-HEADER
           MOVE 0 TO HWC-SC-CODE
           MOVE 0 TO WS-COLS
           PERFORM UNTIL HWC-SC-CODE IS EQUAL 40
             ADD 1 TO HWC-SC-CODE
             IF WS-COLS EQUAL 0 THEN
               MOVE SC-CODE(HWC-SC-CODE) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-CODE) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-CODE) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
                       WITH NO ADVANCING
             ELSE
               MOVE SC-CODE(HWC-SC-CODE) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-CODE) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-CODE) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
             END-IF
             ADD 1 TO WS-COLS
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF
           END-PERFORM.
       END-DISPLAY-CAT.

      * This is the header for the catalogue.  It has been simplified
      * from the original as it is pretty obvious what is going on
       DISPLAY-CAT-HEADER.
           MOVE 0 TO WS-COLS
           MOVE 0 TO HWC-SC-CODE
           PERFORM UNTIL HWC-SC-CODE IS EQUAL 2
             ADD 1 TO HWC-SC-CODE
             IF WS-COLS EQUAL 0 THEN
               DISPLAY CH-CODE WS-GAP
                       CH-PRODUCT WS-GAP
                       CH-PRICE WS-GAP
                       WITH NO ADVANCING
             ELSE
               DISPLAY CH-CODE WS-GAP
                       CH-PRODUCT WS-GAP
                       CH-PRICE WS-GAP
             END-IF
             ADD 1 TO WS-COLS
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF
           END-PERFORM.
       END-DISPLAY-CAT-HEADER.
