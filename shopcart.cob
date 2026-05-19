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
       01 WS-GAP PIC X(4) VALUE SPACES.
      * Data structures for catalogue including temporary counter
       01 HOMEWARECITY-STORAGE.
      * This is the index for the array/table
           05 WS-SC-CODE   PIC 9(2) VALUE 0.
      * Column count for table view of catalogue   
           05 WS-COLS      PIC 9(1) VALUE 0.
      * This is the array/table for the catalogue.  It does not work the
      * the same as other programming languages.
           05 SHOP-CATALOGUE OCCURS 40 TIMES.
             10 SC-CODE PIC 9(2).
             10 SC-PRODUCT PIC X(35).
             10 SC-PRICE PIC 9(3)V99.
      * Display variants of the catalogue     
           05 SHOP-CAT OCCURS 40 TIMES.
             10 SCD-CODE PIC ZZZZ.
             10 SCD-PRODUCT PIC X(35).
             10 SCD-PRICE PIC ZZZZ.99.
      * Variables for the catalogue headers   
           05 CAT-HEADERS.
             10 CH-CODE PIC X(4) VALUE "CODE".
             10 CH-PRODUCT PIC X(35) VALUE "PRODUCT NAME".
             10 CH-PRICE PIC X(7) VALUE "$ PRICE".

       PROCEDURE DIVISION.
      * Build the catalogue from the CSV File
           PERFORM BUILD-CAT
           PERFORM BUILD-DISPLAY-CAT
           PERFORM DISPLAY-CAT-HEADER
           PERFORM DISPLAY-CAT
      
           STOP RUN.

      * Build catalogue from CSV file
       BUILD-CAT.
           OPEN INPUT CSV-FILE.
           PERFORM UNTIL WS-EOF='Y'
             READ CSV-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 ADD 1 TO WS-SC-CODE
                 MOVE WS-SC-CODE TO SC-CODE(WS-SC-CODE)
                 UNSTRING CSV-RECORD
                   DELIMITED BY ','
                   INTO     
                     SC-PRODUCT(WS-SC-CODE)
                     SC-PRICE(WS-SC-CODE)
             END-READ
           END-PERFORM
           CLOSE CSV-FILE.
       END-BUILD-CAT.

       BUILD-DISPLAY-CAT.
           MOVE 0 TO WS-SC-CODE
           PERFORM UNTIL WS-SC-CODE IS EQUAL 40
             ADD 1 TO WS-SC-CODE
             MOVE SC-CODE(WS-SC-CODE) TO SCD-CODE(WS-SC-CODE)
             MOVE SC-PRICE(WS-SC-CODE) TO SCD-PRICE(WS-SC-CODE)
             MOVE SC-PRODUCT(WS-SC-CODE) TO SCD-PRODUCT(WS-SC-CODE)
           END-PERFORM.
       END-BUILD-DISPLAY-CAT.

      * Testing that the catalogue has been created and can be displayed
       TEST-DISPLAY-CAT.
      * Reset the counter to 0 to
           MOVE 0 TO WS-SC-CODE
      * Iterate through table   
           PERFORM UNTIL WS-SC-CODE IS EQUAL 40
             ADD 1 TO WS-SC-CODE
             DISPLAY SC-CODE(WS-SC-CODE) " "
                     SC-PRODUCT(WS-SC-CODE) " "
                     SC-PRICE(WS-SC-CODE)
           END-PERFORM.
       END-TEST-DISPLAY-CAT.

      * Display the catalogue in two alternating columns
       DISPLAY-CAT.
           MOVE 0 TO WS-SC-CODE
           MOVE 0 TO WS-COLS
           PERFORM UNTIL WS-SC-CODE IS EQUAL 40
             ADD 1 TO WS-SC-CODE
             IF WS-COLS EQUAL 0 THEN
               DISPLAY SCD-CODE(WS-SC-CODE) WS-GAP
                       SCD-PRODUCT(WS-SC-CODE) WS-GAP
                       SCD-PRICE(WS-SC-CODE) WS-GAP 
                       WITH NO ADVANCING
             ELSE
               DISPLAY SCD-CODE(WS-SC-CODE) WS-GAP
                       SCD-PRODUCT(WS-SC-CODE) WS-GAP
                       SCD-PRICE(WS-SC-CODE) WS-GAP
             END-IF
             ADD 1 TO WS-COLS
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF
           END-PERFORM.
       END-DISPLAY-CAT.

       DISPLAY-CAT-HEADER.
           MOVE 0 TO WS-COLS
           MOVE 0 TO WS-SC-CODE
           PERFORM UNTIL WS-SC-CODE IS EQUAL 2
             ADD 1 TO WS-SC-CODE
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
