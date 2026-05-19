      * COBOL rewrite of the Python code of the 3rd assessment
      * Because many of the features of modern programming languages
      * are not present in COBOL this is going to be a challenge.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     SHOPCART.
       AUTHOR.         WAYNE JACKSON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "product.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01 CSV-RECORD PIC X(80).
       
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Read from CSV File 
       01 WS-EOF PIC X(1) VALUE 'N'.
      ******************************************************************
      * Data structures for catalogue including temporary counter
       01 HOMEWARECITY-STORAGE.
      * This is the index for the array/table
           05 WS-SC-CODE PIC 9(2) VALUE 0.
      * This is the array/table for the catalogue.  It does not work the
      * the same as other programming languages.
           05 SHOP-CATALOGUE OCCURS 40 TIMES.
             10 SC-CODE PIC 9(2).
             10 SC-PRODUCT PIC X(35).
             10 SC-PRICE PIC 9(3)V99.

       PROCEDURE DIVISION.
      * Build the catalogue from the CSV File
           PERFORM BUILD-CAT
           PERFORM TEST-DISPLAY-CAT
      
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
