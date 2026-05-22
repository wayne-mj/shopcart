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
       01 WS-COLS          PIC 9(1) VALUE 0.
      * Variable for responses for console input
       01 WS-RESP          PIC X(5).
      * Temporary variables
       01 WS-RESP-MEM      PIC 9(2).
       01 WS-RESP-CDE      PIC 9(2).
       01 WS-RESP-CDE-RNG  PIC X(1) VALUE 'N'.
       01 WS-RESP-NUM      PIC 9(2).
       
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
           
      *    PERFORM UNTIL WS-RESP-MEM EQUAL 3
      **      MOVE 0 TO WS-RESP-NUM
      **      MOVE SPACES TO SHD-MEMBER
      *      PERFORM ASK-FOR-MEMBER
      *      DISPLAY WS-RESP-MEM
      *      DISPLAY SHD-MEMBER             
      *    END-PERFORM

           PERFORM UNTIL WS-RESP-CDE-RNG EQUAL 'Y'
             DISPLAY "SEARCH ON WHAT CODE: 1-40:"
               WITH NO ADVANCING
             ACCEPT WS-RESP-CDE
             PERFORM VALIDATE-PRODUCT-CODE
           END-PERFORM
      *    MOVE 1 TO WS-RESP-CDE
      *    PERFORM SEARCH-PRODUCT-CODE
      *    DISPLAY SHD-CODE WS-GAP
      *            SHD-PRODUCT WS-GAP
      *            SHD-PRICE WS-GAP
      *
      *    MOVE 40 TO WS-RESP-CDE 
      *    PERFORM SEARCH-PRODUCT-CODE
      *    DISPLAY SHD-CODE WS-GAP
      *            SHD-PRODUCT WS-GAP
      *            SHD-PRICE WS-GAP
      *    
      *    MOVE 5 TO WS-RESP-CDE 
      *    PERFORM SEARCH-PRODUCT-CODE
      *    DISPLAY SHD-CODE WS-GAP
      *            SHD-PRODUCT WS-GAP
      *            SHD-PRICE WS-GAP
      *
      *    MOVE 50 TO WS-RESP-CDE 
      *    PERFORM SEARCH-PRODUCT-CODE
      *    DISPLAY SHD-CODE WS-GAP
      *            SHD-PRODUCT WS-GAP
      *            SHD-PRICE WS-GAP

           STOP RUN.

      **************************************************************************
      * Build catalogue from CSV file
       BUILD-CAT.
      * Open the CSV file for input
      *    MOVE 0 to HWC-SC-CODE
           OPEN INPUT CSV-FILE.
      * Until the EOF 'boolean' has been set keep reading
           PERFORM UNTIL WS-EOF='Y'
      * Read from the file, and when the EOF is hit, flag it as such
             READ CSV-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
      * This is the catalogue being built as it is read from the file
      * Increment the product code
                 ADD 1 TO HWC-SC-CODE
      * Assign the product code to the table
                 MOVE HWC-SC-CODE TO SC-CODE(HWC-SC-CODE)
      * As the values are are separated by ',' they need to be split
      * and moved into the table
                 UNSTRING CSV-RECORD
                   DELIMITED BY ','
                   INTO     
                     SC-PRODUCT(HWC-SC-CODE)
                     SC-PRICE(HWC-SC-CODE)
             END-READ
           END-PERFORM
      * Close the file
           CLOSE CSV-FILE.
       END-BUILD-CAT.

      **************************************************************************
      * Testing that the catalogue has been created and can be displayed.
       TEST-DISPLAY-CAT.
      * Reset the counter to 0 to
           MOVE 1 TO HWC-SC-CODE
      * Iterate through table   
           PERFORM UNTIL HWC-SC-CODE IS GREATER THAN 40
             
             DISPLAY SC-CODE(HWC-SC-CODE) " "
                     SC-PRODUCT(HWC-SC-CODE) " "
                     SC-PRICE(HWC-SC-CODE)
             ADD 1 TO HWC-SC-CODE
           END-PERFORM.
       END-TEST-DISPLAY-CAT.

      **************************************************************************
      * Display the catalogue in two alternating columns
      * This uses the display catalogue rather than the processing
      * catalogue.  This is going to get confusing rather quickly hence
      * why databases would be more suited for this type of thing.
       DISPLAY-CAT.
      * Display the header for the catalogue
           PERFORM DISPLAY-CAT-HEADER
      * Set the index for 0
           MOVE 1 TO HWC-SC-CODE
      * Set the columns to 0
           MOVE 0 TO WS-COLS
      * Loop through the table
           PERFORM UNTIL HWC-SC-CODE IS GREATER THAN 40

      * If the columns is the first
             IF WS-COLS EQUAL 0 THEN
      * Move the table to the displayable variables and then display
      * without a newline
               MOVE SC-CODE(HWC-SC-CODE) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-CODE) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-CODE) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
                       WITH NO ADVANCING
      * Otherwise just move the displayable variables and display them
             ELSE
               MOVE SC-CODE(HWC-SC-CODE) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-CODE) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-CODE) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
             END-IF
      * Increment the column count
             ADD 1 TO WS-COLS
      * Once the column count is 2 reset
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF
      * Increment the index counter
             ADD 1 TO HWC-SC-CODE
           END-PERFORM.
       END-DISPLAY-CAT.
      
      **************************************************************************
      * This is the header for the catalogue.  It has been simplified
      * from the original as it is pretty obvious what is going on.
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

      **************************************************************************
      * Query if the customer is a member or not, or to terminate the 
      * data entry process.
       ASK-FOR-MEMBER.
      * Ensure that the response is zero'ed out
           MOVE 0 to WS-RESP-MEM
      * Clear out the member status
           MOVE SPACES TO SHD-MEMBER
      * Loop until the correct data is entered
           PERFORM UNTIL WS-RESP-MEM GREATER 0 AND LESS 4
             DISPLAY "************************************"
             DISPLAY "IS THE CUSTOMER A MEMBER? (1/2/3): "
             DISPLAY "1.) YES" WS-GAP "2.) NO" WS-GAP "3.) END" 
                     WS-GAP ">: "
               WITH NO ADVANCING
      * Get the response and convert it to a number if possible
             ACCEPT WS-RESP
             COMPUTE WS-RESP-MEM = FUNCTION NUMVAL(WS-RESP)
      * Perform a test of the number response: 1, 2 or 3 and assign
      * accordingly or if necessary display an error message
             IF WS-RESP-MEM EQUAL 1 THEN
               MOVE "YES" TO SHD-MEMBER
             END-IF
             IF WS-RESP-MEM EQUAL 2 THEN
                 MOVE "NO" TO SHD-MEMBER
             END-IF
             IF WS-RESP-MEM GREATER 3 OR WS-RESP-MEM LESS 1 THEN
                 DISPLAY "INVALID OPTION"
             END-IF
           END-PERFORM.
       END-ASK-FOR-MEMBER.
      
      **************************************************************************
      * Poor man's search of the catalogue
       SEARCH-PRODUCT-CODE.
      * Initialise the variables to a known state before using or returning
           MOVE 1 TO HWC-SC-CODE
           MOVE 0 TO SHD-CODE
           MOVE "No such product" TO SHD-PRODUCT
           MOVE 0 TO SHD-PRICE

      * Ironically, everything starts at 1, so we need to count until after 40
      * exiting the loop when there is a match else increment
           PERFORM UNTIL HWC-SC-CODE GREATER THAN 40
               IF SC-CODE(HWC-SC-CODE) EQUAL WS-RESP-CDE THEN
                 MOVE SC-CODE(HWC-SC-CODE) TO SHD-CODE
                 MOVE SC-PRODUCT(HWC-SC-CODE) TO SHD-PRODUCT
                 MOVE SC-PRICE(HWC-SC-CODE) TO SHD-PRICE
                 EXIT PERFORM
               ELSE
                 ADD 1 TO HWC-SC-CODE
               END-IF               
           END-PERFORM.           
       END-SEARCH-PRODUCT-CODE.

      **************************************************************************

       VALIDATE-PRODUCT-CODE.
           IF WS-RESP-CDE GREATER 0 AND WS-RESP-CDE LESS 41 THEN
             MOVE 'Y' TO WS-RESP-CDE-RNG
           ELSE
             MOVE 'N' TO WS-RESP-CDE-RNG
           END-IF.            
       END-VALIDATE-PRODUCT-CODE.

      **************************************************************************

