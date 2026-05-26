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

           SELECT CSV-FILE2 ASSIGN TO "shopcart.csv"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * File descriptor for CSV file
       FD  CSV-FILE.
      * Each line should be no longer that 80 characters long
       01 CSV-RECORD PIC X(80).
       
      * File descriptor for order CSV file
       FD  CSV-FILE2.
      * Each line should be no longer that 80 characters long
       01  CSV-RECORD2 PIC X(80).

       WORKING-STORAGE SECTION.
      ******************************************************************
      * Read from CSV File 
       01 WS-EOF PIC X(1) VALUE 'N'.
      ******************************************************************
      * Gap between columns
       01 WS-GAP           PIC X(4) VALUE SPACES.
      * Counter for the Shopping Cart List
       01 WS-CART          PIC 9(4) VALUE 0.
      * Max shopping cart size
       01 WS-CART-COUNT    PIC 9(4) VALUE 0.
      * Column count for table view of catalogue   
       01 WS-COLS          PIC 9(1) VALUE 0.
       01 WS-COLS-INDEX    PIC 9(1) VALUE 0.
      * Variable for the total of the shopping cart
       01 WS-CART-TOTAL    PIC 9(5)v99 VALUE 0.
       01 WS-CART-TOTFT    PIC Z(5).99.
      * Variable for responses for console input
       01 WS-RESP          PIC X(5).
      * Temporary variables
       01 WS-RESP-MEM      PIC 9(2).
       01 WS-RESP-CDE      PIC 9(2).
       01 WS-RESP-QNT      PIC 9(2).
       01 WS-RESP-DEL      PIC 9(2).
       01 WS-RESP-QNT-RNG  PIC X(1) VALUE 'N'.
       01 WS-RESP-CDE-RNG  PIC X(1) VALUE 'N'.
       01 WS-RESP-DEL-RNG  PIC X(1) VALUE 'N'.
       01 WS-RESP-NUM      PIC 9(2).
       01 WS-SHIP-FEE      PIC 9(2)v99.
       01 WS-COST          PIC 9(5)v99.
       01 WS-PRICE         PIC 9(5)v99.
       
      * Data structures for catalogue including temporary counter
       01 HOMEWARECITY-STORAGE.
      * This is the index for the array/table
      * HWC-SC-CODE: Shop Catalogue Code
           05 HWC-SC-CODE   PIC 9(2) VALUE 1.
           05 HWC-SC-INDEX  PIC 9(2) VALUE 1.
      * This is the array/table for the catalogue.  It does not work the
      * the same as other programming languages.
           05 SHOP-CATALOGUE OCCURS 40 TIMES.
             10 SC-CODE PIC 9(2).
             10 SC-PRODUCT PIC X(35).
             10 SC-PRICE PIC 9(5)V99.
      * Display variants of the catalogue     
           05 SHOP-CAT-DISP.
             10 SCD-DISP-CODE PIC Z(4).
             10 SCD-DISP-PROD PIC X(35).
             10 SCD-DISP-PRICE PIC Z(5).99.
      * Variables for the catalogue headers   
           05 CATALOGUE-HEADERS.
             10 CH-CODE     PIC X(4)    VALUE "CODE".
             10 CH-PRODUCT  PIC X(35)   VALUE "PRODUCT NAME".
             10 CH-PRICE    PIC X(7)    VALUE "$ PRICE".
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
             10 SHD-PRICE    PIC Z(5).99.
             10 SHD-QUANT    PIC Z(8).
             10 SHD-SHIP     PIC X(15).
             10 SHD-FEE      PIC Z(9).99.
             10 SHD-COST     PIC Z(5).99.
      * Table for the shopping cart list
           05 SHOPPING-CART-LIST OCCURS 1 TO 9999 TIMES
                DEPENDING ON WS-CART
                INDEXED BY CART-INDEX.
             10 SCL-MEMBER   PIC X(3).
             10 SCL-CODE     PIC 9(4).
             10 SCL-PRODUCT  PIC X(35).
             10 SCL-PRICE    PIC 9(5)v99.
             10 SCL-QUANT    PIC 9(2).
             10 SCL-SHIP     PIC X(15).
             10 SCL-FEE      PIC 9(5)v99.
             10 SCL-COST     PIC 9(5)v99.

       PROCEDURE DIVISION.
      * Build the catalogue from the CSV File
           PERFORM BUILD-CAT
           PERFORM BUILD-CART-CSV
      ** Start building the shopping cart by asking if the customer is a member 
      ** or not, or if terminate input.
      *    PERFORM UNTIL WS-RESP-MEM EQUAL 3
      ** Reset to defaults
      *      MOVE 0 TO WS-RESP-NUM
      *      MOVE SPACES TO SHD-MEMBER
      *      PERFORM ASK-FOR-MEMBER
      ** If this is not the end, then build the cart
      *      IF WS-RESP-MEM NOT EQUAL 3 THEN
      *        PERFORM BUILD-SHOPPING-CART
      *      END-IF
      *    END-PERFORM
           
           PERFORM DISPLAY-SHOPPING-CART
           PERFORM CALC-SHOP-CART-TOTAL
           MOVE WS-CART-TOTAL TO WS-CART-TOTFT
           DISPLAY " "
           DISPLAY "TOTAL: $" WS-CART-TOTFT

           STOP RUN.

      **************************************************************************
      *
      *    Build catalogue from CSV file, looping through each line to generate 
      *    the catalogue using the index to generate the product code
      *
      **************************************************************************
       BUILD-CAT.
           OPEN INPUT CSV-FILE.
           PERFORM UNTIL WS-EOF='Y'
             READ CSV-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 MOVE HWC-SC-INDEX TO HWC-SC-CODE
                 MOVE HWC-SC-CODE TO SC-CODE(HWC-SC-INDEX)
                 UNSTRING CSV-RECORD
                   DELIMITED BY ','
                   INTO     
                     SC-PRODUCT(HWC-SC-INDEX)
                     SC-PRICE(HWC-SC-INDEX)
                 ADD 1 TO HWC-SC-INDEX
             END-READ
           END-PERFORM
           CLOSE CSV-FILE
           MOVE 'N' TO WS-EOF.

      **************************************************************************
       BUILD-CART-CSV.
           OPEN INPUT CSV-FILE2.

           PERFORM UNTIL WS-EOF EQUAL 'Y'

             READ CSV-FILE2
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END

                 UNSTRING CSV-RECORD2 
                   DELIMITED BY ','
                   INTO 
                     WS-RESP-MEM
                     WS-RESP-CDE
                     WS-RESP-QNT
                     WS-RESP-DEL
                 PERFORM EVALUATE-MEMBER
                 PERFORM EVALUATE-PRODUCT-CODE
                 PERFORM EVALUATE-QUANTITY
                 PERFORM EVALUATE-DELIVERY-METHOD
                 
                 PERFORM CALC-SHIPPING-FEE
                 MOVE WS-SHIP-FEE TO SHD-FEE
                 PERFORM CALC-PRODUCT-COST
                 MOVE WS-COST TO SHD-COST
                 PERFORM BUILD-SHOPPING-CART-TABLE
             END-READ
           END-PERFORM
           CLOSE CSV-FILE2
           MOVE 'N' TO WS-EOF.

      **************************************************************************
      *
      *    Testing that the catalogue has been created and can be displayed.
      *
      **************************************************************************
       TEST-DISPLAY-CAT.
           MOVE 1 TO HWC-SC-INDEX

           PERFORM UNTIL HWC-SC-INDEX IS GREATER THAN 40
             
             DISPLAY SC-CODE(HWC-SC-INDEX) " "
                     SC-PRODUCT(HWC-SC-INDEX) " "
                     SC-PRICE(HWC-SC-INDEX)
             ADD 1 TO HWC-SC-INDEX
           END-PERFORM.
       
      **************************************************************************
      *
      *    Display the catalogue in two alternating columns
      *    This uses the display catalogue rather than the processing
      *    catalogue.  This is going to get confusing rather quickly hence
      *    why databases would be more suited for this type of thing.
      *
      **************************************************************************

       DISPLAY-CAT.
           PERFORM DISPLAY-CAT-HEADER
           MOVE 1 TO HWC-SC-INDEX
           MOVE 0 TO WS-COLS
           PERFORM UNTIL HWC-SC-INDEX IS GREATER THAN 40

             IF WS-COLS EQUAL 0 THEN
               MOVE SC-CODE(HWC-SC-INDEX) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-INDEX) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-INDEX) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
                       WITH NO ADVANCING
             ELSE
               MOVE SC-CODE(HWC-SC-INDEX) TO SCD-DISP-CODE
               MOVE SC-PRODUCT(HWC-SC-INDEX) TO SCD-DISP-PROD
               MOVE SC-PRICE(HWC-SC-INDEX) TO SCD-DISP-PRICE
               DISPLAY SCD-DISP-CODE WS-GAP
                       SCD-DISP-PROD WS-GAP
                       SCD-DISP-PRICE WS-GAP
             END-IF

             ADD 1 TO WS-COLS
             IF WS-COLS EQUAL 2 THEN
               MOVE 0 TO WS-COLS
             END-IF
             ADD 1 TO HWC-SC-INDEX
           END-PERFORM.
             
      **************************************************************************
      *
      * This is the header for the catalogue.  It has been simplified
      * from the original as it is pretty obvious what is going on.
      *
      **************************************************************************
       DISPLAY-CAT-HEADER.
           MOVE 0 TO WS-COLS
           MOVE 0 TO WS-COLS-INDEX
           PERFORM UNTIL WS-COLS-INDEX IS EQUAL 2
      *      ADD 1 TO WS-COLS-INDEX
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
             ADD 1 TO WS-COLS-INDEX
           END-PERFORM.
       
      **************************************************************************
      *
      * Query if the customer is a member or not, or to terminate the 
      * data entry process.
      *
      **************************************************************************
       ASK-FOR-MEMBER.
           MOVE 0 to WS-RESP-MEM
           MOVE SPACES TO SHD-MEMBER

           PERFORM UNTIL WS-RESP-MEM GREATER 0 AND LESS 4
             DISPLAY "************************************"
             DISPLAY "IS THE CUSTOMER A MEMBER? (1/2/3): "
             DISPLAY "1.) YES" WS-GAP "2.) NO" WS-GAP "3.) END" 
                     WS-GAP ">: "
               WITH NO ADVANCING

             ACCEPT WS-RESP
             COMPUTE WS-RESP-MEM = FUNCTION NUMVAL(WS-RESP)
             PERFORM EVALUATE-MEMBER
      ** Perform a test of the number response: 1, 2 or 3 and assign
      ** accordingly or if necessary display an error message
      *      IF WS-RESP-MEM EQUAL 1 THEN
      *        MOVE "YES" TO SHD-MEMBER
      *      END-IF
      *      IF WS-RESP-MEM EQUAL 2 THEN
      *          MOVE "NO" TO SHD-MEMBER
      *      END-IF
      *      IF WS-RESP-MEM GREATER 3 OR WS-RESP-MEM LESS 1 THEN
      *          DISPLAY "INVALID OPTION"
      *      END-IF
           END-PERFORM.

      **************************************************************************
      *
      * Evaluate if the customer is a member or not, or if to terminate input
      * Handle the response values.
      *
      **************************************************************************
       EVALUATE-MEMBER.
           IF WS-RESP-MEM EQUAL 1 THEN
             MOVE "YES" TO SHD-MEMBER
           END-IF
           IF WS-RESP-MEM EQUAL 2 THEN
             MOVE "NO" TO SHD-MEMBER
           END-IF
           IF WS-RESP-MEM GREATER 3 OR WS-RESP-MEM LESS 1 THEN
             DISPLAY "INVALID MEMBER OPTION"
           END-IF.
      
      **************************************************************************
      *
      * Ask the user for the product code
      *
      **************************************************************************
       ASK-FOR-PRODUCT-CODE.
           MOVE 'N' TO WS-RESP-CDE-RNG
           PERFORM DISPLAY-CAT
      
           PERFORM UNTIL WS-RESP-CDE-RNG EQUAL 'Y'
             DISPLAY " "
             DISPLAY "ENTER A PRODUCT CODE (1-40): "
               WITH NO ADVANCING
             ACCEPT WS-RESP
             COMPUTE WS-RESP-CDE = FUNCTION NUMVAL(WS-RESP)
             PERFORM EVALUATE-PRODUCT-CODE
      *      PERFORM VALIDATE-PRODUCT-CODE
      *      IF WS-RESP-CDE-RNG EQUAL 'Y' THEN
      *        PERFORM SEARCH-PRODUCT-CODE
      *      END-IF
           END-PERFORM.
      
      **************************************************************************
      *
      * Evaluate the product code and if valid execute the search
      *
      **************************************************************************
       EVALUATE-PRODUCT-CODE.
           PERFORM VALIDATE-PRODUCT-CODE
           IF WS-RESP-CDE-RNG EQUAL 'Y' THEN
             PERFORM SEARCH-PRODUCT-CODE
           END-IF.
      
      **************************************************************************
      *
      * Validate the product code range is between inclusively 1 and 40
      * and set the 'boolean' appropriately.
      *
      **************************************************************************
       VALIDATE-PRODUCT-CODE.
           IF WS-RESP-CDE GREATER 0 AND WS-RESP-CDE LESS 41 THEN
             MOVE 'Y' TO WS-RESP-CDE-RNG
           ELSE
             MOVE 'N' TO WS-RESP-CDE-RNG
           END-IF.  

      **************************************************************************
      *
      * Poor man's search of the catalogue
      *
      **************************************************************************
       SEARCH-PRODUCT-CODE.
           MOVE 1 TO HWC-SC-INDEX
           MOVE 0 TO SHD-CODE
           MOVE "No such product" TO SHD-PRODUCT
           MOVE 0 TO SHD-PRICE

           PERFORM UNTIL HWC-SC-INDEX GREATER THAN 40
               IF SC-CODE(HWC-SC-INDEX) EQUAL WS-RESP-CDE THEN
                 MOVE SC-CODE(HWC-SC-INDEX) TO SHD-CODE
                 MOVE SC-PRODUCT(HWC-SC-INDEX) TO SHD-PRODUCT
                 MOVE SC-PRICE(HWC-SC-INDEX) TO WS-PRICE
                 MOVE WS-PRICE TO SHD-PRICE
                 EXIT PERFORM
               ELSE
                 ADD 1 TO HWC-SC-INDEX
               END-IF               
           END-PERFORM.
      
      **************************************************************************
      *
      * Ask the user for the quantity of the product
      *
      **************************************************************************
       ASK-FOR-QUANTITY.
           MOVE 'N' TO WS-RESP-QNT-RNG
           PERFORM UNTIL WS-RESP-QNT-RNG EQUAL 'Y'
             DISPLAY " "
             DISPLAY "ENTER THE QUANTITY OF PRODUCT (1-29): "
               WITH NO ADVANCING
             ACCEPT WS-RESP
             COMPUTE WS-RESP-QNT = FUNCTION NUMVAL(WS-RESP)
             PERFORM EVALUATE-QUANTITY
      *      PERFORM VALIDATE-QUANTITY
      *      MOVE WS-RESP-QNT TO SHD-QUANT
           END-PERFORM.
      
      **************************************************************************
      *
      *    Evaluate and validate the quantity
      *
      **************************************************************************
       EVALUATE-QUANTITY.
           PERFORM VALIDATE-QUANTITY
           MOVE WS-RESP-QNT TO SHD-QUANT.
      
      **************************************************************************
      *    Validate that the quantity chosen is between the inclusive ranges
      *    1 and 29 and return the appropriate 'boolean' response.
      *
      **************************************************************************
       VALIDATE-QUANTITY.
           IF WS-RESP-QNT GREATER 0 AND WS-RESP-QNT LESS 30 THEN
             MOVE 'Y' TO WS-RESP-QNT-RNG
           ELSE
             MOVE 'N' TO WS-RESP-QNT-RNG
           END-IF.           
      
      **************************************************************************
      *
      *    Ask for the delivery method
      *
      **************************************************************************
       ASK-FOR-DELIVERY-METHOD.
           MOVE 'N' TO WS-RESP-DEL-RNG
           PERFORM UNTIL WS-RESP-DEL-RNG EQUAL 'Y'
             DISPLAY " "
             DISPLAY "ENTER THE DELIVERY METHOD"
             DISPLAY "1.) DELIVERY 2). PICK-UP (1-2): "
               WITH NO ADVANCING
             ACCEPT WS-RESP
             COMPUTE WS-RESP-DEL = FUNCTION NUMVAL(WS-RESP)
             PERFORM EVALUATE-DELIVERY-METHOD
      *      PERFORM VALIDATE-DELIVERY-METHOD
      *      IF WS-RESP-DEL-RNG EQUAL 'Y' THEN
      *        IF WS-RESP-DEL EQUAL 1 THEN
      *          MOVE "DELIVERY" TO SHD-SHIP
      *        ELSE
      *          MOVE "PICK-UP" TO SHD-SHIP
      *        END-IF
      *      END-IF
           END-PERFORM.
      
      **************************************************************************
      *
      *    Evaluate and validate the delivery method
      *
      **************************************************************************
       EVALUATE-DELIVERY-METHOD.
           PERFORM VALIDATE-DELIVERY-METHOD
             IF WS-RESP-DEL-RNG EQUAL 'Y' THEN
               IF WS-RESP-DEL EQUAL 1 THEN
                 MOVE "DELIVERY" TO SHD-SHIP
               ELSE
                 MOVE "PICK-UP" TO SHD-SHIP
               END-IF
             END-IF.

      **************************************************************************
      *
      *    Validate the delivery methods is between 1 and 2 inclusively and
      *    set the 'boolean' appropriately.
      *
      **************************************************************************
       VALIDATE-DELIVERY-METHOD.
           IF WS-RESP-DEL GREATER 0 AND WS-RESP-DEL LESS 3 THEN
             MOVE 'Y' TO WS-RESP-DEL-RNG
           ELSE
             MOVE 'N' TO WS-RESP-DEL-RNG
           END-IF.
           
      **************************************************************************
      *
      *    Calculate the shipping fee
      *
      **************************************************************************
       CALC-SHIPPING-FEE.
           MOVE 0 TO WS-SHIP-FEE
           IF WS-RESP-DEL EQUAL 1 THEN
             IF WS-RESP-QNT GREATER 1 THEN
               COMPUTE WS-SHIP-FEE = 2.00 + ((WS-RESP-QNT - 1 ) * 1.6)
             ELSE
               MOVE 2.00 TO WS-SHIP-FEE
             END-IF
           END-IF.
      
      **************************************************************************
      *
      *    Calculate the total cost of the product
      *
      **************************************************************************
       CALC-PRODUCT-COST.
           MOVE 0 TO WS-COST
           COMPUTE WS-COST = (WS-RESP-QNT * WS-PRICE) + WS-SHIP-FEE

           IF SHD-MEMBER = 'YES' THEN
             COMPUTE WS-COST = WS-COST * (90/100)
           END-IF.

      **************************************************************************
      *      
      *    Build the shopping cart based ont he input from the user
      *
      **************************************************************************
       BUILD-SHOPPING-CART.
      *    MOVE SPACES TO SHD-MEMBER
           MOVE 0 TO SHD-CODE
           MOVE SPACES TO SHD-PRODUCT
           MOVE 0 TO SHD-PRICE
           MOVE 0 TO SHD-QUANT
      *    MOVE SPACES TO SHD-SHIP
           MOVE 0 TO SHD-FEE
           MOVE 0 TO SHD-COST

           PERFORM ASK-FOR-PRODUCT-CODE
           PERFORM ASK-FOR-QUANTITY           
           PERFORM ASK-FOR-DELIVERY-METHOD
           PERFORM CALC-SHIPPING-FEE
           MOVE WS-SHIP-FEE TO SHD-FEE
           PERFORM CALC-PRODUCT-COST
           MOVE WS-COST TO SHD-COST
           
           PERFORM BUILD-SHOPPING-CART-TABLE.
      **************************************************************************
      *
      *    Build the shopping cart table
      *
      **************************************************************************
       BUILD-SHOPPING-CART-TABLE.
           MOVE FUNCTION TRIM(SHD-MEMBER) TO SCL-MEMBER(CART-INDEX)
           MOVE SHD-CODE TO SCL-CODE(CART-INDEX)
           MOVE FUNCTION TRIM(SHD-PRODUCT) TO SCL-PRODUCT(CART-INDEX)
           MOVE SHD-PRICE TO SCL-PRICE(CART-INDEX)
           MOVE SHD-QUANT TO SCL-QUANT(CART-INDEX)
           MOVE SHD-SHIP TO SCL-SHIP(CART-INDEX)
           MOVE SHD-FEE TO SCL-FEE(CART-INDEX)
           MOVE SHD-COST TO SCL-COST(CART-INDEX)
           
           ADD 1 TO CART-INDEX
           MOVE CART-INDEX TO WS-CART-COUNT
      *    DISPLAY SHD-MEMBER WS-GAP
      *            SHD-CODE WS-GAP
      *            SHD-PRODUCT WS-GAP
      *            SHD-PRICE WS-GAP
      *            SHD-QUANT WS-GAP
      *            SHD-SHIP WS-GAP
      *            SHD-FEE WS-GAP
      *            SHD-COST WS-GAP

           IF WS-CART-COUNT EQUAL 9000 THEN
             DISPLAY "RECORD COUNT APPROACHING MAXIMUM: " WS-CART-COUNT
                     " OF 9999."
           END-IF.
      
      **************************************************************************
      *
      *    Display the shopping cart after it has been put together
      *
      **************************************************************************
       DISPLAY-SHOPPING-CART.
           MOVE 1 TO CART-INDEX
      *    MOVE SPACES TO SHD-MEMBER
           MOVE 0 TO SHD-CODE
           MOVE SPACES TO SHD-PRODUCT
           MOVE 0 TO SHD-PRICE
           MOVE 0 TO SHD-QUANT
      *    MOVE SPACES TO SHD-SHIP
           MOVE 0 TO SHD-FEE
           MOVE 0 TO SHD-COST
      
           PERFORM UNTIL CART-INDEX EQUAL WS-CART-COUNT
             MOVE SCL-MEMBER(CART-INDEX)   TO SHD-MEMBER
             MOVE SCL-CODE(CART-INDEX)     TO SHD-CODE
             MOVE SCL-PRODUCT(CART-INDEX)  TO SHD-PRODUCT
             MOVE SCL-PRICE(CART-INDEX)    TO SHD-PRICE
             MOVE SCL-QUANT(CART-INDEX)    TO SHD-QUANT
             MOVE SCL-SHIP(CART-INDEX)     TO SHD-SHIP
             MOVE SCL-FEE(CART-INDEX)      TO SHD-FEE
             MOVE SCL-COST(CART-INDEX)     TO SHD-COST
      
             DISPLAY SHD-MEMBER WS-GAP
                     SHD-CODE WS-GAP
                     SHD-PRODUCT WS-GAP
                     SHD-PRICE WS-GAP
                     SHD-QUANT WS-GAP
                     SHD-SHIP WS-GAP
                     SHD-FEE WS-GAP
                     SHD-COST WS-GAP

             ADD 1 to CART-INDEX
           END-PERFORM.

      **************************************************************************
      *
      *    Calculate the total cost of the cart at the end of the transaction
      *
      **************************************************************************
       CALC-SHOP-CART-TOTAL.
           MOVE 1 TO CART-INDEX
           MOVE 0 TO WS-CART-TOTAL

           PERFORM UNTIL CART-INDEX EQUAL WS-CART-COUNT
      *      MOVE SCL-COST(CART-INDEX) TO WS-COST
             COMPUTE WS-CART-TOTAL = WS-CART-TOTAL + 
                     SCL-COST(CART-INDEX)
      *      DISPLAY SCL-COST(CART-INDEX)
             ADD 1 TO CART-INDEX
           END-PERFORM
           .
