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

           SELECT SHOPCART-REPORT-FILE
             ASSIGN TO "shopcart.dat"
             ORGANIZATION IS LINE SEQUENTIAL.

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

       FD  SHOPCART-REPORT-FILE.
       01  SHOPCART-HEADERS.
           05 SCH-MEMBER    PIC X(10).
           05 FILLER        PIC X(4).
           05 SCH-CODE      PIC X(4).
           05 FILLER        PIC X(4).
           05 SCH-PRODUCT   PIC X(35).
           05 FILLER        PIC X(4).
           05 SCH-PRICE     PIC X(8).
           05 FILLER        PIC X(4).
           05 SCH-QUANTITY  PIC X(8).
           05 FILLER        PIC X(4).
           05 SCH-METHOD    PIC X(15).
           05 FILLER        PIC X(4).
           05 SCH-FEE       PIC X(12).
           05 FILLER         PIC X(4).
           05 SCH-COST      PIC X(8).

       01  SHOPCART-REPORT.
           05 SCR-MEMBER    PIC X(10).
           05 FILLER        PIC X(4).
           05 SCR-CODE      PIC Z(4).
           05 FILLER        PIC X(4).
           05 SCR-PRODUCT   PIC X(35).
           05 FILLER        PIC X(4).
           05 SCR-PRICE     PIC Z(5).99.
           05 FILLER        PIC X(4).
           05 SCR-QUANTITY  PIC Z(8).
           05 FILLER        PIC X(4).
           05 SCR-METHOD    PIC X(15).
           05 FILLER        PIC X(4).
           05 SCR-FEE       PIC Z(9).99.
           05 FILLER        PIC X(4).
           05 SCR-COST      PIC Z(5).99.
           05 FILLER        PIC X(4).

       01  SHOPCART-REPORT-TOTAL.
           05 FILLER         PIC X(8) VALUE "TOTAL $ ".
           05 SCR-TOTAL     PIC Z(5).99.
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
           
      *    Gap between headers and columns
       01  WS-GAP      PIC X(4) VALUE SPACES.
      *    Debug gap to check if the gap is looking right
      *01  WS-GAP      PIC X(4) VALUE "....".
      *    Variable to track the columns
       01  WS-COLS     PIC 9.
      *    Max table depth
       01  WS-MAX      PIC 9(4) VALUE 9999.
      *    Underline dashes set to 40 characters long
       01  WS-DASH     PIC X(40) VALUE 
           "----------------------------------------".
      *    Variables for Bubble sort
       01  I           PIC 9(4) VALUE 0.
       01  J           PIC 9(4) VALUE 0.

      *    So I do not have to type over and over and over again
       01  WS-DELIVERY-METHODS.
           05 WS-DEL   PIC X(8) VALUE "DELIVERY".
           05 WS-PU    PIC X(7) VALUE "PICK-UP".

      *    *************************************************************
      *    
      *    Required variables
      *
      *    *************************************************************
       01  WS-REQUIRED-VARIABLES.
      *    Used to track what line errors occurred in the source cart
           05 WS-CART-LINE      PIC 9(7)    VALUE 0.
      *    Boolean variable to track if to display an error message
           05 WS-DISP-ERR       PIC X(1)    VALUE "N".
      *    Buffer for the error message
           05 WS-DISP-MSG       PIC X(15)   VALUE SPACES.
      *    Boolean variable to determine if the response is OK or not
           05 WS-RESP-OK        PIC X(1)    VALUE 'N'.
      *    Boolean to determine if silent operation
           05 WS-SILENT         PIC X(1)    VALUE 'N'.
      *    Member YES/NO/END
           05 WS-MEMBER-RESP    PIC X(3)    VALUE SPACES.
      *    Product Code as string
           05 WS-PRODUCT-RESP   PIC X(2)    VALUE SPACES.
      *    Product code as number
           05 WS-PRODUCT-NUM    PIC 9(2)    VALUE 0.
      *    Formatted product code
           05 WS-PRODUCT-CODE   PIC 9(4)    VALUE 0.
      *    Product description
           05 WS-PRODUCT-DESC   PIC X(35)   VALUE SPACES.
      *    Formatted product price
           05 WS-PRODUCT-PRICE  PIC 9(5)V99 VALUE 0.
      *    Quantity as string
           05 WS-QUANT-RESP     PIC X(2)    VALUE SPACES.
      *    Quantity as number
           05 WS-QUANT-NUM      PIC 9(2)    VALUE 0.
      *    DELIVERY/PICK-UP
           05 WS-DELIVERY       PIC X(15)   VALUE SPACES.
      *    Numerical indicator for DELIVERY/PICK-UP 1 OR 2
           05 WS-DELIVERY-NUM   PIC 9       VALUE 0.
      *    Calculated shipping fee formatted
           05 WS-SHIP-FEE       PIC 9(5)V99 VALUE 0.
      *    Calculated cost formatted
           05 WS-COST           PIC 9(5)V99 VALUE 0.
      *    Calculated total cost formatted
           05 WS-TOTAL-COST     PIC 9(5)V99 VALUE 0.
      *    Used for the report generation to indicate if a record has 
      *    been found
           05 WS-FOUND          PIC X(1)    VALUE 'N'.
      *    Calculated quantity
           05 WS-REPORT-Q       PIC 9(4).
      *    Calculated cost
           05 WS-REPORT-C       PIC 9(5)V99.
      *    Total errors detected
           05 WS-ERRORS         PIC 9(4)    VALUE 0.
      *    Query if the program is to run interactively or note
           05 WS-INTERACT-RESP  PIC X(1)    VALUE SPACE.
             

      *    *************************************************************
      *
      *    Homeware City Storage variables
      *
      *    *************************************************************
       01  HOMEWARECITY-STORAGE.
      *    Manual indexing of the product table
           05 HWC-IDXC   PIC 9(4).
      *    Manual calculation of the product code
           05 HWC-CODE   PIC 9(2).
      *    Shopping cart table index manual method
           05 SCT-INDEX  PIC 9(10).
      *    Count of the table for the shopping cart
           05 SCT-COUNT  PIC 9(10).
      *    Index counter for Shopping Cart Table
           05 SCT-IDXC   PIC 9(4).
      *    Index counter for Shopping Cart Report
           05 SCR-IDXC   PIC 9(4).
      *    Total for the Shopping Cart Table to be displayed
      *    05 SCTD-TOTAL     PIC Z(5).99.

      *    *************************************************************
      *
      *    Data structures for the tables
      *
      *    *************************************************************
           
           05 ERROR-LOG OCCURS 9999 TIMES INDEXED BY ERR-IDX.
             10 EL-MESSAGE          PIC X(80).
             10 EL-LINE             PIC 9(7).
      *    *************************************************************
      *
      *    Product Catalogue Table Data structures
      *
      *    *************************************************************

      *    Structure for Product catalogue table
           05 PRODUCT-CATALOGUE-TABLE OCCURS 40 TIMES
                                      INDEXED BY HWC-IDX.
             10 PCT-CODE            PIC 9(4).
             10 PCT-PRODUCT         PIC X(35).
             10 PCT-PRICE           PIC 9(5)V99.
           
      *    Structure for displaying the data structure
           05 PRODUCT-CATALOGUE-DISPLAY.
             10 PCD-CODE            PIC Z(4).
             10 PCD-PRODUCT         PIC X(35).
             10 PCD-PRICE           PIC Z(5).99.
           
      *    Structure for the headers
           05 PRODUCT-CATALOGUE-HEADERS.
             10 PCH-CODE    PIC X(4)    VALUE "CODE".
             10 PCH-PROD    PIC X(35)   VALUE "PRODUCT NAME".
             10 PCH-PRICE   PIC X(8)    VALUE "$  PRICE".
             10 PCH-CODE-U-LINE  PIC X(4).
             10 PCH-PROD-U-LINE  PIC X(35).
             10 PCH-PRICE-U-LINE PIC X(8).
           
      *    *************************************************************
      *
      *    Shopping Cart Table Data structures
      *
      *    *************************************************************
           
      *    Structure of table shopping cart
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

      *    Structure for headers for shopping cart
           05 SHOPPING-CART-TABLE-HEADERS.
             10 SCTH-MEMBER    PIC X(10)    VALUE "MEMBER".
             10 FILLER         PIC X(4).
             10 SCTH-CODE      PIC X(4)     VALUE "CODE".
             10 FILLER         PIC X(4).
             10 SCTH-PRODUCT   PIC X(35)    VALUE "PRODUCT".
             10 FILLER         PIC X(4).
             10 SCTH-PRICE     PIC X(8)     VALUE "$  PRICE".
             10 FILLER         PIC X(4).
             10 SCTH-QUANTITY  PIC X(8)     VALUE "QUANTITY".
             10 FILLER         PIC X(4).
             10 SCTH-METHOD    PIC X(15)    VALUE "SHIPPING METHOD".
             10 FILLER         PIC X(4).
             10 SCTH-FEE       PIC X(12)    VALUE "SHIPPING FEE".
             10 FILLER         PIC X(4).
             10 SCTH-COST      PIC X(8)     VALUE "$   COST".
           
           05 SHOPPING-CART-TABLE-U-LINE.
             10 SCTH-MEMBER-U    PIC X(10).
             10 FILLER           PIC X(4).
             10 SCTH-CODE-U      PIC X(4).
             10 FILLER           PIC X(4).
             10 SCTH-PRODUCT-U   PIC X(35).
             10 FILLER           PIC X(4).
             10 SCTH-PRICE-U     PIC X(8).
             10 FILLER           PIC X(4).
             10 SCTH-QUANTITY-U  PIC X(8).
             10 FILLER           PIC X(4).
             10 SCTH-METHOD-U    PIC X(15).
             10 FILLER           PIC X(4).
             10 SCTH-FEE-U       PIC X(12).
             10 FILLER           PIC X(4).
             10 SCTH-COST-U      PIC X(8).

      *    Display data structure for shopping cart
           05 SHOPPING-CART-TABLE-DISPLAY.
             10 SCTD-MEMBER    PIC X(10).
             10 FILLER        PIC X(4).
             10 SCTD-CODE      PIC Z(4).
             10 FILLER        PIC X(4).
             10 SCTD-PRODUCT   PIC X(35).
             10 FILLER        PIC X(4).
             10 SCTD-PRICE     PIC Z(5).99.
             10 FILLER        PIC X(4).
             10 SCTD-QUANTITY  PIC Z(8).
             10 FILLER        PIC X(4).
             10 SCTD-METHOD    PIC X(15).
             10 FILLER        PIC X(4).
             10 SCTD-FEE       PIC Z(9).99.
             10 FILLER        PIC X(4).
             10 SCTD-COST      PIC Z(5).99.
           
           05 SHOPPING-CART-TOTAL-DISPLAY.
             10 FILLER         PIC X(8) VALUE "TOTAL $ ".
             10 SCTD-TOTAL     PIC Z(5).99.
           
      *    Temporary data structure for Bubble sort
           05 TEMP-CART.
             10 FILLER    PIC X(3).
             10 FILLER    PIC 9(4).
             10 FILLER    PIC X(35).
             10 FILLER    PIC 9(5)V9(2).
             10 FILLER    PIC 9(2).
             10 FILLER    PIC X(15).
             10 FILLER    PIC 9(5)V99.
             10 FILLER    PIC 9(5)V99.  

      *    *************************************************************
      *
      *    Shopping Cart Report Data structures
      *
      *    *************************************************************

      *    Shopping cart report data structure
           05 SHOPPING-CART-REPORT-INDEX OCCURS 9999 TIMES
               INDEXED BY SCR-IDX.
             10 SCRI-CODE      PIC 9(4).
             10 SCRI-PRODUCT   PIC X(35).
             10 SCRI-PRICE     PIC 9(5)V99.
             10 SCRI-QUANTITY  PIC 9(4).
             10 SCRI-COST      PIC 9(5)V99.
           
           05 SHOPPING-CART-REPORT-DISPLAY.
             10 SCRD-CODE      PIC Z(4).
             10 SCRD-PRODUCT   PIC X(35).
             10 SCRD-PRICE     PIC Z(5).99.
             10 SCRD-QUANTITY  PIC Z(8).
             10 SCRD-COST      PIC Z(5).99.

      *    *************************************************************
      *
      *    Main body of code
      *
      *    *************************************************************
       
       PROCEDURE DIVISION.
      *    REQUIRED FUNCTION
           PERFORM BUILD-CATALOGUE-TABLE
      *    REQUIRED FUNCTION
           
           DISPLAY "IS THIS TO BE RUN INTERACTIVELY OR " 
                   "NON-INTERACTIVELY? (Y/N): "
                   WITH NO ADVANCING
           ACCEPT WS-INTERACT-RESP
           
           EVALUATE TRUE
            WHEN WS-INTERACT-RESP = "Y"
              PERFORM QUERY-USER-VERSION
            WHEN WS-INTERACT-RESP = "N"
              PERFORM QUERY-NON-INTERACTIVE-VERSION
            WHEN OTHER
              DISPLAY "GOOD BYE."
           END-EVALUATE

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
           PERFORM GENERATE-SHIPPING-REPORT
           PERFORM DISPLAY-SHIPPING-REPORT
      
           .
      
      *    *************************************************************
      *
      *    Non-user interactive version of the code
      *
      *    ************************************************************* 
       QUERY-NON-INTERACTIVE-VERSION.
           PERFORM PROCESS-SHOPPING-CART
           
           DISPLAY " "
           PERFORM DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED
           PERFORM WRITE-CONSOLIDATED-DATA
           
           DISPLAY " "

           PERFORM SORT-TABLE
           PERFORM GENERATE-SHIPPING-REPORT
           PERFORM DISPLAY-SHIPPING-REPORT

           DISPLAY " "
           DISPLAY "THERE WERE: " WS-ERRORS " ERRORS DETECTED"

           PERFORM VARYING ERR-IDX FROM 1 BY 1 
                                   UNTIL ERR-IDX EQUAL WS-ERRORS
             DISPLAY "ERROR ON LINE: "
                     EL-LINE(ERR-IDX)
                     " ERROR: "
                     EL-MESSAGE(ERR-IDX)
           END-PERFORM
       .
      *    *************************************************************
      *
      *    Consolidate the data into the data
      *    Increment the index
      *    Check how many records have been recorded so far and notify
      *    the user if closing in on the WS-MAX.
      *
      *    *************************************************************

      *    Consolidate the indexed table
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

      *    Display a warning message at 9000 records
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
       
       SETUP-HEADERS.
           MOVE WS-DASH TO SCTH-MEMBER-U
           MOVE WS-DASH TO SCTH-CODE-U
           MOVE WS-DASH TO SCTH-PRODUCT-U
           MOVE WS-DASH TO SCTH-PRICE-U
           MOVE WS-DASH TO SCTH-QUANTITY-U
           MOVE WS-DASH TO SCTH-METHOD-U
           MOVE WS-DASH TO SCTH-FEE-U
           MOVE WS-DASH TO SCTH-COST-U
       .

       DISPLAY-SHOPPING-CART-HEADERS.
      *    MOVE WS-DASH TO SCTH-MEMBER-U
      *    MOVE WS-DASH TO SCTH-CODE-U
      *    MOVE WS-DASH TO SCTH-PRODUCT-U
      *    MOVE WS-DASH TO SCTH-PRICE-U
      *    MOVE WS-DASH TO SCTH-QUANTITY-U
      *    MOVE WS-DASH TO SCTH-METHOD-U
      *    MOVE WS-DASH TO SCTH-FEE-U
      *    MOVE WS-DASH TO SCTH-COST-U
           PERFORM SETUP-HEADERS
           
           DISPLAY SCTH-MEMBER WS-GAP
                     SCTH-CODE WS-GAP
                     SCTH-PRODUCT WS-GAP
                     SCTH-PRICE WS-GAP
                     SCTH-QUANTITY WS-GAP
                     SCTH-METHOD WS-GAP
                     SCTH-FEE WS-GAP
                     SCTH-COST

           DISPLAY SCTH-MEMBER-U WS-GAP
                     SCTH-CODE-U WS-GAP
                     SCTH-PRODUCT-U WS-GAP
                     SCTH-PRICE-U WS-GAP
                     SCTH-QUANTITY-U WS-GAP
                     SCTH-METHOD-U WS-GAP
                     SCTH-FEE-U WS-GAP
                     SCTH-COST-U
       .
       
      * Display the indexed table
       DISPLAY-CONSOLIDATED-DATA-TABLE-INDEXED.
           MOVE 0 TO WS-TOTAL-COST
                
           PERFORM DISPLAY-SHOPPING-CART-HEADERS

           PERFORM VARYING SCT-IDX FROM 1 BY 1 UNTIL SCT-IDX 
                   EQUAL SCT-IDXC
             MOVE SCTI-MEMBER(SCT-IDX)   TO SCTD-MEMBER
             MOVE SCTI-CODE(SCT-IDX)     TO SCTD-CODE
             MOVE SCTI-PRODUCT(SCT-IDX)  TO SCTD-PRODUCT
             MOVE SCTI-PRICE(SCT-IDX)    TO SCTD-PRICE
             MOVE SCTI-QUANTITY(SCT-IDX) TO SCTD-QUANTITY
             MOVE SCTI-METHOD(SCT-IDX)   TO SCTD-METHOD
             MOVE SCTI-FEE(SCT-IDX)      TO SCTD-FEE
             MOVE SCTI-COST(SCT-IDX)     TO SCTD-COST
             COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + SCTI-COST(SCT-IDX)

             DISPLAY SCTD-MEMBER WS-GAP
                     SCTD-CODE WS-GAP
                     SCTD-PRODUCT WS-GAP
                     SCTD-PRICE WS-GAP
                     SCTD-QUANTITY WS-GAP
                     SCTD-METHOD WS-GAP
                     SCTD-FEE WS-GAP
                     SCTD-COST
           END-PERFORM
           
           DISPLAY " "
           MOVE WS-TOTAL-COST TO SCTD-TOTAL
           DISPLAY "TOTAL: $" SCTD-TOTAL
           .

      *    *************************************************************
      *
      *    Write the shopping cart to a file
      *
      *    ************************************************************* 
       WRITE-CONSOLIDATED-DATA.
           MOVE 0 TO WS-TOTAL-COST
           PERFORM SETUP-HEADERS

           OPEN OUTPUT SHOPCART-REPORT-FILE
             WRITE SHOPCART-HEADERS FROM SHOPPING-CART-TABLE-HEADERS
             WRITE SHOPCART-HEADERS FROM SHOPPING-CART-TABLE-U-LINE
             PERFORM VARYING SCT-IDX FROM 1 BY 1 UNTIL SCT-IDX
                     EQUAL SCT-IDXC
               MOVE SCTI-MEMBER(SCT-IDX)   TO SCTD-MEMBER
               MOVE SCTI-CODE(SCT-IDX)     TO SCTD-CODE
               MOVE SCTI-PRODUCT(SCT-IDX)  TO SCTD-PRODUCT
               MOVE SCTI-PRICE(SCT-IDX)    TO SCTD-PRICE
               MOVE SCTI-QUANTITY(SCT-IDX) TO SCTD-QUANTITY
               MOVE SCTI-METHOD(SCT-IDX)   TO SCTD-METHOD
               MOVE SCTI-FEE(SCT-IDX)      TO SCTD-FEE
               MOVE SCTI-COST(SCT-IDX)     TO SCTD-COST
               COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + 
                                       SCTI-COST(SCT-IDX)
               
               WRITE SHOPCART-REPORT FROM SHOPPING-CART-TABLE-DISPLAY
             END-PERFORM
             
             MOVE WS-TOTAL-COST TO SCTD-TOTAL
             WRITE SHOPCART-REPORT-TOTAL FROM 
                   SHOPPING-CART-TOTAL-DISPLAY
                   AFTER ADVANCING 1 LINE
           CLOSE SHOPCART-REPORT-FILE

       .
      *    *************************************************************
      *
      *    Generate the shipping and dispatch reports
      *
      *    *************************************************************

      *    Generate the shipping report
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

      *    Original code, but modified by Copilot
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
       
       SETUP-SHIPPING-HEADERS.
           MOVE WS-DASH TO SCTH-MEMBER-U
           MOVE WS-DASH TO SCTH-CODE-U
           MOVE WS-DASH TO SCTH-PRODUCT-U
           MOVE WS-DASH TO SCTH-PRICE-U
           MOVE WS-DASH TO SCTH-QUANTITY-U
           MOVE WS-DASH TO SCTH-METHOD-U
           MOVE WS-DASH TO SCTH-FEE-U
           MOVE WS-DASH TO SCTH-COST-U
       .

       DISPLAY-SHIPPING-REPORT-HEADERS.
      *    MOVE WS-DASH TO SCTH-MEMBER-U
      *    MOVE WS-DASH TO SCTH-CODE-U
      *    MOVE WS-DASH TO SCTH-PRODUCT-U
      *    MOVE WS-DASH TO SCTH-PRICE-U
      *    MOVE WS-DASH TO SCTH-QUANTITY-U
      *    MOVE WS-DASH TO SCTH-METHOD-U
      *    MOVE WS-DASH TO SCTH-FEE-U
      *    MOVE WS-DASH TO SCTH-COST-U
           PERFORM SETUP-SHIPPING-HEADERS
           
           DISPLAY SCTH-CODE WS-GAP
                   SCTH-PRODUCT WS-GAP
                   SCTH-PRICE WS-GAP
                   SCTH-QUANTITY WS-GAP
                   SCTH-COST

           DISPLAY SCTH-CODE-U WS-GAP
                   SCTH-PRODUCT-U WS-GAP
                   SCTH-PRICE-U WS-GAP
                   SCTH-QUANTITY-U WS-GAP
                   SCTH-COST-U
       .
      
      *    Display the shipping and dispatch report
       DISPLAY-SHIPPING-REPORT.
           SET SCR-IDX TO 1
           
           PERFORM DISPLAY-SHIPPING-REPORT-HEADERS

           PERFORM VARYING SCR-IDX FROM 1 BY 1 
                                           UNTIL SCR-IDX EQUAL SCR-IDXC
             MOVE SCRI-CODE(SCR-IDX) TO SCRD-CODE
             MOVE SCRI-PRODUCT(SCR-IDX) TO SCRD-PRODUCT
             MOVE SCRI-PRICE(SCR-IDX) TO SCRD-PRICE
             MOVE SCRI-QUANTITY(SCR-IDX) TO SCRD-QUANTITY
             MOVE SCRI-COST(SCR-IDX) TO SCRD-COST

             DISPLAY SCRD-CODE WS-GAP
                     SCRD-PRODUCT WS-GAP
                     SCRD-PRICE WS-GAP
                     SCRD-QUANTITY WS-GAP
                     SCRD-COST
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
           SET HWC-IDX TO 1
           PERFORM UNTIL WS-EOF01 EQUAL 'Y'
             READ CSV-PRODUCT-FILE
               AT END MOVE 'Y' TO WS-EOF01
               NOT AT END
                 MOVE HWC-IDX TO HWC-IDXC
                 MOVE HWC-IDXC TO HWC-CODE
                 MOVE HWC-CODE TO PCT-CODE(HWC-IDX)
                 UNSTRING CSV-PRODUCT-RECORD
                   DELIMITED BY ','
                   INTO 
                     PCT-PRODUCT(HWC-IDX)
                     PCT-PRICE(HWC-IDX)
                 SET HWC-IDX UP BY 1
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
           SET ERR-IDX TO 1

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
                   MOVE WS-CART-LINE TO EL-LINE(ERR-IDX)
                   MOVE WS-DISP-MSG TO EL-MESSAGE(ERR-IDX)
                   SET ERR-IDX UP BY 1
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
           SET HWC-IDX TO 1
           MOVE 0 TO WS-COLS

           PERFORM VARYING HWC-IDX FROM 1 BY 1 
                                   UNTIL HWC-IDX GREATER HWC-IDXC
             MOVE PCT-CODE(HWC-IDX) TO PCD-CODE
             MOVE PCT-PRODUCT(HWC-IDX) TO PCD-PRODUCT
             MOVE PCT-PRICE(HWC-IDX) TO PCD-PRICE
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
           
           MOVE WS-DASH TO PCH-CODE-U-LINE
           MOVE WS-DASH TO PCH-PROD-U-LINE
           MOVE WS-DASH TO PCH-PRICE-U-LINE

           PERFORM UNTIL WS-COLS EQUAL 2
             IF WS-COLS EQUAL 0 THEN
               DISPLAY PCH-CODE-U-LINE WS-GAP
                       PCH-PROD-U-LINE WS-GAP
                       PCH-PRICE-U-LINE WS-GAP
                       WITH NO ADVANCING
             ELSE
               DISPLAY PCH-CODE-U-LINE WS-GAP
                       PCH-PROD-U-LINE WS-GAP
                       PCH-PRICE-U-LINE WS-GAP
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
      
      *    Ensure that the product code is between the ranges of
      *    1 and 40
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
           SET HWC-IDX TO 1
           SEARCH PRODUCT-CATALOGUE-TABLE
             AT END
               DISPLAY "ITEM NOT FOUND"
             WHEN PCT-CODE(HWC-IDX) EQUAL WS-PRODUCT-NUM
               MOVE PCT-CODE(HWC-IDX) TO WS-PRODUCT-CODE
               MOVE PCT-PRODUCT(HWC-IDX) TO WS-PRODUCT-DESC
               MOVE PCT-PRICE(HWC-IDX) TO WS-PRODUCT-PRICE
           END-SEARCH
           .

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

      *    Ensure that the quantity is between the ranges of 1 and 29
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
      *    PERFORM PROCESS-DELIVERY-METHOD
       .

      *    Validate that the appropriate delivery method is chosen
       VALIDATE-DELIVERY-METHOD.
           EVALUATE WS-DELIVERY
             WHEN WS-DEL
               MOVE "Y" TO WS-RESP-OK
             WHEN WS-PU
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
               IF WS-SILENT EQUAL "N" THEN
                 DISPLAY "INVALID DELIVERY METHOD. " 
                         "CHOOSE 'DELIVERY' OR 'PICK-UP'."
               END-IF
           END-EVALUATE.

      *    This is used to determine the what type of delivery method
      *    was used for calculations
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

      *    Calculate shipping fee based on what delivery method used,
      *    quantity, and if the base rate is the only factor or if the 
      *    base and subsequent item fee applies.
       CALCULATE-SHIP-FEE.
           MOVE 0 TO WS-SHIP-FEE
           
           IF WS-DELIVERY EQUAL WS-DEL THEN
             IF WS-QUANT-NUM GREATER THAN 1 THEN
               COMPUTE WS-SHIP-FEE = 2.00 + 
                       (( WS-QUANT-NUM - 1 ) * 1.60)
             ELSE
               MOVE 2.00 TO WS-SHIP-FEE
             END-IF
           END-IF.

      *    Calculate the cost using the above shipping fee added to the
      *    quantity multiplied by the price.  Then if the customer is a
      *    member, apply a discount.
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
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL SCT-IDXC
             PERFORM VARYING J FROM I BY 1 UNTIL J EQUAL SCT-IDXC
               IF SCTI-CODE(I) > SCTI-CODE(J)
                 PERFORM SWAP-RECORD
               END-IF
             END-PERFORM
           END-PERFORM
       .

      *    Swap the records around using a temporary table.
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
       

       
