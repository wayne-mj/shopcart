       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCT.
       AUTHOR WAYNE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *    Define the file name and set it as a sequential file with each
      *    line being beneath the previous.
       FILE-CONTROL.
      *    File access for product and price list
       SELECT CSV-PRODUCT-FILE 
           ASSIGN TO "product.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
      *    File access for shopping cart orders
       SELECT CSV-SHOPPING-CART-FILE
           ASSIGN TO "shop-cart.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
      *    File access for shopping cart orders
       SELECT SHOPCART-REPORT-FILE
           ASSIGN TO "shopcart.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
      *    File access for shipping report
       SELECT SHIP-REPORT-FILE
           ASSIGN TO "shipping.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
      *    File access for error report
       SELECT ERROR-REPORT-FILE
           ASSIGN TO "error.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *    File descriptor for product and price list
       FD  CSV-PRODUCT-FILE.
      *    While it is know to be a comma separated string, the actual
      *    format of the file is not really known as such.
       01  CSV-PROD-RECORD PIC X(80).

      *    File descriptor for shopping cart order file
       FD  CSV-SHOPPING-CART-FILE.
      *    There are four fields, member, product code, quantity, and
      *    delivery method; but as these are of unknown length each time
      *    for a comma separated string it is easier to allocate a large
      *    buffer.
       01  CSV-SHOPPING-CART-RECORD PIC X(80).
       
      *    Shopping cart order file descriptor and associated record
      *    formats
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

      *    Shipping report file descriptor and associated record
      *    formats
       FD  SHIP-REPORT-FILE.
       01  SHIP-REPORT-HEADER.
           05 SRH-CODE      PIC X(4).
           05 FILLER        PIC X(4).
           05 SRH-PRODUCT   PIC X(35).
           05 FILLER        PIC X(4).
           05 SRH-PRICE     PIC X(8).
           05 FILLER        PIC X(4).
           05 SRH-QUANTITY  PIC X(8).
           05 FILLER        PIC X(4).
           05 SRH-COST      PIC X(8).
       
       01  SHIP-REPORT-RECORD.
           05 SRR-CODE      PIC Z(4).
           05 FILLER        PIC X(4).
           05 SRR-PRODUCT   PIC X(35).
           05 FILLER        PIC X(4).
           05 SRR-PRICE     PIC Z(5).99.
           05 FILLER        PIC X(4).
           05 SRR-QUANTITY  PIC Z(8).
           05 FILLER        PIC X(4).
           05 SRR-COST      PIC Z(5).99.

      *    File descriptor for the error report with associated data
      *    records
       FD  ERROR-REPORT-FILE.
       01  ERROR-REPORT-RECORD.
           05 ERR-MESSAGE  PIC X(20).
           05 FILLER       PIC X(4).
           05 ERR-LINE     PIC Z(7).

       WORKING-STORAGE SECTION.
      *    Conditional values for for headers for reports
       01  WS-HEADERS  PIC X.
           88 SHOP-CART VALUE "C".
           88 SHIP-REP  VALUE "S".

      *    End of file marker
       01  WS-EOF01  PIC X(1)  VALUE "N".
      *    Gap between title and decorators
       01  WS-GAP    PIC X(4)  VALUE SPACES.
      *    Maximum number of records
       01  WS-MAX    PIC 9999 VALUE 9999.
      *    Tracking the number of columns
       01  WS-COLS   PIC 9.
      *    The decorator constant
       01  WS-DASH     PIC X(40) VALUE 
           "----------------------------------------".
      *    Title or Underline state
       01  WS-TORU   PIC X(1)  VALUE "N".
      *    Token to signify if the response if valid or not
       01  WS-RESP-OK PIC X(1) VALUE "N".
      *    Lines of the shopping cart order
       01  WS-CART-LINES  PIC 9(4)  VALUE 0.
      *    What type of error has occurred
       01  WS-DISP-MSG    PIC X(15).
      *    Notify of the error
       01  WS-DISP-ERR    PIC X VALUE "N".
      *    Variables required for Bubble Sort
       01  I            PIC 9(4) VALUE 0.
       01  J            PIC 9(4) VALUE 0.

      *    Used for the report generation to indicate if a record has 
      *    been found
       01  WS-FOUND          PIC X(1)    VALUE 'N'.
      *    Calculated quantity
       01  WS-REPORT-Q       PIC 9(4).
      *    Calculated cost
       01  WS-REPORT-C       PIC 9(5)V99.
       01  WS-ERRORS         PIC 9(4)    VALUE 0.

      *    Temporary Data structures and their numerical counterparts
       01  WS-SHOPPING-CART-DS.
           05  WS-MEMBER-RESP   PIC X(3).
           05  WS-PRODUCT-RESP  PIC X(2).
           05  WS-PRODUCT-NUM   PIC 9(2).
           05  WS-QUANT-RESP    PIC X(2).
           05  WS-QUANT-NUM     PIC 9(2).
           05  WS-DELIVERY      PIC X(15).
           05  WS-PRODUCT-CODE  PIC 9(4).
           05  WS-PRODUCT-DESC  PIC X(35).
           05  WS-PRODUCT-PRICE PIC 9(5)V99.
           05  WS-SHIP-FEE      PIC 9(5)V99.
           05  WS-COST          PIC 9(5)V99.
           05  WS-TOTAL-COST    PIC 9(5)V99.

      *    So I do not have to type over and over and over again
           05  WS-DELIVERY-METHODS.
               10 WS-DEL   PIC X(8) VALUE "DELIVERY".
               10 WS-PU    PIC X(7) VALUE "PICK-UP".

      *    *************************************************************
      *
      *    Homeware City Storage variables
      *
      *    *************************************************************
       01  HOMEWARECITY-STORAGE.
      *    Counter for Product Catalogue Table index
           05 HWC-IDXC  PIC 9(4).
      *    Temporary variable for product code
           05 HWC-CODE  PIC 9(4).
      *    Counter for Error Table index
           05 ERR-IDXC  PIC 9(4).
      *    Counter for Shopping Cart Table index
           05 SCT-IDXC  PIC 9(4).
      *    Counter for Shipping Report Table index
           05 SCR-IDXC  PIC 9(4).

      *    *************************************************************
      *
      *    Product Catalogue Table Data structures
      *
      *    *************************************************************

      *    Structure for Product catalogue table
           05  PRODUCT-CATALOGUE-TABLE 
               OCCURS 40 TIMES INDEXED BY HWC-IDX.
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
      *    Error logging table and corresponding display
      *
      *    *************************************************************
           
           05  ERROR-LOG 
               OCCURS 9999 TIMES INDEXED BY ERR-IDX.
             10 EL-MESSAGE  PIC X(20).
             10 FILLER      PIC X(4).
             10 EL-LINE     PIC 9(7).
           
           05 ERROR-LOG-DISPLAY.
             10 ERD-MESSAGE  PIC X(20).
             10 FILLER       PIC X(4).
             10 ERD-LINE     PIC Z(7).

      *    *************************************************************
      *
      *    Shopping Cart Table Data structures
      *
      *    *************************************************************
           
      *    Structure of table shopping cart
           05  SHOPPING-CART-TABLE-INDEXED 
               OCCURS 9999 TIMES
               ASCENDING KEY IS SCT-CODE
               INDEXED BY SCT-IDX.
             10 SCT-MEMBER    PIC X(3).
             10 SCT-CODE      PIC 9(4).
             10 SCT-PRODUCT   PIC X(35).
             10 SCT-PRICE     PIC 9(5)V9(2).
             10 SCT-QUANTITY  PIC 9(2).
             10 SCT-METHOD    PIC X(15).
             10 SCT-FEE       PIC 9(5)V99.
             10 SCT-COST      PIC 9(5)V99.

      *    Structure for headers for shopping cart
           05 SHOPPING-CART-TABLE-HEADERS.
             10 SCTH-MEMBER    PIC X(10).
             10 FILLER         PIC X(4).
             10 SCTH-CODE      PIC X(4).
             10 FILLER         PIC X(4).
             10 SCTH-PRODUCT   PIC X(35).
             10 FILLER         PIC X(4).
             10 SCTH-PRICE     PIC X(8).
             10 FILLER         PIC X(4).
             10 SCTH-QUANTITY  PIC X(8).
             10 FILLER         PIC X(4).
             10 SCTH-METHOD    PIC X(15).
             10 FILLER         PIC X(4).
             10 SCTH-FEE       PIC X(12).
             10 FILLER         PIC X(4).
             10 SCTH-COST      PIC X(8).
       
      *    Display data structure for shopping cart
           05 SHOPPING-CART-TABLE-DISPLAY.
             10 SCTD-MEMBER    PIC X(10).
             10 FILLER         PIC X(4).
             10 SCTD-CODE      PIC Z(4).
             10 FILLER         PIC X(4).
             10 SCTD-PRODUCT   PIC X(35).
             10 FILLER         PIC X(4).
             10 SCTD-PRICE     PIC Z(5).99.
             10 FILLER         PIC X(4).
             10 SCTD-QUANTITY  PIC Z(8).
             10 FILLER         PIC X(4).
             10 SCTD-METHOD    PIC X(15).
             10 FILLER         PIC X(4).
             10 SCTD-FEE       PIC Z(9).99.
             10 FILLER         PIC X(4).
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
      *    Shipping Report Data structures
      *
      *    *************************************************************

      *    Shopping cart report data structure
           05 SHIPPING-REPORT-INDEX OCCURS 9999 TIMES
               INDEXED BY SCR-IDX.
             10 SCRI-CODE      PIC 9(4).
             10 SCRI-PRODUCT   PIC X(35).
             10 SCRI-PRICE     PIC 9(5)V99.
             10 SCRI-QUANTITY  PIC 9(4).
             10 SCRI-COST      PIC 9(5)V99.
      
      *    Shopping cart report display structure
           05 SHIPPING-REPORT-DISPLAY.
             10 SCRD-CODE      PIC Z(4).
             10 FILLER         PIC X(4).
             10 SCRD-PRODUCT   PIC X(35).
             10 FILLER         PIC X(4).
             10 SCRD-PRICE     PIC Z(5).99.
             10 FILLER         PIC X(4).
             10 SCRD-QUANTITY  PIC Z(8).
             10 FILLER         PIC X(4).
             10 SCRD-COST      PIC Z(5).99.
      
      *    Shipping report header
       05 SHIPPING-REPORT-HEADER.
             10 SCRH-CODE      PIC X(4).
             10 FILLER         PIC X(4).
             10 SCRH-PRODUCT   PIC X(35).
             10 FILLER         PIC X(4).
             10 SCRH-PRICE     PIC X(8).
             10 FILLER         PIC X(4).
             10 SCRH-QUANTITY  PIC X(8).
             10 FILLER         PIC X(4).
             10 SCRH-COST      PIC X(8).

      *    *************************************************************
      *
      *    Begin the code block
      *
      *    *************************************************************

       PROCEDURE DIVISION.
           PERFORM BUILD-CATALOGUE-TABLE
           
           PERFORM QUERY-WRITE-DATASETS

           STOP RUN.

      *    *************************************************************
      *
      *    Build the product catalogue database from the product list and
      *    the product price list
      *
      *    *************************************************************

       BUILD-CATALOGUE-TABLE.
           OPEN INPUT CSV-PRODUCT-FILE.
           SET HWC-IDX TO 1
           PERFORM READ-AND-BUILD-TABLE UNTIL WS-EOF01 EQUAL 'Y'
             
           CLOSE CSV-PRODUCT-FILE
      
           MOVE 'N' TO WS-EOF01
       .
      
      *    Read each line from the file until EOF has been hit.

       READ-AND-BUILD-TABLE.
           READ CSV-PRODUCT-FILE
             AT END MOVE 'Y' TO WS-EOF01
             NOT AT END
               MOVE HWC-IDX TO HWC-IDXC
               MOVE HWC-IDXC TO HWC-CODE
               MOVE HWC-CODE TO PCT-CODE(HWC-IDX)
               UNSTRING CSV-PROD-RECORD
                 DELIMITED BY ','
                 INTO 
                   PCT-PRODUCT(HWC-IDX)
                   PCT-PRICE(HWC-IDX)
               SET HWC-IDX UP BY 1
       .
      
      *    *************************************************************
      *
      *    Display the catalogue formatted in two columns with 
      *    headers and decorators
      *
      *    *************************************************************

      *DISPLAY-PRODUCT-TABLE.
      *    SET HWC-IDX TO 1
      *
      *    PERFORM VARYING HWC-IDX FROM 1 BY 1 UNTIL HWC-IDX
      *            GREATER THAN HWC-IDXC
      *      MOVE PCT-CODE(HWC-IDX) TO PCD-CODE
      *      MOVE PCT-PRODUCT(HWC-IDX) TO PCD-PRODUCT
      *      MOVE PCT-PRICE(HWC-IDX) TO PCD-PRICE
      *
      *      DISPLAY PCD-CODE WS-GAP
      *              PCD-PRODUCT WS-GAP
      *              PCD-PRICE WS-GAP
      *    END-PERFORM
      *.

      *    *************************************************************
      *
      *    Display the catalogue formatted in two columns with 
      *    headers and decorators
      *
      *    *************************************************************

      *DISPLAY-CATALOGUE.
      *    PERFORM DISPLAY-CATALOGUE-HEADERS
      *    SET HWC-IDX TO 1
      *    MOVE 0 TO WS-COLS
      *
      *    PERFORM VARYING HWC-IDX FROM 1 BY 1 
      *                            UNTIL HWC-IDX GREATER HWC-IDXC
      *      MOVE PCT-CODE(HWC-IDX) TO PCD-CODE
      *      MOVE PCT-PRODUCT(HWC-IDX) TO PCD-PRODUCT
      *      MOVE PCT-PRICE(HWC-IDX) TO PCD-PRICE
      *      IF WS-COLS EQUAL 0 THEN
      *        DISPLAY PCD-CODE WS-GAP
      *                PCD-PRODUCT WS-GAP
      *                PCD-PRICE WS-GAP
      *                WITH NO ADVANCING
      *      ELSE
      *        DISPLAY PCD-CODE WS-GAP
      *                PCD-PRODUCT WS-GAP
      *                PCD-PRICE WS-GAP           
      *      END-IF
      *      
      *      ADD 1 TO WS-COLS
      *      IF WS-COLS EQUAL 2 THEN
      *        MOVE 0 TO WS-COLS
      *      END-IF
      *
      *    END-PERFORM
      *.

      *    *************************************************************
      *
      *    Generate the headers for the product catalogue table
      *
      *    *************************************************************

       DISPLAY-CATALOGUE-HEADERS.
           MOVE 0 TO WS-COLS
           MOVE "Y" TO WS-TORU
           PERFORM GENERATE-TITLE-COLUMN UNTIL WS-COLS EQUAL 2

           MOVE 0 TO WS-COLS
           MOVE "N" TO WS-TORU
           
           MOVE WS-DASH TO PCH-CODE-U-LINE
           MOVE WS-DASH TO PCH-PROD-U-LINE
           MOVE WS-DASH TO PCH-PRICE-U-LINE

           PERFORM GENERATE-TITLE-COLUMN UNTIL WS-COLS EQUAL 2
       .

      *    *************************************************************
      *
      *    Generate the columns for the title and the decorations
      *    depending on whether it is the first or second column
      *
      *    *************************************************************

       GENERATE-TITLE-COLUMN.
           IF WS-TORU EQUAL "Y" THEN
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
           ELSE
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
           END-IF
       .

      *    *************************************************************
      *
      *    Write datasets
      *
      *    *************************************************************

       QUERY-WRITE-DATASETS.
           PERFORM PROCESS-SHOPPING-CART-ORDERS
           PERFORM WRITE-CONSOLIDATED-DATA
           PERFORM SORT-TABLE
           PERFORM GENERATE-SHIPPING-REPORT
           PERFORM WRITE-SHIPPING-REPORT
           PERFORM WRITE-ERROR-REPORT
       .

      *    *************************************************************
      *
      *    Process the orders from the shopping cart
      *
      *    *************************************************************

       PROCESS-SHOPPING-CART-ORDERS.
           MOVE "N" TO WS-EOF01
           MOVE 0 TO WS-CART-LINES
           MOVE "N" TO WS-RESP-OK
           MOVE "N" TO WS-DISP-ERR
           SET ERR-IDX TO 1

           OPEN INPUT CSV-SHOPPING-CART-FILE
           PERFORM READ-SHOPPING-ORDERS UNTIL WS-EOF01 EQUAL "Y"

           CLOSE CSV-SHOPPING-CART-FILE
           MOVE "N" TO WS-EOF01
       .

      *    *************************************************************
      *
      *    Read the orders from the shopping cart
      *
      *    *************************************************************

       READ-SHOPPING-ORDERS.
           READ CSV-SHOPPING-CART-FILE
             AT END MOVE "Y" TO WS-EOF01
             NOT AT END
               UNSTRING CSV-SHOPPING-CART-RECORD
                 DELIMITED BY "," INTO
                   WS-MEMBER-RESP
                   WS-PRODUCT-RESP
                   WS-QUANT-RESP
                   WS-DELIVERY

      *    Track the shopping cart order line number
               ADD 1 TO WS-CART-LINES
      *    What is the member status, and is it correct?
               PERFORM PROCESS-IS-MEMBER
               IF WS-RESP-OK EQUAL "Y" THEN
      *    What is the product code?
                 PERFORM PROCESS-PRODUCT-CODE
                 IF WS-RESP-OK EQUAL "Y" THEN
      *    How much product is being dispatched
                   PERFORM PROCESS-QUANTITY
                   IF WS-RESP-OK EQUAL "Y" THEN
      *    How is the product being dispatched
                     PERFORM PROCESS-DELIVERY
                     IF WS-RESP-OK EQUAL "Y" THEN
      *    Calculate the shipping fee
                       PERFORM CALCULATE-SHIP-FEE
      *    Calculate the cost
                       PERFORM CALCULATE-COST
      *    Combine all the data points into the table
                       PERFORM CONSOLIDATE-SHOPPING-DATA-TO-TABLE
                     ELSE
                       MOVE "Y" TO WS-DISP-ERR
                       MOVE "DELIVERY" TO WS-DISP-MSG
                   ELSE
                     MOVE "Y" TO WS-DISP-ERR
                     MOVE "QUANTITY" TO WS-DISP-MSG
                   END-IF
                 ELSE
                   MOVE "Y" TO WS-DISP-ERR
                   MOVE "CODE" TO WS-DISP-MSG
                 END-IF
               ELSE
                 MOVE "Y" TO WS-DISP-ERR
                 MOVE "MEMBER" TO WS-DISP-MSG
               END-IF
               
      *    If an error has been detected log that error
               IF WS-DISP-ERR EQUAL "Y" THEN
                 ADD 1 TO WS-ERRORS
                 MOVE WS-CART-LINES TO EL-LINE(ERR-IDX)
                 MOVE WS-DISP-MSG TO EL-MESSAGE(ERR-IDX)
                 SET ERR-IDX UP BY 1
                 MOVE ERR-IDX TO ERR-IDXC
               END-IF
       .

      *    *************************************************************
      *
      *    Process Member Status
      *
      *    *************************************************************

       PROCESS-IS-MEMBER.
           MOVE "N" TO WS-RESP-OK
           PERFORM VALIDATE-MEMBER
       .

      *    *************************************************************
      *
      *    Validate the member status
      *
      *    *************************************************************

       VALIDATE-MEMBER.
           EVALUATE WS-MEMBER-RESP
             WHEN "YES"
               MOVE "Y" TO WS-RESP-OK
             WHEN "NO"
               MOVE "Y" TO WS-RESP-OK
             WHEN "END"
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
              MOVE "N" TO WS-RESP-OK
       .

      *    *************************************************************
      *
      *    Process product code and if valid search for the product
      *
      *    *************************************************************

       PROCESS-PRODUCT-CODE.
           MOVE "N" TO WS-RESP-OK

           COMPUTE WS-PRODUCT-NUM = FUNCTION NUMVAL(WS-PRODUCT-RESP)
           PERFORM VALIDATE-PRODUCT-CODE
           IF WS-RESP-OK EQUAL "Y" THEN
             PERFORM SEARCH-PRODUCT-CODE
       .

      *    *************************************************************
      *
      *    Validate product code is within the appropriate ranges
      *
      *    *************************************************************

       VALIDATE-PRODUCT-CODE.
           EVALUATE TRUE
            WHEN WS-PRODUCT-NUM GREATER 0 AND WS-PRODUCT-NUM LESS 41
              MOVE "Y" TO WS-RESP-OK
            WHEN OTHER
              MOVE "N" TO WS-RESP-OK
       .

      *    *************************************************************
      *    
      *    Search for the product based on the product code
      *    
      *    *************************************************************

       SEARCH-PRODUCT-CODE.
           SET HWC-IDX TO 1
           SEARCH PRODUCT-CATALOGUE-TABLE
             AT END
               DISPLAY "ITEM NO FOUND"
             WHEN PCT-CODE(HWC-IDX) EQUAL WS-PRODUCT-NUM
               MOVE PCT-CODE(HWC-IDX) TO WS-PRODUCT-CODE
               MOVE PCT-PRODUCT(HWC-IDX) TO WS-PRODUCT-DESC
               MOVE PCT-PRICE(HWC-IDX) TO WS-PRODUCT-PRICE
       .

      *    *************************************************************
      *
      *    Process the quantity of product being ordered
      *
      *    *************************************************************

       PROCESS-QUANTITY.
           MOVE "N" TO WS-RESP-OK

           COMPUTE WS-QUANT-NUM = FUNCTION NUMVAL(WS-QUANT-RESP)
           PERFORM VALIDATE-QUANTITY
       .

      *    *************************************************************
      *
      *    Validate the quantity range
      *    
      *    *************************************************************

       VALIDATE-QUANTITY.
           EVALUATE TRUE
            WHEN WS-QUANT-NUM GREATER 0 AND LESS 30
              MOVE "Y" TO WS-RESP-OK
            WHEN OTHER
              MOVE "N" TO WS-RESP-OK
           END-EVALUATE
       .

      *    *************************************************************
      *
      *    Process delivery methods
      *
      *    *************************************************************

       PROCESS-DELIVERY.
           MOVE "N" TO WS-RESP-OK
           PERFORM VALIDATE-DELIVERY-METHOD
       .

      *    *************************************************************
      *
      *    Validate the delivery methods
      *
      *    *************************************************************
       
       VALIDATE-DELIVERY-METHOD.
           EVALUATE WS-DELIVERY
             WHEN WS-DEL
               MOVE "Y" TO WS-RESP-OK
             WHEN WS-PU
               MOVE "Y" TO WS-RESP-OK
             WHEN OTHER
              MOVE "N" TO WS-RESP-OK
       .

      *    *************************************************************
      *
      *    Calculate shipping fee
      *
      *    *************************************************************

       CALCULATE-SHIP-FEE.
           MOVE 0 TO WS-SHIP-FEE

           IF WS-DELIVERY EQUAL WS-DEL THEN
             IF WS-QUANT-NUM GREATER THAN 1 THEN
               COMPUTE WS-SHIP-FEE = 2.00 +
                       (( WS-QUANT-NUM - 1) * 1.60 )
             ELSE
               MOVE 2.00 TO WS-SHIP-FEE
             END-IF
           END-IF
       .

      *    *************************************************************
      *
      *    Calculate overall cost
      *
      *    *************************************************************

       CALCULATE-COST.
           MOVE 0 to WS-COST

           COMPUTE WS-COST = (WS-QUANT-NUM * WS-PRODUCT-PRICE) +
                             WS-SHIP-FEE
           
           IF WS-MEMBER-RESP EQUAL "YES" THEN
             COMPUTE WS-COST = WS-COST * (90 / 100)
           END-IF
       .

      *    *************************************************************
      *
      *    Consolidate the data into the table
      *
      *    *************************************************************
       
       CONSOLIDATE-SHOPPING-DATA-TO-TABLE.
           MOVE WS-MEMBER-RESP TO SCT-MEMBER(SCT-IDX)
           MOVE WS-PRODUCT-CODE TO SCT-CODE(SCT-IDX)
           MOVE WS-PRODUCT-DESC TO SCT-PRODUCT(SCT-IDX)
           MOVE WS-PRODUCT-PRICE TO SCT-PRICE(SCT-IDX)
           MOVE WS-QUANT-NUM TO SCT-QUANTITY(SCT-IDX)
           MOVE WS-DELIVERY TO SCT-METHOD(SCT-IDX)
           MOVE WS-SHIP-FEE TO SCT-FEE(SCT-IDX)
           MOVE WS-COST TO SCT-COST(SCT-IDX)

           SET SCT-IDX UP BY 1
           MOVE SCT-IDX TO SCT-IDXC
           
      *    Monitor the amount of records being tracked and notify
           IF SCT-IDXC EQUAL 9900 OR GREATER 9900 THEN
             DISPLAY " *** WARNING: " SCT-IDXC
                     " RECORDS OF: " WS-MAX " ***"
       .

      *    *************************************************************
      *
      *    Write the consolidated data to disk
      *
      *    *************************************************************

       WRITE-CONSOLIDATED-DATA.
           MOVE 0 TO WS-TOTAL-COST
           SET SHOP-CART TO TRUE

           OPEN OUTPUT SHOPCART-REPORT-FILE
             MOVE "Y" TO WS-TORU
             PERFORM SETUP-REPORT-HEADERS
             WRITE SHOPCART-HEADERS FROM SHOPPING-CART-TABLE-HEADERS
             MOVE "N" TO WS-TORU
             PERFORM SETUP-REPORT-HEADERS
             WRITE SHOPCART-HEADERS FROM SHOPPING-CART-TABLE-HEADERS

             PERFORM VARYING SCT-IDX FROM 1 BY 1 
                     UNTIL SCT-IDX EQUAL SCT-IDXC
               MOVE SCT-MEMBER(SCT-IDX)   TO SCTD-MEMBER
               MOVE SCT-CODE(SCT-IDX)     TO SCTD-CODE
               MOVE SCT-PRODUCT(SCT-IDX)  TO SCTD-PRODUCT
               MOVE SCT-PRICE(SCT-IDX)    TO SCTD-PRICE
               MOVE SCT-QUANTITY(SCT-IDX) TO SCTD-QUANTITY
               MOVE SCT-METHOD(SCT-IDX)   TO SCTD-METHOD
               MOVE SCT-FEE(SCT-IDX)      TO SCTD-FEE
               MOVE SCT-COST(SCT-IDX)     TO SCTD-COST
               COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + 
                                       SCT-COST(SCT-IDX)
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
      *    Setup the headers for each of the reports
      *    
      *    *************************************************************

       SETUP-REPORT-HEADERS.
           EVALUATE TRUE
             WHEN SHOP-CART
               IF WS-TORU EQUAL "Y" THEN
                 MOVE "MEMBER"           TO SCTH-MEMBER
                 MOVE "CODE"             TO SCTH-CODE
                 MOVE "PRODUCT"          TO SCTH-PRODUCT
                 MOVE "$  PRICE"         TO SCTH-PRICE
                 MOVE "QUANTITY"         TO SCTH-QUANTITY
                 MOVE "SHIPPING METHOD"  TO SCTH-METHOD
                 MOVE "SHIPPING FEE"     TO SCTH-FEE
                 MOVE "$   COST"         TO SCTH-COST
               ELSE
                 MOVE WS-DASH TO SCTH-MEMBER
                 MOVE WS-DASH TO SCTH-CODE
                 MOVE WS-DASH TO SCTH-PRODUCT
                 MOVE WS-DASH TO SCTH-PRICE
                 MOVE WS-DASH TO SCTH-QUANTITY
                 MOVE WS-DASH TO SCTH-METHOD
                 MOVE WS-DASH TO SCTH-FEE
                 MOVE WS-DASH TO SCTH-COST
               END-IF
             WHEN SHIP-REP
               IF WS-TORU EQUAL "Y" THEN
                 MOVE "CODE"             TO SCRH-CODE
                 MOVE "PRODUCT"          TO SCRH-PRODUCT
                 MOVE "$  PRICE"         TO SCRH-PRICE
                 MOVE "QUANTITY"         TO SCRH-QUANTITY
                 MOVE "$   COST"         TO SCRH-COST
               ELSE
                 MOVE WS-DASH TO SCRH-CODE
                 MOVE WS-DASH TO SCRH-PRODUCT
                 MOVE WS-DASH TO SCRH-PRICE
                 MOVE WS-DASH TO SCRH-QUANTITY
                 MOVE WS-DASH TO SCRH-COST
               END-IF

      *    IF SHOP-CART THEN
      *      IF WS-TORU EQUAL "Y" THEN
      *        MOVE "MEMBER"           TO SCTH-MEMBER
      *        MOVE "CODE"             TO SCTH-CODE
      *        MOVE "PRODUCT"          TO SCTH-PRODUCT
      *        MOVE "$  PRICE"         TO SCTH-PRICE
      *        MOVE "QUANTITY"         TO SCTH-QUANTITY
      *        MOVE "SHIPPING METHOD"  TO SCTH-METHOD
      *        MOVE "SHIPPING FEE"     TO SCTH-FEE
      *        MOVE "$   COST"         TO SCTH-COST
      *      ELSE
      *        MOVE WS-DASH TO SCTH-MEMBER
      *        MOVE WS-DASH TO SCTH-CODE
      *        MOVE WS-DASH TO SCTH-PRODUCT
      *        MOVE WS-DASH TO SCTH-PRICE
      *        MOVE WS-DASH TO SCTH-QUANTITY
      *        MOVE WS-DASH TO SCTH-METHOD
      *        MOVE WS-DASH TO SCTH-FEE
      *        MOVE WS-DASH TO SCTH-COST
      *    IF SHIP-REP THEN
      *      IF WS-TORU EQUAL "Y" THEN
      *        MOVE "CODE"             TO SCTH-CODE
      *        MOVE "PRODUCT"          TO SCTH-PRODUCT
      *        MOVE "$  PRICE"         TO SCTH-PRICE
      *        MOVE "QUANTITY"         TO SCTH-QUANTITY
      *        MOVE "$   COST"         TO SCTH-COST
      *      ELSE
      *        MOVE WS-DASH TO SCTH-CODE
      *        MOVE WS-DASH TO SCTH-PRODUCT
      *        MOVE WS-DASH TO SCTH-PRICE
      *        MOVE WS-DASH TO SCTH-QUANTITY
      *        MOVE WS-DASH TO SCTH-COST
      *      END-IF
      *    END-IF
       .

      *    *************************************************************
      *
      *    Functions and methods to perform Bubble sort
      *    This is based loosely on the W3 School Python version
      *
      *    *************************************************************

       SORT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL SCT-IDXC
             PERFORM VARYING J FROM I BY 1 UNTIL J EQUAL SCT-IDXC
               IF SCT-CODE(I) > SCT-CODE(J)
                 PERFORM SWAP-RECORD
               END-IF
             END-PERFORM
           END-PERFORM
       .

      *    *************************************************************
      *
      *    Swap the records around using a temporary table.
      *
      *    *************************************************************

       SWAP-RECORD.
           MOVE SHOPPING-CART-TABLE-INDEXED(I) TO TEMP-CART
           MOVE SHOPPING-CART-TABLE-INDEXED(J) TO
                SHOPPING-CART-TABLE-INDEXED(I)
           MOVE TEMP-CART TO SHOPPING-CART-TABLE-INDEXED(J)
       .

      *    *************************************************************
      *
      *    Generate shipping report
      *
      *    *************************************************************

       GENERATE-SHIPPING-REPORT.
           MOVE 1 TO SCR-IDXC
           MOVE "N" TO WS-FOUND
           SET SHIP-REP TO TRUE

           PERFORM VARYING SCT-IDX FROM 1 BY 1
                   UNTIL SCT-IDX EQUAL SCT-IDXC
             PERFORM VARYING SCR-IDX FROM 1 BY 1 
                     UNTIL SCR-IDX EQUAL SCR-IDXC
               IF SCRI-CODE(SCR-IDX) EQUAL SCT-CODE(SCT-IDX) THEN
                 COMPUTE WS-REPORT-Q = SCRI-QUANTITY(SCR-IDX) +
                                       SCT-QUANTITY(SCT-IDX)
                 COMPUTE WS-REPORT-C = SCRI-COST(SCR-IDX) +
                                       SCT-COST(SCT-IDX)
                 MOVE WS-REPORT-Q TO SCRI-QUANTITY(SCR-IDX)
                 MOVE WS-REPORT-C TO SCRI-COST(SCR-IDX)

                 MOVE "Y" TO WS-FOUND
                 EXIT PERFORM
               END-IF
             END-PERFORM

             IF WS-FOUND EQUAL "N" THEN
               MOVE SCT-CODE(SCT-IDX) TO SCRI-CODE(SCR-IDX)
               MOVE SCT-PRODUCT(SCT-IDX) TO SCRI-PRODUCT(SCR-IDX)
               MOVE SCT-PRICE(SCT-IDX) TO SCRI-PRICE(SCR-IDX)
               MOVE SCT-QUANTITY(SCT-IDX) TO SCRI-QUANTITY(SCR-IDX)
               MOVE SCT-COST(SCT-IDX) TO SCRI-COST(SCR-IDX)
               ADD 1 TO SCR-IDXC
             END-IF
             MOVE "N" TO WS-FOUND
           END-PERFORM
       .

      *    *************************************************************
      *
      *    Write the shipping report to file
      *
      *    *************************************************************

       WRITE-SHIPPING-REPORT.
           OPEN OUTPUT SHIP-REPORT-FILE.
             MOVE "Y" TO WS-TORU
             PERFORM SETUP-REPORT-HEADERS
             WRITE SHIP-REPORT-HEADER FROM SHIPPING-REPORT-HEADER

             MOVE "N" TO WS-TORU
             PERFORM SETUP-REPORT-HEADERS
             WRITE SHIP-REPORT-HEADER FROM SHIPPING-REPORT-HEADER

             PERFORM VARYING SCR-IDX FROM 1 BY 1 
                     UNTIL SCR-IDX EQUAL SCR-IDXC
               MOVE SCRI-CODE(SCR-IDX) TO SCRD-CODE
               MOVE SCRI-PRODUCT(SCR-IDX) TO SCRD-PRODUCT
               MOVE SCRI-PRICE(SCR-IDX) TO SCRD-PRICE
               MOVE SCRI-QUANTITY(SCR-IDX) TO SCRD-QUANTITY
               MOVE SCRI-COST(SCR-IDX) TO SCRD-COST

               WRITE SHIP-REPORT-RECORD FROM SHIPPING-REPORT-DISPLAY
             END-PERFORM
           CLOSE SHIP-REPORT-FILE
       .

       WRITE-ERROR-REPORT.
           OPEN OUTPUT ERROR-REPORT-FILE.

             PERFORM VARYING ERR-IDX FROM 1 BY 1
             UNTIL ERR-IDX EQUAL WS-ERRORS
               MOVE EL-LINE(ERR-IDX) TO ERD-LINE
               MOVE EL-MESSAGE(ERR-IDX) TO ERD-MESSAGE

               WRITE ERROR-REPORT-RECORD FROM ERROR-LOG-DISPLAY
             END-PERFORM

           CLOSE ERROR-REPORT-FILE
       .
