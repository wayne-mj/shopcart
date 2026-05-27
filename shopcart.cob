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

           SELECT CVS-PRODUCT-DB
             ASSIGN TO "product.dat"
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

      * File descriptor for the product's database
       FD CVS-PRODUCT-DB.
      * The line length for the database.
       01  CVS-PRODUCT-DB-RECORD.
           05 PROD-DB-CODE  PIC 9(4).
           05 FILLER        PIC X(1) VALUE ";".
           05 PROD-DB-PROD  PIC X(35).
           05 FILLER        PIC X(1) VALUE ";".
           05 PROD-DB-PRICE PIC 9(5)V9(2).
      
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
       01  WS-EOF01                  PIC X(1) VALUE 'N'.
       01  WS-EOF02                  PIC X(1) VALUE 'N'.

      *    *************************************************************
      *
      *    Homeware City Storage variables
      *
      *    *************************************************************
       01  HOMEWARECITY-STORAGE.
           05 HWC-INDEX              PIC 9(10).
           05 HWC-CODE               PIC 9(2).
      *    *************************************************************
      *
      *    Data structures for the tables
      *
      *    *************************************************************
           05 PRODUCT-CATALOGUE-TABLE.
             10 PCT-CODE            PIC 9(4).
             10 PCT-PRODUCT         PIC X(35).
             10 PCT-PRICE           PIC 9(5)V9(2).
      *    *************************************************************
      *
      *    Main body of code
      *
      *    *************************************************************
       PROCEDURE DIVISION.

           STOP RUN.
       
