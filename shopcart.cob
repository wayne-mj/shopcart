      * COBOL rewrite of the Python code of the 3rd assessment
      * Because many of the features of modern programming languages
      * are not present in COBOL this is going to be a challenge.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     SHOPCART.
       AUTHOR.         WAYNE JACKSON.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HOMEWARECITY-STORAGE.
           05 SHOP-CATALOGUE.
             10 SC-CODE PIC 9(2).
             10 SC-PRODUCT PIC X(32).
             10 SC-PRICE PIC 9V9(7).

       PROCEDURE DIVISION.
           
           STOP RUN.
