       IDENTIFICATION DIVISION.
       PROGRAM-ID. muck.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE      PIC X(1).
       01 WS-MEMBER        PIC 9.
       01 WS-PRODUCT-CODE  PIC 9(2).
       01 WS-QUANTITY      PIC 9(2).
       01 WS-DELIVERY      PIC 9.
       
       SCREEN SECTION.
       01  MAIN-MENU-SCREEN.
           05 BLANK SCREEN
               BACKGROUND-COLOR 0
               FOREGROUND-COLOR 2.
           05 LINE 1 COLUMN 1 VALUE "==============================" &
              "==========".
           05 LINE 2 COLUMN 10 VALUE "CUSTOMER ORDERS".
           05 LINE 3 COLUMN 1 VALUE "Member: 1=YES, 2=NO :".
           05 LINE 3 COLUMN 36 PIC 9 TO WS-MEMBER
             REQUIRED.
           05 LINE 5 COLUMN 1 VALUE "Product Code: (1-40) :".
           05 LINE 5 COLUMN 35 PIC 9(2) TO WS-PRODUCT-CODE
             REQUIRED.
           05 LINE 7 COLUMN 1 VALUE "Quantity: (1-29) :".
           05 LINE 7 COLUMN 35 PIC 9(2) TO WS-QUANTITY
             REQUIRED.
           05 LINE 9 COLUMN 1 VALUE "Delivery: 1=Delivery 2=Pick-Up: ".
           05 LINE 9 COLUMN 36 PIC 9 TO WS-DELIVERY
             REQUIRED.
           05 LINE 11 COLUMN 1 VALUE "==============================" &
              "==========".
           05 LINE 14 COLUMN 10 PIC X(1) TO MENU-CHOICE
              REQUIRED AUTO-SKIP.

       PROCEDURE DIVISION.
           DISPLAY MAIN-MENU-SCREEN
           ACCEPT MAIN-MENU-SCREEN.
           
           DISPLAY WS-MEMBER
           DISPLAY WS-PRODUCT-CODE
           DISPLAY WS-QUANTITY
           DISPLAY WS-DELIVERY
           
           STOP RUN.
