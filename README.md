## Shopping cart program

This is another re-write of another Python program project that had two lists, one for products, and the other for prices that were combines to make a CSV string list that could be search to build a shopping cart.

I have shied away from this a bit, as COBOL does not quite work the same way as Python, and trying to keep everything in memory rather than using a database or file storage like I would prefer.

Rewriting some of the code has been a challenge as there are some aspects that just do not translate well:
- Booleans are not the same, while they do sort of exist in COBOL, they behave differently
```COBOL
        01 WS-BOOL PIC X(1) VALUE 'Y'
            
            MOVE 'N' TO WS-BOOL
            IF WS-BOOL EQUAL 'N' THEN
              DISPLAY "NO"
            ELSE
              DISPLAY "YES"
            END-IF
```
- Variables are global and have levels