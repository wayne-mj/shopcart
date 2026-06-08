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

## The plan for this project.

1. Take the CSV file that contains the product and price lists and make the catalogue
2. Using a second CSV file that contains the order information, generate the shopping cart
3. Display the shopping cart after processing the order with the appropriate calculations
4. Display the summary shipping manifest

## What has been accomplished.

- The program will read the product and price lists from the data file, and produce an in memory data structure that can be searched using a 1 indexed table that is also the basis of the product code.
- It is capable of processing 10K of records, and identifying the error and what corrective measure is required and displays the appropriate message to the user.
- Using Bubble sort, it can organise the records in product code order.

## Small changes to the code

- All tables are now indexed
  - This now makes the searching of the table handled internally rather than having to right a separate routine
- Sort is file based, rather than in memory, so this is why Bubble Sort is used to sort and generate the shipping report.

## Comparing the development of Python and COBOL versions

The Python version was more difficult to develop given the restraint that needed to be shown.  There was a wealth of libraries and functions available for use, but the scope of the course prevented access to these.  COBOL on the otherhand, because it was limited to begin with, did not suffer from this.  I knew going in that I had limits and these were set by the language itself.  I knew I had to write the all of the string handling routines from scratch, or at from a primitive to a more advance feature.  I was going to have to be creative about how I accomplished a task by breaking it down into smaller tasks to perform.