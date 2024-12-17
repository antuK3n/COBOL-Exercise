       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-CSV.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT CSV-FILE ASSIGN TO 'RECEIPT.csv'
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.  
       01  CSV-RECORD.
           05  ITEM-NAME            PIC X(50).
           05  FILLER-1             PIC X(1).
           05  QUANTITY             PIC 9(3).
           05  FILLER-2             PIC X(1).
           05  PRICE                PIC X(10).
           05  FILLER-3             PIC X(1).
           05  SUBTOTAL             PIC X(10).

       WORKING-STORAGE SECTION.
       01  EOF-FLAG               PIC X VALUE 'N'.
       01  PRODUCT-NAME           PIC X(50).
       01  PRODUCT-QUANTITY       PIC 9(3).
       01  PRODUCT-PRICE          PIC X(10).
       01  PRODUCT-SUBTOTAL       PIC X(10).
       01  TEMP-QUANTITY          PIC X(10).
       01  TEMP-PRICE             PIC X(15).
       01  TEMP-SUBTOTAL          PIC X(15).
       
       01  SUBTOTAL-TOTAL         PIC X(10) VALUE SPACES.
       01  DISCOUNT-TOTAL         PIC X(10) VALUE SPACES.
       01  TAX-TOTAL              PIC X(10) VALUE SPACES.
       01  GRANDTOTAL-TOTAL       PIC X(10) VALUE SPACES.

       01  ITEM-COUNT             PIC 9(3) VALUE 0.
       01  ITEM-INDEX             PIC 9(3).
       01  DISPLAY-NAME           OCCURS 100 TIMES PIC X(50).
       01  DISPLAY-QUANTITY       OCCURS 100 TIMES PIC 9(3).
       01  DISPLAY-PRICE          OCCURS 100 TIMES PIC X(10).
       01  DISPLAY-SUBTOTAL       OCCURS 100 TIMES PIC X(10).

       PROCEDURE DIVISION.
           OPEN INPUT CSV-FILE
           PERFORM READ-CSV UNTIL EOF-FLAG = 'Y'
           CLOSE CSV-FILE.

           DISPLAY "----------------------------------------"
           DISPLAY "               RECEIPT                  "
           DISPLAY "----------------------------------------"

           PERFORM PRINT-ITEMS

           DISPLAY "----------------------------------------"
           DISPLAY "Subtotal: " SUBTOTAL-TOTAL
           DISPLAY "Discount: " DISCOUNT-TOTAL
           DISPLAY "Tax: " TAX-TOTAL
           DISPLAY "Grand Total: " GRANDTOTAL-TOTAL
           DISPLAY "----------------------------------------"
           DISPLAY "Thank you for shopping with us!"
           DISPLAY "----------------------------------------"

           STOP RUN.

       READ-CSV.
           READ CSV-FILE INTO CSV-RECORD
               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   PERFORM Parse-CSV-Record
           END-READ.

       Parse-CSV-Record.
           UNSTRING CSV-RECORD DELIMITED BY "," INTO
               PRODUCT-NAME
               TEMP-QUANTITY
               TEMP-PRICE
               TEMP-SUBTOTAL
           END-UNSTRING

           MOVE FUNCTION TRIM(PRODUCT-NAME) TO PRODUCT-NAME
           MOVE FUNCTION NUMVAL(TEMP-QUANTITY) TO PRODUCT-QUANTITY
           MOVE FUNCTION TRIM(TEMP-PRICE) TO PRODUCT-PRICE
           MOVE FUNCTION TRIM(TEMP-SUBTOTAL) TO PRODUCT-SUBTOTAL

           IF PRODUCT-NAME = "Subtotal"
               MOVE PRODUCT-SUBTOTAL TO SUBTOTAL-TOTAL
           ELSE IF PRODUCT-NAME = "Discount"
               MOVE PRODUCT-SUBTOTAL TO DISCOUNT-TOTAL
           ELSE IF PRODUCT-NAME = "Tax"
               MOVE PRODUCT-SUBTOTAL TO TAX-TOTAL
           ELSE IF PRODUCT-NAME = "Grand Total"
               MOVE PRODUCT-SUBTOTAL TO GRANDTOTAL-TOTAL
           ELSE
               ADD 1 TO ITEM-COUNT
               MOVE PRODUCT-NAME TO DISPLAY-NAME (ITEM-COUNT)
               MOVE PRODUCT-QUANTITY TO DISPLAY-QUANTITY (ITEM-COUNT)
               MOVE PRODUCT-PRICE TO DISPLAY-PRICE (ITEM-COUNT)
               MOVE PRODUCT-SUBTOTAL TO DISPLAY-SUBTOTAL (ITEM-COUNT)
           END-IF.

       PRINT-ITEMS.
           PERFORM VARYING ITEM-INDEX FROM 1 BY 1
               UNTIL ITEM-INDEX > ITEM-COUNT
                  DISPLAY "Item: " DISPLAY-NAME (ITEM-INDEX)
                  DISPLAY "Quantity: " DISPLAY-QUANTITY (ITEM-INDEX)
                  DISPLAY "Price: " DISPLAY-PRICE (ITEM-INDEX)
                  DISPLAY "Subtotal: " DISPLAY-SUBTOTAL (ITEM-INDEX)
                  DISPLAY ""
           END-PERFORM.
