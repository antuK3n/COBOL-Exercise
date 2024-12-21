       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-CSV.

       *> This program reads data from a CSV file named 'RECEIPT.csv'.
       *> It parses the data to generate and display a receipt.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT CSV-FILE ASSIGN TO 'RECEIPT.csv'
                       ORGANIZATION IS LINE SEQUENTIAL.

       *> The file 'RECEIPT.csv' stores product data and totals.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD.
           05  ITEM-NAME            PIC X(50). *> Item name.
           05  FILLER-1             PIC X(1). *> CSV delimiter.
           05  QUANTITY             PIC 9(3). *> Quantity.
           05  FILLER-2             PIC X(1). *> CSV delimiter.
           05  PRICE                PIC X(10). *> Price.
           05  FILLER-3             PIC X(1). *> CSV delimiter.
           05  SUBTOTAL             PIC X(10). *> Subtotal.

       *> CSV-RECORD defines the structure of the data in the file.

       WORKING-STORAGE SECTION.
       01  EOF-FLAG               PIC X VALUE 'N'. *> End-of-file flag.
       01  PRODUCT-NAME           PIC X(50). *> Product name.
       01  PRODUCT-QUANTITY       PIC 9(3). *> Quantity.
       01  PRODUCT-PRICE          PIC X(10). *> Price.
       01  PRODUCT-SUBTOTAL       PIC X(10). *> Subtotal.
       01  TEMP-QUANTITY          PIC X(10). *> Unformatted quantity.
       01  TEMP-PRICE             PIC X(15). *> Unformatted price.
       01  TEMP-SUBTOTAL          PIC X(15). *> Unformatted subtotal.

       01  SUBTOTAL-TOTAL         PIC X(10) VALUE SPACES. *> Subtotal.
       01  DISCOUNT-TOTAL         PIC X(10) VALUE SPACES. *> Discount.
       01  TAX-TOTAL              PIC X(10) VALUE SPACES. *> Tax.
       01  GRANDTOTAL-TOTAL       PIC X(10) VALUE SPACES. *> Grand total.

       01  ITEM-COUNT             PIC 9(3) VALUE 0. *> Number of items.
       01  ITEM-INDEX             PIC 9(3). *> Item index.
       01  DISPLAY-NAME           OCCURS 100 TIMES PIC X(50). *> Item names.
       01  DISPLAY-QUANTITY       OCCURS 100 TIMES PIC 9(3). *> Quantities.
       01  DISPLAY-PRICE          OCCURS 100 TIMES PIC X(10). *> Prices.
       01  DISPLAY-SUBTOTAL       OCCURS 100 TIMES PIC X(10). *> Subtotals.

       *> Working-storage holds data for processing.

       PROCEDURE DIVISION.
           OPEN INPUT CSV-FILE *> Open the CSV file for reading.
           PERFORM READ-CSV UNTIL EOF-FLAG = 'Y' *> Read until end of file.
           CLOSE CSV-FILE *> Close the file.

           DISPLAY "----------------------------------------"
           DISPLAY "               RECEIPT                  "
           DISPLAY "----------------------------------------"

           PERFORM PRINT-ITEMS *> Display item details.

           DISPLAY "----------------------------------------"
           DISPLAY "Subtotal: " SUBTOTAL-TOTAL
           DISPLAY "Discount: " DISCOUNT-TOTAL
           DISPLAY "Tax: " TAX-TOTAL
           DISPLAY "Grand Total: " GRANDTOTAL-TOTAL
           DISPLAY "----------------------------------------"
           DISPLAY "Thank you for shopping with us!"
           DISPLAY "----------------------------------------"

           STOP RUN. *> End the program.

       READ-CSV.
           READ CSV-FILE INTO CSV-RECORD *> Read a record from the file.
               AT END
                   MOVE 'Y' TO EOF-FLAG *> Set end-of-file flag.
               NOT AT END
                   PERFORM Parse-CSV-Record *> Process the record.
           END-READ.

       Parse-CSV-Record.
           UNSTRING CSV-RECORD DELIMITED BY "," INTO
               PRODUCT-NAME
               TEMP-QUANTITY
               TEMP-PRICE
               TEMP-SUBTOTAL *> Split the record.
           END-UNSTRING

           MOVE FUNCTION TRIM(PRODUCT-NAME) TO PRODUCT-NAME *> Trim name.
           MOVE FUNCTION NUMVAL(TEMP-QUANTITY) TO PRODUCT-QUANTITY *> Convert.
           MOVE FUNCTION TRIM(TEMP-PRICE) TO PRODUCT-PRICE *> Trim price.
           MOVE FUNCTION TRIM(TEMP-SUBTOTAL) TO PRODUCT-SUBTOTAL *>Trim subtotal

           IF PRODUCT-NAME = "Subtotal"
               MOVE PRODUCT-SUBTOTAL TO SUBTOTAL-TOTAL
           ELSE IF PRODUCT-NAME = "Discount"
               MOVE PRODUCT-SUBTOTAL TO DISCOUNT-TOTAL
           ELSE IF PRODUCT-NAME = "Tax"
               MOVE PRODUCT-SUBTOTAL TO TAX-TOTAL
           ELSE IF PRODUCT-NAME = "Grand Total"
               MOVE PRODUCT-SUBTOTAL TO GRANDTOTAL-TOTAL
           ELSE
               ADD 1 TO ITEM-COUNT *> Increment item count.
               MOVE PRODUCT-NAME TO DISPLAY-NAME (ITEM-COUNT)
               MOVE PRODUCT-QUANTITY TO DISPLAY-QUANTITY (ITEM-COUNT)
               MOVE PRODUCT-PRICE TO DISPLAY-PRICE (ITEM-COUNT)
               MOVE PRODUCT-SUBTOTAL TO DISPLAY-SUBTOTAL (ITEM-COUNT)
           END-IF.

       PRINT-ITEMS.
           PERFORM VARYING ITEM-INDEX FROM 1 BY 1
               UNTIL ITEM-INDEX > ITEM-COUNT *> Loop through items.
                  DISPLAY "Item: " DISPLAY-NAME (ITEM-INDEX)
                  DISPLAY "Quantity: " DISPLAY-QUANTITY (ITEM-INDEX)
                  DISPLAY "Price: " DISPLAY-PRICE (ITEM-INDEX)
                  DISPLAY "Subtotal: " DISPLAY-SUBTOTAL (ITEM-INDEX)
                  DISPLAY "" *> Blank line.
           END-PERFORM.
