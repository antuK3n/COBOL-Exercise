       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-INVENTORY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO 'INVENTORY.csv'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD.
           05  FULL-RECORD         PIC X(100).
       
       WORKING-STORAGE SECTION.
       01  EOF-FLAG               PIC X VALUE 'N'.
       01  CONTINUE-FLAG          PIC X VALUE 'Y'.
       01  SORT-CHOICE            PIC 9.
       01  USER-CHOICE            PIC X.
       01  WS-RECORD.
           05  WS-ID               PIC X(5).
           05  WS-NAME             PIC X(20).
           05  WS-CODE             PIC X(5).
           05  WS-AVAILABLE        PIC X(5).
           05  WS-PRICE            PIC X(10).
       01  ITEM-COUNT             PIC 9(3) VALUE 0.
       01  DISPLAY-ITEMS.
           05  ITEM-DISPLAY OCCURS 100 TIMES.
               10  DISPLAY-ID         PIC X(5).
               10  DISPLAY-NAME       PIC X(20).
               10  DISPLAY-CODE       PIC X(5).
               10  DISPLAY-AVAILABLE  PIC 9(5).
               10  DISPLAY-PRICE      PIC 9(5)V99.
       01  TEMP-ITEM.
           05  TEMP-ID             PIC X(5).
           05  TEMP-NAME           PIC X(20).
           05  TEMP-CODE           PIC X(5).
           05  TEMP-AVAILABLE      PIC 9(5).
           05  TEMP-PRICE          PIC 9(5)V99.
       01  DISPLAY-INDEX          PIC 9(3) VALUE 1.
       01  SORT-INDEX             PIC 9(3) VALUE 1.
       01  INNER-INDEX            PIC 9(3) VALUE 1.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-INVENTORY
           PERFORM PROCESS-INVENTORY 
               UNTIL CONTINUE-FLAG = 'N'.
           STOP RUN.

       INITIALIZE-INVENTORY.
           MOVE 'N' TO EOF-FLAG
           MOVE 0 TO ITEM-COUNT
           OPEN INPUT CSV-FILE
           PERFORM READ-CSV UNTIL EOF-FLAG = 'Y'
           CLOSE CSV-FILE.

       PROCESS-INVENTORY.
           PERFORM DISPLAY-UNSORTED-INVENTORY
           PERFORM GET-SORT-CHOICE
           PERFORM SORT-INVENTORY
           PERFORM ASK-CONTINUE.

       DISPLAY-UNSORTED-INVENTORY.
           DISPLAY "----------------------------------------"
           DISPLAY "         CURRENT INVENTORY             "
           DISPLAY "----------------------------------------"
           PERFORM VARYING DISPLAY-INDEX FROM 1 BY 1 
               UNTIL DISPLAY-INDEX > ITEM-COUNT
               DISPLAY "ID: " DISPLAY-ID(DISPLAY-INDEX)
               DISPLAY "Name: " DISPLAY-NAME(DISPLAY-INDEX)
               DISPLAY "Code: " DISPLAY-CODE(DISPLAY-INDEX)
               DISPLAY "Available: " 
                   DISPLAY-AVAILABLE(DISPLAY-INDEX)
               DISPLAY "Price: Php " 
                   DISPLAY-PRICE(DISPLAY-INDEX)
               DISPLAY " "
           END-PERFORM
           DISPLAY "Total Unique Products: " ITEM-COUNT.

       GET-SORT-CHOICE.
           DISPLAY "Choose Sort Option:"
           DISPLAY "1. Sort by Name"
           DISPLAY "2. Sort by ID"
           DISPLAY "3. Sort by Price (Low to High)"
           DISPLAY "4. Sort by Price (High to Low)"
           ACCEPT SORT-CHOICE.

       SORT-INVENTORY.
           EVALUATE SORT-CHOICE
               WHEN 1 PERFORM SORT-BY-NAME
               WHEN 2 PERFORM SORT-BY-ID
               WHEN 3 PERFORM SORT-BY-PRICE-ASC
               WHEN 4 PERFORM SORT-BY-PRICE-DESC
               WHEN OTHER 
                   DISPLAY "Invalid choice. No sorting applied."
           END-EVALUATE
           
           DISPLAY "----------------------------------------"
           DISPLAY "         SORTED INVENTORY              "
           DISPLAY "----------------------------------------"
           PERFORM DISPLAY-SORTED-INVENTORY.

       ASK-CONTINUE.
           DISPLAY "Continue? (Y/N):"
           ACCEPT USER-CHOICE
           MOVE FUNCTION UPPER-CASE(USER-CHOICE) TO USER-CHOICE
           IF USER-CHOICE = 'N'
               MOVE 'N' TO CONTINUE-FLAG
           END-IF.

       READ-CSV.
           READ CSV-FILE
               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   PERFORM PARSE-RECORD
           END-READ.

       PARSE-RECORD.
           UNSTRING FULL-RECORD DELIMITED BY ',' INTO
               WS-ID WS-NAME WS-CODE WS-AVAILABLE WS-PRICE
           
           ADD 1 TO ITEM-COUNT
           MOVE WS-ID TO DISPLAY-ID(ITEM-COUNT)
           MOVE FUNCTION TRIM(WS-NAME) 
               TO DISPLAY-NAME(ITEM-COUNT)
           MOVE WS-CODE TO DISPLAY-CODE(ITEM-COUNT)
           MOVE FUNCTION NUMVAL(WS-AVAILABLE) 
               TO DISPLAY-AVAILABLE(ITEM-COUNT)
           MOVE FUNCTION NUMVAL(WS-PRICE) 
               TO DISPLAY-PRICE(ITEM-COUNT).

       SORT-BY-NAME.
           PERFORM VARYING SORT-INDEX FROM 1 BY 1 
               UNTIL SORT-INDEX >= ITEM-COUNT
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= ITEM-COUNT
                   IF DISPLAY-NAME(INNER-INDEX) > 
                      DISPLAY-NAME(INNER-INDEX + 1)
                       PERFORM SWAP-ITEMS
                   END-IF
               END-PERFORM
           END-PERFORM.

       SORT-BY-ID.
           PERFORM VARYING SORT-INDEX FROM 1 BY 1 
               UNTIL SORT-INDEX >= ITEM-COUNT
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= ITEM-COUNT
                   IF DISPLAY-ID(INNER-INDEX) > 
                      DISPLAY-ID(INNER-INDEX + 1)
                       PERFORM SWAP-ITEMS
                   END-IF
               END-PERFORM
           END-PERFORM.

       SORT-BY-PRICE-ASC.
           PERFORM VARYING SORT-INDEX FROM 1 BY 1 
               UNTIL SORT-INDEX >= ITEM-COUNT
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= ITEM-COUNT
                   IF DISPLAY-PRICE(INNER-INDEX) > 
                      DISPLAY-PRICE(INNER-INDEX + 1)
                       PERFORM SWAP-ITEMS
                   END-IF
               END-PERFORM
           END-PERFORM.

       SORT-BY-PRICE-DESC.
           PERFORM VARYING SORT-INDEX FROM 1 BY 1 
               UNTIL SORT-INDEX >= ITEM-COUNT
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= ITEM-COUNT
                   IF DISPLAY-PRICE(INNER-INDEX) < 
                      DISPLAY-PRICE(INNER-INDEX + 1)
                       PERFORM SWAP-ITEMS
                   END-IF
               END-PERFORM
           END-PERFORM.

       SWAP-ITEMS.
           MOVE DISPLAY-ID(INNER-INDEX) TO TEMP-ID
           MOVE DISPLAY-NAME(INNER-INDEX) TO TEMP-NAME
           MOVE DISPLAY-CODE(INNER-INDEX) TO TEMP-CODE
           MOVE DISPLAY-AVAILABLE(INNER-INDEX) TO TEMP-AVAILABLE
           MOVE DISPLAY-PRICE(INNER-INDEX) TO TEMP-PRICE

           MOVE DISPLAY-ID(INNER-INDEX + 1) 
               TO DISPLAY-ID(INNER-INDEX)
           MOVE DISPLAY-NAME(INNER-INDEX + 1) 
               TO DISPLAY-NAME(INNER-INDEX)
           MOVE DISPLAY-CODE(INNER-INDEX + 1) 
               TO DISPLAY-CODE(INNER-INDEX)
           MOVE DISPLAY-AVAILABLE(INNER-INDEX + 1) 
               TO DISPLAY-AVAILABLE(INNER-INDEX)
           MOVE DISPLAY-PRICE(INNER-INDEX + 1) 
               TO DISPLAY-PRICE(INNER-INDEX)

           MOVE TEMP-ID TO DISPLAY-ID(INNER-INDEX + 1)
           MOVE TEMP-NAME TO DISPLAY-NAME(INNER-INDEX + 1)
           MOVE TEMP-CODE TO DISPLAY-CODE(INNER-INDEX + 1)
           MOVE TEMP-AVAILABLE TO DISPLAY-AVAILABLE(INNER-INDEX + 1)
           MOVE TEMP-PRICE TO DISPLAY-PRICE(INNER-INDEX + 1).

       DISPLAY-SORTED-INVENTORY.
           PERFORM VARYING DISPLAY-INDEX FROM 1 BY 1 
               UNTIL DISPLAY-INDEX > ITEM-COUNT
               DISPLAY "ID: " DISPLAY-ID(DISPLAY-INDEX)
               DISPLAY "Name: " DISPLAY-NAME(DISPLAY-INDEX)
               DISPLAY "Code: " DISPLAY-CODE(DISPLAY-INDEX)
               DISPLAY "Available: " 
                   DISPLAY-AVAILABLE(DISPLAY-INDEX)
               DISPLAY "Price: Php " 
                   DISPLAY-PRICE(DISPLAY-INDEX)
               DISPLAY " "
           END-PERFORM.