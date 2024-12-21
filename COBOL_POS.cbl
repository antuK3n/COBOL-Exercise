       IDENTIFICATION DIVISION.
       PROGRAM-ID. POINT-OF-SALE-SYSTEM.

       *> This program is a simple Point-of-Sale (POS) system
       *> that allows users to add, view, edit, and delete product entries.
       *> It also writes the entries to a CSV file.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCT-FILE ASSIGN TO "PRODUCTS.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

       *> The file PRODUCTS.CSV is used to store product data sequentially.

       DATA DIVISION.
       FILE SECTION.
       FD  PRODUCT-FILE.
       01  PRODUCT-RECORD.
           05  PR-PRODUCT-NAME        PIC X(20). *> Stores the product name.
           05  PR-COMMA-1             PIC X     VALUE ','. *> Separator for CSV.
           05  PR-PRODUCT-QUANTITY    PIC 9(5). *> Stores the product quantity.
           05  PR-COMMA-2             PIC X     VALUE ','. *> Separator for CSV.
           05  PR-PRODUCT-PRICE       PIC 9(7). *> Stores the product price.

       *> This section defines the record structure for storing data in the CSV.

       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  MAX-ENTRIES            PIC 999   VALUE 100. *> Maximum entries.
           05  DECIMAL-SEPARATOR      PIC X     VALUE '.'. *> Price formatting.

       01  WS-PROMPTS.
           05  MENU-HEADER        PIC X(70)
               VALUE "OSPOS | POINT OF SALE MANAGEMENT SYSTEM". *> Header.
           05  MENU-OPTIONS.
               10  OPTION-ADD     PIC X(30)
                   VALUE "1. Add New Entry". *> New entry.
               10  OPTION-VIEW    PIC X(30)
                   VALUE "2. View Entries". *> Viewing entries.
               10  OPTION-EDIT    PIC X(30)
                   VALUE "3. Edit Entry". *> Editing an entry.
               10  OPTION-DELETE  PIC X(30)
                   VALUE "4. Delete Entry". *> Deleting an entry.
               10  OPTION-EXIT    PIC X(30)
                   VALUE "5. Exit System". *> Exiting the system.

       01  WS-ENTRY-DATA.
           05  WS-PRODUCT-DETAILS.
               10  WS-PRODUCT-NAME    PIC X(20). *> Product name.
               10  WS-QUANTITY        PIC 9(5). *> Quantity temporarily.
               10  WS-UNIT-PRICE      PIC 9(7). *> Product price temporarily.

       *> The working-storage section holds temporary and constant data used.

       01  WS-CONTROL-FLAGS.
           05  WS-MENU-CHOICE         PIC X. *> Stores the user's menu choice.
               88  CHOICE-ADD         VALUE "1". *> Add New Entry.
               88  CHOICE-VIEW        VALUE "2". *> View Entries.
               88  CHOICE-EDIT        VALUE "3". *> Edit Entry.
               88  CHOICE-DELETE      VALUE "4". *> Delete Entry.
               88  CHOICE-EXIT        VALUE "5". *> Exit System.
           05  WS-CONTINUE-FLAG       PIC X     VALUE "Y". *> Flag to continue.
               88  CONTINUE-ENTRY     VALUE "Y". *> Indicates continuation.
               88  STOP-ENTRY         VALUE "N". *> Indicates stopping.

       01  WS-ENTRY-MANAGEMENT.
           05  WS-ENTRY-COUNT         PIC 999   VALUE ZERO. *> Entries Tracker.
           05  WS-ENTRIES.
               10  WS-ENTRY           OCCURS 100 TIMES
                   INDEXED BY IDX-ENTRY. *> Stores up to 100 product entries.
                   15  WS-STORED-NAME     PIC X(20). *> Product name.
                   15  WS-STORED-QUANTITY PIC 9(5). *> Product quantity.
                   15  WS-STORED-PRICE    PIC 9(7). *> Product price.

       01  WS-EDIT-DELETE-VARS.
           05  WS-SELECTED-ENTRY      PIC 999.
           05  WS-CONFIRMATION        PIC X.
               88  CONFIRMED          VALUE "Y".
               88  NOT-CONFIRMED      VALUE "N".

       01  WS-STOP-FLAG               PIC X     VALUE "N".
       01  WS-NEW-PRODUCT-NAME        PIC X(20).
       01  WS-NEW-QUANTITY            PIC 9(5).
       01  WS-NEW-PRICE               PIC 9(7).

       *> Main processing logic begins here.
       PROCEDURE DIVISION.

       MAIN-PROCESSING.
           PERFORM MAIN-MENU-PROCESS
               UNTIL WS-MENU-CHOICE = "5". *> Loops until the user exits.

       MAIN-MENU-PROCESS.
           PERFORM DISPLAY-MENU
           PERFORM PROCESS-MENU-CHOICE.

       DISPLAY-MENU.
           DISPLAY SPACES
           DISPLAY MENU-HEADER
           DISPLAY SPACES
           DISPLAY MENU-OPTIONS
           DISPLAY "Enter your choice: "
           ACCEPT WS-MENU-CHOICE.

       *> Handles the user's menu choice.
       PROCESS-MENU-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN "1"
                   PERFORM ENTRY-ADDITION-ROUTINE
               WHEN "2"
                   PERFORM VIEW-ENTRIES-ROUTINE
               WHEN "3"
                   PERFORM EDIT-ENTRY-ROUTINE
               WHEN "4"
                   PERFORM DELETE-ENTRY-ROUTINE
               WHEN "5"
                   PERFORM WRITE-TO-CSV
               WHEN OTHER
                   DISPLAY "Invalid choice. Try again."
           END-EVALUATE.

       *> Routine to add new product entries.
       ENTRY-ADDITION-ROUTINE.
           MOVE "N" TO WS-STOP-FLAG
           PERFORM ADD-ENTRY-LOOP
               UNTIL WS-STOP-FLAG = "Y".

       ADD-ENTRY-LOOP.
           IF WS-ENTRY-COUNT < MAX-ENTRIES
               PERFORM CAPTURE-ENTRY-DETAILS
               PERFORM STORE-ENTRY
               PERFORM CONTINUE-ENTRY-PROMPT
           ELSE
               DISPLAY "Maximum entries reached!"
               MOVE "Y" TO WS-STOP-FLAG
           END-IF.

       CAPTURE-ENTRY-DETAILS.
           DISPLAY "Enter Product Name: "
           ACCEPT WS-PRODUCT-NAME

           DISPLAY "Enter Quantity: "
           ACCEPT WS-QUANTITY

           DISPLAY "Enter Unit Price: "
           ACCEPT WS-UNIT-PRICE.

       STORE-ENTRY.
           ADD 1 TO WS-ENTRY-COUNT
           MOVE WS-PRODUCT-NAME TO WS-STORED-NAME(WS-ENTRY-COUNT)
           MOVE WS-QUANTITY TO WS-STORED-QUANTITY(WS-ENTRY-COUNT)
           MOVE WS-UNIT-PRICE TO WS-STORED-PRICE(WS-ENTRY-COUNT).

       CONTINUE-ENTRY-PROMPT.
           DISPLAY "Add another entry? (Y/N): "
           ACCEPT WS-CONTINUE-FLAG
           IF WS-CONTINUE-FLAG = "N"
               MOVE "Y" TO WS-STOP-FLAG
           END-IF.

       *> Routine to view all product entries.
       VIEW-ENTRIES-ROUTINE.
           IF WS-ENTRY-COUNT = ZERO
               DISPLAY "No entries to display."
           ELSE
               PERFORM DISPLAY-ENTRY-HEADER
               PERFORM DISPLAY-ENTRIES
           END-IF.

       DISPLAY-ENTRY-HEADER.
           DISPLAY "-------------------------------------------"
           DISPLAY "NO PRODUCT NAME      QUANTITY   UNIT PRICE"
           DISPLAY "-------------------------------------------".

       DISPLAY-ENTRIES.
           PERFORM VARYING IDX-ENTRY FROM 1 BY 1
               UNTIL IDX-ENTRY > WS-ENTRY-COUNT
               DISPLAY IDX-ENTRY " "
                   WS-STORED-NAME(IDX-ENTRY)
                   "  "
                   WS-STORED-QUANTITY(IDX-ENTRY)
                   "  "
                   WS-STORED-PRICE(IDX-ENTRY)
           END-PERFORM.

       *> Routine to edit a product entry.
       EDIT-ENTRY-ROUTINE.
           PERFORM VIEW-ENTRIES-ROUTINE
           IF WS-ENTRY-COUNT > ZERO
               DISPLAY "Entry number to edit: "
               ACCEPT WS-SELECTED-ENTRY

               IF WS-SELECTED-ENTRY > 0 AND
                  WS-SELECTED-ENTRY <= WS-ENTRY-COUNT

                   DISPLAY "Current Product Name: "
                       WS-STORED-NAME(WS-SELECTED-ENTRY)
                   DISPLAY "Current Quantity: "
                       WS-STORED-QUANTITY(WS-SELECTED-ENTRY)
                   DISPLAY "Current Unit Price: "
                       WS-STORED-PRICE(WS-SELECTED-ENTRY)

                   DISPLAY "New Product Name (Enter to keep): "
                   ACCEPT WS-NEW-PRODUCT-NAME

                   IF WS-NEW-PRODUCT-NAME NOT = SPACES
                       MOVE WS-NEW-PRODUCT-NAME TO
                           WS-STORED-NAME(WS-SELECTED-ENTRY)
                   END-IF

                   DISPLAY "New Quantity (0 to keep): "
                   ACCEPT WS-NEW-QUANTITY

                   IF WS-NEW-QUANTITY NOT = ZERO
                       MOVE WS-NEW-QUANTITY TO
                           WS-STORED-QUANTITY(WS-SELECTED-ENTRY)
                   END-IF

                   DISPLAY "New Unit Price (0 to keep): "
                   ACCEPT WS-NEW-PRICE

                   IF WS-NEW-PRICE NOT = ZERO
                       MOVE WS-NEW-PRICE TO
                           WS-STORED-PRICE(WS-SELECTED-ENTRY)
                   END-IF

                   DISPLAY "Entry updated successfully!"
               ELSE
                   DISPLAY "Invalid entry number."
               END-IF
           END-IF.

       *> Routine to delete a product entry.
       DELETE-ENTRY-ROUTINE.
           PERFORM VIEW-ENTRIES-ROUTINE
           IF WS-ENTRY-COUNT > ZERO
               DISPLAY "Entry number to delete: "
               ACCEPT WS-SELECTED-ENTRY

               IF WS-SELECTED-ENTRY > 0 AND
                  WS-SELECTED-ENTRY <= WS-ENTRY-COUNT
                   DISPLAY "Confirm delete? (Y/N): "
                   ACCEPT WS-CONFIRMATION

                   IF WS-CONFIRMATION = "Y"
                       PERFORM SHIFT-ENTRIES-DOWN
                       SUBTRACT 1 FROM WS-ENTRY-COUNT
                       DISPLAY "Entry deleted successfully!"
                   END-IF
               ELSE
                   DISPLAY "Invalid entry number."
               END-IF
           END-IF.

       SHIFT-ENTRIES-DOWN.
           PERFORM VARYING IDX-ENTRY FROM WS-SELECTED-ENTRY BY 1
               UNTIL IDX-ENTRY >= WS-ENTRY-COUNT
               MOVE WS-STORED-NAME(IDX-ENTRY + 1)
                   TO WS-STORED-NAME(IDX-ENTRY)
               MOVE WS-STORED-QUANTITY(IDX-ENTRY + 1)
                   TO WS-STORED-QUANTITY(IDX-ENTRY)
               MOVE WS-STORED-PRICE(IDX-ENTRY + 1)
                   TO WS-STORED-PRICE(IDX-ENTRY)
           END-PERFORM.

       *> Routine to write all product entries to the CSV file.
       WRITE-TO-CSV.
           IF WS-ENTRY-COUNT > 0
               DISPLAY "Starting to write to CSV, Total Entries: "
                   WS-ENTRY-COUNT
               OPEN OUTPUT PRODUCT-FILE

               PERFORM VARYING IDX-ENTRY FROM 1 BY 1
                   UNTIL IDX-ENTRY > WS-ENTRY-COUNT

                   *> Write Product Name
                   MOVE WS-STORED-NAME(IDX-ENTRY)
                       TO PR-PRODUCT-NAME
                   WRITE PRODUCT-RECORD FROM PR-PRODUCT-NAME

                   *> Write Quantity
                   MOVE WS-STORED-QUANTITY(IDX-ENTRY)
                       TO PR-PRODUCT-QUANTITY
                   WRITE PRODUCT-RECORD FROM PR-PRODUCT-QUANTITY

                   *> Write Unit Price
                   MOVE WS-STORED-PRICE(IDX-ENTRY)
                       TO PR-PRODUCT-PRICE
                   WRITE PRODUCT-RECORD FROM PR-PRODUCT-PRICE

                   *> Write a blank line between entries for formatting
                   MOVE SPACES TO PRODUCT-RECORD
                   WRITE PRODUCT-RECORD
               END-PERFORM

               CLOSE PRODUCT-FILE
               DISPLAY "Completed writing all entries to CSV."
           ELSE
               DISPLAY "No entries to write to CSV."
           END-IF.

       STOP RUN.
