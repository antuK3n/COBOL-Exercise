       IDENTIFICATION DIVISION.
       PROGRAM-ID. CallBatchFile.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BATCH-COMMAND        PIC X(100) VALUE SPACES.
       01  USER-CHOICE          PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Choose an option:"
           DISPLAY "1 - Clear Inventory"
           DISPLAY "2 - Delete Account"
           DISPLAY "3 - Exit Program"
           DISPLAY "Enter your choice (1, 2, or 3): " WITH NO ADVANCING.
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
               WHEN 1
                   MOVE "cmd /c inventory_delete.bat" TO BATCH-COMMAND
                   CALL "SYSTEM" USING BATCH-COMMAND
                   DISPLAY "Inventory batch file executed successfully."
               WHEN 2
                   MOVE "cmd /c account_delete.bat" TO BATCH-COMMAND
                   CALL "SYSTEM" USING BATCH-COMMAND
                   DISPLAY "Account batch file executed successfully."
               WHEN 3
                   DISPLAY "Exiting program."
               WHEN OTHER
                   DISPLAY "Invalid choice. Please restart the program."
           END-EVALUATE.

           STOP RUN.
