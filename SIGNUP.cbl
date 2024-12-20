       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGNUP-TO-CSV.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT CSV-FILE ASSIGN TO 'USERS.csv'
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD.
           05  CSV-LINE        PIC X(200).

       WORKING-STORAGE SECTION.
       01  USER-USERNAME      PIC X(50) VALUE SPACES.
       01  USER-PASSWORD      PIC X(50) VALUE SPACES.
       01  USER-PIN           PIC 9(6) VALUE 0.
       01  USER-NAME          PIC X(50) VALUE SPACES.
       01  USER-EMAIL         PIC X(50) VALUE SPACES.
       01  USER-CONTACT       PIC 9(11) VALUE 0.
       01  TEMP-RECORD        PIC X(200).

       PROCEDURE DIVISION.
           OPEN OUTPUT CSV-FILE
           DISPLAY "----------------------------------------"
           DISPLAY "            SIGNUP FORM                "
           DISPLAY "----------------------------------------"

           PERFORM GET-USER-INPUT
           PERFORM WRITE-TO-CSV

           CLOSE CSV-FILE
           DISPLAY "----------------------------------------"
           DISPLAY "You have signed up successfully."
           DISPLAY "Head over to the POS app."
           DISPLAY "----------------------------------------"
           STOP RUN.

       GET-USER-INPUT.
           DISPLAY "Enter Username: "
           ACCEPT USER-USERNAME
           DISPLAY "Enter Password: "
           ACCEPT USER-PASSWORD
           DISPLAY "Enter 6-Digit PIN Code: "
           ACCEPT USER-PIN
           DISPLAY "Enter Full Name: "
           ACCEPT USER-NAME
           DISPLAY "Enter Email Address: "
           ACCEPT USER-EMAIL
           DISPLAY "Enter Contact Number (11 digits): "
           ACCEPT USER-CONTACT.

       WRITE-TO-CSV.
           MOVE USER-USERNAME TO CSV-LINE
           WRITE CSV-RECORD
           MOVE USER-PASSWORD TO CSV-LINE
           WRITE CSV-RECORD
           MOVE USER-PIN TO CSV-LINE
           WRITE CSV-RECORD
           MOVE USER-NAME TO CSV-LINE
           WRITE CSV-RECORD
           MOVE USER-EMAIL TO CSV-LINE
           WRITE CSV-RECORD
           MOVE USER-CONTACT TO CSV-LINE
           WRITE CSV-RECORD.
