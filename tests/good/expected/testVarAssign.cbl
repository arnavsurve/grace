       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTVARASSIGN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DEST PIC 9(6).
       01 S_DEST PIC X(30).
       01 S_SOURCE PIC X(30).
       01 SOURCE PIC 9(6).

       PROCEDURE DIVISION.

       MAIN SECTION.
           MOVE 42 TO SOURCE.
           MOVE SOURCE TO DEST.
           DISPLAY DEST.
           MOVE "abc" TO S_SOURCE.
           MOVE S_SOURCE TO S_DEST.
           DISPLAY S_DEST.
           GOBACK.
