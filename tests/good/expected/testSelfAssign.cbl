       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSELFASSIGN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(6).
       01 X PIC X(30).

       PROCEDURE DIVISION.

       MAIN SECTION.
           MOVE "start" TO X.
           MOVE X TO X.
           DISPLAY X.
           MOVE 5 TO I.
           MOVE I TO I.
           DISPLAY I.
           GOBACK.
