       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPRINTEXPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(6).
       01 B PIC 9(6).
       01 GRACE-TMP-INT PIC 9(6).

       PROCEDURE DIVISION.

       MAIN SECTION.
           MOVE 100 TO A.
           MOVE 23 TO B.
           COMPUTE GRACE-TMP-INT = A + B.
           DISPLAY GRACE-TMP-INT.
           DISPLAY 20.
           DISPLAY A.
           GOBACK.
