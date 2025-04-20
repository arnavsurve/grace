       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPRINTEXPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-A PIC 9(3).
       01 GRACE-B PIC 9(2).

      *GRACE Compiler Helper Variables
       01 GRACE-TMP-INT PIC 9(6).

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE 100 TO GRACE-A.
           MOVE 23 TO GRACE-B.
           DISPLAY GRACE-TMP-INT.
           DISPLAY 20.
           DISPLAY GRACE-A.
           GOBACK.
