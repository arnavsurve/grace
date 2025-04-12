       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCEDGECASES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GVAL PIC 9(6).

       PROCEDURE DIVISION.
       DECLARATIVES.

       EMPTY SECTION.
      *proc 'empty()'
           EXIT SECTION.
       SHOWVAL SECTION.
      *proc 'showVal()'
           DISPLAY "Value in proc: ".
           DISPLAY GVAL.
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           MOVE 42 TO GVAL.
           PERFORM EMPTY.
           DISPLAY "Global value: ".
           DISPLAY GVAL.
           PERFORM SHOWVAL.
           GOBACK.
