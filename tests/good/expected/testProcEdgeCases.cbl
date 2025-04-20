       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCEDGECASES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-GVAL PIC 9(2).

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-EMPTY SECTION.
      *proc empty(): void
           EXIT SECTION.

       GRACE-SHOWVAL SECTION.
      *proc showVal(): void
           DISPLAY "Value in proc: ".
           DISPLAY GRACE-GVAL.
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           MOVE 42 TO GRACE-GVAL.
           PERFORM GRACE-EMPTY.
           DISPLAY "Global value: ".
           DISPLAY GRACE-GVAL.
           PERFORM GRACE-SHOWVAL.
           GOBACK.
