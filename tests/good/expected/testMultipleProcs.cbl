       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMULTIPLEPROCS.

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-FIRST SECTION.
      *proc first(): void
           DISPLAY "This is the first procedure".
           EXIT SECTION.

       GRACE-SECOND SECTION.
      *proc second(): void
           DISPLAY "This is the second procedure".
           EXIT SECTION.

       GRACE-THIRD SECTION.
      *proc third(): void
           DISPLAY "This is the third procedure".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           PERFORM GRACE-FIRST.
           PERFORM GRACE-SECOND.
           PERFORM GRACE-THIRD.
           PERFORM GRACE-FIRST.
           GOBACK.
