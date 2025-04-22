       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMULTIPLEPROCS.

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-FIRST SECTION.
           DISPLAY "This is the first procedure".
           EXIT SECTION.

       GRACE-SECOND SECTION.
           DISPLAY "This is the second procedure".
           EXIT SECTION.

       GRACE-THIRD SECTION.
           DISPLAY "This is the third procedure".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           PERFORM GRACE-FIRST.
           PERFORM GRACE-SECOND.
           PERFORM GRACE-THIRD.
           PERFORM GRACE-FIRST.
           GOBACK.
