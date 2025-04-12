       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMULTIPLEPROCS.

       PROCEDURE DIVISION.
       DECLARATIVES.

       FIRST SECTION.
      *proc 'first()'
           DISPLAY "This is the first procedure".
           EXIT SECTION.
       SECOND SECTION.
      *proc 'second()'
           DISPLAY "This is the second procedure".
           EXIT SECTION.
       THIRD SECTION.
      *proc 'third()'
           DISPLAY "This is the third procedure".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           PERFORM FIRST.
           PERFORM SECOND.
           PERFORM THIRD.
           PERFORM FIRST.
           GOBACK.
