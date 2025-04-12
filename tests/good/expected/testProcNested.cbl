       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCNESTED.

       PROCEDURE DIVISION.
       DECLARATIVES.

       INNER SECTION.
      *proc 'inner()'
           DISPLAY "inside inner proc".
           EXIT SECTION.
       OUTER SECTION.
      *proc 'outer()'
           DISPLAY "outer proc start".
           PERFORM INNER.
           DISPLAY "outer proc end".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           DISPLAY "main start".
           PERFORM OUTER.
           DISPLAY "main end".
           GOBACK.
