       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCNESTED.

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-INNER SECTION.
           DISPLAY "inside inner proc".
           EXIT SECTION.

       GRACE-OUTER SECTION.
           DISPLAY "outer proc start".
           PERFORM GRACE-INNER.
           DISPLAY "outer proc end".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           DISPLAY "main start".
           PERFORM GRACE-OUTER.
           DISPLAY "main end".
           GOBACK.
