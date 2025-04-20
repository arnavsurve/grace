       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCNESTED.

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-INNER SECTION.
      *proc inner(): void
           DISPLAY "inside inner proc".
           EXIT SECTION.

       GRACE-OUTER SECTION.
      *proc outer(): void
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
