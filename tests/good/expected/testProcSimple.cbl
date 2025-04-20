       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCSIMPLE.

       PROCEDURE DIVISION.
       DECLARATIVES.
       GRACE-SAYHI SECTION.
      *proc sayHi(): void
           DISPLAY "hi from proc!".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           DISPLAY "calling proc...".
           PERFORM GRACE-SAYHI.
           DISPLAY "...proc called".
           GOBACK.
