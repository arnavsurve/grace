       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROCSIMPLE.

       PROCEDURE DIVISION.
       DECLARATIVES.

       SAYHI SECTION.
      *proc 'sayHi()'
           DISPLAY "hi from proc!".
           EXIT SECTION.

       END DECLARATIVES.

       MAIN SECTION.
           DISPLAY "calling proc...".
           PERFORM SAYHI.
           DISPLAY "...proc called".
           GOBACK.
