       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCONST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING PIC X(30).
       01 MAX_VALUE PIC 9(6).

       PROCEDURE DIVISION.
           MOVE "Hello Const" TO GREETING.
           DISPLAY GREETING.
           MOVE 999 TO MAX_VALUE.
           DISPLAY MAX_VALUE.
           STOP RUN.
