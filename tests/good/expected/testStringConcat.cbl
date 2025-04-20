       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTRINGCONCAT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-S1 PIC X(6).
       01 GRACE-S2 PIC X(5).
       01 GRACE-S3 PIC X(11).
       01 GRACE-S4 PIC X(3).

      *GRACE Compiler Helper Variables
       01 GRACE-TMP-STR PIC X(256).

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE "Hello " TO GRACE-S1.
           MOVE "World" TO GRACE-S2.
           MOVE SPACES TO GRACE-S3.
           STRING GRACE-S1 DELIMITED BY SIZE
                  GRACE-S2 DELIMITED BY SIZE
               INTO GRACE-S3.
           DISPLAY GRACE-S1.
           DISPLAY GRACE-S3.
           DISPLAY GRACE-TMP-STR.
           MOVE SPACES TO GRACE-S2.
           STRING GRACE-S1 DELIMITED BY SIZE
                  "Again" DELIMITED BY SIZE
               INTO GRACE-S2.
           DISPLAY GRACE-S2.
           MOVE "ABC" TO GRACE-S4.
           DISPLAY GRACE-S4.
           GOBACK.
