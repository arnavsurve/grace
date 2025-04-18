       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTRINGCONCAT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 S1 PIC X(30).
       01 S2 PIC X(30).
       01 S3 PIC X(60).
       01 S4 PIC X(30).
       01 GRACE-TMP-STR PIC X(256).

       PROCEDURE DIVISION.

       MAIN SECTION.
           MOVE "Hello " TO S1.
           MOVE "World" TO S2.
           STRING S1 DELIMITED BY SIZE
           S2 DELIMITED BY SIZE
           INTO S3.
           DISPLAY S1.
           DISPLAY S3.
           STRING "Direct: " DELIMITED BY SIZE
           S2 DELIMITED BY SIZE
           "!" DELIMITED BY SIZE
           INTO GRACE-TMP-STR.
           DISPLAY GRACE-TMP-STR.
           STRING S1 DELIMITED BY SIZE
           "Again" DELIMITED BY SIZE
           INTO S2.
           DISPLAY S2.
           MOVE "ABC" TO S4.
           DISPLAY S4.
           GOBACK.
