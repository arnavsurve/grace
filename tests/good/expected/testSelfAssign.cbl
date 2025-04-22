       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSELFASSIGN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-I PIC 9(1).
       01 GRACE-X PIC X(5).

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE "start" TO GRACE-X.
      *Self-assignment of GRACE-X - skipping MOVE
           DISPLAY GRACE-X.
           COMPUTE GRACE-I = 5.
      *Self-assignment of GRACE-I - skipping MOVE
           DISPLAY GRACE-I.
           GOBACK.
