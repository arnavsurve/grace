       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTNESTEDEXPRESSION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(6).
       01 B PIC 9(6).
       01 C PIC 9(6).
       01 RESULT PIC 9(15).

       PROCEDURE DIVISION.

       MAIN SECTION.
           MOVE 5 TO A.
           MOVE 10 TO B.
           MOVE 15 TO C.
           COMPUTE RESULT = ((A + B) * (C - (A + (B / 2)))) / 2.
           DISPLAY "Result of complex expression: ".
           DISPLAY RESULT.
           GOBACK.
