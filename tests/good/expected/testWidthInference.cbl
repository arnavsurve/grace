       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTWIDTHINFERENCE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CF_A PIC 9(6).
       01 CF_B PIC 9(6).
       01 CF_C PIC 9(6).
       01 CF_P PIC 9(6).
       01 H_T PIC 9(13).
       01 H_Y PIC 9(12).
       01 H_Z PIC 9(12).
       01 R PIC 9(6).
       01 S PIC 9(6).
       01 X PIC 9(6).

       PROCEDURE DIVISION.
           MOVE 20 TO CF_A.
           MOVE 100 TO CF_B.
           MOVE 100 TO CF_C.
           MOVE 26 TO CF_P.
           DISPLAY CF_A.
           DISPLAY CF_B.
           DISPLAY CF_C.
           DISPLAY CF_P.
           MOVE 100 TO X.
           COMPUTE H_Y = X * 5.
           COMPUTE H_Z = 5 * X.
           DISPLAY H_Y.
           DISPLAY H_Z.
           MOVE 10 TO R.
           MOVE 20 TO S.
           COMPUTE H_T = (R + S) * R.
           DISPLAY H_T.
           STOP RUN.
