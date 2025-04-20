       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTWIDTHINFERENCE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-CF-A PIC 9(2).
       01 GRACE-CF-B PIC 9(3).
       01 GRACE-CF-C PIC 9(3).
       01 GRACE-CF-P PIC 9(3).
       01 GRACE-H-T PIC 9(5).
       01 GRACE-H-Y PIC 9(4).
       01 GRACE-H-Z PIC 9(4).
       01 GRACE-R PIC 9(2).
       01 GRACE-S PIC 9(2).
       01 GRACE-X PIC 9(3).

      *GRACE Compiler Helper Variables
       01 GRACE-TMP-INT PIC 9(6).

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE 20 TO GRACE-CF-A.
           MOVE 100 TO GRACE-CF-B.
           MOVE 100 TO GRACE-CF-C.
           COMPUTE GRACE-CF-P = ( ( 6 ) ) + ( ( 20 ) ).
           DISPLAY GRACE-CF-A.
           DISPLAY GRACE-CF-B.
           DISPLAY GRACE-CF-C.
           DISPLAY GRACE-CF-P.
           MOVE 100 TO GRACE-X.
           COMPUTE GRACE-H-Y = GRACE-X * 5.
           COMPUTE GRACE-H-Z = 5 * GRACE-X.
           DISPLAY GRACE-H-Y.
           DISPLAY GRACE-H-Z.
           MOVE 10 TO GRACE-R.
           MOVE 20 TO GRACE-S.
           COMPUTE GRACE-H-T = ( ( GRACE-R + GRACE-S ) ) * GRACE-R.
           DISPLAY GRACE-H-T.
           GOBACK.
