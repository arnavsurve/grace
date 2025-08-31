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
       01 GRACE-TMP-INT-1 PIC S9(18).
       01 GRACE-TMP-INT-2 PIC S9(18).
       01 GRACE-TMP-DISPLAY PIC Z(17)9-.

       PROCEDURE DIVISION.
       MAIN SECTION.
           COMPUTE GRACE-CF-A = 20.
           COMPUTE GRACE-CF-B = 100.
           COMPUTE GRACE-CF-C = 100.
           COMPUTE GRACE-CF-P = (6) + (20).
           DISPLAY GRACE-CF-A.
           DISPLAY GRACE-CF-B.
           DISPLAY GRACE-CF-C.
           DISPLAY GRACE-CF-P.
           COMPUTE GRACE-X = 100.
           COMPUTE GRACE-H-Y = GRACE-X * 5.
           COMPUTE GRACE-H-Z = 5 * GRACE-X.
           DISPLAY GRACE-H-Y.
           DISPLAY GRACE-H-Z.
           COMPUTE GRACE-R = 10.
           COMPUTE GRACE-S = 20.
           COMPUTE GRACE-H-T = (GRACE-R + GRACE-S) * GRACE-R.
           DISPLAY GRACE-H-T.
           GOBACK.
