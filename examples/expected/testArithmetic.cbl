       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTARITHMETIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-A PIC 9(2).
       01 GRACE-B PIC 9(1).
       01 GRACE-C PIC 9(3).
       01 GRACE-D PIC 9(3).
       01 GRACE-E PIC 9(2).
       01 GRACE-F PIC 9(2).
       01 GRACE-G PIC 9(3).
       01 GRACE-H PIC 9(4).
       01 GRACE-I PIC 9(4).

      *GRACE Compiler Helper Variables
       01 GRACE-TMP-INT-1 PIC S9(18).
       01 GRACE-TMP-INT-2 PIC S9(18).
       01 GRACE-TMP-DISPLAY PIC Z(17)9-.

       PROCEDURE DIVISION.
       MAIN SECTION.
           COMPUTE GRACE-A = 10.
           COMPUTE GRACE-B = 5.
           COMPUTE GRACE-C = GRACE-A + GRACE-B.
           COMPUTE GRACE-D = GRACE-A - 3.
           COMPUTE GRACE-E = GRACE-B * 2.
           COMPUTE GRACE-F = GRACE-A / 2.
           DISPLAY GRACE-C.
           DISPLAY GRACE-D.
           DISPLAY GRACE-E.
           DISPLAY GRACE-F.
           COMPUTE GRACE-G = GRACE-A + GRACE-B * 2.
           DISPLAY GRACE-G.
           COMPUTE GRACE-H = (GRACE-A + GRACE-B) * 2.
           DISPLAY GRACE-H.
           COMPUTE GRACE-I = GRACE-A * GRACE-B + 2.
           DISPLAY GRACE-I.
           GOBACK.
