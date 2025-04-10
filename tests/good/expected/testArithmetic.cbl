       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTARITHMETIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(6).
       01 B PIC 9(6).
       01 C PIC 9(6).
       01 D PIC 9(6).
       01 E PIC 9(6).
       01 F PIC 9(6).
       01 G PIC 9(6).
       01 H PIC 9(6).
       01 I PIC 9(6).

       PROCEDURE DIVISION.
           MOVE 10 TO A.
           MOVE 5 TO B.
           COMPUTE C = A + B.
           COMPUTE D = A - 3.
           COMPUTE E = B * 2.
           COMPUTE F = A / 2.
           DISPLAY C.
           DISPLAY D.
           DISPLAY E.
           DISPLAY F.
           COMPUTE G = A + (B * 2).
           DISPLAY G.
           COMPUTE H = (A + B) * 2.
           DISPLAY H.
           COMPUTE I = (A * B) + 2.
           DISPLAY I.
           STOP RUN.
