       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTLARGELITERALWIDTH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRACE-LARGE PIC 9(7).
       01 GRACE-MEDIUM PIC 9(6).
       01 GRACE-SMALL PIC 9(3).
       01 GRACE-TOOLARGE PIC 9(9).

       PROCEDURE DIVISION.
       MAIN SECTION.
           COMPUTE GRACE-SMALL = 123.
           COMPUTE GRACE-MEDIUM = 123456.
           COMPUTE GRACE-LARGE = 1234567.
           COMPUTE GRACE-TOOLARGE = 987654321.
           DISPLAY GRACE-SMALL.
           DISPLAY GRACE-MEDIUM.
           DISPLAY GRACE-LARGE.
           DISPLAY GRACE-TOOLARGE.
           GOBACK.
