CBL TEST
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPGM.
      *****************************************************************
      *                                                               *
      *   This is the Test program  * ***  *                          *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 RCV-PARMS.
         05 WS-INPUT      PIC X(5).
         05 WS-OUTPUT     PIC X(5).
       PROCEDURE DIVISION USING RCV-PARMS.
       0001-MAIN.
           MOVE WS-INPUT TO WS-OUTPUT.
           DISPLAY WS-OUTPUT.
           GOBACK.
       END PROGRAM TESTPGM.
