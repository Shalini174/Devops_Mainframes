*******This program calculates the Tax applicxable, on the bases of tax slabs
      * and savings and other incomes provided in the input. **********
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STDRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INCOMEFILE ASSIGN TO INCMFILE
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.
           SELECT OUTPUTFL ASSIGN TO OUTFL
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INCOMEFILE
           RECORDING MODE IS F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 67 CHARACTERS
           DATA RECORD IS INCOMEDETAILS.
      *
       01  INCOMEDETAILS.
           02  PAN-NO        PIC X(7).
           02  FILLER        PIC X(1).
           02  SALARY        PIC 9(9)V99.
           02  FILLER        PIC X(1).
           02  OTH-INCOME    PIC 9(9)V99.
           02  FILLER        PIC X(1).
           02  SAVING        PIC 9(9)V99.
           02  FILLER        PIC X(1).
           02  HRA           PIC 9(9)V99.
           02  FILLER        PIC X(1).
           02  LTA           PIC 9(9)V99.

       FD  OUTPUTFL
           RECORDING MODE IS F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 19 CHARACTERS
           DATA RECORD IS OUTPUTDETAILS.
      *
       01  OUTPUTDETAILS.
           02 PAN-NO-O       PIC X(7).
           02 FILLER         PIC X(1).
           02 INCOME-TAX-O   PIC 9(9)V99.


       WORKING-STORAGE SECTION.

       01  MISCELLANEOUS.
           02  GROSS-SALARY     PIC 9(9)V99 VALUE ZERO.
           02  DED              PIC 9(9)V99 VALUE ZERO.
           02  TAX-SAL          PIC 9(9)V99 VALUE ZERO.
           02  INCOME-TAX       PIC 9(9)V99 VALUE ZERO.
           02  MAX-AMT          PIC 9(8)V99 VALUE 250000.00.
           02  WS-EOF-INP              PIC X(001)  VALUE ' '.
               88  WS-EOF-INP-YES                     VALUE 'Y'.

       PROCEDURE DIVISION.
       0001-MAIN.
           OPEN INPUT INCOMEFILE
                OUTPUT OUTPUTFL
           PERFORM UNTIL WS-EOF-INP-YES
               READ INCOMEFILE
                   AT END
                       SET WS-EOF-INP-YES TO TRUE
                   NOT AT END
                        PERFORM 0002-CALCULATE-TAX THRU 0002-EXIT
                   END-READ
           END-PERFORM


           PERFORM 0004-CLOSEFILE THRU 0004-EXIT.
         0001-EXIT.
           EXIT.
      *
       0002-CALCULATE-TAX.
      *
           IF SAVING > MAX-AMT
             MOVE MAX-AMT TO SAVING
           END-IF
           COMPUTE GROSS-SALARY=SALARY+OTH-INCOME
           COMPUTE DED=HRA+SAVING+LTA
           COMPUTE TAX-SAL=GROSS-SALARY - DED
           EVALUATE TRUE
             WHEN TAX-SAL <= 250000
               MOVE ZEROS TO INCOME-TAX
             WHEN TAX-SAL >250000 AND TAX-SAL <= 500000
               COMPUTE INCOME-TAX = 0.05*TAX-SAL
             WHEN TAX-SAL > 500000
               COMPUTE INCOME-TAX = 0.2*TAX-SAL
           END-EVALUATE.
           DISPLAY 'THE TAX CALCULATED IS: 'INCOME-TAX.
           PERFORM 0003-WRITE-OUTPUT THRU 0003-EXIT.
      *
         0002-EXIT.
      *
       0003-WRITE-OUTPUT.
      *
           MOVE PAN-NO TO PAN-NO-O.
           MOVE INCOME-TAX TO INCOME-TAX-O.
           WRITE OUTPUTDETAILS.
      *
       0003-EXIT.
      *
       0004-CLOSEFILE.
           CLOSE INCOMEFILE, OUTPUTFL
           GOBACK.
       0004-EXIT.
