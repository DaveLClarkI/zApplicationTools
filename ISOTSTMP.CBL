      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    ISOTSTMP.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JUNE 2021.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      BATCH.
      *REMARKS.       RETURN ISO TIMESTAMP TO CALLER.
      *  1. RETURN CCYY-MM-DD-HH.MN.SS.UUUUUU TO CALLER.
      *  2. OPTIONALLY, RETURN CCYYMMDDHHMNSSUUUUUU TO CALLER.

      * CHANGE HISTORY ------------------------------------------------
      * 06/09/2021 DLC ORIGINAL DESIGN.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'ISOTSTMP'.
         03  ADJUSTMENT-DAYS           PIC S9(09)   COMP VALUE ZEROES.

       COPY ABSTIME.

       COPY DATETIME.

       COPY DTEMAN.

      ******************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       COPY ISOTSTMP.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION           USING ISOTSTMP-PARMS.

           CALL 'ASKTIME'           USING ABS-INPUT.
           CALL 'ABSTIME'           USING ABSTIME-PARMS.

           SET  DTE-REQUEST-GREG-EDIT  TO TRUE.
           MOVE 19000101               TO DTE-GNUM.
           CALL 'DTEMAN'            USING DTEMAN-PARMS.
           MOVE DTE-TOTD               TO ADJUSTMENT-DAYS.

           SET  DTE-REQUEST-TOTL-DAYS  TO TRUE.
           COMPUTE DTE-TOTD =  ABS-DAYS + ADJUSTMENT-DAYS.
           CALL 'DTEMAN'            USING DTEMAN-PARMS.

           COMPUTE NUM-TIME = (ABS-HOURS * 10000)
                            + (ABS-MINUTES * 100)
                            +  ABS-SECONDS.

           IF  NUM-FORMAT
               MOVE DTE-GNUM           TO NUM-DATE-FORMAT
               MOVE NUM-TIME           TO NUM-TIME-FORMAT
               MOVE ABS-UNITS          TO NUM-MILLISECONDS
           ELSE
               MOVE DTE-GNUM           TO ISO-DSP-DATE
               INSPECT ISO-DSP-DATE REPLACING ALL SPACES BY '-'
               MOVE ISO-DSP-DATE       TO ISO-DATE-FORMAT
               MOVE '-'                TO ISO-DATE-DELIM
               MOVE NUM-TIME           TO ISO-DSP-TIME
               INSPECT ISO-DSP-TIME REPLACING ALL SPACES BY '.'
               MOVE ISO-DSP-TIME       TO ISO-TIME-FORMAT
               MOVE '.'                TO ISO-TIME-DELIM
               MOVE ABS-UNITS          TO ISO-MILLISECONDS
           END-IF.

           GOBACK.

