      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    DFHFUNCX.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  NOV 2009.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       TRANSLATE EIBFN TO EQUIVALENT TEXT STRING.

      * CHANGE HISTORY ------------------------------------------------
      * 11/24/2009 DLC ORIGINAL PROGRAM.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-2086-A04-140.
       OBJECT-COMPUTER. IBM-2086-A04-140.

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

       01  CONTROL-FIELDS.
         03  THIS-PGM                  PIC  X(08)   VALUE 'DFHFUNCX'.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOVALUE                   PIC  X(01)   VALUE LOW-VALUE.
         03  DFHFUNCT                  PIC  X(08)   VALUE 'DFHFUNCT'.
         03  LN                        PIC S9(04)   BINARY.
         03  SB                        PIC S9(04)   BINARY.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         03  COMM-HEADER.
           05  COMM-FUNC               PIC S9(04)   BINARY.
           05  COMM-FUNCL              PIC S9(04)   BINARY.
         03  COMM-FUNCX                PIC  X(20).

      * EIBFN LITERALS TABLE
       01  EIBFN-TABLE.
         03  EIBFN-COUNT               PIC S9(04)   BINARY.
         03  EIBFN-ENTRY               OCCURS 1 TO 500
                                       DEPENDING ON EIBFN-COUNT.
           05  EIBFN-VALUE             PIC S9(04)   BINARY.
           05  EIBFN-TEXL              PIC S9(04)   BINARY.
           05  EIBFN-TEXT              PIC  X(20).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

           IF  EIBCALEN < LENGTH OF DFHCOMMAREA
               IF  EIBCALEN > LENGTH OF COMM-HEADER
                   MOVE +7             TO COMM-FUNCL
                   COMPUTE LN          =  EIBCALEN
                                       -  LENGTH OF COMM-HEADER
                   MOVE '#CALERR'      TO COMM-FUNCX(1:LN)
               ELSE
                   EXEC CICS ABEND
                             ABCODE('CALN')
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS LOAD
                         PROGRAM(DFHFUNCT)
                         SET(ADDRESS OF EIBFN-TABLE)
                         HOLD
               END-EXEC
               PERFORM WITH TEST BEFORE
                 VARYING SB FROM 1 BY 1
                   UNTIL SB > EIBFN-COUNT
                      OR EIBFN-VALUE(SB) = COMM-FUNC
               END-PERFORM
               IF  SB > EIBFN-COUNT
                   MOVE +7             TO COMM-FUNCL
                   MOVE '@NOTFND'      TO COMM-FUNCX
               ELSE
                   MOVE EIBFN-TEXL(SB) TO COMM-FUNCL
                   MOVE EIBFN-TEXT(SB) TO COMM-FUNCX
               END-IF
               EXEC CICS RELEASE
                         PROGRAM(DFHFUNCT)
               END-EXEC
           END-IF.

           EXEC CICS RETURN END-EXEC.
           GOBACK.
