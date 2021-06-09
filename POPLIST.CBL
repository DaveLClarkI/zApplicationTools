      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    POPLIST.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  MARCH 2008.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
      *REMARKS.       DISPLAY 3270 POP-UP LIST FOR SELECTION.
      *               A LINKED CONVERSATIONAL PROGRAM.

      * CHANGE HISTORY ------------------------------------------------
      * 03/31/2008 DLC ORIGINAL PROGRAM.
/DLC0/* 06/17/2020 DLC EXPAND SUPPORT TO 40-CHARACTER HEADER/TRAILER
/DLC0/*                AND 25-CHARACTER DETAIL DESCRIPTION FIELDS.
/DLC1/* 04/19/2021 DLC SEE IF ATCV ABEND CAN BE PREVENTED.
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
       OBJECT-COMPUTER. IBM-2096-N03.

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
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='$TR$'==
                                       =='PROGRAM'== BY =='POPLIST'==.

         03  HALF-WORD                 PIC S9(04)   BINARY.
         03  TWO-BYTES                 REDEFINES    HALF-WORD
                                       PIC  X(02).
         03  COL                       PIC S9(04)   BINARY.
         03  DTL                       PIC S9(04)   BINARY.
         03  HDR                       PIC S9(04)   BINARY.
         03  IDX                       PIC S9(04)   BINARY.
         03  LEN                       PIC S9(04)   BINARY.
         03  LST                       PIC S9(04)   BINARY.
         03  POS                       PIC S9(04)   BINARY.
         03  ROW                       PIC S9(04)   BINARY.
         03  SUB                       PIC S9(04)   BINARY.
         03  DFLT-CNT                  PIC S9(04)   BINARY VALUE +8.
/DLC0/*  03  DFLT-HDR                  PIC  X(35)   VALUE
/DLC0/   03  DFLT-HDR                  PIC  X(40)   VALUE
             '          Popup Pick List          '.
/DLC0/*  03  DFLT-TLR                  PIC  X(35)   VALUE
/DLC0/   03  DFLT-TLR                  PIC  X(40)   VALUE
             '  ENTER=Pick F3=Exit F7/F8=Scroll  '.
/DLC0/*  03  DFLT-KEYERR               PIC  X(35)   VALUE
/DLC0/   03  DFLT-KEYERR               PIC  X(40)   VALUE
             '   Unassigned attention key...     '.
/DLC0/*  03  DFLT-FIRST                PIC  X(35)   VALUE
/DLC0/   03  DFLT-FIRST                PIC  X(40)   VALUE
             '   This is the first page...       '.
/DLC0/*  03  DFLT-LAST                 PIC  X(35)   VALUE
/DLC0/   03  DFLT-LAST                 PIC  X(40)   VALUE
             '   This is the last page...        '.
/DLC0/*  03  DFLT-CURERR               PIC  X(35)   VALUE
/DLC0/   03  DFLT-CURERR               PIC  X(40)   VALUE
             '   The cursor is not in range...   '.
         03  TIOA                      PIC  X(1920).

       COPY HEXMAN.

/DLC1/ COPY LOGGING.

       COPY UNEXERRW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
         COPY POPLIST.

       COPY COMMTEMP.

       COPY IBM3270.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-INITIALIZATION.

           IF  NOT POPLIST-PGM
           OR  EIBCALEN <= LENGTH OF POPLIST-CONTROL
               IF  EIBCALEN < LENGTH OF POPLIST-CTL
                            + LENGTH OF POPLIST-RTC
                   EXEC CICS ABEND
                             ABCODE('ECOM')
                   END-EXEC
               END-IF
               SET  POPLIST-COMM-ERROR TO TRUE
               GO TO Z00-TERMINATION
           END-IF.

           COPY COMMMAPT.

           COPY COMM3270.

       B00-VERIFICATION.

           IF  POPLIST-ROW < 1
           OR  POPLIST-ROW > SCRNHGHT
           OR  POPLIST-ROW = 1 AND POPLIST-COL = 1
               SET  POPLIST-ROW-ERROR  TO TRUE
               GO TO Z00-TERMINATION
           END-IF.

           IF  POPLIST-CNT < DFLT-CNT
             IF  POPLIST-ROW > (SCRNHGHT - POPLIST-CNT - 1)
               COMPUTE POPLIST-ROW = SCRNHGHT - POPLIST-CNT - 1
             END-IF
           ELSE
             IF  POPLIST-ROW > (SCRNHGHT - DFLT-CNT - 1)
               COMPUTE POPLIST-ROW = SCRNHGHT - DFLT-CNT - 1
             END-IF
           END-IF.

           MOVE POPLIST-ROW            TO HDR.

           IF  POPLIST-COL < 1
           OR  POPLIST-COL > SCRNWDTH
               SET  POPLIST-COL-ERROR  TO TRUE
               GO TO Z00-TERMINATION
           END-IF.

           IF  POPLIST-COL > (SCRNWDTH - LENGTH OF DFLT-HDR)
               COMPUTE POPLIST-COL = SCRNWDTH - LENGTH OF DFLT-HDR
           END-IF.

           IF  POPLIST-CNT < 1
           OR  POPLIST-CNT > 99
           OR  EIBCALEN < LENGTH OF DFHCOMMAREA
               SET  POPLIST-CNT-ERROR  TO TRUE
               GO TO Z00-TERMINATION
           END-IF.

           IF  POPLIST-CNT <= DFLT-CNT
               MOVE POPLIST-CNT        TO DTL
           ELSE
               MOVE DFLT-CNT           TO DTL
           END-IF.
           COMPUTE LST = POPLIST-CNT - DTL + 1.

           IF  NOT POPLIST-BDR-BLUE
           AND NOT POPLIST-BDR-RED
           AND NOT POPLIST-BDR-PINK
           AND NOT POPLIST-BDR-GREEN
           AND NOT POPLIST-BDR-TURQU
           AND NOT POPLIST-BDR-YELLOW
           AND NOT POPLIST-BDR-WHITE
               SET  POPLIST-BDR-TURQU  TO TRUE
           END-IF.

           IF  NOT POPLIST-OPT-NORM
           AND NOT POPLIST-OPT-BRT
           AND NOT POPLIST-OPT-DARK
               SET  POPLIST-OPT-DARK   TO TRUE
           END-IF.

           IF  POPLIST-HDR <= SPACES
               MOVE DFLT-HDR        TO POPLIST-HDR
           END-IF.
           IF  POPLIST-TLR <= SPACES
               MOVE DFLT-TLR        TO POPLIST-TLR
           END-IF.

           MOVE 1                      TO POS.

      /*****************************************************************
      *    POPLIST ROUTINE                                             *
      ******************************************************************
       C00-POPLIST-ROUTINE.

           MOVE 1                      TO LEN.

      * popup header
           MOVE POPLIST-ROW            TO ROW.
           MOVE POPLIST-COL            TO COL.
           COMPUTE HALF-WORD = (ROW - 1) * SCRNWDTH
                             + (COL - 1) - 1.
           STRING SCRSETBA TWO-BYTES
                  SCRSTFEX X'03'
                  TYPFIELD FLDPRBRT
                  TYPHILIT EXHREVRS
                  TYPCOLOR POPLIST-BDR
                  POPLIST-HDR
                  SCRSTFLD POPLIST-OPT    DELIMITED BY SIZE
               INTO TIOA             WITH POINTER LEN.

      * popup detail
           MOVE 1                      TO IDX.
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM POS BY 1
               UNTIL SUB > POPLIST-CNT
                  OR IDX > DTL
             ADD  1                    TO ROW
             COMPUTE HALF-WORD = (ROW - 1) * SCRNWDTH
                               + (COL - 1) - 1
             STRING SCRSETBA TWO-BYTES
                    SCRSTFEX X'03'
                    TYPFIELD FLDPRBRT
                    TYPHILIT EXHREVRS
                    TYPCOLOR POPLIST-BDR X'4040'
                    SCRSTFEX X'03'
                    TYPFIELD FLDUNBRT
                    TYPHILIT EXHULINE
                    TYPCOLOR EXCYELLW     DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
             IF  IDX = 1
                 STRING SCRCURSR          DELIMITED BY SIZE
                     INTO TIOA       WITH POINTER LEN
             END-IF
             STRING X'00'
                    SCRSTFEX X'02'
                    TYPFIELD FLDPRBRT
                    TYPCOLOR EXCYELLW POPLIST-KEY(SUB)
                    SCRSTFEX X'02'
                    TYPFIELD FLDPROT
                    TYPCOLOR EXCGREEN POPLIST-DSC(SUB)
                    SCRSTFEX X'03'
                    TYPFIELD FLDPRBRT
                    TYPHILIT EXHREVRS
                    TYPCOLOR POPLIST-BDR X'4040'
                    SCRSTFLD POPLIST-OPT  DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
             ADD  1                    TO IDX
           END-PERFORM.

      * popup trailer
           ADD  1                      TO ROW.
           COMPUTE HALF-WORD = (ROW - 1) * SCRNWDTH
                             + (COL - 1) - 1.
           STRING SCRSETBA TWO-BYTES
                  SCRSTFEX X'03'
                  TYPFIELD FLDPRBRT
                  TYPHILIT EXHREVRS
                  TYPCOLOR POPLIST-BDR
                  POPLIST-TLR
                  SCRSTFLD POPLIST-OPT    DELIMITED BY SIZE
               INTO TIOA             WITH POINTER LEN.

           SUBTRACT 1                FROM LEN.

       C10-POPLIST-SEND.
/DLC1/     PERFORM WITH TEST BEFORE
/DLC1/       UNTIL EIBRECV NOT = HIGH-VALUES
/DLC1/         MOVE LENGTH OF COMM-HOLDAREA TO UNEX-LENG
/DLC1/         EXEC CICS RECEIVE
/DLC1/                   SET(ADDRESS OF COMM-HOLDAREA)
/DLC1/                   LENGTH(UNEX-LENG)
/DLC1/                   NOHANDLE
/DLC1/         END-EXEC
/DLC1/         IF  UNEX-LENG > ZERO
/DLC1/             SET HEX-DECODE-FROM-CHAR TO TRUE
/DLC1/             IF  UNEX-LENG > 32
/DLC1/                 MOVE 32         TO UNEX-LENG
/DLC1/             END-IF
/DLC1/             MOVE UNEX-LENG      TO HEX-LENG
/DLC1/             MOVE COMM-HOLDAREA(1:UNEX-LENG)
/DLC1/                                 TO HEX-CHRT
/DLC1/             CALL 'HEXMAN'    USING HEXMAN-PARMS
/DLC1/             MOVE 'RCVD='        TO LOGF-MESG
/DLC1/             MOVE HEX-HEXT       TO LOGF-MESG(6:)
/DLC1/             PERFORM Q100-LOGIT THRU Q199-EXIT
/DLC1/         END-IF
/DLC1/         MOVE 'RECEIVE COMPLETED.'
/DLC1/                                 TO LOGF-MESG
/DLC1/         PERFORM Q100-LOGIT    THRU Q199-EXIT
/DLC1/         EXEC CICS SUSPEND END-EXEC
/DLC1/     END-PERFORM.
/DLC1/     IF  EIBSYNC = HIGH-VALUES
/DLC1/         EXEC CICS SYNCPOINT NOHANDLE END-EXEC
/DLC1/         MOVE 'SYNCPOINT COMPLETED.'
/DLC1/                                 TO LOGF-MESG
/DLC1/         PERFORM Q100-LOGIT    THRU Q199-EXIT
/DLC1/     END-IF.

           EXEC CICS SEND
                     FROM(TIOA)
                     LENGTH(LEN)
                     CTLCHAR(WRTFKFRS)
           END-EXEC.

       C20-POPLIST-RECEIVE.

           EXEC CICS HANDLE ABEND LABEL(Y00-TIMEOUT) END-EXEC.
           EXEC CICS RECEIVE NOHANDLE                END-EXEC.
           EXEC CICS HANDLE ABEND CANCEL             END-EXEC.

       C30-POPLIST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               SET POPLIST-NO-ITEM-PICKED TO TRUE
               GO TO Z00-TERMINATION
           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               GO TO C40-POPLIST-PROCESS
           END-EVALUATE.

           PERFORM C90-POPLIST-SETERR.
           STRING DFLT-KEYERR             DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN.
           PERFORM C95-POPLIST-SENDERR.
           GO TO C00-POPLIST-ROUTINE.

       C40-POPLIST-PROCESS.

           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
           SUBTRACT HDR              FROM ROW.

      * scroll backward/forward by page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               IF  EIBAID = AIDPFK07
                 IF  POS = 1
                   PERFORM C50-POPLIST-FIRST
                 ELSE
                   IF  (ROW < 1 OR >= DTL)
                     SUBTRACT DTL    FROM POS
                   ELSE
                     COMPUTE POS = POS - (DTL - ROW)
                   END-IF
                   IF  POS < 1
                     MOVE 1            TO POS
                   END-IF
                 END-IF
               ELSE
                 IF  POS = LST
                   PERFORM C60-POPLIST-LAST
                 ELSE
                   IF  (ROW <= 1 OR > DTL)
                     ADD  DTL          TO POS
                   ELSE
                     COMPUTE POS = POS + (ROW - 1)
                   END-IF
                   IF  POS > LST
                     MOVE LST          TO POS
                   END-IF
                 END-IF
               END-IF
               GO TO C00-POPLIST-ROUTINE
           END-IF.

      * scroll to first/last page
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                 IF  POS = 1
                   PERFORM C50-POPLIST-FIRST
                 ELSE
                   MOVE 1              TO POS
                 END-IF
               ELSE
                 IF  POS = LST
                   PERFORM C60-POPLIST-LAST
                 ELSE
                   MOVE LST            TO POS
                 END-IF
               END-IF
               GO TO C00-POPLIST-ROUTINE
           END-IF.

      * select row by cursor position
           IF  1 <= ROW AND ROW <= DTL
               SET POPLIST-ITEM-PICKED TO TRUE
               COMPUTE POPLIST-PICK = POS + ROW - 1
               GO TO Z00-TERMINATION
           END-IF.

           PERFORM C90-POPLIST-SETERR.
           STRING DFLT-CURERR             DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
           PERFORM C95-POPLIST-SENDERR.

           GO TO C00-POPLIST-ROUTINE.

       C50-POPLIST-FIRST.

           PERFORM C90-POPLIST-SETERR.
           STRING DFLT-FIRST              DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
           PERFORM C95-POPLIST-SENDERR.

       C60-POPLIST-LAST.

           PERFORM C90-POPLIST-SETERR.
           STRING DFLT-LAST               DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
           PERFORM C95-POPLIST-SENDERR.

       C90-POPLIST-SETERR.

           MOVE 1                      TO LEN.
           IF  POPLIST-BDR = EXCRED
             STRING SCRSETBA TWO-BYTES
                    SCRSTFEX X'03'
                    TYPFIELD FLDPRBRT
                    TYPHILIT EXHREVRS
                    TYPCOLOR EXCYELLW     DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
           ELSE
             STRING SCRSETBA TWO-BYTES
                    SCRSTFEX X'03'
                    TYPFIELD FLDPRBRT
                    TYPHILIT EXHREVRS
                    TYPCOLOR EXCRED       DELIMITED BY SIZE
                 INTO TIOA           WITH POINTER LEN
           END-IF.

       C95-POPLIST-SENDERR.

           STRING SCRSTFLD POPLIST-OPT    DELIMITED BY SIZE
               INTO TIOA             WITH POINTER LEN
           SUBTRACT 1                FROM LEN.

           EXEC CICS SEND
                     FROM(TIOA)
                     LENGTH(LEN)
                     CTLCHAR(WRTFKFRS)
                     WAIT
           END-EXEC.

           EXEC CICS DELAY
                     INTERVAL(1)
                     NOHANDLE
           END-EXEC.

/DLC1//*****************************************************************
/DLC1/*    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
/DLC1/******************************************************************
/DLC1/ COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    TERMINAL READ TIMEOUT                                       *
      ******************************************************************
       Y00-TIMEOUT.

           EXEC CICS HANDLE ABEND CANCEL             END-EXEC.

           SET  POPLIST-READ-TIMEOUT   TO TRUE.

      /*****************************************************************
      *    PROGRAM TERMINATION (NORMAL)                                *
      ******************************************************************
       Z00-TERMINATION.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

