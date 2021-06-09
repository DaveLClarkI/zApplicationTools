      *
      * THIS IS A TEMPLATE PROGRAM.  AS SUCH, PLEASE
      * DO !!!NOT!!! DIRECTLY MODIFY THIS PROGRAM!!!
      *
      * INSTEAD, COPY THIS PROGRAM TO A NEW NAME WHICH
      * REFLECTS YOUR PROGRAM'S ACTUAL TRANSACTION ID.
      *
      * THE NAMING CONVENTION IS: $$$$PGM
      * WHERE: $$$$ IS YOUR 4-CHARACTER TRANSACTION ID.
      *
      * AFTER COPYING, MAKE THE FOLLOWING MASS CHANGES:
      *
      * CHANGE: $TR$ TO YOUR 4-CHARACTER TRANSACTION ID.
      * CHANGE: FML  TO YOUR 3-CHARACTER INITIALS.
      * CHANGE: $F1$ TO YOUR 4-CHARACTER PRIMARY FUNCTION CODE.
      * CHANGE: $F2$ TO YOUR 4-CHARACTER SECONDARY FUNCTION CODE.
      * CHANGE: $LINK$ TO YOUR DEFAULT 2ND-LEVEL LINK TRAN/PGM NAME.
      * CHANGE: MYFILE1 TO THE MASTER DLBL NAME ACCESSED BY THIS PGM.
      * CHANGE: MYFILE2 TO THE DETAIL DLBL NAME ACCESSED BY THIS PGM.
      * CHANGE: PFX1 TO DATANAME PREFIX ASSIGNED TO THE MASTER FILE.
      * CHANGE: PFX2 TO DATANAME PREFIX ASSIGNED TO THE DETAIL FILE.
      * CHANGE: SKIPTO-KEY TO THE NAME OF YOUR HEADER KEY FIELD
      *
      * LASTLY, MANUALLY CHANGE EVERYTHING IN <<...>> APPROPRIATELY.
      *
      * THEN, READ ALL OTHER COMMENTS FOR SUGGESTIONS AS TO WHERE TO
      * PLACE ADDITIONAL CODE NEEDED FOR YOUR PARTICULAR PROGRAM.
      *

      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    $TR$PGM.
       AUTHOR.        << YOUR NAME >>
       DATE-WRITTEN.  << MONTH & YEAR >>
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
           << pick your security or create your own >>
       SECURITY.      CICS TRANSACTION SECURITY, ONLY.
       SECURITY.      INTERNAL PROGRAM SECURITY.
       SECURITY.      INTERNAL, VIA WINSCRTY.
           << >>
      *REMARKS.       << TEMPLATE COMMAND-LEVEL CICS PROGRAM. >>

      * CHANGE HISTORY ------------------------------------------------
      * 09/29/2006 FML ORIGINAL PROGRAM.
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
      *
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='$TR$'==
                                       =='PROGRAM'== BY =='$TR$PGM'==
                                       =='LINKPGM'== BY =='$LINK$'==.
         03  $TR$MS                    PIC  X(08)   VALUE '$TR$MS  '.
         03  $F1$MAP                   PIC  X(08)   VALUE '$F1$MAP '.
         03  $F2$MAP                   PIC  X(08)   VALUE '$F2$MAP '.
         03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  MYFILE1                   PIC  X(08)   VALUE 'MYFILE1 '.
         03  MYFILE2                   PIC  X(08)   VALUE 'MYFILE2 '.
         03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  WINSCRTY                  PIC  X(08)   VALUE 'WINSCRTY'.
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  $F1$HEAD                  PIC S9(04)   BINARY VALUE +08.
         03  $F1$ROWS                  PIC S9(04)   BINARY VALUE +16.
         03  $F2$HEAD                  PIC S9(04)   BINARY VALUE +08.
         03  $F2$ROWS                  PIC S9(04)   BINARY VALUE +16.
      *
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.

       COPY DATETIME.
       COPY BINARIES.

      * PLACE YOUR OWN 01-LEVEL AREAS HERE

       << ??? >>

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY CICSSORT.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY LOGGING.

       COPY NUMMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).

       01  WINSCRTY-COMMAREA.
         COPY WINSCRTY.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * $TR$ CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  $TR$-SAVEKEY            PIC  X(8).
      *
           05  $TR$-USERID             PIC  X(8).
           05  $TR$-COMPNUM            PIC  9(5).
           05  $TR$-COMP-NO5 REDEFINES $TR$-COMPNUM.
             07  $TR$-COMP-PFX         PIC  XX.
             07  $TR$-COMP-NO3         PIC  XXX.
      *
      * FIELDS TO MANAGE INTERNAL PROGRAM SECURITY BY COMPANY
           05  $TR$-SECU-CODE          PIC  X.
             88  SECU-DAPSCO-INC                    VALUE 'I'.
             88  SECU-LCL-WGS-WSS                   VALUE 'D'.
             88  SECU-DISTRO-INC                    VALUE 'O'.
             88  SECU-VICE-PRESIDENT                VALUE 'V'.
             88  SECU-RVP-CENTRAL                   VALUE 'C'.
             88  SECU-RVP-EASTERN                   VALUE 'E'.
             88  SECU-RVP-WESTERN                   VALUE 'W'.
             88  SECU-AC-AL                         VALUE 'A'.
             88  SECU-LOCAL-WIN-CO                  VALUE 'L'.
           05  $TR$-SECU-COMP          PIC  X(05).
           05  SAVE-SECURITY.
             07  SAVE-SECU-GRP         PIC  X(02).
             07  SAVE-SECU-LVL         PIC  9(01).
             07  SAVE-SECU-COMP        PIC  X(05).
      *
      * ADD YOUR OWN ADDITIONAL COMMAREA FIELDS HERE

           << ??? >>

           05  TS-QUEUE.
             07  TS-TRAN               PIC  X(4).
             07  TS-TERM               PIC  X(4).
           05  TS-TOTL                 PIC S9(4)    BINARY.
           05  TS-ITEM                 PIC S9(4)    BINARY.
           05  SS-ITEM                 PIC S9(4)    BINARY.
           05  TS-RECD                 PIC  X(80).

           05  $TR$-$F1$-TIOA          PIC  X(3072).
           05  $TR$-SAVE-TIOA          PIC  X(2048).

      * $TR$ TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * MYFILE1 RECORD I/O AREA
       COPY MYFILE1.

      * MYFILE2 RECORD I/O AREA
       COPY MYFILE2.

      * VUSERID RECORD I/O AREA
       COPY VUSERID.

      * BMS TERMINAL I/O AREAS
       COPY $TR$MS.

      *
      * ADD YOUR OWN ADDITIONAL LINKAGE AREAS HERE

       << ??? >>

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-$TR$-PROGRAM-ENTRY.

           IF  EIBTRNID = ALT2-TRN
               MOVE ALT2-TRN           TO THIS-TRN
           END-IF.

           COPY COMMENTR.

           EXEC CICS LINK
                     PROGRAM  ('CICSINFO')
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.
           MOVE CICS-FULLDATE          TO WRK-DSP-DATE.
           MOVE CICS-FULLTIME          TO WRK-DSP-TIME.
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.

           COPY COMMMAPT.

           COPY COMM3270.

           EXEC CICS HANDLE CONDITION
                     DISABLED (Z70-$TR$-CLOSED)
                     NOTOPEN  (Z70-$TR$-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
      * psuedo-conversational program continuation
               GO TO B50-$TR$-ROUTINE-SELECTION
           END-IF.

OPTION*AL  IF  EIBCALEN = ZERO
OPTION*AL  OR (COMM-CURRPGM <= SPACES
OPTION*AL  AND COMM-CURRRTN <= SPACES
OPTION*AL  AND COMM-LINKPGM <= SPACES)
OPTION* if program access did not come from the JOBS menu system...
OPTION*AL      MOVE +1                 TO UNEX-MSGL
OPTION*AL      STRING 'JOBS' UNEX-IC ' <=== '
OPTION*AL            EIBTRNID ' is restricted to the menu system.  '
OPTION*AL            'Press ENTER.'       DELIMITED BY SIZE
OPTION*AL          INTO UNEX-MSG     WITH POINTER UNEX-MSGL
OPTION*AL      SUBTRACT 1            FROM UNEX-MSGL
OPTION*AL      EXEC CICS SEND CONTROL
OPTION*AL                ERASE DEFAULT
OPTION*AL                NOHANDLE
OPTION*AL      END-EXEC
OPTION*AL      EXEC CICS SEND
OPTION*AL                FROM     (UNEX-MSG)
OPTION*AL                LENGTH   (UNEX-MSGL)
OPTION*AL                CTLCHAR  (UNEX-RESET)
OPTION*AL      END-EXEC
OPTION*AL      GO TO Z95-CICS-RETURN
OPTION*AL  END-IF.

       B00-$TR$-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
      * return from 2nd-level program (such as the HELP system)
               GO TO B50-$TR$-ROUTINE-SELECTION
           END-IF.

      * begin one-time-only session initialization

           IF  COMM-CURRRTN NOT = 'RETN' AND 'EXIT'
           OR  COMM-SAVE-FUNC <= SPACES
               MOVE SPACES             TO $TR$-SAVEKEY
           END-IF.

      * call for standard security authorization level
      *    MOVE SPACES                 TO WINSCRTY-COMMAREA.
      *    PERFORM P00-CALL-WINSCRTY THRU P05-EXIT.
      *
      * special ability for auth. user to lower their security level
      *    IF  SCRTY-OLDGRP = 'DI'
      *    AND CICS-STARTCODE = 'TD'
      *        MOVE 13                 TO LEN
      *        EXEC CICS RECEIVE
      *                  SET      (ADDRESS OF COMM-HOLDAREA)
      *                  LENGTH   (LEN)
      *                  NOHANDLE
      *        END-EXEC
      *        IF (EIBRESP = DFHRESP(NORMAL) OR DFHRESP(EOC))
      *        AND (LEN = 7 OR 8 OR 13)
      *        AND COMM-HOLDAREA(5:1) = '='
      *          MOVE COMM-HOLDAREA(6:2) TO SCRTY-OLDGRP
      *          IF  8 <= LEN
      *            MOVE COMM-HOLDAREA(8:1) TO SCRTY-OLDLVL
      *            IF  13 <= LEN
      *              MOVE COMM-HOLDAREA(9:5) TO SCRTY-CONUM
      *            END-IF
      *          END-IF
      *        END-IF
      *    END-IF.
      *
      * save initial security information
      *    MOVE SCRTY-OLDGRP           TO SAVE-SECU-GRP.
      *    MOVE SCRTY-OLDLVL           TO SAVE-SECU-LVL.
      *    MOVE SCRTY-CONUM            TO SAVE-SECU-COMP.

      * translate to security code
      *    EVALUATE $TR$-SECU-LEVL
      *    WHEN   9   SET SECU-DAPSCO-INC   TO TRUE
      *    WHEN   5   SET SECU-LCL-WGS-WSS TO TRUE
      *    WHEN   4   IF  SCRTY-OLDGRP = 'DS'
      *                   SET SECU-DISTRO-INC TO TRUE
      *               ELSE
      *                   EVALUATE VUSERID-IND-LIST(1:2)
      *                   WHEN '*A'  SET SECU-VICE-PRESIDENT TO TRUE
      *                   WHEN '*C'  SET SECU-RVP-CENTRAL TO TRUE
      *                   WHEN '*E'  SET SECU-RVP-EASTERN TO TRUE
      *                   WHEN '*W'  SET SECU-RVP-WESTERN TO TRUE
      *                   WHEN OTHER GO TO Z90-$TR$-TERMINATION
      *                   END-EVALUATE
      *               END-IF
      *    WHEN   3   SET SECU-AC-AL        TO TRUE
      *    WHEN   1   SET SECU-LOCAL-WIN-CO TO TRUE
      *    WHEN OTHER GO TO Z90-$TR$-TERMINATION
      *    END-EVALUATE.
      *
      *    MOVE SAVE-SECU-COMP         TO $TR$-SECU-COMP.
      *    IF  $TR$-COMPNUM NOT NUMERIC
      *        MOVE ZEROES             TO $TR$-COMPNUM
      *    END-IF.

           STRING '<' COMM-HEADER ' ' $TR$-SAVEKEY
               DELIMITED BY SIZE     INTO LOGF-MESG.
           PERFORM Q100-LOGIT        THRU Q199-EXIT.

      * SET UP YOUR FIRST-TIME-ONLY INITIALIZATION CODE HERE

           << ??? >>

       B50-$TR$-ROUTINE-SELECTION.

           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')
           AND COMM-SAVE-FUNC > SPACES
               MOVE COMM-CURRRTN       TO INIT-FLAG
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC
           END-IF.

      * IF NEEDED, YOUR EVERY-TIME INITIALIZATION CODE GOES HERE

           << ??? >>

           EVALUATE COMM-CURRRTN
           WHEN '$F2$' GO TO D00-$F2$-ROUTINE
           WHEN '$F1$' GO TO C00-$F1$-ROUTINE
           END-EVALUATE.

      /*****************************************************************
      *    $F1$ ROUTINE                                                *
      ******************************************************************
       C00-$F1$-ROUTINE.

           IF  ADDRESS OF $F1$MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF $F1$MAPI)
                         LENGTH   (LENGTH OF $F1$MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = '$F1$'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE $TR$-$F1$-TIOA TO $F1$MAPO
                   MOVE LOW-VALUES     TO $TR$-$F1$-TIOA
               END-IF
               PERFORM C80-$F1$-OUTPUT THRU C85-$F1$-SENDMAP
               GO TO C50-$F1$-DISPLAY
           END-IF.

       C10-$F1$-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      ($F1$MAP)
                     INTO     ($F1$MAPI)
                     MAPSET   ($TR$MS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO C70-$F1$-ERRORS
           END-IF.

       C20-$F1$-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               IF  EIBAID = AIDCLEAR
                   MOVE SPACES         TO COMM-LINKPGM
               END-IF
               GO TO Z90-$TR$-TERMINATION

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE $F1$MAPI           TO $TR$-$F1$-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F1$-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO $F1$-MESSAGE-O
               GO TO C80-$F1$-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F1$-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO $F1$-MESSAGE-O
               GO TO C80-$F1$-OUTPUT
           END-EVALUATE.

       C30-$F1$-PROCESS.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      *

           IF  $F1$ HEADER CONTROL FIELD(S) CHANGED
           OR  $F1$-SKIPTO-KEY-L > ZERO
           OR  $F1$-SKIPTO-KEY-F = FLDMODFY OR FLDCURSM
               MOVE SPACES             TP $F1$-KEYNAME-I(1)
               GO TO C50-$F1$-DISPLAY
           END-IF.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM Q20-INITKEY-MYFILE1
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No file entries found.'
                                       TO $F1$-MESSAGE-O
                         GO TO C50-$F1$-DISPLAY
                       END-IF
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   IF  PFX1-KEY = $F1$-KEYNAME-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first page.'
                                       TO $F1$-MESSAGE-O
                   END-IF
               ELSE
                   MOVE HIGH-VALUES    TO PFX1-KEY
                   PERFORM Q21-STARTEQ-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No file entries found.'
                                       TO $F1$-MESSAGE-O
                         GO TO C50-$F1$-DISPLAY
                       END-IF
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   PERFORM Q24-READPREV-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   IF  PFX1-KEY = $F1$-KEYNAME-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last page.'
                                       TO $F1$-MESSAGE-O
                   END-IF
               END-IF
               MOVE PFX1-KEY           TO $F1$-KEYNAME-O(1)
               PERFORM Q25-ENDBR-MYFILE1
               GO TO C50-$F1$-DISPLAY
           END-IF.

      * adjust row and column coordinates for header/detail screens
           SUBTRACT $F1$HEAD         FROM ROW.
      *    COMPUTE COL = (COL + ((SCRNWDTH - $F1$COLS) / $F1$COLS))
      *                       / ((SCRNWDTH - $F1$COLS) / $F1$COLS).

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM Q20-INITKEY-MYFILE1
               MOVE $F1$-KEYNAME-I(1) TO PFX1-KEY
               IF  EIBAID = AIDPFK07
                   IF  ROW < 1 OR >= $F1$ROWS
                   OR  $F1$-KEYNAME-I(ROW) <= SPACES
                       COMPUTE IDX = $F1$ROWS
                   ELSE
                       COMPUTE IDX = $F1$ROWS - ROW
                   END-IF
                   PERFORM Q21-STARTBR-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No file entries found.'
                                       TO $F1$-MESSAGE-O
                         GO TO C50-$F1$-DISPLAY
                       END-IF
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MYFILE1
                   PERFORM Q24-READPREV-MYFILE1
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO $F1$-MESSAGE-O
                       PERFORM Q23-READNEXT-MYFILE1
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-$F1$-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR > $F1$ROWS
                   OR  $F1$-KEYNAME-I(ROW) <= SPACES
                       COMPUTE IDX = $F1$ROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q21-STARTBR-MYFILE1
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No file entries found.'
                                       TO $F1$-MESSAGE-O
                         GO TO C50-$F1$-DISPLAY
                       END-IF
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MYFILE1
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-$F1$-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last entry.'
                                       TO $F1$-MESSAGE-O
                       PERFORM Q24-READPREV-MYFILE1
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-$F1$-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE PFX1-KEY           TO $F1$-KEYNAME-O(1)
               PERFORM Q25-ENDBR-MYFILE1
               GO TO C50-$F1$-DISPLAY
           END-IF.

      * check if default selection needs to be made
           IF  ROW >= 1 AND <= $F1$ROWS
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > $F1$ROWS
                      OR $F1$-SELECT-I(IDX) > SPACE
               END-PERFORM
               IF  IDX > $F1$ROWS
                   MOVE 'S'            TO $F1$-SELECT-O(ROW)
               END-IF
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > $F1$ROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

      * this is an example of editing date input from the screen
               IF  $F1$-MYDATE-L(IDX) > ZERO
               OR  $F1$-MYDATE-F(IDX) = FLDMODFY
                 IF  $F1$-MYDATE-L(IDX) = ZERO
      *            erase-eof was used in this field -- is this ok?
                 ELSE
                   SET  DTE-REQUEST-GREG-EDIT  TO TRUE
                   MOVE $F1$-MYDATE-I(IDX) TO DTE-GREG
                   CALL  'DTEMAN'   USING DTEMAN-PARMS
                   IF  DTE-REQUEST-COMPLETED
                     MOVE DTE-GNUM     TO $F1$-MYDATE-O(IDX)
                                          PFX1-FILEDATE
                   ELSE
                     MOVE 'Input date is invalid.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          $F1$-MYDATE-L(IDX)
                                          $F1$-MESSAGE-L
                                          MULTI-EDIT-MSG
                   END-IF
                 END-IF
               END-IF

      * this is an example of editing numeric input from the screen
               IF  $F1$-MYNUMBER-L(IDX) > ZERO
               OR  $F1$-MYNUMBER-F(IDX) = FLDMODFY
                 IF  $F1$-MYNUMBER-L(IDX) = ZERO
      *            erase-eof was used in this field -- is this ok?
                 ELSE
                   SET  NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE       07       TO NUM-TOTL
                   MOVE        2       TO NUM-DECM
                   MOVE $F1$-MYNUMBER-I(IDX) TO NUM-DATA
                   CALL  'NUMMAN'   USING NUMMAN-PARMS
                   IF  NUM-REQUEST-COMPLETED
                     MOVE NUM-T07-D2   TO $F1$-MYNUMBER-O(IDX)
                                          PFX1-FILENUMBER
                   ELSE
                     MOVE 'Input number is invalid.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          $F1$-MYNUMBER-L(IDX)
                                          $F1$-MESSAGE-L
                                          MULTI-EDIT-MSG
                   END-IF
                 END-IF
               END-IF

      * end of detail loop
           END-PERFORM.

           EVALUATE TRUE
           WHEN ERRORS-FOUND
               GO TO C80-$F1$-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-$F1$-ERRORS
           END-EVALUATE.

           GO TO C50-$F1$-DISPLAY.

       C40-$F1$-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           EVALUATE $F1$-SELECT-I(IDX)
           WHEN 'A'
           WHEN 'S'
           WHEN '*'
               GO TO C45-EXIT

           WHEN 'D'
               IF  $F1$-KEYNAME-I(IDX) NOT > SPACES
                   MOVE LOW-VALUES     TO $F1$-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      *     begin record delete processing

           WHEN OTHER
               MOVE 'Unknown option selection.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          $F1$-SELECT-L(IDX)
                                          $F1$-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO C45-EXIT
           END-EVALUATE.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND $F1$-SELECT-I(IDX) > SPACE
               MOVE '*'                TO $F1$-SELECT-O(IDX)
           END-IF.

       C45-EXIT.
           EXIT.

       C50-$F1$-DISPLAY.

           MOVE '$F1$'                 TO COMM-CURRRTN.

      * skip if heading selections changed or no detail lines
           IF  $F1$-KEYNAME-I(1) NOT > SPACES
               GO TO C60-$F1$-DETAIL
           END-IF.

      * CHECK FOR OPTION SELECTIONS

           PERFORM WITH TEST BEFORE
             VARYING ROW FROM 1 BY 1
               UNTIL ROW > $F1$ROWS
                  OR $F1$-SELECT-I(ROW) > SPACE AND NOT = '*'
           END-PERFORM.

           IF  ROW <= $F1$ROWS
           AND NO-ERRORS-FOUND
               EVALUATE $F1$-SELECT-I(ROW)
      * ... drill down with key data to internal function ...
               WHEN 'S'
                   IF  $F1$-KEYNAME-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO $F1$-SELECT-O(ROW)
                       GO TO C60-$F1$-DETAIL
                   END-IF
                   MOVE $F1$-KEYNAME-I(ROW)
                                       TO $TR$-SAVEKEY
                   MOVE '*'            TO $F1$-SELECT-O(ROW)
                   MOVE $F1$MAPI       TO $TR$-$F1$-TIOA
                   GO TO D00-$F2$-ROUTINE
      * ... transfer with key data to 2nd-level trans/pgm ...
               WHEN 'D'
                   IF  $F1$-KEYNAME-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO $F1$-SELECT-O(ROW)
                       GO TO C60-$F1$-DETAIL
                   END-IF
                   MOVE $F1$-KEYNAME-I(ROW)
                                       TO $TR$-SAVEKEY
                   MOVE '*'            TO $F1$-SELECT-O(ROW)
                   MOVE $F1$MAPI       TO $TR$-$F1$-TIOA
                   MOVE 'EXIT'         TO COMM-TRANSID
                   COMPUTE LINK-LEN =     LENGTH OF COMM-HEADER
                                    +     LENGTH OF COMM-SHARED
                                    +     LENGTH OF CMRC-SAVECOMP
                   GO TO T00-LINK-TRANSFER
      * ... transfer with key data to another trans/pgm ...
               WHEN 'W'
                   IF  $F1$-KEYNAME-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO $F1$-SELECT-O(ROW)
                       GO TO C60-$F1$-DETAIL
                   END-IF
                   MOVE $F1$-KEYNAME-I(ROW)
                                       TO $TR$-SAVEKEY
                   MOVE '*'            TO $F1$-SELECT-O(ROW)
                   MOVE $F1$MAPI       TO $TR$-$F1$-TIOA
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE 'EXIT'         TO COMM-TRANSID
                   MOVE 'MYPGM'        TO COMM-LINKPGM
                   COMPUTE LINK-LEN =     LENGTH OF COMM-HEADER
                                    +     LENGTH OF COMM-SHARED
                                    +     LENGTH OF $TR$-SAVEKEY
                   GO TO T02-LINK-CONTINUE
      * ... pass back key data to 1st-level trans/pgm ...
               WHEN 'X'
                   IF  $F1$-KEYNAME-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO $F1$-SELECT-O(ROW)
                       GO TO C60-$F1$-DETAIL
                   END-IF
                   MOVE 'RETN'         TO COMM-TRANSID
                   MOVE $F1$-KEYNAME-I(ROW)
                                       TO $TR$-SAVEKEY
                   GO TO T00-LINK-TRANSFER
      *
               WHEN OTHER
                 MOVE     -1           TO ERROR-SWITCH
                                          $F1$-SELECT-L(ROW)
                 MOVE  EXHREVRS        TO $F1$-MESSAGE-H
                 MOVE 'Unknown option selection.'
                                       TO $F1$-MESSAGE-O
               END-EVALUATE
               GO TO C80-$F1$-OUTPUT
           END-IF.

       C60-$F1$-DETAIL.

           PERFORM Q20-INITKEY-MYFILE1.
           EVALUATE TRUE
           WHEN $F1$-SKIPTO-KEY-L > ZERO
            AND $F1$-SKIPTO-KEY-I > SPACES
               MOVE $F1$-SKIPTO-KEY-I  TO PFX1-KEY
               PERFORM Q21-STARTBR-MYFILE1
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE HIGH-VALUES    TO PFX1-KEY
                   PERFORM Q21-STARTEQ-MYFILE1
                   PERFORM Q24-READPREV-MYFILE1
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q23-READNEXT-MYFILE1
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C70-$F1$-ERRORS
               END-IF
               IF  $F1$-SKIPTO-KEY-I < PFX1-KEY
                   PERFORM Q24-READPREV-MYFILE1
                     VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO $F1$-MESSAGE-O
                   END-IF
               END-IF
               PERFORM Q25-ENDBR-MYFILE1
           WHEN $TR$-SAVEKEY > SPACES
               MOVE $TR$-SAVEKEY       TO PFX1-KEY
           WHEN $F1$-KEYNAME-I(1) > SPACES
               MOVE $F1$-KEYNAME-I(1)  TO PFX1-KEY
           END-EVALUATE.

           PERFORM Q21-STARTBR-MYFILE1.
           IF  EIBRESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES         TO PFX1-KEY
               PERFORM Q21-STARTBR-MYFILE1
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE 1              TO IDX
                   GO TO C65-$F1$-BLANKS
               END-IF
           END-IF.

           MOVE SPACES                 TO $TR$-SAVEKEY.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > $F1$ROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q23-READNEXT-MYFILE1
               IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                   MOVE LOW-VALUES     TO $F1$MAP-ROW-DETAIL(IDX)

               ELSE
                   MOVE  -1            TO $F1$-SELECT-L(IDX)
                   SUBTRACT 1        FROM IDX
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               PERFORM C65-$F1$-BLANKS
               GO TO C70-$F1$-ERRORS
           END-IF.

           PERFORM Q25-ENDBR-MYFILE1.

       C65-$F1$-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > $F1$ROWS
             MOVE LOW-VALUES           TO $F1$MAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO $F1$-SELECT-A(IDX)
           END-PERFORM.

       C70-$F1$-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F1$-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO $F1$-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       C80-$F1$-OUTPUT.

           MOVE MAPTITLE               TO $F1$-COMPANY-O.
           MOVE WRK-DSP-DATE           TO $F1$-SYSDATE-O.
           MOVE WRK-DSP-TIME           TO $F1$-SYSTIME-O.
           MOVE CICS-NETNAME           TO $F1$-NETNAME-O.
           IF  $F1$-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO $F1$-NETNAME-O
           END-IF.

       C85-$F1$-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      ($F1$MAP)
                             DATAONLY
                             FROM     ($F1$MAPO)
                             MAPSET   ($TR$MS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      ($F1$MAP)
                             DATAONLY
                             FROM     ($F1$MAPO)
                             MAPSET   ($TR$MS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      ($F1$MAP)
                         FROM     ($F1$MAPO)
                         MAPSET   ($TR$MS)
                         TERMINAL
                         FREEKB
                         ERASE
      *                  CURSOR
               END-EXEC
           END-IF.

       C90-$F1$-RETURN.

           EXEC CICS FREEMAIN
                     DATA     ($F1$MAPI)
           END-EXEC.

           GO TO Z80-$TR$-RETURN.

      /*****************************************************************
      *    $F2$ ROUTINE                                                *
      ******************************************************************
       D00-$F2$-ROUTINE.

           IF  ADDRESS OF $F2$MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF $F2$MAPI)
                         LENGTH   (LENGTH OF $F2$MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = '$F2$'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE $TR$-SAVE-TIOA TO $F2$MAPO
                   MOVE LOW-VALUES     TO $TR$-SAVE-TIOA
                   IF  COMM-HELPAREA(1:4) > SPACES
                     MOVE LOW-VALUES   TO COMM-HELPAREA(1:4)
                     EXEC CICS SEND CONTROL ERASE END-EXEC
                   END-IF
               ELSE
                   MOVE ZEROES         TO TS-TOTL
               END-IF
               PERFORM D80-$F2$-OUTPUT THRU D85-$F2$-SENDMAP
               GO TO D50-$F2$-DISPLAY
           END-IF.

       D10-$F2$-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      ($F2$MAP)
                     INTO     ($F2$MAPI)
                     MAPSET   ($TR$MS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-$F2$-ERRORS
           END-IF.

       D20-$F2$-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE SPACES             TO $TR$-SAVEKEY
               GO TO C00-$F1$-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE $F2$MAPI           TO $TR$-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F2$-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO $F2$-MESSAGE-O
               GO TO D80-$F2$-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F2$-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO $F2$-MESSAGE-O
               GO TO D80-$F2$-OUTPUT
           END-EVALUATE.

       D30-$F2$-PROCESS.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      *

           IF  $F2$ HEADER CONTROL FIELDS CHANGED
               MOVE ZEROES             TO TS-TOTL
               GO TO D50-$F2$-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   IF  TS-$F2$-ITEM = 1
                       MOVE 'This is the first page.' TO $F2$-MESSAGE-O
                   ELSE
                       MOVE 1          TO TS-$F2$-ITEM
                   END-IF
               ELSE
                   COMPUTE TS-$F2$-ITEM = TS-$F2$-TOTL
                                        - ($F2$ROWS - 1)
                   IF  TS-$F2$-ITEM < 1
                       MOVE 1          TO TS-$F2$-ITEM
                       MOVE 'This is the last page.' TO $F2$-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-$F2$-DISPLAY
           END-IF.

      * adjust row and column coordinates for header/detail screens
           SUBTRACT $F2$HEAD         FROM ROW.
      *    COMPUTE COL = (COL + ((SCRNWDTH - $F2$COLS) / $F2$COLS))
      *                       / ((SCRNWDTH - $F2$COLS) / $F2$COLS).

           IF  EIBAID = AIDPFK07 OR AIDPFK08
               IF  EIBAID = AIDPFK07
                   IF  (ROW < 1 OR >= $F2$ROWS)
                       MOVE ZEROES     TO ROW
                   END-IF
                   COMPUTE TS-$F2$-ITEM = TS-$F2$-ITEM
                                        - ($F2$ROWS - ROW)
                   IF  TS-$F2$-ITEM < 1
                       MOVE +1         TO TS-$F2$-ITEM
                       MOVE 'This is the first entry.'
                                       TO $F2$-MESSAGE-O
                   END-IF
               ELSE
                   IF  (ROW <= 1 OR > $F2$ROWS)
                       COMPUTE ROW = $F2$ROWS + 1
                   END-IF
                   COMPUTE TS-$F2$-ITEM = TS-$F2$-ITEM
                                        + (ROW - 1)
                   IF  TS-$F2$-ITEM > TS-$F2$-TOTL
                       MOVE TS-$F2$-TOTL TO TS-$F2$-ITEM
                       MOVE 'This is the last entry.'
                                       TO $F2$-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-$F2$-DISPLAY
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

      * end of detail editing

           EVALUATE TRUE
           WHEN ERRORS-FOUND
               GO TO D80-$F2$-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-$F2$-ERRORS
           END-EVALUATE.

       D40-$F2$-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

       D50-$F2$-DISPLAY.

           MOVE '$F2$'                 TO COMM-CURRRTN.

      * PERFORM HEADER CONTROL FIELD EDITING HERE

       D60-$F2$-DETAIL.

           IF  TS-TOTL <= ZEROES
               PERFORM P10-EXTRACT-DATA THRU P15-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO $F2$-MESSAGE-H
                   IF  $F2$-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO $F2$-MESSAGE-O
                   END-IF
                   GO TO D80-$F2$-OUTPUT
               END-IF
               IF  TS-TOTL <= ZERO
                   MOVE 'No entries found.'
                                       TO $F2$-MESSAGE-O
                   MOVE 1              TO IDX
                   GO TO D65-$F2$-BLANKS
               END-IF
           END-IF.

           MOVE 1                      TO IDX.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > $F2$ROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM S32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE

             ELSE
               MOVE -1                 TO $F2$-SELECT-L(IDX)
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

       D65-$F2$-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > $F2$ROWS
             MOVE LOW-VALUES           TO $F2$MAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO $F2$-SELECT-A(IDX)
           END-PERFORM.

       D70-$F2$-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO $F2$-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO $F2$-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       D80-$F2$-OUTPUT.

           MOVE MAPTITLE               TO $F2$-COMPANY-O.
           MOVE WRK-DSP-DATE           TO $F2$-SYSDATE-O.
           MOVE WRK-DSP-TIME           TO $F2$-SYSTIME-O.
           MOVE CICS-NETNAME           TO $F2$-NETNAME-O.
           IF  $F2$-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO $F2$-NETNAME-O
           END-IF.

       D85-$F2$-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      ($F2$MAP)
                             DATAONLY
                             FROM     ($F2$MAPO)
                             MAPSET   ($TR$MS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      ($F2$MAP)
                             DATAONLY
                             FROM     ($F2$MAPO)
                             MAPSET   ($TR$MS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               MOVE -1                 TO $F2$-SELECT-L
               EXEC CICS SEND
                         MAP      ($F2$MAP)
                         FROM     ($F2$MAPO)
                         MAPSET   ($TR$MS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       D90-$F2$-RETURN.

           EXEC CICS FREEMAIN
                     DATA     ($F2$MAPI)
           END-EXEC.

           GO TO Z80-$TR$-RETURN.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P00-CALL-WINSCRTY.
           IF  $TR$-COMP-NO5 > SPACES
               MOVE $TR$-COMP-NO5      TO SCRTY-CONUM
           END-IF.

           EXEC CICS LINK
                     PROGRAM  (WINSCRTY)
                     COMMAREA (WINSCRTY-COMMAREA)
           END-EXEC.
       P05-EXIT.
           EXIT.

       P10-EXTRACT-$F1$-INFO.

      * display popup to let user know we're working on it
           MOVE 'Generating...  '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

      * ETC.

       P19-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-FORMAT-DATE-AND-TIME.
           EXEC CICS ASKTIME    ABSTIME(WRK-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WRK-ABSTIME)
                     FULLDATE  (WRK-DSP-DATE)        DATESEP
                     TIME      (WRK-DSP-TIME)        TIMESEP
           END-EXEC.
       Q02-REFORMAT-TIME.
           IF  WRK-DSP-HH < 12
               MOVE ' AM'              TO WRK-DSP-AM-PM
               IF  WRK-DSP-HH = 00
                   MOVE 12             TO WRK-DSP-HH
               END-IF
           ELSE
               MOVE ' PM'              TO WRK-DSP-AM-PM
               IF  WRK-DSP-HH > 12
                   SUBTRACT 12       FROM WRK-DSP-HH
               END-IF
           END-IF.
       Q05-EXIT.
           EXIT.

       Q10-DISPLAY-POPUP.

           EXEC CICS GETMAIN
                     SET      (ADDRESS OF WAITWINI)
                     LENGTH   (LENGTH OF WAITWINI)
                     INITIMG  (LOVALUE)
           END-EXEC.

           MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.
           EXEC CICS SEND
                     MAP      (WAITWIN)
                     FROM     (WAITWINO)
                     MAPSET   ($TR$MS)
                     TERMINAL
                     CURSOR(+926)      << on 24x80 screen  >>
                     CURSOR(+1655)     << on 27x132 screen >>
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS FREEMAIN
                     DATA     (WAITWINI)
           END-EXEC.

       Q15-EXIT.
           EXIT.

       Q20-INITKEY-MYFILE1.
           IF  ADDRESS OF MYFILE1-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MYFILE1-RECORD)
                         LENGTH   (LENGTH OF MYFILE1-RECORD)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO PFX1-KEY.
       Q21-STARTBR-MYFILE1.
           EXEC CICS STARTBR
                     DATASET  (MYFILE1)
                     RIDFLD   (PFX1-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q21-STARTEQ-MYFILE1.
           EXEC CICS STARTBR
                     DATASET  (MYFILE1)
                     RIDFLD   (PFX1-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q22-RESETBR-MYFILE1.
           EXEC CICS RESETBR
                     DATASET  (MYFILE1)
                     RIDFLD   (PFX1-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-MYFILE1.
           EXEC CICS READNEXT
                     DATASET  (MYFILE1)
                     INTO     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-READPREV-MYFILE1.
           EXEC CICS READPREV
                     DATASET  (MYFILE1)
                     INTO     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     NOHANDLE
           END-EXEC.
       Q25-ENDBR-MYFILE1.
           EXEC CICS ENDBR
                     DATASET  (MYFILE1)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-MYFILE1.
           EXEC CICS READ
                     DATASET  (MYFILE1)
                     INTO     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-MYFILE1.
           EXEC CICS READ
                     DATASET  (MYFILE1)
                     INTO     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q26-READUP-MYFILE1.
           EXEC CICS READ
                     DATASET  (MYFILE1)
                     INTO     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q27-WRITE-MYFILE1.
           EXEC CICS WRITE
                     DATASET  (MYFILE1)
                     FROM     (MYFILE1-RECORD)
                     RIDFLD   (PFX1-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-MYFILE1.
           EXEC CICS REWRITE
                     DATASET  (MYFILE1)
                     FROM     (MYFILE1-RECORD)
                     NOHANDLE
           END-EXEC.
       Q28-DELETE-MYFILE1.
           EXEC CICS DELETE
                     DATASET  (MYFILE1)
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-MYFILE1.
           EXEC CICS UNLOCK
                     DATASET  (MYFILE1)
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q31-NUMITEMS-TS.
           MOVE 1                      TO TS-ITEM.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     ITEM    (TS-ITEM)
                     INTO    (TS-RECD)
                     NUMITEMS(TS-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               MOVE ZEROES             TO TS-TOTL
               MOVE 1                  TO TS-ITEM
           END-IF.
       Q32-READQ-TS.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     INTO    (TS-RECD)
                     ITEM    (SS-ITEM)
                     NOHANDLE
           END-EXEC.
       Q33-WRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     ITEM    (SS-ITEM)
                     AUXILIARY
                     NOHANDLE
           END-EXEC.
       Q34-REWRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     ITEM    (SS-ITEM)
                     REWRITE
                     AUXILIARY
                     NOHANDLE
           END-EXEC.
       Q35-DELETEQ-TS.
           MOVE ZEROES                 TO TS-TOTL
                                          SS-ITEM.
           MOVE 1                      TO TS-ITEM.
           IF  TS-QUEUE <= SPACES
               MOVE THIS-TRN           TO TS-TRAN
               MOVE EIBTRMID           TO TS-TERM
           END-IF.
           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q35-EXIT.
           EXIT.

       Q40-INITKEY-MYFILE2.
           IF  ADDRESS OF MYFILE2-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MYFILE2-RECORD)
                         LENGTH   (LENGTH OF MYFILE2-RECORD)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO PFX2-KEY.
       Q41-STARTBR-MYFILE2.
           EXEC CICS STARTBR
                     DATASET  (MYFILE2)
                     RIDFLD   (PFX2-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q43-READNEXT-MYFILE2.
           EXEC CICS READNEXT
                     DATASET  (MYFILE2)
                     INTO     (MYFILE2-RECORD)
                     RIDFLD   (PFX2-KEY)
                     NOHANDLE
           END-EXEC.
       Q45-ENDBR-MYFILE2.
           EXEC CICS ENDBR
                     DATASET  (MYFILE2)
                     NOHANDLE
           END-EXEC.
       Q49-EXIT.
           EXIT.

       Q80-SORT-QUEUE.

           MOVE 'Sorting...     '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

      * setup to call CICSSORT
           MOVE     TS-QUEUE           TO CICSSORT-QNAME.
           EVALUATE COMM-CURRRTN
           WHEN '$F2$'
               MOVE TS-$F2$-TOTL       TO CICSSORT-RECCNT
           WHEN OTHER
               MOVE TS-$F1$-TOTL       TO CICSSORT-RECCNT
           END-EVALUATE.
           MOVE LENGTH OF TS-RECD      TO CICSSORT-RECLEN.
           IF  SORT-BY-NUMB
               MOVE  1                 TO CICSSORT-KEYOFF
               MOVE  5                 TO CICSSORT-KEYLEN
           ELSE
               MOVE  6                 TO CICSSORT-KEYOFF
               MOVE 20                 TO CICSSORT-KEYLEN
           END-IF.
           SET CICSSORT-ASCENDING      TO TRUE.
           EXEC CICS LINK
                     PROGRAM  (CICSSORT)
                     COMMAREA (CICSSORT-PARMS)
           END-EXEC.

      * delete the old queue
           EXEC CICS DELETEQ TS
                     QUEUE    (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

      * save new queue name
           MOVE CICSSORT-QNAME         TO TS-QUEUE.
           EVALUATE COMM-CURRRTN
           WHEN '$F2$'
               MOVE TS-QUEUE           TO TS-$F2$-QUEUE
           WHEN OTHER
               MOVE TS-QUEUE           TO TS-$F1$-QUEUE
           END-EVALUATE.

       Q85-EXIT.
           EXIT.

      /*****************************************************************
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *
      ******************************************************************
       COPY COMMLINK.
      * the above copybook ends with a return to cics

      ******************************************************************
      *    TRANSFER TO THE CICS HELP SYSTEM                            *
      ******************************************************************
       COPY COMMHELP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    VERIFICATION ROUTINE                                        *
      ******************************************************************
       V10-GET-CONFIRMATION.
           MOVE AIDENTER               TO EIBAID.
           MOVE 1                      TO POS.
      *                         row  adjust            col  adjust
           COMPUTE HALF-WORD = (024 - 1) * SCRNWDTH + (001 - 1).
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           STRING MULTI-EDIT-MSG(1:LEN)
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           EVALUATE TRUE
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2
      *                             row  adjust            col  adjust
               COMPUTE HALF-WORD = (024 - 1) * SCRNWDTH + (080 - 1)
               STRING SCREPEAT TWO-BYTES X'40'
                      SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG
               STRING SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           END-EVALUATE.
           SUBTRACT 1                FROM POS.

           EXEC CICS CONVERSE
                     FROM     (VAR-DATA)
                     FROMLENGTH(POS)
                     CTLCHAR  (WRTFKFSA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(EOC)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       V15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *
      ******************************************************************

       Z70-$TR$-CLOSED.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-$TR$-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-$TR$-TERMINATION.

      * use the following if you don't want to pass back any data
      *    COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.
      * use the following if you want to pass back the full commarea
           COPY COMMEXIT.

      * the above copybook ends with a return to cics and GOBACK

