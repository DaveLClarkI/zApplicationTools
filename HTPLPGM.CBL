 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    HTPLPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  AUG 2024.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      TRANSACTION SECURITY, ONLY.
      *REMARKS.       HTTP JSON/XML TEMPLATE EDITOR PROGRAM.

      * CHANGE HISTORY ------------------------------------------------
      * 08/28/2024 DLC ORIGINAL PROGRAM.
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

       SPECIAL-NAMES.
           CLASS ALPHA-NATIONAL        IS '@', '#', '$',
                                          'A' THRU 'I',
                                          'J' THRU 'R',
                                          'S' THRU 'Z',
           CLASS VALID-CHARACTERS      IS ' ', '_', '-',
                                          '@', '#', '$',
                                          'A' THRU 'I',
                                          'J' THRU 'R',
                                          'S' THRU 'Z',
                                          '0' THRU '9',
           CLASS LINE-COMMAND          IS ' ', 'I', 'C', 'M',
                                          'A', 'B', '"', 'D',
                                          'L', 'U',
           CLASS POSITIVE-DIGIT        IS '1' THRU '9'.

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
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='HTPL'==
                                       =='PROGRAM'== BY =='HTPLPGM'==.
         03  HTPLMS                    PIC  X(08)   VALUE 'HTPLMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  EDITMAP                   PIC  X(08)   VALUE 'EDITMAP '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  HTTPLID                   PIC  X(08)   VALUE 'HTTPLID '.
         03  HTTPLTE                   PIC  X(08)   VALUE 'HTTPLTE '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  JOBSSELT                  PIC  X(08)   VALUE 'JOBSSELT'.
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  EOD-SWITCH                PIC S9(01)   VALUE -1.
           88  EOD-NOT-MARKED                       VALUE -1.
           88  EOD-MARKED                           VALUE ZEROES.
         03  RECORD-SWITCH             PIC  X(01)   VALUE 'N'.
           88  RECORD-NOT-SELECTED                  VALUE 'N'.
           88  RECORD-IS-SELECTED                   VALUE 'Y'.
         03  NOTFOUND-SWITCH           PIC  X(01)   VALUE 'Y'.
           88  RECORD-WAS-FOUND                     VALUE 'Y'.
           88  RECORD-NOT-FOUND                     VALUE 'N'.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  TPID-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  TPLT-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +04.
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +20.
         03  EDITHEAD                  PIC S9(04)   BINARY VALUE +05.
         03  EDITROWS                  PIC S9(04)   BINARY VALUE +20.
         03  EDITPRFX                  PIC S9(04)   BINARY VALUE +07.
         03  EDIT-MAX                  PIC S9(04)   BINARY.
      *
         03  SAVED-TRANSL              PIC  X       VALUE SPACE.
         03  SAVED-TRANSC              PIC  9       VALUE ZERO.
         03  NNN                       PIC  999     VALUE ZEROES.
         03  EDIT-NUMBER               PIC S9(18)   COMP-3 VALUE ZERO.
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.

       COPY BINARIES.
       COPY DATETIME.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY HEXMAN.

       COPY LOGGING.

       COPY UNEXERRW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * HTPL CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  HTPL-SAVEKEY            PIC  x(13).
           05  HTPL-PROCESS            PIC  X(01).
             88  VIEW-PROCESS                       VALUE 'V'.
             88  ADD-PROCESS                        VALUE 'A'.
             88  CHG-PROCESS                        VALUE 'C'.
           05  HTPL-CASE-SWITCH        PIC  X(01).
             88  CASE-MIXED                         VALUE 'M'.
             88  CASE-UPPER                         VALUE 'U'.
           05  HTPL-UPDATE-SWITCH      PIC  X(01).
             88  HTPL-NOT-UPDATED                   VALUE 'N'.
             88  HTPL-WAS-UPDATED                   VALUE 'Y'.

           05  TS-QUEUE.
             07  TS-TRAN               PIC  X(4).
             07  TS-TERM               PIC  X(4).
           05  TS-TOTL                 PIC S9(4)    BINARY.
           05  TS-ITEM                 PIC S9(4)    BINARY.
           05  SS-ITEM                 PIC S9(4)    BINARY.
           05  TS-RECD.
             07  TS-SELECT             PIC  X(5).
             07  TS-DATA.
               09  TS-GRPNAME          PIC  X(25).
               09  TS-GRPDESC          PIC  X(99).
           05  SAVED-DATA              PIC  X(124).

           05  T2-QUEUE.
             07  T2-TERM               PIC  X(4).
             07  T2-TRAN               PIC  X(4).
           05  T2-TOTL                 PIC S9(4)    BINARY.
           05  T2-ITEM                 PIC S9(4)    BINARY.
           05  T2-RECD.
             07  T2-SELECT             PIC  X(5).
             07  T2-DATA               PIC  X(124).

           05  HTPL-LIST-TIOA          PIC  X(3430).
           05  HTPL-SAVE-TIOA          PIC  X(3227).

      * HTPL TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * HTTPLID RECORD I/O AREA
       COPY HTTPLID.

      * HTTPLTE RECORD I/O AREA
       COPY HTTPLTE.

      * BMS TERMINAL I/O AREAS
       COPY HTPLMS.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-HTPL-PROGRAM-ENTRY.

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

           SET  DAPL-LOGGING           TO TRUE.

           EXEC CICS HANDLE CONDITION
                     DISABLED (Z70-HTPL-CLOSED)
                     NOTOPEN  (Z70-HTPL-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
      * psuedo-conversational program continuation
               GO TO B50-HTPL-ROUTINE-SELECTION
           END-IF.

       B00-HTPL-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
      * return from 2nd-level program (such as the HELP system)
               GO TO B50-HTPL-ROUTINE-SELECTION
           END-IF.

      * the following executes one-time-only per user session

           IF  COMM-CURRRTN NOT = 'RETN'
                              AND 'EXIT'
               MOVE SPACES             TO HTPL-SAVEKEY
           END-IF.

           SET  CASE-MIXED             TO TRUE.

      * this program requires mixed-case terminal input
           EXEC CICS SET TERMINAL(EIBTRMID)
                     TRANIDONLY
                     NOHANDLE
           END-EXEC.

       B50-HTPL-ROUTINE-SELECTION.

           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')
           AND COMM-SAVE-FUNC > SPACES
               MOVE COMM-CURRRTN       TO INIT-FLAG
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC
           END-IF.

      * value clause won't accept this large of a number
           MOVE 32760                  TO EDIT-MAX.

           EVALUATE COMM-CURRRTN
           WHEN 'EDIT' GO TO D00-EDIT-ROUTINE
           WHEN 'LIST' GO TO C00-LIST-ROUTINE
           END-EVALUATE.

      /*****************************************************************
      *    LIST ROUTINE                                                *
      ******************************************************************
       C00-LIST-ROUTINE.

           IF  ADDRESS OF LISTMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF LISTMAPI)
                         LENGTH   (LENGTH OF LISTMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'LIST'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN   = 'RETN'
               OR  COMM-SAVE-FUNC = 'RETN'
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HTPL-LIST-TIOA TO LISTMAPO
                   MOVE LOW-VALUES     TO HTPL-LIST-TIOA
                   MOVE SPACES         TO COMM-HELPAREA
               END-IF
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP
               GO TO C50-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LISTMAP)
                     INTO     (LISTMAPI)
                     MAPSET   (HTPLMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO C70-LIST-ERRORS
           END-IF.

       C20-LIST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-HTPL-TERMINATION

           WHEN AIDPFK03
               GO TO Z90-HTPL-TERMINATION

           WHEN AIDPFK01
               MOVE LISTMAPI           TO HTPL-LIST-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK05
               MOVE SPACES             TO HTPL-SAVEKEY
               SET  ADD-PROCESS        TO TRUE
               MOVE LISTMAPI           TO HTPL-LIST-TIOA
               GO TO D00-EDIT-ROUTINE

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
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-EVALUATE.

       C30-LIST-PROCESS.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
           SUBTRACT LISTHEAD         FROM ROW.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM Q20-INITKEY-HTTPLID
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No template ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  TPID-NAME = LIST-TEMPLATE-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first page.'
                                       TO LIST-MESSAGE-O
                   END-IF
               ELSE
                   MOVE HIGH-VALUES    TO TPID-KEY
                   PERFORM Q21-STARTEQ-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No template ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q24-READPREV-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  TPID-NAME = LIST-TEMPLATE-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last page.'
                                       TO LIST-MESSAGE-O
                   END-IF
               END-IF
               MOVE TPID-NAME          TO LIST-TEMPLATE-O(1)
               PERFORM Q25-ENDBR-HTTPLID
               GO TO C50-LIST-DISPLAY
           END-IF.

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM Q20-INITKEY-HTTPLID
               MOVE LIST-TEMPLATE-I(1) TO TPID-NAME
               IF  EIBAID = AIDPFK07
                   IF  ROW < 1 OR >= LISTROWS
                   OR  LIST-TEMPLATE-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = LISTROWS - ROW
                   END-IF
                   PERFORM Q21-STARTBR-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No template ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-HTTPLID
                   PERFORM Q24-READPREV-HTTPLID
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO LIST-MESSAGE-O
                       PERFORM Q23-READNEXT-HTTPLID
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR > LISTROWS
                   OR  LIST-TEMPLATE-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q21-STARTBR-HTTPLID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No template ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-HTTPLID
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last entry.'
                                       TO LIST-MESSAGE-O
                       PERFORM Q24-READPREV-HTTPLID
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE TPID-NAME          TO LIST-TEMPLATE-O(1)
               PERFORM Q25-ENDBR-HTTPLID
               GO TO C50-LIST-DISPLAY
           END-IF.

      * check if default selection needs to be made
           IF  ROW >= 1 AND <= LISTROWS
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > LISTROWS
                      OR LIST-SELECT-I(IDX) > SPACE
               END-PERFORM
               IF  IDX > LISTROWS
                   MOVE 'S'            TO LIST-SELECT-O(ROW)
               END-IF
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               INSPECT LIST-SELECT-I(IDX) CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               IF  LIST-SELECT-I(IDX) > SPACE
                   PERFORM C40-LIST-UPDATES THRU C45-EXIT
               END-IF

      * end of detail loop
           END-PERFORM.

           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-LIST-ERRORS
           END-IF.

           GO TO C50-LIST-DISPLAY.

       C40-LIST-UPDATES.

           IF  LIST-TEMPLATE-I(IDX) NOT > SPACES
               MOVE LOW-VALUES         TO LIST-SELECT-I(IDX)
               GO TO C45-EXIT
           END-IF.

      * PROCESS ANY ALLOWED UPDATES HERE

      * retrieve template attributes
           PERFORM Q20-INITKEY-HTTPLID.
           MOVE LIST-TEMPLATE-I(IDX)   TO TPID-NAME.
           IF  LIST-TEMPLATE-TYPE-I(IDX) = 'RESP'
               SET  TPID-RESP          TO TRUE
           ELSE
               SET  TPID-REQU          TO TRUE
           END-IF.
           PERFORM Q26-READEQ-HTTPLID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C45-EXIT
           END-IF.

           EVALUATE LIST-SELECT-I(IDX)
           WHEN '*'
           WHEN 'S'
           WHEN 'E'
               GO TO C45-EXIT

           WHEN 'D'
      * confirm delete processing
               COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 3
               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
               MOVE SPACES             TO MULTI-EDIT-MSG
               MOVE 1                  TO LEN
               STRING 'Delete this template?  '
                      'Press F2 to confirm.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               PERFORM V10-GET-CONFIRMATION THRU V15-EXIT
               IF  EIBAID  NOT = AIDPFK02
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * delete template body text
               PERFORM Q40-INITKEY-HTTPLTE
               MOVE TPID-KEY           TO TPLT-ID
               EXEC CICS DELETE
                         DATASET(HTTPLTE)
                         RIDFLD(TPLT-ID)
                         KEYLENGTH(LENGTH OF TPLT-ID)
                         GENERIC
                         NOHANDLE
               END-EXEC
      * delete template id header
               IF  EIBRESP = DFHRESP(NORMAL)
                          OR DFHRESP(NOTFND)
                   EXEC CICS DELETE
                             DATASET(HTTPLID)
                             RIDFLD(TPID-KEY)
                             NOHANDLE
                   END-EXEC
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL1(IDX)
               END-IF

           WHEN OTHER
               MOVE -1                 TO ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
               MOVE EXCRED             TO LIST-SELECT-C(IDX)
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'Unknown option selection.'
                                       TO LIST-MESSAGE-O
               GO TO C45-EXIT
           END-EVALUATE.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND LIST-SELECT-I(IDX) > SPACE
               MOVE '*'                TO LIST-SELECT-O(IDX)
           END-IF.

       C45-EXIT.
           EXIT.

       C50-LIST-DISPLAY.

           MOVE 'LIST'                 TO COMM-CURRRTN.

      * CHECK FOR OPTION SELECTIONS

           PERFORM WITH TEST BEFORE
             VARYING ROW FROM 1 BY 1
               UNTIL ROW > LISTROWS
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'
           END-PERFORM.

           IF  ROW <= LISTROWS
           AND NO-ERRORS-FOUND
               EVALUATE LIST-SELECT-I(ROW)
      * ... transfer key data to internal function ...
               WHEN 'S'
               WHEN 'E'
                 IF  LIST-TEMPLATE-I(ROW) NOT > SPACES
                   MOVE LOW-VALUES     TO LIST-SELECT-O(ROW)
                 ELSE
                   MOVE LIST-TEMPLATE-I(ROW)
                                       TO HTPL-SAVEKEY
                   IF  LIST-TEMPLATE-TYPE-I(ROW) = 'RESP'
                     MOVE 'R'          TO HTPL-SAVEKEY(13:)
                   ELSE
                     MOVE 'Q'          TO HTPL-SAVEKEY(13:)
                   END-IF
                   IF  LIST-SELECT-I(ROW) = 'E'
                     SET  CHG-PROCESS  TO TRUE
                   ELSE
                     SET  VIEW-PROCESS TO TRUE
                   END-IF
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO HTPL-LIST-TIOA
                   GO TO D00-EDIT-ROUTINE
                 END-IF
      *
               WHEN OTHER
                 MOVE     -1           TO ERROR-SWITCH
                                          LIST-SELECT-L(ROW)
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H
                 MOVE 'Unknown option selection.'
                                       TO LIST-MESSAGE-O
               END-EVALUATE
               GO TO C80-LIST-OUTPUT
           END-IF.

       C60-LIST-DETAIL.

           MOVE 1                      TO IDX.

           PERFORM Q20-INITKEY-HTTPLID.
           EVALUATE TRUE
           WHEN HTPL-SAVEKEY > SPACES
               MOVE HTPL-SAVEKEY       TO TPID-KEY
           WHEN LIST-TEMPLATE-I(1) > SPACES
               MOVE LIST-TEMPLATE-I(1) TO TPID-NAME
           WHEN OTHER
               PERFORM Q21-STARTBR-HTTPLID
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 CONTINUE
               WHEN DFHRESP(NOTFND)
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
                 MOVE 'No template ids found.'
                                       TO LIST-MESSAGE-O
                 GO TO C65-LIST-BLANKS
               WHEN OTHER
                 GO TO C70-LIST-ERRORS
               END-EVALUATE
               PERFORM Q23-READNEXT-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                             AND DFHRESP(ENDFILE)
                   GO TO C70-LIST-ERRORS
               END-IF
               PERFORM Q25-ENDBR-HTTPLID
           END-EVALUATE.

           PERFORM Q21-STARTBR-HTTPLID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-LIST-ERRORS
           END-IF.

           MOVE SPACES                 TO HTPL-SAVEKEY.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q23-READNEXT-HTTPLID
               IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                 MOVE LOW-VALUES       TO LISTMAP-ROW-DETAIL1(IDX)
                 MOVE TPID-NAME        TO LIST-TEMPLATE-O(IDX)
                 IF  TPID-REQU
                   MOVE 'REQU'         TO LIST-TEMPLATE-TYPE-O(IDX)
                 ELSE
                   MOVE 'RESP'         TO LIST-TEMPLATE-TYPE-O(IDX)
                 END-IF
                 MOVE TPID-DESCRIPTION TO LIST-DESCRIPTION-O(IDX)
                 IF  TPID-JSON
                   MOVE 'JSON'         TO LIST-TEMPLATE-FMT-O(IDX)
                 ELSE
                   MOVE 'XML '         TO LIST-TEMPLATE-FMT-O(IDX)
                 END-IF
                 MOVE TPID-CREATED-BY  TO LIST-CREATED-O(IDX)
                 MOVE TPID-UPDATED-BY  TO LIST-UPDATED-O(IDX)
                 IF  TPID-UPDATED-TIMESTAMP IS NUMERIC
                 AND TPID-UPDATED-TIMESTAMP > ZEROES
                     EXEC CICS FORMATTIME
                               ABSTIME(TPID-UPDATED-TIMESTAMP)
                               YYYYMMDD(LIST-LAST-UPDATED-O(IDX))
                               DATESEP('-')
                               TIME(LIST-LAST-UPDATET-O(IDX))
                               TIMESEP(':')
                     END-EXEC
                 END-IF

               ELSE
                 MOVE  -1              TO LIST-SELECT-L(IDX)
                 SUBTRACT 1          FROM IDX
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               PERFORM C65-LIST-BLANKS
               GO TO C70-LIST-ERRORS
           END-IF.

           PERFORM Q25-ENDBR-HTTPLID.

       C65-LIST-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > LISTROWS
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL1(IDX)
             MOVE FLDASDRK             TO LIST-SELECT-A(IDX)
           END-PERFORM.

       C70-LIST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO LIST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       C80-LIST-OUTPUT.

           MOVE MAPTITLE               TO LIST-COMPANY-O.
           MOVE WRK-DSP-DATE           TO LIST-SYSDATE-O.
           MOVE WRK-DSP-TIME           TO LIST-SYSTIME-O.
           MOVE CICS-NETNAME           TO LIST-NETNAME-O.
           IF  LIST-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO LIST-NETNAME-O
           END-IF.

       C85-LIST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (HTPLMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (HTPLMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               MOVE -1                 TO LIST-SELECT-L(1)
               EXEC CICS SEND
                         MAP      (LISTMAP)
                         FROM     (LISTMAPO)
                         MAPSET   (HTPLMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LISTMAPI)
           END-EXEC.

           GO TO Z80-HTPL-RETURN.

      /*****************************************************************
      *    EDIT ROUTINE                                                *
      ******************************************************************
       D00-EDIT-ROUTINE.

           IF  ADDRESS OF EDITMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF EDITMAPI)
                         LENGTH   (LENGTH OF EDITMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'EDIT'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN   = 'RETN'
               OR  COMM-SAVE-FUNC = 'RETN'
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HTPL-SAVE-TIOA TO EDITMAPO
                   MOVE LOW-VALUES     TO HTPL-SAVE-TIOA
                   MOVE SPACES         TO COMM-HELPAREA
               ELSE
                   MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC
                   SET HTPL-NOT-UPDATED TO TRUE
                   MOVE ZEROES         TO TS-TOTL
               END-IF
               PERFORM D80-EDIT-OUTPUT THRU D85-EDIT-SENDMAP
               GO TO D50-EDIT-DISPLAY
           END-IF.

       D10-EDIT-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (EDITMAP)
                     INTO     (EDITMAPI)
                     MAPSET   (HTPLMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-EDIT-ERRORS
           END-IF.

       D20-EDIT-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
           WHEN AIDPFK03
               IF  HTPL-WAS-UPDATED
                 MOVE SPACES           TO MULTI-EDIT-MSG
                 MOVE 1                TO LEN
                 STRING 'Discard template changes?  '
                        'Press F2 to confirm.' DELIMITED BY SIZE
                     INTO MULTI-EDIT-MSG WITH POINTER LEN
                 SUBTRACT 1          FROM LEN
                 PERFORM V10-GET-CONFIRMATION THRU V15-EXIT
                 IF  EIBAID  NOT = AIDPFK02
                 OR  EIBRESP NOT = DFHRESP(NORMAL)
                     MOVE AIDENTER     TO EIBAID
                     GO TO D30-EDIT-PROCESS
                 END-IF
               END-IF
               PERFORM Q35-DELETEQ-TS
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
               MOVE EDITMAPI           TO HTPL-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK02
           WHEN AIDPFK05
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
           WHEN AIDPFK12
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO EDIT-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO EDIT-MESSAGE-O
               GO TO D80-EDIT-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO EDIT-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO EDIT-MESSAGE-O
               GO TO D80-EDIT-OUTPUT
           END-EVALUATE.

       D30-EDIT-PROCESS.

      * no case toggle nor screen update if in view-only mode
           IF  VIEW-PROCESS
           AND (EIBAID = AIDPFK02 OR AIDPFK05)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO EDIT-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO EDIT-MESSAGE-O
               GO TO D80-EDIT-OUTPUT
           END-IF.

      * allow for cancellation regardless of any changes
           IF  EIBAID = AIDPFK12
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               GO TO C00-LIST-ROUTINE
           END-IF.

      * allow for addition of new templates
           IF  ADD-PROCESS
               INSPECT EDIT-TEMPLATE-I
                               CONVERTING LOWER-CASE
                                       TO UPPER-CASE
               INSPECT EDIT-TEMPLATE-TYPE-I
                               CONVERTING LOWER-CASE
                                       TO UPPER-CASE
               INSPECT EDIT-TEMPLATE-FMT-I
                               CONVERTING LOWER-CASE
                                       TO UPPER-CASE

               IF  EDIT-TEMPLATE-I NOT > SPACES
                 MOVE 'Template name is required.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-TEMPLATE-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF
               PERFORM P90-COMPRESS-TEMPLATE-NAME THRU P95-EXIT
               IF  EDIT-TEMPLATE-I(1:1) NOT ALPHA-NATIONAL
               OR  EDIT-TEMPLATE-I(2:) NOT VALID-CHARACTERS
                 MOVE 'Name contains invalid characters (see HELP).'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-TEMPLATE-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF
               IF  EDIT-TEMPLATE-TYPE-I NOT = 'REQU' AND 'RESP'
                 MOVE 'Type must be ''REQU'' or ''RESP''.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-TEMPLATE-TYPE-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF

               PERFORM Q20-INITKEY-HTTPLID
               MOVE EDIT-TEMPLATE-I    TO TPID-NAME
               IF  EDIT-TEMPLATE-TYPE-I = 'RESP'
                   SET  TPID-RESP      TO TRUE
               ELSE
                   SET  TPID-REQU      TO TRUE
               END-IF
               PERFORM Q26-READEQ-HTTPLID
               IF  EIBRESP = DFHRESP(NORMAL)
                 MOVE 'Template and type already exists.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-TEMPLATE-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF

               IF  EDIT-DESCRIPTION-I NOT > SPACES
                 MOVE 'Description is required.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-DESCRIPTION-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF
               IF  EDIT-TEMPLATE-FMT-I NOT = 'JSON' AND 'XML'
                 MOVE 'Format must be ''JSON'' or ''XML''.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-TEMPLATE-FMT-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF

               INITIALIZE TPID-DATA
               IF  EDIT-TEMPLATE-FMT-I = 'XML'
                   SET  TPID-XML       TO TRUE
               ELSE
                   SET  TPID-JSON      TO TRUE
               END-IF
               MOVE EDIT-DESCRIPTION-I TO TPID-DESCRIPTION
               MOVE CICS-USERID        TO TPID-CREATED-BY
                                          TPID-UPDATED-BY
               EXEC CICS ASKTIME ABSTIME (TPID-CREATED-TIMESTAMP)
               END-EXEC
               MOVE TPID-CREATED-TIMESTAMP
                                       TO TPID-UPDATED-TIMESTAMP

               PERFORM Q27-WRITE-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-EDIT-ERRORS
               END-IF
      * ... then switch to change mode
               MOVE TPID-KEY           TO HTPL-SAVEKEY
               SET CHG-PROCESS         TO TRUE
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
           SUBTRACT EDITHEAD         FROM ROW.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           IF  NOT VIEW-PROCESS

           IF  EIBAID = AIDPFK02
               IF  CASE-MIXED
                   SET  CASE-UPPER     TO TRUE
               ELSE
                   SET  CASE-MIXED     TO TRUE
               END-IF
           END-IF

      * allow description to be changed every time
           IF  EDIT-DESCRIPTION-L > ZERO
           OR  EDIT-DESCRIPTION-F = FLDMODFY OR FLDCURSM
               IF  EDIT-DESCRIPTION-I NOT > SPACES
                 MOVE 'Description is required.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-DESCRIPTION-L
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO D80-EDIT-OUTPUT
               END-IF
               PERFORM Q20-INITKEY-HTTPLID
               MOVE HTPL-SAVEKEY       TO TPID-KEY
               PERFORM Q26-READUP-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-EDIT-ERRORS
               END-IF
               MOVE EDIT-DESCRIPTION-I TO TPID-DESCRIPTION
               MOVE CICS-USERID        TO TPID-UPDATED-BY
               EXEC CICS ASKTIME
                         ABSTIME  (TPID-UPDATED-TIMESTAMP)
               END-EXEC
               PERFORM Q27-REWRITE-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-EDIT-ERRORS
               END-IF
           END-IF

      * loop on body text lines to process them
           MOVE 1                      TO IDX
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > EDITROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

      * ... save text lines, first
             IF  EDIT-TEXT-L(IDX)  > ZERO
             OR  EDIT-TEXT-F(IDX)  = FLDMODFY OR FLDCURSM
               SET  HTPL-WAS-UPDATED   TO TRUE
               IF  EDIT-TEXT-I(IDX) > SPACES
                 MOVE EDIT-TEXT-I(IDX) TO TS-DATA
                 IF  CASE-UPPER
                   INSPECT TS-DATA
                               CONVERTING LOWER-CASE
                                       TO UPPER-CASE
                 END-IF
               ELSE
                 MOVE SPACES           TO TS-DATA
               END-IF
             END-IF

      * ... save line commands for later processing
             IF  EDIT-LCA-I(IDX) > SPACES AND NOT = '*===*'
               INSPECT EDIT-LCA-I(IDX) CONVERTING '*=' TO SPACES
               MOVE EDIT-LCA-I(IDX)    TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE      TO TS-SELECT
               INSPECT TS-SELECT CONVERTING LOWER-CASE
                                         TO UPPER-CASE
               MOVE TS-SELECT          TO EDIT-LCA-I(IDX)
               EVALUATE TRUE
               WHEN TS-SELECT(1:1) IS NOT LINE-COMMAND
                 MOVE 'Line command is unknown.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-LCA-L(IDX)
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN TS-SELECT(2:1) > SPACE
               AND (TS-SELECT(1:1) = 'C' OR 'M')
               OR   TS-SELECT(2:1) > SPACE
               AND  TS-SELECT(2:1) IS NOT POSITIVE-DIGIT
                 MOVE 'Line command is invalid.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          EDIT-LCA-L(IDX)
                                          EDIT-MESSAGE-L
                                          MULTI-EDIT-MSG
               END-EVALUATE
               IF  NO-ERRORS-FOUND
                 EVALUATE TRUE
                 WHEN TS-SELECT(1:1) = 'C' OR 'M'
                   IF  SAVED-DATA = LOW-VALUES
                     MOVE TS-DATA      TO SAVED-DATA
                   ELSE
                     MOVE LOW-VALUES   TO TS-SELECT
                                          EDIT-LCA-I(IDX)
                   END-IF
                 WHEN SAVED-TRANSL > SPACE
                  AND SAVED-TRANSC > ZERO
                   PERFORM P22-TRANSL-REPLICATE THRU P25-EXIT
                 WHEN TS-SELECT(1:1) = 'L' OR 'U'
                   PERFORM P20-SETUP-TRANSLATE THRU P25-EXIT
                 END-EVALUATE
               END-IF
             ELSE
               IF  SAVED-TRANSL > SPACE
               AND SAVED-TRANSC > ZERO
                 PERFORM P22-TRANSL-REPLICATE THRU P25-EXIT
               ELSE
                 MOVE LOW-VALUES       TO TS-SELECT
               END-IF
             END-IF

             PERFORM Q34-REWRITEQ-TS
             ADD  1                    TO IDX
             IF  SS-ITEM = 32767
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
             END-IF
           END-PERFORM

      * ... if no errors, process all line commands here
           IF  NO-ERRORS-FOUND
             PERFORM P30-LINE-COMMANDS THRU P35-EXIT
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO D80-EDIT-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO D70-EDIT-ERRORS
           END-IF.

      * END OF DETAIL FIELD EDITING

           IF  EIBAID = AIDPFK05
               GO TO D40-EDIT-UPDATES
           END-IF

      * scroll to top or bottom of the data
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   IF  TS-ITEM = 1
                       MOVE 'This is the first page.'
                                       TO EDIT-MESSAGE-O
                   ELSE
                       MOVE 1          TO TS-ITEM
                   END-IF
               ELSE
                   COMPUTE TS-ITEM = TS-TOTL - (EDITROWS - 2)
                   IF  TS-ITEM < 1
                       MOVE 1          TO TS-ITEM
                       MOVE 'This is the last page.'
                                       TO EDIT-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-EDIT-DISPLAY
           END-IF.

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               IF  EIBAID = AIDPFK07
                   IF  (ROW < 1 OR >= EDITROWS)
                       MOVE ZEROES     TO ROW
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM - (EDITROWS - ROW)
                   IF  TS-ITEM < 1
                       MOVE +1         TO TS-ITEM
                       MOVE 'This is the first entry.'
                                       TO EDIT-MESSAGE-O
                   END-IF
               ELSE
                   IF  (ROW <= 1 OR > EDITROWS)
                       COMPUTE ROW = EDITROWS + 1
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
                   IF  TS-ITEM > TS-TOTL
                   OR  TS-ITEM < 1
                       MOVE TS-TOTL    TO TS-ITEM
                       MOVE 'This is the last entry.'
                                       TO EDIT-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-EDIT-DISPLAY
           END-IF.

           GO TO D50-EDIT-DISPLAY.

       D40-EDIT-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           PERFORM Q20-INITKEY-HTTPLID.
           MOVE HTPL-SAVEKEY           TO TPID-KEY.
           PERFORM Q26-READUP-HTTPLID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-EDIT-ERRORS
           END-IF.

           MOVE CICS-USERID            TO TPID-UPDATED-BY.
           EXEC CICS ASKTIME
                     ABSTIME  (TPID-UPDATED-TIMESTAMP)
           END-EXEC.

           PERFORM Q27-REWRITE-HTTPLID,
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-EDIT-ERRORS
           END-IF.

           PERFORM Q40-INITKEY-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               EXEC CICS SUSPEND END-EXEC

               PERFORM Q32-READQ-TS

               MOVE SS-ITEM            TO TPLT-SEQ
               PERFORM Q46-READUP-HTTPLTE

               MOVE TS-DATA            TO TPLT-DATA

               EVALUATE EIBRESP
               WHEN DFHRESP(NOTFND)
                   PERFORM Q47-WRITE-HTTPLTE
               WHEN DFHRESP(NORMAL)
                   PERFORM Q47-REWRITE-HTTPLTE
               END-EVALUATE

               IF  SS-ITEM = 32767
                   MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(ENDFILE)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.

           COMPUTE TPLT-SEQ = TS-TOTL + 1.

           PERFORM WITH TEST BEFORE
             VARYING TPLT-SEQ FROM TPLT-SEQ BY 1
               UNTIL EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q46-READUP-HTTPLTE
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q48-DELETE-HTTPLTE
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
               GO TO D70-EDIT-ERRORS
           END-IF.

           PERFORM Q35-DELETEQ-TS.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       D50-EDIT-DISPLAY.

           MOVE 'EDIT'                 TO COMM-CURRRTN.

      * skip if no detail lines present
           IF  TS-TOTL <= ZERO
               GO TO D60-EDIT-DETAIL
           END-IF.

      * PERFORM HEADER CONTROL FIELD EDITING HERE

       D60-EDIT-DETAIL.

           SET  NO-ERRORS-FOUND        TO TRUE.
           MOVE HTPL-SAVEKEY           TO EDIT-TEMPLATE-O.
           MOVE EXCTURQU               TO EDIT-MESSAGE-C.

           IF  TS-TOTL <= ZERO
               PERFORM P10-EXTRACT-TEXT-CONTENT THRU P15-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO EDIT-MESSAGE-H
                   IF  EDIT-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO EDIT-MESSAGE-O
                   END-IF
                   GO TO D80-EDIT-OUTPUT
               END-IF
           END-IF.

      * retrieve and display header fields
           IF  HTPL-SAVEKEY > SPACES
               PERFORM Q20-INITKEY-HTTPLID
               MOVE HTPL-SAVEKEY       TO TPID-KEY
               PERFORM Q26-READEQ-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NOTFND)
                   OR  NOT ADD-PROCESS
                       GO TO D70-EDIT-ERRORS
                   END-IF
                   INITIALIZE TPID-DATA
               END-IF
               EVALUATE TRUE
               WHEN TPID-REQU
                   MOVE 'REQU'         TO EDIT-TEMPLATE-TYPE-O
               WHEN TPID-RESP
                   MOVE 'RESP'         TO EDIT-TEMPLATE-TYPE-O
               WHEN OTHER
                   MOVE LOW-VALUES     TO EDIT-TEMPLATE-TYPE-O
               END-EVALUATE
               MOVE TPID-DESCRIPTION   TO EDIT-DESCRIPTION-O
               EVALUATE TRUE
               WHEN TPID-JSON
                   MOVE 'JSON'         TO EDIT-TEMPLATE-FMT-O
               WHEN TPID-XML
                   MOVE 'XML'          TO EDIT-TEMPLATE-FMT-O
               WHEN OTHER
                   MOVE LOW-VALUES     TO EDIT-TEMPLATE-FMT-O
               END-EVALUATE
           END-IF.

      * retrieve and display detail fields
           MOVE 1                      TO IDX.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > EDITROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                 MOVE LOW-VALUES       TO EDITMAP-ROW-DETAIL1(IDX)
                 MOVE TS-DATA          TO EDIT-TEXT-O(IDX)
             ELSE
               MOVE -1                 TO EDIT-LCA-L(IDX)
               SUBTRACT 1            FROM IDX
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           IF  TS-TOTL >= EDIT-MAX
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.

       D65-EDIT-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > EDITROWS
             MOVE LOW-VALUES           TO EDITMAP-ROW-DETAIL1(IDX)
             MOVE FLDASDRK             TO EDIT-LCA-A(IDX)
                                          EDIT-TEXT-A(IDX)
             IF  EOD-NOT-MARKED
               SET  EOD-MARKED         TO TRUE
               MOVE FLDASBRT           TO EDIT-TEXT-A(IDX)
               MOVE EXCNEUTR           TO EDIT-TEXT-C(IDX)
               MOVE ' -- END OF DATA -- '
                                       TO EDIT-TEXT-O(IDX)
             END-IF
           END-PERFORM.

       D70-EDIT-ERRORS.

           IF  EIBRESP = DFHRESP(ENDFILE)
               MOVE 'Maximum size reached.'
                                       TO EDIT-MESSAGE-O
               GO TO D80-EDIT-OUTPUT
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO EDIT-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO EDIT-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       D80-EDIT-OUTPUT.

           MOVE MAPTITLE               TO EDIT-COMPANY-O.
           MOVE WRK-DSP-DATE           TO EDIT-SYSDATE-O.
           MOVE WRK-DSP-TIME           TO EDIT-SYSTIME-O.
           MOVE CICS-NETNAME           TO EDIT-NETNAME-O.
           IF  EDIT-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO EDIT-NETNAME-O
           END-IF.

           IF  CASE-UPPER
               MOVE 'UPPER'            TO EDIT-CASE-O
           ELSE
               MOVE 'MIXED'            TO EDIT-CASE-O
           END-IF.

           EVALUATE TRUE
           WHEN ADD-PROCESS
               MOVE FLDUNFST           TO EDIT-TEMPLATE-A
                                          EDIT-TEMPLATE-TYPE-A
                                          EDIT-TEMPLATE-FMT-A
                                          EDIT-DESCRIPTION-A
               MOVE EXHULINE           TO EDIT-TEMPLATE-H
                                          EDIT-TEMPLATE-TYPE-H
                                          EDIT-TEMPLATE-FMT-H
                                          EDIT-DESCRIPTION-H
               MOVE FLDASBRT           TO EDIT-CASE-KEY-A
                                          EDIT-UPDATE-KEY-A
               MOVE FLDASKIP           TO EDIT-CASE-TEXT-A
                                          EDIT-UPDATE-TEXT-A
           WHEN CHG-PROCESS
               MOVE FLDASKIP           TO EDIT-TEMPLATE-A
                                          EDIT-TEMPLATE-TYPE-A
                                          EDIT-TEMPLATE-FMT-A
               MOVE FLDUNPRO           TO EDIT-DESCRIPTION-A
               MOVE EXHDEFLT           TO EDIT-TEMPLATE-H
                                          EDIT-TEMPLATE-TYPE-H
                                          EDIT-TEMPLATE-FMT-H
                                          EDIT-DESCRIPTION-H
               MOVE FLDASBRT           TO EDIT-CASE-KEY-A
                                          EDIT-UPDATE-KEY-A
               MOVE FLDASKIP           TO EDIT-CASE-TEXT-A
                                          EDIT-UPDATE-TEXT-A
           WHEN OTHER
               MOVE FLDASKIP           TO EDIT-TEMPLATE-A
                                          EDIT-TEMPLATE-TYPE-A
                                          EDIT-DESCRIPTION-A
                                          EDIT-TEMPLATE-FMT-A
               MOVE EXHDEFLT           TO EDIT-TEMPLATE-H
                                          EDIT-TEMPLATE-TYPE-H
                                          EDIT-DESCRIPTION-H
                                          EDIT-TEMPLATE-FMT-H
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > EDITROWS
                 IF  EDIT-LCA-A(IDX) NOT = FLDASDRK
                   MOVE FLDASKIP       TO EDIT-LCA-A(IDX)
                                          EDIT-TEXT-A(IDX)
                 END-IF
               END-PERFORM
               MOVE FLDASDRK           TO EDIT-CASE-KEY-A
                                          EDIT-CASE-TEXT-A
                                          EDIT-UPDATE-KEY-A
                                          EDIT-UPDATE-TEXT-A
           END-EVALUATE.

       D85-EDIT-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (EDITMAP)
                             DATAONLY
                             FROM     (EDITMAPO)
                             MAPSET   (HTPLMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (EDITMAP)
                             DATAONLY
                             FROM     (EDITMAPO)
                             MAPSET   (HTPLMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               IF  ADD-PROCESS
                   MOVE -1             TO EDIT-TEMPLATE-L
               ELSE
                   DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL
                   ADD  1              TO ROW COL
                   SUBTRACT EDITHEAD FROM ROW
                   IF  ROW < 1
                     MOVE 1            TO ROW
                   ELSE
                     IF  ROW > TS-TOTL
                       MOVE TS-TOTL    TO ROW
                     END-IF
                   END-IF
                   MOVE -1             TO EDIT-LCA-L(ROW)
               END-IF
               EXEC CICS SEND
                         MAP      (EDITMAP)
                         FROM     (EDITMAPO)
                         MAPSET   (HTPLMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       D90-EDIT-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (EDITMAPI)
           END-EXEC.

           GO TO Z80-HTPL-RETURN.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-EXTRACT-TEXT-CONTENT.
           PERFORM Q35-DELETEQ-TS.

           PERFORM Q40-INITKEY-HTTPLTE.
           MOVE HTPL-SAVEKEY           TO TPLT-ID.
           PERFORM Q41-STARTBR-HTTPLTE.

           MOVE ZEROES                 TO SS-ITEM.
           PERFORM WITH TEST BEFORE
             UNTIL TPLT-ID NOT = HTPL-SAVEKEY
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q43-READNEXT-HTTPLTE
             IF  EIBRESP = DFHRESP(NORMAL)
             AND TPLT-ID = HTPL-SAVEKEY
               MOVE SPACES             TO TS-SELECT
               COMPUTE LEN = TPLT-RECL - LENGTH OF TPLT-KEY
               MOVE TPLT-DATA(1:LEN)   TO TS-DATA
               PERFORM Q33-WRITEQ-TS
               IF  SS-ITEM < 32767
                 ADD  1                TO SS-ITEM
               ELSE
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q45-ENDBR-HTTPLTE
               PERFORM Q31-NUMITEMS-TS
               IF  TS-TOTL <= ZERO
                   MOVE SPACES         TO TS-RECD
                   PERFORM Q33-WRITEQ-TS
                   PERFORM Q31-NUMITEMS-TS
               END-IF
           END-IF.
       P15-EXIT.
           EXIT.

       P20-SETUP-TRANSLATE.
           MOVE TS-SELECT(1:1)         TO SAVED-TRANSL.
           IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
               MOVE TS-SELECT(2:1)     TO SAVED-TRANSC
           ELSE
               MOVE 1                  TO SAVED-TRANSC
           END-IF.
       P22-TRANSL-REPLICATE.
           MOVE SAVED-TRANSL           TO TS-SELECT.
           SUBTRACT 1                FROM SAVED-TRANSC.
           IF  SAVED-TRANSC = ZERO
               MOVE SPACES             TO SAVED-TRANSL
           END-IF.
       P25-EXIT.
           EXIT.

       P30-LINE-COMMANDS.
           PERFORM Q65-DELETEQ-2-TS.
           MOVE TS-TOTL                TO T2-TOTL.
           MOVE ZEROES                 TO T2-ITEM.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             EVALUATE TS-SELECT(1:1)
             WHEN 'C'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
             WHEN 'M'
               SET  HTPL-WAS-UPDATED   TO TRUE
               SUBTRACT 1            FROM T2-TOTL
             WHEN 'I'
               SET  HTPL-WAS-UPDATED   TO TRUE
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                 MOVE TS-SELECT(2:1)   TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               MOVE SPACES             TO T2-DATA
               PERFORM WITH TEST BEFORE
                 VARYING CNT FROM 1 BY 1
                   UNTIL CNT > NNN
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                 PERFORM P32-LINE-WRITE
               END-PERFORM
             WHEN 'A'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  SAVED-DATA > LOW-VALUES
                 SET  HTPL-WAS-UPDATED TO TRUE
                 IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                   MOVE TS-SELECT(2:1) TO NNN
                 ELSE
                   MOVE 1              TO NNN
                 END-IF
                 MOVE SAVED-DATA       TO T2-DATA
                 PERFORM WITH TEST BEFORE
                   VARYING CNT FROM 1 BY 1
                     UNTIL CNT > NNN
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   PERFORM P32-LINE-WRITE
                 END-PERFORM
                 MOVE LOW-VALUES       TO SAVED-DATA
               END-IF
             WHEN 'B'
               IF  SAVED-DATA > LOW-VALUES
                 SET  HTPL-WAS-UPDATED TO TRUE
                 IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                   MOVE TS-SELECT(2:1) TO NNN
                 ELSE
                   MOVE 1              TO NNN
                 END-IF
                 MOVE SAVED-DATA       TO T2-DATA
                 PERFORM WITH TEST BEFORE
                   VARYING CNT FROM 1 BY 1
                     UNTIL CNT > NNN
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   PERFORM P32-LINE-WRITE
                 END-PERFORM
                 MOVE LOW-VALUES       TO SAVED-DATA
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                 MOVE TS-DATA          TO T2-DATA
                 PERFORM P32-LINE-WRITE
               END-IF
             WHEN 'L'
               SET  HTPL-WAS-UPDATED   TO TRUE
               MOVE TS-DATA            TO T2-DATA
               INSPECT T2-DATA CONVERTING UPPER-CASE
                                       TO LOWER-CASE
               PERFORM P32-LINE-WRITE
             WHEN 'U'
               SET  HTPL-WAS-UPDATED   TO TRUE
               MOVE TS-DATA            TO T2-DATA
               INSPECT T2-DATA CONVERTING LOWER-CASE
                                       TO UPPER-CASE
               PERFORM P32-LINE-WRITE
             WHEN '"'
               SET  HTPL-WAS-UPDATED   TO TRUE
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                 MOVE TS-SELECT(2:1)   TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               MOVE TS-DATA            TO T2-DATA
               PERFORM WITH TEST BEFORE
                 VARYING CNT FROM 1 BY 1
                   UNTIL CNT > NNN
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                 PERFORM P32-LINE-WRITE
               END-PERFORM
             WHEN 'D'
               SET  HTPL-WAS-UPDATED   TO TRUE
               MOVE TS-SELECT(2:3) TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               IF  UNEX-VARIABLE(1:UNEX-LENG) IS NUMERIC
                 MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               IF  NNN > (TS-TOTL - SS-ITEM + 1)
                 COMPUTE NNN = TS-TOTL - SS-ITEM + 1
               END-IF
               SUBTRACT NNN          FROM T2-TOTL
               COMPUTE SS-ITEM = SS-ITEM + (NNN - 1)
             WHEN OTHER
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
             END-EVALUATE
             IF  SS-ITEM = 32767
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
             END-IF
           END-PERFORM.

           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.

           MOVE T2-QUEUE               TO TS-QUEUE.
           MOVE T2-ITEM                TO TS-TOTL.

           IF  TS-TOTL >= EDIT-MAX
           OR  EIBRESP = DFHRESP(ITEMERR)
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.

           GO TO P35-EXIT.

       P32-LINE-WRITE.
           EXEC CICS SUSPEND END-EXEC.
           MOVE LOW-VALUES             TO T2-SELECT.
           PERFORM Q63-WRITEQ-2-TS.

       P35-EXIT.
           EXIT.

       P90-COMPRESS-TEMPLATE-NAME.
           MOVE 1                      TO PTR.
           PERFORM WITH TEST BEFORE
             VARYING CNT FROM 1 BY 1
               UNTIL CNT >= LENGTH OF EDIT-TEMPLATE-I
             IF  EDIT-TEMPLATE-I(PTR:1) > SPACE
               ADD  1                  TO PTR
             ELSE
               MOVE EDIT-TEMPLATE-I(PTR + 1:)
                                       TO EDIT-TEMPLATE-I(PTR:)
             END-IF
           END-PERFORM.
       P95-EXIT.
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

       Q20-INITKEY-HTTPLID.
           IF  ADDRESS OF HTTPLID-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HTTPLID-RECORD)
                         LENGTH   (LENGTH OF HTTPLID-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO TPID-KEY.
       Q21-STARTBR-HTTPLID.
           EXEC CICS STARTBR
                     DATASET  (HTTPLID)
                     RIDFLD   (TPID-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q21-STARTEQ-HTTPLID.
           EXEC CICS STARTBR
                     DATASET  (HTTPLID)
                     RIDFLD   (TPID-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q22-RESETBR-HTTPLID.
           EXEC CICS RESETBR
                     DATASET  (HTTPLID)
                     RIDFLD   (TPID-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READNEXT
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-READPREV-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READPREV
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     NOHANDLE
           END-EXEC.
       Q25-ENDBR-HTTPLID.
           EXEC CICS ENDBR
                     DATASET  (HTTPLID)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READ
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READ
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q26-READUP-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READ
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q27-WRITE-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS WRITE
                     DATASET  (HTTPLID)
                     FROM     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS REWRITE
                     DATASET  (HTTPLID)
                     FROM     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     NOHANDLE
           END-EXEC.
       Q28-DELETE-HTTPLID.
           EXEC CICS DELETE
                     DATASET  (HTTPLID)
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-HTTPLID.
           EXEC CICS UNLOCK
                     DATASET  (HTTPLID)
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
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q34-REWRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     ITEM    (SS-ITEM)
                     REWRITE
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q35-DELETEQ-TS.
           MOVE ZEROES                 TO TS-ITEM.
           IF  TS-QUEUE <= SPACES
               MOVE THIS-TRN           TO TS-TRAN
               MOVE EIBTRMID           TO TS-TERM
           END-IF.
           MOVE SPACES                 TO TS-RECD.
           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q39-EXIT.
           EXIT.

       Q40-INITKEY-HTTPLTE.
           IF  ADDRESS OF HTTPLTE-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HTTPLTE-RECORD)
                         LENGTH   (LENGTH OF HTTPLTE-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO TPLT-ID
           MOVE ZEROES                 TO TPLT-SEQ.
       Q41-STARTBR-HTTPLTE.
           EXEC CICS STARTBR
                     DATASET  (HTTPLTE)
                     RIDFLD   (TPLT-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q43-READNEXT-HTTPLTE.
           MOVE LENGTH OF HTTPLTE-RECORD TO TPLT-RECL.
           EXEC CICS READNEXT
                     DATASET  (HTTPLTE)
                     INTO     (HTTPLTE-RECORD)
                     LENGTH   (TPLT-RECL)
                     RIDFLD   (TPLT-KEY)
                     NOHANDLE
           END-EXEC.
       Q45-ENDBR-HTTPLTE.
           EXEC CICS ENDBR
                     DATASET  (HTTPLTE)
                     NOHANDLE
           END-EXEC.
       Q46-READUP-HTTPLTE.
           MOVE LENGTH OF HTTPLTE-RECORD TO TPLT-RECL.
           EXEC CICS READ
                     DATASET  (HTTPLTE)
                     INTO     (HTTPLTE-RECORD)
                     LENGTH   (TPLT-RECL)
                     RIDFLD   (TPLT-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q47-WRITE-HTTPLTE.
           PERFORM Q49-SET-TEXT-LENGTH.
           COMPUTE TPLT-RECL = LENGTH OF TPLT-KEY + LEN.
           EXEC CICS WRITE
                     DATASET  (HTTPLTE)
                     FROM     (HTTPLTE-RECORD)
                     LENGTH   (TPLT-RECL)
                     RIDFLD   (TPLT-KEY)
                     NOHANDLE
           END-EXEC.
       Q47-REWRITE-HTTPLTE.
           PERFORM Q49-SET-TEXT-LENGTH.
           COMPUTE TPLT-RECL = LENGTH OF TPLT-KEY + LEN.
           EXEC CICS REWRITE
                     DATASET  (HTTPLTE)
                     FROM     (HTTPLTE-RECORD)
                     LENGTH   (TPLT-RECL)
                     NOHANDLE
           END-EXEC.
       Q48-DELETE-HTTPLTE.
           EXEC CICS DELETE
                     DATASET  (HTTPLTE)
                     NOHANDLE
           END-EXEC.
       Q49-SET-TEXT-LENGTH.
           IF  TPLT-DATA > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF TPLT-DATA BY -1
                   UNTIL LEN < 1
                      OR TPLT-DATA(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
           END-IF.
       Q49-EXIT.
           EXIT.

       Q63-WRITEQ-2-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (T2-QUEUE)
                     FROM    (T2-RECD)
                     ITEM    (T2-ITEM)
                     MAIN
                     NOHANDLE
           END-EXEC.
       Q65-DELETEQ-2-TS.
           MOVE TS-TRAN                TO T2-TRAN.
           MOVE TS-TERM                TO T2-TERM.
           MOVE SPACES                 TO T2-RECD.
           EXEC CICS DELETEQ TS
                     QUEUE   (T2-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q69-EXIT.
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
      *    CONFIRMATION ROUTINE                                        *
      ******************************************************************
       V10-GET-CONFIRMATION.
           MOVE AIDENTER               TO EIBAID.
           MOVE 1                      TO POS.
      *                         row  adjust            col  adjust
           COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (001 - 1).
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           STRING MULTI-EDIT-MSG(1:LEN)
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           EVALUATE TRUE
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2
      *                             row  adjust            col  adjust
               COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (080 - 1)
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

       Z70-HTPL-CLOSED.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-HTPL-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-HTPL-TERMINATION.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

      * use the following if you don't want to pass back any data
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.

      * the above copybook ends with a return to cics and GOBACK

