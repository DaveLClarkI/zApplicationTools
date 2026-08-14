JCL      TITLE '$JOBEX00 - JCL USER EXIT ROUTINE - z/VSE v4.2 and up'
&DBGOPT  SETC  'OFF'                   .SET DEBUGGING {OFF|ON}
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      J O B   C O N T R O L   U S E R   E X I T   R O U T I N E      *
*                                                                     *
*                                                                     *
* Winsupply Group Services                            Written by:     *
* 3110 Kettering Blvd.                             Dave Leonard Clark *
* Dayton, OH  45439                                  (501)750-8248    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This routine causes "/."  JCL  label  statements to log on the *
* system console if the  statement  contains comments after the label *
* field.   In addition,  any comments specified ("*" JCL comments and *
* "// PAUSE" statements,  as  well)  may contain symbolic parameters, *
* which this routine  will  resolve  appropriately.   Such logged JCL *
* labels are useful for indicating job "steps" to inform the operator *
* of progress and to provide branch points for use in conditional JCL *
* (e.g., automatic recovery or debugging).                            *
*                                                                     *
*      Symbolic parameter  resolution  is  executed  in reverse order *
* (right-to-left across columns 3  thru 71).   Scanning from right to *
* left ensures that nested or derived variable names (e.g., &SYM&BOL) *
* are evaluated correctly:  right-hand suffix variables resolve first *
* to form the complete symbol name for the left-hand prefix variable. *
* Explicit concatenation  periods  (&SYM.&BOL),  on  the  other hand, *
* remain intact  until  the  left-hand  variable  itself is resolved. *
* Thus,  SETPARM statements are  also  included in this right-to-left *
* resolution algorithm.                                               *
*                                                                     *
*      In addition,  this routine  prevents  runaway "LOG" statements *
* (caused by omitted "NOLOG"  statements) from forcing all subsequent *
* JCL in the partition  to  list  on the system console.   Logging is *
* automatically suppressed upon  reaching  the first "EXEC" statement *
* following the "LOG" statement in question.                          *
*                                                                     *
* ------------------------------------------------------------------- *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This routine is named  to  be  part  of the "Multiple JCL Exit *
* Routines" function as described in  the  "IBM VSE/ESA V1 R2 Release *
* Information Guide" pp.   30-34.  It may be renamed to "$JOBEXIT" in *
* order to run as the  only  JCL exit routine if desired.   In either *
* case, this routine must be linked as SVA-eligible.  For example:    *
*                                                                     *
*      PHASE  $JOBEX00,S,SVA                                          *
*                                                                     *
* It must also reside in the  system  library at IPL time in order to *
* get loaded into the SVA at that time.  For example:                 *
*                                                                     *
*      SET SDL                                                        *
*      $JOBEX00,SVA                                                   *
*      /* EOD                                                         *
*                                                                     *
* Also,  if part of a "Multiple JCL  Exit List",  it must be added to *
* your "$JOBEXIT" exit list as follows:                               *
*                                                                     *
*      DC    CL8'$JOBEX00',CL8'STEPLOG '                              *
*                                                                     *
*      As  supplied,  this  routine  will  handle  a  maximum  of  48 *
* partitions in any combination of static and dynamic varieties.   If *
* you have more than this,  you must change the duplication factor at *
* symbol "WRKSPDUP" in the WRKSPACE DSECT.                            *
*                                                                     *
* ------------------------------------------------------------------- *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invocation is  normally  taken  care  of  automatically by the *
* system at IPL time  and  for  each  JCL  statement processed by Job *
* Control.   However,  the  following  JCC  statements  (executed  in *
* response to a JCL  "PAUSE"  statement  in  the BG partition) may be *
* useful:                                                             *
*                                                                     *
*      JCLEXIT DISABLE,$JOBEX00                                       *
*                                                                     *
*      JCLEXIT ENABLE,$JOBEX00                                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
COMREG   MAPCOMR EQU=YES               PARTITION COMMUNICATION REGION
         USING COMREG,R1               ESTABLISH REG 1 AS BASE
* =================================================================== *
         EJECT
* =================================================================== *
SYSCOM   SYSCOM                        SYSTEM COMMUNICATION REGION
         USING SYSCOM,R2               ESTABLISH REG 2 AS BASE
* =================================================================== *
         EJECT
* =================================================================== *
JCVTBL   DSECT                         JOB CONTROL VECTOR TABLE
         USING JCVTBL,R3               ESTABLISH REG 3 AS BASE
* ------------------------------------------------------------------- *
JCOPER   DS    CL7
JCINFO   DS    CL3
* =================================================================== *
         EJECT
* =================================================================== *
JCLBUF   DSECT                         JCL BUFFER
         USING JCLBUF,R4               ESTABLISH REG 4 AS BASE
* ------------------------------------------------------------------- *
JCLDATA  DS    0C
* =================================================================== *
         EJECT
* =================================================================== *
WRKSPACE DSECT                         EXIT WORK SPACE
         USING WRKSPACE,R8             ESTABLISH REG 8 AS BASE
* ------------------------------------------------------------------- *
WRKPID   DS    H                       OWNING PARTITION ID
WRKLENG  DS    H                       LENFLD FOR GETSYMB
WRKREGS  DS    15F                     REGISTER SAVEAREA
WRKOPTB  DS    XL1                     OPTION BYTE
WRKUSLOG EQU   X'80'                    USER SPECIFIED "LOG"
WRKTERMF EQU   X'40'                    JOB TERMINATION IN PROGRESS
WRKPARM  DS    CL7                     PARMNAME FOR GETSYMB
WRKVALU  DS    CL50                    VALBUF FOR GETSYMB
WRKALGN  DS    CL2                     (ALIGNMENT/PADDING)
WRKAREA  DS    XL100                   AREA FOR GETSYMB
         DS    0D
WRKSPLN  EQU   *-WRKSPACE              LENGTH OF ONE WORKSPACE
         ORG   WRKSPACE
WRKSPDUP DS    48XL(WRKSPLN)           48 PARTITION WORKSPACES
WRKSPTLN EQU   *-WRKSPACE              TOTAL LENGTH OF ALL WORKSPACES
WRKSPCNT EQU   WRKSPTLN/WRKSPLN-1      WORKSPACE COUNT - 1
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* ------------------------------------------------------------------- *
         COPY  IF
         COPY  WHILE
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
R0       EQU   0                       'SDOS'
R1       EQU   1                       COMREG DSECT BASE REG
R2       EQU   2                       SYSCOM DSECT BASE REG
R3       EQU   3                       JCVTBL DSECT BASE REG
R4       EQU   4                       JCLBUF DSECT BASE REG
R5       EQU   5                       NUMBER OF CONTINUATION LINES
R6       EQU   6                       ANCHOR ADDRESS FOR DYN STORAGE
R7       EQU   7                       WORK REG
R8       EQU   8                       WRKSPACE DSECT BASE REG
R9       EQU   9                       WORK REG
R10      EQU   10                      WORK REG
R11      EQU   11                      WORK REG
R12      EQU   12                      $JOBEX00 CSECT BASE REG
R13      EQU   13                      WORK REG
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
$JOBEX00 CSECT
         USING $JOBEX00,R15            ESTABLISH TEMP BASE REGISTER
* ------------------------------------------------------------------- *
         J     START                   BRANCH AROUND CORE EYE-CATCHER
*
DATA     LOCTR
         DC    CL8'$JOBEX00'           PROGRAM
         DC    CL4' 1.3'               VERSION
         DC    CL4'DLC'                AUTHOR
         DC    CL8'(C)JUL92'           DATE
*
CODE     LOCTR
START    DS    0H
         DROP  15                      DISESTABLISH TEMP BASE REGISTER
         LR    R12,R15                 LOAD PROGRAM BASE REGISTER
         USING $JOBEX00,R12            ESTABLISH PERMANENT BASE REG
*
* CHECK IF EXIT ROUTINE HAS DISABLED ITSELF
*
         IF    STORAGE,EQ,X'80',BC,FINISH
         ENDIF
*
* CHECK TO MAKE SURE ENOUGH STORAGE IS AVAILABLE
*
         IF    IJBNPART,(HT,CLC),NPARTS
          WTO  'STEPLOG: WORK SPACE TOO SMALL FOR NPARTS'
          NI   STORAGE,X'80'           DISABLE EXIT ROUTINE
          J    FINISH                  SKIP ALL PROCESSING
         ENDIF
*
* FIRST TIME, ALLOCATE GETVIS STORAGE AND INITIALIZE IT
*
         IF    R6,(ZERO,LTR),R6        IF FIRST TIME
          LR   R7,R1                    SAVE COMREG ADDRESS
          L    R0,WSLEN
          GETVIS SVA=YES,LOC=BELOW,PAGE=NO
          IF   R15,(NZERO,LTR),R15
           WTO 'STEPLOG: FAILED TO ACQUIRE GETVIS STORAGE'
           NI  STORAGE,X'80'           DISABLE EXIT ROUTINE
           J   FINISH                  SKIP ALL PROCESSING
          ENDIF
          LR   R6,R1                   SAVE ADDRESS OF FIRST WORKSPACE
          LR   R0,R1                   INITIALIZE WORKSPACES FOR
          L    R1,WSLEN                 TARGET LENGTH OF STORAGE AREA
          XR   R8,R8                     USING DUMMY SOURCE ADDRESS
          XR   R9,R9                      AND ZERO SOURCE LENGTH
          MVCL R0,R8                       AND LOW-VALUE PADDING BYTE
          WTO  'STEPLOG: GETVIS ACQUIRED AND INITIALIZED'
          LR   R1,R7                    RESTORE COMREG ADDRESS
         ENDIF
*
* FIND THIS PARTITION'S WORKSPACE OR SETUP AN UNUSED ONE
*
         LR    R8,R6                   ADDRESS OF FIRST WORKSPACE
         LA    R7,WRKSPCNT             NO. OF WORKSPACES-1 AVAILABLE
         WHILE R7,(PLUS,LTR),R7,AND,WRKPID,(NE,CLC),PID
          IF   WRKPID,(EQ,CLC),=H'0'    NEW WORKSPACE FOUND?
           WEXIT                         USE THIS WORKSPACE
          ENDIF                         ENDIF
          LA   R8,WRKSPLN(,R8)          ADDRESS OF NEXT WORKSPACE
          BCTR R7,0                     DECREMENT WORKSPACE COUNT
         WEND                          CONTINUE WORKSPACE SCAN
         MVC   WRKPID,PID              SET OWNER OF THIS WORKSPACE
*
* IF SCANCEL ON AND ANY OF THESE CONDITIONS EXIST (NOT ZERO), PAUSE JOB
*
         IF STDOPT2,(ON,TM),OPTSCNCL   IF SCANCEL OPTION IS ON
          IF   JCSW1,(Z,TM),JOBEND      IF  NOT 'JOB BEING FLUSHED'
          AND  JCSW7,(Z,TM),OPCNCL      AND NOT 'OPERATOR CANCEL'
          AND  JCSW7,(Z,TM),IJBJCCNL    AND NOT 'JOB CONTROL CANCEL'
          AND  JCSW8,(Z,TM),IJBCNCPD    AND NOT 'CANCEL PENDING'
          AND  JCSW8,(Z,TM),IJBABTRM    AND NOT 'JOB CONTROL ABEND'
           NI  WRKOPTB,X'FF'-WRKTERMF    RESET JOB TERMINATION FLAG
          ELSE                          ELSE
           IF  WRKOPTB,(NOT,TM),WRKTERMF IF JOB TERMINATION NOT SET
            OI WRKOPTB,WRKTERMF           WRITE TO OPERATOR ONLY ONCE
            LR R7,R1                      SAVE COMREG ADDRESS
            WTO 'STEPLOG: JOB TERMINATION DETECTED'
            LR R1,R7                      RESTORE COMREG ADDRESS
           ENDIF                         ENDIF
          ENDIF                         ENDIF
          IF   WRKOPTB,(ON,TM),WRKTERMF IF DURING TERMINATION
           OI  JCSW1,IJBPAUSE            TURN ON PAUSE AT EOJ
           OI  JCSW4,X'20'               AND PAUSE AT END OF STEP
          ENDIF                         ENDIF
         ENDIF                         ENDIF SCANCEL ON
*
* DETERMINE WHETHER SOMEONE HAS LEFT LOGGING ON OR I TURNED IT ON
*
         IF    JCOPER(3),(EQ,CLC),=C'LOG'
          OI   WRKOPTB,WRKUSLOG         DON'T TURN OFF LOGGING
         ENDIF
*
         IF    JCSW1,(ON,TM),JCLOGMSK
         AND   WRKOPTB,(NOT,TM),WRKUSLOG
          XI   JCSW1,JCLOGMSK          TURN OFF LOGGING
         ENDIF
*
         IF    JCOPER(5),(EQ,CLC),=C'NOLOG',OR,                        X
               JCOPER(4),(EQ,CLC),=C'EXEC'
          NI   WRKOPTB,X'FF'-WRKUSLOG  OK TO TURN OFF LOGGING NOW
         ENDIF
*
* DETERMINE IF THIS STATEMENT NEEDS RIGHT-TO-LEFT SCANNING OR NOT
*
         IF    JCOPER(2),(EQ,CLC),=C'IF',BC,SYMBOLS
         ENDIF
         IF    JCOPER(7),(EQ,CLC),=C'SETPARM',BC,SYMBOLS
         ENDIF
         IF    JCOPER(5),(EQ,CLC),=C'PAUSE',BC,SYMBOLS
         ENDIF
         IF    JCLDATA(2),(EQ,CLC),=C'* ',BC,SYMBOLS
         ENDIF
         IF    JCLDATA(2),(NE,CLC),=C'/.',BC,FINISH
         ENDIF
*
* DETERMINE IF A LABEL STATEMENT HAS COMMENTS OR NOT
*
         LA    R7,JCLDATA+2            SCAN FROM COLUMN 3
         LA    R9,JCLDATA+70                TILL COLUMN 71
*
BLNL     EQU   X'BF'                   MASK FOR BLANKS AND NULLS
*
         WHILE R7,(LT,CR),R9,AND,0(R7),(Z,TM),BLNL
          LA   R7,1(,R7)               FIND THE START OF THE LABEL
         WEND
         IF    R7,(EQ,CR),R9,BC,FINISH
         ENDIF
         WHILE R7,(LT,CR),R9,AND,0(R7),(NZ,TM),BLNL
          LA   R7,1(,R7)               FIND THE END OF THE LABEL
         WEND
         IF    R7,(EQ,CR),R9,BC,FINISH
         ENDIF
         WHILE R7,(LT,CR),R9,AND,0(R7),(Z,TM),BLNL
          LA   R7,1(,R7)               FIND THE START OF A COMMENT
         WEND
         IF    R7,(EQ,CR),R9,BC,FINISH
         ENDIF
*
* LABEL STATEMENT HAS COMMENTS, CAUSE IT TO LOG ON THE CONSOLE
*
         OI    JCSW1,JCLOGMSK          WRITE STATEMENT TO SYSLOG
*
* ISOLATE SYMBOLICS IN STATEMENT AND PERFORM REVERSE RESOLUTION
*
SYMBOLS  DS    0H
         AIF   ('&DBGOPT' EQ 'OFF').SKIP1
         MVC   DBGRSLB,=C'ORIGINL'     MOVE ORIGINAL LABEL TO DEBUG MSG
         MVC   DBGRSLT,JCLDATA         MOVE ORIGINAL VALUE TO DEBUG MSG
         WTO   MF=(E,DBGRSL)           ISSUE WTO FROM STORAGE LOCATION
.SKIP1   ANOP
*
         LA    R7,JCLDATA+2            SCAN FROM COLUMN 3
         LA    R9,JCLDATA+70                THRU COLUMN 71
         WHILE R9,(NLT,CR),R7          WHILE END PTR >= COL 3
         LR    R10,R9                  GET SCAN START POSITION
         SR    R10,R7                  MINUS SCAN END POSITION
         IF    R10,(Z,EXRL),FINDAMP    SCAN BACKWARD FOR AMPERSAND
          WEXIT                         NO AMPERSAND FOUND; ALL DONE
         ENDIF                         ENDIF
         LR    R9,R1                   SAVE SCAN STOP POSITION
*
         IF    0(R1),EQ,C'&&'          IF AMPERSAND FOUND
         AIF   ('&DBGOPT' EQ 'OFF').SKIP2
          ST   R1,WRKREGS+4            SAVE REGISTER 1
          WTO  'STEPLOG: AMPERSAND FOUND'
          L    R1,WRKREGS+4            RESTORE REGISTER 1
.SKIP2   ANOP
*
          LA   R7,1(,R9)               POINT TO START OF SYMBOL NAME
          XR   R1,R1                   CLEAR TRT HIT REG
          LA   R10,JCLDATA+70          LAST SCANNABLE BYTE (COL 71)
          SR   R10,R7                  REMAINING LENGTH PAST AMPERSAND
          IF   R10,(H,CH),=H'8'        IF MORE THAN 8 BYTES TO SCAN
           LHI R10,8                    CAP SCAN LENGTH AT 8 BYTES
          ENDIF                        ENDIF
          SHI  R10,1                   ADJUST FOR ZERO-REF EXECUTE
          IF   R10,(Z,EXRL),FINDSYM    SCAN FORW TO END OF SYMBOL NAME
           LA  R1,1(R10,R7)             DELIM NOTFND: POINT PAST SCAN
          ENDIF                        ENDIF
          LR   R10,R1                  END OF SYMBOL MINUS START
          SR   R10,R7                   OF SYMBOL IS LEN OF SYMBOL
*
          IF   R10,(P,LTR),R10,AND,    SYMBOL FOUND AND                X
               R10,(NH,CH),=H'7'        LENGTH NOT MORE THAN 7?
           MVI WRKPARM,C' '            CLEAR PARM AREAS TO BLANKS
           MVC WRKPARM+1(L'WRKPARM-1+L'WRKVALU),WRKPARM
           SHI R10,1                   ADJUST FOR ZERO-REF EXECUTE
           EX  R10,MVCSYMB             MOVE SYMBOL NAME INTO PARM AREA
           STM R0,R14,WRKREGS          SAVE REGISTERS
         AIF   ('&DBGOPT' EQ 'OFF').SKIP3
           MVC DBGNAME,WRKPARM         MOVE SYMBOL NAME TO DEBUG MSG
           WTO MF=(E,DBGNAM)           ISSUE WTO FROM STORAGE LOCATION
.SKIP3   ANOP
* The following destroys registers 13, 14, 15, 0, and 1
           GETSYMB AREA=WRKAREA,       RETRIEVE, FOR THIS              X
               PARMNAM=WRKPARM,         SYMBOL NAME, ITS               X
               VALBUF=WRKVALU,           REPLACMENT VALUE AND          X
               LENFLD=WRKLENG             ASSOCIATED LENGTH
           LM  R0,R14,WRKREGS          RESTORE REGISTERS
           IF  R15,(NZERO,LTR),R15     IF RESOLUTION ERROR
            CVD R15,WRKVALU             PUT PACKED RC IN WORK VALUE
            MVC JCLDATA+72(8),=C'SYM=0000'  PUT TEXT IN JCL 73-80
            UNPK JCLDATA+76(4),WRKVALU+5(3) ADD ZONED RC TO JCL
            OI JCLDATA+79,X'F0'             REMOVE ANY SIGN
         AIF   ('&DBGOPT' EQ 'OFF').SKIP4
            ST R1,WRKREGS+4              SAVE REGISTER 1
            WTO 'STEPLOG: SYMBOL NOT FOUND'
            L  R1,WRKREGS+4              RESTORE REGISTER 1
.SKIP4   ANOP
           ELSE                        ELSE RESOLVED SUCCESSFULLY
         AIF   ('&DBGOPT' EQ 'OFF').SKIP5
            ST R1,WRKREGS+4             SAVE REGISTER 1
            MVC DBGVALU,WRKVALU         MOVE SYMBOL VALUE TO DEBUG MSG
            WTO MF=(E,DBGVAL)           ISSUE WTO FROM STORAGE LOCATION
            L  R1,WRKREGS+4             RESTORE REGISTER 1
.SKIP5   ANOP
            MVI WRKALGN,C' '            CLEAR AN AREA (50+21=71)
            MVC WRKALGN+1(20),WRKALGN    FOR CONCATENATION WORK
            LA R11,WRKVALU              POINT TO RETURNED VALUE
            AH R11,WRKLENG              TARGET ADDR FOR TAIL TEXT
            LR R7,R1                    POINT TO CHAR AFTER SYMBOL
            IF 0(R7),EQ,C'.'            IF REM COMMENT STARTS WITH '.'
             AHI R7,1                    IT AS PART OF SYMBOL NAME
            ENDIF                       ENDIF
            LA R13,JCLDATA+71           POINT PAST END OF DATA
            SR R13,R7                   CALC LENGTH OF TRAILING TEXT
            IF R13,(P,LTR),R13          IF TRAILING TEXT AVAILABLE
         AIF   ('&DBGOPT' EQ 'OFF').SKIP6
             ST R1,WRKREGS+4             SAVE REGISTER 1
             MVC DBGREG7,0(R7)           MOVE CHARACTER TO DEBUG MSG
             MVC DBGREG9,0(R9)           MOVE CHARACTER TO DEBUG MSG
             CVD R10,DBG8                CONVERT R10 TO DECIMAL
             UNPK DBGREG10,DBG8+5(3)     CONVERT TO ZONED
             OI DBGREG10+3,X'F0'         REMOVE ANY SIGN
             CVD R13,DBG8                CONVERT R13 TO DECIMAL
             UNPK DBGREG13,DBG8+5(3)     CONVERT TO ZONED
             OI DBGREG13+3,X'F0'         REMOVE ANY SIGN
             LH R1,WRKLENG               GET WRKLENG
             CVD R1,DBG8                 CONVERT R1 TO DECIMAL
             UNPK DBGWRKL,DBG8+5(3)      CONVERT TO ZONED
             OI DBGWRKL+3,X'F0'          REMOVE ANY SIGN
             WTO MF=(E,DBGREG)           ISSUE WTO FROM STORAGE
             L R1,WRKREGS+4              RESTORE REGISTER 1
.SKIP6   ANOP
             SHI R13,1                   ADJUST FOR ZERO-REF EXECUTE
             EXRL R13,MVCCMNT            SHIFT TRAILING TEXT (R11, R7)
            ENDIF                       ENDIF
            LA R10,1(,R13)              INCR. CMNT LENGTH FROM ZERO REF
            AH R10,WRKLENG              PLUS LENGTH OF SYMBOL VALUE
            LA R13,JCLDATA+71           GET ADDRESS OF END OF COMMENT
            SR R13,R9                   LENGTH OF COMMENT AVAILABLE
            IF R10,(LT,CR),R13          IF L'NEEDED < L'AVAILABLE
             LR R13,R10                  USE ONLY L'NEEDED
            ENDIF                       ENDIF
            IF R13,(P,LTR),R13          IF SYMBOL VALUE AVAILABLE
             SHI R13,1                   ADJUST FOR ZERO-REF EXECUTE
             EXRL R13,MVCVALU            MOVE VALUE OVER AMPERSAND (R9)
            ENDIF                       ENDIF
         AIF   ('&DBGOPT' EQ 'OFF').SKIP7
            MVC DBGRSLB,=C'RESULT.'     MOVE RESULT LABEL TO DEBUG MSG
            MVC DBGRSLT,JCLDATA         MOVE RESULT VALUE TO DEBUG MSG
            WTO MF=(E,DBGRSL)           ISSUE WTO FROM STORAGE LOCATION
.SKIP7   ANOP
           ENDIF                       ENDIF R15<>ZERO
          ENDIF                        ENDIF SYMBOL FOUND
         ENDIF                         ENDIF AMPERSAND FOUND
*
         LA    R7,JCLDATA+2            RESTORE LEFT BOUNDARY (COL 3)
         SHI   R9,1                    POINT TO BYTE BEFORE AMPERSAND
         WEND                          CONTINUE REVERSE SCAN
* ------------------------------------------------------------------- *
FINISH   XR    R15,R15                 CLEAR RC FOR NORMAL PROCESSING
         BR    R14                     RETURN TO CALLER
*
FINDAMP  TRTR  0(0,R9),AMPERSAND       REVERSE SCAN FOR AMPERSAND
FINDSYM  TRT   0(0,R7),NONALPHANUM     FORWARD SCAN FOR NON-ALPHANUM
MVCSYMB  MVC   WRKPARM(0),0(R7)        MOVE SYMBOL NAME TO PARM BUFFER
MVCCMNT  MVC   0(0,R11),0(R7)          SHIFT TRAILER PAST REPLACEMENT
MVCVALU  MVC   0(0,R9),WRKVALU         OVERWRITE AT AMPERSAND POSITION
         CORG  0,256
* =================================================================== *
         EJECT
* =================================================================== *
DATA     LOCTR
STORAGE  DS    0D
* ------------------------------------------------------------------- *
AMPERSAND DC   256XL1'00'              ACCEPT EVERYTHING
         ORG   AMPERSAND+C'&&'          EXCEPT AMPERSAND
         DC    X'FF'
         ORG
NONALPHANUM DC 256XL1'FF'              REJECT EVERYTHING
         ORG   NONALPHANUM+C'$'         EXCEPT NATIONALS,
         DC    X'00'
         ORG   NONALPHANUM+C'#'
         DC    X'0000'
         ORG   NONALPHANUM+C'a'         ...LOWER-CASE, AND
         DC    X'000000000000000000'
         ORG   NONALPHANUM+C'j'
         DC    X'000000000000000000'
         ORG   NONALPHANUM+C's'
         DC    X'0000000000000000'
         ORG   NONALPHANUM+C'A'         ...UPPER-CASE, AND
         DC    X'000000000000000000'
         ORG   NONALPHANUM+C'J'
         DC    X'000000000000000000'
         ORG   NONALPHANUM+C'S'
         DC    X'0000000000000000'
         ORG   NONALPHANUM+C'0'         ...NUMBERS.
         DC    X'00000000000000000000'
         ORG
*
WSLEN    DC    A(WRKSPTLN)             WORKSPACE LENGTH FOR GETVIS
NPARTS   DC    YL2(WRKSPTLN/WRKSPLN)   NUMBER OF WORK SPACES ALLOCATED
*
         AIF   ('&DBGOPT' EQ 'OFF').SKIP8
DBG8     DS    PL8
DBGNAM   WTO   'STEPLOG: SYMBOL NAME  FOUND: "       "',MF=L
DBGNAME  EQU   DBGNAM+34,7,C'C'
DBGVAL   WTO   'STEPLOG: SYMBOL VALUE FOUND: "        "',MF=L
DBGVALU  EQU   DBGVAL+34,8,C'C'
DBGREG   WTO   'STEPLOG: R7=" ",R9=" ",R10=0000,R13=0000,WRKL=0000',   X
               MF=L
DBGREG7  EQU   DBGREG+17,1,C'C'
DBGREG9  EQU   DBGREG+24,1,C'C'
DBGREG10 EQU   DBGREG+31,4,C'C'
DBGREG13 EQU   DBGREG+40,4,C'C'
DBGWRKL  EQU   DBGREG+50,4,C'C'
DBGRSL   WTO   'RESULT.:                                               X
                   ',MF=L
DBGRSLB  EQU   DBGRSL+4,7,C'C'
DBGRSLT  EQU   DBGRSL+13,50,C'C'
.SKIP8   ANOP
*
         LTORG
         CORG  0,256
* =================================================================== *
         END
