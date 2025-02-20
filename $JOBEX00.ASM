JCL      TITLE '$JOBEX00 - JCL USER EXIT ROUTINE - z/VSE 4.2.1'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      J O B   C O N T R O L   U S E R   E X I T   R O U T I N E      *
*                                                                     *
*                                                                     *
* DAPSCO Information Systems                          Written by:     *
* 3110 Kettering Blvd.                             Dave Leonard Clark *
* Dayton, OH  45439                                  (501)750-8248    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This routine causes "/."  JCL  label  statements to log on the *
* system console if the  statement  contains comments after the label *
* field.   In addition,  any comments specified  ("*" JCL comment and *
* "// PAUSE" statements,  too) may  contain symbolic parameters which *
* this  routine  will  resolve  appropriately.   This  is  useful for *
* indicating job "steps"  both  to  inform  the  operator as to which *
* "step" the job  is  in  and  to  provide  branch  points for use in *
* conditional JCL, e.g.  automatic recovery or debugging functions.   *
*      In addition,  this routine will  stop runaway "LOG" statements *
* (in the  case  of  omitted  "NOLOG"  statements)  from  causing all *
* subsequent JCL  statements  in  the  partition  (not  just  for the *
* problem JOB) from listing  on  the console.   This will occur after *
* the  first  "EXEC"  statement  following  the  "LOG"  statement  in *
* question.                                                           *
*                                                                     *
* =================================================================== *
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
* It must also reside in the  system  library at ipl time in order to *
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
* =================================================================== *
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
* =================================================================== *
JCOPER   DS    CL7
JCINFO   DS    CL3
* =================================================================== *
         EJECT
* =================================================================== *
JCLBUF   DSECT                         JCL BUFFER
         USING JCLBUF,R4               ESTABLISH REG 4 AS BASE
* =================================================================== *
JCLDATA  DS    0C
* =================================================================== *
         EJECT
* =================================================================== *
WRKSPACE DSECT                         EXIT WORK SPACE
         USING WRKSPACE,R8             ESTABLISH REG 8 AS BASE
* =================================================================== *
WRKPID   DS    H                       OWNING PARTITION ID
WRKLENG  DS    H                       LENFLD FOR GETSYMB
WRKREGS  DS    15F                     REGISTER SAVEAREA
WRKOPTB  DS    XL1                     OPTION BYTE
WRKUSLOG EQU   X'80'                    USER SPECIFIED "LOG"
WRKTERMF EQU   X'40'                    JOB TERMINATION IN PROGRESS
WRKPARM  DS    CL7                     PARMNAME FOR GETSYMB
WRKVALU  DS    CL50                    VALBUF FOR GETSYMB
WRKALGN  DS    CL2                     (ALIGNMENT)
WRKAREA  DS    XL100                   AREA FOR GETSYMB
         DS    0D
WRKSPLN  EQU   *-WRKSPACE              LENGTH OF ONE WORKSPACE
         ORG   WRKSPACE
WRKSPDUP DS    48XL(WRKSPLN)           48 PARTITION WORKSPACES
WRKSPCT  EQU   (*-WRKSPACE)/WRKSPLN-1  WORKSPACE COUNT - 1
WRKSPTLN EQU   *-WRKSPACE              TOTAL LENGTH OF ALL WORKSPACES
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         COPY  IF
         COPY  WHILE
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* =================================================================== *
R0       EQU   0                       'SDOS'
R1       EQU   1                       COMREG DSECT BASE REG
R2       EQU   2                       SYSCOM DSECT BASE REG
R3       EQU   3                       JCVTBL DSECT BASE REG
R4       EQU   4                       JCLBUF DSECT BASE REG
R5       EQU   5                       NUMBER OF CONTINUATION LINES
R6       EQU   6                       ANCHOR ADDRESS FOR STORAGE
R7       EQU   7
R8       EQU   8                       WRKSPACE DSECT BASE REG
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12                      $JOBEX00 CSECT BASE REG
R13      EQU   13
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
$JOBEX00 CSECT
         USING $JOBEX00,R15            ESTABLISH TEMP BASE REGISTER
* =================================================================== *
         B     START                   BRANCH AROUND CORE EYE-CATCHER
*
         DC    CL8'$JOBEX00'           PROGRAM
         DC    CL4' 1.2'               VERSION
         DC    CL4'DLC'                AUTHOR
         DC    CL8'(C)JUL92'           DATE
*
START    DS    0H
         DROP  15                      DISESTABLISH TEMP BASE REGISTER
         LR    R12,R15                 LOAD PROGRAM BASE REGISTER
         USING $JOBEX00,R12            ESTABLISH PERMANENT BASE REG
         XR    R15,R15                 CLEAR RETURN CODE REGISTER
*
* CHECK IF EXIT ROUTINE HAS DISABLED ITSELF
*
         IF    TRTABLE,EQ,X'80',BC,FINISH
         ENDIF
*
* CHECK TO MAKE SURE ENOUGH STORAGE IS AVAILABLE
*
         IF    IJBNPART,(HT,CLC),NPARTS
          WTO  'STEPLOG: WORK SPACE TOO SMALL FOR NPARTS'
          NI   TRTABLE,X'80'           DISABLE EXIT ROUTINE
          B    FINISH                  SKIP ALL PROCESSING
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
           NI  TRTABLE,X'80'           DISABLE EXIT ROUTINE
           B   FINISH                  SKIP ALL PROCESSING
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
         LA    R7,WRKSPCT              NO. OF WORKSPACES-1 AVAILABLE
         WHILE R7,(PLUS,LTR),R7,AND,WRKPID,(NE,CLC),PID
          IF   WRKPID,(EQ,CLC),=H'0'
           WEXIT                         NEW PARTITION WORKSPACE
          ENDIF
          LA   R8,WRKSPLN(,R8)          ADDRESS OF NEXT WORKSPACE
          BCTR R7,0                     DECREMENT WORKSPACE COUNT
         WEND
         MVC   WRKPID,PID              SET OWNER OF THIS WORKSPACE
*
* IF SCANCEL ON AND ANY OF THESE CONDITIONS EXIST (NOT ZERO), PAUSE JOB
*
         IF    STDOPT2,(ON,TM),OPTSCNCL
          IF   JCSW1,(Z,TM),JOBEND              JOB TO BE FLUSHED
          AND  JCSW7,(Z,TM),OPCNCL+IJBJCCNL     OPERATOR OR JC CANCEL
          AND  JCSW8,(Z,TM),IJBCNCPD+IJBABTRM   CANCEL PENDING OR ABEND
           NI  WRKOPTB,X'FF'-WRKTERMF    RESET JOB TERMINATION FLAG
          ELSE                          ELSE
           OI  JCSW1,IJBPAUSE            TURN ON PAUSE AT EOJ
           OI  JCSW4,X'20'               AND PAUSE AT END OF STEP
           OI  JCSW8,IJBCNCPD+IJBRCCNC   AND FAKE OPERATOR CANCEL
           IF  WRKOPTB,(NOT,TM),WRKTERMF IF JOB TERMINATION NOT SET
            LR R7,R1                      SAVE COMREG ADDRESS
            WTO 'STEPLOG: JOB TERMINATION DETECTED'
            LR R1,R7                      RESTORE COMREG ADDRESS
            OI WRKOPTB,WRKUSLOG           WRITE TO OPERATOR ONLY ONCE
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         ENDIF                         ENDIF
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
* DETERMINE IF THIS IS A STATEMENT I NEED TO PROCESS OR NOT
*
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
         LA    R9,JCLDATA+71                TILL COLUMN 72
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
* ISOLATE SYMBOLIC PARAMETERS IN COMMENTS AND PERFORM RESOLUTION
*
SYMBOLS  DS    0H
         LA    R7,JCLDATA+2            SCAN FROM COLUMN 3
         LA    R9,JCLDATA+71                THRU COLUMN 71
*
         WHILE R7,(LT,CR),R9           SCAN COMMENTS FOR SYMBOLICS
*
         WHILE R7,(LT,CR),R9,AND,0(R7),NE,C'&&'
          LA   R7,1(,R7)               FIND THE START OF A SYMBOL
         WEND
         LA    R7,1(,R7)               POINT TO CHARACTER AFTER '&'
         IF    R7,(NLT,CR),R9
          WEXIT                        NO ROOM FOR SYMBOL NAME
         ENDIF
*
         XR    R1,R1                   CLEAR ADDRESS REG FOR "TRT"
         LR    R10,R9                  CALCULATE "TRT" LENGTH
         SR    R10,R7                   COMMENT END - SYMBOL START
         BCTR  R10,0                     LESS 1 FOR ZERO REFERENCE
         EX    R10,TRTCMNT             SEARCH FOR END OF SYMBOL NAME
         LR    R9,R7                   SAVE START OF SYMBOL NAME
         LR    R7,R1                   GET ADDRESS WHERE "TRT" ENDED
         LR    R10,R7                  POINT AFTER END OF SYMBOL NAME
         SR    R10,R9                  CALCULATE LENGTH OF SYMBOL NAME
*
         IF    R10,(PLUS,LTR),R10,AND,R10,(NH,CH),=H'7'
          MVI  WRKPARM,C' '  CLEAR PARM AND VALUE AREAS TO BLANK PAD'EM
          MVC  WRKPARM+1(L'WRKPARM-1+L'WRKVALU),WRKPARM
          BCTR R10,0                   DECR. LENGTH FOR ZERO REFERENCE
          EX   R10,MVCSYMB             MOVE SYMBOL NAME INTO PARM AREA
          STM  R0,R14,WRKREGS          SAVE REGISTERS
* THE FOLLOWING DESTROYS REGS 13, 14, 15, 0, AND 1
          GETSYMB AREA=WRKAREA,        RETRIEVE, FOR THIS              *
               PARMNAM=WRKPARM,         SYMBOL NAME, ITS               *
               VALBUF=WRKVALU,           REPLACMENT VALUE AND          *
               LENFLD=WRKLENG             ASSOCIATED LENGTH
          LM   R0,R14,WRKREGS          RESTORE REGISTERS
          IF   R15,(NZERO,LTR),R15     IF ERROR
           CVD R15,WRKVALU              CONVERT RETURN CODE TO DECIMAL
           MVC JCLDATA+72(8),=C'SYM=0000'  SHOW RETURN CODE IN 73-80
           UNPK JCLDATA+76(4),WRKVALU+5(3) UNPACK INTO 77-80
           OI  JCLDATA+79,X'F0'            CLEAR ANY SIGN IN 80
          ELSE                         ELSE
           MVI WRKALGN,C' '             CLEAR AN AREA (50+22=72)
           MVC WRKALGN+1(21),WRKALGN     FOR CONCATENATION WORK
           LA  R11,WRKVALU              GET ADDRESS OF SYMBOL VALUE
           AH  R11,WRKLENG              CALC. WHERE TO PUT REM. COMMENT
           IF  0(R7),EQ,C'.'            IF REM. COMMENT STARTS WITH '.'
            LA R7,1(,R7)                 TREAT IT AS PART OF SYMB. NAME
           ENDIF                        ENDIF
           LA  R13,JCLDATA+71           GET ADDRESS OF END OF COMMENT
           SR  R13,R7                   LENGTH OF REMAINING COMMENT
           BCTR R13,0                   DECR. FOR ZERO REFERENCE
           EX  R13,MVCCMNT              CONCAT. REM. COMMENT & SYMB VAL
           BCTR R9,0                    POINT TO "&" OF SYMBOL NAME
           LA  R13,1(,R13)              INCR. CMNT LENGTH FROM ZERO REF
           AH  R13,WRKLENG              PLUS LENGTH OF SYMBOL VALUE
           LA  R10,2(,R10)              LENGTH OF SYMBOLIC TO REPLACE
           IF  R10,(PLUS,SH),WRKLENG,   IF L'VALUE < L'SYMBOLIC        X
               OR,R10,(EQ,CH),=H'0'     OR L'VALUE IS ZERO
            AR R13,R10                   ADD SOME PADDING
            SR R7,R10                    AND ADJUST FOR NEXT SCAN LOC.
            BCTR R7,0                    DECR. FOR ZERO REFERENCE
           ENDIF                        ENDIF
           LA  R10,JCLDATA+71           GET ADDRESS OF END OF COMMENT
           SR  R10,R9                   LENGTH OF COMMENT AVAILABLE
           IF  R13,(LT,CR),R10          IF L'NEEDED < L'AVAILABLE
            LR R10,R13                   USE ONLY L'NEEDED
           ENDIF                        ENDIF
           BCTR R10,0                   DECR. LENGTH FOR ZERO REF.
           EX  R10,MVCVALU              MOVE SYMBOL VALUE INTO COMMENT
          ENDIF                        ENDIF
         ENDIF
*
         LA    R9,JCLDATA+71           RE-POINT TO COLUMN 71
         WEND                          CONTINUE SCAN
*
FINISH   XR    R15,R15                 CLEAR RC FOR NORMAL PROCESSING
         BR    R14                     RETURN TO CALLER
*
TRTCMNT  TRT   0(0,R7),TRTABLE         (SEE "EX" INSTRUCTION ABOVE)
MVCSYMB  MVC   WRKPARM(0),0(R9)        (SEE "EX" INSTRUCTION ABOVE)
MVCCMNT  MVC   0(0,R11),0(R7)          (SEE "EX" INSTRUCTION ABOVE)
MVCVALU  MVC   0(0,R9),WRKVALU         (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
TRTABLE  DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  0.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  1.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  2.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  3.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  4.
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF'  5.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  6.
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF'  7.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  8.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  9.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  A.
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  B.
         DC    X'FF000000000000000000FFFFFFFFFFFF'  C.
         DC    X'FF000000000000000000FFFFFFFFFFFF'  D.
         DC    X'FFFF0000000000000000FFFFFFFFFFFF'  E.
         DC    X'00000000000000000000FFFFFFFFFFFF'  F.
*                .0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F
WSLEN    DC    A(WRKSPTLN)             WORKSPACE LENGTH FOR GETVIS
NPARTS   DC    YL2(WRKSPTLN/WRKSPLN)   NUMBER OF WORK SPACES ALLOCATED
         LTORG
* =================================================================== *
         END
