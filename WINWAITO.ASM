WIN      TITLE 'WINWAITO - WAIT FOR CICS/OLTP ACB TO OPEN'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     W A I T   F O R   C I C S / O L T P   A C B   T O   O P E N     *
*                                                                     *
*                                                                     *
* Winsupply Group Services                            Written by:     *
* 3110 Kettering Blvd.                             Dave Leonard Clark *
* Dayton, Ohio  45439                                (937)294-5331    *
*                                                                     *
* No warranty is expressed  or  implied.   Neither the author nor the *
* company is responsible for  any  loss  or damage resulting from the *
* use of this code.  Source is provided on an as-is basis.            *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This routine allows  a  job  or  program  to  determine if the *
* online partition is ready to process requests.  The caller does not *
* need to know the partition id  --  as they may make this request by *
* the POWER job name.   But,  if preferred, the 2-character partition *
* syslog id may be specified.                                         *
*                                                                     *
*      If executed from a job, this routine automatically waits up to *
* 15 minutes for the online partition's ACB to open before timing out *
* and continuing with  a  non-zero  return  code.   If  called from a *
* program,  this routine does  not  wait  (unless  it is specifically *
* requested) but indicates the online status via a return code.       *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      If supplied,  this  routine  will  validate  any  partition id *
* specified.   An invoking COBOL program  can  use the following data *
* area for passing parameters:                                        *
*                                                                     *
*      77  WINWAITO                    PIC  X(08)   VALUE 'WINWAITO'. *
*                                                                     *
*      01  WINWAITO-PARMS.                                            *
*        03  WINWAITO-LEN              PIC S9(04)   BINARY.           *
*          88  WINWAITO-BYPID                       VALUE 2.          *
*          88  WINWAITO-BYJOB                       VALUE 8.          *
*          88  WINWAITO-BYJOB-WAIT                  VALUE 9.          *
*          88  WINWAITO-BYPID-WAIT                  VALUE 9.          *
*        03  WINWAITO-DATA.                                           *
*          05  WINWAITO-JOB-OR-PID     PIC  X(08)   VALUE SPACES.     *
*          05  WINWAITO-WAIT-OPT       PIC  X(01)   VALUE SPACES.     *
*            88  WINWAITO-WAIT                      VALUE 'W'.        *
*                                                                     *
* =================================================================== *
*                                                                     *
*                              INVOCATION                             *
*                                                                     *
*      In a batch job, you would invoke this routine one of two ways: *
*                                                                     *
* // EXEC PGM=WINWAITO,SIZE=*,PARM='CICSDATA'                         *
* // IF   $RC > 0 GOTO $EOJ                                           *
*         ... or ...                                                  *
* // EXEC PGM=WINWAITO,SIZE=*,PARM='I2'                               *
* // IF   $RC > 0 GOTO $EOJ                                           *
*                                                                     *
*      In a batch program,  to  check  on  the current status of CICS *
* without waiting, you would call this routine as follows:            *
*                                                                     *
*   --1----+----2----+----3----+----4----+----5----+----6----+----7-- *
*       SET  WINWAITO-BYJOB         TO TRUE.                          *
*       MOVE 'CICSDATA'             TO WINWAITO-JOB-OR-PID.           *
*       CALL WINWAITO            USING WINWAITO-PARMS.                *
*       IF  RETURN-CODE NOT = ZERO                                    *
*           ... error handling ...                                    *
*       END-IF.                                                       *
*                                                                     *
*      To do the same thing but with a wait:                          *
*                                                                     *
*   --1----+----2----+----3----+----4----+----5----+----6----+----7-- *
*       SET  WINWAITO-BYJOB-WAIT    TO TRUE.                          *
*       MOVE 'CICSDATA'             TO WINWAITO-JOB-OR-PID.           *
*       SET  WINWAITO-WAIT          TO TRUE.                          *
*       CALL WINWAITO            USING WINWAITO-PARMS.                *
*       IF  RETURN-CODE NOT = ZERO                                    *
*           ... timeout handling ...                                  *
*       END-IF.                                                       *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             RETURN CODES                            *
*                                                                     *
* 15   one of several more severe errors, e.g.:                       *
*        length parm is invalid;                                      *
*        or job parm is invalid;                                      *
* 12   partition or job not found;                                    *
*  8   partition or job not executing DFHSIP;                         *
*  4   DFHSIP ACB not yet open (or 15-min timeout);                   *
*  0   DFHSIP ACB is open.                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
WINPARMS DSECT                    WINWAITO PARMS DUMMY SECTION
* =================================================================== *
WINLENG  DS    H                  LENGTH
WINPID   DS    0CL2               PARTITION LOG ID
WINJOBN  DS    CL8                POWER JOB NAME
WINWAIT  DS    C                  WAIT OPTION
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES                                              *
* =================================================================== *
R0       EQU   0                  PARM REG
R1       EQU   1                  PARM REG OR POWJB
R2       EQU   2
R3       EQU   3
R4       EQU   4                  PIK
R5       EQU   5                  PCB/PCE
R6       EQU   6                  COMREG
R7       EQU   7                  OTHER COMREG
R8       EQU   8                  PCBATAB
R9       EQU   9
R10      EQU   10                 WINPARM DSECT BASE REG
R11      EQU   11                 WINWAITO CSECT BASE REG
R12      EQU   12                 LE RESERVED REG
R13      EQU   13                 REGISTER SAVEAREA
R14      EQU   14                 RETURN REG
R15      EQU   15                 ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED CONSTRUCT
         COPY  WHILE                   STRUCTURED CONSTRUCT
         PRINT ON
*
*        SGLOWC DSECT=YES              LOW CORE LAYOUT
         MAPPCB ,                      PCB LAYOUT
         MAPCOMR ,                     COMREG LAYOUT
         MAPPOWJB ,                    POWER JOB INFORMATION
         DFHAFCD TYPE=DSECT
         DFHAFCS TYPE=DSECT
         DFHCSAD TYPE=DSECT
* =================================================================== *
         EJECT
* =================================================================== *
WINWAITO HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)JAN25',            X
               BASE=R11
* =================================================================== *
         L     R10,0(,R1)         GET ADDRESS OF PARAMETER
         USING WINPARMS,R10       ADDRESSABILITY FOR PARAMETER
*
         LA    R15,WINSAVE        LOAD ADDRESS OF MY SAVEAREA
         ST    R15,8(,R13)        SET NEXT SAVEAREA IN CALLER'S
         ST    R13,4(,R15)        SET PREV SAVEAREA IN MINE
         LR    R13,R15            GET ADDRESS OF MY SAVEAREA
         XR    R15,R15            CLEAR RETURN CODE REGISTER
*
* RUNNING AS MAIN PROGRAM?
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
*        L     R6,CRADDR          (FROM SGLOWC MACRO)
         L     R6,x'14'           GET MY COMREG ADDRESS FROM LOW CORE
         POP   ACONTROL
         USING COMREG,R6          ADDRESSABILITY FOR COMREG
         IF    IJBPHNAM,(EQ,CLC),PGMNAME
          SF   MAINFLG             TURN ON MAIN PGM FLAG
          SF   WAITFLG             TURN ON DEFAULT WAIT
         ELSE
          CF   MAINFLG             TURN OFF MAIN PGM FLAG
          CF   WAITFLG             TURN OFF DEFAULT WAIT
         ENDIF
         DROP  R6                 REMOVE ADDRESSABILITY
*
* VALIDATE PARAMETERS
         XR    R0,R0              SET TO ZERO
         IF    R0,(NL,CH),WINLENG IF ZEROES NOT < PARM LENGTH
          LHI  R15,15              SET RETURN CODE
          B    FINISH              GO RETURN TO CALLER
         ENDIF
         IF    WINJOBN,(NH,CLC),BLANKS IF PARM DATA NOT > BLANKS
          LHI  R15,15              SET RETURN CODE
          B    FINISH              GO RETURN TO CALLER
         ENDIF
         IF    WINWAIT,EQ,C'W'    IF WAIT REQUESTED
          SF   WAITFLG             TURN ON WAIT
         ENDIF
*
         CF    WAITMSG            TURN OFF WAIT MSG FLAG
         SF    WAITPID            TURN ON  PID MSG FLAG
         LH    R0,=AL2(60*15)     15 MINUTES, IN SECONDS
         STH   R0,WAITMAX         STORE IT
*
* SEARCH ALL PARTITIONS?
         IF    WINPID+2,HT,C' '   HAVE JOB NAME?
          CF   WAITPID             TURN OFF PID MSG FLAG
SEARCH    DS   0H                  SEARCH ALL PARTITIONS FOR JOB
          XR   R0,R0               POINT TO LOW CORE
*         USING SYSS00,R0          (ADDRESSABILITY FOR SGLOWC LAYOUT)
          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
*         L    R7,CRADDR           (FROM SGLOWC MACRO)
          L    R7,x'14'            GET MY COMREG ADDR  FROM LOW CORE
*         L    R8,APCBATAB         (FROM SGLOWC MACRO)
          L    R8,x'2C4'           GET ADDR OF PCBATAB FROM LOW CORE
          POP  ACONTROL
*         DROP R0
          LA   R8,4(,R8)           SKIP SYSTEM PCB ENTRY
          WHILE =F'-1',(NE,CLC),0(R8) WHILE NOT END OF TABLE
           IF  =F'0',(NE,CLC),0(R8) IF NOT INACTIVE ENTRY
            L  R5,0(R8)              GET ADDRESS OF PCB/PCE
            USING PCBADR,R5          ADDRESSABILITY FOR PCB/PCE
            L  R6,PCECOMRA           GET ADDRESS OF COMREG
            USING COMREG,R6          ADDRESSABILITY FOR COMREG
            IF R6,(NE,CR),R7         IF NOT MY COMREG
             L R1,PCEPOWJB            GET ADDRESS OF POWJB INFO
             USING PJBADR,R1          ADDRESSABILITY FOR POWJB INFO
             IF PJBPNAME,(EQ,CLC),WINJOBN
              MVC WINPID,PCELID        CAPTURE PARTITION ID
              MVI WINPID+2,C' '        INDICATE PID PRESENT
              WEXIT                    EXIT LOOP
             ENDIF
            ENDIF
            DROP R5,R6,R1
           ENDIF
           LA  R8,4(,R8)            POINT TO NEXT PCBATAB ENTRY
          WEND                     LOOP UNTIL END OF TABLE
          IF   WINPID+2,HT,C' '    STILL HAVE JOB NAME?
          AND  WAITFLG,(NO,TF),$NOOP  AND NO WAITING?
           LHI R15,12               SET RETURN CODE
           B   FINISH               GO RETURN TO CALLER
          ENDIF
          IF   WINPID+2,HT,C' '    STILL HAVE JOB NAME?
           LH  R0,WAITMAX          CHECK REMAINING WAIT TIME
           IF  R0,(NH,CH),=H'0'    IF WAIT TIMED OUT
            LHI R15,12              SET RETURN CODE
            B  TIMEOUT              GO RETURN TO CALLER
           ENDIF
           BAS R3,DOWAIT           WAIT 5 SECONDS AND TRY AGAIN
           B   SEARCH              GO SEARCH UNTIL JOB COMES UP
          ENDIF
         ENDIF
*
* BEGIN PROCESSING PARTITION
CHECK    DS    0H                 CHECK PID FOR JOB, PGM, AND ACB
         SF    WAITPID            TURN ON  PID MSG FLAG
         GETFLD FIELD=PIK,LOGID=WINPID,RETURN=YES
         IF    R15,(NZ,LTR),R15   IF PARTITION NOT FOUND
          IF   WAITFLG,(NO,TF),$NOOP  IF NO WAITING
           LHI R15,12              SET RETURN CODE
           B   FINISH              GO RETURN TO CALLER
          ENDIF
          LH   R0,WAITMAX          CHECK REMAINING WAIT TIME
          IF   R0,(NH,CH),=H'0'    IF WAIT TIMED OUT
           LHI R15,12               SET RETURN CODE
           B   TIMEOUT              GO RETURN TO CALLER
          ENDIF
          BAS  R3,DOWAIT           WAIT 5 SECONDS AND TRY AGAIN
          B    CHECK               GO CHECK UNTIL PTN COMES ACTIVE
         ENDIF
*
         LR    R4,R1              SAVE PIK
         GETFLD FIELD=PCBPTR,PART=(4),RETURN=YES
         IF    R15,(NZ,LTR),R15   IF NO ACTIVE PCB FOR PARTITION
          IF   WAITFLG,(NO,TF),$NOOP  IF NO WAITING
           LHI R15,12              SET RETURN CODE
           B   FINISH              GO RETURN TO CALLER
          ENDIF
          LH   R0,WAITMAX          CHECK REMAINING WAIT TIME
          IF   R0,(NH,CH),=H'0'    IF WAIT TIMED OUT
           LHI R15,12               SET RETURN CODE
           B   TIMEOUT              GO RETURN TO CALLER
          ENDIF
          BAS  R3,DOWAIT           WAIT 5 SECONDS AND TRY AGAIN
          B    CHECK               GO CHECK UNTIL PTN COMES ACTIVE
         ENDIF
*
         LR    R5,R1              GET ADDRESS OF PCB/PCE
         USING  PCBADR,R5         ADDRESSABILITY FOR PCB/PCE
         L     R6,PCECOMRA        GET ADDRESS OF COMREG
         USING  COMREG,R6         ADDRESSABILITY FOR COMREG
         L     R1,PCEPOWJB        GET ADDRESS OF POWJB INFO
         USING  PJBADR,R1         ADDRESSABILITY FOR POWJB INFO
*
         IF    IJBPHNAM,(NE,CLC),=CL8'DFHSIP'  IF NOT CICS IN PTN
          IF   WAITFLG,(NO,TF),$NOOP  IF NO WAITING
           LHI R15,8               SET RETURN CODE
           B   FINISH              GO RETURN TO CALLER
          ENDIF
          LH   R0,WAITMAX          CHECK REMAINING WAIT TIME
          IF   R0,(NH,CH),=H'0'    IF WAIT TIMED OUT
           LHI R15,8                SET RETURN CODE
           B   TIMEOUT              GO RETURN TO CALLER
          ENDIF
          BAS  R3,DOWAIT           WAIT 5 SECONDS AND TRY AGAIN
          B    CHECK               GO CHECK UNTIL PGM COMES ACTIVE
         ENDIF
*
         CF    WAITPID            TURN OFF PID MSG FLAG
         MVC   WINJOBN,PJBPNAME   CAPTURE CICS JOB NAME
*
         LR    R4,R6              COPY CICS COMREG TO R4
         STM   R1,R10,TMPSAVE     SAVE MY REGISTERS
         BAS   R14,IS_CICS_UP     CHECK IF THE CICS ACB IS OPEN
         LM    R1,R10,TMPSAVE     RESTORE MY REGISTERS
         IF    R15,(Z,LTR),R15    IF CICS ACB IS NOT YET OPEN
          IF   WAITFLG,(NO,TF),$NOOP  IF NO WAITING
           LHI R15,4               SET RETURN CODE
           B   FINISH              GO RETURN TO CALLER
          ENDIF
          LH   R0,WAITMAX          CHECK REMAINING WAIT TIME
          IF   R0,(NH,CH),=H'0'    IF WAIT TIMED OUT
           LHI R15,4                SET RETURN CODE
           B   TIMEOUT              GO RETURN TO CALLER
          ENDIF
          BAS  R3,DOWAIT           WAIT 5 SECONDS AND TRY AGAIN
          B    CHECK               GO CHECK UNTIL PGM COMES ACTIVE
         ENDIF
         DROP  R5,R6,R1
*
         IF    MAINFLG,(ON,TF),$NOOP   FOR MAIN PGM, ONLY
          MVI  MSG,C' '                 CLEAR MESSAGE FIELD
          MVC  MSG+1(L'MSG-1),MSG
          MVC  MSG+0(8),PGMNAME         SET PGM NAME IN MSG
          MVC  MSG+8(2),=CL2': '
          IF   WAITPID,(ON,TF),$NOOP    HAVE PID, ONLY?
           MVC MSG+10(2),WINPID          SET PID IN MSG
           MVC MSG+12(10),=CL10' COMPLETED'
           MVC MSG+22(15),=CL15' INITIALIZATION'
          ELSE                          ELSE
           MVC MSG+10(8),WINJOBN         SET JOBNAME IN MSG
           MVC MSG+18(10),=CL10' COMPLETED'
           MVC MSG+28(15),=CL15' INITIALIZATION'
          ENDIF
          PUT  CONSOLE                 WRITE MSG TO CONSOLE
         ENDIF
*
         XR    R15,R15            PASS BACK ZERO RETURN CODE
         B     FINISH             GO RETURN TO CALLER
* ------------------------------------------------------------------- *
TIMEOUT  DS    0H                 WAIT TIMED OUT
         IF    MAINFLG,(ON,TF),$NOOP   FOR MAIN PGM, ONLY
          LR   R3,R15                   SAVE RC
          MVI  MSG,C' '                 CLEAR MESSAGE FIELD
          MVC  MSG+1(L'MSG-1),MSG
          MVC  MSG+0(8),PGMNAME         SET PGM NAME IN MSG
          MVC  MSG+8(11),=CL11': WAIT FOR '
          IF   WAITPID,(ON,TF),$NOOP    HAVE PID, ONLY?
           MVC MSG+19(2),WINPID          SET PID IN MSG
           MVC MSG+21(10),=CL10' TIMED OUT'
          ELSE                          ELSE
           MVC MSG+19(8),WINJOBN         SET JOBNAME IN MSG
           MVC MSG+27(10),=CL10' TIMED OUT'
          ENDIF
          PUT  CONSOLE                  WRITE MSG TO CONSOLE
          LR   R15,R3                   RESTORE RC
         ENDIF
* ------------------------------------------------------------------- *
FINISH   DS    0H
         TRAILER RETCODE=R15,SVAREA=WINSAVE
* =================================================================== *
         EJECT
* =================================================================== *
DOWAIT   DS    0H                 DO A WAIT FOR 5 SECONDS
* ------------------------------------------------------------------- *
         IF    WAITMSG,(NO,TF),$NOOP   FOR FIRST TIME, ONLY
          SF   WAITMSG
          IF   MAINFLG,(ON,TF),$NOOP   FOR MAIN PGM, ONLY
           MVI MSG,C' '                 CLEAR MESSAGE FIELD
           MVC MSG+1(L'MSG-1),MSG
           MVC MSG+0(8),PGMNAME         SET PGM NAME IN MSG
           MVC MSG+8(14),=CL14': WAITING FOR '
           IF  WAITPID,(ON,TF),$NOOP    HAVE PID, ONLY?
            MVC MSG+22(2),WINPID         SET PID IN MSG
            MVC MSG+24(12),=CL12' TO COMPLETE'
            MVC MSG+36(15),=CL15' INITIALIZATION'
           ELSE                         ELSE
            MVC MSG+22(8),WINJOBN        SET JOBNAME IN MSG
            MVC MSG+30(12),=CL12' TO COMPLETE'
            MVC MSG+42(15),=CL15' INITIALIZATION'
           ENDIF
           PUT CONSOLE                 WRITE MSG TO CONSOLE
          ENDIF
         ENDIF
*
         LHI   R2,5               SET FOR 5-SECOND TIMER
         SETIME (2),MYTECB        ISSUE COMMAND AND
         WAIT  MYTECB              WAIT FOR TIMER INTERRUPT
         LH    R0,WAITMAX         GET REMAINING WAIT TIME
         IF    R0,(HT,CH),=H'0'   IF WAIT NOT TIMED OUT
          SHI  R0,5                SUBTRACT WAIT TIME
          STH  R0,WAITMAX          PUT REMAINING WAIT TIME
         ENDIF
* ------------------------------------------------------------------- *
         BR    R3                 RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
* IS_CICS_UP:
* AUTHOR: TONY THIGPEN TONY@THIGPENS.COM
*         WITH MODIFICATIONS BY DAVE L CLARK.
* BEFORE CALLING THIS ROUTINE, R4 NEEDS TO HAVE THE ADDRESS COMREG OF
* THE CICS PARTITION OF INTEREST.
* THIS SUBROUTINE USES CROSS-MEMORY ACCESS TO LOOK AT THE CICS
* CONTROL BLOCKS TO SEE IF THE MESSAGE 'CONTROL IS RETURNED TO CICS'
* HAS BEEN ISSUED TO VERIFY THAT THE CICS IS ACTIVE AND USABLE.
* INPUT:
* R4  ADDRESS OF CICS COMREG
* R14 RETURN ADDRESS
* UPON RETURN:
* R15 = 0 JOB NOT FOUND OR THE CICS IS NOT YET ACTIVE AND READY
* R15 = 1 JOB FOUND AND THE CICS IS ACTIVE AND READY
*
IS_CICS_UP DS  0H
         AMODESW SET,AMODE=31
* ------------------------------------------------------------------- *
         ST    R14,ICU_R14             SAVE RETURN REG
         LAM   R0,R15,=16F'0'          CLEAR ACCESS REGISTERS
         MODESET KEY=ZERO              UNPROTECT EXTERNAL STORAGE
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
         L     R1,X'260'               GET ADDRESS TCB
         POP   ACONTROL
         MVC   TCBVEND,2(R1)           SAVE TCBVEND
         OI    2(R1),X'80'             ALLOW ACCESS REGISTERS
         MODESET KEY=USER              RESET TO PROTECT LOW CORE
         USING COMREG,R4               CICS COMREG
         LH    R0,PID                  R0 <- PIK
         GETFLD FIELD=ALET,PART=(0),RETURN=YES
         IF    R15,(NZ,LTR),R15        IF NO ACCESS TO CICS PTN
          MVC  ICU_TRACE(4),=A(-1)      SIGNAL COMPLETE FAILURE
         ELSE                          ELSE
          SAR  R2,R1                    **AR2 SET**
          MVI  ICU_TRACE,X'00'          SIGNAL INITIAL SUCCESS
          SAC  512                      SET ACCESS REGISTER MODE
          ST   R4,ICU_TRACE+4           SAVE CICS COMREG ADDR
          L    R2,IJBAFCB
          ST   R2,ICU_TRACE+8           SAVE CICS AFCB ADDR
          L    R2,AFCSA-DFHAFCB(,R2)
          ST   R2,ICU_TRACE+12          SAVE CICS CSA ADDR
          MVC  ICU_TRACE(3),CSAXST-DFHCSADS(R2)  SAVE 3 STATUS BYTES
         ENDIF                         ENDIF
         SAC   0                       SET PRIMARY SPACE MODE
         DROP  R4
         LAM   R0,R15,=16F'0'          CLEAR ACCESS REGISTERS
         MODESET KEY=ZERO              UNPROTECT EXTERNAL STORAGE
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
         L     R1,X'260'               GET ADDRESS TCB
         POP   ACONTROL
         MVC   2(1,R1),TCBVEND         RESTORE TCBVEND
         MODESET KEY=USER              RESET TO PROTECT LOW CORE
         LHI   R15,0                   SIGNAL CICS IS DOWN (DEFAULT)
         IF    ICU_TRACE(4),(NE,CLC),=A(-1)
          IF   ICU_TRACE+2,(EQ,CLI),CSAXSINC
           LHI R15,1                   SIGNAL CICS IS UP  (OVERRIDE)
          ENDIF
         ENDIF
         L     R14,ICU_R14             LOAD RETURN ADDRESS
* ------------------------------------------------------------------- *
         AMODESW SET,AMODE=24
         BR    R14                     RETURN TO CALLER
* =================================================================== *
         EJECT
DATA     LOCTR
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
WINSAVE  DS    9D                 SAVE AREA
TMPSAVE  DS    5D                 TEMP SAVE AREA
*
ICU_R14  DS    A(0)
ICU_TRACE DC   XL(40)'00'
TCBVEND  DS    X'00'
*
BLANKS   DC    CL8' '
RUNOPTS  DS    X                  RUN-TIME OPTION FLAGS
MAINFLG  DF    B'10000000'        MAIN PGM FLAG
WAITFLG  DF    B'01000000'        ACB WAIT FLAG
WAITMSG  DF    B'00100000'        WAIT MSG FLAG
WAITPID  DF    B'00010000'        WAIT PID MSG FLAG
WAITMAX  DS    H                  IN SECONDS
MYTECB   TECB                     TIMER EVENT CONTROL BLOCK
MSG      DS    CL60                    CONSOLE MESSAGE AREA
         LTORG
CONSOLE  DTFCN DEVADDR=SYSLOG,BLKSIZE=60,IOAREA1=MSG,MODNAME=WINWTO
         CORG  0,256
* =================================================================== *
         END
