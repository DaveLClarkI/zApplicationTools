SYM      TITLE 'RXVSESYM - REXX SUB TO GET/SET VSE SYMBOLIC PARMS'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Dapsco Information Systems                          Written by:     *
* 3110 Kettering Boulevard                         Dave Leonard Clark *
* Dayton, OH  45439                                  (937)294-5331    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This external REXX function allows  a REXX program to directly *
* retrieve or set the value of one or more VSE symbolic parameters by *
* name.                                                               *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This program is to be compiled as a standard batch phase.   It *
* will not,  however, then be available for just any program to call. *
* Only a REXX procedure will be able to call it.                      *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX subroutine is as easy as using the *
* REXX "CALL"  statement.   The  following  is  an  example of either *
* retrieving or  setting  the  value  of  one  or  more  VSE symbolic *
* parameters:                                                         *
*                                                                     *
*     Call RXVSESYM 'Get','SYM1','SYM2',...                           *
*     If rc ¬= 0 Then Do                                              *
*       Say " RXVSESYM Get error: rc = "rc", result = "result;        *
*       Exit 4;                                                       *
*     End                                                             *
*                                                                     *
*     Call RXVSESYM 'Set','SYM1','SYM2',...                           *
*     If rc ¬= 0 Then Do                                              *
*       Say " RXVSESYM Set error: rc = "rc", result = "result;        *
*       Exit 4;                                                       *
*     End                                                             *
*                                                                     *
* Note that the second through  nth  argument values are to represent *
* both REXX and VSE variable names.  As such, their format is limited *
* to the more restrictive  rules  (capabilities)  of the VSE symbolic *
* parameter naming conventions.   For example, the length of the name *
* cannot be greater than  seven  characters  and  the value cannot be *
* more than 40 characters in length.                                  *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
*      The second through nth  argument  values must either represent *
* existing REXX variable names  (for  'Set' input processing) or will *
* represent REXX variable  names  (for 'Get' output processing).   In *
* other words,  'Set'  will  create  (or  update)  the equivalent VSE *
* symbolic parameter  with  the  value  of  an existing REXX-supplied *
* variable;  while 'Get' will retrieve  the  value of an existing VSE *
* symbolic parameter and use it  to create (or update) the equivalent *
* REXX variable.                                                      *
*                                                                     *
*      In addition,  the RXVSESYM subroutine  also sets the REXX "RC" *
* special variable with a value  indicating the success or failure of *
* the requested operation.   You  should  analyze the "RC" variable's *
* value for errors.  Those values are as follows:                     *
*                                                                     *
*    0   no errors occurred                                           *
*    4   argument error -- missing, too many, or name/value too long  *
*    8   internal REXX, GETSYMB, or $IJBPROC failure -- also, the R15 *
*          value is returned in the REXX "RESULT" special variable    *
*   12   REXX environment not present                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* ------------------------------------------------------------------- *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PWORK8   DS    PL8                     PACKED DOUBLE-WORD
EWORK8   DS    CL8                     EDITED DOUBLE-WORD
SYMREGS  DS    4F                      SAVE REGS DURING GETSYMB
*
SYMBOLL  DS    H                       LENGTH OF SYMBOL
SYMLENG  DS    H                       LENGTH OF SYMBOL'S VALUE
OPT      DS    CL1                     PROCESSING OPTION
OPTGET   EQU   C'G'
OPTSET   EQU   C'S'
OPTFLAG  DS    XL1                     OPTION FLAGS
SET_KLZ  EQU   B'00001000'             KEEP LEADING ZEROES
SYMPARM  DS    CL7                     PARMNAME FOR GETSYMB
SYMVALU  DS    CL50                    VALBUF FOR GETSYMB
         DS    0F                      (ALIGNMENT)
SYMWRKA  DS    XL100                   WORK AREA FOR GETSYMB
*
SAVEENVB DS    F                       ENVIRONMENT BLOCK ADDRESS
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPLAST EQU   *-4                       POINT TO LAST PARM FOR FLAG
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
SHVPARM5 DS    XL4
*
PARMAREA DS    6F
SYMBUFR  DS    CL6000
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
         ARXENVB                       REXX ENVIRONMENT BLOCK
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXEFPL                       EXTERNAL FUNCTION PARAMETER LIST
         ARXEVALB                      REXX EVALUATION BLOCK
         ARXARGTB                      REXX PARSED ARGUMENT TABLE
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK
R1       EQU   1                       ON ENTRY, REXX EFPL REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9                       REXX EXTERNAL ENTRY-POINT VECTOR
R10      EQU   10                      REXX EVALUATION BLOCK
R11      EQU   11                      SHARED VARIABLES CONTROL BLOCK
R12      EQU   12                      RX3270IO CSECT BASE REG
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* ------------------------------------------------------------------- *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
RXVSESYM HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)MAY04',            X
               BASE=R12,SVAREA=SALEN
* ------------------------------------------------------------------- *
         USING DYNASTOR,R13
*
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RXVSESYM: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LA   R15,12                  SET ABNORMAL RETURN CODE
          J    RETR15                  GO RETURN ERROR
         ENDIF
         L     R9,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R9
         ST    R5,SAVEENVB             SAVE ADDR. OF ENV. BLOCK
         DROP  R5                      REMOVE ADDR. TO ENV. BLOCK
*
         LR    R8,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R8
         L     R10,EFPLEVAL            REXX EVALUATION BLOCK POINTER
         L     R10,0(,R10)             REXX EVALUATION BLOCK
         USING EVALBLOCK,R10
         MVI   EVALBLOCK_EVDATA,X'00'  INIT FIRST BYTE OF RESULTS BUF
*
         LA    R11,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R11              SHARED VARIABLES CONTROL BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R0,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R1,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R3,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R0,R3,SHVPARMB          PUT IN PARM BLOCK
         OI    SHVPLAST,X'80'          SET END OF PARM BLOCK
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         LA    R0,SYMPARM              GET ADDRESS FOR SHVNAMA
         LA    R1,L'SYMPARM            GET LENGTH  FOR SHVNAML
         LA    R2,SYMVALU              GET ADDRESS FOR SHVVALA
         LA    R3,L'SYMVALU            GET LENGTH  FOR SHVVALL
         STM   R0,R3,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R1,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R3,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
* POINT TO ARGUMENT(S)
*
         L     R6,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R6
         DROP  R8
         LR    R7,R6                   REXX ARGUMENT STRING
         USING ARGSTRING,R7
*
* PREPARE SYMBOL BUFFER FOR "SET"
*
         LA    R8,SYMBUFR
         MVC   0(2,R8),=X'FFFF'
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    OPT,OPT
*
         WHILE ARGTABLE_END,(NE,CLC),=XL8'FFFFFFFFFFFFFFFF'

         L     R2,ARGTABLE_ARGSTRING_LENGTH
         L     R4,ARGTABLE_ARGSTRING_PTR
         IF    OPT,EQ,X'00'            IF NO FIRST ARG YET
          IF   R2,(NZ,LTR),R2           IF ARG SUPPLIED,
           MVC OPT,0(R4)                 GET CALLER ARG
           TR  OPT,TRTBL                  AND TRANSLATE TO UPPER-CASE
          ENDIF                         ENDIF
          IF   OPT,NE,OPTGET            IF NOT GET
          AND  OPT,NE,OPTSET            AND NOT SET
           LA  R5,4                      SET RESULT CODE
           J   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ELSE                          ELSE
          IF   R2,(H,C),=F'7',OR,R2,(L,C),=F'1'  NAME TOO SHORT/LONG?
           LA  R5,4                      SET RESULT CODE
           J   RETERR                    GO RETURN ERROR
MVPARM     MVC SYMPARM(0),0(R4)   (SEE "EXECUTE" INSTRUCTION BELOW)
          ENDIF                         ENDIF
          MVI  SYMPARM,C' '             BLANK PAD PARM AND VALUE AREAS
          MVC  SYMPARM+1(L'SYMPARM-1+L'SYMVALU),SYMPARM
          STH  R2,SYMBOLL               SAVE VAR NAME LENGTH
          BCTR R2,0                     DECR. FOR ZERO OFFSET
          EXRL R2,MVPARM                MOVE SYMBOL NAME
          TR   SYMPARM,TRTBL            TRANSLATE TO UPPER-CASE
*
          IF   OPT,EQ,OPTSET            IF SET
           LH  R3,SYMBOLL                GET LENGTH OF VAR NAME
           JAS R2,GETVAR                 RETRIEVE THE VARIABLE VALUE
           IF  R5,(NZ,LTR),R5,OR,        IF VARIABLE NOT FOUND         X
               SHVVALL,(HT,CLC),=F'50'   OR VALUE LENGTH > 50
            LA R5,4                       SET RESULT CODE
            J  RETERR                     GO RETURN ERROR
MVDATA      MVC 10(0,R8),SYMVALU  (SEE "EXECUTE" INSTRUCTION BELOW)
           ENDIF                         ENDIF
           L   R2,SHVVALL                GET LENGTH OF DATA
           STCM R2,B'0011',0(R8)         PUT LENGTH OF DATA IN BUFFER
           XC  2(1,R8),2(R8)             SET INDICATOR BYTE IN BUFFER
           MVC 3(L'SYMPARM,R8),SYMPARM   SET PARM NAME IN BUFFER
           BCTR R2,0                     DECREMENT FOR EXECUTE
           EXRL R2,MVDATA                SET PARM DATA IN BUFFER
           AH  R8,=H'11'                 ADD LENGTH OF HEADER + 1
           AR  R8,R2                     ADD LENGTH OF PARM DATA - 1
           MVC 0(2,R8),=X'FFFF'          FLAG THE END OF THE BUFFER
*
          ELSE                          ELSE GET
           LR  R8,R13                    SAVE REG 13 FOR QUICK RESTORE
           STM R14,R1,SYMREGS            SAVE REGS 14, 15, 0, AND 1
* THE FOLLOWING DESTROYS REGS 13, 14, 15, 0, AND 1
           GETSYMB AREA=SYMWRKA,         RETRIEVE, FOR THIS            *
               PARMNAM=SYMPARM,           SYMBOL NAME, ITS             *
               VALBUF=SYMVALU,             REPLACMENT VALUE AND        *
               LENFLD=SYMLENG               ASSOCIATED LENGTH
           LR  R13,R8                    RESTORE REG 13
           ST  R15,SYMREGS+4             SAVE REG 15
           LM  R14,R1,SYMREGS            RESTORE REGS 14, 15, 0, AND 1
           IF  R15,(NZERO,LTR),R15       IF ERROR
            LA R5,4                       SET RESULT CODE
            J  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           LH  R2,SYMLENG                SET LENGTH OF DATA VALUE
           ST  R2,SHVVALL                 AND PUT IN CONTROL BLOCK
           LH  R3,SYMBOLL                COMPUTE LENGTH OF VAR NAME
           JAS R5,SETVAR                 CREATE THE VARIABLE
          ENDIF                         ENDIF
*
         ENDIF                         ENDIF

         LA    R6,L'ARGTABLE_END(,R6)  ADVANCE TO NEXT ARGUMENT
         LR    R7,R6
         WEND
*
* READY TO PROCEED?
*
         IF    OPT,EQ,OPTSET           IF SET
          LOAD $IJBPROC,SYS=YES         LOCATE SYSTEM ROUTINE
          LR   R15,R1                   GET ENTRY POINT ADDRESS
          XC   PARMAREA(6*4),PARMAREA   CLEAR PARM AREA
          LA   R1,PARMAREA              GET ADDRESS OF PARM AREA
          MVI  0(R1),8                  SELECT "SETPAR" FUNCTION
          LA   R0,SYMBUFR               GET ADDRESS OF DATA AREA
          ST   R0,16(R1)                PUT IN PARM AREA
          XR   R0,R0                    CLEAR PARM REG
          BASR R14,R15                  INVOKE SYSTEM ROUTINE
          IF   R15,(NZERO,LTR),R15      IF ERROR
           LA  R5,8                      SET RESULT CODE
           J   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   NI    OPTFLAG,X'FF'-SET_KLZ   REG2ZON3 NO LEADING ZEROES
         LR    R2,R15                  COPY R15 TO R2
         LA    R3,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         JAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         ST    R2,EVALBLOCK_EVLEN      LENGTH OF CONVERTED RESULT
*
         LR    R2,R5                   COPY R5 TO R2
         LA    R3,SYMVALU              POINT TO VARIABLE DATA AREA
         JAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         LA    R1,SYMVALU              POINT BACK TO VAR DATA AREA
         STM   R1,R2,SHVVALA           PUT IN SHV CONTROL BLOCK
*
         LA    R3,2                    SET LENGTH OF VARIABLE NAME
         MVC   SYMPARM+0(3),=C'RC '    SET VARIABLE NAME
         JAS   R5,SETVAR               CREATE THE VARIABLE
* ------------------------------------------------------------------- *
         XR    R15,R15                 CLEAR REG15 FOR RETURN CODE
RETR15   TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZON3 DS    0H    CONVERT REG2 TO LEFT-SIGN, ZONED-DECIMAL AT REG3
* ------------------------------------------------------------------- *
         CVD   R2,PWORK8               CONVERT REG2 TO PACKED-DECIMAL
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          MVC  EWORK8(7),=X'40212020202060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
         ELSE
          MVC  EWORK8(7),=X'40202020212060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+5             POINT TO SIGNIFICANCE BYTE
         ENDIF
         LA    R2,EWORK8+6             POINT TO SIGN BYTE (& IGNORE IT)
         IF    EWORK8(7),(M,EDMK),PWORK8+5   IF NEGATIVE NUMBER
          BCTR R1,0                    DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'                FLOAT A LEFT MINUS SIGN
         ENDIF
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
          LA   R2,4
         ELSE
          SR   R2,R1                   CALC NO. OF CHARS TO MOVE
         ENDIF
         BCTR  R2,0                    DECR. LEN FOR ZERO REF.
         EXRL  R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
*
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
ZON2REG3 DS    0H    CONVERT ZONED-DECIMAL AT REG2 (L'REG3) INTO REG3
* ------------------------------------------------------------------- *
         BCTR  R3,0                    DECR. FOR ZERO OFFSET
         EXRL  R3,PKINPT               PACK 0(?,R2) FOR LENGTH OF R3
         CVB   R3,PWORK8               CONVERT PACKED-DECIMAL INTO REG3
*
         BR    R15
PKINPT   PACK  PWORK8,0(0,R2)          (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H    GET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         XR    R5,R5                   ZERO RETURN CODE REGISTER
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          LA   R5,8                     SET RESULT CODE
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           IC  R15,SHVRET                SAVE ARXEXCOM CC
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R2
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H    SET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   SYMPARM(3),(NE,CLC),=C'RC '  IF NOT 'RC', REPORT ERRORS
           IF  R15,(NZ,LTR),R15          IF NON-ZERO RETURN CODE
            LA R5,8                       SET RESULT CODE
            J  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           IF  SHVRET,NE,SHVNEWV         IF NOT NEW VARIABLE
            IC R15,SHVRET                 SAVE ARXEXCOM CC
            LA R5,8                       SET RESULT CODE
            J  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
DATA     LOCTR
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
TRTBL    DC    256AL1(*-TRTBL)         Init for no translation, then
         ORG   TRTBL+C'a'              Translate lower-case
         DC    C'ABCDEFGHI'               to upper-case
         ORG   TRTBL+C'j'              Translate lower-case
         DC    C'JKLMNOPQR'               to upper-case
         ORG   TRTBL+C's'              Translate lower-case
         DC    C'STUVWXYZ'                to upper-case
         ORG   TRTBL+X'FA'             Translate unpacked data
         DC    C'ABCDEF'                  to valid hex
         ORG
*
SALEN    DC    A(DYNALENG)
         LTORG
*
         CORG  0,256
* =================================================================== *
         END
