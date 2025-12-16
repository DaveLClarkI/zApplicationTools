CARD     TITLE 'TEST OF READING PAST /* EOD-OF-DATA CARD'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*                      C A R D   I N P U T   W /                      *
*            A L T E R N A T E   E O D   D E L I M I T E R            *
*                                                                     *
*                                                                     *
* No warranty is expressed or implied.       Written by Dave L Clark  *
* Neither the author nor the company is      Phone: (937)751-3300     *
* responsible for any loss or damage         Winsupply Group Services *
* resulting from the use of this code.       3110 Kettering Blvd.     *
* If provided, source on as-is basis.        Dayton, OH  45439        *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This is a sample program  to demonstrate the ability to bypass *
* the standard end-of-data delimiter  (/*)  and use an alternate end- *
* of-data delimiter specified by  the  user of this program--via EXEC *
* PARM input (see below).   Note that  this is basically what the VSE *
* LIBR  utility  does  for  the  purpose  of cataloging,  e.g.,  REXX *
* procedure source  code,  which  uses  the  /*  indicator for source *
* comment lines and these comment  lines can begin in column 1--where *
* they would otherwise mimic  the  standard end-of-data delimiter and *
* cause premature EOF problems.                                       *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      This is a standard batch  program.   Compile and execute it as *
* you would any other  program.   This  sample simply copies the data *
* input to print line output for verification purposes.               *
*                                                                     *
*   // JOB    CARDASM   TEST OF READING PAST /* EOD-OF-DATA CARD      *
*   // EXEC   PGM=CARDASM,SIZE=*,PARM='DLM=/+'                        *
*   * $$ SLI MEM=MYREXX.PROC,S=DAP.PROD                               *
*   /+ EOD                                                            *
*   /& EOJ                                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT                                                          00005300
* =================================================================== *
EXECPARM DSECT                         EXEC PARAMETER AREA
         USING *,R1
* ------------------------------------------------------------------- *
DLMLEN   DS    H                       PARAMETER LENGTH
DLMKWD   DS    CL4                     PARAMETER KEYWORD
DLMVAL   DS    CL2                     PARAMETER VALUE
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* ------------------------------------------------------------------- *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  DATAAREA                DATA STORAGE ALIGNMENT
         COPY  DATAEND                 DATA END     ALIGNMENT
         COPY  IF                      IF/AND/ELSE/ENDIF CONSTRUCT
         COPY  DOLOOP                  DOLOOP/LEAVE/ITER/UNTIL
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES                                              *
* ------------------------------------------------------------------- *
R0       EQU   0                  PARM REG
R1       EQU   1                  PARM REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                  RPL DSECT REG
R9       EQU   9                  ACB DSECT REG
R10      EQU   10
R11      EQU   11                 PGM BASE REG
R12      EQU   12                 LE RESERVED REG
R13      EQU   13                 REGISTER SAVEAREA
R14      EQU   14                 RETURN REG
R15      EQU   15                 ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
CARDASM  HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)DEC25',            X
               BASE=10,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         IF    R1,(NE,CR),R10      IF THERE IS AN EXEC PARM
          L    R1,0(,R1)            GET ADDRESS OF EXEC PARM AREA
          IF   DLMLEN,(NL,CLC),=H'6',AND,=C'DLM=',(EQ,CLC),DLMKWD
           MVC DLM,DLMVAL            GET CUSTOM DELIMITER
          ENDIF                     END IF
         ENDIF                     END IF
         DROP  R1                  EXEC PARM NO LONGER NEEDED
*
         OPEN  CARDIN,LINEOUT      OPEN SEQUENTIAL DTFS
GETCARD  DOLOOP                    READ CARDS UNTIL CUSTOM EOD
          GET  CARDIN                      GET A CARD
          IF   =C'/&&',(NE,CLC),CARD_IA    IF  NOT EOJ
          AND  DLM,(NE,CLC),CARD_IA        AND NOT CUSTOM EOD
DATAIN     DS  0H                           ACCEPT INPUT (AND /* EOF)
*          LHI R2,128                       IF READ LENGTH NEEDED
*          SH  R2,CARDIN                     SUBT. CCB RESIDUAL COUNT
           TR  CARD_IA,TRTABLE              XLATE NULLS TO SPACES
           MVC LINEDATA,CARD_IA             SEND INPUT CARD
           PUT LINEOUT                        TO OUTPUT DEVICE
          ENDIF                            END IF
         UNTIL =C'/&&',(EQ,CLC),CARD_IA,  LOOP UNTIL EOJ               X
               OR,DLM,(EQ,CLC),CARD_IA            OR CUSTOM EOD
* ------------------------------------------------------------------- *
ENDIN    DS    0H                  END OF CARD INPUT
         CLOSE LINEOUT,CARDIN      CLOSE SEQUENTIAL DTFS
         XR    R15,R15             ZERO RETURN CODE
EXIT     DS    0H                  PROGRAM EXIT
         TRAILER RETCODE=R15,SVAREA=SAVEAREA
* =================================================================== *
         EJECT                                                          00059000
* =================================================================== *
STORAGE  DATAAREA                                                       00019900
* ------------------------------------------------------------------- *
SAVEAREA DS    9D
*
TRTABLE  DC    256AL1(*-TRTABLE)       INITIALIZE FOR NO TRANSLATION
         ORG   TRTABLE+X'00'             TRANSLATE LOW-VALUE            00029900
         DC    C' '                        TO A SPACE                   00029900
         ORG
*
DLM      DC    C'/+'
CARD_IA  DS    CL128
LINE_OA  DS    0CL121
LINE_CC  DC    C' '
LINEDATA DC    CL120' '
         LTORG
* ------------------------------------------------------------------- *
         EJECT                                                          00059000
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
* =================================================================== *
CARDIN   DTFCD DEVADDR=SYSRDR,DEVICE=3505,IOAREA1=CARD_IA,BLKSIZE=128, X
               ERROPT=IGNORE,EOFADDR=DATAIN
* =================================================================== *
         EJECT                                                          00059000
* =================================================================== *
LINEOUT  DTFDI DEVADDR=SYSLST,IOAREA1=LINE_OA,RECSIZE=121
* =================================================================== *
         POP   ACONTROL
         DATAEND
* =================================================================== *
         END   CARDASM
