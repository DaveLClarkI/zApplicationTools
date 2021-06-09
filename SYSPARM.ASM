SYS      TITLE 'SUBROUTINE TO RETURN SYSPARM TO CALLER'
* CHANGE HISTORY  ------------------------------------------------
* 06/13/2019 DLC0 ORIGINAL PROGRAM.
* END OF HISTORY  ------------------------------------------------
         EJECT
* =================================================================== *
PARMS    DSECT                         SYSPARM PARMS DATA SECTION
         USING *,R10                   DATA SECTION BASE REGISTER
* =================================================================== *
PARMAREA DS    XL8
* =================================================================== *
         EJECT
* =================================================================== *
COMREG   MAPCOMR EQU=YES               PARTITION COMMUNICATION REGION
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* =================================================================== *
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10                      SYSPARM DSECT BASE REG
R11      EQU   11                      SYSPARM CSECT BASE REG
R12      EQU   12
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         PRINT ON
* =================================================================== * 00012800
         EJECT                                                          00022700
* =================================================================== * 00022800
SYSPARM  HEADER VERSION=' 1.0',AUTHOR=' DLC',DATE='(C)JUN19',          X
               BASE=R11,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         L     R10,0(,R1)              SET ADDRESSABILITY TO PARM AREA

         COMRG REG=(R1)                GET ADDR OF PARTITION COMREG
         USING COMREG,R1               SET ADDRESSABILITY TO COMREG
         L     R2,SYSPAR               GET ADDRESS OF SYSPARM BLOCK
         DROP  R1                      DISESTABLISH REG 1 AS BASE

         MVC   PARMAREA,1(R2)          PASS BACK SYSPARM VALUE
         TR    PARMAREA,TRTABLE        XLATE LOW-VALUES TO SPACES
* ------------------------------------------------------------------- *
RETURN   EQU   *                       RETURN TO CALLER
         TRAILER SVAREA=SAVEAREA
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D                      IN-PROGRAM STORAGE AREA
* ------------------------------------------------------------------- *
SAVEAREA DS    9D
TRTABLE  DC    256AL1(*-TRTABLE)       INITIALIZE FOR NO TRANSLATION
         ORG   TRTABLE+X'00'             TRANSLATE LOW-VALUE            00029900
         DC    C' '                        TO A SPACE                   00029900
         ORG
         LTORG
* =================================================================== *
         END
