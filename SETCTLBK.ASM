SET      TITLE 'RETURN CONTROL BLOCK ADDRESSES TO CALLER'
SETCTLBK AMODE 31
SETCTLBK RMODE ANY
* CHANGE HISTORY  ------------------------------------------------
* 06/01/2021 DLC0 ORIGINAL PROGRAM.
* END OF HISTORY  ------------------------------------------------
         EJECT
* =================================================================== *
PARMS    DSECT                         SYSPARM PARMS DATA SECTION
* =================================================================== *
PARMCOMA DS    A
PARMPCEA DS    A
PARMPJBA DS    A
PARMSYSA DS    A
PARMTODA DS    A
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
SETCTLBK HEADER VERSION=' 1.0',AUTHOR=' DLC',DATE='(C)JUN21',          X
               BASE=R11,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         LR    R10,R1                  GET ADDR OF PASSED PARM LIST
         USING PARMS,R10               SET ADDRESSABILITY TO PARM LIST
*
         COMRG REG=(R1)                GET ADDR OF PARTITION COMREG
         USING COMREG,R1               SET ADDRESSABILITY TO COMREG
*
         TM    PARMCOMA,X'80'          IF 1ST PARM IS LAST PARM
         BO    SETCOMA                   GO SET JUST FIRST PARM
         TM    PARMPCEA,X'80'          IF 2ND PARM IS LAST PARM
         BO    SETPCEA                   GO SET FIRST TWO PARMS
         TM    PARMPJBA,X'80'          IF 3RD PARM IS LAST PARM
         BO    SETPJBA                   GO SET FIRST THREE PARMS
         TM    PARMSYSA,X'80'          IF 4TH PARM IS LAST PARM
         BO    SETSYSA                   GO SET FIRST FOUR PARMS
*                                      ELSE SET ALL FIVE PARMS
SETTODA  L     R2,TODCOM               GET ADDRESS OF TODCOM BLOCK
         L     R3,PARMTODA             GET ADDRESS OF TODCOM PARM
         ST    R2,0(,R3)               PUT TO 5TH PARM LOCATION
SETSYSA  L     R2,SYSPAR               GET ADDRESS OF SYSPARM BLOCK
         L     R3,PARMSYSA             GET ADDRESS OF SYSPARM PARM
         ST    R2,0(,R3)               PUT TO 4TH PARM LOCATION
SETPJBA  L     R2,IJBPCEPT             GET ADDRESS OF PCE BLOCK
         L     R2,X'24'(,R2)           GET ADDRESS OF PJB BLOCK
         L     R3,PARMPJBA             GET ADDRESS OF PJB PARM
         ST    R2,0(,R3)               PUT TO 3RD PARM LOCATION
SETPCEA  L     R2,IJBPCEPT             GET ADDRESS OF PCE BLOCK
         L     R3,PARMPCEA             PUT ADDRESS OF PCE PARM
         ST    R2,0(,R3)               PUT TO 2ND PARM LOCATION
SETCOMA  L     R3,PARMCOMA             GET ADDRESS OF COMREG PARM
         ST    R1,0(,R3)               PUT TO 1ST PARM LOCATION
* ------------------------------------------------------------------- *
RETURN   EQU   *                       RETURN TO CALLER
         TRAILER SVAREA=SAVEAREA
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D                      IN-PROGRAM STORAGE AREA
* ------------------------------------------------------------------- *
SAVEAREA DS    9D
         LTORG
* =================================================================== *
         END
