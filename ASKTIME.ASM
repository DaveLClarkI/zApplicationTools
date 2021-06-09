ASK      TITLE 'Get CICS ASKTIME ABSTIME Equivalent'
         PRINT ON,GEN                                                   00000200
ASKTIME  AMODE 31
ASKTIME  RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*           G E T   C I C S   A S K T I M E   A B S T I M E           * 00000500
*                 F O R   C O B O L   P R O G R A M S                 * 00000600
*                                                                     * 00000700
*                                                                     * 00000800
* Winsupply Group Services                           Written by:      *
* 3110 Kettering Boulevard                        Dave Leonard Clark  *
* Dayton, OH  45439                                 (937)294-5331     *
*                                                                     * 00001200
* =================================================================== * 00001300
*                                                                     * 00001400
*                               PURPOSE                               * 00001500
*                                                                     * 00001600
*      This routine  provides  the  batch  COBOL  programmer  with an * 00001700
* efficient  means  of  extracting   an  absolute  timestamp  roughly * 00001800
* equivalent to the CICS ASKTIME ABSTIME parameter.                   * 00001900
*                                                                     * 00002000
* =================================================================== * 00002100
*                                                                     * 00002200
*                           IMPLEMENTATION                            * 00002300
*                                                                     * 00002400
* The invoking COBOL program should  use  the following data area for * 00005000
* passing parameters:                                                 * 00005100
*                                                                     * 00005200
*      01  ASKTIME-PARMS.                                             * 00005300
*        03  ASK-ABSTIME               PIC S9(15)   PACKED-DECIMAL.   * 00006800
*                                                                     * 00011300
* =================================================================== * 00011400
*                                                                     * 00011500
*                             INVOKATION                              * 00011600
*                                                                     * 00011700
* The subroutine can be invoked as follows:                           * 00011800
*                                                                     * 00013000
*     * VALIDATE AND CONVERT AN INPUT JULIAN DATE                     * 00013100
*          CALL 'ASKTIME' USING ASKTIME-PARMS.                        * 00013500
*          MOVE ASK-ABSTIME  TO my-file-field.                        * 00013600
*                                                                     * 00014100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00018200
         EJECT                                                          00018300
* =================================================================== * 00018400
ASKPARM  DSECT                    ASKTIME PARMS DUMMY SECTION           00018500
         USING *,R10              ESTABLISH REG 10 AS BASE              00018600
* =================================================================== * 00018700
ABSTIME  DS    PL8                ABSOLUTE TIME                         00018800
* =================================================================== * 00020500
         EJECT
* =================================================================== *
COMREG   MAPCOMR EQU=YES               PARTITION COMMUNICATION REGION
* =================================================================== *
         EJECT                                                          00020600
* =================================================================== * 00020700
*        REGISTER USAGES                                                00020800
* =================================================================== * 00020900
R0       EQU   0                  PARM REG                              00021000
R1       EQU   1                  PARM REG                              00021100
R2       EQU   2                                                        00021200
R3       EQU   3                                                        00021300
R4       EQU   4                                                        00021400
R5       EQU   5                                                        00021500
R6       EQU   6                                                        00021600
R7       EQU   7                                                        00021700
R8       EQU   8                                                        00021800
R9       EQU   9                                                        00021900
R10      EQU   10                 ASKPARM DSECT BASE REG                00022000
R11      EQU   11                 ASKTIME CSECT BASE REG                00022100
R12      EQU   12                                                       00022200
R13      EQU   13                 REGISTER SAVEAREA                     00022300
R14      EQU   14                 RETURN REG                            00022400
R15      EQU   15                 ENTRY REG                             00022500
* =================================================================== * 00022600
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
ASKTIME  HEADER VERSION=' 1.0',AUTHOR=' DLC',DATE='(C)JUN19',          X
               BASE=R11,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         L     R10,0(,R1)              SET UP REG 10 FOR ARGUMENTS
*                                                                       00024500
         STCK  TSTAMP                  GET CURRENT TIMESTAMP
         LG    R3,TSTAMP               LOAD 64-BIT VALUE
         SRLG  R3,R3,12                DROP RIGHTMOST 12 BITS
         DSGF  R2,=F'1000'             CHANGE TO MILLISECONDS
         CVDG  R3,TSTAMP16             CONVERT TO PACKED-DECIMAL
         ZAP   ABSTIME,TSTAMP16        PASS BACK TO CALLER
*
         COMRG REG=(R1)                GET ADDR OF PARTITION COMREG
         USING COMREG,R1               SET ADDRESSABILITY TO COMREG
         XR    R2,R2                   CLEAR 1ST OF REGISTER PAIR
         LH    R3,JOBZON               GET JOB TIME ZONE IN MINUTES
         DROP  R1                      DISESTABLISH REG 1 AS BASE
*
         M     R2,=A(60*1000)          CHANGE TO MILLISECONDS
         CVD   R3,JOBZONE              CONVERT TO PACKED-DECIMAL
         AP    ABSTIME,JOBZONE         ADJUST CALLER'S PARAMETER
* ------------------------------------------------------------------- *
         XR    R15,R15                 CLEAR RETURN CODE REGISTER
         TRAILER RETCODE=R15,SVAREA=SAVEAREA   RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    9D                      REGISTER SAVE AREA
TSTAMP   DC    D'0'                    TIMESTAMP IN BINARY FORMAT
TSTAMP16 DC    PL16'0'                 TIMESTAMP IN PACKED FORMAT
JOBZONE  DC    PL8'0'                  JOB TIME ZONE OFFSET
         LTORG
* =================================================================== *
         END
