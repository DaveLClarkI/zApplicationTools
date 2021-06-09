ABS      TITLE 'ABSTIME  - DECODE CICS ABSTIME INTO DDDDDDDHHMMSSUU'    00000100
         PRINT ON,GEN                                                   00000200
ABSTIME  AMODE 31
ABSTIME  RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
* DAPSCO Information Services                         Written by:     * 00000500
* 3110 Kettering Blvd.                             Dave Leonard Clark * 00000600
* Dayton, OH  45439                                (937)294-5331 x234 * 00000700
*                                                                     * 00000800
* =================================================================== * 00000900
*                                                                     * 00001000
*                               PURPOSE                               * 00001100
*                                                                     * 00001200
*      This routine is  intended  for  use  in  CICS programs for the * 00001300
* purpose of extracting the milliseconds portion of the absolute time * 00001400
* (ABSTIME) value returned by  the CICS ASKTIME command.   The reason * 00001500
* this routine is needed is  because the CICS FORMATTIME command only * 00001600
* formats down to the seconds level of the ABSTIME value.             * 00001700
*                                                                     * 00002800
* =================================================================== * 00002900
*                                                                     * 00003000
*                           IMPLEMENTATION                            * 00003100
*                                                                     * 00003200
*      This  routine  executes  as  a  CALLed  subroutine  in  a CICS * 00003300
* Command-level COBOL  program  (linked  as  an  object module).   No * 00003400
* system or CICS changes are required to impliment this routine.      * 00003500
*      It accepts  the  CICS  15-digit  ABSTIME  value  and returns a * 00004500
* series of values that represent the number of days, hours, minutes, * 00004500
* seconds,  and hundredths of  a  second  (in milliseconds) that that * 00004500
* ABSTIME value represents.                                           * 00004500
*                                                                     * 00004500
* The invoking COBOL program should  use  the following data area for * 00005000
* passing parameters:                                                 * 00005100
*                                                                     * 00005200
*      01  ABSTIME-PARMS.                                             * 00005300
*          05  ABS-INPUT               PIC S9(15)   COMP-3.           * 00008700
*          05  ABS-DAYS                PIC S9(11)   COMP-3.           * 00008800
*          05  ABS-HOURS               PIC S9(03)   COMP-3.           * 00008800
*          05  ABS-MINUTES             PIC S9(03)   COMP-3.           * 00008800
*          05  ABS-SECONDS             PIC S9(03)   COMP-3.           * 00008800
*          05  ABS-UNITS               PIC S9(07)   COMP-3.           * 00008800
*          05  ABS-WORKAREA            PIC  X(20).                    * 00008800
*                                                                     * 00011300
* =================================================================== * 00004600
*                                                                     * 00004700
*                             INVOCATION                              * 00004800
*                                                                     * 00004900
*      The subroutine could be invoked as follows:                    * 00011800
*                                                                     * 00013000
*     * GET CURRENT TIME AND EXTRACT MILLISECONDS                     * 00013100
*          EXEC CICS ASKTIME ABSTIME(ABS-INPUT) END-EXEC              * 00013300
*          CALL  'ABSTIME'   USING ABSTIME-PARMS                      * 00013500
*                                                                     * 00014100
*      On return, the remaining fields in the parameter block will be * 00014200
* initialized with the  associated  values  extracted from the passed * 00014300
* value.                                                              * 00014400
*                                                                     * 00015100
*      However,  the most common use for  this capability may be when * 00015200
* one wishes to know the elapsed time between two ABSTIME values with * 00015300
* millisecond accuracy.   Thus,  one would  probably actually use the * 00015400
* routine as follows:                                                 * 00009700
*                                                                     * 00009700
*     * GET START TIME                                                * 00013100
*          EXEC CICS ASKTIME ABSTIME(BEG-TIME) END-EXEC               * 00013300
*            .                                                        * 00009700
*            .                                                        * 00009700
*            .                                                        * 00009700
*     * GET STOP TIME                                                 * 00013100
*          EXEC CICS ASKTIME ABSTIME(END-TIME) END-EXEC               * 00013300
*            .                                                        * 00009700
*            .                                                        * 00009700
*            .                                                        * 00009700
*     * COMPUTE DIFFERENCE AND EXTRACT MILLISECONDS                   * 00013100
*          SUBTRACT BEG-TIME FROM END-TIME                            * 00013100
*                            GIVING ABS-INPUT                         * 00013100
*          CALL  'ABSTIME'   USING ABSTIME-PARMS                      * 00013500
*                                                                     * 00014100
*      On return,  the remaining fields  in the parameter block would * 00014200
* contain the elapsed days, hours, minutes, seconds, and milliseconds * 00014300
* between the two absolute time values.                               * 00014400
*                                                                     * 00014100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00009800
         EJECT                                                          00009900
* =================================================================== * 00010000
         EJECT                                                          00010400
* =================================================================== * 00010500
ABSPARM  DSECT                    ABSTIME PARMS DUMMY SECTION           00010600
         USING *,R10              ESTABLISH REG 10 AS BASE              00010700
* =================================================================== * 00010800
ABSINPUT DS    PL8                ABSOLUTE TIME INPUT                   00010900
ABSDAYS  DS    PL6                ABSOLUTE DAYS OUTPUT                  00011000
ABSHOURS DS    PL2                ABSOLUTE HOURS OUTPUT                 00020400
ABSMINTS DS    PL2                ABSOLUTE HOURS OUTPUT                 00020400
ABSSECDS DS    PL2                ABSOLUTE HOURS OUTPUT                 00020400
ABSUNITS DS    PL4                ABSOLUTE HOURS OUTPUT                 00020400
*
PTEMP    DS    PL8                                                      00020000
PWORK    DS    0PL11                                                    00020000
PDAYS    DS    PL6                                                      00020000
PREM     DS    PL5                                                      00020000
* =================================================================== * 00020500
         EJECT                                                          00020600
* =================================================================== * 00020700
*        REGISTER USAGES                                                00020800
* =================================================================== * 00020900
R0       EQU   0                   PARM REG                             00021000
R1       EQU   1                   PARM REG                             00021100
R2       EQU   2                                                        00021200
R3       EQU   3                                                        00021300
R4       EQU   4                   DIVIDE REMAINDER (MILLISECS VALUE)   00021400
R5       EQU   5                   DIVIDE QUOTIENT (SECONDS VALUE)      00021500
R6       EQU   6                   TEMP REG (MINUTES VALUE)             00021600
R7       EQU   7                   TEMP REG (HOURS VALUE)               00021700
R8       EQU   8                   TEMP REG (DAYS VALUE)                00021800
R9       EQU   9                                                        00021900
R10      EQU   10                  ABSPARM DSECT BASE REG               00022000
R11      EQU   11                  LINK REGISTER                        00022100
R12      EQU   12                                                       00022200
R13      EQU   13                  REGISTER SAVEAREA                    00022300
R14      EQU   14                  RETURN REG                           00022400
R15      EQU   15                  ENTRY REG                            00022500
* =================================================================== * 00022600
         EJECT                                                          00022700
* =================================================================== * 00022800
ABSTIME  CSECT                                                          00022900
         USING ABSTIME,R15         ESTABLISH ENTRY-POINT REG AS BASE    00023000
* =================================================================== * 00023100
         B     START               BRANCH AROUND CORE EYE-CATCHER       00023200
*                                                                       00023300
         DC    CL8'ABSTIME'        PROGRAM                              00023400
         DC    CL4'1.0'            VERSION                              00023500
         DC    CL4'DLC'            AUTHOR                               00023600
         DC    CL8'(C)OCT01'       DATE                                 00023700
*                                                                       00023800
START    DS    0H                                                       00023900
         SAVE  (14,12)             SAVE CALLER'S REGISTERS              00024000
         LR    R12,R15             INITIALIZE ROUTINE BASE REGISTER     00024100
         DROP  R15                 DISESTABLISH TEMP BASE REGISTER      00024200
         USING ABSTIME,R12         ESTABLISH PERM BASE REGISTER         00024300
*                                                                       00024400
         L     R10,0(,R1)          GET ADDRESS OF PARAMETERS            00024500
*                                                                       00027700
         ZAP   PWORK,ABSINPUT      PREPARE ABSTIME INPUT                00027900
         DP    PWORK,=P'86400000'  DIV. BY MILLISECONDS IN A DAY        00028000
         ZAP   ABSDAYS,PDAYS       SAVE DAYS TO OUTPUT FIELD            00029600
         ZAP   PTEMP,PREM          COPY REMAINDER TO WORK FIELD
         CVB   R4,PTEMP            CONVERT REMAINDER TO BINARY
         SRDA  R4,32               CONVERT REMAINDER TO 64-BIT NUMBER   00028300
         D     R4,=F'3600000'      DIV. BY MILLISECONDS IN AN HOUR      00028400
         LR    R7,R5               SAVE EVEN NUMBER OF HOURS            00028600
         SRDA  R4,32               CONVERT REMAINDER TO 64-BIT NUMBER   00028700
         D     R4,=F'60000'        DIV. BY MILLISECONDS IN A MINUTE     00028800
         LR    R6,R5               SAVE EVEN NUMBER OF MINUTES          00029000
         SRDA  R4,32               CONVERT REMAINDER TO 64-BIT NUMBER   00029100
         D     R4,=F'1000'         DIV. BY MILLISECONDS IN A SECOND     00029200
*                                                                       00029800
         CVD   R7,PTEMP            CONVERT HOURS TO DECIMAL             00029900
         ZAP   ABSHOURS,PTEMP      SAVE TO OUTPUT FIELD                 00030000
*
         CVD   R6,PTEMP            CONVERT MINUTES TO DECIMAL           00029500
         ZAP   ABSMINTS,PTEMP      SAVE TO OUTPUT FIELD                 00029600
*                                                                       00029800
         CVD   R5,PTEMP            CONVERT SECONDS TO DECIMAL           00029900
         ZAP   ABSSECDS,PTEMP      SAVE TO OUTPUT FIELD                 00030000
*                                                                       00029800
         CVD   R4,PTEMP            CONVERT MILLISECONDS TO DECIMAL      00029900
         ZAP   ABSUNITS,PTEMP      SAVE TO OUTPUT FIELD                 00030000
*                                                                       00029800
         RETURN (14,12)            RETURN TO CALLER                     00058800
* =================================================================== * 00058900
         EJECT                                                          00059000
* =================================================================== * 00059100
STORAGE  DS    0D                                                       00059200
* =================================================================== * 00059300
         LTORG                                                          00061000
* =================================================================== * 00061100
         END                                                            00061200
