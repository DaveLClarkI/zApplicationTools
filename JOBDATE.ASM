JDT      TITLE 'JOBDATE  - SET PARTITION JOB DATE TO SELECTED VALUE'    00000100
         PRINT ON,GEN                                                   00000200
JOBDATE  AMODE 31
JOBDATE  RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*           J O B   D A T E   S E T T I N G   R O U T I N E           * 00000500
*                                                                     * 00000600
*                                                                     * 00000700
* CLARKE Industries, Inc.                             Written by:     * 00000800
* 2100 Highway 265                                 Dave Leonard Clark * 00000900
* Springdale, AR  72764                              (501)750-8248    * 00001000
*                                                                     * 00001100
* No warranty is expressed  or  implied.   Neither the author nor the * 00001200
* company is responsible for  any  loss  or damage resulting from the * 00001300
* use of this code.  Source is provided on an as-is basis.            * 00001400
*                                                                     * 00001500
* =================================================================== * 00001600
*                                                                     * 00001700
*                               PURPOSE                               * 00001800
*                                                                     * 00001900
*      This routine allows a program to change the partition job date * 00002000
* without requiring a JCL "// DATE" card.   Although this routine may * 00002100
* be executed from JCL, it's greatest power is when it is executed as * 00002200
* a subroutine of some other  program which has intelligently decided * 00002300
* what the date should be.                                            * 00002400
*                                                                     * 00002500
* =================================================================== * 00002600
*                                                                     * 00002700
*                           IMPLEMENTATION                            * 00002800
*                                                                     * 00002900
*      This routine does not validate the passed date parameter.  The * 00003000
* invoking COBOL  program  should  use  the  following  data area for * 00003100
* passing a date:                                                     * 00003200
*                                                                     * 00003300
*      01  JOBDATE-PARMS.                                             * 00003400
*          05  JOB-DLEN                PIC  9(03)   COMP VALUE 8.     * 00003500
*          05  JOB-DATE                PIC  X(08)   VALUE SPACES.     * 00003600
*                                                                     * 00003700
* =================================================================== * 00003800
*                                                                     * 00003900
*                             INVOCATION                              * 00004000
*                                                                     * 00004100
*      The subroutine is invoked as  follows (the date format MUST be * 00004200
* as shown but, obviously, does not have to be a literal value):      * 00004300
*                                                                     * 00004400
*     * SET PARTITION JOB DATE                                        * 00004500
*          MOVE  'MM/DD/YY'  TO JOB-DATE                              * 00004600
*          CALL  'JOBDATE'   USING JOBDATE-PARMS                      * 00004700
*                                                                     * 00004800
* Note that this  routine  changes  the  "MM/DD"  to  "DD/MM" if your * 00004900
* system has the "STDOPT DATE=DMY" standard option specified.         * 00005000
*                                                                     * 00005100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00005200
         EJECT                                                          00005300
* =================================================================== * 00005400
JDTPARM  DSECT                    JOBDATE PARMS DUMMY SECTION           00005500
         USING *,R10              ESTABLISH REG 10 AS BASE              00005600
* =================================================================== * 00005700
JDTLENG  DS    H                  LENGTH OF REQUEST JOB DATE            00005800
JDTREQU  DS    CL8                REQUEST JOB DATE                      00005900
* =================================================================== * 00006000
         EJECT                                                          00006100
* =================================================================== * 00006200
*        REGISTER USAGES                                              * 00006300
* =================================================================== * 00006400
R0       EQU   0                  PARM REG                              00006500
R1       EQU   1                  PARM REG                              00006600
R2       EQU   2                                                        00006700
R3       EQU   3                                                        00006800
R4       EQU   4                                                        00006900
R5       EQU   5                                                        00007000
R6       EQU   6                                                        00007100
R7       EQU   7                                                        00007200
R8       EQU   8                                                        00007300
R9       EQU   9                                                        00007400
R10      EQU   10                 JDTPARM DSECT BASE REG                00007500
R11      EQU   11                                                       00007600
R12      EQU   12                 JOBDATE CSECT BASE REG                00007700
R13      EQU   13                 REGISTER SAVEAREA                     00007800
R14      EQU   14                 RETURN REG                            00007900
R15      EQU   15                 ENTRY REG                             00008000
* =================================================================== * 00008100
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
* =================================================================== * 00012800
         EJECT                                                          00008200
* =================================================================== * 00008300
JOBDATE  CSECT                                                          00008400
         USING JOBDATE,R15        ESTABLISH ENTRY-POINT REG AS BASE     00008500
* =================================================================== * 00008600
         B     START              BRANCH AROUND CORE EYE-CATCHER        00008700
*                                                                       00008800
         DC    C'JOBDATE '        PROGRAM                               00008900
         DC    C'1.0 '            VERSION                               00009000
         DC    C'DLC '            AUTHOR                                00009100
         DC    C'(C)NOV91'        DATE                                  00009200
*                                                                       00009300
START    DS    0H                                                       00009400
         STM   R14,R12,12(R13)    SAVE CALLER'S REGISTERS               00009500
         LR    R12,R15            INITIALIZE ROUTINE BASE REGISTER      00009600
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00009700
         USING JOBDATE,R12        ESTABLISH PERM BASE REGISTER          00009800
*                                                                       00009900
         L     R10,0(,R1)         GET ADDRESS OF PARAMETER              00010000
         IF    R1,(EQ,CR),R12,OR,JDTLENG,(NE,CLC),=H'8'                 00010100
          LA   R15,8               SET "NOPARM" RETURN CODE             00010200
         ELSE                     ELSE                                  00010300
          COMRG REG=(R9)           GET ADDRESS OF COMREG IN REG 9       00010400
          MODESET KEY=ZERO         SET FOR UPDATE OF EXTERNAL STORAGE   00010500
          MVC  0(8,R9),JDTREQU     PUT PARAMETER IN JOB DATE FIELD      00010600
          IF   53(R9),(ON,TM),X'80' IF 'DD/MM/YY' FORMAT IN USE         00010700
           XC  0(2,R9),3(R9)         SWAP MM                            00010800
           XC  3(2,R9),0(R9)          AND DD                            00010900
           XC  0(2,R9),3(R9)           FIELDS IN PLACE                  00011000
           MVC JDTREQU,0(R9)        PUT JOB DATE FIELD IN PARAMETER     00011100
          ENDIF                                                         00011200
          OI   59(R9),X'02'        SET IMPLICIT '// DATE' PROCESSED     00011300
          MODESET KEY=USER         RESET TO PROTECT EXTERNAL STORAGE    00011400
          XR   R15,R15             SET "OK" RETURN CODE                 00011500
         ENDIF                    ENDIF                                 00011600
*                                                                       00011700
         ST    R15,16(,R13)       SAVE RETURN CODE IN CALLER'S SAVEAREA 00011800
         LM    R14,R12,12(R13)    RESTORE CALLER'S REGISTERS            00011900
         BR    R14                RETURN TO CALLER                      00012000
* =================================================================== * 00012100
         EJECT                                                          00012200
* =================================================================== * 00012300
STORAGE  DS    0D                                                       00012400
* =================================================================== * 00012500
         LTORG                                                          00012600
* =================================================================== * 00012700
         END                                                            00012800
