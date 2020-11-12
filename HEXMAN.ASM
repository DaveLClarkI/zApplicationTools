HEX      TITLE 'HEXMAN   - HEX MANIPULATION FOR COBOL PROGRAMS'         00000100
         PRINT ON,GEN                                                   00000200
HEXMAN   AMODE 31
HEXMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*        H E X   M A N I P U L A T I O N   S U B R O U T I N E        * 00000500
*                 F O R   C O B O L   P R O G R A M S                 * 00000600
*                                                                     * 00000700
*                                                                     * 00000800
* CLARKE Industries, Inc.                             Written by:     * 00000900
* 2100 Highway 265                                 Dave Leonard Clark * 00001000
* Springdale, AR  72764                              (501)750-8248    * 00001100
*                                                                     * 00001200
* =================================================================== * 00001300
*                                                                     * 00001400
*                               PURPOSE                               * 00001500
*                                                                     * 00001600
*      This routine provides the  COBOL  programmer with an efficient * 00001700
* means of converting character data into hex data and from hex data. * 00001800
*                                                                     * 00001900
* =================================================================== * 00002000
*                                                                     * 00002100
*                           IMPLEMENTATION                            * 00002200
*                                                                     * 00002300
*      This implementation  will  convert  to/from  a  maximum of 256 * 00002400
* bytes  of  character  data  from/to  a  maximum  of  512  bytes  of * 00002500
* hexidecimal  data.   The  invoking  COBOL  program  should  use the * 00002600
* following data area for passing parameters:                         * 00002700
*                                                                     * 00002800
*      01  HEXMAN-PARMS.                                              * 00002900
*          05  HEX-REQU                PIC  X(1)    VALUE 'D'.        * 00003000
*              88  HEX-DECODE-FROM-CHAR             VALUE 'D'.        * 00003100
*              88  HEX-ENCODE-FROM-HEX              VALUE 'E'.        * 00003200
*          05  HEX-RETN                PIC  X(1)    VALUE SPACES.     * 00003300
*              88  HEX-REQUEST-COMPLETED            VALUE ' '.        * 00003400
*              88  HEX-INVALID-REQU                 VALUE 'E'.        * 00003500
*              88  HEX-INVALID-LENG                 VALUE 'L'.        * 00003600
*              88  HEX-INVALID-HEXE                 VALUE 'X'.        * 00003700
*          05  HEX-LENG                PIC  9(4)    COMP VALUE ZEROES.* 00003800
*          05  HEX-CHRT.                                              * 00003900
*            10  HEX-CHRE              PIC  X(1)                      * 00004000
*                          OCCURS 0256 TIMES                          * 00004100
*                          DEPENDING   HEX-LENG.                      * 00004200
*          05  FILLER                  PIC  X(1).                     * 00004300
*          05  HEX-HEXT.                                              * 00004400
*            10  HEX-HEXE              PIC  X(2)                      * 00004500
*                          OCCURS 0256 TIMES                          * 00004600
*                          DEPENDING   HEX-LENG.                      * 00004700
*          05  FILLER                  PIC  X(1).                     * 00004800
*                                                                     * 00004900
* =================================================================== * 00005000
*                                                                     * 00005100
*                             INVOKATION                              * 00005200
*                                                                     * 00005300
*      The subroutine is invoked in  one  of two ways (depending upon * 00005400
* the function desired):                                              * 00005500
*                                                                     * 00005600
*     * DECODE HEX-CHRT INTO HEX-HEXT                                 * 00005700
*          MOVE     'D'      TO HEX-REQU                              * 00005800
*          MOVE your-chrleng TO HEX-LENG                              * 00005900
*          MOVE your-chrdata TO HEX-CHRT                              * 00006000
*          CALL   'HEXMAN'   USING HEXMAN-PARMS                       * 00006100
*          IF  NOT HEX-REQUEST-COMPLETED                              * 00006200
*     *        your-error-handling-code                               * 00006300
*          ELSE                                                       * 00006400
*     *        your-hex-evaluation-code                               * 00006500
*          END-IF                                                     * 00006600
*                                                                     * 00006700
*                               - or -                                * 00006800
*                                                                     * 00006900
*     * ENCODE HEX-HEXT INTO HEX-CHRT                                 * 00007000
*          MOVE     'E'      TO HEX-REQU                              * 00007100
*          MOVE your-hexleng TO HEX-LENG                              * 00007200
*          MOVE your-hexdata TO HEX-HEXT                              * 00007300
*          CALL   'HEXMAN'   USING HEXMAN-PARMS                       * 00007400
*          IF  NOT HEX-REQUEST-COMPLETED                              * 00007500
*     *        your-error-handling-code                               * 00007600
*          ELSE                                                       * 00007700
*     *        your-chr-evaluation-code                               * 00007800
*          END-IF                                                     * 00007900
*                                                                     * 00008000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00008100
         EJECT                                                          00008200
* =================================================================== * 00008300
HEXPARM  DSECT                                                          00008400
         USING HEXPARM,R10        ESTABLISH REG 10 AS BASE              00008500
* =================================================================== * 00008600
HEXREQU  DS    CL1                REQUEST CODE                          00008700
HEXRETN  DS    CL1                RETURN CODE                           00008800
HEXLENG  DS    H                  TABLES LENGTH                         00008900
HEXCHRT  DS    0C                 CHAR TABLE                            00009000
* =================================================================== * 00009100
         EJECT                                                          00009200
* =================================================================== * 00009300
*        REGISTER USAGE                                                 00009400
* =================================================================== * 00009500
R0       EQU   0                                                        00009600
R1       EQU   1                                                        00009700
R2       EQU   2                                                        00009800
R3       EQU   3                                                        00009900
R4       EQU   4                                                        00010000
R5       EQU   5                                                        00010100
R6       EQU   6                                                        00010200
R7       EQU   7                  CODING LENGTH REG                     00010300
R8       EQU   8                  CHAR CODING TABLE ADDR                00010400
R9       EQU   9                  HEX CODING TABLE ADDR                 00010500
R10      EQU   10                 DSECT BASE REG (HEXPARM)              00010600
R11      EQU   11                                                       00010700
R12      EQU   12                 CSECT BASE REG (HEXMAN)               00010800
R13      EQU   13                 REGS SAVE AREA                        00010900
R14      EQU   14                 RETURN REG                            00011000
R15      EQU   15                 ENTRY REG                             00011100
* =================================================================== * 00011200
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
         COPY  WHILE
* =================================================================== * 00012800
         EJECT                                                          00011300
* =================================================================== * 00011400
HEXMAN   CSECT                                                          00011500
         USING HEXMAN,R15         ESTABLISH ENTRY-POINT REG AS BASE     00011600
* =================================================================== * 00011700
         B     START              BRANCH AROUND CORE EYE-CATCHER        00011800
*                                                                       00011900
         DC    C'HEXMAN  '        PROGRAM                               00012000
         DC    C'1.0 '            VERSION                               00012100
         DC    C'DLC '            AUTHOR                                00012200
         DC    C'(C)DEC89'        DATE                                  00012300
*                                                                       00012400
START    DS    0H                                                       00012500
         SAVE  (14,12)            SAVE CALLER'S REGISTERS               00012600
         LR    R12,R15            INITIALIZE ROUTINE BASE REGISTER      00012700
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00012800
         USING HEXMAN,R12         ESTABLISH PERM BASE REGISTER          00012900
*                                                                       00013000
         L     R10,0(,R1)         GET ADDRESS OF PARAMETERS             00013100
         MVI   HEXRETN,C' '       INITIALIZE RETURN CODE                00013200
*                                                                       00013300
         LH    R7,HEXLENG         GET LENGTH OF CODING AREA             00013400
         IF    R7,(Z,LTR),R7,OR,R7,(HT,CH),=H'256'                      00013500
          MVI  HEXRETN,C'L'       SIGNAL LENGTH VALUE ERROR             00013600
          B    RETURN             GO RETURN TO CALLER                   00013700
         ENDIF                                                          00013800
*                                                                       00013900
         LA    R8,HEXCHRT         GET ADDRESS OF CHAR TABLE             00014000
         LR    R9,R8              COMPUTE ADDRESS OF HEX TABLE          00014100
         AR    R9,R7              AS OFFSET BY LENGTH FROM CHAR TABLE   00014200
         LA    R9,1(,R9)          PLUS 1 SLACK BYTE BETWEEN TABLES      00014300
*                                                                       00014400
         IF    HEXREQU,EQ,C'D',BC,DECODE                                00014500
         ENDIF                                                          00014600
         IF    HEXREQU,EQ,C'E',BC,ENCODE                                00014700
         ENDIF                                                          00014800
*                                                                       00014900
         MVI   HEXRETN,C'E'       SET RETURN CODE                       00015000
         B     RETURN             GO RETURN TO CALLER                   00015100
         EJECT                                                          00015200
* ------------------------------------------------------------------- * 00015300
DECODE   EQU   *                                                        00015400
* ------------------------------------------------------------------- * 00015500
         WHILE R7,(NZ,LTR),R7                                           00015600
          UNPK 0(3,R9),0(2,R8)    UNPACK CHAR INTO HEX TABLE            00015700
          TR   0(2,R9),TRTABLE    CHANGE X'FAFBFCFDFEFF' TO C'ABCDEF'   00015800
          LA   R9,2(,R9)          ADVANCE TO NEXT HEX ENTRY             00015900
          LA   R8,1(,R8)          ADVANCE TO NEXT CHAR ENTRY            00016000
          BCTR R7,0               DECREMENT CODING LENGTH               00016100
         WEND                                                           00016200
*                                                                       00016300
         MVI   0(R9),C' '         CLEAR LAST UNUSED HEX BYTE            00016400
*                                                                       00016500
         B     RETURN             GO RETURN TO CALLER                   00016600
         EJECT                                                          00016700
* ------------------------------------------------------------------- * 00016800
ENCODE   EQU   *                                                        00016900
* ------------------------------------------------------------------- * 00017000
         BCTR  R7,0               DECREMENT FOR ZERO REFERENCE          00017100
         IF    R7,(NZ,EX),ENCODET CHECK FOR ALL '0'-'9' & 'A'-'F'       00017200
          MVI  HEXRETN,C'X'       SIGNAL HEX VALUE ERROR                00017300
          B    RETURN             GO RETURN TO CALLER                   00017400
ENCODET  TRT   0(0,R9),VALIDATE   (SEE "EX" INSTRUCTION ABOVE)          00017500
         ENDIF                                                          00017600
         LA    R7,1(,R7)          ADD BACK 1 SUBTRACTED BEFORE          00017700
*                                                                       00017800
         WHILE R7,(NZ,LTR),R7                                           00017900
          TR   0(2,R9),TRTABLE    CHANGE C'ABCDEF' TO X'FAFBFCFDFEFF'   00018000
          PACK 0(2,R8),0(3,R9)    PACK HEX INTO CHAR TABLE              00018100
          TR   0(2,R9),TRTABLE    CHANGE X'FAFBFCFDFEFF' TO C'ABCDEF'   00018200
          LA   R9,2(,R9)          ADVANCE TO NEXT HEX ENTRY             00018300
          LA   R8,1(,R8)          ADVANCE TO NEXT CHAR ENTRY            00018400
          BCTR R7,0               DECREMENT CODING LENGTH               00018500
         WEND                                                           00018600
*                                                                       00018700
         MVI   0(R8),C' '         CLEAR LAST UNUSED CHAR BYTE           00018800
*                                                                       00018900
         B     RETURN             GO RETURN TO CALLER                   00019000
         EJECT                                                          00019100
* ------------------------------------------------------------------- * 00019200
RETURN   EQU   *                                                        00019300
* ------------------------------------------------------------------- * 00019400
         RETURN (14,12)           RETURN TO CALLER                      00019500
* =================================================================== * 00019600
         EJECT                                                          00019700
* =================================================================== * 00019800
STORAGE  DS    0D                                                       00019900
* =================================================================== * 00020000
TRTABLE  EQU   *-192                                                    00020100
         DC    X'00FAFBFCFDFEFF000000000000000000'                      00020200
         DC    X'00000000000000000000000000000000'                      00020300
         DC    X'00000000000000000000000000000000'                      00020400
         DC    X'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'                      00020500
*                                                                       00020600
VALIDATE EQU   *                                                        00020700
         DC    256X'FF'                DISALLOW EVERYTHING              00020800
         ORG   VALIDATE+C'A'                                            00020900
         DC    X'000000000000'         ALLOW FOR 'ABCDEF'               00021000
         ORG   VALIDATE+C'0'                                            00021100
         DC    X'00000000000000000000' ALLOW FOR '0123456789'           00021200
         ORG                                                            00021300
*                                                                       00021400
         LTORG                                                          00021500
* =================================================================== * 00021600
         END                                                            00021700
