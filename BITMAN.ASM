BIT      TITLE 'BITMAN   - BIT MANIPULATION FOR COBOL PROGRAMS'         00000100
         PRINT ON,NOGEN                                                 00000200
BITMAN   AMODE 31
BITMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*        B I T   M A N I P U L A T I O N   S U B R O U T I N E        * 00000500
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
* means of converting character data into bit data and from bit data. * 00001800
*                                                                     * 00001900
* =================================================================== * 00002000
*                                                                     * 00002100
*                           IMPLEMENTATION                            * 00002200
*                                                                     * 00002300
*      This implementation will convert to/from a maximum of one byte * 00002400
* of character data from/to a  maximum  of eight bits of binary data. * 00002500
* The invoking COBOL program should  use  the following data area for * 00002600
* passing parameters:                                                 * 00002700
*                                                                     * 00002800
*      01  BITMAN-PARMS.                                              * 00002900
*          05  BIT-REQU                PIC  X(1)    VALUE 'D'.        * 00003000
*              88  BIT-DECODE-FROM-BYTE             VALUE 'D'.        * 00003100
*              88  BIT-ENCODE-FROM-BITS             VALUE 'E'.        * 00003200
*          05  BIT-RETN                PIC  X(1)    VALUE SPACES.     * 00003300
*              88  BIT-REQUEST-COMPLETED            VALUE ' '.        * 00003400
*              88  BIT-INVALID-REQU                 VALUE 'E'.        * 00003500
*          05  FILLER                  PIC  X(1)    VALUE SPACES.     * 00003600
*          05  BIT-BYTE                PIC  X(1)    VALUE LOW-VALUE.  * 00003700
*          05  BIT-DATA.                                              * 00003800
*              10  BIT-0               PIC  9(1)    VALUE 0.          * 00003900
*                  88  BIT-0-IS-OFF                 VALUE 0.          * 00004000
*                  88  BIT-0-IS-ON                  VALUE 1.          * 00004100
*              10  BIT-1               PIC  9(1)    VALUE 0.          * 00004200
*                  88  BIT-1-IS-OFF                 VALUE 0.          * 00004300
*                  88  BIT-1-IS-ON                  VALUE 1.          * 00004400
*              10  BIT-2               PIC  9(1)    VALUE 0.          * 00004500
*                  88  BIT-2-IS-OFF                 VALUE 0.          * 00004600
*                  88  BIT-2-IS-ON                  VALUE 1.          * 00004700
*              10  BIT-3               PIC  9(1)    VALUE 0.          * 00004800
*                  88  BIT-3-IS-OFF                 VALUE 0.          * 00004900
*                  88  BIT-3-IS-ON                  VALUE 1.          * 00005000
*              10  BIT-4               PIC  9(1)    VALUE 0.          * 00005100
*                  88  BIT-4-IS-OFF                 VALUE 0.          * 00005200
*                  88  BIT-4-IS-ON                  VALUE 1.          * 00005300
*              10  BIT-5               PIC  9(1)    VALUE 0.          * 00005400
*                  88  BIT-5-IS-OFF                 VALUE 0.          * 00005500
*                  88  BIT-5-IS-ON                  VALUE 1.          * 00005600
*              10  BIT-6               PIC  9(1)    VALUE 0.          * 00005700
*                  88  BIT-6-IS-OFF                 VALUE 0.          * 00005800
*                  88  BIT-6-IS-ON                  VALUE 1.          * 00005900
*              10  BIT-7               PIC  9(1)    VALUE 0.          * 00006000
*                  88  BIT-7-IS-OFF                 VALUE 0.          * 00006100
*                  88  BIT-7-IS-ON                  VALUE 1.          * 00006200
*          05  BIT-TABLE   REDEFINES   BIT-DATA.                      * 00006300
*              10  BIT     OCCURS 08   PIC  9(1).                     * 00006400
*                  88  BIT-IS-OFF                   VALUE 0.          * 00006500
*                  88  BIT-IS-ON                    VALUE 1.          * 00006600
*                                                                     * 00006700
* =================================================================== * 00006800
*                                                                     * 00006900
*                             INVOKATION                              * 00007000
*                                                                     * 00007100
*      The subroutine is invoked in  one  of two ways (depending upon * 00007200
* the function desired):                                              * 00007300
*                                                                     * 00007400
*     * DECODE BIT-BYTE INTO BIT-DATA                                 * 00007500
*          MOVE     'D'      TO BIT-REQU                              * 00007600
*          MOVE  your-byte   TO BIT-BYTE                              * 00007700
*          CALL   'BITMAN'   USING BITMAN-PARMS                       * 00007800
*          IF  NOT BIT-REQUEST-COMPLETED                              * 00007900
*     *        your-error-handling-code                               * 00008000
*          ELSE                                                       * 00008100
*     *        your-bit-evaluation-code                               * 00008200
*          END-IF                                                     * 00008300
*                                                                     * 00008400
*                               - or -                                * 00008500
*                                                                     * 00008600
*     * ENCODE BIT-DATA INTO BIT-BYTE                                 * 00008700
*          MOVE     'E'      TO BIT-REQU                              * 00008800
*          MOVE  your-bit-0  TO BIT-0                                 * 00008900
*          MOVE  your-bit-1  TO BIT-1                                 * 00009000
*          MOVE  your-bit-2  TO BIT-2                                 * 00009100
*          MOVE  your-bit-3  TO BIT-3                                 * 00009200
*          MOVE  your-bit-4  TO BIT-4                                 * 00009300
*          MOVE  your-bit-5  TO BIT-5                                 * 00009400
*          MOVE  your-bit-6  TO BIT-6                                 * 00009500
*          MOVE  your-bit-7  TO BIT-7                                 * 00009600
*          CALL   'BITMAN'   USING BITMAN-PARMS                       * 00009700
*          IF  NOT BIT-REQUEST-COMPLETED                              * 00009800
*     *        your-error-handling-code                               * 00009900
*          ELSE                                                       * 00010000
*     *        your-byte-evaluation-code                              * 00010100
*          END-IF                                                     * 00010200
*                                                                     * 00010300
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00010400
         EJECT                                                          00010500
* =================================================================== * 00010600
BITPARM  DSECT                                                          00010700
         USING BITPARM,R10             ESTABLISH REG 10 AS BASE         00010800
* =================================================================== * 00010900
BITREQU  DS    CL1                     REQUEST CODE                     00011000
BITRETN  DS    CL1                     RETURN CODE                      00011100
         DS    CL1                                                      00011200
BITBYTE  DS    XL1                     BYTE VALUE                       00011300
BITBIT0  DS    CL1                     BIT0 VALUE                       00011400
BITBIT1  DS    CL1                     BIT1 VALUE                       00011500
BITBIT2  DS    CL1                     BIT2 VALUE                       00011600
BITBIT3  DS    CL1                     BIT3 VALUE                       00011700
BITBIT4  DS    CL1                     BIT4 VALUE                       00011800
BITBIT5  DS    CL1                     BIT5 VALUE                       00011900
BITBIT6  DS    CL1                     BIT6 VALUE                       00012000
BITBIT7  DS    CL1                     BIT7 VALUE                       00012100
* =================================================================== * 00012200
         EJECT                                                          00012300
* =================================================================== * 00012400
*        REGISTER USAGE                                                 00012500
* =================================================================== * 00012600
R0       EQU   0                                                        00012700
R1       EQU   1                                                        00012800
R2       EQU   2                                                        00012900
R3       EQU   3                                                        00013000
R4       EQU   4                                                        00013100
R5       EQU   5                                                        00013200
R6       EQU   6                                                        00013300
R7       EQU   7                                                        00013400
R8       EQU   8                                                        00013500
R9       EQU   9                                                        00013600
R10      EQU   10                      DSECT BASE REG (BITPARM)         00013700
R11      EQU   11                                                       00013800
R12      EQU   12                      CSECT BASE REG (BITMAN)          00013900
R13      EQU   13                      REGS SAVE AREA                   00014000
R14      EQU   14                      RETURN REG                       00014100
R15      EQU   15                      ENTRY REG                        00014200
* =================================================================== * 00014300
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
* =================================================================== * 00012800
         EJECT                                                          00014400
* =================================================================== * 00014500
BITMAN   CSECT                                                          00014600
         USING BITMAN,R15         ESTABLISH ENTRY-POINT REG AS BASE     00014700
* =================================================================== * 00014800
         B     START              BRANCH AROUND CORE EYE-CATCHER        00014900
*                                                                       00015000
         DC    C'BITMAN  '        PROGRAM                               00015100
         DC    C'1.0 '            VERSION                               00015200
         DC    C'DLC '            AUTHOR                                00015300
         DC    C'(C)DEC89'        DATE                                  00015400
*                                                                       00015500
START    DS    0H                                                       00015600
         SAVE  (14,12)            SAVE CALLER'S REGISTERS               00015700
         LR    R12,R15            INITIALIZE ROUTINE BASE REGISTER      00015800
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00015900
         USING BITMAN,R12         ESTABLISH PERM BASE REGISTER          00016000
*                                                                       00016100
         L     R10,0(,R1)         GET ADDRESS OF PARAMETERS             00016200
         MVI   BITRETN,C' '       INITIALIZE RETURN CODE                00016300
*                                                                       00016400
         IF    BITREQU,EQ,C'D',BC,DECODE                                00016500
         ENDIF                                                          00016600
         IF    BITREQU,EQ,C'E',BC,ENCODE                                00016700
         ENDIF                                                          00016800
*                                                                       00016900
         MVI   BITRETN,C'E'       SIGNAL REQUEST ERROR                  00017000
         B     RETURN                                                   00017100
         EJECT                                                          00017200
* ------------------------------------------------------------------- * 00017300
DECODE   EQU   *                                                        00017400
* ------------------------------------------------------------------- * 00017500
         MVC   BITBIT0(8),=C'00000000' SET ALL BITS OFF BY DEFAULT      00017600
*                                                                       00017700
         IF    BITBYTE,(O,TM),B'10000000'                               00017800
          MVI  BITBIT0,C'1'       SET BIT 0 ON                          00017900
         ENDIF                                                          00018000
         IF    BITBYTE,(O,TM),B'01000000'                               00018100
          MVI  BITBIT1,C'1'       SET BIT 1 ON                          00018200
         ENDIF                                                          00018300
         IF    BITBYTE,(O,TM),B'00100000'                               00018400
          MVI  BITBIT2,C'1'       SET BIT 2 ON                          00018500
         ENDIF                                                          00018600
         IF    BITBYTE,(O,TM),B'00010000'                               00018700
          MVI  BITBIT3,C'1'       SET BIT 3 ON                          00018800
         ENDIF                                                          00018900
         IF    BITBYTE,(O,TM),B'00001000'                               00019000
          MVI  BITBIT4,C'1'       SET BIT 4 ON                          00019100
         ENDIF                                                          00019200
         IF    BITBYTE,(O,TM),B'00000100'                               00019300
          MVI  BITBIT5,C'1'       SET BIT 5 ON                          00019400
         ENDIF                                                          00019500
         IF    BITBYTE,(O,TM),B'00000010'                               00019600
          MVI  BITBIT6,C'1'       SET BIT 6 ON                          00019700
         ENDIF                                                          00019800
         IF    BITBYTE,(O,TM),B'00000001'                               00019900
          MVI  BITBIT7,C'1'       SET BIT 7 ON                          00020000
         ENDIF                                                          00020100
* ------------------------------------------------------------------- * 00020200
         B     RETURN                                                   00020300
         EJECT                                                          00020400
* ------------------------------------------------------------------- * 00020500
ENCODE   EQU   *                                                        00020600
* ------------------------------------------------------------------- * 00020700
         MVI   BITBYTE,X'00'      SET ALL BITS IN BYTE OFF BY DEFAULT   00020800
*                                                                       00020900
         IF    BITBIT0,EQ,C'1'                                          00021000
          OI   BITBYTE,B'10000000' SET BIT 0 IN BYTE ON                 00021100
         ENDIF                                                          00021200
         IF    BITBIT1,EQ,C'1'                                          00021300
          OI   BITBYTE,B'01000000' SET BIT 1 IN BYTE ON                 00021400
         ENDIF                                                          00021500
         IF    BITBIT2,EQ,C'1'                                          00021600
          OI   BITBYTE,B'00100000' SET BIT 2 IN BYTE ON                 00021700
         ENDIF                                                          00021800
         IF    BITBIT3,EQ,C'1'                                          00021900
          OI   BITBYTE,B'00010000' SET BIT 3 IN BYTE ON                 00022000
         ENDIF                                                          00022100
         IF    BITBIT4,EQ,C'1'                                          00022200
          OI   BITBYTE,B'00001000' SET BIT 4 IN BYTE ON                 00022300
         ENDIF                                                          00022400
         IF    BITBIT5,EQ,C'1'                                          00022500
          OI   BITBYTE,B'00000100' SET BIT 5 IN BYTE ON                 00022600
         ENDIF                                                          00022700
         IF    BITBIT6,EQ,C'1'                                          00022800
          OI   BITBYTE,B'00000010' SET BIT 6 IN BYTE ON                 00022900
         ENDIF                                                          00023000
         IF    BITBIT7,EQ,C'1'                                          00023100
          OI   BITBYTE,B'00000001' SET BIT 7 IN BYTE ON                 00023200
         ENDIF                                                          00023300
* ------------------------------------------------------------------- * 00023400
         B     RETURN                                                   00023500
         EJECT                                                          00023600
* ------------------------------------------------------------------- * 00023700
RETURN   EQU   *                                                        00023800
* ------------------------------------------------------------------- * 00023900
         RETURN (14,12)            RETURN TO CALLER                     00024000
* =================================================================== * 00024100
         EJECT                                                          00024200
* =================================================================== * 00024300
STORAGE  DS    0D                                                       00024400
* =================================================================== * 00024500
         LTORG                                                          00024600
* =================================================================== * 00024700
         END                                                            00024800
