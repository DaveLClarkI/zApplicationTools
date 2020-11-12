SBA      TITLE 'SBA3270  - IBM 3270 "SET BUFFER ADDRESS" 12-BIT ADDRESS*00000100
                CALCULATION ROUTINE'                                    00000200
         PRINT ON,NOGEN                                                 00000300
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000400
*                                                                     * 00000500
* CLARKE Industries, Inc.                             Written by:     * 00000600
* 2100 Highway 265                                 Dave Leonard Clark * 00000700
* Springdale, AR  72764                              (501)750-8248    * 00000800
*                                                                     * 00000900
* =================================================================== * 00001000
*                                                                     * 00001100
*                               PURPOSE                               * 00001200
*                                                                     * 00001300
*      This routine provides the  programmer  of IBM 3270 datastreams * 00001400
* with an  efficient  means  of  converting  character-encoded buffer * 00001500
* addresses  into  relative  and  absolute  address  values  and from * 00001600
* relative or absolute address values.                                * 00001700
*                                                                     * 00001800
* =================================================================== * 00001900
*                                                                     * 00002000
*                           IMPLEMENTATION                            * 00002100
*                                                                     * 00002200
*      The invoking COBOL program should  use the following data area * 00002300
* for passing parameters:                                             * 00002400
*                                                                     * 00002500
*     *                                                               * 00002600
*     * IBM 3270 "SET BUFFER ADDRESS" 12-BIT ADDRESS CALCULATION      * 00002700
*     *                                                               * 00002800
*      01  SBA3270-PARMS.                                             * 00002900
*          05  SBA-REQU                PIC  X(1)    VALUE 'E'.        * 00003000
*              88  SBA-DECODE-ADDR-FROM-CHARS       VALUE 'D'.        * 00003100
*              88  SBA-ENCODE-ADDR-FROM-NUMBS       VALUE 'E'.        * 00003200
*          05  SBA-RETN                PIC  X(1)    VALUE SPACES.     * 00003300
*              88  SBA-REQUEST-COMPLETED            VALUE ' '.        * 00003400
*              88  SBA-INVALID-REQU                 VALUE 'E'.        * 00003500
*              88  SBA-INVALID-POSN                 VALUE 'P'.        * 00003600
*              88  SBA-INVALID-ROWN                 VALUE 'R'.        * 00003700
*              88  SBA-INVALID-COLN                 VALUE 'C'.        * 00003800
*              88  SBA-INVALID-HGHT                 VALUE 'H'.        * 00003900
*              88  SBA-INVALID-WDTH                 VALUE 'W'.        * 00004000
*          05  SBA-ADDR                             VALUE SPACES.     * 00004100
*              10  SBA-HADR            PIC  X(1).                     * 00004200
*              10  SBA-LADR            PIC  X(1).                     * 00004300
*          05  SBA-POSN                PIC S9(4)    COMP   VALUE ZERO.* 00004400
*          05  SBA-ROWN                PIC S9(4)    COMP   VALUE ZERO.* 00004500
*          05  SBA-COLN                PIC S9(4)    COMP   VALUE ZERO.* 00004600
*          05  SBA-HGHT                PIC S9(4)    COMP   VALUE +24. * 00004700
*          05  SBA-WDTH                PIC S9(4)    COMP   VALUE +80. * 00004800
*                                                                     * 00004900
* =================================================================== * 00005000
*                                                                     * 00005100
*                             INVOKATION                              * 00005200
*                                                                     * 00005300
*      First you must inform the routine as to the dimensions of your * 00005400
* screen.  The following is an easy way to do this:                   * 00005500
*                                                                     * 00005600
*          EXEC CICS ASSIGN                                           * 00005700
*                    SCRNHT(SBA-HGHT)                                 * 00005800
*                    SCRNWD(SBA-WDTH)                                 * 00005900
*                    END-EXEC.                                        * 00006000
*                                                                     * 00006100
* Then the subroutine is invoked in  one  of two ways (depending upon * 00006200
* the function desired):                                              * 00006300
*                                                                     * 00006400
*     * DECODE SBA-ADDR INTO SBA-POSN, SBA-ROWN, AND SBA-COLN         * 00006500
*          MOVE     'D'      TO SBA-REQU                              * 00006600
*          MOVE char-address TO SBA-ADDR                              * 00006700
*          CALL  'SBA3270'   USING SBA3270-PARMS                      * 00006800
*          IF  NOT SBA-REQUEST-COMPLETED                              * 00006900
*     *        your-error-handling-code                               * 00007000
*          ELSE                                                       * 00007100
*     *        your-position-evaluation-code                          * 00007200
*          END-IF                                                     * 00007300
*                                                                     * 00007400
*                               - or -                                * 00007500
*                                                                     * 00007600
*     * ENCODE SBA-ROWN AND SBA-COLN INTO SBA-ADDR                    * 00007700
*          MOVE     'E'      TO SBA-REQU                              * 00007800
*          MOVE    ZEROES    TO SBA-POSN                              * 00007900
*          MOVE your-row-num TO SBA-ROWN                              * 00008000
*          MOVE your-col-num TO SBA-COLN                              * 00008100
*          CALL  'SBA3270'   USING SBA3270-PARMS                      * 00008200
*          IF  NOT SBA-REQUEST-COMPLETED                              * 00008300
*     *        your-error-handling-code                               * 00008400
*          ELSE                                                       * 00008500
*     *        your-address-handling-code                             * 00008600
*          END-IF                                                     * 00008700
*                                                                     * 00008800
* Alternatively,  the "encode" example  above  may  be coded using an * 00008900
* absolute screen position (relative  to  1)  instead of relative row * 00009000
* and column numbers (which,  for your information, are also relative * 00009100
* to 1) as follows:                                                   * 00009200
*                                                                     * 00009300
*     * ENCODE SBA-POSN INTO SBA-ADDR                                 * 00009400
*          MOVE     'E'      TO SBA-REQU                              * 00009500
*          MOVE your-position TO SBA-POSN                             * 00009600
*          CALL  'SBA3270'   USING SBA3270-PARMS                      * 00006800
*          IF  NOT SBA-REQUEST-COMPLETED                              * 00009800
*     *        your-error-handling-code                               * 00009900
*          ELSE                                                       * 00010000
*     *        your-address-handling-code                             * 00010100
*          END-IF                                                     * 00010200
*                                                                     * 00010300
* Note that character-encoded buffer addresses are always relative to * 00010400
* absolute screen position zero  as  defined  by the IBM 3270 Inform- * 00010500
* ation Display System.                                               * 00010600
*                                                                     * 00010700
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00010800
         EJECT                                                          00010900
* =================================================================== * 00011000
SBAPARM  DSECT                                                          00011100
         USING SBAPARM,R11         ESTABLISH REG 11 AS BASE             00011200
* =================================================================== * 00011300
SBAREQU  DS    CL1                 REQUEST CODE                         00011400
SBARETN  DS    CL1                 RETURN CODE                          00011500
SBAADDR  DS    XL2                 BUFFER ADDRESS  (REL. TO ZERO)       00011600
SBAPOSN  DS    H                   BUFFER POSITION (REL. TO ONE)        00011700
SBAROWN  DS    H                   BUFFER ROW      (REL. TO ONE)        00011800
SBACOLN  DS    H                   BUFFER COLUMN   (REL. TO ONE)        00011900
SBAHGHT  DS    H                   3270 SCREEN HEIGHT                   00012000
SBAWDTH  DS    H                   3270 SCREEN WIDTH                    00012100
* =================================================================== * 00012200
         EJECT                                                          00012300
* =================================================================== * 00012400
*        REGISTER USAGE                                                 00012500
* =================================================================== * 00012600
R0       EQU   0                                                        00012700
R1       EQU   1                   TRT PARM REG                         00012800
R2       EQU   2                   TRT PARM REG                         00012900
R3       EQU   3                                                        00013000
R4       EQU   4                                                        00013100
R5       EQU   5                                                        00013200
R6       EQU   6                   POSITION VALUE/MSB OF ADDRESS        00013300
R7       EQU   7                   POSITION VALUE/LSB OF ADDRESS        00013400
R8       EQU   8                   ROW VALUE                            00013500
R9       EQU   9                   COLUMN VALUE                         00013600
R10      EQU   10                  SCREEN WIDTH VALUE                   00013700
R11      EQU   11                  DSECT BASE REG (SBAPARM)             00013800
R12      EQU   12                  CSECT BASE REG (SBA3270)             00013900
R13      EQU   13                  REGISTER SAVE AREA                   00014000
R14      EQU   14                  RETURN REG                           00014100
R15      EQU   15                  ENTRY REG                            00014200
* =================================================================== * 00014300
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         COPY  IF
* =================================================================== *
         EJECT                                                          00014400
* =================================================================== * 00014500
SBA3270  CSECT                                                          00014600
         USING SBA3270,R15         ESTABLISH ENTRY-POINT REG AS BASE    00014700
* =================================================================== * 00014800
         B     START               BRANCH AROUND CORE EYE-CATCHER       00014900
*                                                                       00015000
         DC    C'SBA3270 '         PROGRAM                              00015100
         DC    C'1.0 '             VERSION                              00015200
         DC    C'DLC '             AUTHOR                               00015300
         DC    C'(C)MAR90'         DATE                                 00015400
*                                                                       00015500
START    DS    0H                                                       00015600
         DROP  R15                 DISESTABLISH TEMP BASE REGISTER      00015700
         SAVE  (14,12)             SAVE CALLER'S REGISTERS              00015800
         LR    R12,R15             INITIALIZE ROUTINE BASE REGISTER     00015900
         USING SBA3270,R12         ESTABLISH PERM BASE REGISTER         00016000
*                                                                       00016100
         L     R11,0(,R1)          GET ADDRESS OF PARAMETERS            00016200
         LH    R10,SBAWDTH         GET WIDTH VALUE FROM PARM AREA       00016300
         LH    R9,SBAHGHT          GET HEIGHT VALUE FROM PARM AREA      00016400
*                                                                       00016500
         IF    R10,(EQ,CH),=H'160'                                      00016600
          IF   R9,(EQ,CH),=H'25',BC,SETUP,B,ERROR                       00016700
          ENDIF                                                         00016800
         ENDIF                                                          00016900
*                                                                       00017000
         IF    R10,(EQ,CH),=H'132'                                      00017100
          IF   R9,(EQ,CH),=H'27',BC,SETUP,B,ERROR                       00017200
          ENDIF                                                         00017300
         ENDIF                                                          00017400
*                                                                       00017500
         IF    R10,(EQ,CH),=H'80'                                       00017600
          IF   R9,(EQ,CH),=H'24',BC,SETUP                               00017700
          ENDIF                                                         00017800
          IF   R9,(EQ,CH),=H'32',BC,SETUP                               00017900
          ENDIF                                                         00018000
          IF   R9,(EQ,CH),=H'43',BC,SETUP                               00018100
ERROR      MVI SBARETN,C'H'        SIGNAL HEIGHT VALUE ERROR            00018200
           B   RETURN              AND GO RETURN TO CALLER              00018300
          ENDIF                                                         00018400
         ENDIF                                                          00018500
*                                                                       00018600
         MVI   SBARETN,C'W'        SIGNAL WIDTH VALUE ERROR             00018700
         B     RETURN              AND GO RETURN TO CALLER              00018800
*                                                                       00018900
SETUP    EQU   *                                                        00019000
         IF    SBAREQU,EQ,C'D',BC,DECODE                                00019100
         ENDIF                                                          00019200
         IF    SBAREQU,EQ,C'E',BC,ENCODE                                00019300
         ENDIF                                                          00019400
*                                                                       00019500
         MVI   SBARETN,C'E'        SET RETURN CODE                      00019600
         B     RETURN              GO RETURN TO CALLER                  00019700
         EJECT                                                          00019800
* ------------------------------------------------------------------- * 00019900
DECODE   EQU   *   CONVERT ADDR CHARACTERS TO POSN/ROWN/COLN NUMBERS    00020000
* ------------------------------------------------------------------- * 00020100
         IF    SBAADDR,(NZ,TRT),TRTABLE                                 00020200
          MVI  SBARETN,C'A'        SIGNAL ADDR VALUE ERROR              00020300
          B    RETURN              AND GO RETURN TO CALLER              00020400
         ENDIF                                                          00020500
*                                                                       00020600
         TR    SBAADDR,CVTABLE     CONVERT ADDR VALUE TO NUMBERS        00020700
         XR    R6,R6               CLEAR ADDR VALUE REGISTER 7          00020800
         ICM   R6,B'0011',SBAADDR  GET ADDR VALUE FROM PARM AREA        00020900
         TR    SBAADDR,CVTABLE     PRESERVE ADDR VALUE AS CHARACTERS    00021000
*                                                                       00021100
         SRDL  R6,8                RIGHT JUSTIFY MOST SIG. 6 BITS       00021300
         SLL   R7,2                LEFT JUSTIFY LEAST SIG. 6 BITS       00021500
         SLDL  R6,6                JOIN MSB/LSB IN REGISTER 6           00021600
         LA    R6,1(,R6)           INCREMENT 1 FOR ABSOLUTE POS VALUE   00021800
         STH   R6,SBAPOSN          STORE ABSOLUTE POS IN PARM AREA      00021900
*                                                                       00022000
         IF    R6,(HIGHER,C),=F'4000'                                   00022100
          MVI  SBARETN,C'P'        SIGNAL POSITION ERROR                00022200
          B    RETURN              AND GO RETURN TO CALLER              00022300
         ENDIF                                                          00022400
*                                                                       00022500
         BCTR  R6,R0               DECREMENT 1 FOR RELATIVE POS VALUE   00022600
         SRDA  R6,32(0)            CONVERT TO 64-BIT NUMBER             00022700
         DR    R6,R10              DIVIDE BY WIDTH VALUE                00022800
         LA    R8,1(,R7)           ABSOLUTE ROW IS QUOTIENT PLUS 1      00022900
         LA    R9,1(,R6)           ABSOLUTE COL IS REMAINDER PLUS 1     00023000
         STH   R8,SBAROWN          STORE ABSOLUTE ROW IN PARM AREA      00023100
         STH   R9,SBACOLN          STORE ABSOLUTE COL IN PARM AREA      00023200
*                                                                       00023300
         IF    R8,(NPLUS,LTR),R8,OR,R8,(HIGHER,CH),SBAHGHT              00023400
          MVI  SBARETN,C'R'        SIGNAL ROW VALUE ERROR               00023500
          B    RETURN              AND GO RETURN TO CALLER              00023600
         ENDIF                                                          00023700
         IF    R9,(NPLUS,LTR),R9,OR,R9,(HIGHER,CH),SBAWDTH              00023800
          MVI  SBARETN,C'C'        SIGNAL COL VALUE ERROR               00023900
          B    RETURN              AND GO RETURN TO CALLER              00024000
         ENDIF                                                          00024100
*                                                                       00024200
         MVI   SBARETN,C' '        SIGNAL NORMAL COMPLETION             00024300
         B     RETURN              GO RETURN TO CALLER                  00024400
         EJECT                                                          00024500
* ------------------------------------------------------------------- * 00024600
ENCODE   EQU   *   CONVERT POSN/ROWN/COLN NUMBERS TO ADDR CHARACTERS    00024700
* ------------------------------------------------------------------- * 00024800
         LH    R6,SBAPOSN          GET POS VALUE FROM PARM AREA         00024900
         LH    R8,SBAROWN          GET ROW VALUE FROM PARM AREA         00025000
         LH    R9,SBACOLN          GET COL VALUE FROM PARM AREA         00025100
*                                                                       00025200
         IF    R6,(NPLUS,LTR),R6                                        00025300
          IF   R8,(NPLUS,LTR),R8,OR,R8,(HIGHER,CH),SBAHGHT              00025400
           MVI SBARETN,C'R'        SIGNAL ROW VALUE ERROR               00025500
           B   RETURN              AND GO RETURN TO CALLER              00025600
          ENDIF                                                         00025700
          IF   R9,(NPLUS,LTR),R9,OR,R9,(HIGHER,CH),SBAWDTH              00025800
           MVI SBARETN,C'C'        SIGNAL COL VALUE ERROR               00025900
           B   RETURN              AND GO RETURN TO CALLER              00026000
          ENDIF                                                         00026100
          BCTR R8,R0               DECREMENT 1 FOR RELATIVE ROW VALUE   00026200
          MH   R8,SBAWDTH          MULTIPLY ROW BY SCREEN WIDTH         00026300
          AR   R8,R9               ADD ABS. COL TO GET ABSOLUTE POS     00026400
          LR   R6,R8               COPY RESULT AS SCREEN POSITION       00026500
          STH  R6,SBAPOSN          STORE ABSOLUTE POS IN PARM AREA      00026600
         ENDIF                                                          00026700
*                                                                       00026800
         IF    R6,(HIGHER,C),=F'4000'                                   00026900
          MVI  SBARETN,C'P'        SIGNAL POSITION ERROR                00027000
          B    RETURN              AND GO RETURN TO CALLER              00027100
         ENDIF                                                          00027200
*                                                                       00027300
         BCTR  R6,R0               DECREMENT 1 FOR RELATIVE POS VALUE   00027400
         SRDA  R6,32(0)            CONVERT TO 64-BIT NUMBER             00027500
         DR    R6,R10              DIVIDE BY WIDTH VALUE                00027600
         LA    R8,1(,R7)           ABSOLUTE ROW IS QUOTIENT PLUS 1      00027700
         LA    R9,1(,R6)           ABSOLUTE COL IS REMAINDER PLUS 1     00027800
         STH   R8,SBAROWN          STORE ABSOLUTE ROW IN PARM AREA      00027900
         STH   R9,SBACOLN          STORE ABSOLUTE COL IN PARM AREA      00028000
*                                                                       00028100
         LH    R6,SBAPOSN          REFRESH POS VALUE FROM PARM AREA     00028200
         BCTR  R6,R0               DECREMENT 1 FOR RELATIVE POS         00028300
         SRDL  R6,6                RIGHT JUSTIFY MOST SIG. 6 BITS       00028400
         SRL   R7,2                JUSTIFY LEAST SIG. 6 BITS TO 8 BITS  00028500
         SLDL  R6,8                JOIN MSB/LSB IN REGISTER 6           00028600
         STCM  R6,B'0011',SBAADDR  STORE ADDR VALUE IN PARM AREA        00028700
         TR    SBAADDR,CVTABLE     CONVERT ADDR VALUE TO CHARACTERS     00028900
*                                                                       00029000
         MVI   SBARETN,C' '        SIGNAL NORMAL COMPLETION             00029100
         B     RETURN              RETURN TO CALLER                     00029200
         EJECT                                                          00029300
* ------------------------------------------------------------------- * 00029400
RETURN   EQU   *                                                        00029500
* ------------------------------------------------------------------- * 00029600
         RETURN (14,12)            RETURN TO CALLER                     00029700
* =================================================================== * 00029800
         EJECT                                                          00029900
* =================================================================== * 00030000
STORAGE  DS    0D                                                       00030100
* =================================================================== * 00030200
CVTABLE  EQU   *                                                        00030300
         DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'                      00030400
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'                      00030500
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'                      00030600
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'                      00030700
         DC    X'000000000000000000000A0B0C0D0E0F'                      00030800
         DC    X'100000000000000000001A1B1C1D1E1F'                      00030900
         DC    X'202100000000000000002A2B2C2D2E2F'                      00031000
         DC    X'000000000000000000003A3B3C3D3E3F'                      00031100
         DC    X'00000000000000000000000000000000'                      00031200
         DC    X'00000000000000000000000000000000'                      00031300
         DC    X'00000000000000000000000000000000'                      00031400
         DC    X'00000000000000000000000000000000'                      00031500
         DC    X'00010203040506070809000000000000'                      00031600
         DC    X'00111213141516171819000000000000'                      00031700
         DC    X'00002223242526272829000000000000'                      00031800
         DC    X'30313233343536373839000000000000'                      00031900
TRTABLE  EQU   *                                                        00032000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00032100
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00032200
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00032300
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00032400
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000'                      00032500
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000'                      00032600
         DC    X'0000FFFFFFFFFFFFFFFF000000000000'                      00032700
         DC    X'FFFFFFFFFFFFFFFFFFFF000000000000'                      00032800
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00032900
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00033000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00033100
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                      00033200
         DC    X'FF000000000000000000FFFFFFFFFFFF'                      00033300
         DC    X'FF000000000000000000FFFFFFFFFFFF'                      00033400
         DC    X'FFFF0000000000000000FFFFFFFFFFFF'                      00033500
         DC    X'00000000000000000000FFFFFFFFFFFF'                      00033600
         LTORG                                                          00033700
* =================================================================== * 00033800
         END                                                            00033900
