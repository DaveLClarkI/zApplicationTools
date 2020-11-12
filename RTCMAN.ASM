RTC      TITLE 'RTCMAN   - RETURN CODE MANAGER FOR COBOL PROGRAMS'      00000100
         PRINT ON,GEN                                                   00000200
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*     R E T U R N - C O D E   M A N A G E R   S U B R O U T I N E     * 00000500
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
*      This routine provides  the  COBOL  programmer  with a means of * 00001700
* handling  return  codes  for  use  in  VSE  batch  jobs  which  use * 00001800
* Conditional JCL.   Additionally,  the ability to cancel or dump the * 00001900
* COBOL program is included.                                          * 00002000
*                                                                     * 00002100
* =================================================================== * 00002200
*                                                                     * 00002300
*                           IMPLEMENTATION                            * 00002400
*                                                                     * 00002500
*      This implementation uses the  VSE  $IJBCJC  routine for all of * 00002600
* the return code handling  provided.   This  eliminates the need for * 00002700
* the CALLing of ILBDSET0  or  the  PATCHing  of ILBDMNS0 in order to * 00002800
* pass return codes in register 15.  COBOL-II programs should not use * 00002900
* this routine to  pass  return  codes  (use the COBOL-II RETURN-CODE * 00003000
* special register instead) but may use it to obtain $MRC and for the * 00003100
* cancel or dump feature.                                             * 00003150
*      Implemented return code functions are as follows:              * 00003200
*                                                                     * 00003300
*      "S"TART   - get $MRC value and set $RC to zero                 * 00003400
*      "U"PDATE  - set $RC to passed return code if greater           * 00003500
*      "F"INISH  - "U"PDATE return code and display on console        * 00003600
*      "D"UMP    - "U"PDATE return code, display and issue DUMP       * 00003700
*      "C"ANCEL  - "U"PDATE return code, display and issue CANCEL ALL * 00003800
*                                                                     * 00003900
* A "S"TART request retrieves the  value  of the $MRC Conditional JCL * 00004000
* variable and sets the value of  the $RC Conditional JCL variable to * 00004100
* zero.   Subsequent requests compare  the  passed  return code value * 00004200
* with the current $RC value  to  insure that the greater return code * 00004300
* is always kept.                                                     * 00004400
*      The invoking COBOL program should  use the following data area * 00004500
* for passing parameters:                                             * 00004600
*                                                                     * 00004700
*      01  RTCMAN-PARMS.                                              * 00004800
*          05  RTC-REQU                PIC  X(01)   VALUE 'S'.        * 00004900
*              88  RTC-REQUEST-START                VALUE 'S'.        * 00005000
*              88  RTC-REQUEST-UPDATE               VALUE 'U'.        * 00005100
*              88  RTC-REQUEST-FINISH               VALUE 'F'.        * 00005200
*              88  RTC-REQUEST-DUMP                 VALUE 'D'.        * 00005300
*              88  RTC-REQUEST-CANCEL               VALUE 'C'.        * 00005400
*          05  RTC-RETN                PIC  X(01)   VALUE SPACES.     * 00005500
*              88  RTC-REQUEST-COMPLETED            VALUE ' '.        * 00005600
*              88  RTC-PUTRC-ERROR                  VALUE 'E'.        * 00005700
*              88  RTC-GETLRC-ERROR                 VALUE 'L'.        * 00005800
*              88  RTC-GETMRC-ERROR                 VALUE 'M'.        * 00005900
*          05  RTC-CODE                PIC S9(04)   COMP VALUE ZERO.  * 00006000
*                                                                     * 00006100
* =================================================================== * 00006200
*                                                                     * 00006300
*                             INVOKATION                              * 00006400
*                                                                     * 00006500
*      The subroutine is invoked in  any of five ways (depending upon * 00006600
* the function desired).   The example  below demonstrates a "normal" * 00006700
* scenario for using return codes in a COBOL program:                 * 00006800
*                                                                     * 00006900
*      PROCEDURE DIVISION.                                            * 00007000
*     * SET INITIAL ZERO RETURN CODE ("S"TART IS THE INITIAL DEFAULT) * 00007100
*          CALL   'RTCMAN'   USING RTCMAN-PARMS                       * 00007200
*          IF  RTC-REQUEST-COMPLETED                                  * 00007300
*          AND RTC-CODE NOT = ZERO                                    * 00007400
*     * CANCEL THIS JOB STEP IF JOB HAS NONZERO MAXIMUM RETURN CODE   * 00007500
*              MOVE   'C'    TO RTC-REQU                              * 00007600
*              MOVE   20     TO RTC-CODE                              * 00007700
*              CALL 'RTCMAN' USING RTCMAN-PARMS                       * 00007800
*          END-IF.                                                    * 00007900
*        .                                                            * 00008000
*        .                                                            * 00008100
*        .                                                            * 00008200
*     * UPDATE RETURN CODE ("S"TART AUTOMATICALLY SETS "U"PDATE)      * 00008300
*          MOVE your-retcode TO RTC-CODE                              * 00008400
*          CALL   'RTCMAN'   USING RTCMAN-PARMS                       * 00008500
*        .                                                            * 00008600
*        .                                                            * 00008700
*        .                                                            * 00008800
*     * DISPLAY FINAL RETURN CODE ON THE CONSOLE                      * 00008900
*          MOVE     'F'      TO RTC-REQU                              * 00009000
*          CALL   'RTCMAN'   USING RTCMAN-PARMS                       * 00009100
*          STOP RUN.                                                  * 00009200
*                                                                     * 00009300
* As a suggestion, the following return codes may be used:            * 00009400
*                                                                     * 00009500
*       0  -  All  critical  and  non-critical  processing  completed * 00009600
*             normally so execution was terminated normally;          * 00009700
*                                                                     * 00009800
*       4  -  Critical processing completed normally,  however,  some * 00009900
*             non-critical processing was not completed (e.g.  unable * 00010000
*             to close one or more  files)  but execution was able to * 00010100
*             continue to normal termination;                         * 00010200
*                                                                     * 00010300
*       8  -  Some critical  processing  was  not completed (e.g.   a * 00010400
*             record needed for  reference  was  not found) but exec- * 00010500
*             ution was able to continue to normal termination;       * 00010600
*                                                                     * 00010700
*      12  -  Critical processing  could  not  be completed (e.g.   a * 00010800
*             record needed for  update  was  not found) so execution * 00010900
*             was terminated prematurely;                             * 00011000
*                                                                     * 00011100
*      16  -  An unexpected  situation  has  occurred (e.g.   OPEN or * 00011200
*             WRITE errors) so execution was terminated prematurely.  * 00011300
*                                                                     * 00011400
*      20  -  Terminated due to prior non-zero maximum return code.   * 00011500
*                                                                     * 00011600
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00011700
         EJECT                                                          00011800
* =================================================================== * 00011900
RTCPARM  DSECT                         RTCCODE PARMS DUMMY SECTION      00012000
         USING *,R10                   ESTABLISH REG 10 AS BASE         00012100
* =================================================================== * 00012200
RTCREQU  DS    CL1                     REQUEST CODE                     00012300
RTCRETN  DS    CL1                     RETURN CODE                      00012400
RTCCODE  DS    H                       RETURN CODE VALUE                00012500
* =================================================================== * 00012600
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PDEC     DS    D                                                        00028400
MSG      DS    CL48                                                     00028500
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT                                                          00012700
* =================================================================== * 00012800
*        REGISTER EQUATES                                             * 00012900
* =================================================================== * 00013000
R0       EQU   0                       PARM REG                         00013100
R1       EQU   1                       PARM REG                         00013200
R2       EQU   2                                                        00013300
R3       EQU   3                                                        00013400
R4       EQU   4                                                        00013500
R5       EQU   5                                                        00013600
R6       EQU   6                                                        00013700
R7       EQU   7                                                        00013800
R8       EQU   8                                                        00013900
R9       EQU   9                                                        00014000
R10      EQU   10                      DSECT BASE REG                   00014100
R11      EQU   11                                                       00014200
R12      EQU   12                      CSECT BASE REG                   00014300
R13      EQU   13                      SAVEAREA ADDR                    00014400
R14      EQU   14                      RETURN REG                       00014500
R15      EQU   15                      ENTRY REG                        00014600
* =================================================================== * 00014700
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF
         PRINT ON
* =================================================================== * 00012800
         EJECT                                                          00014800
* =================================================================== * 00014900
RTCMAN   HEADER VERSION='2.1',AUTHOR='DLC',DATE='(C)MAR91',            X
               BASE=R12,SVAREA=SALEN
* =================================================================== *
         USING DYNASTOR,R13
*                                                                       00017400
         L     R10,0(,R1)              INITIALIZE PARM DSECT            00017500
         MVI   RTCRETN,C' '            INITIALIZE RETURN CODE           00017600
         XC    PDEC,PDEC                CLEAR PARM AREA
*                                                                       00017700
         IF    RTCREQU,NE,C'U',AND,RTCREQU,NE,C'F'                      00017800
         AND   RTCREQU,NE,C'C',AND,RTCREQU,NE,C'D'                      00017900
          MODESET KEY=ZERO              UNPROTECT EXTERNAL STORAGE      00018000
          LOAD $IJBCJC,SYS=YES          LOCATE SYSTEM ROUTINE           00018100
          LR   R15,R1                   GET ENTRY POINT ADDRESS         00018200
          LA   R0,2                     SELECT "GETMRC" FUNCTION        00018300
          LA   R1,PDEC                  GET ADDRESS OF PARM AREA        00018400
          BALR R14,R15                  INVOKE SYSTEM ROUTINE           00018500
          IF   R15,(NZERO,LTR),R15      IF "GETMRC" NOT SUCCESSFULL     00018600
           IF  R15,(EQUAL,CH),=H'4'      IF "GETMRC" INFO NOT AVAIL.    00018700
            XR R15,R15                    ZERO RETURN CODE REGISTER     00018800
           ELSE                          ELSE                           00018900
            MVI RTCRETN,C'M'              SET "GETMRC ERROR" CODE       00019000
           ENDIF                         ENDIF                          00019100
          ELSE                          ELSE                            00019200
           PACK PDEC,PDEC(4)             PACK ZONED DECIMAL NUMBER      00019300
           CVB R15,PDEC                  CONVERT PACKED TO BINARY       00019400
          ENDIF                         ENDIF                           00019500
          STH  R15,RTCCODE              PUT $MRC IN HALFWORD PARM       00019600
          MODESET KEY=USER              PROTECT EXTERNAL STORAGE        00019700
          XR   R11,R11                  ZERO CURRENT $RC VALUE          00019800
          MVI  RTCREQU,C'U'             CHANGE START REQUEST TO UPDATE  00019900
         ELSE                          ELSE                             00020000
          MODESET KEY=ZERO              UNPROTECT EXTERNAL STORAGE      00020100
          LOAD $IJBCJC,SYS=YES          LOCATE SYSTEM ROUTINE           00020200
          LR   R15,R1                   GET ENTRY POINT ADDRESS         00020300
          LA   R0,1                     SELECT "GETLRC" FUNCTION        00020400
          LA   R1,PDEC                  GET ADDRESS OF PARM AREA        00020500
          BALR R14,R15                  INVOKE SYSTEM ROUTINE           00020600
          IF   R15,(NZERO,LTR),R15      IF "GETRC" NOT SUCCESSFULL      00020700
           IF  R15,(EQUAL,CH),=H'4'      IF "GETRC" INFO NOT AVAIL.     00020800
            XR R15,R15                    ZERO RETURN CODE REGISTER     00020900
           ELSE                          ELSE                           00021000
            MVI RTCRETN,C'L'              SET "GETLRC ERROR" CODE       00021100
           ENDIF                         ENDIF                          00021200
          ELSE                          ELSE                            00021300
           PACK PDEC,PDEC(4)             PACK ZONED DECIMAL NUMBER      00021400
           CVB R15,PDEC                  CONVERT PACKED TO BINARY       00021500
          ENDIF                         ENDIF                           00021600
          LR   R11,R15                  SAVE CURRENT $RC VALUE          00021700
          MODESET KEY=USER              PROTECT EXTERNAL STORAGE        00021800
          IF   R11,(HT,CH),RTCCODE,OR,RTCRETN,EQ,C'L'                   00021900
           STH R11,RTCCODE              KEEP CURRENT VALUE              00022000
          ELSE                                                          00022100
           LH  R11,RTCCODE              KEEP REQUESTED VALUE            00022200
          ENDIF                                                         00022300
         ENDIF                         ENDIF                            00022400
*                                                                       00022500
         CVD   R11,PDEC                CONVERT $RC TO PACKED DECIMAL    00022600
         UNPK  PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL     00022700
         OI    PDEC+3,X'F0'            AND CLEAR ANY SIGN               00022800
*                                                                       00022900
         MODESET KEY=ZERO              SET FOR UPDATE OF SVA            00023000
         LOAD  $IJBCJC,SYS=YES         LOCATE SYSTEM ROUTINE            00023100
         LR    R15,R1                  GET ENTRY POINT ADDRESS          00023200
         LA    R0,3                    SELECT "PUTRC" FUNCTION          00023300
         LA    R1,PDEC                 GET ADDRESS OF PARM AREA         00023400
         BALR  R14,R15                 INVOKE SYSTEM ROUTINE            00023500
         IF    R15,(NZERO,LTR),R15     IF "PUTRC" NOT SUCCESSFULL       00023600
          MVI  RTCRETN,C'E'             SET "PUTRC ERROR" CODE          00023700
          STH  R15,RTCCODE              SAVE RETURN CODE                00023800
          LR   R11,R15                   AND OVERRIDE LRC               00023900
         ENDIF                         ENDIF                            00024000
         MODESET KEY=USER              RESET TO PROTECT SVA             00024100
*                                                                       00024200
         IF    RTCREQU,EQUAL,C'U',BC,FINISH                             00024300
         ENDIF                                                          00024400
*                                                                       00024500
         MVC   MSG,=C'PGM ........  SET RETURN CODE=0000              '
         IF    RTCREQU,NEQUAL,C'F'     IF NOT A "FINISH" REQUEST        00024600
          MVC  MSG+36(3),=C'AND'        PUT CONNECTOR IN MESSAGE        00024700
          IF   RTCREQU,EQUAL,C'D'       IF REQUESTED TO "DUMP"          00024800
           MVC MSG+40(6),=C'DUMPED'      PUT USER REQUEST IN MESSAGE    00024900
          ELSE                          ELSE REQUEST TO "CANCEL"        00025000
           MVC MSG+40(8),=C'CANCELED'    PUT USER REQUEST IN MESSAGE    00025100
          ENDIF                         ENDIF                           00025200
         ENDIF                         ENDIF                            00025300
         CVD   R11,PDEC                CHANGE $RC TO PACKED DECIMAL     00025400
         UNPK  MSG+30(4),PDEC+5(3)     UNPACK $RC INTO MESSAGE          00025500
         OI    MSG+33,X'F0'            AND CLEAR THE SIGN               00025600
         COMRG REG=(R1)                GET ADDRESS OF COMREG IN R1      00025700
         MVC   MSG+4(8),216(R1)        PUT EXEC PHASE NAME IN MESSAGE   00025800
         LA    R11,MSG                 GET ADDRESS OF I/O AREA
         STCM  R11,B'0111',CONSOLE+25  PUT IN CONSOLE CCW
         OPEN  CONSOLE                 OPEN CONSOLE FOR MESSAGES        00025900
         PUT   CONSOLE                 DISPLAY MESSAGE ON CONSOLE       00026000
         CLOSE CONSOLE                 CLOSE CONSOLE                    00026100
*                                                                       00026200
         IF    RTCREQU,EQUAL,C'F',BC,FINISH                             00026300
         ENDIF                                                          00026400
*                                                                       00026500
         IF    RTCREQU,EQUAL,C'D'      DUMP PARTITION                   00026600
          DUMP                                                          00026700
         ENDIF                                                          00026800
*                                                                       00026900
         CANCEL ALL                    CANCEL PARTITION                 00027000
* ------------------------------------------------------------------- * 00027100
FINISH   DS    0H                                                       00027200
* ------------------------------------------------------------------- * 00027300
         XR    R15,R15                 ZERO RETURN CODE REG             00027500
         TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== * 00027800
         EJECT                                                          00027900
* =================================================================== * 00028000
STORAGE  DS    0D                                                       00028100
* =================================================================== * 00028200
SALEN    DC    A(DYNALENG)
         LTORG                                                          00028700
* =================================================================== * 00028800
         EJECT                                                          00028900
* =================================================================== * 00029000
CONSOLE  DTFCN DEVADDR=SYSLOG,BLKSIZE=L'MSG,IOAREA1=0,MODNAME=RTCWTO    00029100
* =================================================================== * 00029200
         END                                                            00029300
