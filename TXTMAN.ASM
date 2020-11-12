TXT      TITLE 'TXTMAN   - TEXT MANIPULATION FOR COBOL PROGRAMS'        00000100
         PRINT ON,NOGEN                                                 00000200
TXTMAN   AMODE 31
TXTMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*       T E X T   M A N I P U L A T I O N   S U B R O U T I N E       * 00000500
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
* means of performing various text string operations.                 * 00001800
*                                                                     * 00001900
* =================================================================== * 00002000
*                                                                     * 00002100
*                           IMPLEMENTATION                            * 00002200
*                                                                     * 00002300
*      This implementation will  handle  a  master  "buffer" of up to * 00002400
* 9,998 characters in length and  one  or more parameter "string"s of * 00002500
* up to 256 characters in length.   The invoking COBOL program should * 00002600
* use the following data areas for passing parameters:                * 00002700
*                                                                     * 00002800
*      01  TXTMAN-PARMS.                                              * 00002900
*          05  TXT-REQU                PIC  X(01)   VALUE SPACES.     * 00003000
*              88  TXT-REQUEST-COPY                 VALUE 'C'.        * 00003100
*              88  TXT-REQUEST-DELETE               VALUE 'D'.        * 00003200
*              88  TXT-REQUEST-FIND                 VALUE 'F'.        * 00003300
*              88  TXT-REQUEST-INSERT               VALUE 'I'.        * 00003400
*              88  TXT-REQUEST-OVERLAY              VALUE 'O'.        * 00003500
*              88  TXT-REQUEST-PARSE                VALUE 'P'.        * 00003600
*          05  TXT-RETN                PIC  X(01)   VALUE SPACES.     * 00003700
*              88  TXT-REQUEST-COMPLETED            VALUE ' '.        * 00003800
*              88  TXT-STRING-NOT-FOUND             VALUE 'N'.        * 00003900
*              88  TXT-STRING-WAS-FOUND             VALUE 'Y'.        * 00004000
*              88  TXT-PARSED-AT-BUFEND             VALUE LOW-VALUE.  * 00004100
*              88  TXT-INVALID-REQU                 VALUE 'E'.        * 00004200
*              88  TXT-INVALID-PNTR                 VALUE 'P'.        * 00004300
*              88  TXT-INVALID-BUFLEN               VALUE 'B'.        * 00004400
*              88  TXT-INVALID-STRLEN               VALUE 'S'.        * 00004500
*          05  TXT-PNTR                PIC  9(04)   COMP VALUE ZEROES.* 00004600
*     * THESE DELIMITERS MAY BE CHANGED FOR THE TEXT "PARSE" REQUEST  * 00004700
*          05  TXT-DLMS.                                              * 00004800
*              10  TXT-DLM1            PIC  X(01)   VALUE '('.        * 00004900
*              10  TXT-DLM2            PIC  X(01)   VALUE ')'.        * 00005000
*              10  TXT-DLM3            PIC  X(01)   VALUE ','.        * 00005100
*              10  TXT-DLM4            PIC  X(01)   VALUE QUOTE.      * 00005200
*              10  TXT-DLM5            PIC  X(01)   VALUE '='.        * 00005300
*              10  TXT-DLM6            PIC  X(01)   VALUE SPACE.      * 00005400
*              10  TXT-DLM7            PIC  X(01)   VALUE SPACE.      * 00005500
*              10  TXT-DLM8            PIC  X(01)   VALUE SPACE.      * 00005600
*          05  FILLER                  PIC  X(260)  VALUE LOW-VALUES. * 00005700
*                                                                     * 00005800
*      01  TXTMAN-BUFFER.                                             * 00005900
*          05  TXT-MSTR-BUFLEN         PIC  9(04)   COMP VALUE 9998.  * 00006000
*          05  TXT-MSTR-BUFFER.                                       * 00006100
*              10  TXT-MSTR-BYTE       PIC  X(01)                     * 00006200
*                          OCCURS 9998 TIMES                          * 00006300
*                          DEPENDING   TXT-MSTR-BUFLEN.               * 00006400
*                                                                     * 00006500
*      01  TXTMAN-STRING.                                             * 00006600
*          05  TXT-PARM-STRLEN         PIC  9(04)   COMP VALUE 256.   * 00006700
*          05  TXT-PARM-STRING.                                       * 00006800
*              10  TXT-PARM-BYTE       PIC  X(01)                     * 00006900
*                          OCCURS 0256 TIMES                          * 00007000
*                          DEPENDING   TXT-PARM-STRLEN.               * 00007100
*                                                                     * 00007200
* =================================================================== * 00007300
*                                                                     * 00007400
*                             INVOKATION                              * 00007500
*                                                                     * 00007600
*      The subroutine is invoked in  any  of six ways (depending upon * 00007700
* the function desired).   Supplied functions  provide the ability to * 00007800
* "C"OPY/"P"ARSE,   "I"NSERT/"O"VERLAY,   "D"ELETE  and  "F"IND  data * 00007900
* strings.   These functions operate based  on  a master "buffer" and * 00008000
* parameter "string"(s) as follows:                                   * 00008100
*                                                                     * 00008200
*      "C"OPY    - copy from master buffer into parm string(s)        * 00008300
*      "P"ARSE   - parse master buffer into parm string(s)            * 00008400
*      "I"NSERT  - insert parm string(s) into master buffer           * 00008500
*      "O"VERLAY - overlay parm string(s) onto master buffer          * 00008600
*      "D"ELETE  - delete parm string from master buffer              * 00008700
*      "F"IND    - find parm string in master buffer                  * 00008800
*                                                                     * 00008900
*      The  example  below  demonstrates  how  to  "F"IND  a  string, * 00009000
* starting at  a  given  position,  in  the  buffer.   Note  that all * 00009100
* operations start at the  buffer  position  pointed to by "TXT-PNTR" * 00009200
* and proceed  for  the  length  of  the parameter string(s);  except * 00009300
* "P"ARSE where the length  is based on delimiter location(s).   Also * 00009400
* note that  all  functions  (except  "D"ELETE) automatically advance * 00009500
* "TXT-PNTR"  an  amount  equal  to   the  length  of  the  parameter * 00009600
* string(s);  except "F"IND which advances "TXT-PNTR" to the location * 00009700
* of the string, if found, or to the end of the buffer.               * 00009800
*                                                                     * 00009900
*          MOVE     'F'      TO TXT-REQU                              * 00010000
*          MOVE      80      TO TXT-PNTR                              * 00010100
*          MOVE your-buflen  TO TXT-BUFLEN                            * 00010200
*          MOVE your-buffer  TO TXT-BUFFER                            * 00010300
*          MOVE your-strlen  TO TXT-STRLEN                            * 00010400
*          MOVE your-string  TO TXT-STRING                            * 00010500
*          CALL   'TXTMAN'   USING TXTMAN-PARMS                       * 00010600
*                                  TXTMAN-BUFFER                      * 00010700
*                                  TXTMAN-STRING                      * 00010800
*          IF  TXT-STRING-NOT-FOUND                                   * 00010900
*     *        your-string-not-found-code                             * 00011000
*          ELSE-IF NOT TXT-STRING-WAS-FOUND                           * 00011100
*     *        your-error-handling-code                               * 00011200
*          ELSE                                                       * 00011300
*     *        your-string-was-found-code                             * 00011400
*          END-IF                                                     * 00011500
*                                                                     * 00011600
*      You  do  not  have  to   use  the  structures  represented  by * 00011700
* "TXTMAN-BUFFER" and "TXTMAN-STRING" (or  those names) in the "CALL" * 00011800
* statement shown above.  You may code your own names, and associated * 00011900
* structures,  as long as  those  structures  begin  with a half-word * 00012000
* length field,  as shown,  and  are  not  longer  than the structure * 00012100
* lengths shown.   In fact, functions that support multiple parameter * 00012200
* strings ("C"OPY/"P"ARSE and "I"NSERT/"O"VERLAY) will *require* that * 00012300
* you code your own parameter  string  structures in order to specify * 00012400
* multiple parameter string names in the "CALL" statement.            * 00012500
*                                                                     * 00012600
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00012700
         EJECT                                                          00012800
* =================================================================== * 00012900
*        PROGRAM DUMMY SECTIONS                                         00013000
* =================================================================== * 00013100
TXTPARM  DSECT                                                          00013200
         USING TXTPARM,R8         ESTABLISH REG 8 AS BASE               00013300
TXTREQU  DS    CL1                REQUEST CODE                          00013400
TXTRETN  DS    CL1                RETURN CODE                           00013500
TXTPNTR  DS    H                  STRING POINTER                        00013600
TXTDLMS  DS    CL8                PARSE DELIMITERS                      00013700
SAVEADDR DS    F                  (INTERNAL USE ONLY)                   00013800
TRTTABLE DS    CL256              (INTERNAL USE ONLY)                   00013900
* ------------------------------------------------------------------- * 00014000
TXTMSTR  DSECT                                                          00014100
         USING TXTMSTR,R9         ESTABLISH REG 9 AS BASE               00014200
TXTBUFL  DS    H                  MASTER BUFFER LENGTH                  00014300
TXTBUFR  DS    9998CL1            MASTER BUFFER                         00014400
* ------------------------------------------------------------------- * 00014500
TXTINPT  DSECT                                                          00014600
         USING TXTINPT,R10        ESTABLISH REG 10 AS BASE              00014700
TXTSTRL  DS    H                  INPUT/OUTPUT STRING LENGTH            00014800
TXTSTRG  DS    256CL1             INPUT/OUTPUT STRING                   00014900
* =================================================================== * 00015000
         EJECT                                                          00015100
* =================================================================== * 00015200
*        REGISTER USAGES                                                00015300
* =================================================================== * 00015400
R0       EQU   0                  PARM REG                              00015500
R1       EQU   1                  PARM REG (INPUT & TRT INSTRUCTION)    00015600
R2       EQU   2                  PARM REG (TRT INSTRUCTION)            00015700
R3       EQU   3                  WORK REG (PNTR/COUNTER/COMPARATOR)    00015800
R4       EQU   4                  WORK REG (FROM ADDRESS)               00015900
R5       EQU   5                  WORK REG (FROM LENGTH)                00016000
R6       EQU   6                  WORK REG ( TO    ADDRESS)             00016100
R7       EQU   7                  WORK REG ( TO    LENGTH)              00016200
R8       EQU   8                  TXTPARM DSECT BASE REG                00016300
R9       EQU   9                  TXTMSTR DSECT BASE REG                00016400
R10      EQU   10                 TXTINPT DSECT BASE REG                00016500
R11      EQU   11                 NOT USED                              00016600
R12      EQU   12                 TXTMAN CSECT BASE REG                 00016700
R13      EQU   13                 REGISTER SAVEAREA                     00016800
R14      EQU   14                 RETURN REG                            00016900
R15      EQU   15                 ENTRY REG                             00017000
* =================================================================== * 00017100
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
         COPY  WHILE
* =================================================================== * 00012800
         EJECT                                                          00017200
* =================================================================== * 00017300
TXTMAN   CSECT                                                          00017400
         USING TXTMAN,R15         ESTABLISH ENTRY-POINT REG AS BASE     00017500
* =================================================================== * 00017600
         B     START              BRANCH AROUND CORE EYE-CATCHER        00017700
*                                                                       00017800
         DC    C'TXTMAN  '        PROGRAM                               00017900
         DC    C'1.0 '            VERSION                               00018000
         DC    C'DLC '            AUTHOR                                00018100
         DC    C'(C)DEC89'        DATE                                  00018200
*                                                                       00018300
START    DS    0H ESTABLISH ADDRESSABILITY                              00018400
         SAVE  (14,12)            SAVE CALLER'S REGISTERS               00018500
         LR    R12,R15            SET UP PRIMARY BASE REGISTER          00018600
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00018700
         USING TXTMAN,R12         ESTABLISH PERM BASE REGISTER          00018800
*                                                                       00018900
         LM    R8,R10,0(R1)       GET ADDRESSES OF PARAMETERS           00019000
         LA    R10,0(,R10)        CLEAR LAST PARM INDICATOR             00019100
         MVI   TXTRETN,C' '       CLEAR RETURN CODE FIELD               00019200
         LA    R1,8(,R1)          INCREMENT TO LAST PARM REFERENCED     00019300
         ST    R1,SAVEADDR        SAVE PARAMETER ADDRESSES POINTER      00019400
         B     CHECK              GO BEGIN PARAMETER CHECKING           00019500
*                                                                       00019600
SETUP    EQU   *                                                        00019700
         L     R1,SAVEADDR        GET PARAMETER ADDRESS POINTER         00019800
         L     R10,0(,R1)         GET ADDRESS OF LAST PARAMETER         00019900
         ST    R10,SAVEADDR       STORE ADDRESS FOR TESTING             00020000
*                                                                       00020100
         IF    SAVEADDR,(O,TM),X'80',BC,RETURN                          00020200
         ENDIF                                                          00020300
*                                                                       00020400
         L     R10,4(,R1)         GET ADDRESS OF NEXT PARAMETER         00020500
         LA    R10,0(,R10)        CLEAR LAST PARM INDICATOR             00020600
         MVI   TXTRETN,C' '       CLEAR RETURN CODE FIELD               00020700
         LA    R1,4(,R1)          INCREMENT TO LAST PARM REFERENCED     00020800
         ST    R1,SAVEADDR        SAVE PARAMETER ADDRESSES POINTER      00020900
         B     CHECK              GO BEGIN PARAMETER CHECKING           00021000
         EJECT                                                          00021100
* ------------------------------------------------------------------- * 00021200
CHECK    EQU   *                                                        00021300
* ------------------------------------------------------------------- * 00021400
         LH    R3,TXTBUFL         GET MASTER BUFFER LENGTH              00021500
         IF    TXTREQU,NE,C'P'                                          00021600
          IF   R3,(Z,LTR),R3,OR,R3,(H,CH),=H'9998'                      00021700
           MVI TXTRETN,C'B'       SIGNAL BUFFER LENGTH ERROR            00021800
           B   RETURN             GO RETURN TO CALLER                   00021900
          ENDIF                                                         00022000
          LH   R3,TXTSTRL         GET PARAMETER STRING LENGTH           00022100
          IF   R3,(Z,LTR),R3,OR,R3,(H,CH),=H'256'                       00022200
           MVI TXTRETN,C'S'       SIGNAL STRING LENGTH ERROR            00022300
           B   RETURN             GO RETURN TO CALLER                   00022400
          ENDIF                                                         00022500
         ELSE                                                           00022600
          IF   R3,(Z,LTR),R3,OR,R3,(H,CH),=H'256'                       00022700
           MVI TXTRETN,C'B'       SIGNAL BUFFER LENGTH ERROR            00022800
           B   RETURN             GO RETURN TO CALLER                   00022900
          ENDIF                                                         00023000
         ENDIF                                                          00023100
*                                                                       00023200
         LH    R3,TXTPNTR         GET BUFFER POINTER VALUE              00023300
         IF    TXTREQU,NE,C'F'                                          00021600
          IF   R3,(Z,LTR),R3,OR,R3,(H,CH),TXTBUFL                       00023400
           MVI TXTRETN,C'P'       SIGNAL POINTER VALUE ERROR            00023500
           B   RETURN             GO RETURN TO CALLER                   00023600
          ENDIF                                                         00023700
         ELSE                                                           00023700
          IF   R3,(M,LTR),R3,OR,R3,(H,CH),TXTBUFL                       00023400
           MVI TXTRETN,C'P'       SIGNAL POINTER VALUE ERROR            00023500
           B   RETURN             GO RETURN TO CALLER                   00023600
          ENDIF                                                         00023700
         ENDIF                                                          00023700
*                                                                       00023800
         IF    TXTREQU,EQ,C'C',BC,TXTCOPY                               00023900
         ENDIF                                                          00024000
         IF    TXTREQU,EQ,C'D',BC,TXTDLET                               00024100
         ENDIF                                                          00024200
         IF    TXTREQU,EQ,C'F',BC,TXTFIND                               00024300
         ENDIF                                                          00024400
         IF    TXTREQU,EQ,C'I',BC,TXTISRT                               00024500
         ENDIF                                                          00024600
         IF    TXTREQU,EQ,C'O',BC,TXTCOPY                               00024700
         ENDIF                                                          00024800
         IF    TXTREQU,EQ,C'P',BC,TXTPARS                               00024900
         ENDIF                                                          00025000
*                                                                       00025100
         MVI   TXTRETN,C'E'       SIGNAL TEXT REQUEST ERROR             00025200
         B     RETURN             GO RETURN TO CALLER                   00025300
         EJECT                                                          00025400
* ------------------------------------------------------------------- * 00025500
TXTCOPY  EQU   *   COPY THE REQUESTED STRING FROM/OVER THE BUFFER       00025600
* ------------------------------------------------------------------- * 00025700
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00025800
         AH    R4,TXTPNTR         ADD POINTER VALUE                     00025900
         BCTR  R4,R0              LESS 1 FOR ZERO OFFSET                00026000
         LH    R5,TXTSTRL         GET STRING LENGTH                     00026100
         BCTR  R5,R0              LESS 1 FOR ZERO REFERENCE             00026200
*                                                                       00026300
         IF    TXTREQU,EQ,C'C',OR,TXTREQU,EQ,C'P'                       00026400
          EX   R5,TXTMVCX         COPY(PARSE), DO IT                    00026500
         ELSE                                                           00026600
          EX   R5,TXTMVCO         OVERLAY(INSERT), DO IT                00026700
         ENDIF                                                          00026800
*                                                                       00026900
         LH    R3,TXTPNTR         GET POINTER VALUE                     00027000
         AH    R3,TXTSTRL         ADD STRING LENGTH                     00027100
         STH   R3,TXTPNTR         PUT POINTER VALUE                     00027200
         B     SETUP              GO GET THE NEXT PARAMETER             00027300
*                                                                       00027400
TXTMVCX  MVC   TXTSTRG(0),0(R4)   EXTRACT STRING FROM BUFFER            00027500
TXTMVCO  MVC   0(0,R4),TXTSTRG    OVERLAY BUFFER WITH STRING            00027600
         EJECT                                                          00027700
* ------------------------------------------------------------------- * 00027800
TXTDLET  EQU   *   DELETE THE REQUESTED AMOUNT FROM BUFFER              00027900
* ------------------------------------------------------------------- * 00028000
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00028100
         AH    R4,TXTPNTR         ADD POINTER VALUE                     00028200
         BCTR  R4,R0              LESS 1 FOR ZERO OFFSET                00028300
         LH    R5,TXTBUFL         GET LENGTH OF BUFFER                  00028400
         SH    R5,TXTPNTR         SUBTRACT POINTER VALUE                00028500
         LA    R5,1(,R5)          PLUS 1 FOR ACTUAL LENGTH              00028600
         LR    R6,R4              COPY BUFFER ADDRESS + POINTER         00028700
         AH    R6,TXTSTRL         ADD DELETE LENGTH                     00028800
         LR    R7,R5              COPY LENGTH OF BUFFER - POINTER       00028900
         SH    R7,TXTSTRL         SUBTRACT DELETE LENGTH                00029000
         ICM   R7,B'1000',TXTSTRG GET PAD CHARACTER                     00029100
         MVCL  R4,R6              PERFORM OPERATION                     00029200
         B     RETURN             GO RETURN TO CALLER                   00029300
         EJECT                                                          00029400
* ------------------------------------------------------------------- * 00029500
TXTFIND  EQU   *   FIND THE REQUESTED STRING IN BUFFER                  00029600
* ------------------------------------------------------------------- * 00029700
         XC    TRTTABLE,TRTTABLE  CLEAR TRANSLATE TABLE                 00029800
         XR    R3,R3              CLEAR REGISTER                        00029900
         IC    R3,TXTSTRG         GET FIRST CHARACTER OF STRING         00030000
         STC   R3,TRTTABLE(R3)    PUT IT IN TRANSLATE & TEST TABLE      00030100
*                                                                       00030200
         XR    R1,R1              CLEAR ADDRESS REGISTER                00030300
         XR    R2,R2              CLEAR FUNCTION REGISTER               00030400
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00030500
         AH    R4,TXTPNTR         ADD POINTER VALUE                     00030600
         LH    R5,TXTBUFL         GET BUFFER LENGTH                     00030700
         SH    R5,TXTPNTR         SUBTRACT POINTER VALUE                00030800
         MVI   TXTRETN,C'N'       ASSUME TEXT WILL NOT BE FOUND         00030900
*                                                                       00031000
         WHILE R5,(P,LTR),R5                                            00031100
          LH   R7,=H'256'         GET MAXIMUM LENGTH FOR "TRT" INSTR.   00031200
          IF   R7,(H,CR),R5       IF MAX LEN > BUF LEN                  00031300
           LR  R7,R5               USE REMAINING BUFFER LENGTH          00031400
          ENDIF                                                         00031500
          BCTR R7,R0              SUBTRACT 1 FOR ZERO REFERENCE         00031600
          IF   R7,(Z,EX),TXTTRT   IF CHARACTER NOT FOUND                00031700
           LA  R7,1(,R7)           ADD BACK THE 1 SUBTRACTED BEFORE     00031800
           AR  R4,R7               ADD SEARCH LEN TO BUFFER ADDRESS     00031900
           SR  R5,R7               SUBTRACT SEARCH LEN FROM BUFFER LEN  00032000
           WLOOP                   GO REPEAT LOOP                       00032100
TXTTRT     TRT 0(0,R4),TRTTABLE    (SEE "EX" INSTRUCTION ABOVE)         00032200
          ENDIF                                                         00032300
          SR   R1,R4              CALCULATE LENGTH SEARCHED             00032400
          AR   R4,R1              ADD SEARCH LENGTH TO BUFFER ADDRESS   00032500
          LH   R3,TXTSTRL         GET STRING LENGTH                     00032600
          BCTR R3,R0                  LESS 1 FOR ZERO REFERENCE         00032700
          IF   R3,(EQ,EX),TXTCLC  IF STRING MATCHES                     00032800
           MVI TXTRETN,C'Y'        SIGNAL THAT THE STRING WAS FOUND     00032900
           WEXIT                   GO EXIT LOOP                         00033000
TXTCLC     CLC 0(0,R4),TXTSTRG     (SEE "EX" INSTRUCTION ABOVE)         00033100
          ENDIF                                                         00033200
          LA   R4,1(,R4)          INCREMENT PAST THIS CHAR OCCURANCE    00033300
          SR   R5,R1              SUBTRACT SEARCH LEN FROM BUFFER LEN   00033400
          BCTR R5,R0                  LESS 1 FOR THIS CHAR OCCURANCE    00033500
         WEND                                                           00033600
*                                                                       00033700
         LA    R3,TXTBUFR         GET BUFFER ADDRESS                    00033800
         SR    R4,R3              SUBTRACT BUFFER ADR FROM PNTR ADR     00033900
         LA    R4,1(,R4)          INCREMENT FOR POINTER VALUE           00034000
         STH   R4,TXTPNTR         PUT POINTER VALUE                     00034100
         B     RETURN             GO RETURN TO CALLER                   00034200
         EJECT                                                          00034300
* ------------------------------------------------------------------- * 00034400
TXTISRT  EQU   *   INSERT THE REQUESTED STRING INTO BUFFER              00034500
* ------------------------------------------------------------------- * 00034600
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00034700
         LH    R5,TXTBUFL         GET BUFFER LENGTH                     00034800
         AR    R4,R5              CALCULATE ADDRESS OF BUFFER END       00034900
         BCTR  R4,R0                  LESS 1 FOR ZERO REFERENCE         00035000
         LR    R6,R4              COPY ADDRESS OF BUFFER END            00035100
         SH    R6,TXTSTRL         SUBTRACT STRING LEN FROM BUFFER ADR   00035200
         SH    R5,TXTSTRL         SUBTRACT STRING LEN FROM BUFFER LEN   00035300
         SH    R5,TXTPNTR         SUBTRACT POINTER FROM BUFFER LEN      00035400
         LA    R5,1(,R5)              PLUS 1 FOR ACTUAL LENGTH          00035500
         WHILE R5,(NZ,LTR),R5                                           00035600
          BCTR R5,R0              DECREMENT LOOP COUNT                  00035700
          MVC  0(1,R4),0(R6)      MOVE CHARACTER TO THE RIGHT           00035800
          BCTR R6,R0              DECREMENT FROM ADDRESS FOR MOVE       00035900
          BCTR R4,R0              DECREMENT  TO  ADDRESS FOR MOVE       00036000
         WEND                                                           00036100
         B     TXTCOPY            GO OVERLAY INSERT TEXT                00036200
         EJECT                                                          00036300
* ------------------------------------------------------------------- * 00036400
TXTPARS  EQU   *   PARSE THE BUFFER INTO THE STRING(S)                  00036500
* ------------------------------------------------------------------- * 00036600
         XC    TRTTABLE,TRTTABLE  CLEAR TRANSLATE TABLE                 00036700
         XR    R3,R3              CLEAR REGISTER                        00036800
         LA    R6,TXTDLMS         GET ADDRESS OF DELIMITERS             00036900
         LA    R7,8(,R0)          LOOP FOR EIGHT DELIMITERS             00037000
         WHILE R7,(NZ,LTR),R7                                           00037100
          BCTR R7,R0              DECREMENT LOOP COUNT                  00037200
          IC   R3,0(R6)           GET DELIMITER CHARACTER               00037300
          STC  R3,TRTTABLE(R3)    PUT IT IN TRANSLATE & TEST TABLE      00037400
          LA   R6,1(,R6)          POINT TO NEXT DELIMITER               00037500
         WEND                                                           00037600
*                                                                       00037700
         XR    R1,R1              CLEAR DELIMETER ADDR REGISTER         00037800
         XR    R2,R2              CLEAR DELIMETER CHAR REGISTER         00037900
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00038000
         AH    R4,TXTPNTR         ADD POINTER VALUE                     00038100
         BCTR  R4,R0              LESS 1 FOR ZERO REFERENCE             00038200
         LH    R5,TXTBUFL         GET BUFFER LENGTH                     00038300
         SH    R5,TXTPNTR         SUBTRACT POINTER VALUE                00038400
*                                                                       00038500
         IF    R5,(Z,EX),TXTTRT   IF NO DELIMITERS FOUND                00038600
          LR   R1,R4              COPY BUFFER ADDRESS                   00038700
          AR   R1,R5              ADD REMAINING LENGTH                  00038800
          MVI  TXTRETN,X'00'      SIGNAL END OF BUFFER                  00038900
         ELSE                                                           00039000
          IF   R2,(EQ,CLM,B'0001'),=X'40'                               00039100
           LA  R6,TXTBUFR         GET BUFFER ADDRESS                    00039200
           AH  R6,TXTBUFL         CALCULATE ADDRESS OF BUFFER END       00039300
           BCTR R6,R0                 LESS 1 FOR ZERO REFERENCE         00039400
           WHILE R1,(NE,CR),R6,AND,1(R1),EQ,X'40'                       00039500
            LA  R1,1(,R1)          POINT TO NEXT BYTE                   00039600
           WEND                                                         00039700
           IF  R1,(EQ,CR),R6                                            00039800
            MVI TXTRETN,X'00'     SIGNAL END OF BUFFER                  00039900
           ENDIF                                                        00040000
          ENDIF                                                         00040100
         ENDIF                                                          00040200
*                                                                       00040300
         LR    R3,R1              COPY DELIMITER ADDRESS                00040400
         SR    R3,R4              SUBTRACT STARTING POINT               00040500
         LA    R3,1(,R3)          PLUS 1 FOR ACTUAL LENGTH              00040600
         STH   R3,TXTSTRL         PUT IN STRING LENGTH FIELD            00040700
         B     TXTCOPY            GO COPY PARSE TEXT                    00040800
         EJECT                                                          00040900
* ------------------------------------------------------------------- * 00041000
RETURN   EQU   *                                                        00041100
* ------------------------------------------------------------------- * 00041200
         RETURN (14,12)           RETURN TO CALLER                      00041300
* =================================================================== * 00041400
         EJECT                                                          00041500
* =================================================================== * 00041600
STORAGE  DS    0D                                                       00041700
* =================================================================== * 00041800
         LTORG                                                          00041900
* =================================================================== * 00042000
         END                                                            00042100
