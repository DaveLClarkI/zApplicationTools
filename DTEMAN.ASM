DTE      TITLE 'DTEMAN   - DATE MANIPULATION FOR COBOL PROGRAMS'        00000100
         PRINT ON,NOGEN                                                 00000200
DTEMAN   AMODE 31
DTEMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*       D A T E   M A N I P U L A T I O N   S U B R O U T I N E       * 00000500
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
* means of converting various input  date formats into various output * 00001800
* date formats with simultaneous date validation.                     * 00001900
*                                                                     * 00002000
* =================================================================== * 00002100
*                                                                     * 00002200
*                           IMPLEMENTATION                            * 00002300
*                                                                     * 00002400
*      This implementation validates and  converts between any of the * 00002500
* following date formats  (delimiter  characters  in  input dates are * 00002600
* stripped out when dates are output, i.e.  input dates may be alpha- * 00002700
* numeric but output dates are always numeric):                       * 00002800
*                                                                     * 00002900
*      CURRENT    -  USES THE CPU TIME-OF-DAY CLOCK                   * 00002950
*      Gregorian  -  using YYYY/MM/DD, YY/MM/DD, YYYYMMDD or YYMMDD   * 00003000
*      Julian     -  using YYYY/DDD, YY/DDD, YYYYDDD or YYDDD         * 00003100
*      TotalDays  -  using ddddddd starting at Jan. 1, 0001           * 00003200
*                                                                     * 00003300
* Where: YYYY    may be any year from 0001 to 2400, inclusive         * 00003400
*                (Note that the leading zeroes are required for the   * 00003500
*                 years from 0001 through 0099, inclusive.)           * 00003600
*        YY      may be any year from 00 to 99 in the 20th century    * 00003700
*                (Note that in order for YY to represent a year in    * 00003800
*                 the 20th century, the date field must not have any  * 00003900
*                 leading digits that are not part of the YY value.)  * 00004000
*        MM      is a Gregorian month from 01 to 12                   * 00004100
*        DD      is a Gregorian day from 01 to 31, depending on month * 00004200
*        DDD     is a Julian day from 001 to 366, depending on year   * 00004300
*        DDDDDDD is a number from 1  to 876,582 and is representative * 00004400
*                of the total number of days from Jan. 1, 0001 to the * 00004500
*                selected Gregorian  or Julian date,  inclusive,  and * 00004600
*                allows for "leap"  years  every  4th and 400th years * 00004700
*                (there is no "leap" year every 100th year).          * 00004800
*                                                                     * 00004900
* The invoking COBOL program should  use  the following data area for * 00005000
* passing parameters:                                                 * 00005100
*                                                                     * 00005200
*      01  DTEMAN-PARMS.                                              * 00005300
*          05  DTE-REQU                PIC  X(01)   VALUE "G".        * 00005400
*              88  DTE-REQUEST-CURRENT-DATE         VALUE "C".        * 00005450
*              88  DTE-REQUEST-GREG-EDIT            VALUE "G".        * 00005500
*              88  DTE-REQUEST-JULN-EDIT            VALUE "J".        * 00005600
*              88  DTE-REQUEST-TOTL-DAYS            VALUE "T".        * 00005700
*          05  DTE-RETN                PIC  X(01)   VALUE SPACES.     * 00005800
*              88  DTE-REQUEST-COMPLETED            VALUE " ".        * 00005900
*              88  DTE-INVALID-CLOCK                VALUE "C".        * 00005950
*              88  DTE-INVALID-REQU                 VALUE "E".        * 00006000
*              88  DTE-INVALID-GREG                 VALUE "G".        * 00006100
*              88  DTE-INVALID-JULN                 VALUE "J".        * 00006200
*              88  DTE-INVALID-TOTD                 VALUE "T".        * 00006300
*              88  DTE-INVALID-YEAR                 VALUE "Y".        * 00006400
*              88  DTE-INVALID-MONTH                VALUE "M".        * 00006500
*              88  DTE-INVALID-DAY                  VALUE "D".        * 00006600
*     *                                                               * 00006700
*          05  DTE-GREG                             VALUE SPACES.     * 00006800
*              10  FILLER              PIC  XX.                       * 00006900
*              10  DTE-GRYR            PIC  9999.                     * 00007000
*              10  DTE-GRMO            PIC  99.                       * 00007100
*              10  DTE-GRDA            PIC  99.                       * 00007200
*          05  DTE-GNUM    REDEFINES   DTE-GREG     PIC  9(10).       * 00007300
*          05  FILLER      REDEFINES   DTE-GREG.                      * 00007400
*              10  FILLER              PIC  XXXX.                     * 00007500
*              10  DTE-YYMMDD          PIC  9(06).                    * 00007600
*     *                                                               * 00007700
*          05  DTE-JULN                             VALUE SPACES.     * 00007800
*              10  FILLER              PIC  X.                        * 00007900
*              10  DTE-JUYR            PIC  9999.                     * 00008000
*              10  DTE-JUDA            PIC  999.                      * 00008100
*          05  DTE-JNUM    REDEFINES   DTE-JULN     PIC  9(08).       * 00008200
*          05  FILLER      REDEFINES   DTE-JULN.                      * 00008300
*              10  FILLER              PIC  XXX.                      * 00008400
*              10  DTE-YYDDD           PIC  9(05).                    * 00008500
*     *                                                               * 00008600
*          05  DTE-TOTD                PIC  9(07)   COMP-3 VALUE ZERO.* 00008700
*          05  DTE-MNTH                PIC  9(03)   COMP-3 VALUE ZERO.* 00008800
*              88  DTE-JANUARY                      VALUE 1.          * 00008900
*              88  DTE-FEBRUARY                     VALUE 2.          * 00009000
*              88  DTE-MARCH                        VALUE 3.          * 00009100
*              88  DTE-APRIL                        VALUE 4.          * 00009200
*              88  DTE-MAY                          VALUE 5.          * 00009300
*              88  DTE-JUNE                         VALUE 6.          * 00009400
*              88  DTE-JULY                         VALUE 7.          * 00009500
*              88  DTE-AUGUST                       VALUE 8.          * 00009600
*              88  DTE-SEPTEMBER                    VALUE 9.          * 00009700
*              88  DTE-OCTOBER                      VALUE 10.         * 00009800
*              88  DTE-NOVEMBER                     VALUE 11.         * 00009900
*              88  DTE-DECEMBER                     VALUE 12.         * 00010000
*          05  DTE-DAYN                PIC  9(01)   COMP-3 VALUE ZERO.* 00010100
*              88  DTE-SUNDAY                       VALUE 1.          * 00010200
*              88  DTE-MONDAY                       VALUE 2.          * 00010300
*              88  DTE-TUESDAY                      VALUE 3.          * 00010400
*              88  DTE-WEDNESDAY                    VALUE 4.          * 00010500
*              88  DTE-THURSDAY                     VALUE 5.          * 00010600
*              88  DTE-FRIDAY                       VALUE 6.          * 00010700
*              88  DTE-SATURDAY                     VALUE 7.          * 00010800
*          05  DTE-LEAP                PIC  X(01)   VALUE SPACES.     * 00010900
*              88  DTE-LEAP-YEAR                    VALUE "L".        * 00011000
*              88  DTE-NONLEAP-YEAR                 VALUE "N".        * 00011100
*          05  FILLER                  PIC  X(04)   VALUE LOW-VALUES. * 00011200
*                                                                     * 00011300
* =================================================================== * 00011400
*                                                                     * 00011500
*                             INVOKATION                              * 00011600
*                                                                     * 00011700
*      The subroutine is invoked in any of three ways (depending upon * 00011800
* the function desired).   The function needed is directly related to * 00011900
* the format of the input  date  you are supplying,  as follows (note * 00012000
* that in all cases all dates MUST have the year on the left):        * 00012100
*                                                                     * 00012200
*      "C"URRENT   - GENERATES THE INPUT DATE FROM THE CPU TOD CLOCK  * 00012250
*      "G"REGORIAN - input date is in DTE-GREG or DTE-GNUM            * 00012300
*      "J"ULIAN    - input date is in DTE-JULN or DTE-JNUM            * 00012400
*      "T"OTALDAYS - input date is in DTE-TOTD                        * 00012500
*                                                                     * 00012600
*      The example below demonstrates how  to first validate and then * 00012700
* convert a numeric input  "J"ULIAN  date  to the various output date * 00012800
* formats.                                                            * 00012900
*                                                                     * 00013000
*     * VALIDATE AND CONVERT AN INPUT JULIAN DATE                     * 00013100
*          MOVE     'J'      TO DTE-REQU                              * 00013200
*          MOVE   EIBDATE    TO DTE-JNUM                              * 00013300
*          ADD    1900000    TO DTE-JNUM                              * 00013400
*          CALL   'DTEMAN'   USING DTEMAN-PARMS                       * 00013500
*          IF  NOT DTE-REQUEST-COMPLETED                              * 00013600
*     *        your-error-handling-code                               * 00013700
*          ELSE                                                       * 00013800
*     *        your-date-evaluation-code                              * 00013900
*          END-IF                                                     * 00014000
*                                                                     * 00014100
*      On return,  if no errors,  you select the output numeric field * 00014200
* definition based on whether you  need  the full year ("DTE-GNUM" or * 00014300
* "DTE-JNUM") or a two digit year ("DTE-YYMMDD" or "DTE-YYDDD").   In * 00014400
* addition,  a separate month number field ("DTE-MNTH") and a day-of- * 00014500
* the-week number field  ("DTE-DAYN")  provide  easy  testing for the * 00014600
* month or  day  of  the  week  which  corresponds  to  the date last * 00014700
* returned by  the  subroutine.   Lastly,  a  convenient indicator is * 00014800
* given ("DTE-LEAP")  to  inform  you  as  to  whether  the year just * 00014900
* manipulated is a leap-year or not.                                  * 00015000
*                                                                     * 00015100
*      The most common use for  this  capability is to take keyboard- * 00015200
* entered date string data and  validate/convert  it for storage in a * 00015300
* numeric date field of a  file record.   In addition,  a date may be * 00015400
* adjusted forward or backward,  using the "T"OTAL DAYS function,  to * 00015500
* obtain an appropriate date in  another month or year without having * 00015600
* to allow for varying lengths of months or for leap years.           * 00015700
*                                                                     * 00015800
*      Errors may be detected  after  changes  have  been made to the * 00015900
* passed date string  but  before  all  changes  have been completed. * 00016000
* Hence,  for your information, the following outlines the logic used * 00016100
* and the points at which errors may be reported:                     * 00016200
*                                                                     * 00016300
*      1) Validate request parameter and report any error;            * 00016400
*      2) If "T"OTDEDIT,                                              * 00016500
*         a. If DTE-TOTD < 1 or > 292194 then report error,           * 00016600
*         b. generate DTE-JNUM and DTE-GNUM;                          * 00016700
*      3) If "J"ULNEDIT,                                              * 00016800
*         a. Right justify any numeric digits,                        * 00016900
*         b. If DTE-JULN < 5 digits long then report error,           * 00017000
*         c. Left zero any unused character positions,                * 00017100
*         d. Validate year and day reporting any error,               * 00017200
*         e. generate DTE-GNUM and DTE-TOTD;                          * 00017300
*      4) If "G"REGEDIT,                                              * 00017400
*         a. Right justify any numeric digits,                        * 00017500
*         b. If DTE-GREG < 6 digits long then report error,           * 00017600
*         c. Left zero any unused character positions,                * 00017700
*         d. Validate year, month and day reporting any error,        * 00017800
*         e. generate DTE-TOTD and DTE-JNUM;                          * 00017900
*      5) generate DTE-MNTH, DTE-DAYN and DTE-LEAP.                   * 00018000
*                                                                     * 00018100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00018200
         EJECT                                                          00018300
* =================================================================== * 00018400
DTEPARM  DSECT                    DTEMAN PARMS DUMMY SECTION            00018500
         USING *,R10              ESTABLISH REG 10 AS BASE              00018600
* =================================================================== * 00018700
DTEREQU  DS    CL1                REQUEST CODE                          00018800
DTERETN  DS    CL1                RETURN CODE                           00018900
DTEGREG  DS    0CL10              GREGORIAN DATE                        00019000
DTEGRFL  DS    ZL2                     FILLER                           00019100
DTEGRYR  DS    ZL4                     YEAR                             00019200
DTEGRMO  DS    ZL2                     MONTH                            00019300
DTEGRDA  DS    ZL2                     DAY                              00019400
DTEJULN  DS    0CL8               JULIAN DATE                           00019500
DTEJUFL  DS    ZL1                     FILLER                           00019600
DTEJUYR  DS    ZL4                     YEAR                             00019700
DTEJUDA  DS    ZL3                     DAY                              00019800
DTETOTD  DS    PL4                TOTAL DAYS                            00019900
PWORK    DS    0PL8               (INTERNAL USE ONLY)                   00020000
DTEMNTH  DS    PL2                MONTH NUMBER                          00020100
DTEDAYN  DS    PL1                DAY NUMBER                            00020200
LEAPYEAR DS    CL1                LEAP YEAR INDICATOR                   00020300
PWORK4   DS    PL4                 SIGNIFICANT RIGHT 4 BYTES            00020400
* =================================================================== * 00020500
         EJECT                                                          00020510
* =================================================================== * 00020520
COMREG   MAPCOMR EQU=YES          PARTITION COMMUNICATION REGION        00020530
         USING COMREG,R1          ESTABLISH REG 1 AS BASE               00020540
* =================================================================== * 00020550
         EJECT                                                          00020600
* =================================================================== * 00020700
*        REGISTER USAGES                                                00020800
* =================================================================== * 00020900
R0       EQU   0                  PARM REG                              00021000
R1       EQU   1                  PARM REG                              00021100
R2       EQU   2                  WORK REG (LOOP/INDEX REGISTER)        00021200
R3       EQU   3                  WORK REG (PNTR/INDEX REGISTER)        00021300
R4       EQU   4                  WORK REG (DAY VALUE)                  00021400
R5       EQU   5                  WORK REG (DIVIDE QUOTIENT)            00021500
R6       EQU   6                  WORK REG (MONTH VALUE)                00021600
R7       EQU   7                  WORK REG (MONTH INDEX)                00021700
R8       EQU   8                  WORK REG (YEAR VALUE)                 00021800
R9       EQU   9                  WORK REG (TOTAL DAYS VALUE)           00021900
R10      EQU   10                 DTEPARM DSECT BASE REG                00022000
R11      EQU   11                 LINK REGISTER                         00022100
R12      EQU   12                 DTEMAN CSECT BASE REG                 00022200
R13      EQU   13                 REGISTER SAVEAREA                     00022300
R14      EQU   14                 RETURN REG                            00022400
R15      EQU   15                 ENTRY REG                             00022500
* =================================================================== * 00022600
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
         COPY  WHILE
* =================================================================== * 00012800
         EJECT                                                          00022700
* =================================================================== * 00022800
DTEMAN   CSECT                                                          00022900
         USING DTEMAN,R15         ESTABLISH ENTRY-POINT REG AS BASE     00023000
* =================================================================== * 00023100
         B     START              BRANCH AROUND CORE EYE-CATCHER        00023200
*                                                                       00023300
         DC    CL8'DTEMAN'        PROGRAM                               00023400
         DC    CL4'2.0'           VERSION                               00023500
         DC    CL4'DLC'           AUTHOR                                00023600
         DC    CL8'(C)SEP90'      DATE                                  00023700
*                                                                       00023800
START    DS    0H                                                       00023900
         SAVE  (14,12)            SAVE CALLER'S REGISTERS               00024000
         LR    R12,R15            INITIALIZE ROUTINE BASE REGISTER      00024100
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00024200
         USING DTEMAN,R12         ESTABLISH PERM BASE REGISTER          00024300
*                                                                       00024400
         L     R10,0(,R1)         GET ADDRESS OF PARAMETERS             00024500
         MVI   DTERETN,C' '       INITIALIZE RETURN CODE                00024600
*                                                                       00024700
         IF    DTEREQU,EQ,C'C',BC,CURRDATE     GREG=>TOTD=>JULN CONVERT 00024720
         ENDIF                                                          00024750
         IF    DTEREQU,EQ,C'G',BC,GREGEDIT     GREG=>TOTD=>JULN CONVERT 00024800
         ENDIF                                                          00024900
         IF    DTEREQU,EQ,C'J',BC,JULNEDIT     JULN=>GREG=>TOTD CONVERT 00025000
         ENDIF                                                          00025100
         IF    DTEREQU,EQ,C'T',BC,TOTDEDIT     TOTD=>JULN=>GREG CONVERT 00025200
         ENDIF                                                          00025300
*                                                                       00025400
         MVI   DTERETN,C'E'       DATE REQUEST IS IN ERROR              00025500
         B     RETURN             GO RETURN TO CALLER                   00025600
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00025700
* ------------------------------------------------------------------- * 00025800
TOTDEDIT EQU   *   EDIT TOTAL DAYS FOR VALID RANGES                     00025900
* ------------------------------------------------------------------- * 00026000
         ZAP   PWORK,DTETOTD      PUT TOTAL DAYS IN WORK FIELD          00026100
         CVB   R9,PWORK           CONVERT PACKED DECIMAL TO BINARY      00026200
*                                                                       00026300
         IF    R9,(PLUS,LTR),R9,AND,R9,(NHIGHER,C),=F'876582'           00026400
          B    TOTLDAYS                                                 00026500
         ENDIF                                                          00026600
*                                                                       00026700
         MVI   DTERETN,C'T'       SIGNAL TOTAL DAYS ERROR               00026800
         B     RETURN             GO RETURN TO CALLER                   00026900
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00027000
* ------------------------------------------------------------------- * 00027100
TOTLDAYS EQU   *   CONVERT TOTAL DAYS TO A JULIAN DATE                  00027200
* ------------------------------------------------------------------- * 00027300
         MVC   DTEJULN,=C'0000000000' CLEAR JULIAN DATE FIELD           00027400
         ZAP   PWORK,DTETOTD      PUT TOTAL DAYS IN WORK FIELD          00027500
         CVB   R9,PWORK           CONVERT PACKED DECIMAL TO BINARY      00027600
*                                                                       00027700
         LR    R4,R9              COPY TOTAL DAYS TO REG 4              00027800
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00027900
         D     R4,=F'146097'      DIV. BY DAYS IN 400 YEARS             00028000
         SR    R9,R5              SUB. QUOTIENT FROM TOTAL DAYS         00028100
         LR    R4,R9              COPY TOTAL DAYS TO REG 4              00028200
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00028300
         D     R4,=F'36524'       DIV. BY DAYS IN 100 YEARS             00028400
         AR    R9,R5              ADD QUOTIENT TO TOTAL DAYS            00028500
         LR    R4,R9              COPY TOTAL DAYS TO REG 4              00028600
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00028700
         D     R4,=F'1461'        DIV. BY DAYS IN 4 YEARS               00028800
         SR    R9,R5              SUB. QUOTIENT FROM TOTAL DAYS         00028900
         LR    R4,R9              COPY TOTAL DAYS TO REG 4              00029000
         BCTR  R4,0               DECREMENT FOR ZERO REFERENCE
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00029100
         D     R4,=F'365'         DIVIDE BY DAYS IN 1 YEAR              00029200
         LR    R8,R5              SAVE YEAR IN REG 8
*
         ZAP   PWORK,DTETOTD      PUT TOTAL DAYS IN WORK FIELD          00027500
         CVB   R9,PWORK           CONVERT PACKED DECIMAL TO BINARY      00027600
         LR    R5,R8              COPY YEARS TO WORK REG 5              00049200
         MH    R5,=H'365'         MULT. BY DAYS IN NON-LEAP YEAR        00049300
         SR    R9,R5              DECREMENT TOTAL DAYS ACCOUNTED FOR    00049400
         LR    R4,R8              COPY YEARS TO WORK REG 4              00049500
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00049600
         D     R4,=F'4'           DIV. BY YEARS BETWEEN LEAPS           00049700
         SR    R9,R5              DECREMENT TOTAL DAYS ACCOUNTED FOR    00049800
         LR    R4,R8              COPY YEARS TO WORK REG 4              00049900
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00050000
         D     R4,=F'100'         DIV. BY YEARS TILL 1ST EXCEPTION      00050100
         AR    R9,R5              ACCUMULATE TOTAL DAYS ACCOUNTED FOR   00050200
         LR    R4,R8              COPY YEARS TO WORK REG 4              00050300
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00050400
         D     R4,=F'400'         DIV. BY YEARS TILL 2ND EXCEPTION      00050500
         SR    R9,R5              DECREMENT TOTAL DAYS ACCOUNTED FOR    00050600
*                                                                       00029400
         LA    R8,1(,R8)          PLUS THE BASE YEAR OF JAN. 1, 0001    00029300
         CVD   R8,PWORK           CONVERT YEAR TO DECIMAL               00029500
         UNPK  DTEJUYR,PWORK4     UNPACK INTO JULIAN YEAR               00029600
         OI    DTEJUYR+3,X'F0'    REMOVE ANY SIGN                       00029700
*                                                                       00029800
         CVD   R9,PWORK           CONVERT DAYS TO DECIMAL               00029900
         UNPK  DTEJUDA,PWORK4     UNPACK INTO JULIAN DAYS               00030000
         OI    DTEJUDA+2,X'F0'    REMOVE ANY SIGN                       00030100
*                                                                       00030200
         IF    DTEREQU,EQ,C'G',BC,EXITDATE                              00030300
         ENDIF                                                          00030400
         IF    DTEREQU,EQ,C'C',BC,EXITDATE                              00030420
         ENDIF                                                          00030450
*                                                                       00030500
         B     JULNDATE           GO PROCESS JULIAN DATE                00030600
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00030700
* ------------------------------------------------------------------- * 00030800
JULNEDIT EQU   *   EDIT JULIAN DATE FOR VALID RANGES                    00030900
* ------------------------------------------------------------------- * 00031000
         LA    R2,L'DTEJULN-1     SET LOOP/INDEX REGISTER               00031100
         LR    R3,R2              SET PNTR/INDEX REGISTER               00031200
*                                                                       00031300
         WHILE R2,(NMINUS,LTR),R2                                       00031400
          IC   R1,DTEJULN(R2)     GET CHARACTER FOR TESTING             00031500
          BCTR R2,0               DECREMENT LOOP/INDEX FOR NEXT CHAR    00031600
          IF   R1,(NL,CLM,B'0001'),=C'0',AND,R1,(NH,CLM,B'0001'),=C'9'  00031700
           STC R1,DTEJULN(R3)     RIGHT JUSTIFY NUMERIC CHARACTERS      00031800
           BCTR R3,0              DECREMENT PNTR/INDEX FOR NEXT CHAR    00031900
          ENDIF                                                         00032000
         WEND                                                           00032100
*                                                                       00032200
         IF    R3,(H,CH),=Y(L'DTEJULN-1-5)   IF < 5 DIGITS LONG         00032300
          MVI  DTERETN,C'J'       SIGNAL JULIAN DATE ERROR              00032400
          B    RETURN             GO RETURN TO CALLER                   00032500
         ENDIF                                                          00032600
         IF    R3,(EQ,CH),=Y(L'DTEJULN-1-5)   IF = 5 DIGITS LONG        00032700
          LA   R2,2000            ONLY YY SPECIFIED, DEFAULT            00032800
         ELSE                                                           00032900
          XR   R2,R2              AT LEAST YYY SPECIFIED, NO DEFAULT    00033000
         ENDIF                                                          00033100
*                                                                       00033200
         IC    R1,=C'0'           GET CHARACTER FOR LEFT-ZERO           00033300
         WHILE R3,(NMINUS,LTR),R3 REPEAT WHILE REG 3 IS NOT MINUS       00033400
          STC  R1,DTEJULN(R3)     LEFT-ZERO THE RIGHT-JUSTIFIED FIELD   00033500
          BCTR R3,0               DECREMENT PNTR/INDEX FOR NEXT CHAR    00033600
         WEND                                                           00033700
*                                                                       00033800
         PACK  PWORK,DTEJUYR      CONVERT ZONED DECIMAL TO PACKED       00033900
         CVB   R8,PWORK           CONVERT PACKED DECIMAL TO BINARY      00034000
         IF    R2,(PLUS,LTR),R2                                         00034100
          AR   R8,R2              ASSUME ABOVE SELECTED DEFAULT         00034200
          CVD  R8,PWORK           CONVERT BINARY TO PACKED DECIMAL      00034300
          UNPK DTEJUYR,PWORK4     CONVERT PACKED DECIMAL TO ZONED       00034400
          OI   DTEJUYR+3,X'F0'    CHANGE X'C?' TO X'F?' FOR SIGN        00034500
         ENDIF                                                          00034600
*                                                                       00034700
         IF    R8,(NPLUS,LTR),R8,OR,R8,(HIGHER,CH),=H'2400'             00034800
          MVI  DTERETN,C'Y'       SIGNAL JULIAN YEAR ERROR              00034900
          B    RETURN             GO RETURN TO CALLER                   00035000
         ENDIF                                                          00035100
*                                                                       00035200
         PACK  PWORK,DTEJUDA      CONVERT ZONED DECIMAL TO PACKED       00035300
         CVB   R4,PWORK           CONVERT PACKED DECIMAL TO BINARY      00035400
         LR    R1,R8              COPY YEAR TO PARM REG                 00035500
         BAS   R11,LEAPTEST       GO TEST YEAR FOR LEAP-YEAR            00035600
*                                                                       00035700
         IF    LEAPYEAR,EQ,C'L'   IF LEAPYEAR FLAG IS A 'L'             00035800
          IF   R4,(PLUS,LTR),R4,AND,R4,(NHIGHER,CH),=H'366'             00035900
           B   JULNDATE           GO PROCESS JULIAN DATE                00036000
          ENDIF                                                         00036100
         ELSE                                                           00036200
          IF   R4,(PLUS,LTR),R4,AND,R4,(NHIGHER,CH),=H'365'             00036300
           B   JULNDATE           GO PROCESS JULIAN DATE                00036400
          ENDIF                                                         00036500
         ENDIF                                                          00036600
*                                                                       00036700
         MVI   DTERETN,C'D'       SIGNAL JULIAN DAY ERROR               00036800
         B     RETURN             GO RETURN TO CALLER                   00036900
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00037000
* ------------------------------------------------------------------- * 00037100
JULNDATE EQU   *   CONVERT JULIAN DATE TO A GREGORIAN DATE              00037200
* ------------------------------------------------------------------- * 00037300
         MVC   DTEGREG,=C'0000000000' CLEAR GREGORIAN DATA FIELD        00037400
         MVC   DTEGRYR,DTEJUYR    MOVE JULIAN YEAR TO GREG. YEAR        00037500
         PACK  PWORK,DTEJUYR      PACK JULIAN YEAR INTO WORK FIELD      00037600
         CVB   R8,PWORK           CONVERT PACKED DECIMAL TO BINARY      00037700
*                                                                       00037800
         PACK  PWORK,DTEJUDA      PACK DAYS INTO WORK FIELD             00037900
         CVB   R4,PWORK           CONVERT PACKED DECIMAL TO BINARY      00038000
*                                                                       00038100
         LR    R1,R8              COPY YEAR TO PARM REG                 00038200
         BAS   R11,LEAPTEST       GO TEST YEAR FOR LEAP-YEAR            00038300
*                                                                       00038400
         LA    R6,1               SET MONTH NUMBER TO 1                 00038500
         XR    R7,R7              ZERO MONTH INDEX REG 7                00038600
         WHILE R4,(HIGHER,CH),MTHTBL(R7) REPEAT WHILE R4 > DAYS IN MTH  00038700
          IF   LEAPYEAR,EQ,C'L',AND,R6,(EQ,CH),=H'2'                    00038800
           IF  R4,(EQ,CH),=H'29'  IF REG 4 EQUALS 29                    00038900
            WEXIT                  END LOOP                             00039000
           ENDIF                                                        00039100
           BCTR R4,0              DECREMENT FOR LEAP-MONTH              00039200
          ENDIF                                                         00039300
          SH   R4,MTHTBL(R7)      SUBTRACT DAYS IN MONTH                00039400
          LA   R6,1(,R6)          INCREMENT MONTH NUMBER                00039500
          LA   R7,2(,R7)          INCREMENT MONTH INDEX                 00039600
         WEND                                                           00039700
*                                                                       00039800
         CVD   R6,PWORK           CONVERT MONTH # TO PACKED             00039900
         UNPK  DTEGRMO,PWORK4     UNPACK IT INTO GREG. MONTH            00040000
         OI    DTEGRMO+1,X'F0'    REMOVE ANY SIGN                       00040100
*                                                                       00040200
         CVD   R4,PWORK           CONVERT DAY # TO PACKED               00040300
         UNPK  DTEGRDA,PWORK4     UNPACK IT INTO GREG. DAY              00040400
         OI    DTEGRDA+1,X'F0'    REMOVE ANY SIGN                       00040500
*                                                                       00040600
         IF    DTEREQU,EQ,C'T',BC,EXITDATE                              00040700
         ENDIF                                                          00040800
*                                                                       00040900
         B     GREGDATE           GO PROCESS GREGORIAN DATE             00041000
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00041100
* ------------------------------------------------------------------- * 00041200
GREGEDIT EQU   *   EDIT GREGORIAN DATE FOR VALID RANGES                 00041300
* ------------------------------------------------------------------- * 00041400
         LA    R2,L'DTEGREG-1     SET LOOP/INDEX REGISTER               00041500
         LA    R3,L'DTEGREG-1     SET PNTR/INDEX REGISTER               00041600
*                                                                       00041700
         WHILE R2,(NMINUS,LTR),R2                                       00041800
          IC   R1,DTEGREG(R2)     GET CHARACTER FOR TESTING             00041900
          BCTR R2,0               DECREMENT LOOP/INDEX FOR NEXT CHAR    00042000
          IF   R1,(NL,CLM,B'0001'),=C'0',AND,R1,(NH,CLM,B'0001'),=C'9'  00042100
           STC R1,DTEGREG(R3)     RIGHT JUSTIFY NUMERIC CHARACTERS      00042200
           BCTR R3,0              DECREMENT PNTR/INDEX FOR NEXT CHAR    00042300
          ENDIF                                                         00042400
         WEND                                                           00042500
*                                                                       00042600
         IF    R3,(H,CH),=Y(L'DTEGREG-1-6)   IF < 6 DIGITS LONG         00042700
          MVI  DTERETN,C'G'       SIGNAL GREGORIAN DATE ERROR           00042800
          B    RETURN             GO RETURN TO CALLER                   00042900
         ENDIF                                                          00043000
         IF    R3,(EQ,CH),=Y(L'DTEGREG-1-6)   IF = 6 DIGITS LONG        00043100
          LA   R2,2000            ONLY YY SPECIFIED, DEFAULT            00043200
         ELSE                                                           00043300
          XR   R2,R2              AT LEAST YYY SPECIFIED, NO DEFAULT    00043400
         ENDIF                                                          00043500
*                                                                       00043600
         IC    R1,=C'0'           GET CHARACTER FOR LEFT-ZERO           00043700
         WHILE R3,(NMINUS,LTR),R3 REPEAT WHILE REG 3 IS NOT MINUS       00043800
          STC  R1,DTEGREG(R3)     LEFT-ZERO THE RIGHT-JUSTIFIED FIELD   00043900
          BCTR R3,0               DECREMENT PNTR/INDEX FOR NEXT CHAR    00044000
         WEND                                                           00044100
*                                                                       00044200
         PACK  PWORK,DTEGRYR      CONVERT ZONED DECIMAL TO PACKED       00044300
         CVB   R8,PWORK           CONVERT PACKED DECIMAL TO BINARY      00044400
         IF    R2,(PLUS,LTR),R2                                         00044500
          AR   R8,R2              ASSUME ABOVE SELECTED DEFAULT         00044600
          CVD  R8,PWORK           CONVERT BINARY TO PACKED DECIMAL      00044700
          UNPK DTEGRYR,PWORK4     CONVERT PACKED DECIMAL TO ZONED       00044800
          OI   DTEGRYR+3,X'F0'    CHANGE X'C?' TO X'F?' FOR SIGN        00044900
         ENDIF                                                          00045000
*                                                                       00045100
         IF    R8,(NPLUS,LTR),R8,OR,R8,(HIGHER,CH),=H'2400'             00045200
          MVI  DTERETN,C'Y'       SIGNAL GREGORIAN YEAR ERROR           00045300
          B    RETURN             GO RETURN TO CALLER                   00045400
         ENDIF                                                          00045500
*                                                                       00045600
         PACK  PWORK,DTEGRMO      CONVERT ZONED DECIMAL TO PACKED       00045700
         CVB   R6,PWORK           CONVERT PACKED DECIMAL TO BINARY      00045800
*                                                                       00045900
         IF    R6,(NPLUS,LTR),R6,OR,R6,(HIGHER,CH),=H'12'               00046000
          MVI  DTERETN,C'M'       SIGNAL GREGORIAN MONTH ERROR          00046100
          B    RETURN             GO RETURN TO CALLER                   00046200
         ENDIF                                                          00046300
*                                                                       00046400
         PACK  PWORK,DTEGRDA      CONVERT ZONED DECIMAL TO PACKED       00046500
         CVB   R4,PWORK           CONVERT PACKED DECIMAL TO BINARY      00046600
*                                                                       00046700
         LR    R1,R8              COPY YEAR TO PARM REG                 00046800
         BAS   R11,LEAPTEST       GO TEST YEAR FOR LEAP-YEAR            00046900
*                                                                       00047000
         IF    LEAPYEAR,EQ,C'L',AND,R6,(EQ,CH),=H'2'                    00047100
          IF   R4,(EQ,CH),=H'29',BC,GREGDATE                            00047200
          ENDIF                                                         00047300
         ENDIF                                                          00047400
         BCTR  R6,0               MAKE MONTH NUMBER 0 TO 11 AND         00047500
         SLA   R6,1               MULTIPLY BY 2 TO CREATE INDEX         00047600
         IF    R4,(PLUS,LTR),R4,AND,R4,(NHIGHER,CH),MTHTBL(R6)          00047700
          B    GREGDATE           GO PROCESS GREGORIAN DATE             00047800
         ENDIF                                                          00047900
*                                                                       00048000
         MVI   DTERETN,C'D'       SIGNAL GREGORIAN DAY ERROR            00048100
         B     RETURN             GO RETURN TO CALLER                   00048200
* ------------------------------------------------------------------- * 00048208
         EJECT                                                          00048202
* ------------------------------------------------------------------- * 00048204
CURRDATE EQU   *   GENERATE GREGORIAN DATE FROM TIME-OF-DAY CLOCK       00048206
* ------------------------------------------------------------------- * 00048208
         MVI   DTERETN,C'C'       SIGNAL TOD CLOCK ERROR                00048210
         STCK  PWORK              STORE TOD CLOCK                       00048212
         BC    7,RETURN           IF TOD CLOCK NOT 'SET STATE'--ERROR   00048214
         MVI   DTERETN,C' '       SIGNAL TOD CLOCK IS OK                00048216
*                                                                       00048218
         COMRG REG=(R1)           GET ADDRESS OF PARTITION COMREG       00048220
         LH    R5,JOBZON          LOAD ZONE VALUE IN MINUTES            00048222
         M     R4,=F'60000000'    MICRO-SECONDS IN A MINUTE             00048224
         SLDL  R4,12              ALIGN WITH TOD CLOCK VALUE            00048226
         LM    R0,R1,PWORK        LOAD AS INPUT PARM TO "GETIME"        00048228
         AR    R0,R4              ADJUST TOD CLOCK FOR "ZONE"           00048230
         ST    R0,PWORK           SAVE TOD CLOCK ADJUSTMENT             00048232
         GETIME TU,CLOCK=YES      CONVERT TO:                           00048234
*                                  TIME IN REG 1 (1/300TH SECONDS)      00048236
*                                  DATE IN REGS 14 AND 15               00048238
         SRL   R15,16             SHIFT C'YY00' VALUE TO C'00YY' VALUE  00048240
         L     R5,=F'52594560'    MINUTES IN 100 YEARS (+LEAPS)         00048242
         M     R4,=F'60000000'    MICRO-SECONDS IN A MINUTE             00048244
         SLDL  R4,12              ALIGN WITH TOD CLOCK VALUE            00048246
         IF    R4,(HT,C),PWORK    IF CLOCK HAS NOT REACHED 100 YEARS    00048248
          ICM  R15,B'1100',=C'19'  FORCE CENTURY TO 20TH                00048250
         ELSE                     ELSE                                  00048252
          ICM  R15,B'1100',=C'20'  FORCE CENTURY TO 21ST                00048254
         ENDIF                    ENDIF                                 00048256
*                                                                       00048258
         MVC   DTEGREG,=C'0000000000' CLEAR GREGORIAN DATA FIELD        00048260
         ST    R15,DTEGRYR        STORE YYYY PORTION OF DATE            00048262
         ST    R14,DTEGRMO        STORE MMDD PORTION OF DATE            00048264
*                                                                       00048266
         B     GREGDATE           GO PROCESS GREGORIAN DATE             00048268
* ------------------------------------------------------------------- * 00048208
         EJECT                                                          00048300
* ------------------------------------------------------------------- * 00048400
GREGDATE EQU   *   CONVERT GREGORIAN DATE TO TOTAL DAYS                 00048500
* ------------------------------------------------------------------- * 00048600
         PACK  PWORK,DTEGRYR      PACK GREGORIAN YEAR INTO WORK FIELD   00048700
         CVB   R8,PWORK           CONVERT PACKED DECIMAL TO BINARY      00048800
         LR    R1,R8              COPY YEAR TO PARM REG FOR LEAP TEST   00048900
         BCTR  R8,0               LESS THE BASE YEAR OF JAN. 1, 0001    00049000
*                                                                       00049100
         LR    R5,R8              COPY YEARS TO WORK REG 5              00049200
         MH    R5,=H'365'         MULT. BY DAYS IN NON-LEAP YEAR        00049300
         LR    R9,R5              ACCUMULATE TOTAL DAYS ACCOUNTED FOR   00049400
         LR    R4,R8              COPY YEARS TO WORK REG 4              00049500
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00049600
         D     R4,=F'4'           DIV. BY YEARS BETWEEN LEAPS           00049700
         AR    R9,R5              ACCUMULATE TOTAL DAYS ACCOUNTED FOR   00049800
         LR    R4,R8              COPY YEARS TO WORK REG 4              00049900
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00050000
         D     R4,=F'100'         DIV. BY YEARS TILL 1ST EXCEPTION      00050100
         SR    R9,R5              DECREMENT TOTAL DAYS ACCOUNTED FOR    00050200
         LR    R4,R8              COPY YEARS TO WORK REG 4              00050300
         SRDA  R4,32              CONVERT TO 64-BIT NUMBER              00050400
         D     R4,=F'400'         DIV. BY YEARS TILL 2ND EXCEPTION      00050500
         AR    R9,R5              ACCUMULATE TOTAL DAYS ACCOUNTED FOR   00050600
*                                                                       00050700
         PACK  PWORK,DTEGRMO      PACK GREGORIAN MONTH INTO WORK FIELD  00050800
         CVB   R6,PWORK           CONVERT PACKED DECIMAL TO BINARY      00050900
         BAS   R11,LEAPTEST       GO TEST YEAR FOR LEAP-YEAR            00051000
*                                                                       00051100
         XR    R4,R4              ZERO MONTH INDEX REG 4                00051200
         LA    R5,1               SET MONTH NUMBER TO 1                 00051300
         WHILE R5,(NE,CR),R6      REPEAT WHILE REG 5 NOT EQUAL REG 6    00051400
          AH   R9,MTHTBL(R4)      ADD DAYS IN MONTH TO PREV. RESULT     00051500
          IF   LEAPYEAR,EQ,C'L',AND,R5,(EQ,CH),=H'2'                    00051600
           LA  R9,1(,R9)          INCREMENT FOR LEAP-MONTH              00051700
          ENDIF                                                         00051800
          LA   R4,2(,R4)          INCREMENT MONTH INDEX                 00051900
          LA   R5,1(,R5)          INCREMENT MONTH NUMBER                00052000
         WEND                                                           00052100
*                                                                       00052200
         PACK  PWORK,DTEGRDA      PACK GREGORIAN DAY INTO WORK FIELD    00052300
         CVB   R4,PWORK           CONVERT PACKED DECIMAL TO BINARY      00052400
*                                                                       00052500
         AR    R9,R4              ADD DAYS THIS MONTH TO PREV. RESULT   00052600
*                                                                       00052700
         CVD   R9,PWORK           CONVERT RESULT TO PACKED DECIMAL      00052800
         ZAP   DTETOTD,PWORK4     UNPACK INTO TOTAL DAYS                00052900
         OI    DTETOTD+3,X'0F'    REMOVE ANY SIGN                       00053000
*                                                                       00053100
         IF    DTEREQU,EQ,C'J',BC,EXITDATE                              00053200
         ENDIF                                                          00053300
*                                                                       00053400
         B     TOTLDAYS           GO PROCESS TOTAL DAYS                 00053500
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00053600
* ------------------------------------------------------------------- * 00053700
EXITDATE EQU   *                                                        00053800
* ------------------------------------------------------------------- * 00053900
         PACK  PWORK,DTEGRYR      PACK GREGORIAN YEAR INTO WORK FIELD   00054000
         CVB   R1,PWORK           CONVERT PACKED DECIMAL TO BINARY      00054100
         ZAP   PWORK,DTETOTD      PUT TOTAL DAYS IN WORK FIELD          00054200
         CVB   R8,PWORK           CONVERT PACKED DECIMAL TO BINARY      00054300
*                                                                       00054400
         SRDA  R8,32              CONVERT TO 64-BIT NUMBER              00054500
         D     R8,=F'7'           DIV. BY DAYS IN A WEEK                00054600
         LA    R8,1(,R8)          INCR. REMAINDER BY 1                  00054700
*                                                                       00054800
         CVD   R8,PWORK           CONVERT DAY # TO PACKED               00054900
         ZAP   DTEDAYN,PWORK4     PLACE IT INTO GREG. DAY               00055000
         OI    DTEDAYN,X'0F'      REMOVE ANY SIGN                       00055100
*                                                                       00055200
         PACK  DTEMNTH,DTEGRMO    COPY GREGORIAN MONTH TO MONTH NO.     00055300
         BAS   R11,LEAPTEST       GO TEST YEAR FOR LEAP-YEAR            00055400
*                                                                       00055500
         B     RETURN                                                   00055600
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00055700
* ------------------------------------------------------------------- * 00055800
LEAPTEST EQU   *                                                        00055900
* ------------------------------------------------------------------- * 00056000
         MVI   LEAPYEAR,C'N'      SET FOR NON-LEAP YEAR                 00056100
*                                                                       00056200
         LR    R2,R1              COPY YEAR TO REG 2                    00056300
         SRDA  R2,32              CONVERT IT TO 64-BIT NUMBER           00056400
         D     R2,=F'400'         DIV. BY YEARS BETWEEN LEAPS           00056500
         IF    R2,(ZERO,LTR),R2,BC,LEAPSET  CHECK REMAINDER             00056600
         ENDIF                                                          00056700
*                                                                       00056800
         LR    R2,R1              COPY YEAR TO REG 2                    00056900
         SRDA  R2,32              CONVERT IT TO 64-BIT NUMBER           00057000
         D     R2,=F'100'         DIV. BY YEARS BETWEEN LEAPS           00057100
         IF    R2,(ZERO,LTR),R2,BC,LEAPEXIT  CHECK REMAINDER            00057200
         ENDIF                                                          00057300
*                                                                       00057400
         LR    R2,R1              COPY YEAR TO REG 2                    00057500
         SRDA  R2,32              CONVERT IT TO 64-BIT NUMBER           00057600
         D     R2,=F'4'           DIV. BY YEARS BETWEEN LEAPS           00057700
         IF    R2,(NZERO,LTR),R2,BC,LEAPEXIT  CHECK REMAINDER           00057800
         ENDIF                                                          00057900
*                                                                       00058000
LEAPSET  MVI   LEAPYEAR,C'L'      SET FOR LEAP YEAR                     00058100
*                                                                       00058200
LEAPEXIT BR    R11                                                      00058300
* ------------------------------------------------------------------- * 00025800
         EJECT                                                          00058400
* ------------------------------------------------------------------- * 00058500
RETURN   EQU   *                                                        00058600
* ------------------------------------------------------------------- * 00058700
         RETURN (14,12)            RETURN TO CALLER                     00058800
* =================================================================== * 00058900
         EJECT                                                          00059000
* =================================================================== * 00059100
STORAGE  DS    0D                                                       00059200
* =================================================================== * 00059300
MTHTBL   DS    12H                                                      00059400
         ORG   MTHTBL                                                   00059500
MTHJAN   DC    H'31'                                                    00059600
MTHFEB   DC    H'28'                                                    00059700
MTHMAR   DC    H'31'                                                    00059800
MTHAPR   DC    H'30'                                                    00059900
MTHMAY   DC    H'31'                                                    00060000
MTHJUN   DC    H'30'                                                    00060100
MTHJUL   DC    H'31'                                                    00060200
MTHAUG   DC    H'31'                                                    00060300
MTHSEP   DC    H'30'                                                    00060400
MTHOCT   DC    H'31'                                                    00060500
MTHNOV   DC    H'30'                                                    00060600
MTHDEC   DC    H'31'                                                    00060700
         ORG                                                            00060800
*                                                                       00060900
         LTORG                                                          00061000
* =================================================================== * 00061100
         END                                                            00061200
