NUM      TITLE 'NUMMAN   - NUMBER MANIPULATION FOR COBOL PROGRAMS'      00000100
         PRINT ON,NOGEN                                                 00000200
NUMMAN   AMODE 31
NUMMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*     N U M B E R   M A N I P U L A T I O N   S U B R O U T I N E     * 00000500
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
* means of  converting  character-formatted  number  data to numeric- * 00001800
* formatted number  data.   This  function  is  most  useful  in CICS * 00001900
* programs where numeric  map  fields  allow  negative and/or decimal * 00002000
* values to be input by the user.                                     * 00002100
*                                                                     * 00002200
* =================================================================== * 00002300
*                                                                     * 00002400
*                           IMPLEMENTATION                            * 00002500
*                                                                     * 00002600
*      This implementation will  convert  a  maximum  of  15 bytes of * 00002700
* character data  to  its  numeric  equivalent  with  a  maximum of 9 * 00002800
* decimal  digits.    The  invoking  COBOL  program  should  use  the * 00002900
* following data area for passing parameters:                         * 00003000
*                                                                     * 00003100
*      01  NUMMAN-PARMS.                                              * 00003200
*          05  NUM-REQU                PIC  X(01)   VALUE 'E'.        * 00003300
*              88  NUM-REQUEST-NORMAL-EDIT          VALUE 'E'.        * 00003400
*              88  NUM-REQUEST-IGNORE-FORMAT        VALUE 'I'.        * 00003500
*          05  NUM-RETN                PIC  X(01)   VALUE SPACES.     * 00003600
*              88  NUM-REQUEST-COMPLETED            VALUE ' '.        * 00003700
*              88  NUM-INVALID-REQU                 VALUE 'E'.        * 00003800
*              88  NUM-INVALID-TOTL                 VALUE 'T'.        * 00003900
*              88  NUM-INVALID-DECM                 VALUE 'D'.        * 00004000
*              88  NUM-INVALID-COMB                 VALUE 'C'.        * 00004100
*              88  NUM-FORMAT-ERROR                 VALUE 'F'.        * 00004200
*              88  NUM-OVERFLOW-HIGH                VALUE '<'.        * 00004300
*              88  NUM-OVERFLOW-DECM                VALUE '>'.        * 00004400
*          05  NUM-TOTL                PIC S9(04)   COMP   VALUE +15. * 00004500
*          05  NUM-DECM                PIC S9(04)   COMP   VALUE +9.  * 00004600
*          05  FILLER                  PIC  X(01)   VALUE LOW-VALUE.  * 00004700
*          05  NUM-DATA                PIC  X(15)   VALUE SPACES.     * 00004800
*     *                                                               * 00004900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00005000
*              10  NUM-T15-D0          PIC S9(15).                    * 00005100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00005200
*              10  NUM-T15-D1          PIC S9(14)V9.                  * 00005300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00005400
*              10  NUM-T15-D2          PIC S9(13)V99.                 * 00005500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00005600
*              10  NUM-T15-D3          PIC S9(12)V999.                * 00005700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00005800
*              10  NUM-T15-D4          PIC S9(11)V9(4).               * 00005900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00006000
*              10  NUM-T15-D5          PIC S9(10)V9(5).               * 00006100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00006200
*              10  NUM-T15-D6          PIC S9(09)V9(6).               * 00006300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00006400
*              10  NUM-T15-D7          PIC S9(08)V9(7).               * 00006500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00006600
*              10  NUM-T15-D8          PIC S9(07)V9(8).               * 00006700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00006800
*              10  NUM-T15-D9          PIC S9(06)V9(9).               * 00006900
*     *                                                               * 00007000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00007100
*              10  FILLER              PIC  X(01).                    * 00007200
*              10  NUM-T14-D0          PIC S9(14).                    * 00007300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00007400
*              10  FILLER              PIC  X(01).                    * 00007500
*              10  NUM-T14-D1          PIC S9(13)V9.                  * 00007600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00007700
*              10  FILLER              PIC  X(01).                    * 00007800
*              10  NUM-T14-D2          PIC S9(12)V99.                 * 00007900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00008000
*              10  FILLER              PIC  X(01).                    * 00008100
*              10  NUM-T14-D3          PIC S9(11)V999.                * 00008200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00008300
*              10  FILLER              PIC  X(01).                    * 00008400
*              10  NUM-T14-D4          PIC S9(10)V9(4).               * 00008500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00008600
*              10  FILLER              PIC  X(01).                    * 00008700
*              10  NUM-T14-D5          PIC S9(09)V9(5).               * 00008800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00008900
*              10  FILLER              PIC  X(01).                    * 00009000
*              10  NUM-T14-D6          PIC S9(08)V9(6).               * 00009100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00009200
*              10  FILLER              PIC  X(01).                    * 00009300
*              10  NUM-T14-D7          PIC S9(07)V9(7).               * 00009400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00009500
*              10  FILLER              PIC  X(01).                    * 00009600
*              10  NUM-T14-D8          PIC S9(06)V9(8).               * 00009700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00009800
*              10  FILLER              PIC  X(01).                    * 00009900
*              10  NUM-T14-D9          PIC S9(05)V9(9).               * 00010000
*     *                                                               * 00010100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00010200
*              10  FILLER              PIC  X(02).                    * 00010300
*              10  NUM-T13-D0          PIC S9(13).                    * 00010400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00010500
*              10  FILLER              PIC  X(02).                    * 00010600
*              10  NUM-T13-D1          PIC S9(12)V9.                  * 00010700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00010800
*              10  FILLER              PIC  X(02).                    * 00010900
*              10  NUM-T13-D2          PIC S9(11)V99.                 * 00011000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00011100
*              10  FILLER              PIC  X(02).                    * 00011200
*              10  NUM-T13-D3          PIC S9(10)V999.                * 00011300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00011400
*              10  FILLER              PIC  X(02).                    * 00011500
*              10  NUM-T13-D4          PIC S9(09)V9(4).               * 00011600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00011700
*              10  FILLER              PIC  X(02).                    * 00011800
*              10  NUM-T13-D5          PIC S9(08)V9(5).               * 00011900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00012000
*              10  FILLER              PIC  X(02).                    * 00012100
*              10  NUM-T13-D6          PIC S9(07)V9(6).               * 00012200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00012300
*              10  FILLER              PIC  X(02).                    * 00012400
*              10  NUM-T13-D7          PIC S9(06)V9(7).               * 00012500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00012600
*              10  FILLER              PIC  X(02).                    * 00012700
*              10  NUM-T13-D8          PIC S9(05)V9(8).               * 00012800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00012900
*              10  FILLER              PIC  X(02).                    * 00013000
*              10  NUM-T13-D9          PIC S9(04)V9(9).               * 00013100
*     *                                                               * 00013200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00013300
*              10  FILLER              PIC  X(03).                    * 00013400
*              10  NUM-T12-D0          PIC S9(12).                    * 00013500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00013600
*              10  FILLER              PIC  X(03).                    * 00013700
*              10  NUM-T12-D1          PIC S9(11)V9.                  * 00013800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00013900
*              10  FILLER              PIC  X(03).                    * 00014000
*              10  NUM-T12-D2          PIC S9(10)V99.                 * 00014100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00014200
*              10  FILLER              PIC  X(03).                    * 00014300
*              10  NUM-T12-D3          PIC S9(09)V999.                * 00014400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00014500
*              10  FILLER              PIC  X(03).                    * 00014600
*              10  NUM-T12-D4          PIC S9(08)V9(4).               * 00014700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00014800
*              10  FILLER              PIC  X(03).                    * 00014900
*              10  NUM-T12-D5          PIC S9(07)V9(5).               * 00015000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00015100
*              10  FILLER              PIC  X(03).                    * 00015200
*              10  NUM-T12-D6          PIC S9(06)V9(6).               * 00015300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00015400
*              10  FILLER              PIC  X(03).                    * 00015500
*              10  NUM-T12-D7          PIC S9(05)V9(7).               * 00015600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00015700
*              10  FILLER              PIC  X(03).                    * 00015800
*              10  NUM-T12-D8          PIC S9(04)V9(8).               * 00015900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00016000
*              10  FILLER              PIC  X(03).                    * 00016100
*              10  NUM-T12-D9          PIC S9(03)V9(9).               * 00016200
*     *                                                               * 00016300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00016400
*              10  FILLER              PIC  X(04).                    * 00016500
*              10  NUM-T11-D0          PIC S9(11).                    * 00016600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00016700
*              10  FILLER              PIC  X(04).                    * 00016800
*              10  NUM-T11-D1          PIC S9(10)V9.                  * 00016900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00017000
*              10  FILLER              PIC  X(04).                    * 00017100
*              10  NUM-T11-D2          PIC S9(09)V99.                 * 00017200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00017300
*              10  FILLER              PIC  X(04).                    * 00017400
*              10  NUM-T11-D3          PIC S9(08)V999.                * 00017500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00017600
*              10  FILLER              PIC  X(04).                    * 00017700
*              10  NUM-T11-D4          PIC S9(07)V9(4).               * 00017800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00017900
*              10  FILLER              PIC  X(04).                    * 00018000
*              10  NUM-T11-D5          PIC S9(06)V9(5).               * 00018100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00018200
*              10  FILLER              PIC  X(04).                    * 00018300
*              10  NUM-T11-D6          PIC S9(05)V9(6).               * 00018400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00018500
*              10  FILLER              PIC  X(04).                    * 00018600
*              10  NUM-T11-D7          PIC S9(04)V9(7).               * 00018700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00018800
*              10  FILLER              PIC  X(04).                    * 00018900
*              10  NUM-T11-D8          PIC S9(03)V9(8).               * 00019000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00019100
*              10  FILLER              PIC  X(04).                    * 00019200
*              10  NUM-T11-D9          PIC S9(02)V9(9).               * 00019300
*     *                                                               * 00019400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00019500
*              10  FILLER              PIC  X(05).                    * 00019600
*              10  NUM-T10-D0          PIC S9(10).                    * 00019700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00019800
*              10  FILLER              PIC  X(05).                    * 00019900
*              10  NUM-T10-D1          PIC S9(09)V9.                  * 00020000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00020100
*              10  FILLER              PIC  X(05).                    * 00020200
*              10  NUM-T10-D2          PIC S9(08)V99.                 * 00020300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00020400
*              10  FILLER              PIC  X(05).                    * 00020500
*              10  NUM-T10-D3          PIC S9(07)V999.                * 00020600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00020700
*              10  FILLER              PIC  X(05).                    * 00020800
*              10  NUM-T10-D4          PIC S9(06)V9(4).               * 00020900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00021000
*              10  FILLER              PIC  X(05).                    * 00021100
*              10  NUM-T10-D5          PIC S9(05)V9(5).               * 00021200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00021300
*              10  FILLER              PIC  X(05).                    * 00021400
*              10  NUM-T10-D6          PIC S9(04)V9(6).               * 00021500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00021600
*              10  FILLER              PIC  X(05).                    * 00021700
*              10  NUM-T10-D7          PIC S9(03)V9(7).               * 00021800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00021900
*              10  FILLER              PIC  X(05).                    * 00022000
*              10  NUM-T10-D8          PIC S9(02)V9(8).               * 00022100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00022200
*              10  FILLER              PIC  X(05).                    * 00022300
*              10  NUM-T10-D9          PIC S9(01)V9(9).               * 00022400
*     *                                                               * 00022500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00022600
*              10  FILLER              PIC  X(06).                    * 00022700
*              10  NUM-T09-D0          PIC S9(09).                    * 00022800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00022900
*              10  FILLER              PIC  X(06).                    * 00023000
*              10  NUM-T09-D1          PIC S9(08)V9.                  * 00023100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00023200
*              10  FILLER              PIC  X(06).                    * 00023300
*              10  NUM-T09-D2          PIC S9(07)V99.                 * 00023400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00023500
*              10  FILLER              PIC  X(06).                    * 00023600
*              10  NUM-T09-D3          PIC S9(06)V999.                * 00023700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00023800
*              10  FILLER              PIC  X(06).                    * 00023900
*              10  NUM-T09-D4          PIC S9(05)V9(4).               * 00024000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00024100
*              10  FILLER              PIC  X(06).                    * 00024200
*              10  NUM-T09-D5          PIC S9(04)V9(5).               * 00024300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00024400
*              10  FILLER              PIC  X(06).                    * 00024500
*              10  NUM-T09-D6          PIC S9(03)V9(6).               * 00024600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00024700
*              10  FILLER              PIC  X(06).                    * 00024800
*              10  NUM-T09-D7          PIC S9(02)V9(7).               * 00024900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00025000
*              10  FILLER              PIC  X(06).                    * 00025100
*              10  NUM-T09-D8          PIC S9(01)V9(8).               * 00025200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00025300
*              10  FILLER              PIC  X(06).                    * 00025400
*              10  NUM-T09-D9          PIC SV9(9).                    * 00025500
*     *                                                               * 00025600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00025700
*              10  FILLER              PIC  X(07).                    * 00025800
*              10  NUM-T08-D0          PIC S9(08).                    * 00025900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00026000
*              10  FILLER              PIC  X(07).                    * 00026100
*              10  NUM-T08-D1          PIC S9(07)V9.                  * 00026200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00026300
*              10  FILLER              PIC  X(07).                    * 00026400
*              10  NUM-T08-D2          PIC S9(06)V99.                 * 00026500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00026600
*              10  FILLER              PIC  X(07).                    * 00026700
*              10  NUM-T08-D3          PIC S9(05)V999.                * 00026800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00026900
*              10  FILLER              PIC  X(07).                    * 00027000
*              10  NUM-T08-D4          PIC S9(04)V9(4).               * 00027100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00027200
*              10  FILLER              PIC  X(07).                    * 00027300
*              10  NUM-T08-D5          PIC S9(03)V9(5).               * 00027400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00027500
*              10  FILLER              PIC  X(07).                    * 00027600
*              10  NUM-T08-D6          PIC S9(02)V9(6).               * 00027700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00027800
*              10  FILLER              PIC  X(07).                    * 00027900
*              10  NUM-T08-D7          PIC S9(01)V9(7).               * 00028000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00028100
*              10  FILLER              PIC  X(07).                    * 00028200
*              10  NUM-T08-D8          PIC SV9(8).                    * 00028300
*     *                                                               * 00028400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00028500
*              10  FILLER              PIC  X(08).                    * 00028600
*              10  NUM-T07-D0          PIC S9(07).                    * 00028700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00028800
*              10  FILLER              PIC  X(08).                    * 00028900
*              10  NUM-T07-D1          PIC S9(06)V9.                  * 00029000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00029100
*              10  FILLER              PIC  X(08).                    * 00029200
*              10  NUM-T07-D2          PIC S9(05)V99.                 * 00029300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00029400
*              10  FILLER              PIC  X(08).                    * 00029500
*              10  NUM-T07-D3          PIC S9(04)V999.                * 00029600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00029700
*              10  FILLER              PIC  X(08).                    * 00029800
*              10  NUM-T07-D4          PIC S9(03)V9(4).               * 00029900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00030000
*              10  FILLER              PIC  X(08).                    * 00030100
*              10  NUM-T07-D5          PIC S9(02)V9(5).               * 00030200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00030300
*              10  FILLER              PIC  X(08).                    * 00030400
*              10  NUM-T07-D6          PIC S9(01)V9(6).               * 00030500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00030600
*              10  FILLER              PIC  X(08).                    * 00030700
*              10  NUM-T07-D7          PIC SV9(7).                    * 00030800
*     *                                                               * 00030900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00031000
*              10  FILLER              PIC  X(09).                    * 00031100
*              10  NUM-T06-D0          PIC S9(06).                    * 00031200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00031300
*              10  FILLER              PIC  X(09).                    * 00031400
*              10  NUM-T06-D1          PIC S9(05)V9.                  * 00031500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00031600
*              10  FILLER              PIC  X(09).                    * 00031700
*              10  NUM-T06-D2          PIC S9(04)V99.                 * 00031800
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00031900
*              10  FILLER              PIC  X(09).                    * 00032000
*              10  NUM-T06-D3          PIC S9(03)V999.                * 00032100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00032200
*              10  FILLER              PIC  X(09).                    * 00032300
*              10  NUM-T06-D4          PIC S9(02)V9(4).               * 00032400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00032500
*              10  FILLER              PIC  X(09).                    * 00032600
*              10  NUM-T06-D5          PIC S9(01)V9(5).               * 00032700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00032800
*              10  FILLER              PIC  X(09).                    * 00032900
*              10  NUM-T06-D6          PIC SV9(6).                    * 00033000
*     *                                                               * 00033100
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00033200
*              10  FILLER              PIC  X(10).                    * 00033300
*              10  NUM-T05-D0          PIC S9(05).                    * 00033400
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00033500
*              10  FILLER              PIC  X(10).                    * 00033600
*              10  NUM-T05-D1          PIC S9(04)V9.                  * 00033700
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00033800
*              10  FILLER              PIC  X(10).                    * 00033900
*              10  NUM-T05-D2          PIC S9(03)V99.                 * 00034000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00034100
*              10  FILLER              PIC  X(10).                    * 00034200
*              10  NUM-T05-D3          PIC S9(02)V999.                * 00034300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00034400
*              10  FILLER              PIC  X(10).                    * 00034500
*              10  NUM-T05-D4          PIC S9(01)V9(4).               * 00034600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00034700
*              10  FILLER              PIC  X(10).                    * 00034800
*              10  NUM-T05-D5          PIC SV9(5).                    * 00034900
*     *                                                               * 00035000
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00035100
*              10  FILLER              PIC  X(11).                    * 00035200
*              10  NUM-T04-D0          PIC S9(04).                    * 00035300
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00035400
*              10  FILLER              PIC  X(11).                    * 00035500
*              10  NUM-T04-D1          PIC S9(03)V9.                  * 00035600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00035700
*              10  FILLER              PIC  X(11).                    * 00035800
*              10  NUM-T04-D2          PIC S9(02)V99.                 * 00035900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00036000
*              10  FILLER              PIC  X(11).                    * 00036100
*              10  NUM-T04-D3          PIC S9(01)V999.                * 00036200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00036300
*              10  FILLER              PIC  X(11).                    * 00036400
*              10  NUM-T04-D4          PIC SV9(4).                    * 00036500
*     *                                                               * 00036600
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00036700
*              10  FILLER              PIC  X(12).                    * 00036800
*              10  NUM-T03-D0          PIC S9(03).                    * 00036900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00037000
*              10  FILLER              PIC  X(12).                    * 00037100
*              10  NUM-T03-D1          PIC S9(02)V9.                  * 00037200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00037300
*              10  FILLER              PIC  X(12).                    * 00037400
*              10  NUM-T03-D2          PIC S9(01)V99.                 * 00037500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00037600
*              10  FILLER              PIC  X(12).                    * 00037700
*              10  NUM-T03-D3          PIC SV999.                     * 00037800
*     *                                                               * 00037900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00038000
*              10  FILLER              PIC  X(13).                    * 00038100
*              10  NUM-T02-D0          PIC S9(02).                    * 00038200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00038300
*              10  FILLER              PIC  X(13).                    * 00038400
*              10  NUM-T02-D1          PIC S9(01)V9.                  * 00038500
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00038600
*              10  FILLER              PIC  X(13).                    * 00038700
*              10  NUM-T02-D2          PIC SV99.                      * 00038800
*     *                                                               * 00038900
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00039000
*              10  FILLER              PIC  X(14).                    * 00039100
*              10  NUM-T01-D0          PIC S9(01).                    * 00039200
*          05  FILLER      REDEFINES   NUM-DATA.                      * 00039300
*              10  FILLER              PIC  X(14).                    * 00039400
*              10  NUM-T01-D1          PIC SV9.                       * 00039500
*                                                                     * 00039600
* =================================================================== * 00039700
*                                                                     * 00039800
*                             INVOKATION                              * 00039900
*                                                                     * 00040000
*      The subroutine is invoked in  one  of two ways (depending upon * 00040100
* the function desired):                                              * 00040200
*                                                                     * 00040300
*     * CONVERT CHARACTER DATA REPORTING FORMAT ERRORS                * 00040400
*          MOVE     'E'      TO NUM-REQU                              * 00040500
*          MOVE your-totdgts TO NUM-TOTL                              * 00040600
*          MOVE your-decdgts TO NUM-DECM                              * 00040700
*          MOVE your-chrdata TO NUM-DATA                              * 00040800
*          CALL   'NUMMAN'   USING NUMMAN-PARMS                       * 00040900
*          IF  NOT NUM-REQUEST-COMPLETED                              * 00041000
*     *        your-error-handling-code                               * 00041100
*          ELSE                                                       * 00041200
*     *        your-number-evaluation-code                            * 00041300
*          END-IF                                                     * 00041400
*                                                                     * 00041500
*                               - or -                                * 00041600
*                                                                     * 00041700
*     * CONVERT CHARACTER DATA IGNORING FORMAT ERRORS                 * 00041800
*          MOVE     'I'      TO NUM-REQU                              * 00041900
*          MOVE your-totdgts TO NUM-TOTL                              * 00042000
*          MOVE your-decdgts TO NUM-DECM                              * 00042100
*          MOVE your-chrdata TO NUM-DATA                              * 00042200
*          CALL   'NUMMAN'   USING NUMMAN-PARMS                       * 00042300
*          IF  NOT NUM-REQUEST-COMPLETED                              * 00042400
*     *        your-error-handling-code                               * 00042500
*          ELSE                                                       * 00042600
*     *        your-number-evaluation-code                            * 00042700
*          END-IF                                                     * 00042800
*                                                                     * 00042900
* Where: your-totdgts is the maximum "total" numeric "digits" you can * 00043000
*                     accept in the result (include decimal digits),  * 00043100
*        your-decdgts is the maximum numeric "decimal digits" you can * 00043200
*                     accept in the result, and                       * 00043300
*        your-chrdata is the character-formatted number data you want * 00043400
*                     converted to a numeric-formatted number.        * 00043500
*                                                                     * 00043600
*      The "REDEFINES" of "NUM-DATA"  provide  all the possible COBOL * 00043700
* "PIC"tures needed to allow you to "MOVE" the result to your dataset * 00043800
* record area, for instance.  The following example shows how:        * 00043900
*                                                                     * 00044000
* Dataset Record Contains:                                            * 00044100
*          05  NUMBER-VALUE            PIC S9(5)V99 COMP-3.           * 00044200
*                                                                     * 00044300
* Map I/O Area Contains:                                              * 00044400
*          05  MAPNUML                 PIC S9(4) COMP.                * 00044500
*          05  MAPNUMA                 PIC X(1).                      * 00044600
*          05  MAPNUMI                 PIC X(10).                     * 00044700
*          05  MAPNUMO REDEFINES MAPNUMI PIC ZZ,ZZ9.99-.              * 00044800
* and:                                                                * 00044900
*          05  MAPMSGL                 PIC S9(4) COMP.                * 00045000
*          05  MAPMSGA                 PIC X(1).                      * 00045100
*          05  MAPMSGI                 PIC X(50).                     * 00045200
*          05  MAPMSGO REDEFINES MAPMSGI PIC X(50).                   * 00045300
*                                                                     * 00045400
* Invoke NUMMAN as follows:                                           * 00045500
*          MOVE     'E'      TO NUM-REQU.                             * 00045600
*          MOVE     07       TO NUM-TOTL.                             * 00045700
*          MOVE      2       TO NUM-DECM.                             * 00045800
*          MOVE   MAPNUMI    TO NUM-DATA.                             * 00045900
*          CALL   'NUMMAN'   USING NUMMAN-PARMS.                      * 00046000
*          IF  NOT NUM-REQUEST-COMPLETED                              * 00046100
*              MOVE    -1    TO MAPNUML                               * 00046200
*              MOVE DFHUNIMD TO MAPNUMA                               * 00046300
*              MOVE 'VALUE NOT FORMATTED AS "99,999.99-", RESPECIFY.' * 00046400
*                  TO MAPMSGO                                         * 00046500
*          ELSE                                                       * 00046600
*          MOVE  NUM-T07-D2  TO MAPNUMO NUMBER-VALUE.                 * 00046700
*                                                                     * 00046800
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00046900
         EJECT                                                          00047000
* =================================================================== * 00047100
NUMPARM  DSECT                    NUMMAN PARMS DUMMY SECTION            00047200
         USING *,R10              ESTABLISH REG 10 AS BASE              00047300
* =================================================================== * 00047400
NUMREQU  DS    CL1                REQUEST CODE                          00047500
NUMRETN  DS    CL1                RETURN CODE                           00047600
NUMTOTL  DS    H                  TOTAL DIGITS ALLOWED                  00047700
NUMDECM  DS    H                  DECIMAL DIGITS ALLOWED                00047800
FLAGS    DS    BL1                (INTERNAL USE ONLY)                   00047900
FMINUS   EQU   B'10000000'         MINUS SIGN DETECTION                 00048000
FPLUS    EQU   B'01000000'         PLUS SIGN DETECTION                  00048100
FDIGIT   EQU   B'00100000'         NUMERIC DIGIT DETECTION              00048200
FPOINT   EQU   B'00010000'         DECIMAL POINT DETECTION              00048300
FCOMMA   EQU   B'00001000'         COMMA FORMAT DETECTION               00048400
FBLANK   EQU   B'00000100'         IMBEDDED BLANK DETECTION             00048500
NUMDATA  DS    CL15               NUMBER DATA                           00048600
* =================================================================== * 00048700
         EJECT                                                          00048800
* =================================================================== * 00048900
*        REGISTER USAGES                                                00049000
* =================================================================== * 00049100
R0       EQU   0                  PARM REG                              00049200
R1       EQU   1                  PARM REG                              00049300
R2       EQU   2                  WORK REG (INSERT CHAR FOR COMPARE)    00049400
R3       EQU   3                  WORK REG (DIGITS BETWEEN COMMAS)      00049500
R4       EQU   4                  WORK REG (TOTAL DIGITS FOUND)         00049600
R5       EQU   5                  WORK REG (DECIMAL DIGITS FOUND)       00049700
R6       EQU   6                  WORK REG (LOOP/INDEX REGISTER)        00049800
R7       EQU   7                  WORK REG (POINTER REGISTER)           00049900
R8       EQU   8                  WORK REG (MVC " TO " ADDRESS)         00050000
R9       EQU   9                  WORK REG (MVC "FROM" ADDRESS)         00050100
R10      EQU   10                 NUMPARM DSECT BASE REG                00050200
R11      EQU   11                                                       00050300
R12      EQU   12                                                       00050400
R13      EQU   13                 REGISTER SAVEAREA ADDR                00050500
R14      EQU   14                 RETURN REG                            00050600
R15      EQU   15                 ENTRY REG                             00050700
* =================================================================== * 00050800
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  IF
         COPY  WHILE
* =================================================================== * 00012800
         EJECT                                                          00050900
* =================================================================== * 00051000
NUMMAN   CSECT                    NUMMAN CONTROL SECTION                00051100
         USING *,R15              ESTABLISH ENTRY REG AS BASE REG       00051200
* =================================================================== * 00051300
         STM   R14,R12,12(R13)    SAVE CALLER'S REGISTERS               00051400
         B     START              SKIP OVER CORE EYE-CATCHER            00051500
*                                                                       00051600
         DC    CL8'NUMMAN  '      PROGRAM                               00051700
         DC    CL4'1.0 '          VERSION                               00051800
         DC    CL4'DLC '          AUTHOR                                00051900
         DC    CL8'(C)SEP90'      DATE                                  00052000
*                                                                       00052100
START    DS    0H                                                       00052200
         L     R10,0(,R1)         GET ADDRESS OF PARAMETERS             00052300
*                                                                       00052400
         LH    R3,NUMTOTL         GET TOTAL DIGIT ALLOWANCE             00052500
         IF    R3,(NPLUS,LTR),R3,OR,R3,(HIGHER,CH),=H'15'               00052600
          MVI  NUMRETN,C'T'       TOTAL DIGITS IS IN ERROR              00052700
          B    RETURN             GO RETURN TO CALLER                   00052800
         ENDIF                                                          00052900
*                                                                       00053000
         LH    R3,NUMDECM         GET DECIMAL DIGIT ALLOWANCE           00053100
         IF    R3,(MINUS,LTR),R3,OR,R3,(HIGHER,CH),=H'9'                00053200
          MVI  NUMRETN,C'D'       DECIMAL DIGITS IS IN ERROR            00053300
          B    RETURN             GO RETURN TO CALLER                   00053400
         ENDIF                                                          00053500
*                                                                       00053600
         IF    R3,(HIGHER,CH),NUMTOTL                                   00053700
          MVI  NUMRETN,C'C'       COMBINATION IS IN ERROR               00053800
          B    RETURN             GO RETURN TO CALLER                   00053900
         ENDIF                                                          00054000
*                                                                       00054100
         IF    NUMREQU,EQUAL,C'E',OR,NUMREQU,EQUAL,C'I'                 00054200
          B    PROCESS            GO PROCESS THE REQUEST                00054300
         ENDIF                                                          00054400
*                                                                       00054500
         MVI   NUMRETN,C'E'       NUMBER REQUEST IS IN ERROR            00054600
         B     RETURN             GO RETURN TO CALLER                   00054700
         EJECT                                                          00054800
* ------------------------------------------------------------------- * 00054900
PROCESS  EQU   *                                                        00055000
* ------------------------------------------------------------------- * 00055100
         MVI   NUMRETN,C' '       INITIALIZE RETURN CODE                00055200
         XC    FLAGS,FLAGS        CLEAR FLAGS                           00055300
         XR    R2,R2              CLEAR EDIT CHAR REGISTER              00055400
         XR    R3,R3              CLEAR WHOLE DIGITS REGISTER           00055500
         XR    R4,R4              CLEAR TOTAL DIGITS REGISTER           00055600
         XR    R5,R5              CLEAR DECIMAL DIGITS REGISTER         00055700
         LA    R6,L'NUMDATA-1     SET LOOP/INDEX REGISTER               00055800
         LA    R7,L'NUMDATA-1     SET POINTER REGISTER                  00055900
*                                                                       00056000
         WHILE R6,(NMINUS,LTR),R6                                       00056100
*                                                                       00056200
          IC   R2,NUMDATA(R6)     GET CHARACTER FOR TESTING             00056300
          BCTR R6,0               POINT TO NEXT CHARACTER               00056400
*                                                                       00056500
          IF   R2,(EQUAL,CLM,B'0001'),=C' '                             00056600
           IF  FLAGS,(ZEROS,TM),FMINUS+FPLUS+FDIGIT+FPOINT+FCOMMA       00056700
            WLOOP                 SKIP ALL TRAILING BLANKS              00056800
           ENDIF                                                        00056900
           OI  FLAGS,FBLANK       SET IMBEDED BLANK CHECKING ON         00057000
           IF  FLAGS,(NOT,TM),FCOMMA                                    00057100
            WLOOP                 COMMA CHECKING NOT ON                 00057200
           ENDIF                                                        00057300
           IF  R3,(HIGHER,CH),=H'3',BC,FMTERR   COMMA NOT AT 3 DIGITS   00057400
           ENDIF                                                        00057500
           LA  R3,0               RESET WHOLE DIGITS REGISTER           00057600
           WLOOP                                                        00057700
          ENDIF                                                         00057800
*                                                                       00057900
          IF   FLAGS,(ON,TM),FBLANK,AND,NUMREQU,NEQUAL,C'I'             00058000
           B   FMTERR             NUMBER CONTAINS IMMBEDED BLANKS       00058100
          ENDIF                                                         00058200
*                                                                       00058300
          IF   R2,(EQUAL,CLM,B'0001'),=C'.'                             00058400
           IF  FLAGS,(ON,TM),FPOINT,BC,FMTERR   ONLY 1 POINT ALLOWED    00058500
           ENDIF                                                        00058600
           OI  FLAGS,FPOINT       SET POINT CHAR ENCOUNTERED            00058700
           LR  R5,R3              MOVE WHOLES COUNT AS DECIMAL COUNT    00058800
           LA  R3,0               RESET WHOLE DIGITS REGISTER           00058900
           WLOOP                                                        00059000
          ENDIF                                                         00059100
*                                                                       00059200
          IF   R2,(EQUAL,CLM,B'0001'),=C'+'                             00059300
           IF  FLAGS,(NZEROS,TM),FMINUS+FPLUS,BC,FMTERR   ONLY 1 SIGN   00059400
           ENDIF                                                        00059500
           OI  FLAGS,FPLUS        SET PLUS CHAR ENCOUNTERED             00059600
           WLOOP                                                        00059700
          ENDIF                                                         00059800
*                                                                       00059900
          IF   R2,(EQUAL,CLM,B'0001'),=C'-'                             00060000
           IF  FLAGS,(NZEROS,TM),FMINUS+FPLUS,BC,FMTERR   ONLY 1 SIGN   00060100
           ENDIF                                                        00060200
           OI  FLAGS,FMINUS       SET DASH CHAR ENCOUNTERED             00060300
           WLOOP                                                        00060400
          ENDIF                                                         00060500
*                                                                       00060600
          IF   R2,(EQUAL,CLM,B'0001'),=C','                             00060700
           IF  FLAGS,(NOT,TM),FPOINT                                    00060800
            LH R1,NUMDECM         GET DECIMAL DIGIT ALLOWANCE           00060900
            IF R1,(PLUS,LTR),R1,BC,FMTERR   COMMA RIGHT OF POINT        00061000
            ENDIF                                                       00061100
           ENDIF                                                        00061200
           OI  FLAGS,FCOMMA       SET COMMA CHAR ENCOUNTERED            00061300
           IF  R3,(NEQUAL,CH),=H'3',BC,FMTERR   COMMA NOT AT 3 DIGITS   00061400
           ENDIF                                                        00061500
           LA  R3,0               RESET WHOLE DIGITS REGISTER           00061600
           WLOOP                                                        00061700
          ENDIF                                                         00061800
*                                                                       00061900
          IF   R2,(NL,CLM,B'0001'),=C'0',AND,R2,(NH,CLM,B'0001'),=C'9'  00062000
           OI  FLAGS,FDIGIT       SET DIGIT CHAR ENCOUNTERED            00062100
           STC R2,NUMDATA(R7)     RIGHT JUSTIFY NUMERIC CHARACTER       00062200
           BCTR R7,0              DECREMENT POINTER FOR NEXT CHAR       00062300
           LA  R3,1(,R3)          INCREMENT WHOLE DIGITS                00062400
           LA  R4,1(,R4)          INCREMENT TOTAL DIGITS                00062500
           WLOOP                                                        00062600
          ENDIF                                                         00062700
*                                                                       00062800
FMTERR    IF   NUMREQU,NEQUAL,C'I'                                      00062900
           MVI NUMRETN,C'F'       NUMBER IS NOT IN NUMERIC FORMAT       00063000
           B   RETURN             GO RETURN TO CALLER                   00063100
          ENDIF                                                         00063200
*                                                                       00063300
         WEND                                                           00063400
*                                                                       00063500
         IF    R3,(HIGHER,CH),=H'3',AND,FLAGS,(ON,TM),FCOMMA            00063600
          IF   NUMREQU,NEQUAL,C'I'                                      00063700
           MVI NUMRETN,C'F'       NUMBER IS NOT IN NUMERIC FORMAT       00063800
           B   RETURN             GO RETURN TO CALLER                   00063900
          ENDIF                                                         00064000
         ENDIF                                                          00064100
*                                                                       00064200
         IC    R2,=C'0'           GET CHARACTER FOR LEFT-ZERO           00064300
         WHILE R7,(NMINUS,LTR),R7                                       00064400
          STC  R2,NUMDATA(R7)     LEFT-ZERO THE RIGHT-JUSTIFIED FIELD   00064500
          BCTR R7,0               DECREMENT POINTER FOR NEXT CHAR       00064600
         WEND                                                           00064700
*                                                                       00064800
         SR    R4,R5              WHOLES FOUND IS TOTAL - DECIMAL FOUND 00064900
         LH    R3,NUMTOTL         CALCULATE WHOLES ALLOWED AS           00065000
         SH    R3,NUMDECM         TOTAL ALLOWED MINUS DECIMAL ALLOWED   00065100
*                                                                       00065700
         IF    R5,(HIGHER,CH),NUMDECM                                   00065800
          MVI  NUMRETN,C'>'       TOO MANY DIGITS RIGHT OF DECIMAL      00065900
          B    RETURN             GO RETURN TO CALLER                   00066000
         ENDIF                                                          00066100
*                                                                       00065200
         IF    R4,(HIGHER,CR),R3                                        00065300
          IF   NUMREQU,NEQUAL,C'I'
           MVI NUMRETN,C'<'       TOO MANY DIGITS LEFT OF DECIMAL       00065400
           B   RETURN             GO RETURN TO CALLER                   00065500
          ELSE                                                          00065500
           B   SETSIGN            GO SET SIGN                           00065500
          ENDIF                                                         00065600
         ENDIF                                                          00065600
*                                                                       00066200
         LH    R3,NUMDECM         CALCULATE AMOUNT OF ALIGNMENT AS      00066300
         IF    R3,(PLUS,SR),R5    DECIMAL ALLOWED MINUS DECIMAL FOUND   00066400
          LA   R6,NUMDATA          GET " TO " ADDRESS FOR ALIGNMENT     00066500
          LR   R8,R6               GET "FROM" ADDRESS FOR ALIGNMENT     00066600
          LA   R7,L'NUMDATA        GET " TO " LENGTH FOR ALIGNMENT      00066700
          LR   R9,R7               GET "FROM" LENGTH FOR ALIGNMENT      00066800
          AR   R8,R3               ADD THAT  TO  "FROM" ADDRESS         00066900
          SR   R9,R3               SUBTRACT FROM "FROM" LENGTH          00067000
          ICM  R9,B'1000',=C'0'    SET " TO " RIGHT PAD CHARACTER       00067100
          MVCL R6,R8               DECIMAL ALIGN NUMERIC DATA FIELD     00067200
         ENDIF                                                          00067300
*                                                                       00067400
SETSIGN  NI    NUMDATA+14,X'CF'   CHANGE X'F?' SIGN TO X'C?' SIGN       00067500
         IF    FLAGS,(ON,TM),FMINUS                                     00067600
          OI   NUMDATA+14,X'D0'   PUT A NEGATIVE SIGN ON THE NUMBER     00067700
         ENDIF                                                          00067800
*                                                                       00067900
         B     RETURN             GO RETURN TO CALLER                   00068000
         EJECT                                                          00068100
* ------------------------------------------------------------------- * 00068200
RETURN   EQU   *                                                        00068300
* ------------------------------------------------------------------- * 00068400
         LM    R14,R12,12(R13)    RESTORE CALLER'S REGISTERS            00068500
         XR    R15,R15            ZERO RETURN CODE REGISTER             00068600
         BR    R14                RETURN TO CALLER                      00068700
* =================================================================== * 00068800
         EJECT                                                          00068900
* =================================================================== * 00069000
STORAGE  DS    0D                                                       00069100
* =================================================================== * 00069200
         LTORG                                                          00069300
* =================================================================== * 00069400
         END                                                            00069500
