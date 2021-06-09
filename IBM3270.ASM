3270     TITLE 'IBM3270  - IBM 3270 INFORMATION DISPLAY SYSTEM - DATA SX00000100
               TREAM CONTROL CHARACTER LITERALS'                        00000200
         PRINT ON,NOGEN                                                 00000300
*IBM3270  AMODE 31
*IBM3270  RMODE ANY
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
*      This table provides the  datastream control character literals * 00001400
* necessary for a COBOL program to fully utilize the IBM 3270 Inform- * 00001500
* ation Display System in a CICS  environment with or without the use * 00001600
* of Basic Mapping Support.                                           * 00001700
*                                                                     * 00001800
* =================================================================== * 00001900
*                                                                     * 00002000
*                           IMPLEMENTATION                            * 00002100
*                                                                     * 00002200
*      The invoking COBOL program should  use the following data area * 00002300
* to obtain addressability to the table:                              * 00002400
*                                                                     * 00002500
*     *                                                               * 00002600
*     * IBM 3270 IDS - DATA STREAM CONTROL CHARACTERS                 * 00002700
*     *                                                               * 00002800
*      01  IBM3270.                                                   * 00002900
*     *                                                               * 00003000
*     * LOCAL AND REMOTE COMMAND CODES                                * 00003100
*     *                                                               * 00003200
*        02  CMDCODES.                                                * 00003300
*          05  LCLNOOP     PIC  X(01).                                * 00003400
*     *                                LOCAL NO OPERATION             * 00003500
*          05  LCLERAUP    PIC  X(01).                                * 00003600
*     *                                LOCAL ERASE ALL UNPROTECTED    * 00003700
*          05  LCLERWRA    PIC  X(01).                                * 00003800
*     *                                LOCAL ERASE/WRITE ALTERNATE    * 00003900
*          05  LCLERWRT    PIC  X(01).                                * 00004000
*     *                                LOCAL ERASE/WRITE              * 00004100
*          05  LCLRDBUF    PIC  X(01).                                * 00004200
*     *                                LOCAL READ BUFFER              * 00004300
*          05  LCLRDMDF    PIC  X(01).                                * 00004400
*     *                                LOCAL READ MODIFIED            * 00004500
*          05  LCLWRITE    PIC  X(01).                                * 00004600
*     *                                LOCAL WRITE                    * 00004700
*          05  LCLWRSTR    PIC  X(01).                                * 00004800
*     *                                LOCAL WRITE STRUCTURED         * 00004900
*          05  RMTCOPY     PIC  X(01).                                * 00005000
*     *                                REMOTE COPY                    * 00005100
*          05  RMTERAUP    PIC  X(01).                                * 00005200
*     *                                REMOTE ERASE ALL UNPROTECTED   * 00005300
*          05  RMTERWRA    PIC  X(01).                                * 00005400
*     *                                REMOTE ERASE/WRITE ALTERNATE   * 00005500
*          05  RMTERWRT    PIC  X(01).                                * 00005600
*     *                                REMOTE ERASE/WRITE             * 00005700
*          05  RMTRDBUF    PIC  X(01).                                * 00005800
*     *                                REMOTE READ BUFFER             * 00005900
*          05  RMTRDMDF    PIC  X(01).                                * 00006000
*     *                                REMOTE READ MODIFIED           * 00006100
*          05  RMTWRITE    PIC  X(01).                                * 00006200
*     *                                REMOTE WRITE                   * 00006300
*          05  RMTWRSTR    PIC  X(01).                                * 00006400
*     *                                REMOTE WRITE STRUCTURED        * 00006500
*     *                                                               * 00006600
*     * WRITE CONTROL CHARACTERS                                      * 00006700
*     *                                                               * 00006800
*        02  WRTCHARS.                                                * 00006900
*          05  WRTLKFST    PIC  X(01).                                * 00007000
*     *                                CTRL=(LEAVEKB,FSET)            * 00007100
*          05  WRTLKFRS    PIC  X(01).                                * 00007200
*     *                                CTRL=(LEAVEKB,FRSET)           * 00007300
*          05  WRTFKFST    PIC  X(01).                                * 00007400
*     *                                CTRL=(FREEKB,FSET)             * 00007500
*          05  WRTFKFRS    PIC  X(01).                                * 00007600
*     *                                CTRL=(FREEKB,FRSET)            * 00007700
*          05  WRTLKFSA    PIC  X(01).                                * 00007800
*     *                                CTRL=(LEAVEKB,FSET,ALARM)      * 00007900
*          05  WRTLKFRA    PIC  X(01).                                * 00008000
*     *                                CTRL=(LEAVEKB,FRSET,ALARM)     * 00008100
*          05  WRTFKFSA    PIC  X(01).                                * 00008200
*     *                                CTRL=(FREEKB,FSET,ALARM)       * 00008300
*          05  WRTFKFRA    PIC  X(01).                                * 00008400
*     *                                CTRL=(FREEKB,FRSET,ALARM)      * 00008500
*          05  WRTPR132    PIC  X(01).                                * 00008600
*     *                                CTRL=(PRINT,HONEOM)            * 00008700
*          05  WRTPA132    PIC  X(01).                                * 00008800
*     *                                CTRL=(PRINT,HONEOM,ALARM)      * 00008900
*          05  WRTPRL40    PIC  X(01).                                * 00009000
*     *                                CTRL=(PRINT,L40)               * 00009100
*          05  WRTPAL40    PIC  X(01).                                * 00009200
*     *                                CTRL=(PRINT,L40,ALARM)         * 00009300
*          05  WRTPRL64    PIC  X(01).                                * 00009400
*     *                                CTRL=(PRINT,L64)               * 00009500
*          05  WRTPAL64    PIC  X(01).                                * 00009600
*     *                                CTRL=(PRINT,L64,ALARM)         * 00009700
*          05  WRTPRL80    PIC  X(01).                                * 00009800
*     *                                CTRL=(PRINT,L80)               * 00009900
*          05  WRTPAL80    PIC  X(01).                                * 00010000
*     *                                CTRL=(PRINT,L80,ALARM)         * 00010100
*     *                                                               * 00010200
*     * SCREEN BUFFER CONTROL ORDERS                                  * 00010300
*     *                                                               * 00010400
*        02  SCRORDER.                                                * 00010500
*          05  SCRPGTAB    PIC  X(01).                                * 00010600
*     *                                PROGRAM TAB                    * 00010700
*          05  SCRGRESC    PIC  X(01).                                * 00010800
*     *                                GRAPHIC ESCAPE + 1-BYTE CHAR   * 00010900
*          05  SCRSETBA    PIC  X(01).                                * 00011000
*     *                                SET BUFFER ADDRESS + 2-BYTE ADR* 00011100
*          05  SCRERASE    PIC  X(01).                                * 00011200
*     *                                ERASE UNPROTECTED + 2-BYTE ADDR* 00011300
*          05  SCRCURSR    PIC  X(01).                                * 00011400
*     *                                INSERT CURSOR                  * 00011500
*          05  SCRSTFLD    PIC  X(01).                                * 00011600
*     *                                START FIELD + 1-BYTE ATTRIBUTE * 00011700
*          05  SCRSETAT    PIC  X(01).                                * 00011800
*     *                                SET ATTRIBUTE + 2-BYTE TYPE-VAL* 00011900
*          05  SCRSTFEX    PIC  X(01).                                * 00012000
*     *                                START FIELD EXT + CNT + TYP-VAL* 00012100
*          05  SCRMODFY    PIC  X(01).                                * 00012200
*     *                                MODIFY FIELD + COUNT + TYPE-VAL* 00012300
*          05  SCREPEAT    PIC  X(01).                                * 00012400
*     *                                REPEAT TO ADDR + ADDRESS + CHAR* 00012500
*     *                                                               * 00012600
*     * PRINTER CONTROL ORDERS                                        * 00012700
*     *                                                               * 00012800
*        02  PRTORDER.                                                * 00012900
*          05  PRTEJECT    PIC  X(01).                                * 00013000
*     *                                FORMS FEED                     * 00013100
*          05  PRTRETRN    PIC  X(01).                                * 00013200
*     *                                CARRIAGE RETURN                * 00013300
*          05  PRTNEWLN    PIC  X(01).                                * 00013400
*     *                                NEW LINE (CR/LF)               * 00013500
*          05  PRTENDOM    PIC  X(01).                                * 00013600
*     *                                END OF MESSAGE (DSC ONLY)      * 00013700
*          05  PRTCOLOR    PIC  X(01).                                * 00013800
*     *                                SELECT COLOR + 1-BYTE VALUE    * 00013900
*          05  PRTLFEED    PIC  X(01).                                * 00014000
*     *                                LINE FEED (SCS ONLY)           * 00014100
*          05  PRTBLACK    PIC  X(01).                                * 00014200
*     *                                COLOR=BLACK ATTRIBUTE          * 00014300
*          05  PRTBLUE     PIC  X(01).                                * 00014400
*     *                                COLOR=BLUE ATTRIBUTE           * 00014500
*          05  PRTRED      PIC  X(01).                                * 00014600
*     *                                COLOR=RED ATTRIBUTE            * 00014700
*          05  PRTGREEN    PIC  X(01).                                * 00014800
*     *                                COLOR=GREEN ATTRIBUTE          * 00014900
*     *                                                               * 00015000
*     * STANDARD FIELD ATTRIBUTES                                     * 00015100
*     *                                                               * 00015200
*        02  FLDATTRB-LIT.                                            * 00015300
*          05  FLDUNPRO    PIC  X(01).                                * 00015400
*     *                                ATTRB=(UNPROT,NORM)            * 00015500
*          05  FLDUNFST    PIC  X(01).                                * 00015600
*     *                                ATTRB=(UNPROT,NORM,FSET)       * 00015700
*          05  FLDUNDET    PIC  X(01).                                * 00015800
*     *                                ATTRB=(UNPROT,NORM,DET)        * 00015900
*          05  FLDUNDTF    PIC  X(01).                                * 00016000
*     *                                ATTRB=(UNPROT,NORM,DET,FSET)   * 00016100
*          05  FLDUNBRT    PIC  X(01).                                * 00016200
*     *                                ATTRB=(UNPROT,BRT)             * 00016300
*          05  FLDUNBRF    PIC  X(01).                                * 00016400
*     *                                ATTRB=(UNPROT,BRT,FSET)        * 00016500
*          05  FLDUNDRK    PIC  X(01).                                * 00016600
*     *                                ATTRB=(UNPROT,DRK)             * 00016700
*          05  FLDUNDKF    PIC  X(01).                                * 00016800
*     *                                ATTRB=(UNPROT,DRK,FSET)        * 00016900
*          05  FLDUNNUM    PIC  X(01).                                * 00017000
*     *                                ATTRB=(UNPROT,NORM,NUM)        * 00017100
*          05  FLDUNNMF    PIC  X(01).                                * 00017200
*     *                                ATTRB=(UNPROT,NORM,NUM,FSET)   * 00017300
*          05  FLDUNNMD    PIC  X(01).                                * 00017400
*     *                                ATTRB=(UNPROT,NORM,NUM,DET)    * 00017500
*          05  FLDUNNDF    PIC  X(01).                                * 00017600
*     *                                ATTRB=(UNPROT,NORM,NUM,DET,FSET* 00017700
*          05  FLDUNBRN    PIC  X(01).                                * 00017800
*     *                                ATTRB=(UNPROT,BRT,NUM)         * 00017900
*          05  FLDUNBNF    PIC  X(01).                                * 00018000
*     *                                ATTRB=(UNPROT,BRT,NUM,FSET)    * 00018100
*          05  FLDUNDKN    PIC  X(01).                                * 00018200
*     *                                ATTRB=(UNPROT,DRK,NUM)         * 00018300
*          05  FLDUNDNF    PIC  X(01).                                * 00018400
*     *                                ATTRB=(UNPROT,DRK,NUM,FSET)    * 00018500
*          05  FLDPROT     PIC  X(01).                                * 00018600
*     *                                ATTRB=(PROT,NORM)              * 00018700
*          05  FLDPRFST    PIC  X(01).                                * 00018800
*     *                                ATTRB=(PROT,NORM,FSET)         * 00018900
*          05  FLDPRDET    PIC  X(01).                                * 00019000
*     *                                ATTRB=(PROT,NORM,DET)          * 00019100
*          05  FLDPRDTF    PIC  X(01).                                * 00019200
*     *                                ATTRB=(PROT,NORM,DET,FSET)     * 00019300
*          05  FLDPRBRT    PIC  X(01).                                * 00019400
*     *                                ATTRB=(PROT,BRT)               * 00019500
*          05  FLDPRBRF    PIC  X(01).                                * 00019600
*     *                                ATTRB=(PROT,BRT,FSET)          * 00019700
*          05  FLDPRDRK    PIC  X(01).                                * 00019800
*     *                                ATTRB=(PROT,DRK)               * 00019900
*          05  FLDPRDKF    PIC  X(01).                                * 00020000
*     *                                ATTRB=(PROT,DRK,FSET)          * 00020100
*          05  FLDASKIP    PIC  X(01).                                * 00020200
*     *                                ATTRB=(ASKIP,NORM)             * 00020300
*          05  FLDASFST    PIC  X(01).                                * 00020400
*     *                                ATTRB=(ASKIP,NORM,FSET)        * 00020500
*          05  FLDASDET    PIC  X(01).                                * 00020600
*     *                                ATTRB=(ASKIP,NORM,DET)         * 00020700
*          05  FLDASDTF    PIC  X(01).                                * 00020800
*     *                                ATTRB=(ASKIP,NORM,DET,FSET)    * 00020900
*          05  FLDASBRT    PIC  X(01).                                * 00021000
*     *                                ATTRB=(ASKIP,BRT)              * 00021100
*          05  FLDASBRF    PIC  X(01).                                * 00021200
*     *                                ATTRB=(ASKIP,BRT,FSET)         * 00021300
*          05  FLDASDRK    PIC  X(01).                                * 00021400
*     *                                ATTRB=(ASKIP,DRK)              * 00021500
*          05  FLDASDKF    PIC  X(01).                                * 00021600
*     *                                ATTRB=(ASKIP,DRK,FSET)         * 00021700
*        02  FLDATTRB-TBL  REDEFINES   FLDATTRB-LIT.                  * 00021800
*          05  FLDATTRB    PIC  X(01)  OCCURS 32                      * 00021900
*                                      INDEXED BY FLDATTRB-IDX.       * 00022000
*     *                                                               * 00022100
*        02  FLDMODFY      PIC  X(01).                                * 00022200
*     *                                FIELD MODIFIED ("RECEIVE MAP") * 00022300
*     *                                                               * 00022400
*     * EXTENDED FIELD TYPES AND ATTRIBUTES                           * 00022500
*     *                                                               * 00022600
*        02  EXTATTRB.                                                * 00022700
*          05  TYPRESET    PIC  X(01).                                * 00022800
*     *                                "TYPE" RESET                   * 00022900
*          05  TYPHILIT    PIC  X(01).                                * 00023000
*     *                                "TYPE" HILIGHT                 * 00023100
*          05  TYPCOLOR    PIC  X(01).                                * 00023200
*     *                                "TYPE" COLOR                   * 00023300
*          05  TYPPSYMB    PIC  X(01).                                * 00023400
*     *                                "TYPE" PROGRAMMED SYMBOLS      * 00023500
*          05  TYPFIELD    PIC  X(01).                                * 00023600
*     *                                "TYPE" FIELD ATTRIBUTE         * 00023700
*        02  EXHATTRB-LIT.                                            * 00023800
*          05  EXHDEFLT    PIC  X(01).                                * 00023900
*     *                                HILITE=DEFAULT                 * 00024000
*          05  EXHBLINK    PIC  X(01).                                * 00024100
*     *                                HILITE=BLINK                   * 00024200
*          05  EXHREVRS    PIC  X(01).                                * 00024300
*     *                                HILITE=REVERSE                 * 00024400
*          05  FILLER      PIC  X(01).                                * 00024500
*     *                                HILITE=?                       * 00024600
*          05  EXHULINE    PIC  X(01).                                * 00024700
*     *                                HILITE=UNDERLINE               * 00024800
*        02  EXHATTRB-TBL  REDEFINES   EXHATTRB-LIT.                  * 00024900
*          05  EXHATTRB    PIC  X(01)  OCCURS 5.                      * 00025000
*     *                                                               * 00025100
*        02  EXCATTRB-LIT.                                            * 00025200
*          05  EXCDEFLT    PIC  X(01).                                * 00025300
*     *                                COLOR=DEFAULT (GREEN)          * 00025400
*          05  EXCBLUE     PIC  X(01).                                * 00025500
*     *                                COLOR=BLUE                     * 00025600
*          05  EXCRED      PIC  X(01).                                * 00025700
*     *                                COLOR=RED                      * 00025800
*          05  EXCPINK     PIC  X(01).                                * 00025900
*     *                                COLOR=PINK                     * 00026000
*          05  EXCGREEN    PIC  X(01).                                * 00026100
*     *                                COLOR=GREEN                    * 00026200
*          05  EXCTURQU    PIC  X(01).                                * 00026300
*     *                                COLOR=TURQUOISE                * 00026400
*          05  EXCYELLW    PIC  X(01).                                * 00026500
*     *                                COLOR=YELLOW                   * 00026600
*          05  EXCNEUTR    PIC  X(01).                                * 00026700
*     *                                COLOR=NEUTRAL (WHITE)          * 00026800
*        02  EXCATTRB-TBL  REDEFINES   EXCATTRB-LIT.                  * 00026900
*          05  EXCATTRB    PIC  X(01)  OCCURS 8.                      * 00027000
*     *                                                               * 00027100
*     * ATTENTION IDENTIFICATION VALUES                               * 00027200
*     *                                                               * 00027300
*        02  AIDVALUE-LIT.                                            * 00027400
*          05  AIDNOAID    PIC  X(01).                                * 00027500
*     *                                NO AID GENERATED (NO DATA)     * 00027600
*          05  AIDCLEAR    PIC  X(01).                                * 00027500
*     *                                CLEAR KEY                      * 00027600
*          05  AIDENTER    PIC  X(01).                                * 00027700
*     *                                ENTER KEY                      * 00027800
*          05  AIDCURSR    PIC  X(01).                                * 00027900
*     *                                CURSOR SELECT KEY/LIGHT PEN    * 00028000
*          05  AIDPAK01    PIC  X(01).                                * 00028100
*     *                                PA1 KEY                        * 00028200
*          05  AIDPAK02    PIC  X(01).                                * 00028300
*     *                                PA2 KEY                        * 00028400
*          05  AIDPAK03    PIC  X(01).                                * 00028500
*     *                                PA3 KEY                        * 00028600
*          05  AIDPFK01    PIC  X(01).                                * 00028700
*     *                                PF1 KEY                        * 00028800
*          05  AIDPFK02    PIC  X(01).                                * 00028900
*     *                                PF2 KEY                        * 00029000
*          05  AIDPFK03    PIC  X(01).                                * 00029100
*     *                                PF3 KEY                        * 00029200
*          05  AIDPFK04    PIC  X(01).                                * 00029300
*     *                                PF4 KEY                        * 00029400
*          05  AIDPFK05    PIC  X(01).                                * 00029500
*     *                                PF5 KEY                        * 00029600
*          05  AIDPFK06    PIC  X(01).                                * 00029700
*     *                                PF6 KEY                        * 00029800
*          05  AIDPFK07    PIC  X(01).                                * 00029900
*     *                                PF7 KEY                        * 00030000
*          05  AIDPFK08    PIC  X(01).                                * 00030100
*     *                                PF8 KEY                        * 00030200
*          05  AIDPFK09    PIC  X(01).                                * 00030300
*     *                                PF9 KEY                        * 00030400
*          05  AIDPFK10    PIC  X(01).                                * 00030500
*     *                                PF10 KEY                       * 00030600
*          05  AIDPFK11    PIC  X(01).                                * 00030700
*     *                                PF11 KEY                       * 00030800
*          05  AIDPFK12    PIC  X(01).                                * 00030900
*     *                                PF12 KEY                       * 00031000
*          05  AIDPFK13    PIC  X(01).                                * 00031100
*     *                                PF13 KEY                       * 00031200
*          05  AIDPFK14    PIC  X(01).                                * 00031300
*     *                                PF14 KEY                       * 00031400
*          05  AIDPFK15    PIC  X(01).                                * 00031500
*     *                                PF15 KEY                       * 00031600
*          05  AIDPFK16    PIC  X(01).                                * 00031700
*     *                                PF16 KEY                       * 00031800
*          05  AIDPFK17    PIC  X(01).                                * 00031900
*     *                                PF17 KEY                       * 00032000
*          05  AIDPFK18    PIC  X(01).                                * 00032100
*     *                                PF18 KEY                       * 00032200
*          05  AIDPFK19    PIC  X(01).                                * 00032300
*     *                                PF19 KEY                       * 00032400
*          05  AIDPFK20    PIC  X(01).                                * 00032500
*     *                                PF20 KEY                       * 00032600
*          05  AIDPFK21    PIC  X(01).                                * 00032700
*     *                                PF21 KEY                       * 00032800
*          05  AIDPFK22    PIC  X(01).                                * 00032900
*     *                                PF22 KEY                       * 00033000
*          05  AIDPFK23    PIC  X(01).                                * 00033100
*     *                                PF23 KEY                       * 00033200
*          05  AIDPFK24    PIC  X(01).                                * 00033300
*     *                                PF24 KEY                       * 00033400
*        02  AIDVALUE-TBL  REDEFINES   AIDVALUE-LIT.                  * 00033500
*          05  AIDVALUE    PIC  X(01)  OCCURS 31                      * 00033600
*                                      INDEXED BY AIDVALUE-IDX.       * 00033700
*     *                                                               * 00033800
*     * COPYBOOK LENGTH VALIDATION FIELD                              * 00033900
*     *                                                               * 00034000
*        02  IBMLDOK       PIC  X(02).                                * 00034100
*          88  IBM3270-LOADED-OK       VALUE 'OK'.                    * 00034200
*     *                                                               * 00033800
*        02  FLDCURSR      PIC  X(01).                                * 00022200
*     *                                CURSOR IN FIELD ("RECEIVE MAP")* 00022300
*        02  FLDMODCU      PIC  X(01).                                * 00022200
*     *                                CURSOR IN MODIFIED FIELD       * 00022300
*                                                                     * 00034300
* =================================================================== * 00034400
*                                                                     * 00034500
*                             INVOKATION                              * 00034600
*                                                                     * 00034700
*      In a CICS  environment,  the  table  is  loaded into the COBOL * 00034800
* LINKAGE SECTION as follows:                                         * 00034900
*                                                                     * 00035000
*     * for DOS/VS COBOL:                                             * 00035100
*                                                                     * 00035200
*          EXEC CICS LOAD                                             * 00035300
*                    PROGRAM('IBM3270')                               * 00035400
*                    SET(IBM3270P)                                    * 00035500
*                    NOHANDLE                                         * 00035600
*                    END-EXEC.                                        * 00035700
*          SERVICE  RELOAD   IBM3270.                                 * 00035800
*          IF EIBRESP NOT = NORMAL                                    * 00035900
*          OR NOT IBM3270-LOADED-OK                                   * 00036000
*              EXEC CICS ABEND                                        * 00036100
*                        ABCODE('3270')                               * 00036200
*                        CANCEL                                       * 00036300
*                        END-EXEC                                     * 00036400
*          ELSE NEXT SENTENCE.                                        * 00036500
*                                                                     * 00036600
*     * for VSE/COBOL-II:                                             * 00036700
*                                                                     * 00036800
*          EXEC CICS LOAD                                             * 00036900
*                    PROGRAM('IBM3270')                               * 00037000
*                    SET(ADDRESS OF IBM3270)                          * 00037100
*                    NOHANDLE                                         * 00037200
*                    END-EXEC.                                        * 00037300
*          IF EIBRESP NOT = NORMAL                                    * 00037400
*          OR NOT IBM3270-LOADED-OK                                   * 00037500
*              EXEC CICS ABEND                                        * 00037600
*                        ABCODE('3270')                               * 00037700
*                        CANCEL                                       * 00037800
*                        END-EXEC                                     * 00037900
*          END-IF.                                                    * 00038000
*                                                                     * 00038100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00038200
         EJECT                                                          00038300
IBM3270  CSECT                                                          00038400
         SPACE 3                                                        00038500
* ******************************************************************* * 00038600
*        LOCAL AND REMOTE COMMAND CODES                               * 00038700
* ******************************************************************* * 00038800
         SPACE 1                                                        00038900
LCLNOOP  DC    X'03'         LOCAL  CC=(NOOP)                           00039000
LCLERAUP DC    X'0F'         LOCAL  CC=(ERASE,AUP)                      00039100
LCLERWRA DC    X'0D'         LOCAL  CC=(ERASE,WRITE,ALTERNATE)          00039200
LCLERWRT DC    X'05'         LOCAL  CC=(ERASE,WRITE)                    00039300
LCLRDBUF DC    X'02'         LOCAL  CC=(READ,BUFFER)                    00039400
LCLRDMDF DC    X'06'         LOCAL  CC=(READ,MODIFIED)                  00039500
LCLWRITE DC    X'01'         LOCAL  CC=(WRITE)                          00039600
LCLWRSTR DC    X'11'         LOCAL  CC=(WRITE,STRUCTURED)               00039700
RMTCOPY  DC    X'F7'         REMOTE CC=(COPY)                           00039800
RMTERAUP DC    X'6F'         REMOTE CC=(ERASE,AUP)                      00039900
RMTERWRA DC    X'7E'         REMOTE CC=(ERASE,WRITE,ALTERNATE)          00040000
RMTERWRT DC    X'F5'         REMOTE CC=(ERASE,WRITE)                    00040100
RMTRDBUF DC    X'F2'         REMOTE CC=(READ,BUFFER)                    00040200
RMTRDMDF DC    X'F6'         REMOTE CC=(READ,MODIFIED)                  00040300
RMTWRITE DC    X'F1'         REMOTE CC=(WRITE)                          00040400
RMTWRSTR DC    X'F3'         REMOTE CC=(WRITE,STRUCTURED)               00040500
         SPACE 1                                                        00040600
* ******************************************************************* * 00040700
         SPACE 3                                                        00040800
* ******************************************************************* * 00040900
*        WRITE CONTROL CHARACTERS                                     * 00041000
* ******************************************************************* * 00041100
         SPACE 1                                                        00041200
*              B'XX......'   SET/RESET TO FORM AN "EBCDIC" GRAPHIC CHAR 00041300
*              B'..00....'   USE  "NLEOM"  TO DETERMINE LINE LENGTH     00041400
*              B'..01....'   PRINT 40 CHARACTERS PER LINE               00041500
*              B'..10....'   PRINT 64 CHARACTERS PER LINE               00041600
*              B'..11....'   PRINT 80 CHARACTERS PER LINE               00041700
*              B'....0...'   OMIT "PRINT"  WRITE CONTROL                00041800
*              B'....1...'   USE  "PRINT"  WRITE CONTROL                00041900
*              B'.....0..'   OMIT "ALARM"  WRITE CONTROL                00042000
*              B'.....1..'   USE  "ALARM"  WRITE CONTROL                00042100
*              B'......0.'   OMIT "FREEKB" WRITE CONTROL  ("LEAVEKB")   00042200
*              B'......1.'   USE  "FREEKB" WRITE CONTROL                00042300
*              B'.......0'   OMIT "FRSET"  WRITE CONTROL  ("FSET")      00042400
*              B'.......1'   USE  "FRSET"  WRITE CONTROL                00042500
         SPACE 1                                                        00042600
WRTLKFST DC    B'01000000'   X'40' C' ' CTRL=(LEAVEKB,FSET)             00042700
WRTLKFRS DC    B'11000001'   X'C1' C'A' CTRL=(LEAVEKB,FRSET)            00042800
WRTFKFST DC    B'11000010'   X'C2' C'B' CTRL=(FREEKB,FSET)              00042900
WRTFKFRS DC    B'11000011'   X'C3' C'C' CTRL=(FREEKB,FRSET)             00043000
WRTLKFSA DC    B'11000100'   X'C4' C'D' CTRL=(LEAVEKB,FSET,ALARM)       00043100
WRTLKFRA DC    B'11000101'   X'C5' C'E' CTRL=(LEAVEKB,FRSET,ALARM)      00043200
WRTFKFSA DC    B'11000110'   X'C6' C'F' CTRL=(FREEKB,FSET,ALARM)        00043300
WRTFKFRA DC    B'11000111'   X'C7' C'G' CTRL=(FREEKB,FRSET,ALARM)       00043400
WRTPR132 DC    B'11001000'   X'C8' C'H' CTRL=(PRINT,HONEOM)             00043500
WRTPA132 DC    B'01001100'   X'4C' C'<' CTRL=(PRINT,HONEOM,ALARM)       00043600
WRTPRL40 DC    B'11011000'   X'D8' C'Q' CTRL=(PRINT,L40)                00043700
WRTPAL40 DC    B'01011100'   X'5C' C'*' CTRL=(PRINT,L40,ALARM)          00043800
WRTPRL64 DC    B'11101000'   X'E8' C'Y' CTRL=(PRINT,L64)                00043900
WRTPAL64 DC    B'01101100'   X'6C' C'%' CTRL=(PRINT,L64,ALARM)          00044000
WRTPRL80 DC    B'11111000'   X'F8' C'8' CTRL=(PRINT,L80)                00044100
WRTPAL80 DC    B'01111100'   X'7C' C'@' CTRL=(PRINT,L80,ALARM)          00044200
         SPACE 1                                                        00044300
* ******************************************************************* * 00044400
         SPACE 3                                                        00044500
* ******************************************************************* * 00044600
*        SCREEN BUFFER ORDERS                                         * 00044700
* ******************************************************************* * 00044800
         SPACE 1                                                        00044900
SCRPGTAB DC    X'05'         PROGRAM TAB                                00045000
SCRGRESC DC    X'08'         GRAPHIC ESCAPE + 1-BYTE CHARACTER          00045100
SCRSETBA DC    X'11'         SET BUFFER ADDRESS + 2-BYTE ADDRESS        00045200
SCRERASE DC    X'12'         ERASE UNPROTECTED + 2-BYTE ADDRESS         00045300
SCRCURSR DC    X'13'         INSERT CURSOR                              00045400
SCRSTFLD DC    X'1D'         START FIELD + 1-BYTE ATTRIBUTE             00045500
SCRSETAT DC    X'28'         SET ATTRIBUTE + 2-BYTE TYPE-VALUE          00045600
SCRSTFEX DC    X'29'         START FIELD EXTENDED + COUNT + TYPE-VALUE  00045700
SCRMODFY DC    X'2C'         MODIFY FIELD + COUNT + 2-BYTE TYPE-VALUE   00045800
SCREPEAT DC    X'3C'         REPEAT TO ADDRESS + 2-BYTE ADDRESS + CHAR  00045900
         SPACE 1                                                        00046000
* ******************************************************************* * 00046100
         SPACE 3                                                        00046200
* ******************************************************************* * 00046300
*        PRINTER CONTROL ORDERS                                       * 00046400
* ******************************************************************* * 00046500
         SPACE 1                                                        00046600
PRTEJECT DC    X'0C'         FORMS FEED                                 00046700
PRTRETRN DC    X'0D'         CARRIAGE RETURN                            00046800
PRTNEWLN DC    X'15'         NEW LINE (CR/LF)                           00046900
PRTENDOM DC    X'19'         END OF MESSAGE/MEDIUM (DSC ONLY)           00047000
PRTCOLOR DC    X'1D'         SELECT COLOR PRINT + 1-BYTE ATTRIBUTE      00047100
PRTLFEED DC    X'25'         LINE FEED (SCS ONLY)                       00047200
PRTBLACK DC    X'40'         C' ' COLOR=BLACK ATTRIBUTE                 00047300
PRTBLUE  DC    X'60'         C'-' COLOR=BLUE ATTRIBUTE                  00047400
PRTRED   DC    X'C8'         C'H' COLOR=RED ATTRIBUTE                   00047500
PRTGREEN DC    X'E8'         C'Y' COLOR=GREEN ATTRIBUTE                 00047600
         SPACE 1                                                        00047700
* ******************************************************************* * 00047800
         SPACE 3                                                        00047900
* ******************************************************************* * 00048000
*        STANDARD FIELD ATTRIBUTES                                    * 00048100
* ******************************************************************* * 00048200
         SPACE 1                                                        00048300
*              B'XX......'   SET/RESET TO FORM AN "EBCDIC" GRAPHIC CHAR 00048400
*              B'..0.....'   FIELD HAS THE "UNPROT" ATTRIBUTE           00048500
*              B'..1.....'   FIELD HAS THE "PROT" ATTRIBUTE             00048600
*              B'...0....'   FIELD IS DEFINED AS ALPHANUMERIC           00048700
*              B'...1....'   FIELD HAS THE "NUM" DEFINITION             00048800
*              B'..11....'   FIELD HAS THE "ASKIP" FEATURE              00048900
*              B'....00..'   FIELD IS "NORM" IN INTENSITY               00049000
*              B'....01..'   FIELD IS "NORM" & HAS THE "DET" ATTRIBUTE  00049100
*              B'....10..'   FIELD IS "BRT" (IMPLICIT "DET" ATTRIBUTE)  00049200
*              B'....11..'   FIELD IS "DRK" IN INTENSITY                00049300
*              B'......0.'   RESERVED.  MUST ALWAYS BE ZERO.            00049400
*              B'.......0'   FIELD'S "MDT" IS RESET                     00049500
*              B'.......1'   FIELD HAS THE "FSET" ATTRIBUTE             00049600
         SPACE 1                                                        00049700
FLDUNPRO DC    B'01000000'   X'40' C' ' ATTRB=(UNPROT,NORM)             00049800
FLDUNFST DC    B'11000001'   X'C1' C'A' ATTRB=(UNPROT,NORM,FSET)        00049900
FLDUNDET DC    B'11000100'   X'C4' C'D' ATTRB=(UNPROT,NORM,DET)         00050000
FLDUNDTF DC    B'11000101'   X'C5' C'E' ATTRB=(UNPROT,NORM,DET,FSET)    00050100
FLDUNBRT DC    B'11001000'   X'C8' C'H' ATTRB=(UNPROT,BRT)              00050200
FLDUNBRF DC    B'11001001'   X'C9' C'I' ATTRB=(UNPROT,BRT,FSET)         00050300
FLDUNDRK DC    B'01001100'   X'4C' C'<' ATTRB=(UNPROT,DRK)              00050400
FLDUNDKF DC    B'01001101'   X'4D' C'(' ATTRB=(UNPROT,DRK,FSET)         00050500
FLDUNNUM DC    B'01010000'   X'50' C'&' ATTRB=(UNPROT,NORM,NUM)         00050600
FLDUNNMF DC    B'11010001'   X'D1' C'J' ATTRB=(UNPROT,NORM,NUM,FSET)    00050700
FLDUNNMD DC    B'11010100'   X'D4' C'M' ATTRB=(UNPROT,NORM,NUM,DET)     00050800
FLDUNNDF DC    B'11010101'   X'D5' C'N' ATTRB=(UNPROT,NORM,NUM,DET,FSET 00050900
FLDUNBRN DC    B'11011000'   X'D8' C'Q' ATTRB=(UNPROT,BRT,NUM)          00051000
FLDUNBNF DC    B'11011001'   X'D9' C'R' ATTRB=(UNPROT,BRT,NUM,FSET)     00051100
FLDUNDKN DC    B'01011100'   X'5C' C'*' ATTRB=(UNPROT,DRK,NUM)          00051200
FLDUNDNF DC    B'01011101'   X'5D' C')' ATTRB=(UNPROT,DRK,NUM,FSET)     00051300
FLDPROT  DC    B'01100000'   X'60' C'-' ATTRB=(PROT,NORM)               00051400
FLDPRFST DC    B'01100001'   X'61' C'/' ATTRB=(PROT,NORM,FSET)          00051500
FLDPRDET DC    B'11100100'   X'E4' C'U' ATTRB=(PROT,NORM,DET)           00051600
FLDPRDTF DC    B'11100101'   X'E5' C'V' ATTRB=(PROT,NORM,DET,FSET)      00051700
FLDPRBRT DC    B'11101000'   X'E8' C'Y' ATTRB=(PROT,BRT)                00051800
FLDPRBRF DC    B'11101001'   X'E9' C'Z' ATTRB=(PROT,BRT,FSET)           00051900
FLDPRDRK DC    B'01101100'   X'6C' C'%' ATTRB=(PROT,DRK)                00052000
FLDPRDKF DC    B'01101101'   X'6D' C'_' ATTRB=(PROT,DRK,FSET)           00052100
FLDASKIP DC    B'11110000'   X'F0' C'0' ATTRB=(ASKIP,NORM)              00052200
FLDASFST DC    B'11110001'   X'F1' C'1' ATTRB=(ASKIP,NORM,FSET)         00052300
FLDASDET DC    B'11110100'   X'F4' C'4' ATTRB=(ASKIP,NORM,DET)          00052400
FLDASDTF DC    B'11110101'   X'F5' C'5' ATTRB=(ASKIP,NORM,DET,FSET)     00052500
FLDASBRT DC    B'11111000'   X'F8' C'8' ATTRB=(ASKIP,BRT)               00052600
FLDASBRF DC    B'11111001'   X'F9' C'9' ATTRB=(ASKIP,BRT,FSET)          00052700
FLDASDRK DC    B'01111100'   X'7C' C'@' ATTRB=(ASKIP,DRK)               00052800
FLDASDKF DC    B'01111101'   X'7D' C''' ATTRB=(ASKIP,DRK,FSET)          00052900
         SPACE 1                                                        00053000
FLDMODFY DC    B'10000000'   X'80' FIELD MODIFIED (ON "RECEIVE")        00053100
         SPACE 1                                                        00053200
* ******************************************************************* * 00053300
         SPACE 3                                                        00053400
* ******************************************************************* * 00053500
*        EXTENDED FIELD ATTRIBUTES                                    * 00053600
* ******************************************************************* * 00053700
         SPACE 1                                                        00053800
TYPRESET DC    X'00'         "TYPE" RESET                               00053900
TYPHILIT DC    X'41'         "TYPE" HILIGHT                             00054000
TYPCOLOR DC    X'42'         "TYPE" COLOR                               00054100
TYPPSYMB DC    X'43'         "TYPE" PROGRAMMED SYMBOLS                  00054200
TYPFIELD DC    X'C0'         "TYPE" FIELD ATTRIBUTE                     00054300
         SPACE 1                                                        00054400
EXHDEFLT DC    X'00'         HILITE=DEFAULT                             00054500
EXHBLINK DC    X'F1'         HILITE=BLINK                               00054600
EXHREVRS DC    X'F2'         HILITE=REVERSE                             00054700
         DC    X'F4'         HILITE=?                                   00054800
EXHULINE DC    X'F4'         HILITE=UNDERLINE                           00054900
         SPACE 1                                                        00055000
EXCDEFLT DC    X'00'         COLOR=DEFAULT (GREEN)                      00055100
EXCBLUE  DC    X'F1'         COLOR=BLUE                                 00055200
EXCRED   DC    X'F2'         COLOR=RED                                  00055300
EXCPINK  DC    X'F3'         COLOR=PINK                                 00055400
EXCGREEN DC    X'F4'         COLOR=GREEN                                00055500
EXCTURQU DC    X'F5'         COLOR=TURQUOISE                            00055600
EXCYELLW DC    X'F6'         COLOR=YELLOW                               00055700
EXCNEUTR DC    X'F7'         COLOR=NEUTRAL (WHITE)                      00055800
         SPACE 1                                                        00055900
* ******************************************************************* * 00056000
         SPACE 3                                                        00056100
* ******************************************************************* * 00056200
*        ATTENTION IDENTIFICATION VALUES                              * 00056300
* ******************************************************************* * 00056400
         SPACE 1                                                        00056500
AIDNOAID DC    X'60'         C'-' NO AID GENERATED (NO DATA TO READ)    00056600
AIDCLEAR DC    X'6D'         C'_' CLEAR KEY                             00056600
AIDENTER DC    X'7D'         C''' ENTER KEY                             00056700
AIDCURSR DC    X'7E'         C'=' CURSOR SELECT KEY (OR LIGHT PEN)      00056800
AIDPAK01 DC    X'6C'         C'%' PA1 KEY                               00056900
AIDPAK02 DC    X'6E'         C'>' PA2 KEY                               00057000
AIDPAK03 DC    X'6B'         C',' PA3 KEY                               00057100
AIDPFK01 DC    X'F1'         C'1' PF1 KEY                               00057200
AIDPFK02 DC    X'F2'         C'2' PF2 KEY                               00057300
AIDPFK03 DC    X'F3'         C'3' PF3 KEY                               00057400
AIDPFK04 DC    X'F4'         C'4' PF4 KEY                               00057500
AIDPFK05 DC    X'F5'         C'5' PF5 KEY                               00057600
AIDPFK06 DC    X'F6'         C'6' PF6 KEY                               00057700
AIDPFK07 DC    X'F7'         C'7' PF7 KEY                               00057800
AIDPFK08 DC    X'F8'         C'8' PF8 KEY                               00057900
AIDPFK09 DC    X'F9'         C'9' PF9 KEY                               00058000
AIDPFK10 DC    X'7A'         C':' PF10 KEY                              00058100
AIDPFK11 DC    X'7B'         C'#' PF11 KEY                              00058200
AIDPFK12 DC    X'7C'         C'@' PF12 KEY                              00058300
AIDPFK13 DC    X'C1'         C'A' PF13 KEY                              00058400
AIDPFK14 DC    X'C2'         C'B' PF14 KEY                              00058500
AIDPFK15 DC    X'C3'         C'C' PF15 KEY                              00058600
AIDPFK16 DC    X'C4'         C'D' PF16 KEY                              00058700
AIDPFK17 DC    X'C5'         C'E' PF17 KEY                              00058800
AIDPFK18 DC    X'C6'         C'F' PF18 KEY                              00058900
AIDPFK19 DC    X'C7'         C'G' PF19 KEY                              00059000
AIDPFK20 DC    X'C8'         C'H' PF20 KEY                              00059100
AIDPFK21 DC    X'C9'         C'I' PF21 KEY                              00059200
AIDPFK22 DC    X'4A'         C'¢' PF22 KEY                              00059300
AIDPFK23 DC    X'4B'         C'.' PF23 KEY                              00059400
AIDPFK24 DC    X'4C'         C'<' PF24 KEY                              00059500
         SPACE 1                                                        00059600
* ******************************************************************* * 00059700
         SPACE 3                                                        00059800
* ******************************************************************* * 00059900
*        LITERAL TO CHECK DATA ALIGNMENT AGAINST                      * 00060000
* ******************************************************************* * 00060100
         SPACE 1                                                        00060200
IBMLDOK  DC    C'OK'                                                    00060300
         SPACE 1                                                        00060200
FLDCURSR DC    B'00000010'   X'02' CURSOR IN FIELD (ON "RECEIVE")       00053100
FLDCURSM DC    B'10000010'   X'82' CURSOR IN FIELD W/ MDT=ON            00053100
         SPACE 1                                                        00060400
DUPKEY   DC    X'1C'         3270 DUPLICATE KEY                         00056600
FLDMRK   DC    X'1E'         3270 FIELD MARK KEY                        00056600
ESCAPE   DC    X'27'         EBCDIC ESCAPE CHARACTER                    00056600
         SPACE 1                                                        00060400
* ******************************************************************* * 00060500
         SPACE 3                                                        00060600
         END                                                            00060700
