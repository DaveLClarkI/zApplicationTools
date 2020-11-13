      *
      * IBM 3270 IDS - DATA STREAM CONTROL CHARACTERS
      *
       01  IBM3270.
      *
      * LOCAL AND REMOTE COMMAND CODES
      *
         02  CMDCODES.
           05  LCLNOOP     PIC  X(01).
      *                                LOCAL NO OPERATION
           05  LCLERAUP    PIC  X(01).
      *                                LOCAL ERASE ALL UNPROTECTED
           05  LCLERWRA    PIC  X(01).
      *                                LOCAL ERASE/WRITE ALTERNATE
           05  LCLERWRT    PIC  X(01).
      *                                LOCAL ERASE/WRITE
           05  LCLRDBUF    PIC  X(01).
      *                                LOCAL READ BUFFER
           05  LCLRDMDF    PIC  X(01).
      *                                LOCAL READ MODIFIED
           05  LCLWRITE    PIC  X(01).
      *                                LOCAL WRITE
           05  LCLWRSTR    PIC  X(01).
      *                                LOCAL WRITE STRUCTURED
           05  RMTCOPY     PIC  X(01).
      *                                REMOTE COPY
           05  RMTERAUP    PIC  X(01).
      *                                REMOTE ERASE ALL UNPROTECTED
           05  RMTERWRA    PIC  X(01).
      *                                REMOTE ERASE/WRITE ALTERNATE
           05  RMTERWRT    PIC  X(01).
      *                                REMOTE ERASE/WRITE
           05  RMTRDBUF    PIC  X(01).
      *                                REMOTE READ BUFFER
           05  RMTRDMDF    PIC  X(01).
      *                                REMOTE READ MODIFIED
           05  RMTWRITE    PIC  X(01).
      *                                REMOTE WRITE
           05  RMTWRSTR    PIC  X(01).
      *                                REMOTE WRITE STRUCTURED
      *
      * WRITE CONTROL CHARACTERS
      *
         02  WRTCHARS.
           05  WRTLKFST    PIC  X(01).
      *                                CTRL=(LEAVEKB,FSET)
           05  WRTLKFRS    PIC  X(01).
      *                                CTRL=(LEAVEKB,FRSET)
           05  WRTFKFST    PIC  X(01).
      *                                CTRL=(FREEKB,FSET)
           05  WRTFKFRS    PIC  X(01).
      *                                CTRL=(FREEKB,FRSET)
           05  WRTLKFSA    PIC  X(01).
      *                                CTRL=(LEAVEKB,FSET,ALARM)
           05  WRTLKFRA    PIC  X(01).
      *                                CTRL=(LEAVEKB,FRSET,ALARM)
           05  WRTFKFSA    PIC  X(01).
      *                                CTRL=(FREEKB,FSET,ALARM)
           05  WRTFKFRA    PIC  X(01).
      *                                CTRL=(FREEKB,FRSET,ALARM)
           05  WRTPR132    PIC  X(01).
      *                                CTRL=(PRINT,HONEOM)
           05  WRTPA132    PIC  X(01).
      *                                CTRL=(PRINT,HONEOM,ALARM)
           05  WRTPRL40    PIC  X(01).
      *                                CTRL=(PRINT,L40)
           05  WRTPAL40    PIC  X(01).
      *                                CTRL=(PRINT,L40,ALARM)
           05  WRTPRL64    PIC  X(01).
      *                                CTRL=(PRINT,L64)
           05  WRTPAL64    PIC  X(01).
      *                                CTRL=(PRINT,L64,ALARM)
           05  WRTPRL80    PIC  X(01).
      *                                CTRL=(PRINT,L80)
           05  WRTPAL80    PIC  X(01).
      *                                CTRL=(PRINT,L80,ALARM)
      *
      * SCREEN BUFFER CONTROL ORDERS
      *
         02  SCRORDER.
           05  SCRPGTAB    PIC  X(01).
      *                                PROGRAM TAB
           05  SCRGRESC    PIC  X(01).
      *                                GRAPHIC ESCAPE + 1-BYTE CHAR
           05  SCRSETBA    PIC  X(01).
      *                                SET BUFFER ADDRESS + 2-BYTE ADR
           05  SCRERASE    PIC  X(01).
      *                                ERASE UNPROTECTED + 2-BYTE ADDR
           05  SCRCURSR    PIC  X(01).
      *                                INSERT CURSOR
           05  SCRSTFLD    PIC  X(01).
      *                                START FIELD + 1-BYTE ATTRIBUTE
           05  SCRSETAT    PIC  X(01).
      *                                SET ATTRIBUTE + 2-BYTE TYPE-VAL
           05  SCRSTFEX    PIC  X(01).
      *                                START FIELD EXT + CNT + TYP-VAL
           05  SCRMODFY    PIC  X(01).
      *                                MODIFY FIELD + COUNT + TYPE-VAL
           05  SCREPEAT    PIC  X(01).
      *                                REPEAT TO ADDR + ADDRESS + CHAR
      *
      * PRINTER CONTROL ORDERS
      *
         02  PRTORDER.
           05  PRTEJECT    PIC  X(01).
      *                                FORMS FEED
           05  PRTRETRN    PIC  X(01).
      *                                CARRIAGE RETURN
           05  PRTNEWLN    PIC  X(01).
      *                                NEW LINE (CR/LF)
           05  PRTENDOM    PIC  X(01).
      *                                END OF MESSAGE (DSC ONLY)
           05  PRTCOLOR    PIC  X(01).
      *                                SELECT COLOR + 1-BYTE VALUE
           05  PRTLFEED    PIC  X(01).
      *                                LINE FEED (SCS ONLY)
           05  PRTBLACK    PIC  X(01).
      *                                COLOR=BLACK ATTRIBUTE
           05  PRTBLUE     PIC  X(01).
      *                                COLOR=BLUE ATTRIBUTE
           05  PRTRED      PIC  X(01).
      *                                COLOR=RED ATTRIBUTE
           05  PRTGREEN    PIC  X(01).
      *                                COLOR=GREEN ATTRIBUTE
      *
      * STANDARD FIELD ATTRIBUTES
      *
         02  FLDATTRB-LIT.
           05  FLDUNPRO    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM)
           05  FLDUNFST    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,FSET)
           05  FLDUNDET    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,DET)
           05  FLDUNDTF    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,DET,FSET)
           05  FLDUNBRT    PIC  X(01).
      *                                ATTRB=(UNPROT,BRT)
           05  FLDUNBRF    PIC  X(01).
      *                                ATTRB=(UNPROT,BRT,FSET)
           05  FLDUNDRK    PIC  X(01).
      *                                ATTRB=(UNPROT,DRK)
           05  FLDUNDKF    PIC  X(01).
      *                                ATTRB=(UNPROT,DRK,FSET)
           05  FLDUNNUM    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,NUM)
           05  FLDUNNMF    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,NUM,FSET)
           05  FLDUNNMD    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,NUM,DET)
           05  FLDUNNDF    PIC  X(01).
      *                                ATTRB=(UNPROT,NORM,NUM,DET,FSET
           05  FLDUNBRN    PIC  X(01).
      *                                ATTRB=(UNPROT,BRT,NUM)
           05  FLDUNBNF    PIC  X(01).
      *                                ATTRB=(UNPROT,BRT,NUM,FSET)
           05  FLDUNDKN    PIC  X(01).
      *                                ATTRB=(UNPROT,DRK,NUM)
           05  FLDUNDNF    PIC  X(01).
      *                                ATTRB=(UNPROT,DRK,NUM,FSET)
           05  FLDPROT     PIC  X(01).
      *                                ATTRB=(PROT,NORM)
           05  FLDPRFST    PIC  X(01).
      *                                ATTRB=(PROT,NORM,FSET)
           05  FLDPRDET    PIC  X(01).
      *                                ATTRB=(PROT,NORM,DET)
           05  FLDPRDTF    PIC  X(01).
      *                                ATTRB=(PROT,NORM,DET,FSET)
           05  FLDPRBRT    PIC  X(01).
      *                                ATTRB=(PROT,BRT)
           05  FLDPRBRF    PIC  X(01).
      *                                ATTRB=(PROT,BRT,FSET)
           05  FLDPRDRK    PIC  X(01).
      *                                ATTRB=(PROT,DRK)
           05  FLDPRDKF    PIC  X(01).
      *                                ATTRB=(PROT,DRK,FSET)
           05  FLDASKIP    PIC  X(01).
      *                                ATTRB=(ASKIP,NORM)
           05  FLDASFST    PIC  X(01).
      *                                ATTRB=(ASKIP,NORM,FSET)
           05  FLDASDET    PIC  X(01).
      *                                ATTRB=(ASKIP,NORM,DET)
           05  FLDASDTF    PIC  X(01).
      *                                ATTRB=(ASKIP,NORM,DET,FSET)
           05  FLDASBRT    PIC  X(01).
      *                                ATTRB=(ASKIP,BRT)
           05  FLDASBRF    PIC  X(01).
      *                                ATTRB=(ASKIP,BRT,FSET)
           05  FLDASDRK    PIC  X(01).
      *                                ATTRB=(ASKIP,DRK)
           05  FLDASDKF    PIC  X(01).
      *                                ATTRB=(ASKIP,DRK,FSET)
         02  FLDATTRB-TBL  REDEFINES   FLDATTRB-LIT.
           05  FLDATTRB    PIC  X(01)  OCCURS 32
                                       INDEXED BY FLDATTRB-IDX.
      *
         02  FLDMODFY      PIC  X(01).
      *                                FIELD MODIFIED ("RECEIVE MAP")
      *
      * EXTENDED FIELD TYPES AND ATTRIBUTES
      *
         02  EXTATTRB.
           05  TYPRESET    PIC  X(01).
      *                                "TYPE" RESET
           05  TYPHILIT    PIC  X(01).
      *                                "TYPE" HILIGHT
           05  TYPCOLOR    PIC  X(01).
      *                                "TYPE" COLOR
           05  TYPPSYMB    PIC  X(01).
      *                                "TYPE" PROGRAMMED SYMBOLS
           05  TYPFIELD    PIC  X(01).
      *                                "TYPE" FIELD ATTRIBUTE
         02  EXHATTRB-LIT.
           05  EXHDEFLT    PIC  X(01).
      *                                HILITE=DEFAULT
           05  EXHBLINK    PIC  X(01).
      *                                HILITE=BLINK
           05  EXHREVRS    PIC  X(01).
      *                                HILITE=REVERSE
           05  FILLER      PIC  X(01).
      *                                HILITE=?
           05  EXHULINE    PIC  X(01).
      *                                HILITE=UNDERLINE
         02  EXHATTRB-TBL  REDEFINES   EXHATTRB-LIT.
           05  EXHATTRB    PIC  X(01)  OCCURS 5.
      *
         02  EXCATTRB-LIT.
           05  EXCDEFLT    PIC  X(01).
      *                                COLOR=DEFAULT (GREEN)
           05  EXCBLUE     PIC  X(01).
      *                                COLOR=BLUE
           05  EXCRED      PIC  X(01).
      *                                COLOR=RED
           05  EXCPINK     PIC  X(01).
      *                                COLOR=PINK
           05  EXCGREEN    PIC  X(01).
      *                                COLOR=GREEN
           05  EXCTURQU    PIC  X(01).
      *                                COLOR=TURQUOISE
           05  EXCYELLW    PIC  X(01).
      *                                COLOR=YELLOW
           05  EXCNEUTR    PIC  X(01).
      *                                COLOR=NEUTRAL (WHITE)
         02  EXCATTRB-TBL  REDEFINES   EXCATTRB-LIT.
           05  EXCATTRB    PIC  X(01)  OCCURS 8.
      *
      * ATTENTION IDENTIFICATION VALUES
      *
         02  AIDVALUE-LIT.
           05  AIDNOAID    PIC  X(01).
      *                                NO AID GENERATED (NO DATA)
           05  AIDCLEAR    PIC  X(01).
      *                                CLEAR KEY
           05  AIDENTER    PIC  X(01).
      *                                ENTER KEY
           05  AIDCURSR    PIC  X(01).
      *                                CURSOR SELECT KEY/LIGHT PEN
           05  AIDPAK01    PIC  X(01).
      *                                PA1 KEY
           05  AIDPAK02    PIC  X(01).
      *                                PA2 KEY
           05  AIDPAK03    PIC  X(01).
      *                                PA3 KEY
           05  AIDPFK01    PIC  X(01).
      *                                PF1 KEY
           05  AIDPFK02    PIC  X(01).
      *                                PF2 KEY
           05  AIDPFK03    PIC  X(01).
      *                                PF3 KEY
           05  AIDPFK04    PIC  X(01).
      *                                PF4 KEY
           05  AIDPFK05    PIC  X(01).
      *                                PF5 KEY
           05  AIDPFK06    PIC  X(01).
      *                                PF6 KEY
           05  AIDPFK07    PIC  X(01).
      *                                PF7 KEY
           05  AIDPFK08    PIC  X(01).
      *                                PF8 KEY
           05  AIDPFK09    PIC  X(01).
      *                                PF9 KEY
           05  AIDPFK10    PIC  X(01).
      *                                PF10 KEY
           05  AIDPFK11    PIC  X(01).
      *                                PF11 KEY
           05  AIDPFK12    PIC  X(01).
      *                                PF12 KEY
           05  AIDPFK13    PIC  X(01).
      *                                PF13 KEY
           05  AIDPFK14    PIC  X(01).
      *                                PF14 KEY
           05  AIDPFK15    PIC  X(01).
      *                                PF15 KEY
           05  AIDPFK16    PIC  X(01).
      *                                PF16 KEY
           05  AIDPFK17    PIC  X(01).
      *                                PF17 KEY
           05  AIDPFK18    PIC  X(01).
      *                                PF18 KEY
           05  AIDPFK19    PIC  X(01).
      *                                PF19 KEY
           05  AIDPFK20    PIC  X(01).
      *                                PF20 KEY
           05  AIDPFK21    PIC  X(01).
      *                                PF21 KEY
           05  AIDPFK22    PIC  X(01).
      *                                PF22 KEY
           05  AIDPFK23    PIC  X(01).
      *                                PF23 KEY
           05  AIDPFK24    PIC  X(01).
      *                                PF24 KEY
         02  AIDVALUE-TBL  REDEFINES   AIDVALUE-LIT.
           05  AIDVALUE    PIC  X(01)  OCCURS 31
                                       INDEXED BY AIDVALUE-IDX.
      *
      * COPYBOOK LENGTH VALIDATION FIELD
      *
         02  IBMLDOK       PIC  X(02).
           88  IBM3270-LOADED-OK       VALUE 'OK'.
      *                                                                 00033800
         02  FLDCURSR      PIC  X(01).                                  00022200
      *                                CURSOR IN FIELD ("RECEIVE MAP")  00022300
         02  FLDCURSM      PIC  X(01).                                  00022200
      *                                CURSOR IN FIELD W/ MDT=ON        00022300
         02  DUPKEY        PIC  X(01).                                  00022200
      *                                3270 DUPLICATE KEY               00022300
         02  FLDMRK        PIC  X(01).                                  00022200
      *                                3270 FIELD MARK KEY              00022300
         02  ESCAPE        PIC  X(01).                                  00022200
      *                                EBCDIC ESCAPE CHARACTER          00022300
