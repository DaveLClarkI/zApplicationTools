VSM      TITLE 'RXVSAMXR - RXVSAMIO -- EXTRACT RECORD FIELDS'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* E X T R A C T   R E C O R D   F I E L D S   f o r   R X V S A M I O *
*                                                                     *
*                                                                     *
* No warranty is expressed or implied.       Written by Dave L Clark  *
* Neither the author nor the company is      Phone: (937)751-3300     *
* responsible for any loss or damage         Winsupply Group Services *
* resulting from the use of this code.       3110 Kettering Blvd.     *
* If provided, source on as-is basis.        Dayton, OH  45439        *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This subroutine is intended for use with the RXVSAMIO external *
* assembler REXX function.   That program provides all of the dynamic *
* storage and REXX and VSAM control blocks that this subroutine uses. *
* See that program for more information.                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
FILETBL  DSECT                         FILE TABLE ENTRY
         USING *,R9                    PERMANENT ADDRESSABILITY
* ------------------------------------------------------------------- *
FILETBLE DS    0CL32                   FILE TABLE ENTRY
FILEDD   DS    CL7
FILEOPT  DS    XL1                     OPTION FLAGS
FILECLUS DF    B'10000000'             DEFINED AS CLUSTER
FILEAIX  DF    B'01000000'             DEFINED AS AIX
FILEPATH DF    B'00100000'             DEFINED AS PATH
FILESEQU DF    B'00000100'             ACCESS IS SEQUENTIAL VS. DIRECT
FILEAPND DF    B'00000010'             OPENED FOR APPEND
FILEUPDT DF    B'00000001'             OPENED FOR UPDATE
FILECAT  DS    F                       MY CAT ACB ADDRESS
FILECATR DS    F                       MY CAT RPL ADDRESS
FILEACB  DS    F                       MY ACB BASE ADDRESS
FILERPL  DS    F                       MY RPL BASE ADDRESS
FILETYPE DS    XL1                     ATTRIBUTE FLAGS
ESDS     DF    B'01111000'             ENTRY-SEQUENCED (REVERSED)
KSDS     DF    B'01000000'             KEY-SEQUENCED
RRDS     DF    B'00001000'             RELATIVE-RECORD
VRDS     DF    B'01001000'             VARIABLE-RELATIVE
SAMD     DF    B'00010000'             MSAM CIFORMAT DATASET
SAME     DF    B'00010000'             MSAM-EXPLICIT DEFINE
SAMI     DF    B'10010000'             MSAM-IMPLICIT DEFINE
SAMN     DF    B'00100000'             MSAM-NOCIFORMAT
SAMB     DF    B'00000100'             MSAM-BLOCKED
SAMV     DF    B'00000010'             MSAM-VARIABLE
SAMF     DF    B'00000001'             MSAM-FIXED
FILEDCI  DS    XL3                     CATALOG DATA COMP. CI NUMBER
         DS    XL1
FILEICI  DS    XL3                     CATALOG INDEX COMP. CI NUMBER
* =================================================================== *
         EJECT
* =================================================================== *
DATAAREA DSECT                         PASSED DATA AREA
         USING *,R10                   PERMANENT ADDRESSABILITY
* ------------------------------------------------------------------- *
AWORK4   DS    3A                      SAVE ADDRESSES
SAVE_R0  DS    A                       SAVE REGISTER
SAVE_R5  DS    A                       SAVE REGISTER
SAVE_RE  DS    A                       SAVE REGISTER
SAVE_RF  DS    A                       SAVE REGISTER
EPVECTOR DS    A                       REXX ENTRY-POINT VECTOR ADDRESS
EVALBLKA DS    A                       REXX EVALUATION BLOCK ADDRESS
*
DYNATBL  DS    9XL(L'FILETBLE)         BEGINNING OF FILE TABLE
DYNATBLE DS    XL8                     END OF FILE TABLE
*
PWORK16  DS    0PL16                   DOUBLE PACKED WORK FIELD
PWORK8   DS    PL8                     SINGLE PACKED WORK FIELD
         DS    XL8                     RESERVED
EWORK    DS    CL34                    EDITED WORK FIELD
CWORK    DS    CL34                    CHARACTER WORK FIELD
*
SAYPARMB DS    3F                      ARXSAY PARAMETER BLOCK
SAYPLAST EQU   *-4                       POINT TO LAST PARM FOR FLAG
SAYPARM1 DS    CL8                     'WRITE   '
SAYPARM2 DS    XL4                     ADDRESS OF SAY OUTPUT
SAYPARM3 DS    XL4                     LENGTH OF SAY OUTPUT
*
SAVEENVB DS    F                       ENVIRONMENT BLOCK ADDRESS
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPLAST EQU   *-4                       POINT TO LAST PARM FOR FLAG
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
DDNLENG  DS    F                       STEM NAME LENGTH
DDNAME   DS    CL8                     BASE STEM NAME
REQUEST  DS    CL8                     ACCESS REQUEST
RECNAMEL DS    F                       STEM NAME LENGTH
RECNAME  DS    CL10                    RECORD NAME STEM
RQARG2   DS    CL8                     REQUEST ARGUMENT 2
RQARG3   DS    CL8                     REQUEST ARGUMENT 3
*
MACRO    DS    CL8                     LAST FAILED MACRO (IF ANY)
OPTFLAG  DS    XL1                     OPTION FLAGS
ACBERRF  EQU   B'00001000'             GET ACB ERROR INFO
RPLERRF  EQU   B'00000100'             GET RPL ERROR INFO
CTLBLKF  EQU   B'00000010'             GET CTLBLK ERROR INFO
SET_KLZ  EQU   B'00000001'             KEEP LEADING ZEROES
CINUM    DS    XL3                     CATALOG CI NUMBER
         DS    0F
VARDATA  DS    CL256                   VARIABLE VALUE
VARNAME  DS    CL256                   VARIABLE NAME
SHOWAREA DS    XL512                   SHOWCAT/SHOWCB RESULT AREA
PARMLIST DS    XL256                   GENCB/MODCB PARMLIST AREA
*
         COPY  DTEMANA
*
         DS    0D                      ALIGN TRANSLATE TABLE
TRTBL    DS    CL256                   TEMPORARY TRANSLATE TABLE
         DS    0D                      ALIGN RECORD KEY
KEYBUF   DS    CL255                   STORAGE FOR RECORD KEY AREA
         DS    0D                      ALIGN RECORD AREA
RECBUF   DS    CL65535                 STORAGE FOR RECORD I/O AREA
* =================================================================== *
         EJECT
* =================================================================== *
*        CONTROL BLOCKS
* ------------------------------------------------------------------- *
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
R0       EQU   0                       WORK REGISTER
R1       EQU   1                       ON ENTRY, CALLER'S PARM LIST
R2       EQU   2                       WORK REGISTER
R3       EQU   3                       WORK REGISTER
R4       EQU   4                       WORK REGISTER
R5       EQU   5                       WORK REGISTER
R6       EQU   6                       WORK REGISTER
R7       EQU   7                       WORK REGISTER
R8       EQU   8                       WORK REGISTER
R9       EQU   9                       POINTER TO FILE TBL ENTRY
R10      EQU   10                      POINTER TO DYNAMIC STORAGE
R11      EQU   11                      RXVSAMVL CSECT BASE REG #2
R12      EQU   12                      RXVSAMVL CSECT BASE REG
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN POINT REG
R15      EQU   15                      ENTRY POINT REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MY MACROS                                                    *
* ------------------------------------------------------------------- *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
RXVSAMXR HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)FEB22',            X
               BASE=(R12,R11),SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         LM    R9,R10,0(R1)            LOAD CALLER'S PARMS              00013500
         LA    R8,SHVCTLB              STORAGE FOR
         USING SHVBLOCK,R8              SHARED VARIABLES CONTROL BLOCK
*
         LA    R1,RECFLD               POINT TO STEM ELEMENT NAME
         LHI   R2,L'RECFLD             SET LENGTH OF ELEMENT NAME
         LA    R3,=C'0'                POINT TO STEM ELEMENT VALUE
         LHI   R4,1                    SET LENGTH OF ELEMENT VALUE
         BAS   R5,SET_NAME             CREATE A STEM ELEMENT NAME
         BAS   R5,GETVAR               RETRIEVE STEM ELEMENT ENTRY

         LA    R1,VARDATA              POINT TO ZONED ELEMENT COUNT
         L     R2,SHVVALL              GET LENGTH OF VAR DATA
         BAS   R5,ZONE2REG             CONVERT TO BINARY IN REG2
         STH   R2,EMAX                  AND SAVE AS HALFWORD VALUE
         XR    R7,R7                   INTIALIZE ELEMENT COUNTER

         WHILE R7,(LT,CH),EMAX         LOOP ON FIELD DEFINITIONS
          AHI  R7,1                     INCREMENT ELEMENT COUNTER
          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LGFR R2,R7                    GET CURRENT ELEMENT NUMBER
          XR   R4,R4                    ZERO DECIMAL PLACES
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,CWORK                 POINT BACK TO ELEMENT NUMBER
          LR   R4,R2                    SET LENGTH OF ELEMENT NUMBER
          LA   R1,RECFLD                POINT TO STEM ELEMENT NAME
          LHI  R2,L'RECFLD              SET LENGTH OF ELEMENT NAME
          BAS  R5,SET_NAME              CREATE A STEM ELEMENT NAME
          BAS  R5,GETVAR                RETRIEVE STEM ELEMENT ENTRY
          IF   SHVVALL,(NH,CLC),=F'0',OR,  IF VAL LENGTH NOT > ZERO    +
               SHVVALL,(NL,CLC),=F'65'     OR NOT < 65
           MVC MACRO,=CL8'GETVAR'        LAST FAILED MACRO
           LR  R5,R7                     RETURN ELEMENT NUMBER
           LHI R15,-7                    SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          BAS  R5,PARSE_FLDDEF          PARSE THE FIELD DEFINITION

          LA   R1,FLD_NAME              POINT BACK TO FIELD NAME
          LA   R2,FLD_NAME+L'FLD_NAME-1 POINT TO LAST BYTE OF NAME
          BAS  R5,SET_LENG              SET LENGTH IN R2
          LR   R3,R1                    MOVE NAME ADDRESS TO R3
          LR   R4,R2                    MOVE NAME LENGTH TO R4
          LA   R1,RECFLD                POINT BACK TO ELEM NAME
          LHI  R2,L'RECFLD              SET LENGTH OF ELEM NAME
          BAS  R5,SET_NAME              CREATE A STEM ELEMENT NAME

          IF   FLD_LENG,(LT,CLC),=F'1',OR,  MIN CHAR LENGTH SUPPORTED  +
               FLD_LENG,(HT,CLC),=F'256'    MAX CHAR LENGTH SUPPORTED
           MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
           LR  R5,R7                     RETURN ELEMENT NUMBER
           LHI R15,-7                    SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF

          L    R5,FLD_OFFS              INDEX TO VAR DATA
          LA   R1,RECBUF(R5)            POINT BACK TO VAR DATA
          L    R2,FLD_LENG              SET LENGTH OF VAR DATA
          IF   FLD_TYPE,NE,C'B'         IF NOT BINARY TYPE
          AND  FLD_TYPE,NE,C'U'         AND NOT UBIN TYPE
           SHI R2,1                      DECREMENT BEFORE EXECUTE
           LA  R3,LOVAL                  CHECK FOR LOW VALUES
           IF  R2,(EQ,EX),CHECK_IT       IF LOW-VALUES
            LA R1,=C'*NULL'               PASS BACK KEYWORD
            LHI R2,5                      WITH KEYWORD LENGTH
            BAS R5,SET_ELEM               CREATE STEM ELEMENT ENTRY
            WLOOP                         NEXT FIELD ENTRY
CHECK_IT CLC   0(0,R1),0(R3)           (SEE "EX" INSTR. ABOVE&&BELOW)
           ENDIF                         ENDIF
           LA  R3,HIVAL                  CHECK FOR HIGH VALUES
           IF  R2,(EQ,EX),CHECK_IT       IF HIGH-VALUES
            LA R1,=C'*HIGH'               PASS BACK KEYWORD
            LHI R2,5                      WITH KEYWORD LENGTH
            BAS R5,SET_ELEM               CREATE STEM ELEMENT ENTRY
            WLOOP                         NEXT FIELD ENTRY
           ENDIF                         ENDIF
           AHI R2,1                      INCREMENT AFTER EXECUTE
          ENDIF                         ENDIF

          SELECT
          WHEN FLD_TYPE,EQ,C'C'         "CHAR"
           IF  FLD_LENG,(LT,CLC),=F'1',OR,  MIN CHAR LENGTH SUPPORTED  +
               FLD_LENG,(HT,CLC),=F'256'    MAX CHAR LENGTH SUPPORTED
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF

          WHEN FLD_TYPE,EQ,C'P'         "PACKED"
           IF  FLD_LENG,(LT,CLC),=F'1',OR,  MIN PACK LENGTH SUPPORTED  +
               FLD_LENG,(HT,CLC),=F'16'     MAX PACK LENGTH SUPPORTED
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF
           SHI R2,1                      DECREMENT BEFORE EXECUTE
           SLL R2,4                      SHIFT LEFT FOR TEST-PACKED
           IF  R2,(NZ,EX),TEST_IT        IF NOT VALID PACKED DATA
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
TEST_IT  TP    0(0,R1)                 (SEE EXECUTE INSTRUCTION ABOVE)
ZAP_IT   ZAP   PWORK16,0(0,R1)         (SEE EXECUTE INSTRUCTION BELOW)
           ENDIF
           SRL R2,4                      SHIFT RIGHT FOR ZAP-PACKED
           EX  R2,ZAP_IT                 MOVE VAR DATA TO WORK FIELD
           LA  R1,CWORK                  POINT TO "RESULT" AREA
           CVBG R2,PWORK16               CONVERT TO 64-BIT BINARY
           L   R0,FLD_LENG               SET LENGTH OF VAR DATA
           L   R4,FLD_DECM               GET NO. OF DECIMAL PLACES
           BAS R5,REG2ZONE               CONVERT TO ZONED WITH DECIMAL

          WHEN FLD_TYPE,EQ,C'I'         "IZONED"
           IF  FLD_LENG,(LT,CLC),=F'1',OR,  MIN ZONE LENGTH SUPPORTED  +
               FLD_LENG,(HT,CLC),=F'31'     MAX ZONE LENGTH SUPPORTED
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF
           LAY R3,-1(R2,R1)              POINT TO LAST BYTE OF DATA
           IF  0(R3),NL,X'A0'            IF VALID
            SHI R2,1                      DECREMENT BEFORE EXECUTE
            EX  R2,PACK_IT                PACK UP TO 31 BYTES
            CVBG R2,PWORK16               CONVERT TO 64-BIT BINARY
            L  R0,FLD_LENG                SET LENGTH OF VAR DATA
            LA R1,CWORK                   POINT TO "RESULT" AREA
            L  R4,FLD_DECM                GET NO. OF DECIMAL PLACES
            OI OPTFLAG,SET_KLZ            KEEP LEADING ZEROS
            BAS R5,REG2ZONE               BACK TO ZONED WITH DECIMAL
            NI OPTFLAG,X'FF'-SET_KLZ      RESET TO DEFAULT
           ENDIF                         ENDIF

          WHEN FLD_TYPE,EQ,C'Z'         "ZONED"
           IF  FLD_LENG,(LT,CLC),=F'1',OR,  MIN ZONE LENGTH SUPPORTED  +
               FLD_LENG,(HT,CLC),=F'31'     MAX ZONE LENGTH SUPPORTED
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
PACK_IT  PKA   PWORK16,0(0,R1)         (SEE "EX" INSTR. ABOVE && BELOW)
           ENDIF
           LAY R3,-1(R2,R1)              POINT TO LAST BYTE OF DATA
           IF  0(R3),NL,X'A0'            IF VALID
            SHI R2,1                      DECREMENT BEFORE EXECUTE
            EX  R2,PACK_IT                PACK UP TO 31 BYTES
            CVBG R2,PWORK16               CONVERT TO 64-BIT BINARY
            IF 0(R3),(NO,TM),X'F0'        IF WAS NEGATIVE
            AND 0(R3),(ON,TM),X'10'
             MGHI R2,-1                    NEGATE THE 64-BIT NUMBER
            ENDIF
            L  R0,FLD_LENG                SET LENGTH OF VAR DATA
            LA R1,CWORK                   POINT TO "RESULT" AREA
            L  R4,FLD_DECM                GET NO. OF DECIMAL PLACES
            BAS R5,REG2ZONE               BACK TO ZONED WITH DECIMAL
           ENDIF

          WHEN FLD_TYPE,EQ,C'B'         "BINARY"
           IF  FLD_LENG,(NE,CLC),=F'2'    HALF-WORD
           AND FLD_LENG,(NE,CLC),=F'4'    FULL-WORD
           AND FLD_LENG,(NE,CLC),=F'8'    DOUBLE-WORD
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF
           L   R0,FLD_LENG                SET LENGTH OF VAR DATA
           LA  R1,CWORK                   POINT TO "RESULT" AREA
           IF  FLD_LENG,(EQ,CLC),=F'8'    IF DOUBLE-WORD
            LG R2,RECBUF(R5)               LOAD DOUBLE-WORD SIGNED
           ELSE                           ELSE
            IF FLD_LENG,(EQ,CLC),=F'4'     IF FULL-WORD
             LGF R2,RECBUF(R5)              LOAD FULL-WORD SIGNED
            ELSE                           ELSE
             LGH R2,RECBUF(R5)              LOAD HALF-WORD SIGNED
            ENDIF                          ENDIF
           ENDIF                          ENDIF
           L   R4,FLD_DECM                GET NO. OF DECIMAL PLACES
           BAS R5,REG2ZONE                CONVERT TO ZONED-DECIMAL

          WHEN FLD_TYPE,EQ,C'U'         "UBIN"
           IF  FLD_LENG,(NE,CLC),=F'1'    BYTE
           AND FLD_LENG,(NE,CLC),=F'2'    HALF-WORD
           AND FLD_LENG,(NE,CLC),=F'4'    FULL-WORD
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF
           L   R0,FLD_LENG                SET LENGTH OF VAR DATA
           LA  R1,CWORK                   POINT TO "RESULT" AREA
           IF  FLD_LENG,(EQ,CLC),=F'4'    IF FULL-WORD
            LLGF R2,RECBUF(R5)             LOAD FULL-WORD UNSIGNED
           ELSE                           ELSE
            IF FLD_LENG,(EQ,CLC),=F'2'     IF HALF-WORD
             LLGH R2,RECBUF(R5)             LOAD HALF-WORD UNSIGNED
            ELSE                           ELSE
             LLGC R2,RECBUF(R5)             LOAD CHARACTER UNSIGNED
            ENDIF                          ENDIF
           ENDIF                          ENDIF
           L   R4,FLD_DECM                GET NO. OF DECIMAL PLACES
           BAS R5,REG2ZONE                CONVERT TO ZONED-DECIMAL

          WHEN FLD_TYPE,EQ,C'H'         "HEX"
           IF  FLD_LENG,(LT,CLC),=F'1',OR,  MIN HEX LENGTH SUPPORTED   +
               FLD_LENG,(HT,CLC),=F'128'    MAX HEX LENGTH SUPPORTED
            MVC MACRO,=CL8'CONVERT'       LAST FAILED OPERATION
            LR  R5,R7                     RETURN ELEMENT NUMBER
            LHI R15,-7                    SET RETURN CODE
            B   RETERR                    GO RETURN ERROR
           ENDIF
           LA  R1,SHOWAREA               DECODE INTO WORK AREA
           XR  R2,R2                      STARTING LENGTH IS ZERO
           L   R5,FLD_OFFS               INDEX TO VAR DATA
           LA  R3,RECBUF(R5)             POINT BACK TO VAR DATA
           L   R4,FLD_LENG               SET LENGTH OF VAR DATA
           WHILE R4,(NZ,LTR),R4          LOOP ON CHARACTER LENGTH       00015600
            UNPK 0(3,R1),0(2,R3)          UNPACK CHAR INTO HEX AREA     00015700
            TR   0(2,R1),TRTBL            X'FAFBFCFDFEFF' TO C'ABCDEF'  00015800
            AHI  R1,2                     ADVANCE TO NEXT HEX ENTRY     00015900
            AHI  R2,2                     INCREMENT HEX LENGTH          00016100
            AHI  R3,1                     ADVANCE TO NEXT CHAR ENTRY    00016000
            SHI  R4,1                     DECREMENT CHAR LENGTH         00016100
           WEND                                                         00016200
           LA  R1,SHOWAREA               POINT BACK TO RESULTS

          WHEN OTHER                    NOT SUPPORTED
           MVC MACRO,=CL8'PARSE'         LAST FAILED OPERATION
           LR  R5,R7                     RETURN ELEMENT NUMBER
           LHI R15,-7                    SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDSL

          BAS  R5,SET_ELEM              CREATE THE STEM ELEMENT ENTRY
         WEND                          LOOP ON FIELD DEFINITIONS
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
R15ZERO  XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   DS    0H
         ST    R5,SAVE_R5              SAVE REGISTER 5
         ST    R15,SAVE_RF             SAVE REGISTER 15
*
         LA    R1,VARDATA              POINT TO VARIABLE DATA AREA
         LGF   R2,SAVE_R5              COPY R5 TO R2 AS 64 BITS
         XR    R4,R4                   ZERO DECIMAL PLACES
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
         MVC   VARNAME+0(4),=C'SRC '   MOVE IN THE VARIABLE NAME
         LHI   R4,3                    SET LENGTH OF VARIABLE NAME
         ST    R4,SHVNAML              PUT IN SHV CONTROL BLOCK
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         L     R15,SAVE_RF             RETURN R15 AS RETURN CODE
* ------------------------------------------------------------------- *
         TRAILER RETCODE=R15,SVAREA=SAVEAREA
* =================================================================== *
         EJECT
* =================================================================== *
PARSE_FLDDEF DS 0H           PARSE FIELD DEFINITION IN UP TO 5 PIECES
         ST    R5,AWORK4               SAVE RETURN ADDRESS
* ------------------------------------------------------------------- *
         MVI   FLD_NAME,X'40'          INTIALIZE 5 PIECES OF DEFINITION
         MVC   FLD_NAME+1(L'FLD_NAME-1),FLD_NAME
         MVC   FLD_TYPE,SPACES           2
         XC    FLD_OFFS,FLD_OFFS         3
         XC    FLD_LENG,FLD_LENG         4
         XC    FLD_DECM,FLD_DECM         5
*
         LA    R3,VARDATA              POINT TO BEG. OF FIELD DEF.
         L     R4,SHVVALL              SET TOTAL LENGTH OF FIELD DEF.
         LAY   R2,-1(,R4)              DECREMENT BEFORE EXECUTE
         EX    R2,UCASE                TRANSLATE TO UPPER-CASE
*
         LR    R1,R3                   SAVE START OF NEXT PIECE
         XR    R2,R2                   INITIALIZE LENGTH COUNTER
         LA    R5,FLD_NAME             POINT TO NEXT PIECE TO LOAD
         WHILE R4,(NZ,LTR),R4,AND,     LOOP UNTIL END OF DEFINITION    +
               0(R3),HT,X'40'          OR A SPACE IS FOUND
          AHI  R2,1                     INCREMENTING LENGTH COUNTER
          AHI  R3,1                     AND INCREMENTING ADDRESS
          SHI  R4,1                     AND DECREMENTING TOTAL LEN.
         WEND
         IF    R2,(Z,LTR),R2,OR,       FIELD NAME IS REQUIRED          +
               R2,(HT,C),=F'32'        OR IF TOO LONG...
          MVC  MACRO,=CL8'PARSE1_X'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
UCASE    TR    VARDATA(0),TRTBL        (SEE "EX" STATEMENT ABOVE)
         ENDIF
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,PARSE_MVC            SAVE FIELD NAME FROM DEFINITION
*
         WHILE R4,(NZ,LTR),R4,AND,     FIND THE BEG. OF NEXT PIECE     +
               0(R3),EQ,X'40'
          AHI  R3,1                     INCREMENTING ADDRESS
          SHI  R4,1                     DECREMENTING TOTAL LEN.
         WEND
         IF    R4,(Z,LTR),R4           IF END OF FIELD DEFINITION
          MVC  MACRO,=CL8'PARSE2_N'     LAST FAILED MACRO
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
*
         LR    R1,R3                   SAVE START OF NEXT PIECE
         XR    R2,R2                   INITIALIZE LENGTH COUNTER
         LA    R5,FLD_TYPE             POINT TO NEXT PIECE TO LOAD
         WHILE R4,(NZ,LTR),R4,AND,     LOOP UNTIL END OF DEFINITION    +
               0(R3),HT,X'40'          OR A SPACE IS FOUND
          AHI  R2,1                     INCREMENTING LENGTH COUNTER
          AHI  R3,1                     AND INCREMENTING ADDRESS
          SHI  R4,1                     AND DECREMENTING TOTAL LEN.
         WEND
         IF    R2,(Z,LTR),R2,OR,       FIELD TYPE IS REQUIRED          +
               R2,(HT,C),=F'6'         OR IF TOO LONG...
          MVC  MACRO,=CL8'PARSE2_X'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,PARSE_MVC            SAVE FIELD TYPE FROM DEFINITION
*
         WHILE R4,(NZ,LTR),R4,AND,     FIND THE BEG. OF NEXT PIECE     +
               0(R3),EQ,X'40'
          AHI  R3,1                     INCREMENTING ADDRESS
          SHI  R4,1                     DECREMENTING TOTAL LEN.
         WEND
         IF    R4,(Z,LTR),R4           IF END OF FIELD DEFINITION
          MVC  MACRO,=CL8'PARSE3_N'     LAST FAILED MACRO
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
*
         LR    R1,R3                   SAVE START OF NEXT PIECE
         XR    R2,R2                   INITIALIZE LENGTH COUNTER
         WHILE R4,(NZ,LTR),R4,AND,     LOOP UNTIL END OF DEFINITION    +
               0(R3),HT,X'40'          OR A SPACE IS FOUND
          AHI  R2,1                     INCREMENTING LENGTH COUNTER
          AHI  R3,1                     AND INCREMENTING ADDRESS
          SHI  R4,1                     AND DECREMENTING TOTAL LEN.
         WEND
         IF    R2,(Z,LTR),R2,OR,       FIELD OFFSET IS REQUIRED        +
               R2,(HT,C),=F'3'         OR IF MORE THAN 3 DIGITS
          MVC  MACRO,=CL8'PARSE3_X'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         BAS   R5,ZONE2REG             CONVERT FLD OFFSET INTO REG2
         IF    R2,(LT,C),=F'0',OR,     IF FIELD OFFSET < ZERO          +
               R2,(HT,C),=F'32767'     OR HIGHER THAN FLD MAX...
          MVC  MACRO,=CL8'PARSE3_Y'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         ST    R2,FLD_OFFS             SAVE FLD OFFSET FROM DEFINITION
*
         WHILE R4,(NZ,LTR),R4,AND,     FIND THE BEG. OF NEXT PIECE     +
               0(R3),EQ,X'40'
          AHI  R3,1                     INCREMENTING ADDRESS
          SHI  R4,1                     DECREMENTING TOTAL LEN.
         WEND
         IF    R4,(Z,LTR),R4           IF END OF FIELD DEFINITION
          MVC  MACRO,=CL8'PARSE4_N'     LAST FAILED MACRO
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
*
         LR    R1,R3                   SAVE START OF NEXT PIECE
         XR    R2,R2                   INITIALIZE LENGTH COUNTER
         LA    R5,FLD_LENG             POINT TO NEXT PIECE TO LOAD
         WHILE R4,(NZ,LTR),R4,AND,     LOOP UNTIL END OF DEFINITION    +
               0(R3),HT,X'40'          OR A SPACE IS FOUND
          AHI  R2,1                     INCREMENTING LENGTH COUNTER
          AHI  R3,1                     AND INCREMENTING ADDRESS
          SHI  R4,1                     AND DECREMENTING TOTAL LEN.
         WEND
         IF    R2,(Z,LTR),R2,OR,       FIELD LENGTH IS REQUIRED        +
               R2,(HT,C),=F'3'         OR IF MORE THAN 3 DIGITS
          MVC  MACRO,=CL8'PARSE4_X'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         BAS   R5,ZONE2REG             CONVERT FLD LENGTH INTO REG2
         IF    R2,(LT,C),=F'1',OR,     IF FIELD LENGTH < ONE           +
               R2,(HT,C),=F'256'       OR HIGHER THAN FLD MAX...
          MVC  MACRO,=CL8'PARSE4_Y'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         ST    R2,FLD_LENG             SAVE FLD LENGTH FROM DEFINITION
*
         WHILE R4,(NZ,LTR),R4,AND,     FIND THE BEG. OF NEXT PIECE     +
               0(R3),EQ,X'40'
          AHI  R3,1                     INCREMENTING ADDRESS
          SHI  R4,1                     DECREMENTING TOTAL LEN.
         WEND
         IF    R4,(Z,LTR),R4           IF END OF FIELD DEFINITION
          IF   FLD_TYPE,EQ,C'C',OR,     IF CHAR OR HEX                 +
               FLD_TYPE,EQ,C'H'         DECIMALS NOT ALLOWED
           B   PARSE_EXIT                IGNORE NO DECIMALS
          ENDIF                         ENDIF
          MVC  MACRO,=CL8'PARSE5_N'     LAST FAILED MACRO
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         IF    FLD_TYPE,EQ,C'C',OR,    IF CHAR OR HEX                  +
               FLD_TYPE,EQ,C'H'        DECIMALS NOT ALLOWED
          MVC  MACRO,=CL8'PARSE5_E'     LAST FAILED MACRO
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
*
         LR    R1,R3                   SAVE START OF NEXT PIECE
         XR    R2,R2                   INITIALIZE LENGTH COUNTER
         WHILE R4,(NZ,LTR),R4,AND,     LOOP UNTIL END OF DEFINITION    +
               0(R3),HT,X'40'          OR A SPACE IS FOUND
          AHI  R2,1                     INCREMENTING LENGTH COUNTER
          AHI  R3,1                     AND INCREMENTING ADDRESS
          SHI  R4,1                     AND DECREMENTING TOTAL LEN.
         WEND
         IF    R2,(Z,LTR),R2,OR,       FIELD DECIMALS EXPECTED         +
               R2,(HT,C),=F'2'         OR IF MORE THAN 2 DIGITS
          MVC  MACRO,=CL8'PARSE5_X'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         BAS   R5,ZONE2REG             CONVERT FLD DECIMALS INTO REG2
         IF    R2,(NZ,LTR),R2          IF FLD DECIMALS NON-ZERO
          IF   FLD_TYPE,NE,C'P'         ONLY PACKED AND ZONED
          AND  FLD_TYPE,NE,C'Z'         CAN HAVE NON-ZERO DECIMALS
           MVC MACRO,=CL8'PARSE5_Z'      LAST FAILED MACRO
           LR  R5,R7                     RETURN ELEMENT NUMBER
           LHI R15,-7                    SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF
         ENDIF
         IF    R2,(LT,C),=F'0',OR,     IF FIELD DECIMALS < ZERO        +
               R2,(HT,C),=F'15'        OR HIGHER THAN DECIMALS MAX
          MVC  MACRO,=CL8'PARSE5_Y'     LAST FAILED OPERATION
          LR   R5,R7                    RETURN ELEMENT NUMBER
          LHI  R15,-7                   SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF
         ST    R2,FLD_DECM             SAVE FLD DECIMAL FROM DEFINITION
* ------------------------------------------------------------------- *
PARSE_EXIT DS  0H
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
PARSE_MVC MVC  0(0,R5),0(R1)           SEE EXECUTE INSTRUCTIONS ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZONE DS    0H            CONVERT 64-BIT REG2 INTO A LEFT-SIGNED,
*                                               ZONED-DECIMAL AT REG1
*                                               INPUT  LENGTH IN REG0
*                                               RETURN LENGTH IN REG2
*                                               WORK REGISTER IS REG3
*                                              DECIMAL PLACES IN REG4
* ------------------------------------------------------------------- *
         LR    R3,R1                   SAVE R1 IN R3
         CVDG  R2,PWORK16              CONVERT REG2 TO PACKED-DECIMAL
         IF    R4,(Z,LTR),R4           IF ZERO DECIMAL PLACES
          MVC  EWORK,MASK0              USE NO DECIMAL PLACES MASK
          LA   R2,EWORK+L'MASK0-2       POINT TO SIGN BYTE (& IGNORE)
          LR   R1,R2                    SET UP FOR SIGNIFICANCE
         ELSE                          ELSE
          MVC  EWORK,MASK1              USE DECIMAL PLACES MASK
          LA   R2,EWORK+L'MASK1-1       POINT TO SIGN BYTE (& IGNORE)
          LA   R1,EWORK+L'MASK1-2       POINT TO LAST DIGIT OF MASK
          SR   R1,R4                    SUBTRACT NO. OF DECIMAL PLACES
          MVI  0(R1),X'4B'              PLACE DECIMAL POINT MASK
         ENDIF                         ENDIF
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          MVI  EWORK,X'F0'              CHANGE FILL BYTE
          IF   R4,(NZ,LTR),R4           IF ZERO DECIMAL PLACES
           LA  R1,EWORK+L'MASK1-2        POINT TO LAST DIGIT OF MASK
          ENDIF
          SR   R1,R0                    POSITION OF FIRST ZERO TO KEEP
          SHI  R1,1                     SET POSITION OF SIGNIFICANCE
         ELSE                          ELSE
          SHI  R1,2                     SET POSITION OF SIGNIFICANCE
         ENDIF                         ENDIF
         MVI   0(R1),X'21'             PLACE SIGNIFICANCE MASK
         AHI   R1,1                    NEXT BYTE (DEFAULT START POINT)
         IF    EWORK,(M,EDMK),PWORK16  IF NEGATIVE NUMBER
          SHI  R1,1                     DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'               FLOAT A LEFT MINUS SIGN
         ENDIF
         SR    R2,R1                   CALC NO. OF CHARS TO MOVE
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,MOVE_RESULT          MOVE VALUE TO RESULT AREA
         AHI   R2,1                    INCREMENT AFTER EXECUTE
         LR    R1,R3                   RESTORE R1 FROM R3
* ------------------------------------------------------------------- *
         BR    R5
MOVE_RESULT MVC 0(0,R3),0(R1)          SEE EXECUTE INSTRUCTION ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
ZONE2REG DS    0H    CONVERT ZONED-DECIMAL AT REG1 (L'REG2) INTO REG2
* ------------------------------------------------------------------- *
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,PACK_RESULT          PACK 0(?,R1) FOR LENGTH OF R2
         CVB   R2,PWORK8               CONVERT PACKED-DECIMAL INTO REG2
* ------------------------------------------------------------------- *
         BR    R5
PACK_RESULT PACK PWORK8,0(0,R1)        SEE EXECUTE INSTRUCTION ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
SET_ELEM DS    0H            CREATE A STEM ELEMENT ENTRY
         ST    R5,AWORK4               SAVE RETURN ADDRESS
* ------------------------------------------------------------------- *
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,ELEMDATA_MVC         MOVE DATA TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT AFTER EXECUTE
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
         BAS   R5,SETVAR               CREATE THE VARIABLE
* ------------------------------------------------------------------- *
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
ELEMDATA_MVC MVC VARDATA(0),0(R1)      SEE FIRST EXECUTE JUST ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
SET_LENG DS    0H    STRIP END OF R1 DATA AREA && RETURN R2 LENGTH
* ------------------------------------------------------------------- *
         IF    0(R1),NH,X'40'          IF FIRST CHAR IS BLANK
          LHI  R2,0                     SET ZERO LENGTH
         ELSE                          ELSE
          WHILE 0(R2),NH,X'40'          STRIP OFF NON-DISPLAY
           SHI R2,1                     ONE AT A TIME
          WEND
          SR   R2,R1                    COMPUTE LENGTH OF DATA
          AHI  R2,1                     ADJUST FOR ZERO OFFSET
         ENDIF                         ENDIF
* ------------------------------------------------------------------- *
         BR    R5                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
SET_NAME DS    0H            CREATE THE NAME FOR A STEM ELEMENT
         ST    R5,AWORK4               SAVE RETURN ADDRESS
* ------------------------------------------------------------------- *
         LA    R5,VARNAME              POINT TO VAR NAME BUFFER
         XR    R6,R6                   STARTING WITH ZERO LENGTH
*
         MVC   0(L'RECNAME,R5),RECNAME MOVE IN STEM ROOT
         A     R5,RECNAMEL             INCR. VAR NAME ADDR.
         A     R6,RECNAMEL             INCR. VAR NAME LENGTH
         MVI   0(R5),C'.'              ADD A PERIOD
         AHI   R5,1                    INCR. VAR NAME ADDR.
         AHI   R6,1                    INCR. VAR NAME LENGTH
*
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,ELEMNAME_MVC         MOVE IN ELEMENT NAME
         AHI   R2,1                    INCREMENT AFTER EXECUTE
         AR    R5,R2                   INCR. VAR NAME ADDR.
         AR    R6,R2                   INCR. VAR NAME LENGTH
         MVI   0(R5),C'.'              ADD A PERIOD
         AHI   R5,1                    INCR. VAR NAME ADDR.
         AHI   R6,1                    INCR. VAR NAME LENGTH
*
         LR    R1,R3                   TRANSFER NAME ADDRESS
         LR    R2,R4                   TRANSFER NAME LENGTH
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,ELEMNAME_MVC         MOVE NAME TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT AFTER EXECUTE
         AR    R6,R2                   INCR. VAR NAME LENGTH
*
         ST    R6,SHVNAML              PUT IN SHV CONTROL BLOCK
* ------------------------------------------------------------------- *
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
ELEMNAME_MVC MVC 0(0,R5),0(R1)         SEE EXECUTE INSTRUCTIONS ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H            GET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         L     R0,SAVEENVB             PASS ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          MVC  MACRO,=CL8'ARXEXCOM'     LAST FAILED MACRO
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           LLC R5,SHVRET                 SAVE ARXEXCOM CC
          ELSE                          ELSE
           LR  R5,R15                    COPY TO R5
          ENDIF                         ENDIF
          LHI  R15,8                    SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
* ------------------------------------------------------------------- *
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H            SET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         L     R0,SAVEENVB             PASS ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   VARNAME(3),(NE,CLC),=C'SRC '  IF NOT 'SRC', REPORT ERR
          AND  SHVRET,NE,SHVNEWV        AND NOT NEW VARIABLE
           MVC MACRO,=CL8'ARXEXCOM'      LAST FAILED MACRO
           IF  R15,(Z,LTR),R15           IF ZERO RETURN CODE
            LLC R5,SHVRET                 SAVE ARXEXCOM CC
           ELSE                          ELSE
            LR R5,R15                     COPY TO R5
           ENDIF                         ENDIF
           LHI R15,8                     SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF
* ------------------------------------------------------------------- *
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    18D                     STANDARD OR F4SA REG. SVAEAREA
SPACES   DC    CL10' '
EMAX     DS    H                       MAXIMUM NO. OF ELEMENTS
*
FLD_NAME DS    CL32
FLD_TYPE DS    CL6
FLD_OFFS DS    F
FLD_LENG DS    F
FLD_DECM DS    F
*
MASK0    DC    x'402020202020202020202020202020202020202020202020202020-
               20202020206040'
MASK1    DC    x'402020202020202020202020202020202020202020202020202020-
               20202020202060'
*
LOVAL    DC    256X'00'
HIVAL    DC    256X'FF'
*
         LTORG
*
* ******************************************************************* * 00053500
*        STEM EXTENSION NAMES TO CREATE                               * 00053600
* ******************************************************************* * 00053700
RECFLD   DC    C'_FLD'                 record field definitions
* =================================================================== *
         END   RXVSAMXR
