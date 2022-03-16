VSM      TITLE 'RXVSAMXA - RXVSAMIO -- EXTRACT DATASET ATTRIBUTES'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    D A T A S E T   A T T R I B U T E S   f o r   R X V S A M I O    *
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
FILECLUS EQU   B'10000000'             DEFINED AS CLUSTER
FILEAIX  EQU   B'01000000'             DEFINED AS AIX
FILEPATH EQU   B'00100000'             DEFINED AS PATH
FILESEQU EQU   B'00000100'             ACCESS IS SEQUENTIAL VS. DIRECT
FILEAPND EQU   B'00000010'             OPENED FOR APPEND
FILEUPDT EQU   B'00000001'             OPENED FOR UPDATE
FILECAT  DS    F                       MY CAT ACB ADDRESS
FILECATR DS    F                       MY CAT RPL ADDRESS
FILEACB  DS    F                       MY ACB BASE ADDRESS
FILERPL  DS    F                       MY RPL BASE ADDRESS
FILETYPE DS    XL1                     ATTRIBUTE FLAGS
ESDS     EQU   B'01111000'             ENTRY-SEQUENCED (REVERSED)
KSDS     EQU   B'01000000'             KEY-SEQUENCED
RRDS     EQU   B'00001000'             RELATIVE-RECORD
VRDS     EQU   B'01001000'             VARIABLE-RELATIVE
SAMD     EQU   B'00010000'             MSAM CIFORMAT DATASET
SAME     EQU   B'00010000'             MSAM-EXPLICIT DEFINE
SAMI     EQU   B'10010000'             MSAM-IMPLICIT DEFINE
SAMN     EQU   B'00100000'             MSAM-NOCIFORMAT
SAMB     EQU   B'00000100'             MSAM-BLOCKED
SAMV     EQU   B'00000010'             MSAM-VARIABLE
SAMF     EQU   B'00000001'             MSAM-FIXED
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
SAYPARM1 DS    CL8                     'WRITE   '
SAYPARM2 DS    XL4                     ADDRESS OF SAY OUTPUT
SAYPARM3 DS    XL4                     LENGTH OF SAY OUTPUT
*
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
DDNLENG  DS    F                       STEM NAME LENGTH
DDNAME   DS    CL8                     STEM NAME
REQUEST  DS    CL8                     ACCESS REQUEST
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
VARDATA  DS    CL256                   VARIABLE VALUE
VARNAME  DS    CL256                   VARIABLE NAME
SHOWAREA DS    XL512                   SHOWCAT/SHOWCB RESULT AREA
PARMLIST DS    XL256                   GENCB/MODCB PARMLIST AREA
*
DTEPLIST DS    A                       DTEMAN PLIST
DTEPARMS DS    0CL32                   DTEMAN PARMS
DTEREQU  DS    CL1                     REQUEST CODE
DTERETN  DS    CL1                     RETURN CODE
DTEGREG  DS    0CL10                   GREGORIAN DATE
DTEG_FIL DS    CL2                       00
DTEG_MC  DS    CL2                       CC
DTEG_YY  DS    CL2                       YY
DTEG_MM  DS    CL2                       MM
DTEG_DD  DS    CL2                       DD
DTEJULN  DS    0CL8                    JULIAN DATE
DTEJ_FIL DS    CL1                       0
DTEJ_MC  DS    CL2                       CC
DTEJ_YY  DS    CL2                       YY
DTEJ_DDD DS    CL3                       DDD
DTETOTD  DS    PL4                     TOTAL DAYS
DTEMNTH  DS    PL2                     MONTH NUMBER (01-12, beg:JAN)
DTEDAYN  DS    PL1                     DAY NUMBER (1-7, beg:SUNDAY)
DTELEAP  DS    CL1                     LEAP YEAR INDICATOR (Y/N)
DTEWORK  DS    PL4                     DTEMAN WORK AREA
*
         DS    0D                      ALIGN TRANSLATE TABLE
TRTBL    DS    CL256                   TEMPORARY TRANSLATE TABLE
         DS    0D                      ALIGN RECORD KEY
KEYAREA  DS    CL255                   STORAGE FOR RECORD KEY AREA
         ORG   KEYAREA
KEYBUF   DS    CL44                    STORAGE FOR RECORD KEY AREA
         ORG
         DS    0D                      ALIGN RECORD AREA
RECBUF   DS    CL504                   STORAGE FOR RECORD I/O AREA
* =================================================================== *
         EJECT
* =================================================================== *
*        CONTROL BLOCKS
* ------------------------------------------------------------------- *
         COPY  AMDSBDS                 VSAM DATA STATISTICS BLOCK
         IKQACB DSECT=YES              VSAM ACB DESCT
         IKQRPL DSECT=YES              VSAM RPL DESCT
*
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
RXVSAMXA HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)FEB22',            X
               BASE=(R12,R11),SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         LM    R9,R10,0(R1)            LOAD CALLER'S PARMS              00013500
*
         L     R8,FILECAT              GET CATALOG ACB ADDRESS
         USING IKQACB,R8               TEMPORARY ADDRESSABILITY
         LA    R1,ACBDDNM              POINT TO CATALOG LABEL
         DROP  R8                      REMOVE ADDRESSABILITY
         LA    R2,7-1(,R1)             POINT TO LAST BYTE OF CATLBL
         BAS   R5,SET_LENG             SET LENGTH IN R2
         LA    R3,CATLBL               POINT BACK TO VAR NAME
         LHI   R4,L'CATLBL             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R8,FILECAT              GET CATALOG ACB ADDRESS
         USING IKQACB,R8               TEMPORARY ADDRESSABILITY
         L     R1,ACBAMBL              POINT TO CATALOG AMBL
         LA    R1,152(R1)              POINT TO CATALOG NAME
         DROP  R8                      REMOVE ADDRESSABILITY
         LA    R2,44-1(,R1)            POINT TO LAST BYTE OF CATDSN
         BAS   R5,SET_LENG             SET LENGTH IN R2
         LA    R3,CATDSN               POINT BACK TO VAR NAME
         LHI   R4,L'CATDSN             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R8,FILEACB              GET FILE ACB ADDRESS
         USING IKQACB,R8               TEMPORARY ADDRESSABILITY
         L     R1,ACBAMBL              POINT TO DATASET AMBL
         LA    R1,152(R1)              POINT TO DATASET NAME
         DROP  R8                      REMOVE ADDRESSABILITY
         MVC   KEYBUF,0(R1)            SET KEY BUFFER TO DSN
         L     R8,FILECATR             GET CATALOG RPL ADDRESS
         BAS   R6,GETREC               GET THE CATALOG RECORD
         XC    KEYBUF,KEYBUF           SET KEY BUFFER TO LOW VALUES
         MVC   KEYBUF+1(3),RECBUF+44   POINT TO BASE CI
         BAS   R6,GETREC               GET THE BASE RECORD

         MVC   PWORK8(3),RECBUF+101    GET CREATION DATE
         MVC   CWORK(7),=X'F0202020212060' SET EDIT MASK
         ED    CWORK(7),PWORK8         EDIT THE DATE
         MVI   DTEREQU,C'J'            REQUEST JULIAN CONVERSION
         MVC   DTEJULN,SPACES          CLEAR JULIAN DATE FIELD
         MVC   DTEJULN(5),CWORK+1      SET JULIAN DATE FIELD
         LA    R1,DTEPLIST             GET ADDRESS OF PARM BLOCK
         L     R15,=V(DTEMAN)          GET ADDRESS OF SUBROUTINE
         BALR  R14,R15                 PERFORM CONVERSION/VALIDATION
         IF    DTERETN,NE,C' '         IF ERROR RETURNED
          MVC  MACRO,=CL8'DTEMAN'       INDICATE OPERATION PERFORMED
          XR   R15,R15                  CLEAR WORK REGISTER
          IC   R15,DTERETN              INSERT RETURN CODE CHARACTER
          LHI  R5,8                     SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
         MVC   CWORK(8),DTEGREG+2      GET GREGORIAN DATE
         LA    R1,CWORK                GET ADDRESS OF VAR DATA
         LHI   R2,8                    SET LENGTH OF VAR DATA
         LA    R3,DSCREA               POINT BACK TO VAR NAME
         LHI   R4,L'DSCREA             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    RECBUF+104(3),(HT,CP),=PL3'0'  IF NOT ZERO
          MVC  PWORK8(3),RECBUF+104     GET EXPIRATION DATE
         ENDIF
         MVC   CWORK(7),=X'F0202020212060' SET EDIT MASK
         ED    CWORK(7),PWORK8         EDIT THE DATE
         MVI   DTEREQU,C'J'            REQUEST JULIAN CONVERSION
         MVC   DTEJULN,SPACES          CLEAR JULIAN DATE FIELD
         MVC   DTEJULN(5),CWORK+1      SET JULIAN DATE FIELD
         LA    R1,DTEPLIST             GET ADDRESS OF PARM BLOCK
         L     R15,=V(DTEMAN)          GET ADDRESS OF SUBROUTINE
         BALR  R14,R15                 PERFORM CONVERSION/VALIDATION
         IF    DTERETN,NE,C' '         IF ERROR RETURNED
          MVC  MACRO,=CL8'DTEMAN'       INDICATE OPERATION PERFORMED
          XR   R15,R15                  CLEAR WORK REGISTER
          IC   R15,DTERETN              INSERT RETURN CODE CHARACTER
          LHI  R5,8                     SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
         MVC   CWORK(8),DTEGREG+2      GET GREGORIAN DATE
         LA    R1,CWORK                GET ADDRESS OF VAR DATA
         LHI   R2,8                    SET LENGTH OF VAR DATA
         LA    R3,DSEXPD               POINT BACK TO VAR NAME
         LHI   R4,L'DSEXPD             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    FILEOPT,(ON,TM),FILEAIX  FOR AN AIX...
          IF   RECBUF+107,(ON,TM),B'10000000'
           LA  R1,=C'1'                  INDICATE UPGRADE
          ELSE
           LA  R1,=C'0'                  INDICATE NOUPGRADE
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_UPGR                POINT BACK TO VAR NAME
          LHI  R4,L'A_UPGR              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF                         ENDIF

         IF    FILEOPT,(ON,TM),FILEPATH  FOR A PATH...
          IF   RECBUF+107,(ON,TM),B'10000000'
           LA  R1,=C'1'                  INDICATE UPDATE
          ELSE
           LA  R1,=C'0'                  INDICATE NOUPDATE
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_UPDT                POINT BACK TO VAR NAME
          LHI  R4,L'A_UPDT              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF                         ENDIF

         XC    KEYBUF,KEYBUF           SET KEY BUFFER TO LOW VALUES
         MVC   KEYBUF+1(3),FILEDCI     POINT TO DATA COMP. CI
         L     R8,FILECATR             GET CATALOG RPL ADDRESS
         BAS   R6,GETREC               GET THE CATALOG RECORD
         IF    RECBUF+1(3),(EQ,CLC),FILEDCI   IF RECORD WAS FOUND
          LA   R1,RECBUF+49             POINT BACK TO COMP. NAME
          LA   R2,RECBUF+49+44-1        POINT TO LAST BYTE OF NAME
          BAS  R5,SET_LENG              SET LENGTH IN R2
          LA   R3,DATANM                POINT BACK TO VAR NAME
          LHI  R4,L'DATANM              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          IF   RECBUF+107,(ON,TM),B'10000000'
           LA  R1,=C'0'                  INDICATE SPEED
          ELSE
           LA  R1,=C'1'                  INDICATE RECOVERY
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_RCVR                POINT BACK TO VAR NAME
          LHI  R4,L'A_RCVR              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          IF   RECBUF+107,(ON,TM),B'00100000'
           LA  R1,=C'1'                  INDICATE REUSE
          ELSE
           LA  R1,=C'0'                  INDICATE NOREUSE
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_REUS                POINT BACK TO VAR NAME
          LHI  R4,L'A_REUS              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          IF   RECBUF+107,(ON,TM),B'00010000'
           LA  R1,=C'1'                  INDICATE ERASE
          ELSE
           LA  R1,=C'0'                  INDICATE NOERASE
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_ERAS                POINT BACK TO VAR NAME
          LHI  R4,L'A_ERAS              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          MVC  CWORK(3),SPACES          WORK AREA FOR SHROPTNs
          ICM  R0,b'1000',RECBUF+108    NEED b'XXXX....' PART OF BYTE
          SRDL R0,30                    RIGHT-JUSTIFY 2 HI-ORDER BITS
          SRL  R1,30                    RIGHT-JUSTIFY 2 HI-ORDER BITS
          AHI  R0,C'1'                  YIELD '1' TO '4' VALUE
          STC  R0,CWORK                   FOR INTRASYSTEM SHROPT
          AHI  R1,C'1'                  YIELD '1' TO '4' VALUE
          STC  R1,CWORK+2                 FOR INTERSYSTEM SHROPT
          LA   R1,CWORK                 GET ADDRESS OF VAR DATA
          LHI  R2,3                     SET LENGTH OF VAR DATA
          LA   R3,DSSHRO                POINT BACK TO VAR NAME
          LHI  R4,L'DSSHRO              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          XGR  R2,R2                    CLEAR 64-BIT REGISTER
          ICM  R2,B'0111',RECBUF+114    GET PRIMARY ALLOCATION VALUE
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,DATPAL                POINT BACK TO VAR NAME
          LHI  R4,L'DATPAL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          XGR  R2,R2                    CLEAR 64-BIT REGISTER
          ICM  R2,B'0111',RECBUF+117    GET SECONDARY ALLOCATION VALUE
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,DATSAL                POINT BACK TO VAR NAME
          LHI  R4,L'DATSAL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          IF   RECBUF+120,(ON,TM),B'11000000'
           LA  R1,=C'CYL'                CYLINDER ALLOCATION
          ELSE
           IF  RECBUF+120,(ON,TM),B'10000000'
            LA R1,=C'TRK'                 TRACK ALLOCATION
           ELSE
            LA R1,=C'BLK'                 BLOCK ALLOCATION
           ENDIF
          ENDIF
          LHI  R2,3                     SET LENGTH OF TYPE ALLOC
          LA   R3,DATTAL                POINT BACK TO VAR NAME
          LHI  R4,L'DATTAL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          XGR  R2,R2                    CLEAR 64-BIT REGISTER
          ICM  R2,B'1111',RECBUF+129    GET AVERAGE RECL VALUE
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,RECAVG                POINT BACK TO VAR NAME
          LHI  R4,L'RECAVG              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          MVC  CINUM,FILEDCI            GET DATA COMP. CI
          BAS  R14,GET_VOLS             EXTRACT VOLUME LIST
          MVC  RECBUF(L'VARDATA),VARDATA MOVE VOL.LIST TO REC. BUF
          LA   R1,RECBUF                POINT BACK TO VOL.LIST
          LA   R2,RECBUF+L'VARDATA-1    POINT TO LAST BYTE OF LIST
          BAS  R5,SET_LENG              SET LENGTH IN R2
          LA   R3,DATVOL                POINT BACK TO VAR NAME
          LHI  R4,L'DATVOL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF

         L     R8,FILEACB              GET ACB ADDRESS
         L     R8,4(,R8)               GET AMBL ADDRESS
         L     R8,4(,R8)               GET DATA AMDSB ADDRESS
         USING AMDSBDS,R8              TEMPORARY ADDRESSABILITY

         MVC   CWORK,SPACES            CLEAR "RESULT" AREA
         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGC  R2,AMDPCTCA             GET FREE CI PCT PER CA
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         MVC   CWORK+4(3),CWORK        JUSTIFY DATA TO THE RIGHT
         MVC   CWORK(3),SPACES         CLEAR LEFT SIDE OF VALUE
         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGC  R2,AMDPCTCI             GET FREE BYTE PCT PER CI
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LHI   R2,7                    SET LENGTH OF VAR DATA
         LA    R3,DATFSP               POINT BACK TO VAR NAME
         LHI   R4,L'DATFSP             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDATTR3,(ON,TM),B'00010000'
          LA   R1,=C'1'                 INDICATE COMPRESSED
         ELSE
          LA   R1,=C'0'                 INDICATE NOCOMPRESS
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_COMP               POINT BACK TO VAR NAME
         LHI   R4,L'A_COMP             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDDATTR,(ON,TM),B'01000000'
          LA   R1,=C'1'                 INDICATE WRITECHECK
         ELSE
          LA   R1,=C'0'                 INDICATE NOWRITECHK
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_WCHK               POINT BACK TO VAR NAME
         LHI   R4,L'A_WCHK             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDDATTR,(ON,TM),B'00100000'
          LA   R1,=C'1'                 INDICATE IMBED
         ELSE
          LA   R1,=C'0'                 INDICATE NOIMBED
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_IMBD               POINT BACK TO VAR NAME
         LHI   R4,L'A_IMBD             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDDATTR,(ON,TM),B'00010000'
          LA   R1,=C'1'                 INDICATE REPLICATE
         ELSE
          LA   R1,=C'0'                 INDICATE NOREPLICATE
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_REPL               POINT BACK TO VAR NAME
         LHI   R4,L'A_REPL             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDDATTR,(ON,TM),B'00001000'
          LA   R1,=C'1'                 INDICATE ORDERED
         ELSE
          LA   R1,=C'0'                 INDICATE UNORDERED
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_ORDR               POINT BACK TO VAR NAME
         LHI   R4,L'A_ORDR             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    AMDDATTR,(ON,TM),B'00000001'
          LA   R1,=C'1'                 INDICATE SPANNED
         ELSE
          LA   R1,=C'0'                 INDICATE NONSPANNED
         ENDIF
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,A_SPAN               POINT BACK TO VAR NAME
         LHI   R4,L'A_SPAN             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    FILEOPT,(ON,TM),FILEAIX  FOR AN AIX...
          IF   AMDATTR3,(ON,TM),B'10000000'
           LA  R1,=C'1'                  INDICATE UNIQUEKEY
          ELSE
           LA  R1,=C'0'                  INDICATE NONUNIQUEKEY
          ENDIF
          LHI  R2,1                     SET LENGTH OF VAR DATA
          LA   R3,A_UKEY                POINT BACK TO VAR NAME
          LHI  R4,L'A_UKEY              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF                         ENDIF

         DROP  R8                      REMOVE ADDRESSABILITY
         L     R8,FILEACB              GET FILE ACB ADDRESS
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
         SHOWCB ACB=(R8),AM=VSAM,OBJECT=DATA,                          +
               AREA=(S,SHOWAREA),LENGTH=L'SHOWAREA,                    +
               FIELDS=(CINV,ENDRBA,HALCRBA,NEXCP,NEXT,STMST,           +
               NCIS,NSSS,NLOGR,NDELR,NINSR,NUPDR,NRETR)
         POP   ACONTROL
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  MACRO,=CL8'SHOWCB/A'     INDICATE OPERATION PERFORMED
          ST   R0,SAVE_R0               SAVE REGISTER 0
          OI   OPTFLAG,CTLBLKF          SIGNAL ERROR IN CTLBLK
          BAS  R5,SHOWRESN              PASS BACK RTN AND RSN CODES
          LHI  R5,16                    SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+0           GET CI SIZE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATCSZ               POINT BACK TO VAR NAME
         LHI   R4,L'DATCSZ             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+4           GET HI USED RBA
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATHUS               POINT BACK TO VAR NAME
         LHI   R4,L'DATHUS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+8           GET HI ALLOCATED RBA
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATHAL               POINT BACK TO VAR NAME
         LHI   R4,L'DATHAL             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+12          GET NUMBER OF EXCPs
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATEXC               POINT BACK TO VAR NAME
         LHI   R4,L'DATEXC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+16          GET NUMBER OF DATA EXTENTS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATEXT               POINT BACK TO VAR NAME
         LHI   R4,L'DATEXT             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         MVI   CWORK,X'40'             SET WORK AREA TO SPACES
         MVC   CWORK+1(L'CWORK-1),CWORK
         LM    R0,R1,SHOWAREA+20       GET TIMESTAMP VALUE
         GETIME STANDARD,LOCAL,CLOCK=YES  AND CONVERT TO DATE/TIME
         STCM  R15,B'0011',CWORK+0     STORE DATE IN WORK AREA
         STCM  R15,B'1100',CWORK+2      USING CCYYMMDD FORMAT
         STCM  R14,B'1111',CWORK+4
         ST    R1,PWORK8               STORE TIME IN WORK AREA
         UNPK  CWORK+8(7),PWORK8(4)     AND UNPACK IT
         OI    CWORK+14,X'F0'           AND SET AS UNSIGNED             00015800
         MVI   CWORK+8,X'40'           SEPARATE DATE AND TIME
         LA    R1,CWORK                GET ADDRESS OF VAR DATA
         LHI   R2,15                   SET LENGTH OF VAR DATA
         LA    R3,DSTSTP               POINT BACK TO VAR NAME
         LHI   R4,L'DSTSTP             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+28          GET NUMBER OF CI SPLITS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATCIS               POINT BACK TO VAR NAME
         LHI   R4,L'DATCIS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+32          GET NUMBER OF CA SPLITS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,DATCAS               POINT BACK TO VAR NAME
         LHI   R4,L'DATCAS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+36          GET NO. OF RECORDS IN FILE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,S_RECS               POINT BACK TO VAR NAME
         LHI   R4,L'S_RECS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+40          GET NO. OF DELETED RECORDS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,S_DELS               POINT BACK TO VAR NAME
         LHI   R4,L'S_DELS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+44          GET NO. OF INSERTED RECORDS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,S_ADDS               POINT BACK TO VAR NAME
         LHI   R4,L'S_ADDS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+48          GET NO. OF UPDATED RECORDS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,S_UPDS               POINT BACK TO VAR NAME
         LHI   R4,L'S_UPDS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         LA    R1,CWORK                POINT TO "RESULT" AREA
         LLGF  R2,SHOWAREA+52          GET NO. OF RETRIEVED RECORDS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,S_GETS               POINT BACK TO VAR NAME
         LHI   R4,L'S_GETS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT

         IF    FILEICI,(HT,CLC),=XL3'00'  IF INDEX PRESENT

          XC   KEYBUF,KEYBUF            SET KEY BUFFER TO LOW VALUES
          MVC  KEYBUF+1(3),FILEICI      POINT TO INDEX COMP. CI
          L    R8,FILECATR              GET CATALOG RPL ADDRESS
          BAS  R6,GETREC                GET THE CATALOG RECORD
          IF   RECBUF+1(3),(EQ,CLC),FILEICI  IF RECORD WAS FOUND
           LA  R1,RECBUF+49              POINT BACK TO COMP. NAME
           LA  R2,RECBUF+49+44-1         POINT TO LAST BYTE OF NAME
           BAS R5,SET_LENG               SET LENGTH IN R2
           LA  R3,INDXNM                 POINT BACK TO VAR NAME
           LHI R4,L'INDXNM               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT

           LA  R1,CWORK                  POINT TO "RESULT" AREA
           XGR R2,R2                     CLEAR 64-BIT REGISTER
           ICM R2,B'0111',RECBUF+114     GET PRIMARY ALLOCATION VALUE
           BAS R5,REG2ZONE               CONVERT THAT TO ZONED-DECIMAL
           LA  R3,IDXPAL                 POINT BACK TO VAR NAME
           LHI R4,L'IDXPAL               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT

           LA  R1,CWORK                  POINT TO "RESULT" AREA
           XGR R2,R2                     CLEAR 64-BIT REGISTER
           ICM R2,B'0111',RECBUF+117     GET SECONDARY ALLOCATN VALUE
           BAS R5,REG2ZONE               CONVERT THAT TO ZONED-DECIMAL
           LA  R3,IDXSAL                 POINT BACK TO VAR NAME
           LHI R4,L'IDXSAL               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT

           IF  RECBUF+120,(ON,TM),B'11000000'
            LA R1,=C'CYL'                 CYLINDER ALLOCATION
           ELSE
            IF RECBUF+120,(ON,TM),B'10000000'
             LA R1,=C'TRK'                 TRACK ALLOCATION
            ELSE
             LA R1,=C'BLK'                 BLOCK ALLOCATION
            ENDIF
           ENDIF
           LHI R2,3                      SET LENGTH OF TYPE ALLOC
           LA  R3,IDXTAL                 POINT BACK TO VAR NAME
           LHI R4,L'IDXTAL               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT

           MVC CINUM,FILEICI             GET INDEX COMP. CI
           BAS R14,GET_VOLS              EXTRACT VOLUME LIST
           MVC RECBUF(L'VARDATA),VARDATA MOVE VOL.LIST TO REC. BUF
           LA  R1,RECBUF                 POINT BACK TO VOL.LIST
           LA  R2,RECBUF+L'VARDATA-1     POINT TO LAST BYTE OF LIST
           BAS R5,SET_LENG               SET LENGTH IN R2
           LA  R3,IDXVOL                 POINT BACK TO VAR NAME
           LHI R4,L'IDXVOL               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT
          ENDIF                         ENDIF

          L    R8,FILEACB               GET ACB ADDRESS
          L    R8,4(,R8)                GET AMBL ADDRESS
          L    R8,8(,R8)                GET INDEX AMDSB ADDRESS
          USING AMDSBDS,R8              TEMPORARY ADDRESSABILITY

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LGH  R2,AMDNIL                GET NUMBER OF INDEX LEVELS
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXLVL                POINT BACK TO VAR NAME
          LHI  R4,L'IDXLVL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,AMDSSRBA              GET SEQUENCE SET INDEX RBA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXSSR                POINT BACK TO VAR NAME
          LHI  R4,L'IDXSSR              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,AMDHLRBA              GET HIGH-LEVEL INDEX RBA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXHLR                POINT BACK TO VAR NAME
          LHI  R4,L'IDXHLR              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          DROP R8                       REMOVE ADDRESSABILITY
          L    R8,FILEACB               GET FILE ACB ADDRESS
          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
          SHOWCB ACB=(R8),AM=VSAM,OBJECT=INDEX,                        +
               AREA=(S,SHOWAREA),LENGTH=L'SHOWAREA,                    +
               FIELDS=(CINV,ENDRBA,HALCRBA,NEXCP,NEXT)
          POP  ACONTROL
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'SHOWCB/A'      INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+0            GET CI SIZE
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXCSZ                POINT BACK TO VAR NAME
          LHI  R4,L'IDXCSZ              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+4            GET HI USED RBA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXHUS                POINT BACK TO VAR NAME
          LHI  R4,L'IDXHUS              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+8            GET HI ALLOCATED RBA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXHAL                POINT BACK TO VAR NAME
          LHI  R4,L'IDXHAL              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+12           GET NUMBER OF EXCPs
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXEXC                POINT BACK TO VAR NAME
          LHI  R4,L'IDXEXC              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+16           GET NUMBER OF DATA EXTENTS
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,IDXEXT                POINT BACK TO VAR NAME
          LHI  R4,L'IDXEXT              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

         ENDIF                         ENDIF
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
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R8,SHVCTLB              STORAGE FOR
         USING SHVBLOCK,R8              SHARED VARIABLES CONTROL BLOCK
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
         DROP  R8
         MVC   VARNAME+0(3),=C'RC '    SET VARIABLE NAME
         LHI   R4,2                    SET LENGTH OF VARIABLE NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         L     R15,SAVE_RF             RETURN R15 AS RETURN CODE
* ------------------------------------------------------------------- *
         TRAILER RETCODE=R15,SVAREA=SAVEAREA
* =================================================================== *
         EJECT
* =================================================================== *
GETREC   DS    0H            READ A RECORD BASED UPON RPL=(R8)
* ------------------------------------------------------------------- *
         GET   RPL=(R8)                READ A RECORD
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  MACRO,=CL8'GET'          INDICATE OPERATION PERFORMED
          OI   OPTFLAG,RPLERRF          SIGNAL ERROR IN RPL
          B    RETURN                   GO RETURN ERROR
         ENDIF                         ENDIF
* ------------------------------------------------------------------- *
         BR    R6                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
GET_VOLS DS    0H            GET VOLUME LIST FOR REQUESTED COMPONENT
* ------------------------------------------------------------------- *
         ST    R14,SAVE_RE             SAVE RETURN REGISTER
         L     R8,FILECATR             GET CATALOG RPL ADDRESS
*
         XC    KEYBUF,KEYBUF           SET KEY BUFFER TO LOW VALUES
         MVC   KEYBUF+1(3),CINUM       POINT TO COMP. RECORD CI
         BAS   R6,GETREC               GET THE CATALOG RECORD
         IF    RECBUF+1(3),(NE,CLC),CINUM   IF RECORD NOT FOUND
          MVC  MACRO,=CL8'GETREC'       INDICATE OPERATION PERFORMED
          LHI  R15,14                   SIGNAL NOT FOUND
          B    RETURN                   RETURN ERROR TO CALLER
         ENDIF                         ENDIF
*
         LA    R2,RECSAVE              SET ADDRESS OF  "TO"  FIELD
         LHI   R3,L'RECSAVE            SET LENGTH  OF  "TO"  FIELD
         LA    R4,RECBUF               SET ADDRESS OF "FROM" FIELD
         LHI   R5,L'RECBUF             SET LENGTH  OF "FROM" FIELD
*        ICM   R5,B'1000',LOVALUE      PAD "TO" FIELD WITH LOVALUE
         MVCL  R2,R4                   SAVE THE CATALOG RECORD
         MVI   VARDATA,X'40'           CLEAR AREA FOR VOLUME LIST
         MVC   VARDATA+1(L'VARDATA-1),VARDATA
*
         XR    R2,R2                   CLEAR WORK REGISTER
         ICM   R2,B'0001',RECSAVE+148  GET GOP COUNT
         LA    R3,RECSAVE+149          POINT TO FIRST GOP
         LA    R4,VARDATA              POINT TO VAR DATA AREA
         WHILE R2,(NZ,LTR),R2          LOOP ON GOP COUNT
          SELECT
*
          WHEN 3(R3),EQ,X'03'           IF THIS IS AN INLINE ENTRY
           XR  R0,R0                     CLEAR WORK REGISTER
           ICM R0,B'0001',RECSAVE+148    GET GOP COUNT, AGAIN
           MHI R0,5                       AND MULTIPLY BY GOP LENGTH
           LA  R5,RECSAVE+149            START AT BEG. OF GOP AREA
           AR  R5,R0                     PLUS LENGTH OF GO POINTERS
           AH  R5,1(R3)                  PLUS OFFSET TO INLINE ENTRY
           MVC 0(6,R4),6(R5)             GET VOLUME ID
           AHI R4,7                      INCR. VAR DATA AREA
*
          WHEN 3(R3),EQ,X'83'           IF THIS IS AN EXTREC ENTRY
           IF  RECBUF+1(3),(NE,CLC),0(R3) AND EXTREC NOT IN BUFFER
            MVC KEYBUF+1(3),0(R3)         POINT TO EXTREC CI
            BAS R6,GETREC                 GET THE EXTREC RECORD
            IF RECBUF+1(3),(NE,CLC),0(R3) IF RECORD NOT FOUND
             MVC MACRO,=CL8'GETREC'        INDICATE OPERATION PERFORMED
             LHI R15,14                    SIGNAL NOT FOUND
             B RETURN                      RETURN ERROR TO CALLER
            ENDIF                         ENDIF
           ENDIF                         ENDIF
           XR  R5,R5                     CLEAR WORK REGISTER
           ICM R5,B'0001',RECBUF+54      GET GOP COUNT
           LA  R6,RECBUF+55              POINT TO FIRST EXT GOP
           WHILE R5,(NZ,LTR),R5          LOOP ON EXT GOP COUNT
            IF 3(R6),EQ,X'03'
            AND 4(1,R6),(EQ,CLC),4(R3)    FIND MATCHING EXT GOP ENTRY
             WEXIT
            ENDIF
            BCTR R5,0                     DECREMENT EXT GOP COUNT
            LA R6,5(,R6)                  POINT TO NEXT EXT GOP
           WEND
           IF  R5,(NZ,LTR),R5            IF EXT GOP WAS FOUND
            XR R0,R0                      CLEAR WORK REGISTER
            ICM R0,B'0001',RECBUF+54      GET GOP COUNT, AGAIN
            MHI R0,5                      AND MULTIPLY BY GOP LENGTH
            LA R5,RECBUF+55               START AT BEG. OF EXT GOP AREA
            AR R5,R0                      PLUS LENGTH OF GO POINTERS
            AH R5,1(,R6)                  PLUS OFFSET TO INLINE ENTRY
            MVC 0(6,R4),6(R5)             GET VOLUME ID
            AHI R4,7                      INCR. VAR DATA AREA
           ENDIF                         ENDIF
*
          ENDSL                         ENDSL
          BCTR R2,0                     DECREMENT GOP COUNT
          LA   R3,5(,R3)                POINT TO NEXT GOP
         WEND                          LOOP ON GOP COUNT
* ------------------------------------------------------------------- *
         L     R14,SAVE_RE             RESTORE RETURN REGISTER
         BR    R14                     RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZONE DS    0H    CONVERT REG2 TO LEFT-SIGN, ZONED-DECIMAL AT REG1
*                                               RETURN LENGTH IN REG2
* ------------------------------------------------------------------- *
         LR    R3,R1                   SAVE R1 IN R3
         XR    R1,R1                   CLEAR R1 FOR EDIT AND MARK
         CVDG  R2,PWORK16              CONVERT REG2 TO PACKED-DECIMAL
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          MVC  EWORK,=X'F0212020202020202020202020202020202020206040'
          LA   R1,EWORK+2              POINT TO SIGNIFICANCE BYTE
         ELSE
          MVC  EWORK,=X'40202020202020202020202020202020202021206040'
          LA   R1,EWORK+19             POINT TO SIGNIFICANCE BYTE
         ENDIF
         LA    R2,EWORK+20             POINT TO SIGN BYTE (& IGNORE IT)
         IF    EWORK(22),(M,EDMK),PWORK16+6  IF NEGATIVE NUMBER
          SHI  R1,1                     DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'               FLOAT A LEFT MINUS SIGN
         ENDIF
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          IF   0(R1),EQ,C'-'
           LA  R1,EWORK                  POINT TO SIGN BYTE
           LHI R2,20                     SET LENGTH OF DATA
          ELSE
           LA  R1,EWORK+1                POINT TO SIGNIFICANT BYTE
           LHI R2,19                     SET LENGTH OF DATA
          ENDIF
         ELSE
          SR   R2,R1                    CALC NO. OF CHARS TO MOVE
         ENDIF
         SHI   R2,1                    DECR. LEN FOR ZERO REF.
         EX    R2,MOVE_RESULT          MOVE VALUE TO RESULT AREA
         AHI   R2,1                    INCR. LEN FOR NORMAL REF.
         LR    R1,R3                   RESTORE R1 FROM R3
*
         BR    R5
*
MOVE_RESULT MVC 0(0,R3),0(R1)          SEE EXECUTE INSTRUCTION ABOVE
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
*
         BR    R5                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
SET_STEM DS    0H    CREATE A STEM ELEMENT
* ------------------------------------------------------------------- *
         ST    R5,AWORK4               SAVE RETURN ADDRESS
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,STEMDATA_MVC         MOVE DATA TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT BACK
         LA    R8,SHVCTLB              STORAGE FOR
         USING SHVBLOCK,R8              SHARED VARIABLES CONTROL BLOCK
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
         DROP  R8
*
         LR    R1,R3                   TRANSFER NAME ADDRESS
         LR    R2,R4                   TRANSFER NAME LENGTH
         LA    R3,VARNAME              POINT TO VAR NAME BUFFER
         XR    R4,R4                   STARTING WITH ZERO LENGTH
         MVC   0(L'DDNAME,R3),DDNAME   MOVE IN STEM NAME
         A     R3,DDNLENG              INCR. VAR NAME ADDR.
         A     R4,DDNLENG              INCR. VAR NAME LENGTH
         MVI   0(R3),C'.'              ADD A PERIOD
         AHI   R3,1                    INCR. VAR NAME ADDR.
         AHI   R4,1                    INCR. VAR NAME LENGTH
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,STEMNAME_MVC         MOVE NAME TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT BACK
         AR    R4,R2                   INCR. VAR NAME LENGTH
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
*
STEMDATA_MVC MVC VARDATA(0),0(R1)      SEE FIRST EXECUTE UP ABOVE
STEMNAME_MVC MVC 0(0,R3),0(R1)         SEE SECOND EXECUTE JUST ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H    SET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         LA    R8,SHVCTLB              STORAGE FOR
         USING SHVBLOCK,R8              SHARED VARIABLES CONTROL BLOCK
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R4,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   VARNAME(3),(NE,CLC),=C'RC '  IF NOT 'RC', REPORT ERRORS
          AND  SHVRET,NE,SHVNEWV        AND NOT NEW VARIABLE
           MVC MACRO,=CL8'ARXEXCOM'      LAST FAILED MACRO
           LHI R5,8                      SET RESULT CODE
           IF  R15,(Z,LTR),R15           IF ZERO RETURN CODE
            IC R15,SHVRET                 SAVE ARXEXCOM CC
           ENDIF                         ENDIF
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         DROP  R8
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
VSAMERRC DS    0H    PASS BACK VSAM RTN AND RSN CODES
* ------------------------------------------------------------------- *
         ST    R5,AWORK4+4             SAVE RETURN ADDRESS
*
         LA    R1,CWORK               POINT TO "RESULT" AREA
         LGR   R2,R15                  GET VSAM RTN CODE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         XR    R15,R15                 CLEAR FOR NO RETURN CODE
         LA    R3,VSAMRC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMRC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         MVC   CWORK,SPACES           CLEAR WORK AREA
         LA    R1,CWORK               POINT TO "RESULT" AREA
         IF    OPTFLAG,(ON,TM),RPLERRF IF RPL ERROR OCCURRED...
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          LLGC R2,RPLFDBKC              GET RPL FEEDBACK CODE
          DROP R8                       REMOVE ADDRESSABILITY
         ELSE                          ELSE ACB ERROR OCCURRED...
          USING IKQACB,R8               TEMPORARY ADDRESSABILITY
          LLGC R2,ACBERFLG              GET ACB ERROR FLAG
          DROP R8                       REMOVE ADDRESSABILITY
         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         XR    R15,R15                 CLEAR FOR NO RETURN CODE
         LA    R3,VSAMEC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMEC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R5,AWORK4+4             RESTORE RETURN ADDRESS
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
SHOWRESN DS    0H    PASS BACK SHOWCAT RTN AND RSN CODES
* ------------------------------------------------------------------- *
         ST    R5,AWORK4+4             SAVE RETURN ADDRESS
*
         LA    R1,CWORK               POINT TO "RESULT" AREA
         IF    R15,(LT,C),=F'12',OR,                                   X
               OPTFLAG,(ON,TM),CTLBLKF
          LGR  R2,R15
         ELSE
          LGH  R2,SHOWAREA+2           GET SHOWCAT RTN CODE
         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         XR    R15,R15                 CLEAR FOR NO RETURN CODE
         LA    R3,VSAMRC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMRC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         MVC   CWORK,SPACES           CLEAR WORK AREA
         LA    R1,CWORK               POINT TO "RESULT" AREA
         IF    OPTFLAG,(ON,TM),CTLBLKF
          LGF  R2,SAVE_R0              RESTORE REGISTER 0
         ELSE
          LGH  R2,SHOWAREA+4           GET SHOWCAT RTN CODE
         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         XR    R15,R15                 CLEAR FOR NO RETURN CODE
         IF    SHOWAREA+6,NE,X'00',AND, MODULE CODE PRESENT?           X
               OPTFLAG,(Z,TM),CTLBLKF
          MVC  CWORK+4(2),SHOWAREA+6   PLUS THE MODULE CODE             00015800
          LHI  R2,6                     SET LENGTH OF VAR DATA
         ENDIF                         ENDIF
         LA    R3,VSAMEC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMEC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R5,AWORK4+4             RESTORE RETURN ADDRESS
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    18D                     STANDARD OR F4SA REG. SVAEAREA
SPACES   DC    CL8' '
RECSAVE  DS    CL505                   SAVE AREA FOR CATALOG RECORD
*
         LTORG
*
* ******************************************************************* * 00053500
*        STEM EXTENSION NAMES TO CREATE                               * 00053600
* ******************************************************************* * 00053700
VSAMRC   DC    C'_RC'                  VSAM return code
VSAMEC   DC    C'_EC'                  VSAM error code
*                       the following are all for cat/dsn attributes
CATLBL   DC    C'_CATLBL'              catalog label
CATDSN   DC    C'_CATDSN'              catalog name
DDN      DC    C'_DDN'                 dataset label
DSN      DC    C'_DSN'                 dataset name
TYP      DC    C'_TYP'                 dataset type
KEYMAX   DC    C'_KMX'                 key maximum length
KEYPOS   DC    C'_RKP'                 relative key position
RECMAX   DC    C'_RMX'                 record maximum length
RECAVG   DC    C'_RAV'                 record average length
*                       the following are all for dataset statistics
DSCREA   DC    C'_CREA'                creation date
DSEXPD   DC    C'_EXPD'                expiration date
DSTSTP   DC    C'_TSTP'                last updated date/time
DSSHRO   DC    C'_SHRO'                share options
*                       the following are all for component statistics
DATANM   DC    C'_DATA'                DATA component name
DATCSZ   DC    C'_DCSZ'                DATA CI size
DATEXC   DC    C'_DEXC'                DATA number of EXCPs
DATEXT   DC    C'_DEXT'                DATA number of extents
DATFSP   DC    C'_DFSP'                DATA freespace CA/CI pct
DATCAS   DC    C'_DCAS'                DATA CA splits
DATCIS   DC    C'_DCIS'                DATA CI splits
DATHAL   DC    C'_DHAL'                DATA high allocated RBA/CI
DATHUS   DC    C'_DHUS'                DATA high used RBA/CI
DATTAL   DC    C'_DTAL'                DATA type allocation
DATPAL   DC    C'_DPAL'                DATA primary allocation
DATSAL   DC    C'_DSAL'                DATA secondary allocation
DATVOL   DC    C'_DVOL'                DATA volume list
*
INDXNM   DC    C'_INDX'                INDEX component name
IDXCSZ   DC    C'_ICSZ'                INDEX CI size
IDXEXC   DC    C'_IEXC'                INDEX number of EXCPs
IDXEXT   DC    C'_IEXT'                INDEX number of extents
IDXLVL   DC    C'_ILVL'                INDEX number of levels
IDXSSR   DC    C'_ISSR'                INDEX sequence set RBA
IDXHLR   DC    C'_IHLR'                INDEX high elvel RBA
IDXHAL   DC    C'_IHAL'                INDEX high allocated RBA
IDXHUS   DC    C'_IHUS'                INDEX high used RBA
IDXPAL   DC    C'_IPAL'                INDEX type allocation
IDXSAL   DC    C'_ISAL'                INDEX primary allocation
IDXTAL   DC    C'_ITAL'                INDEX secondary allocation
IDXVOL   DC    C'_IVOL'                INDEX volume list
*                       the following are all for dataset attributes
A_COMP   DC    C'_COMP'                compressed?
A_ERAS   DC    C'_ERAS'                erase?
A_IMBD   DC    C'_IMBD'                imbed?
A_ORDR   DC    C'_ORDR'                ordered?
A_RCVR   DC    C'_RCVR'                recovery?
A_REPL   DC    C'_REPL'                replicate?
A_REUS   DC    C'_REUS'                reused?
A_SPAN   DC    C'_SPAN'                spanned?
A_UKEY   DC    C'_UKEY'                uniquekey? (AIX)
A_UPGR   DC    C'_UPGR'                upgrade?  (AIX)
A_UPDT   DC    C'_UPDT'                update?  (PATH)
A_WCHK   DC    C'_WCHK'                writecheck?
*                       the following are all for dataset statistics
S_RECS   DC    C'_RECS'                total records
S_DELS   DC    C'_DELS'                deleted records
S_ADDS   DC    C'_ADDS'                inserted records
S_UPDS   DC    C'_UPDS'                updated records
S_GETS   DC    C'_GETS'                retreived records
* =================================================================== *
         END   RXVSAMXA
