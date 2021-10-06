INIT     TITLE 'VSAMINIT - ADD/DEL A RECORD TO INITIALIZE A VSAM FILE'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*        V S A M   I N I T I A L I Z A T I O N   R O U T I N E        * 00000500
*                                                                     * 00000600
*                                                                     * 00000700
* Winsupply Group Services                            Written by:     * 00000800
* 3110 Kettering Boulevard                         Dave Leonard Clark * 00000900
* Morraine, OH  45439  USA                           (937)751-3300    * 00001000
*                                                                     * 00001100
* No warranty is expressed  or  implied.   Neither the author nor the * 00001200
* company is responsible for  any  loss  or damage resulting from the * 00001300
* use of this code.  Source is provided on an as-is basis.            * 00001400
*                                                                     * 00001500
* =================================================================== * 00001600
*                                                                     * 00001700
*                               PURPOSE                               * 00001800
*                                                                     * 00001900
*      This routine will initialize the specified (and newly defined) * 00002000
* VSAM dataset with a binary zeroes record.  VSAM catalog information * 00002100
* is used to determine the length of the record written.  If the VSAM * 00002200
* dataset is a KSDS cluster,  then  this routine will also optionally * 00002300
* delete the initialization record  after  it is written.   In either * 00002400
* case,  this allows subsequent  programs  (and particularly CICS) to * 00002500
* not have to open the cluster in LOAD mode.                          * 00002500
*                                                                     * 00002500
* =================================================================== * 00002600
*                                                                     * 00002700
*                           IMPLEMENTATION                            * 00002800
*                                                                     * 00002900
*      This program should be compiled as a standard batch program.   * 00003000
*                                                                     * 00002500
* =================================================================== * 00003800
*                                                                     * 00003900
*                             INVOCATION                              * 00004000
*                                                                     * 00004100
*      The target VSAM dataset is specified  by DLBL name via the JCL * 00002500
* EXEC PARM parameter.  If omitted, a DLBL name of "VINIT" is used by * 00002500
* default.   Just ensure there is a  job or standard label for either * 00002500
* the specified DLBL name or the default DLBL name.                   * 00002500
*                                                                     * 00002500
*      For example:                                                   * 00002500
*                                                                     * 00002500
* // EXEC PGM=VSAMINIT,SIZE=*,PARM='MYDLBL'                           * 00002500
*                                                                     * 00005100
*                                                                     * 00005100
*      Note that  the  above  does  not  give  the  oportunity to run * 00002500
* BLDINDEX in the case that  there  is  an upgrade AIX built over the * 00002500
* base cluster being initialized.   To allow that, you may specify an * 00002500
* UPSI  switch   (before   separate   executions)   to   control  the * 00002500
* initialization process.                                             * 00002500
*                                                                     * 00002500
*      For example:                                                   * 00002500
*                                                                     * 00002500
* // UPSI 00                   /* default, add+del */                 * 00002500
* // UPSI 10                   /* add, only    */                     * 00002500
* // UPSI 01                   /* delete, only */                     * 00002500
* // UPSI 11                   /* add, only    */                     * 00002500
*                                                                     * 00005100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00005200
         EJECT                                                          00005300
* =================================================================== *
DYNASTOR DSECT
* ------------------------------------------------------------------- *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
DWORD    DS    D                       DOUBLE-WORD FOR CONVERSIONS
CHAR8    DS    CL8                     CHARACTER AREA FOR CONVERSIONS
MYDLBL   DS    CL8                     VSAM DLBL NAME
MYACBBA  DS    F                       MY ACB BASE ADDRESS
MYRPLBA  DS    F                       MY RPL BASE ADDRESS
SHOWAREA DS    XL64
         ORG   SHOWAREA
KEYLN    DS    F                       KEY LENGTH
KEYOF    DS    F                       KEY OFFSET
MAXRL    DS    F                       MAX. RECORD LENGTH
         ORG
OPCODE   DS    CL8                     OPERATION CODE
ERRCODE  DS    X                       COMMON ERROR CODE
CINUM    DS    XL3                     CATALOG CI NUMBER
CATACB   DS    F                       CATALOG ACB ADDRESS
OPT1     DS    X                       OPTION BITS
ADDONLY  EQU   B'10000000'             ONLY ADD THE INIT RECORD
DELONLY  EQU   B'01000000'             ONLY DEL THE INIT RECORD
SHWCATF  EQU   B'00001000'             GET SHOWCAT ERROR INFO
ACBERRF  EQU   B'00000100'             GET ACB ERROR INFO
RPLERRF  EQU   B'00000010'             GET RPL ERROR INFO
KSDS     EQU   B'00000001'             CURRENT FILE IS KSDS
MBUF     DS    CL120                   MESSAGE BUFFER
*
         DS    0D
KBUF     DS    XL255                   KEY BUFFER
         DS    0D
RBUF     DS    XL32761                 RECORD BUFFER
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES                                              *
* ------------------------------------------------------------------- *
R0       EQU   0                  PARM REG
R1       EQU   1                  PARM REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                  RPL DSECT REG
R9       EQU   9                  ACB DSECT REG
R10      EQU   10
R11      EQU   11                 PGM BASE REG
R12      EQU   12                 LE RESERVED REG
R13      EQU   13                 REGISTER SAVEAREA
R14      EQU   14                 RETURN REG
R15      EQU   15                 ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      IF/AND/ELSE/ENDIF CONSTRUCT
         COPY  SELECT                  SELECT/CASE/ENDSL CONSTRUCT
         PRINT ON
COMREG   MAPCOMR EQU=YES               PARTITION COMMUNICATION REGION
         IKQACB DSECT=YES              VSAM ACB DSECT
         IKQRPL DSECT=YES              VSAM RPL DESCT
* =================================================================== *
         EJECT
* =================================================================== *
VSAMINIT HEADER VERSION='1.2',AUTHOR='DLC',DATE='(C)JAN10',            +
               BASE=R11,SVAREA=SALEN
* ------------------------------------------------------------------- *
         USING DYNASTOR,R13            SET BASE FOR GETVIS STORAGE
         XC    OPT1,OPT1               CLEAR ERROR OPTIONS
*
         IF    R1,(EQ,CR),R11          IF NO EXEC PARM PASSED
          MVC  MYDLBL,=CL8'VINIT'       SET DEFAULT DLBL NAME
         ELSE                          ELSE
          MVC  MYDLBL,=CL8' '           SET INITIAL PADDING
          L    R1,0(,R1)                GET ADDRESS OF EXEC PARM
          IF   0(2,R1),(HT,CLC),=H'7'   IF LONGER THAN 7 BYTES
           LHI R2,7                      DEFAULT TO 7
          ELSE                          ELSE
           LH  R2,0(,R1)                 GET EXEC PARM LENGTH
           IF  R2,(Z,LTR),R2             IF EXEC PARM LENGTH IS ZERO
            MVC OPCODE,=CL8'EXECPARM'     INDICATE OPERATION PERFORMED
            LHI R0,1                      SET ERROR CODE
            LHI R15,8                     SET RETURN CODE
            B  ERRMSG                     GO PASS BACK RETURN CODE
           ENDIF                         ENDIF
          ENDIF                         ENDIF
          BCTR R2,0                     DECREMENT FOR EXECUTE
          EX   R2,GETPARM               GET DLBL NAME FROM EXEC PARM
          IF   MYDLBL,(EQ,CLC),=CL8' '  IF DLBL NAME REMAINS BLANK
           MVC OPCODE,=CL8'EXECPARM'     INDICATE OPERATION PERFORMED
           LHI R0,2                      SET ERROR CODE
           LHI R15,8                     SET RETURN CODE
           B   ERRMSG                    GO PASS BACK RETURN CODE
GETPARM  MVC   MYDLBL(0),2(R1)         (SEE "EX" INSTRUCTION ABOVE)
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         COMRG REG=(R1)                GET ADDR OF PARTITION COMREG
         USING COMREG,R1               SET ADDRESSABILITY TO COMREG
         SELECT                        DEFAULT IS BOTH ADD & DEL
         CASE  UPSI,(ON,TM),ADDONLY    IF ADD-ONLY OPTION IS ON
          OI   OPT1,ADDONLY             SIGNAL ADD-ONLY
         CASE  UPSI,(ON,TM),DELONLY    IF DEL-ONLY OPTION IS ON
          OI   OPT1,DELONLY             SIGNAL DEL-ONLY
         ENDSL
         DROP  R1                      REMOVE COMREG ADDRESSABILITY
*
         XC    SHOWAREA,SHOWAREA       CLEAR SHOWCAT DISPLAY AREA
         LA    R2,L'SHOWAREA           GET LENGTH OF SHOWCAT AREA
         STH   R2,SHOWAREA             PUT IN FIRST TWO BYTES OF AREA
*
         LA    R2,MYDLBL               POINT TO CLUSTER DLBL NAME
         LA    R3,SHOWAREA             POINT TO SHOWCAT DISPLAY AREA
         SHOWCAT DDNAME=(R2),AREA=(R3)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'SHOWCAT'     INDICATE OPERATION PERFORMED
          OI   OPT1,SHWCATF             SIGNAL ERROR IN SHOWCAT
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
         IF    SHOWAREA+8,NE,C'C'      IF NOT A CLUSTER RECORD
          LA   R1,L'INVDLBL             GET LENGTH OF MESSAGE
          LA   R2,INVDLBL               GET ADDRESS OF MESSAGE
          BAS  R7,PRTMSG                GO PRT MSG ON SYSLST
          LHI  R15,12                   SET RETURN CODE
          B    FINISH                   RETURN TO CALLER
         ENDIF                         ENDIF
         IF    SHOWAREA+12,EQ,C'D'     IF FIRST PAIR IS FOR DATA COMP.
          MVC  CINUM,SHOWAREA+13        POINT TO FIRST CI NUMBER
         ELSE                          ELSE
          MVC  CINUM,SHOWAREA+17        POINT TO SECOND CI NUMBER
         ENDIF                         ENDIF
         MVC   CATACB,SHOWAREA+4       SAVE CATALOG ACB ADDRESS
*
         LA    R2,CINUM                POINT TO CI NUMBER OF CAT REC
         L     R3,CATACB               GET ADDRESS OF THE CATALOG ACB
         LA    R4,SHOWAREA             POINT TO SHOWCAT DISPLAY AREA
         SHOWCAT CI=(R2),ACB=(R3),AREA=(R4)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'SHOWDAT'     INDICATE OPERATION PERFORMED
          OI   OPT1,SHWCATF             SIGNAL ERROR IN SHOWCAT
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
         IF    SHOWAREA+8,NE,C'D'      IF NOT A DATA COMP. RECORD
          LA   R1,L'INVDATA             GET LENGTH OF MESSAGE
          LA   R2,INVDATA               GET ADDRESS OF MESSAGE
          BAS  R7,PRTMSG                GO PRT MSG ON SYSLST
          LHI  R15,12                   SET RETURN CODE
          B    FINISH                   RETURN TO CALLER
         ENDIF                         ENDIF
*
         IF    SHOWAREA+9,(ON,TM),B'01000000'
         AND   SHOWAREA+9,(Z,TM),B'00111000'
          OI   OPT1,KSDS                FLAG AS KSDS DATASET
          GENCB BLK=ACB,AM=VSAM,DDNAME=(*,MYDLBL),                     +
               MACRF=(KEY,SEQ,OUT,NRS)
         ELSE
          NI   OPT1,X'FF'-KSDS          SET OFF KSDS FLAG
          GENCB BLK=ACB,AM=VSAM,DDNAME=(*,MYDLBL),                     +
               MACRF=(ADR,SEQ,OUT,NRS)
         ENDIF
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'GENACB'      INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
         ST    R1,MYACBBA              SAVE ACB ADDRESS
*
         L     R9,MYACBBA              GET ACB ADDRESS
         USING IKQACB,R9                AND ESTABLISH ADDRESSABILITY
*
         BAS   R4,OPENACB              OPEN THE ACB
*
         GENCB BLK=RPL,AM=VSAM,ACB=(R9),                               +
               AREA=(S,RBUF),AREALEN=(*,MAXRL),                        +
               ARG=(S,KBUF),KEYLEN=(*,KEYLN),                          +
               OPTCD=(KEY,SEQ,NSP)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'GENRPL'      INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
         ST    R1,MYRPLBA              SAVE RPL ADDRESS
*
         L     R8,MYRPLBA              GET RPL ADDRESS
         USING IKQRPL,R8                AND ESTABLISH ADDRESSABILITY
*
         LR    R1,R8                   COPY RPL ADDR FOR MOD SHORT FORM
         L     R0,MAXRL                GET MAXIMUM RECORD LENGTH
*
         MODCB RPL=(1),RECLEN=(0)      SET REC LENGTH VIA SHORT FORM
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'MODCB1'      INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         XC    KBUF,KBUF               INITIALIZE KEY BUFFER
         LA    R4,RBUF                 GET ADDRESS OF REC BUF
         L     R5,MAXRL                GET LENGTH OF REC NEEDED
         LA    R6,KBUF                 GET ADDRESS OF KEY BUF
         L     R7,KEYLN                GET LENGTH OF KEY NEEDED
         ICM   R7,B'1000',KBUF         SET PAD CHARACTER                00029100
         MVCL  R4,R6                   INITIALIZE RECORD BUFFER
*
         IF    OPT1,(NOT,TM),DELONLY   IF NOT DEL-ONLY
*
          PUT  RPL=(R8)                WRITE INITIALIZING RECORD
          IF   R15,(NZ,LTR),R15        IF FAILED
           MVC OPCODE,=CL8'PUTREC'      INDICATE OPERATION PERFORMED
           OI  OPT1,RPLERRF             SIGNAL ERROR IN RPL
           B   ERRMSG                   GO PASS BACK RETURN CODE
          ENDIF                        ENDIF
          LA   R1,L'RECADD             GET LENGTH OF MESSAGE
          LA   R2,RECADD               GET ADDRESS OF MESSAGE
          BAS  R7,PRTMSG               GO PRT MSG ON SYSLST
*
         ENDIF
*
         BAS   R4,CLOSEACB             CLOSE THE ACB
*
         IF    OPT1,(NOT,TM),KSDS,BC,COMPLETE  IF NOT KSDS, WE'RE DONE
         ENDIF                         ENDIF
         IF    OPT1,(ON,TM),ADDONLY,BC,COMPLETE  IF ADD-ONLY, WE DONE
         ENDIF                         ENDIF
*
         MODCB ACB=(R9),AM=VSAM,MACRF=(IN)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'MODCB2'      INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         BAS   R4,OPENACB              OPEN THE ACB
*
         MODCB RPL=(R8),AM=VSAM,OPTCD=(UPD)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'MODCB3'      INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         GET   RPL=(R8)                GET A RECORD
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'GETREC'      INDICATE OPERATION PERFORMED
          OI   OPT1,RPLERRF             SIGNAL ERROR IN RPL
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         ERASE RPL=(R8)                DELETE A RECORD
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'ERASE'       INDICATE OPERATION PERFORMED
          OI   OPT1,RPLERRF             SIGNAL ERROR IN RPL
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
         LA    R1,L'RECDEL             GET LENGTH OF MESSAGE
         LA    R2,RECDEL               GET ADDRESS OF MESSAGE
         BAS   R7,PRTMSG               GO PRT MSG ON SYSLST
*
         BAS   R4,CLOSEACB             CLOSE THE ACB
*
COMPLETE LA    R1,L'DONE               GET LENGTH OF MESSAGE
         LA    R2,DONE                 GET ADDRESS OF MESSAGE
         BAS   R7,PRTMSG               GO PRT MSG ON SYSLST
         LHI   R15,0                   SET RETURN CODE ZERO
         B     FINISH                  RETURN TO CALLER
* ------------------------------------------------------------------- *
ERRMSG   DS    0H
         STC   R15,ERRCODE             GET R15 ERROR CODE               00713000
         BAS   R4,XERRCODE             TRANSLATE ERROR CODE
         LA    R2,MBUF                 GET ADDRESS OF MESSAGE BUFFER
         MVC   0(L'R15MSG,R2),R15MSG   MOVE MESSAGE TO OUTPUT AREA
         MVC   R15MSG1(L'R15MSG2,R2),OPCODE AND PUT IN OPERATION CODE
         MVC   R15MSG3(L'R15MSG4,R2),CHAR8  AND PUT IN HEX CODE         00015800
         MVC   R15MSG5(L'R15MSG6,R2),CHAR8+4    PUT IN DEC CODE         00015800
         LA    R1,L'R15MSG             GET LENGTH OF MESSAGE
*
         SELECT                        SELECT
         CASE  OPT1,(ON,TM),SHWCATF    WHEN SHOWCAT ERROR OCCURRED...
          AR   R2,R1                    POINT TO END OF MESSAGE
          MVC  0(L'SHWCAT,R2),SHWCAT     AND ADD ACB ERR INFO MSG
          UNPK CHAR8(5),SHOWAREA+2(3)   UNPACK SHOWCAT RTN CODE
          TR   CHAR8(4),TRTABLE          AND TRANSLATE TO PRINTABLE     00015800
          MVC  SHWCAT1(L'SHWCAT2,R2),CHAR8   AND PUT IN HEX CODE        00015800
          UNPK CHAR8(5),SHOWAREA+4(3)   UNPACK SHOWCAT RSN CODE
          TR   CHAR8(4),TRTABLE          AND TRANSLATE TO PRINTABLE     00015800
          MVC  SHWCAT3(L'SHWCAT4,R2),CHAR8   AND PUT IN HEX CODE        00015800
          MVC  SHWCAT5(L'SHWCAT6,R2),SHOWAREA+6  PUT IN MODULE CODE     00015800
          LA   R3,L'SHWCAT              GET LENGTH OF SHOWCAT MSG
          AR   R1,R3                     AND ADD TO LEN OF ORIG MSG
          B    ERRMSGX                  GO PRINT OUT MESSAGE
         CASE  OPT1,(ON,TM),ACBERRF    WHEN ACB ERROR OCCURRED...
          MVC  ERRCODE,ACBERFLG         GET ACB ERROR FLAG
         CASE  OPT1,(ON,TM),RPLERRF    WHEN RPL ERROR OCCURRED...
          MVC  ERRCODE,RPLFDBKC         GET RPL FEEDBACK CODE
         CASE  OTHER                   OTHERWISE
          STC  R0,ERRCODE               GET R0 ERROR CODE               00713000
         ENDSL                         ENDSL
         BAS   R4,XERRCODE             TRANSLATE ERROR CODE
         AR    R2,R1                   POINT TO END OF MESSAGE
         MVC   0(L'ACBERR,R2),ACBERR    AND ADD ACB ERR INFO MSG
         MVC   ACBERR1(L'ACBERR2,R2),CHAR8  AND PUT IN HEX CODE         00015800
         MVC   ACBERR3(L'ACBERR4,R2),CHAR8+4    PUT IN DEC CODE         00015800
         LA    R3,L'ACBERR             GET LENGTH OF ACB INFO MSG
         AR    R1,R3                    AND ADD TO LEN OF ORIG MSG
*
         IF    OPT1,(ON,TM),RPLERRF    IF RPL ERROR OCCURRED...
          UNPK CHAR8(7),RPLFDBK(4)      UNPACK ACB ERROR FLAG
          TR   CHAR8(6),TRTABLE          AND TRANSLATE TO PRINTABLE     00015800
          AR   R2,R3                    POINT TO END OF MESSAGE
          MVC  0(L'RPLERR,R2),RPLERR     AND ADD RPL ERR INFO MSG
          MVC  RPLERR1(L'RPLERR2,R2),CHAR8  AND PUT IN HEX CODE
          LA   R3,L'RPLERR              GET LENGTH OF RPL INFO MSG
          AR   R1,R3                     AND ADD TO LEN OF ORIG MSG
         ENDIF                         ENDIF
*
ERRMSGX  NI    OPT1,X'FF'-SHWCATF-ACBERRF-RPLERRF   SET 'EM OFF
         LA    R2,MBUF                 GET START OF FULL MESSAGE
         BAS   R7,PRTMSG               GO PRT MSG ON SYSLST
         LA    R15,8                   SET RETURN CODE
* ------------------------------------------------------------------- *
FINISH   DS    0H
* ------------------------------------------------------------------- *
         TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
OPENACB  DS    0H            OPEN ACB POINTED TO BY R9
* ------------------------------------------------------------------- *
         OPEN  (R9)                    OPEN THE ACB
         IF    R15,(NZ,LTR),R15        IF ERROR
          MVC  OPCODE,=CL8'OPEN'        INDICATE OPERATION PERFORMED
          OI   OPT1,ACBERRF             SIGNAL ERROR IN ACB
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         SHOWCB ACB=(R9),AM=VSAM,OBJECT=DATA,                          +
               AREA=(S,KEYLN),LENGTH=3*4,                              +
               FIELDS=(KEYLEN,RKP,LRECL)
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'SHOWACB'     INDICATE OPERATION PERFORMED
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         BR    R4                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
CLOSEACB DS    0H            CLOSE ACB POINTED TO BY R9
* ------------------------------------------------------------------- *
         CLOSE (R9)                    CLOSE THE ACB
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  OPCODE,=CL8'CLOSE'       INDICATE OPERATION PERFORMED
          OI   OPT1,ACBERRF             SIGNAL ERROR IN ACB
          B    ERRMSG                   GO PASS BACK RETURN CODE
         ENDIF                         ENDIF
*
         BR    R4                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
XERRCODE DS    0H            TRANSLATE ERROR CODE TO HEX AND ZONED DEC.
*                                 IN = ERRCODE
*                                 WRK= R9 AND DWORD
*                                 OUT= CHAR8(2) AND CHAR8+4(3)
*                                 R4 = RETURN REGISTER
* ------------------------------------------------------------------- *
         UNPK  CHAR8(3),ERRCODE(2)     UNPACK INTO CHARACTER AREA       00015700
         TR    CHAR8(2),TRTABLE         TRANSLATE TO PRINTABLE          00015800
*
         XR    R9,R9                   CLEAR A WORK REGISTER
         IC    R9,ERRCODE              GET ERROR CODE
         CVD   R9,DWORD                CONVERT TO DECIMAL
         OI    DWORD+L'DWORD-1,X'0F'    NUETER THE SIGN
         UNPK  CHAR8+4(3),DWORD+6(2)     CHANGE TO PRINTABLE
*
         BR    R4                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
PRTMSG   DS    0H            PRINT A MESSAGE ON SYSLST
*                                 R1 = msg length
*                                 R2 = msg value
*                                 R7 = return register
* ------------------------------------------------------------------- *
         MVC   OUTPUT,=CL256' '        CLEAR PRINT LINE
         BCTR  R1,0                    DECR. FOR 0-BASED LENGTH
         EX    R1,PRTMSGX              MOVE TO I/O AREA
         PUT   PRINTER                 PRINT LINE
*
         BR    R7                      RETURN TO CALLER
PRTMSGX  MVC   OUTPUT(0),0(R2)         (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
TRTABLE  EQU   *-192                   TRANSLATE HEX2CHR AND CHR2HEX    00020100
         DC    X'00FAFBFCFDFEFF000000000000000000'                      00020200
         DC    X'00000000000000000000000000000000'                      00020300
         DC    X'00000000000000000000000000000000'                      00020400
         DC    X'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'                      00020500
*
SALEN    DC    A(DYNALENG)
         LTORG
*                                 PRINT MESSAGES
INVDLBL  DC    C'DLBL IS NOT FOR A VSAM CLUSTER.'
INVDATA  DC    C'UNABLE TO SHOW DATA COMPONENT INFO.'
RECADD   DC    C'INITIALIZING RECORD ADDED...'
RECDEL   DC    C'INITIALIZING RECORD DELETED...'
DONE     DC    C'FILE INITIALIZATION COMPLETED.'
*                                 R15 RETURN CODE
R15MSGS  DC    C'MACRO '
R15MSG1  EQU   *-R15MSGS
R15MSG2  DC    CL8' '
         DC    C' RC=X'''
R15MSG3  EQU   *-R15MSGS
R15MSG4  DC    CL2' '
         DC    C'''('
R15MSG5  EQU   *-R15MSGS
R15MSG6  DC    CL3' '
         DC    C')'
R15MSG   EQU   R15MSGS,*-R15MSGS,C'C'
*                                 SHOWCAT ERROR CODE
SHWCATS  DC    C',CR=X'''
SHWCAT1  EQU   *-SHWCATS
SHWCAT2  DC    CL4' '
         DC    C''',RS=X'''
SHWCAT3  EQU   *-SHWCATS
SHWCAT4  DC    CL4' '
         DC    C''',CM='''
SHWCAT5  EQU   *-SHWCATS
SHWCAT6  DC    CL2' '
         DC    C''''
SHWCAT   EQU   SHWCATS,*-SHWCATS,C'C'
*                                 ACB ERROR CODE
ACBERRS  DC    C',EC=X'''
ACBERR1  EQU   *-ACBERRS
ACBERR2  DC    CL2' '
         DC    C'''('
ACBERR3  EQU   *-ACBERRS
ACBERR4  DC    CL3' '
         DC    C')'
ACBERR   EQU   ACBERRS,*-ACBERRS,C'C'
*                                 RPL FEEDBACK CODES
RPLERRS  DC    C',FB=X'''
RPLERR1  EQU   *-RPLERRS
RPLERR2  DC    CL6' '
         DC    C''''
RPLERR   EQU   RPLERRS,*-RPLERRS,C'C'
*                                 LIOCS I/O AREA
IOAREA   DS    0CL121
CTLCHR   DC    CL1' '
OUTPUT   DC    CL120' '
*                                 LIOCS INTERFACES
PRINTER  DTFPR DEVADDR=SYSLST,IOAREA1=IOAREA,BLKSIZE=121,CTLCHR=ASA
* =================================================================== *
         END
