3270     TITLE 'RX3270IO - REXX SUB TO ACCESS LOCAL NON-SNA 3270'
         PRINT ON,GEN
RX3270IO AMODE 24
RX3270IO RMODE 24
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Dapsco Information Systems                          Written by:     *
* 3110 Kettering Boulevard                         Dave Leonard Clark *
* Dayton, OH  45439                                  (937)294-5331    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This external REXX function  allows  a  REXX procedure to send *
* and receive  3270  datastreams  to  and  from  (via  EXCP) a local, *
* non-sna 3270 device.   Individual  3270  fields  may  be defined as *
* multiple specifications in a compound REXX stem variable (where the *
* entire stem variable  defines  a  single  "map")  and supplied as a *
* parameter to  this  subroutine.   Multiple  maps  would  have to be *
* handled by having a different stem variable for each "map".         *
*                                                                     *
*      This function normally takes  care  of formatting the outgoing *
* 3270 datastream and  parsing  the  incoming  3270 datastream -- all *
* based  upon  the  specifications  in  the  supplied  stem variable. *
* However,  the caller may  optionally  pre-format their own outgoing *
* datastream.   This invokes a  scenario  that  can  be thought of as *
* "conversational" mode (i.e.,  an  immediate read-modified operation *
* will follow such  a pre-formatted write operation).   Also,  except *
* for AID byte and  cursor  position,  the  input datastream from the *
* "conversation" is not parsed for individual field data.             *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      Assemble this program  as  a  standard  assembler batch phase. *
* However,  it will not,  then,  be available for just any program to *
* call -- only a REXX program will be able to call it.                *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX subroutine is as easy as using the *
* REXX "CALL" statement.  The following is an example of the complete *
* process for sending  a  map  to  a  local,  non-sna 3270 device and *
* receiving the user's input.                                         *
*                                                                     *
*             /* initialize REXX program with 3270 literals */        *
*   Call RX3270IN;                                                    *
*   If rc <> 0 Then Do                                                *
*     Say " RX3270IN call error: rc = "rc", result = "result;         *
*     Exit 4;                                                         *
*   End                                                               *
*             /* define the map "header" */                           *
*   map.     = '';                                                    *
*   map._lun = D2C(lun);     /* programmer logical unit #: 000-255 */ *
*   map._wcc = RX3270_WCC.LIT_FREEKB_FRSET;  /* write control char */ *
*                                                                     *
*             /* initialize stem variable with device attributes */   *
*   Call RX3270IO 'QUERY','map.';                                     *
*   If rc <> 0 Then Do                                                *
*     Say " RX3270IO Query error: rc = "rc", result = "result;        *
*     Exit 4;                                                         *
*   End                                                               *
*             /* define map fields: row,col */                        *
*   n = RX3270FN(map._rows, map._cols, 3, 10); /* gen a field name */ *
*   map.n.attrib = RX3270_ATR.LIT_ASKIP_NORM ||,                      *
*                  RX3270_CSR.LIT_NO         ||,                      *
*                  RX3270_EXC.LIT_GREEN      ||,                      *
*                  RX3270_EXH.LIT_OFF          ;                      *
*   map.n.output = 'Enter your name:';                                *
*                                                                     *
*   n = RX3270FN(map._rows, map._cols, 3, 27); /* gen a field name */ *
*   map.n.attrib = RX3270_ATR.LIT_UNPROT_NORM||,                      *
*                  RX3270_CSR.LIT_YES        ||,                      *
*                  RX3270_EXC.LIT_YELLOW     ||,                      *
*                  RX3270_EXH.LIT_UNDERLINE    ;                      *
*   map.n.output = '';                                                *
*   my_field = n;     /* save the name for your own reference */      *
*                                                                     *
*   n = RX3270FN(map._rows, map._cols, 3, 58); /* gen a field name */ *
*   map.n.attrib = RX3270_ATR.LIT_ASKIP_NORM ||,                      *
*                  RX3270_CSR.LIT_NO         ||,                      *
*                  RX3270_EXC.LIT_DEFAULT    ||,                      *
*                  RX3270_EXH.LIT_OFF          ;                      *
*   map.n.output = '';                                                *
*                                                                     *
*             /* begin I/O processing */                              *
*   Do Until map.my_field.input <> ''                                 *
*     Call RX3270IO 'ERWRT','map.';      /* send the map */           *
*     If rc <> 0 Then Do                                              *
*       Say " RX3270IO Write error: rc = "rc", result = "result;      *
*       Exit 4;                                                       *
*     End                                                             *
*     Call RX3270IO 'READM','map.';      /* receive the map */        *
*     If rc <> 0 Then Do                                              *
*       Say " RX3270IO Read error: rc = "rc", result = "result;       *
*       Exit 4;                                                       *
*     End                                                             *
*     If map._aid == RX3270_AID.LIT_CLEAR_KEY Then Leave;             *
*   End                                                               *
*             /* end of I/O processing */                             *
*   Say " Name = "map.my_field.input;                                 *
*   Exit 0;                                                           *
*                                                                     *
* Note that the passed stem variable name must include the period and *
* have a maximum length of nine characters -- including the period.   *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                INPUT                                *
*                                                                     *
* On input, the first argument may have one of these eleven values:   *
*                                                                     *
*   QUERY       queries the device to see what it supports (must      *
*                 be done for each "map" and if 'stem.lun' changes),  *
*   ERASE       erases the screen without writing any data but        *
*                 does use the specified write control character,     *
*   ERALT       same as 'ERASE', except alternate screen size used,   *
*   ERAUP       erases only the unprotected fields on the screen      *
*                 but writes no additional data,                      *
*   ERWRT       erases the screen and performs a write operation,     *
*   ERWRA       same as 'ERWRT', except alternate screen size used,   *
*   WRITE       is a write operation (does not erase the screen),     *
*   WRITA       same as 'WRITE', except alternate buffer size used,   *
*   WRTDS       indicates the outgoing datastream is pre-formatted    *
*                 and the input datastream is not to be parsed,       *
*   READM       is a read-modified operation (normally parsed), and   *
*   READB       is a read-buffer operation (except for AID byte and   *
*                 cursor position, the input buffer is not parsed).   *
*                                                                     *
* Note that the sole purpose of  the 'WRITA' function is to tell this *
* program to perform validation  based  on  the alternate buffer size *
* instead of the default  buffer  size.   In other words,  errors can *
* occur if you use this  function  and  the display is not already in *
* alternate screen mode or the LUN is assigned to a 3270 printer.     *
*                                                                     *
* Also, the following stem variable entries are referenced:           *
*                                                                     *
*   ..._lun     supplies the numeric portion (in binary) of the       *
*                 assigned programmer logical unit no. (e.g., SYS000) *
*                 for any I/O operations using this map definition;   *
*   ..._ccc     for 'WRTDS', supplies the full pre-formatted outgoing *
*                 datastream (first char. is channel command code);   *
*   ..._wcc     supplies the write control character for any          *
*                 write operations (except 'WRTDS');                  *
*   ...n.attrib specifies 4-bytes for the values of the (1) field,    *
*                 (2) cursor, (3) color, and (4) hilite attributes    *
*                 (always in that specific order); and,               *
*   ...n.output specifies the field output data for display.          *
*                                                                     *
* Note that 'n' (above)  is  a  RX3270FN-generated field name with an *
* "FNnnnn" format:  where, 'nnnn' is the 3270 buffer position.   This *
* format is critical as this function gathers up the stem.FN* entries *
* to build the output map and to parse the input map.                 *
*                                                                     *
* Also note that although the  attribute variable must always be four *
* bytes in length, the extended color and hilite will only be used if *
* the queried device indicates that they are supported.  Also, if the *
* first byte is binary zero (to  indicated there should be no field), *
* then this function uses  the  SET_ATTRIBUTE order instead of either *
* START_FIELD or  START_FIELD_EXTENDED  --  but,  again,  only if the *
* queried device indicates that extended attributes are supported.    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
* On query output, the following values are returned (and must not be *
* modified by the REXX program for any reason whatsoever):            *
*                                                                     *
*   ..._raw     the un-parsed input from the query operation,         *
*   ..._qry     indicates the device has been queried (1),            *
*   ..._dev     indicates the device type ('D'isplay, 'P'rinter),     *
*   ..._bufsz   the default size                                      *
*   ..._bufsa     and the alternate size of the device buffer,        *
*   ..._rows    the number of default rows                            *
*   ..._cols      and default columns supported by the device,        *
*   ..._rowa    the number of alternate rows                          *
*   ..._cola      and alternate columns supported by the device,      *
*   ..._exc     indicates whether the device supports extended color  *
*   ..._exh       and extended hilite (1) or not (0).                 *
*                                                                     *
* On other output, the following stem variable entries are returned:  *
*                                                                     *
*   ..._raw     the datastream sent for a write operation or          *
*                 the un-parsed input from a read operation,          *
*   ..._aid     the attention identifier returned and the             *
*   ..._csr       relative cursor position on a read operation, and   *
*   ...n.input  is the value of any modified field input received.    *
*                                                                     *
* In addition,  the  RX3270IO  subroutine  also  sets  the  REXX "RC" *
* special variable with a value  indicating the success or failure of *
* the requested operation.   You  should  analyze the "RC" variable's *
* value for errors.  Those values are as follows:                     *
*                                                                     *
*   -16  internal or device buffer overflow                           *
*   -9   at least one stem.n.output is more than 256 bytes            *
*          or more than 256 field data bytes found on input           *
*   -8   at least one stem.n.attrib is not four bytes long            *
*   -7   stem._exh is invalid                                         *
*   -6   stem._exc is invalid                                         *
*   -5   stem._bufsz (or _bufsa) is invalid                           *
*   -4   stem._wcc or stem._ccc is invalid                            *
*   -3   stem._lun assigned device has not been queried               *
*   -2   stem._lun not assigned to a 3270 device                      *
*   -1   stem._lun is invalid                                         *
*    0   no errors occurred                                           *
*    4   argument error -- missing, too many, too long, or no period  *
*    8   internal REXX function failed                                *
*          also, the failing REXX function's register 15 value        *
*          is returned in the REXX "RESULT" special variable          *
*   12   REXX environment not present                                 *
*   14   3270 appendage routine not loaded                            *
*   16   a device error occurred                                      *
*          also, the device error code                                *
*          is returned in the REXX "RESULT" special variable          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PWORK8   DS    0PL8                    PACKED DOUBLE-WORD
AWORK4   DS    F
         DS    F
EWORK8   DS    CL8                     EDITED WORK FIELD
*
MYCCB    CCB   SYS000,MYCCW
MYCCW    CCW   X'00',SCRBUF,X'20',L'SCRBUF
MYTECB   TECB
MYPUB    DS    XL8                     STORAGE FOR PUB DSECT
*
SAVEEVAL DS    F                       EVALUATION BLOCK ADDRESS
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
BUFSZ    DS    F
BUFEND   DS    F
STEMLENG DS    F                       STEM NAME LENGTH
STEMNAME DS    CL9                     STEM NAME
OPT      DS    CL5                     FUNCTION CODE
QRYFLAG  DS    XL1                     QUERY FLAGS
CCBQUEUE DF    B'10000000',QRYFLAG     QUEUED I/O PENDING
TECBWAIT DF    B'01000000',QRYFLAG     USE TECB FOR CONTROLLED WAIT
SET_81   DF    B'00100000',QRYFLAG     QUERY x'81' (USABLE AREA)
SET_A6   DF    B'00010000',QRYFLAG     QUERY x'A6' (IMPLICIT PARTITION)
SET_KLZ  DF    B'00001000',QRYFLAG     KEEP LEADING ZEROES
SET_EXC  DF    B'00000010',QRYFLAG     EXTENDED COLOR
SET_EXH  DF    B'00000001',QRYFLAG     EXTENDED HILITE
*
         DS    0D                      ALIGNMENT
TRTBL    DS    CL256                   TEMPORARY TRANSLATE TABLE
VARNAME  DS    CL250                   VARIABLE NAME
VARDATA  DS    CL256                   VARIABLE VALUE
*
         DS    0D                      ALIGNMENT
USRCCC   DS    XL1                     USER CHANNEL COMMAND CODE
SCRBUF   DS    CL4096                  STORAGE FOR 3270 BUFFER
         DS    D                       PADDING TO TEST FOR OVERFLOW
*
DYNALENG EQU   *-DYNASTOR              LENGTH OF DYNAMIC STORAGE
* =================================================================== *
         EJECT
* =================================================================== *
         IJBPUB                        PHYSICAL UNIT BLOCK
         MAPDEVTY                      DEVICE TYPE EQUATES
         ARXENVB                       REXX ENVIRONMENT BLOCK
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXEFPL                       EXTERNAL FUNCTION PARAMETER LIST
         ARXEVALB                      REXX EVALUATION BLOCK
         ARXARGTB                      REXX PARSED ARGUMENT TABLE
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* =================================================================== *
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK
R1       EQU   1                       ON ENTRY, REXX EFPL REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9                       REXX EXTERNAL ENTRY-POINT VECTOR
R10      EQU   10                      SHARED VARIABLES CONTROL BLOCK
R11      EQU   11                      RX3270IO CSECT BASE REG 1
R12      EQU   12                      RX3270IO CSECT BASE REG 2
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
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
RX3270IO HEADER VERSION='1.2',AUTHOR='DLC',DATE='(C)MAR04',            X
               BASE=(R11,R12),SVAREA=SALEN   ALLOC GETVIS DYNAMIC STG
* =================================================================== *
         USING DYNASTOR,R13            POINT TO GETVIS DYNAMIC STORAGE
*
         SAM31                         SWITCH TO 31-BIT ADDRESSING
*                                       BECAUSE OF DEBUGGER: TRAPPER
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5             ESTABLISH ADDRESSABILITY
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RX3270IO: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LA   R15,12                  SET ABNORMAL RETURN CODE
          B    RETR15                  GO RETURN ERROR
         ENDIF
         LR    R8,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R8                 ESTABLISH ADDRESSABILITY
         L     R9,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R9              ESTABLISH ADDRESSABILITY
         ST    R5,SAVEENVB             SAVE ADDR. OF ENV. BLOCK
         DROP  R5                      REMOVE ADDR. TO ENV. BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R10,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R10              SHARED VARIABLES CONTROL BLOCK
*
         LA    R0,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R1,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R3,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R0,R3,SHVPARMB          PUT IN PARM BLOCK
         OI    SHVPLAST,X'80'          SET END OF PARM BLOCK
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         XC    VARNAME,VARNAME         CLEAR BUFFER FOR VAR. NAME
         XC    VARDATA,VARDATA         CLEAR BUFFER FOR VAR. VALUE
         LA    R0,VARNAME              GET ADDRESS FOR SHVNAMA
         LA    R1,L'VARNAME            GET LENGTH  FOR SHVNAML
         LA    R2,VARDATA              GET ADDRESS FOR SHVVALA
         LA    R3,L'VARDATA            GET LENGTH  FOR SHVVALL
         STM   R0,R3,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R1,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R3,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
* INITIALIZE A TRANSLATE TABLE
*
         XR    R1,R1
         LA    R2,TRTBL
         WHILE R1,(LT,CH),=H'256'
          STC  R1,0(,R2)
          LA   R1,1(,R1)
          LA   R2,1(,R2)
         WEND
         LA    R2,TRTBL+C'a'           Translate lower-case
         MVC   0(9,R2),=C'ABCDEFGHI'      to upper-case
         LA    R2,TRTBL+C'j'           Translate lower-case
         MVC   0(9,R2),=C'JKLMNOPQR'      to upper-case
         LA    R2,TRTBL+C's'           Translate lower-case
         MVC   0(8,R2),=C'STUVWXYZ'       to upper-case
         LA    R2,TRTBL+X'FA'          Translate unpacked data
         MVC   0(6,R2),=C'ABCDEFGHI'      to valid hex
*
* POINT TO ARGUMENT(S)
*
         L     R6,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R6
         L     R7,EFPLEVAL             REXX EVALUATION BLOCK POINTER
         DROP  R8                      EFP LIST NO LONGER NEEDED
         L     R7,0(,R7)               REXX EVALUATION BLOCK
         ST    R7,SAVEEVAL               SAVE FOR LATER
         LR    R7,R6                   REXX ARGUMENT STRING
         USING ARGSTRING,R7
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    OPT,OPT
         XC    STEMLENG,STEMLENG
*
         WHILE ARGTABLE_END,(NE,CLC),=XL8'FFFFFFFFFFFFFFFF'

         L     R2,ARGTABLE_ARGSTRING_LENGTH
         L     R4,ARGTABLE_ARGSTRING_PTR
         IF    OPT,EQ,X'00'            IF NO FIRST ARG YET
          IF   R2,(NZ,LTR),R2           IF ARG SUPPLIED,
           MVC OPT,0(R4)                 GET CALLER ARG
           TR  OPT,TRTBL                  AND TRANSLATE TO UPPER-CASE
          ENDIF                         ENDIF
          IF   OPT,(NE,CLC),=C'QUERY'   IF
          AND  OPT,(NE,CLC),=C'ERASE'    FIRST
          AND  OPT,(NE,CLC),=C'ERALT'     PARM
          AND  OPT,(NE,CLC),=C'ERAUP'      IS
          AND  OPT,(NE,CLC),=C'ERWRA'       NOT
          AND  OPT,(NE,CLC),=C'ERWRT'        ONE
          AND  OPT,(NE,CLC),=C'READB'         OF
          AND  OPT,(NE,CLC),=C'READM'          THESE
          AND  OPT,(NE,CLC),=C'WRTDS'           ELEVEN
          AND  OPT,(NE,CLC),=C'WRITA'            FUNCTION
          AND  OPT,(NE,CLC),=C'WRITE'             CODES
           LA  R5,4                      SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ELSE                          ELSE
          IF   STEMLENG,(NE,CLC),=F'0'  IF ALREADY GOT STEM ARG
           LA  R5,4                      SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R2,STEMLENG              SAVE STEM NAME LENGTH
          MVC  STEMNAME,0(R4)           SAVE STEM NAME
          TR   STEMNAME,TRTBL           TRANSLATE TO UPPER-CASE
          LA   R3,STEMNAME-1            POINT TO LAST CHARACTER
          AR   R3,R2                     OF STEM NAME VALUE
          IF   STEMLENG,(H,CLC),=F'9',OR,  IF LEN > 9                  X
               0(R3),NE,C'.'            OR LAST CHAR NOT A PERIOD
           LA  R5,4                      SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF

         LA    R6,L'ARGTABLE_END(,R6)  ADVANCE TO NEXT ARGUMENT
         LR    R7,R6
         WEND
*
         DROP  R6                      ARGUMENT TABLE NO LONGER NEEDED
         DROP  R7                      ARGUMENT STRING NO LONGER NEEDED
*
* READY TO PROCEED?
*
         IF    OPT,EQ,X'00',OR,        IF NO ARGS                      X
               STEMLENG,(EQ,CLC),=F'0' OR DON'T HAVE STEM ARG
          LA   R5,4                     SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
* OUTPUT TO SYSLST -- ARXSAY
*
         LA    R0,SAYPARM1             GET ADDRESS OF PARM 1
         LA    R1,SAYPARM2             GET ADDRESS OF PARM 2
         LA    R2,SAYPARM3             GET ADDRESS OF PARM 3
         STM   R0,R2,SAYPARMB          PUT IN PARM BLOCK
         OI    SAYPLAST,X'80'          SET END OF PARM BLOCK
*
         MVC   SAYPARM1,=C'WRITE   '   SET PARM 1
         LA    R0,VARDATA              GET ADDRESS FOR PARM 2
         LA    R1,L'VARDATA            GET LENGTH FOR PARM 3
         STM   R0,R1,SAYPARM2          PUT IN PARMS 2 AND 3
*
         IF    OPT,(EQ,CLC),=C'QUERY'  IF FUNCTION IS 'QUERY'
          MVC  VARDATA+0(18),=C'RX3270IO Load at: '
          ST   R11,AWORK4               STORE BASE ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+18(8),EWORK8     THEN PUT IN MSG AREA
          LHI  R3,26                    SET LENGTH OF OUTPUT
          BAS  R5,SAYIT                 GO SAY IT
         ENDIF                         ENDIF
*
* SET STEM NAME PREFIX (FOR VARIABLE RETRIEVAL/CREATION)
*
         LA    R4,VARNAME              POINT TO VAR NAME BUFFER
         MVC   0(L'STEMNAME,R4),STEMNAME  MOVE IN STEM NAME
         A     R4,STEMLENG                POINT TO AFTER STEM NAME
*
         XC    QRYFLAG,QRYFLAG          SET ALL BITS OFF
*
* LOAD APPENDAGE ROUTINE
*
         IF    CARTNBEG,(EQ,CLC),=F'0' IF NOT YET LOADED...
          SAM24                         SWITCH TO 24-BIT MODE
          CDLOAD RX3270IO               MAKE THIS RTN STAY IN MEMORY
          IF   R15,(NZ,LTR),R15         IF CDLOAD ERROR
           LH  R5,=H'13'                 INDICATE LOAD FAILURE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          CDLOAD RX3270AP,PAGE=YES      LOAD APPENDAGE ROUTINE
          IF   R15,(NZ,LTR),R15         IF CDLOAD ERROR
           LH  R5,=H'14'                 INDICATE APPENDAGE FAILURE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R1,CARTNEP               SAVE ENTRY POINT
          ST   R0,CARTNBEG              SAVE LOAD POINT
          BCTR R14,0                    MODULE LENGTH MINUS ONE
          ST   R14,CARTNLEN             SAVE MODULE LENGTH
          LA   R1,CARTNBEG              POINT TO PFIX PARM BLOCK
          PFIX (1),RETURN=YES           PFIX PROGRAM STORAGE
          IF   R15,(NZ,LTR),R15         IF PFIX ERROR
           PDUMP CARTNBEG,CARTNFLG+3     DUMP PARM BLOCK
           LH  R5,=H'15'                 INDICATE APPENDAGE FAILURE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          SAM31                         SWITCH TO 31-BIT MODE
          L    R1,=XL4'FF000000'        REQUEST KEY 0 STATE
          SVC  13                       ISSUE AUTHORIZE REQ
          L    R1,CARTNEP               GET ENT.PNT OF APPENDAGE RTN.
          SVC  73                       ALLOW CHANNEL APPENDAGE RTN.
          L    R1,=XL4'FF0000FF'        RELEASE KEY 0 STATE
          SVC  12                       ISSUE UN-AUTHORIZE REQ
         ENDIF                         ENDIF
*
* INITIALIZE CCB
*
         XC    MYCCB,MYCCB             CLEAR CCB
         LA    R0,MYCCW                GET ADDRESS OF CCW
         L     R1,CARTNEP              GET ENT.PNT OF APPENDAGE RTN.
         STM   R0,R1,MYCCB+8           PUT IN CHANNEL CONTROL BLOCK
         MVI   MYCCB+12,X'40'          SET FLAG FOR APPENDAGE RTN.
*
* RETRIEVE LOGICAL UNIT NUMBER
*
         MVC   0(L'DEVLUN,R4),DEVLUN   ADD QUALIFIER TO STEM NAME
         L     R3,STEMLENG             COMPUTE LENGTH OF STEM NAME
         LA    R3,L'DEVLUN(,R3)        PLUS LENGTH OF QUALIFIER
         BAS   R2,GETVAR               RETRIEVE THE VARIABLE VALUE
         IF    R5,(NZ,LTR),R5,OR,      IF VARIABLE NOT FOUND           X
               SHVVALL,(NE,CLC),=F'1'  OR VALUE LENGTH NOT = 1
          LH   R5,=H'-1'                INDICATE INVALID STEM._LUN
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
         MVI   MYCCB+6,X'01'           UNIT CLASS (SYS000)
         MVC   MYCCB+7(1),VARDATA      PUT LGL UNIT NUMBER IN CCB
*
* VERIFY ASSIGNED TO A 3270 DEVICE
*
         EXTRACT ID=PUB,SEL=(S,MYCCB+6),AREA=(S,MYPUB),LEN=L'MYPUB
         IF    R15,(NZ,LTR),R15        IF BAD RETURN CODE
          LA   R5,8                     SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         LA    R6,MYPUB                ESTABLISH
         USING IJBPUB,R6                ADDRESSABILITY
         IF    IJBPDEVT,NE,T3277,AND,  IF NOT 3277                     X
               IJBPDEVT,NE,T3270       AND NOT 3270
          LH   R5,=H'-2'                INDICATE NON-3270 STEM._LUN
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
         DROP  R6
*
* EXCEPT FOR QUERY FUNCTION...
*
         IF    OPT,(NE,CLC),=C'QUERY'  IF FUNCTION IS NOT 'QUERY'
*
* ...RETRIEVE QUERY INDICATOR
*
          MVC  0(L'DEVQRY,R4),DEVQRY    ADD QUALIFIER TO STEM NAME
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'DEVQRY(,R3)         PLUS LENGTH OF QUALIFIER
          BAS  R2,GETVAR                RETRIEVE THE VARIABLE VALUE
          IF   R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND          X
               VARDATA,NE,C'1'          OR QUERY VALUE NOT = '1'
           LH  R5,=H'-3'                 INDICATE INVALID STEM._QRY
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
*
         ENDIF                         ENDIF
*
* ONLY FOR WRTDS FUNCTION...
*
         IF    OPT,(EQ,CLC),=C'WRTDS'  IF FUNCTION IS 'WRTDS'
*
* ...RETRIEVE THE _CCC VARIABLE
*
          LA   R2,USRCCC                GET ADDRESS FOR SHVVALA
          L    R3,BUFLEN                GET LENGTH FOR SHVVALL
          LA   R3,1(,R3)                  PLUS 1 (FOR CCC)
          STM  R2,R3,SHVVALA            PUT IN SHV CONTROL BLOCK
          MVC  0(L'RAWOUT,R4),RAWOUT    ADD QUALIFIER TO STEM NAME
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'RAWOUT(,R3)         PLUS LENGTH OF QUALIFIER
          BAS  R2,GETVAR                RETRIEVE THE VARIABLE VALUE
          IF   R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND          X
               SHVVALL,(NH,CLC),=F'1'   OR VALUE LENGTH NOT > 1
           LH  R5,=H'-4'                 INDICATE INVALID STEM._CCC
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          MVC  BUFEND,SHVVALL           SAVE LENGTH OF USER DATA
          LA   R2,VARDATA               GET ADDRESS FOR SHVVALA
          LA   R3,L'VARDATA             GET LENGTH  FOR SHVVALL
          STM  R2,R3,SHVVALA            PUT IN SHV CONTROL BLOCK
*
         ENDIF                         ENDIF
*
* BEGIN PROCESSING FOR ALL WRITE-TYPE FUNCTIONS
*       (WHICH ARE ERASE, ERALT, ERAUP, ERWRT, ERWRA,
*                  WRITE, WRITA, WRTDS, AND QUERY)
*
         IF    OPT(4),(NE,CLC),=C'READ'
*
* INITIALIZE CCW
         IF    OPT,(EQ,CLC),=C'QUERY'  IF FUNCTION IS 'QUERY'
          LA   R0,QUERY                 GET ADDRESS OF QUERY BUFFER
          LA   R1,QUERYL                GET LENGTH OF QUERY BUFFER
         ELSE                          ELSE
          LA   R0,SCRBUF                GET ADDRESS OF SCREEN BUFFER
          L    R1,BUFLEN                GET LENGTH OF SCREEN BUFFER
         ENDIF                         ENDIF
         STM   R0,R1,MYCCW             PUT IN CHANNEL COMMAND WORD
         MVI   MYCCW+4,X'20'           SUPPRESS WLR
*
         SELECT
         WHEN  OPT,(EQ,CLC),=C'ERASE',OR,                              X
               OPT,(EQ,CLC),=C'ERWRT'
          MVC  MYCCW+0(1),LCLERWRT     PERFORM ERASE-WRITE
         WHEN  OPT,(EQ,CLC),=C'ERALT',OR,                              X
               OPT,(EQ,CLC),=C'ERWRA'
          MVC  MYCCW+0(1),LCLERWRA     PERFORM ERASE-WRITE-ALTERNATE
         WHEN  OPT,(EQ,CLC),=C'ERAUP'
          MVC  MYCCW+0(1),LCLERAUP     PERFORM ERASE-ALL-UNPROTECTED
         WHEN  OPT,(EQ,CLC),=C'QUERY'
          MVC  MYCCW+0(1),LCLWRSTR     PERFORM WRITE-STRUCTURED
         WHEN  OTHER
          IF   OPT,(NE,CLC),=C'WRTDS'  IF FUNCTION IS NOT 'WRTDS'
           MVC MYCCW+0(1),LCLWRITE      PERFORM WRITE
          ELSE                         ELSE
           MVC MYCCW+0(1),USRCCC        PUT CHANNEL COMMAND IN CCW
           L   R0,BUFEND                GET LEN OF USER DATA
           BCTR R0,0                     MINUS 1 FOR CCC
           STH R0,MYCCW+6               AND STORE IN CCW
          ENDIF                        ENDIF
         ENDSL
*
* BUILD OUTGOING DATASTREAM
         SELECT
*
         WHEN  OPT,(EQ,CLC),=C'WRTDS'
          MVC  OPT,=C'READI'            PERFORM READ-IMMEDIATE NEXT
*
         WHEN  OPT,(EQ,CLC),=C'QUERY'
          MVC  OPT,=C'READQ'            PERFORM READ-QUERY NEXT
*
         WHEN  OTHER
*
* ...retrieve write control char
          MVC  0(L'DEVWCC,R4),DEVWCC    ADD QUALIFIER TO STEM NAME
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'DEVWCC(,R3)         PLUS LENGTH OF QUALIFIER
          BAS  R2,GETVAR                RETRIEVE THE VARIABLE VALUE
          IF   R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND          X
               SHVVALL,(NE,CLC),=F'1'   OR VALUE LENGTH NOT = 1
           LH  R5,=H'-4'                 INDICATE INVALID STEM._WCC
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          MVC  SCRBUF(1),VARDATA        PUT WCC IN BUFFER
*
* ...that's all we need -- if this is an erase-only function
          IF   OPT(3),(EQ,CLC),=C'ERA'  IF 'ERASE', 'ERALT', OR 'ERAUP'
*
           MVC MYCCW+6(2),=H'1'          SET BUFFER LENGTH TO 1
*
          ELSE
*
* ...retrieve buffer size
           IF  OPT,(EQ,CLC),=C'WRITA'    IF FUNCTION IS 'WRITA'
            MVC 0(L'DEVBSA,R4),DEVBSA     ADD QUALIFIER TO STEM NAME
           ELSE                          ELSE
            MVC 0(L'DEVBSZ,R4),DEVBSZ     ADD QUALIFIER TO STEM NAME
           ENDIF                         ENDIF
           L   R3,STEMLENG               COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVBSZ(,R3)          PLUS LENGTH OF QUALIFIER
           BAS R2,GETVAR                 RETRIEVE THE VARIABLE VALUE
           IF  R5,(NZ,LTR),R5,OR,        IF VARIABLE NOT FOUND         X
               SHVVALL,(NE,CLC),=F'4'    OR VALUE LENGTH NOT = 4
            LH R5,=H'-5'                  INDICATE INVALID STEM._BUFSZ
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           LA  R2,VARDATA                POINT TO VARIABLE DATA
           L   R3,SHVVALL                GET LENGTH OF VARIABLE DATA
           BAS R15,ZON2REG3              CONVERT THAT TO BINARY IN R3
           BCTR R3,0                     DECREMENT FOR ZERO OFFSET
           ST  R3,BUFSZ                   AND SAVE THE RESULT
*
* ...retrieve extended color setting
           MVC 0(L'DEVEXC,R4),DEVEXC     ADD QUALIFIER TO STEM NAME
           L   R3,STEMLENG               COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVEXC(,R3)          PLUS LENGTH OF QUALIFIER
           BAS R2,GETVAR                 RETRIEVE THE VARIABLE VALUE
           IF  R5,(NZ,LTR),R5,OR,        IF VARIABLE NOT FOUND         X
               SHVVALL,(NE,CLC),=F'1'    OR VALUE LENGTH NOT = 1
            LH R5,=H'-6'                  INDICATE INVALID STEM._EXC
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           IF  VARDATA,EQ,C'1'           IF EXTENDED COLOR SUPPORTED
            SF SET_EXC                    INDICATE EXT. COLOR SUPPORT
           ENDIF                         ENDIF
*
* ...retrieve extended hilite setting
           MVC 0(L'DEVEXH,R4),DEVEXH     ADD QUALIFIER TO STEM NAME
           L   R3,STEMLENG               COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVEXH(,R3)          PLUS LENGTH OF QUALIFIER
           BAS R2,GETVAR                 RETRIEVE THE VARIABLE VALUE
           IF  R5,(NZ,LTR),R5,OR,        IF VARIABLE NOT FOUND         X
               SHVVALL,(NE,CLC),=F'1'    OR VALUE LENGTH NOT = 1
            LH R5,=H'-7'                  INDICATE INVALID STEM._EXH
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           IF  VARDATA,EQ,C'1'           IF EXTENDED HILITE SUPPORTED
            SF SET_EXH                    INDICATE EXT. HILITE SUPPORT
           ENDIF                         ENDIF
*
* ...build output 3270 buffer
           LA  R6,SCRBUF+1               START AFTER WCC
           LA  R7,1                       AND ITS LENGTH
           XR  R8,R8                     CREATE COUNTER FOR FIELD NAMES
           SF  SET_KLZ                   REG2ZON3 KEEP LEADING ZEROES
           MVC 0(2,R4),=C'FN'            ADD QUALIFIER TO STEM NAME
*
           WHILE R8,(LT,C),BUFSZ        LOOP WITHIN BUFFER SIZE
            LR  R2,R8                    COPY COUNTER TO REG2
            LA  R3,2(,R4)                POINT TO VARIABLE NAME AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            MVC 6(L'FLDATR,R4),FLDATR    ADD QUALIFIER TO FIELD NAME
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,6(,R3)                 PLUS LENGTH OF FIELD NAME
            LA  R3,L'FLDATR(,R3)          PLUS LENGTH OF QUALIFIER
            BAS R2,GETVAR                RETRIEVE THE VARIABLE VALUE
            IF  R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND         X
               SHVVALL,(NE,CLC),=F'4'    OR VALUE LENGTH NOT = 4
             IF SHVRET,EQ,SHVNEWV,OR,     IF NEW VARIABLE              X
               SHVVALL,(EQ,CLC),=F'0'     OR VALUE LENGTH = 0
              LA R8,1(,R8)                 NEXT FIELD
              WLOOP                        LOOP
             ENDIF                        ENDIF
             LH R5,=H'-8'                 SET INVALID STEM.N.ATTRIB
             B  RETERR                    GO RETURN ERROR
            ENDIF                        ENDIF

            MVC 0(L'SCRSETBA,R6),SCRSETBA  PUT ORD IN SCREEN BUFFER
            LA  R6,L'SCRSETBA(,R6)       ADD TO BUFFER POINTER
            LA  R7,L'SCRSETBA(,R7)        AND TO BUFFER LENGTH

            STH R8,0(,R6)                PUT ADR IN SCREEN BUFFER
            LA  R6,2(,R6)                ADD TO BUFFER POINTER
            LA  R7,2(,R7)                 AND TO BUFFER LENGTH

            IF  SET_EXC,(NOT,TF),$NOOP,OR,SET_EXH,(NOT,TF),$NOOP
             MVC 0(L'SCRSTFLD,R6),SCRSTFLD PUT ORD IN SCREEN BUFFER
             MVC 1(1,R6),VARDATA+0         PUT ATR IN SCREEN BUFFER
             LA R6,L'SCRSTFLD(,R6)         ADD TO BUFFER POINTER
             LA R7,L'SCRSTFLD(,R7)          AND TO BUFFER LENGTH
            ELSE
             IF VARDATA+0,EQ,X'00'        IF NO FIELD ATTRIBUTE
              IF VARDATA+2,EQ,X'00',AND,VARDATA+3,EQ,X'00'
               MVC 0(L'SCRSETAC,R6),SCRSETAC PUT ORD IN SCREEN BUFFER
               MVC 1(2,R6),=x'0000'          PUT ALL IN SCREEN BUFFER
               LA R6,L'SCRSETAC(,R6)         ADD TO BUFFER POINTER
               LA R7,L'SCRSETAC(,R7)          AND TO BUFFER LENGTH
              ELSE
               MVC 0(L'SCRSETAC,R6),SCRSETAC PUT ORD IN SCREEN BUFFER
               MVC 2(1,R6),VARDATA+2         PUT CLR IN SCREEN BUFFER
               LA R6,L'SCRSETAC(,R6)         ADD TO BUFFER POINTER
               LA R7,L'SCRSETAC(,R7)          AND TO BUFFER LENGTH
               MVC 0(L'SCRSETAH,R6),SCRSETAH PUT ORD IN SCREEN BUFFER
               MVC 2(1,R6),VARDATA+3         PUT HLT IN SCREEN BUFFER
               LA R6,L'SCRSETAH(,R6)         ADD TO BUFFER POINTER
               LA R7,L'SCRSETAH(,R7)          AND TO BUFFER LENGTH
              ENDIF
             ELSE                         ELSE START FIELD EXTENDED
              MVC 0(L'SCRSTFEX,R6),SCRSTFEX PUT ORD IN SCREEN BUFFER
              MVC 3(1,R6),VARDATA+0         PUT ATR IN SCREEN BUFFER
              MVC 5(1,R6),VARDATA+2         PUT CLR IN SCREEN BUFFER
              MVC 7(1,R6),VARDATA+3         PUT HLT IN SCREEN BUFFER
              LA R6,L'SCRSTFEX(,R6)         ADD TO BUFFER POINTER
              LA R7,L'SCRSTFEX(,R7)          AND TO BUFFER LENGTH
             ENDIF
            ENDIF

            IF  VARDATA+1(1),(EQ,CLC),SCRCURSR
             MVC 0(L'SCRCURSR,R6),SCRCURSR PUT ORD IN SCREEN BUFFER
             LA R6,L'SCRCURSR(,R6)         ADD TO BUFFER POINTER
             LA R7,L'SCRCURSR(,R7)          AND TO BUFFER LENGTH
            ENDIF

            MVC 6(L'FLDOUT,R4),FLDOUT    ADD QUALIFIER TO FIELD NAME
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,6(,R3)                 PLUS LENGTH OF FIELD NAME
            LA  R3,L'FLDOUT(,R3)          PLUS LENGTH OF QUALIFIER
            BAS R2,GETVAR                RETRIEVE THE VARIABLE VALUE
            IF  R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND         X
               SHVVALL,(EQ,CLC),=F'0'    OR VALUE LENGTH = 0
             IF SHVRET,EQ,SHVNEWV,OR,     IF NEW VARIABLE              X
               SHVVALL,(EQ,CLC),=F'0'     OR VALUE LENGTH = 0
              LA R8,1(,R8)                 NEXT FIELD
              WLOOP                        LOOP
             ENDIF                        ENDIF
             LH R5,=H'-9'                 SET INVALID STEM.N.OUTPUT
             B  RETERR                    GO RETURN ERROR
MV2FLD   MVC   0(0,R6),VARDATA    (SEE "EX" INSTRUCTION BELOW)
            ENDIF                        ENDIF

            L   R3,SHVVALL               GET LENGTH OF OUTPUT DATA
            BCTR R3,0                    DECR. FOR ZERO OFFSET
            EX  R3,MV2FLD                MOVE TO SCREEN BUFFER
            LA  R3,1(,R3)                INCR. FOR NORMAL OFFSET
            AR  R6,R3                    ADD TO BUFFER POINTER
            AR  R7,R3                     AND TO BUFFER LENGTH

            IF R7,(HT,C),=A(L'SCRBUF)    IF R7 > INTERNAL BUFFER
*              R7,(HT,C),BUFSZ,OR,       OR R7 > DEVICE BUFER
             LR R15,R7                    RETURN SIZE TO CALLER
             LH R5,=H'-16'                INDICATE BUFFER OVERFLOW
             B  RETERR                    GO RETURN ERROR
            ENDIF                        ENDIF

            LA R8,1(,R8)                 NEXT FIELD
           WEND                         LOOP
*
           LA  R8,4095                   RECREATE DYNASTOR
           LA  R8,1(R8,R13)               ADDRESS IN REG 8
*
           CF  SET_KLZ                   REG2ZON3 NO LEADING ZEROES
           STH R7,MYCCW+6                AND STORE BUFFER LENGTH IN CCW
          ENDIF                         ENDIF
*
         ENDSL                         END SELECT
*
* WRITE DATA TO 3270 DEVICE
         LA    R1,MYCCB                SET ADDRESS OF CCB
         IF    CCBQUEUE,(ON,TF),$NOOP  IF ALREADY QUEUED FOR READ
          SVC  25                       DEQUE CHANNEL QUEUE (HIO)
          CF   CCBQUEUE                 CLEAR THE FLAG
         ENDIF                         ENDIF
         EXCP  (1)                     START THE I/O OPERATION
         WAIT  (1)                     WAIT FOR I/O COMPLETION
         IF    MYCCB+4(2),(NE,CLC),=X'0C00' IF NOT CE+DE, ERROR
          LA   R5,14                    SET RC VALUE
          XR   R15,R15                  CLEAR REGISTER
          ICM  R15,B'0011',MYCCB+4      SET RESULT VALUE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         ENDIF                         END ALL WRITE-TYPE PROCESSING
*
* BEGIN PROCESSING FOR ALL READ-TYPE FUNCTIONS
*       (WHICH ARE READB, READI, READM, AND READQ)
*
         IF    OPT(4),(EQ,CLC),=C'READ'
*
* INITIALIZE CCW
         LA    R0,SCRBUF               GET ADDRESS OF BUFFER
         L     R1,BUFLEN               GET MAX LENGTH OF BUFFER
         STM   R0,R1,MYCCW+0           PUT IN CHANNEL COMMAND WORD
         MVI   MYCCW+4,X'20'           SUPPRESS WLR
*
         IF    OPT,(NE,CLC),=C'READB'  IF NOT READ-BUFFER
         AND   OPT,(NE,CLC),=C'READQ'  AND NOT READ-QUERY, NOOP WAIT
*
* WAIT FOR 3270 ATTN SIGNAL
          LA   R1,MYCCB                 SET CCB ADDRESS
          IF   CCBQUEUE,(NOT,TF),$NOOP  NEED QUEUEING, IF NOT QUEUED
           MVC MYCCW+0(1),LCLNOOP        USE NOOP FOR CHANNEL QUEUE
           MVC MYCCW+6(2),=H'1'          WITH I/O LENGTH OF 1
           EXCP (1)                      PUT IN CHANNEL QUEUE
           SF  CCBQUEUE                  SET QUEUED FLAG
           NI  MYCCB+2,x'FF'-x'80'       MAKE SURE WAIT BIT IS OFF
          ENDIF                         ENDIF
          WAIT (1)                      WAIT FOR ATTN INTERRUPT
          IF   MYCCB+4(2),(NE,CLC),=X'0C00' IF NOT CE+DE, ERROR
           LA  R5,14                     SET RC VALUE
           XR  R15,R15                   CLEAR REGISTER
           ICM R15,B'0011',MYCCB+4       SET RESULT VALUE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          CF   CCBQUEUE                 CLEAR QUEUED FLAG
*
         ENDIF                         ENDIF
*
* READ DATA FROM 3270 DEVICE
*
         SF    TECBWAIT                MAKE SURE LOOP IS ENTERED
         WHILE TECBWAIT,(ON,TF),$NOOP  WHILE TECB WAIT FLAG IS ON
          IF   OPT,(EQ,CLC),=C'READB'
           MVC MYCCW+0(1),LCLREADB      PERFORM READ-BUFFER
          ELSE
           MVC MYCCW+0(1),LCLREADM      PERFORM READ-MODIFIED
          ENDIF
          L    R0,BUFLEN                PUT MAX LENGTH OF BUFFER
          STH  R0,MYCCW+6                IN CHANNEL COMMAND WORD
          LA   R1,MYCCB                 SET CCB ADDRESS
          EXCP (1)                      START THE I/O OPERATION
          WAIT (1)                      WAIT FOR I/O COMPLETION
          IF   MYCCB+4(2),(NE,CLC),=X'0C00' IF NOT CE+DE, ERROR
           LA  R5,16                     SET RC VALUE
           XR  R15,R15                   CLEAR REGISTER
           ICM R15,B'0011',MYCCB+4       SET RESULT VALUE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          IF   OPT,(NE,CLC),=C'READQ'   IF NOT READ-QUERY
            WEXIT                        EXIT WHILE LOOP
          ELSE                          ELSE
           IF  SCRBUF,EQ,X'88'           IF QUERY RECEIVED
            CF TECBWAIT                   END WHILE LOOP
           ELSE                          ELSE
            XR R0,R0                      CLEAR PARM REG
            LA R1,30                      SET .1-SECOND TIMER
            LA R2,MYTECB                  SET ADDRESS OF TECB
            SETIME (1),(2),PREC           SETUP 1/300 OF SECOND TIMER
            WAIT (1)                      WAIT FOR TIMER INTERRUPT
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         WEND                          WEND
*
* STORE RAW 3270 BUFFER INPUT
         LA    R2,SCRBUF               THEN, GET ADDRESS FOR SHVVALA
         LH    R3,MYCCW+6              CALCULATE LENGTH
         SH    R3,MYCCB+0               OF THE 3270 DATA READ
         STM   R2,R3,SHVVALA           PUT IN SHV CONTROL BLOCK
         ST    R3,BUFEND               SAVE BUFFER LENGTH
         L     R3,STEMLENG             COMPUTE LENGTH OF STEM NAME
         LA    R3,L'RAWINP(,R3)        PLUS LENGTH OF QUALIFIER
         MVC   0(L'RAWINP,R4),RAWINP   ADD QUALIFIER TO STEM NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
         LA    R2,VARDATA              POINT BACK TO VAR DATA AREA
         ST    R2,SHVVALA              PUT IN SHV CONTROL BLOCK
*
         MVC   VARDATA(1),SCRBUF       FIRST, SAVE AID BYTE
         LA    R2,1                    SET LENGTH OF DATA VALUE
         ST    R2,SHVVALL               AND PUT IN CONTROL BLOCK
         L     R3,STEMLENG             COMPUTE LENGTH OF STEM NAME
         LA    R3,L'KYBAID(,R3)        PLUS LENGTH OF QUALIFIER
         MVC   0(L'KYBAID,R4),KYBAID   ADD QUALIFIER TO STEM NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         IF    SCRBUF,EQ,X'88'         IF A STRUCTURED FIELD AID
          LA   R2,0                     NO CURSOR POSITION
         ELSE                          ELSE
          LA   R2,SCRBUF+1              POINT TO CURSOR POSITION
          BAS  R5,BUFADR                TRANSLATE 12-BIT TO 14-BIT ADR
         ENDIF                         ENDIF
         LA    R3,VARDATA              POINT TO VARIABLE DATA AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         ST    R2,SHVVALL              LENGTH OF CONVERTED RESULT
         L     R3,STEMLENG             COMPUTE LENGTH OF STEM NAME
         LA    R3,L'SCRCSR(,R3)        PLUS LENGTH OF QUALIFIER
         MVC   0(L'SCRCSR,R4),SCRCSR   ADD QUALIFIER TO STEM NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* PARSE 3270 BUFFER INPUT FOR FIELD INFORMATION
*
         IF    OPT,(EQ,CLC),=C'READM'  FOR 'READM' ONLY
*
          XC   TRTBL,TRTBL              CLEAR FOR TRANSLATE AND TEST
          MVI  TRTBL+X'11',1              FIND SET BUFFER ADDR ORDERS
          SF   SET_KLZ                  REG2ZON3 KEEP LEADING ZEROES
          MVC  0(2,R4),=C'FN'           ADD QUALIFIER TO STEM NAME
          LA   R7,SCRBUF                BEGINNING OF BUFFER
          A    R7,BUFEND                 PLUS BUFFER LENGTH
          ST   R7,BUFEND                  EQUALS ENDING POSITION
          LA   R7,SCRBUF+3              POINT TO FIRST FIELD POSITION
*
          WHILE R7,(LT,C),BUFEND,AND,   LOOP THROUGH BUFFER            X
               0(L'SCRSETBA,R7),(EQ,CLC),SCRSETBA
*
* ...find start of next field (at eob or at set buffer address order)
           LA  R6,3(,R7)                 POINT TO SEARCH AREA AND
           LA  R5,255                      SET LENGTH-1 TO SEARCH
           IF  R5,(Z,EX),FNDSBA          IF SBA NOT FOUND
            IF 256(L'SCRSETBA,R6),(EQ,CLC),SCRSETBA  MAX DATA?
             LA R1,256(,R6)                POINT FOR MAX DATA
            ELSE                          ELSE
             L R1,BUFEND                   POINT TO END OF DATA
            ENDIF                         ENDIF
           ELSE                          ELSE
            IF R1,(NL,C),BUFEND           IF PAST END OF DATA
             L R1,BUFEND                   POINT TO END OF DATA
            ENDIF                         ENDIF
           ENDIF                         ENDIF
*
* ...calculate length of the data in the current field...
           LR  R5,R6                     COPY ADDRESS FOR SHVVALA
           SR  R1,R6                     CALC LENGTH OF DATA
           LR  R6,R1                     COPY LENGTH  FOR SHVVALL
           IF  R6,(HT,CH),=H'256'        IF GREATER THAN 256
            LH R5,=H'-9'                  INDICATE INVALID LENGTH
            B  RETERR                     GO RETURN ERROR
FNDSBA      TRT 0(0,R6),TRTBL            (SEE "EX" INSTRUCTION ABOVE)
           ENDIF                         ENDIF
           STM R5,R6,SHVVALA             PUT IN SHV CONTROL BLOCK
*
* ...store the current field's data in a REXX variable...
           LA  R2,1(,R7)                 POINT TO BUFFER POSITION
           BAS R5,BUFADR                 TRANSLATE 12-BIT TO 14-BIT ADR
           BCTR R2,0                     DECREMENT BY ONE
           LA  R3,2(,R4)                 POINT TO VARIABLE NAME AREA
           BAS R15,REG2ZON3              CONVERT THAT TO ZONED-DECIMAL
           MVC 6(L'FLDINP,R4),FLDINP     ADD QUALIFIER TO FIELD NAME
           L   R3,STEMLENG               COMPUTE LENGTH OF STEM NAME
           LA  R3,6(,R3)                  PLUS LENGTH OF FIELD NAME
           LA  R3,L'FLDINP(,R3)           PLUS LENGTH OF QUALIFIER
           BAS R5,SETVAR                 CREATE THE VARIABLE
*
           LA  R7,3(,R7)                 INCR. FOR SBA ORDER + BUF ADDR
           AR  R7,R6                     POINT TO NEXT SBA ORDER
          WEND                          LOOP
*
          SF   SET_KLZ                  REG2ZON3 NO LEADING ZEROES
         ENDIF                         END OF 'READM' PROCESSING
*
         IF    OPT,(EQ,CLC),=C'READQ'  FOR 'READQ' ONLY
          MVI  VARDATA,C'1'             INDICATE QUERY HAS BEEN DONE
          LA   R2,1                     SET LENGTH OF DATA VALUE
          ST   R2,SHVVALL                AND PUT IN CONTROL BLOCK
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'DEVQRY(,R3)         PLUS LENGTH OF QUALIFIER
          MVC  0(L'DEVQRY,R4),DEVQRY    ADD QUALIFIER TO STEM NAME
          BAS  R5,SETVAR                CREATE THE VARIABLE
*
          LA   R6,SCRBUF+1              POINT AT 2ND BYTE OF INPUT
          LA   R7,SCRBUF-1              CALCULATE THE ENDING ADDRESS
          A    R7,BUFEND                  OF THE QUERY INPUT
*
          WHILE R6,(LT,CR),R7           LOOP UNTIL POINTER => EOD
*
          SELECT                        BASED ON QUERY REPLY TYPE
*
          WHEN 3(1,R6),(EQ,CLC),QRY_81  USABLE AREA QUERY REPLY
           SF  SET_81                   SET X'81' QUERY REPLY RECEIVED

           IF  4(R6),(NO,TF),X'10'
            MVI VARDATA,C'D'            DISPLAY
           ELSE
            MVI VARDATA,C'P'            PRINTER
           ENDIF
           LA  R2,1                     SET LENGTH OF DATA VALUE
           ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
           L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVTYP(,R3)         PLUS LENGTH OF QUALIFIER
           MVC 0(L'DEVTYP,R4),DEVTYP    ADD QUALIFIER TO STEM NAME
           BAS R5,SETVAR                CREATE THE VARIABLE

           LH  R2,6(,R6)                COLS (DEF)
           LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
           BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
           ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
           L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVWDT(,R3)         PLUS LENGTH OF QUALIFIER
           MVC 0(L'DEVWDT,R4),DEVWDT    ADD QUALIFIER TO STEM NAME
           BAS R5,SETVAR                CREATE THE VARIABLE

           LH  R2,8(,R6)                ROWS (DEF)
           LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
           BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
           ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
           L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVHGT(,R3)         PLUS LENGTH OF QUALIFIER
           MVC 0(L'DEVHGT,R4),DEVHGT    ADD QUALIFIER TO STEM NAME
           BAS R5,SETVAR                CREATE THE VARIABLE

           LH  R2,21(,R6)               BUFSZ (DEF)
           LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
           BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
           ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
           L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
           LA  R3,L'DEVBSZ(,R3)         PLUS LENGTH OF QUALIFIER
           MVC 0(L'DEVBSZ,R4),DEVBSZ    ADD QUALIFIER TO STEM NAME
           BAS R5,SETVAR                CREATE THE VARIABLE

           IF  SET_A6,(NOT,TF),$NOOP     IF NO X'A6' QUERY REPLY YET
            MVI VARDATA,C'0'             ROWS (ALT)
            LA  R2,1                     SET LENGTH OF DATA VALUE
            ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVHGA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVHGA,R4),DEVHGA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            MVI VARDATA,C'0'             COLS (ALT)
            LA  R2,1                     SET LENGTH OF DATA VALUE
            ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVWDA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVWDA,R4),DEVWDA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            MVI VARDATA,C'0'             BUFSZ (ALT)
            LA  R2,1                     SET LENGTH OF DATA VALUE
            ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVBSA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVBSA,R4),DEVBSA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
           ENDIF                        ENDIF
*
          WHEN 3(1,R6),(EQ,CLC),QRY_A6  IMPLICIT PARTITION QUERY REPLY
           SF  SET_A6
           LA  R8,6(,R6)                POINT TO SELF-DEFINING PARM

           IF  1(R8),EQ,X'01'           IMPLICIT PARTITION SIZES (DISP)
            MVI VARDATA,C'D'             DISPLAY
            LA  R2,1                     SET LENGTH OF DATA VALUE
            ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVTYP(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVTYP,R4),DEVTYP    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            LH  R2,3(,R8)                COLS (DEF)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVWDT(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVWDT,R4),DEVWDT    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            LH  R2,5(,R8)                ROWS (DEF)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVHGT(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVHGT,R4),DEVHGT    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            LH  R2,7(,R8)                COLS (ALT)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVWDA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVWDA,R4),DEVWDA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            LH  R2,9(,R8)                ROWS (ALT)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVHGA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVHGA,R4),DEVHGA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            IF SET_81,(NOT,TF),$NOOP     IF NO X'81' QUERY REPLY YET
             MVI VARDATA,C'0'             BUFSZ (DEF)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVBSZ(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVBSZ,R4),DEVBSZ    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
             MVI VARDATA,C'0'             BUFSZ (ALT)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVBSA(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVBSA,R4),DEVBSA    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
            ENDIF                        ENDIF
           ENDIF                        ENDIF

           IF  1(R8),EQ,X'03'           IMPLICIT PARTITION SIZES (PRTR)
            MVI VARDATA,C'P'             PRINTER
            LA  R2,1                     SET LENGTH OF DATA VALUE
            ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVTYP(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVTYP,R4),DEVTYP    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            L   R2,3(,R8)                BUFSZ (DEF)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVBSZ(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVBSZ,R4),DEVBSZ    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            L   R2,7(,R8)                BUFSZ (ALT)
            LA  R3,VARDATA               POINT TO VARIABLE DATA AREA
            BAS R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
            ST  R2,SHVVALL               LENGTH OF CONVERTED RESULT
            L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
            LA  R3,L'DEVBSA(,R3)         PLUS LENGTH OF QUALIFIER
            MVC 0(L'DEVBSA,R4),DEVBSA    ADD QUALIFIER TO STEM NAME
            BAS R5,SETVAR                CREATE THE VARIABLE
            IF SET_81,(NOT,TF),$NOOP     IF NO X'81' QUERY REPLY YET
             MVI VARDATA,C'0'             ROWS (DEF)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVHGT(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVHGT,R4),DEVHGT    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
             MVI VARDATA,C'0'             COLS (DEF)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVWDT(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVWDT,R4),DEVWDT    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
             MVI VARDATA,C'0'             ROWS (ALT)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVHGA(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVHGA,R4),DEVHGA    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
             MVI VARDATA,C'0'             COLS (ALT)
             LA  R2,1                     SET LENGTH OF DATA VALUE
             ST  R2,SHVVALL                AND PUT IN CONTROL BLOCK
             L   R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
             LA  R3,L'DEVWDA(,R3)         PLUS LENGTH OF QUALIFIER
             MVC 0(L'DEVWDA,R4),DEVWDA    ADD QUALIFIER TO STEM NAME
             BAS R5,SETVAR                CREATE THE VARIABLE
            ENDIF                        ENDIF
           ENDIF                        ENDIF
*
          WHEN 3(1,R6),(EQ,CLC),QRY_86  COLOR QUERY REPLY
           IF  5(R6),HT,4,AND,                                         X
               12(1,R6),(EQ,CLC),13(R6)
            SF  SET_EXC                 INDICATE EXT. COLOR SUPPORT
           ELSE
            CF  SET_EXC                       NO EXT. COLOR SUPPORT
           ENDIF
*
          WHEN 3(1,R6),(EQ,CLC),QRY_87  HILITE QUERY REPLY
           IF  4(R6),HT,1,AND,                                         X
               9(1,R6),(EQ,CLC),10(R6)
            SF  SET_EXH                 INDICATE EXT. HILITE SUPPORT
           ELSE
            CF  SET_EXH                       NO EXT. HILITE SUPPORT
           ENDIF
*
          ENDSL
*
          AH   R6,0(R6)                 INCREMENT TO NEXT QUERY REPLY
          WEND                          LOOP
*
          IF   SET_EXC,(ON,TF),$NOOP
           MVI VARDATA,C'1'             EXT. COLOR SUPPORT
          ELSE
           MVI VARDATA,C'0'             NO EXT. COLOR SUPPORT
          ENDIF
          LA   R2,1                     SET LENGTH OF DATA VALUE
          ST   R2,SHVVALL                AND PUT IN CONTROL BLOCK
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'DEVEXC(,R3)         PLUS LENGTH OF QUALIFIER
          MVC  0(L'DEVEXC,R4),DEVEXC    ADD QUALIFIER TO STEM NAME
          BAS  R5,SETVAR                CREATE THE VARIABLE
*
          IF   SET_EXH,(ON,TF),$NOOP
           MVI VARDATA,C'1'             EXT. HILITE SUPPORT
          ELSE
           MVI VARDATA,C'0'             NO EXT. HILITE SUPPORT
          ENDIF
          LA   R2,1                     SET LENGTH OF DATA VALUE
          ST   R2,SHVVALL                AND PUT IN CONTROL BLOCK
          L    R3,STEMLENG              COMPUTE LENGTH OF STEM NAME
          LA   R3,L'DEVEXH(,R3)         PLUS LENGTH OF QUALIFIER
          MVC  0(L'DEVEXH,R4),DEVEXH    ADD QUALIFIER TO STEM NAME
          BAS  R5,SETVAR                CREATE THE VARIABLE
*
         ENDIF                         END OF 'READQ' PROCESSING
*
         ENDIF                         END OF ALL READ-TYPE PROCESSING
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   SAM31                         MAKE SURE IT IS 31-BIT MODE
         L     R8,SAVEEVAL             GET EVALUATION BLOCK ADDRESS
         USING EVALBLOCK,R8            ESTABLISH ADDRESSABILITY
         LA    R1,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         LA    R2,4                    CONVERTED LENGTH IS 4 BYTES
         ST    R2,EVALBLOCK_EVLEN            FOR HEX "RESULT" VALUE
         STH   R15,EWORK8              STORE 2 BYTES INTO WORK AREA
         UNPK  0(5,R1),EWORK8(3)       UNPACK CHAR INTO HEX && TRANSL.
         TR    0(4,R1),TRTBL             X'FAFBFCFDFEFF' TO C'ABCDEF'
         DROP  R8                      EVAL BLOCK NO LONGER NEEDED
*
         CF    SET_KLZ                 REG2ZON3 NO LEADING ZEROES
         LR    R2,R5                   COPY R5 TO R2
         LA    R3,VARDATA              POINT TO VARIABLE DATA AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         LA    R1,VARDATA              POINT BACK TO VAR DATA AREA
         STM   R1,R2,SHVVALA           PUT IN SHV CONTROL BLOCK
*
         LA    R3,2                    SET LENGTH OF VARIABLE NAME
         MVC   VARNAME+0(3),=C'RC '    SET VARIABLE NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
* ------------------------------------------------------------------- *
         XR    R15,R15                 CLEAR REG15 FOR RETURN CODE
RETR15   TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZON3 DS    0H    CONVERT REG2 TO LEFT-SIGN, ZONED-DECIMAL AT REG3
*
         CVD   R2,PWORK8               CONVERT REG2 TO PACKED-DECIMAL
         IF    SET_KLZ,(ON,TF),$NOOP   KEEP LEADING ZEROES?
          MVC  EWORK8(7),=X'40212020202060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
         ELSE
          MVC  EWORK8(7),=X'40202020212060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+5             POINT TO SIGNIFICANCE BYTE
         ENDIF
         LA    R2,EWORK8+6             POINT TO SIGN BYTE (& IGNORE IT)
         IF    EWORK8(7),(M,EDMK),PWORK8+5   IF NEGATIVE NUMBER
          BCTR R1,0                    DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'                FLOAT A LEFT MINUS SIGN
         ENDIF
         IF    SET_KLZ,(ON,TF),$NOOP   KEEP LEADING ZEROES?
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
          LA   R2,4
         ELSE
          SR   R2,R1                   CALC NO. OF CHARS TO MOVE
         ENDIF
         BCTR  R2,0                    DECR. LEN FOR ZERO REF.
         EX    R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
*
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
ZON2REG3 DS    0H    CONVERT ZONED-DECIMAL AT REG2 (L'REG3) INTO REG3
*
         BCTR  R3,0                    DECR. FOR ZERO OFFSET
         EX    R3,PKINPT               PACK 0(?,R2) FOR LENGTH OF R3
         CVB   R3,PWORK8               CONVERT PACKED-DECIMAL INTO REG3
*
         BR    R15
PKINPT   PACK  PWORK8,0(0,R2)          (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
SAYIT    DS    0H    USE REXX "SAY" TO DISPLAY MESSAGE
         ST    R3,SAYPARM3             PUT BUFFER LENGTH IN SAY PARM 3
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SAYPARMB             GET ADDRESS OF SAY PARM BLOCK
         L     R15,ARXSAY              POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXSAY
         IF    R15,(NZ,LTR),R15        IF NON-ZERO RETURN CODE
          LA   R5,8                      SET RESULT CODE
          B    RETERR                    GO RETURN ERROR
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H    GET REXX VARIABLE VALUE
         XR    R5,R5                   ZERO RETURN CODE REGISTER
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          LA   R5,8                     SET RESULT CODE
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           IC  R15,SHVRET                SAVE ARXEXCOM CC
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R2
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H    SET REXX VARIABLE VALUE
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   VARNAME(3),(NE,CLC),=C'RC '  IF NOT 'RC', REPORT ERRORS
           IF  R15,(NZ,LTR),R15          IF NON-ZERO RETURN CODE
            LA R5,8                       SET RESULT CODE
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           IF  SHVRET,NE,SHVNEWV         IF NOT NEW VARIABLE
            IC R15,SHVRET                 SAVE ARXEXCOM CC
            LA R5,8                       SET RESULT CODE
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
BUFADR   DS    0H    CONVERT 3270 12-BIT ADDR TO 14-BIT ADDR
         NI    0(R2),B'00111111'       CLEAR HIGH-ORDER BITS
         NI    1(R2),B'00111111'        ON BOTH BYTES
         LH    R2,0(,R2)               GET BOTH BYTES
*
         SRDL  R2,8                    RIGHT JUSTIFY MOST SIG. 6 BITS
         SLL   R3,2                    LEFT JUSTIFY LEAST SIG. 6 BITS
         SLDL  R2,6                    JOIN MSB/LSB IN REGISTER 2
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
SALEN    DC    A(DYNALENG)
BUFLEN   DC    A(L'SCRBUF)
CARTNEP  DC    F'0'               CHANNEL APPENDAGE RTN ENTRY POINT
CARTNBEG DC    F'0'               PFIX LOAD POINT ADDRESS
CARTNLEN DC    F'0'               PFIX MODULE LENGTH MINUS ONE
CARTNFLG DC    F'-1'              PFIX PARM BLOCK END FLAG
         LTORG
*
QUERY    DC    AL2(QUERYL)        STRUCTURED FIELD LENGTH
         DC    X'01'              READ PARTITION
         DC    X'FF'              PARTITION ID (X'FF' = "QUERY")
         DC    X'03'              QUERY LIST
         DC    B'00000000'        QUERY TYPE (QCODE LIST)
QRY_81   DC    X'81'              QUERY ID (USABLE AREA)
QRY_A6   DC    X'A6'              QUERY ID (IMPLICIT PARTITION)
QRY_86   DC    X'86'              QUERY ID (COLOR)
QRY_87   DC    X'87'              QUERY ID (HILITE)
QUERYL   EQU   *-QUERY            CALCULATE QUERY LENGTH
         SPACE 3
* ******************************************************************* *
*        LOCAL CCW COMMAND CODES                                      *
* ******************************************************************* *
LCLWRITE DC    X'01'              LOCAL  CC=(WRITE)
LCLREADB DC    X'02'              LOCAL  CC=(READ,BUFFER)
LCLNOOP  DC    X'03'              LOCAL  CC=(NOOP)
LCLERWRT DC    X'05'              LOCAL  CC=(ERASE,WRITE)
LCLREADM DC    X'06'              LOCAL  CC=(READ,MODIFIED)
LCLERWRA DC    X'0D'              LOCAL  CC=(ERASE,WRITE,ALTERNATE)
LCLERAUP DC    X'0F'              LOCAL  CC=(ERASE,AUP)
LCLWRSTR DC    X'11'              LOCAL  CC=(WRITE,STRUCTURED)
         SPACE 3
* ******************************************************************* *
*        3270 BUFFER ORDERS                                           *
* ******************************************************************* *
AIDNOAID DC    X'60'              NO AID GENERATED (NO DATA TO READ)
SCRSETBA DC    X'11'              SET BUFFER ADDRESS + 2-BYTE ADDRESS
SCRCURSR DC    X'13'              INSERT CURSOR
SCRSTFLD DC    X'1D00'            SF + ATTRIBUTE
SCRSETAC DC    X'284200'          SA + TYPE-VALUE PAIR
SCRSETAH DC    X'284100'          SA + TYPE-VALUE PAIR
SCRSTFEX DC    X'2903C00042004100' SFE + COUNT + TYPE-VALUE PAIRS
         SPACE 3
* ******************************************************************* *
*        VARIABLE NAMES TO RETRIEVE/CREATE                            *
* ******************************************************************* *
DEVLUN   DC    C'_LUN'            LOGICAL UNIT NUMBER
FLDATR   DC    C'.ATTRIB'         FIELD ATTRIBUTES
FLDOUT   DC    C'.OUTPUT'         FIELD OUTPUT
FLDINP   DC    C'.INPUT'          FIELD INPUT
DEVBSZ   DC    C'_BUFSZ'          DEFAULT BUFFER SIZE
DEVBSA   DC    C'_BUFSA'             ALT. BUFFER SIZE
DEVHGT   DC    C'_ROWS'           DEFAULT ROWS (HEIGHT)
DEVHGA   DC    C'_ROWA'              ALT. ROWS (HEIGHT)
DEVWDT   DC    C'_COLS'           DEFAULT COLUMNS (WIDTH)
DEVWDA   DC    C'_COLA'              ALT. COLUMNS (WIDTH)
RAWOUT   DC    C'_CCC'            RAW OUTPUT DS (STARTING WITH CCC)
RAWINP   DC    C'_RAW'            RAW INPUT DATA STREAM
DEVWCC   DC    C'_WCC'            WRITE CONTROL CHARACTER
KYBAID   DC    C'_AID'            ATTENTION IDENTIFIER
SCRCSR   DC    C'_CSR'            CURSOR POSITION
DEVQRY   DC    C'_QRY'            QUERY INDICATOR
DEVTYP   DC    C'_DEV'            DEVICE TYPE
DEVEXC   DC    C'_EXC'            EXTENDED COLOR
DEVEXH   DC    C'_EXH'            EXTENDED HILITE
* =================================================================== *
         END   RX3270IO
