*ASM XOPTS(NOPROLOG NOEPILOG)
PRT      TITLE 'P R T M A N       - CICS PRINTER I/O MANAGEMENT'
         PRINT ON,GEN
PRTMAN   AMODE ANY
PRTMAN   RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      P R I N T   M A N I P U L A T I O N   S U B P R O G R A M      *
*               F O R   C I C S   P R O G R A M M E R S               *
*                                                                     *
*                                                                     *
* CLARKE Industries, Inc.                             Written by:     *
* 2100 Highway 265                                 Dave Leonard Clark *
* Springdale, AR  72764                              (501)750-8248    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This CICS Command-Level  program  provides the CICS programmer *
* with an efficient means  of  formatting  and buffering CICS printer *
* output based on the  specific  printer  the  "LINK"ing CICS task is *
* attached  to.   The  justification  for  this  program  is  that it *
* alleviates the need for  a  programmer  to  have  to manage (and be *
* familiar with) printer data streams in CICS.   Furthermore, because *
* this program also supports PCL,  the  programmer also does not have *
* to know PCL nor have to  figure  out  how to get the PCL codes into *
* the printer data stream.   Additionally, it eliminates the need for *
* a printer program to  know  the  difference between SCS and non-SCS *
* data stream contents and provides  a more efficient flow of printer *
* data by optimizing the contents and size of the print buffer.       *
*                                                                     *
*      Alternatively, this program can also be used to submit jobs to *
* the VSE/POWER RDR queue or send  output to the VSE/POWER LST or PUN *
* queues.   Though complex print formatting  and PCL is not required, *
* in this  case,  this  feature  still  can  shorten the programmer's *
* effort.   Note that the last LINK  (request code 'E') is merely for *
* closing the VSE/POWER interface (i.e., no data is processed) -- but *
* is required none-the-less.   Lastly,  if needed, the spooled output *
* can be deleted (request code  'D')  before  it is released into the *
* queue and actually made available for external processing.          *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This implementation accepts a print line of up to 132 bytes in *
* length  and  reformats  it,  with  the  requested  carriage control *
* characters,   into  a  buffer  of  up  to  4016  bytes  in  length. *
* Optionally,   PCL  codes  may  also  be  requested,  for  automatic *
* insertion into the print buffer,  in order to format the output for *
* PCL   printers   using   page-orientation,    character-formatting, *
* characters-per-inch,  margins,  lines-per-inch,  and  lines-of-text *
* specifications.   Blanks (and unprintable characters) on the end of *
* print lines are removed and the buffer is automatically sent to the *
* printer when it is full.  A "LINK"ing COBOL CICS program should use *
* the following COMMAREA for  passing parameters ("LINK"ing Assembler *
* CICS programs should use the same DSECT this program uses):         *
*                                                                     *
*      01  PRT-PGM-COMM.                                              *
*        02  PRT-PGM-NAME              PIC  X(08)   VALUE 'PRTMAN  '. *
*        02  PRT-PGM-COML              PIC S9(04)   COMP VALUE +4250. *
*        02  PRT-LNK-REQU              PIC  X(01)   VALUE SPACES.     *
*            88  PRINT-ASA-CC                       VALUES ' ' '-' '+'*
*                                                         '0' THRU '9'*
*                                                        'A' THRU 'C'.*
*            88  PRINT-DELSPOOL                     VALUE 'D'.        *
*            88  PRINT-ENDOFMSG                     VALUE 'E'.        *
*            88  PRINT-FORMFEED                     VALUE 'F'.        *
*            88  PRINT-NEWLINE                      VALUE 'N'.        *
*            88  PRINT-OMIT-CC                      VALUE 'O'.        *
*            88  PRINT-PCL-CODE                     VALUE 'P'.        *
*            88  PRINT-RETURN                       VALUE 'R'.        *
*        02  PRT-LNK-RETN              PIC  X(01)   VALUE SPACES.     *
*            88  PRINT-COMPLETED                    VALUE ' '.        *
*            88  PRINT-REQU-ERROR                   VALUE 'E'.        *
*            88  PRINT-PCL-INVALID                  VALUE 'I'.        *
*            88  PRINT-LENG-ERROR                   VALUE 'L'.        *
*            88  PRINT-PRTR-ERROR                   VALUE 'P'.        *
*            88  PRINT-QUEUE-ERROR                  VALUE 'Q'.        *
*            88  PRINT-SPOOL-ERROR                  VALUE 'S'.        *
*            88  PRINT-UNEX-ERROR                   VALUE 'X'.        *
*        02  PRT-LNK-RESP              PIC S9(08)   COMP VALUE ZEROES.*
*        02  PRT-LNK-RSP2              PIC S9(08)   COMP VALUE ZEROES.*
*        02  PRT-LNK-LCNT              PIC S9(04)   COMP VALUE +0001. *
*        02  PRT-LNK-TERM              PIC  X(02)   VALUE LOW-VALUES. *
*     *                                                               *
*        02  PRT-PWR-JQUE              PIC  X(03)   VALUE LOW-VALUES. *
*          88  PRT-PWR-NONE                         VALUE LOW-VALUES. *
*          88  PRT-PWR-LSTQ                         VALUE 'LST'.      *
*          88  PRT-PWR-PUNQ                         VALUE 'PUN'.      * 00005900
*          88  PRT-PWR-RDRQ                         VALUE 'RDR'.      *
*        02  PRT-PWR-JCLS              PIC  X(01)   VALUE 'A'.        *
*        02  PRT-PWR-FNO               PIC  X(04)   VALUE SPACES.     *
*        02  PRT-PWR-JOBN              PIC  X(08)   VALUE SPACES.     *
*        02  PRT-PWR-NODE              PIC  X(08)   VALUE '*'.        *
*        02  PRT-PWR-USER              PIC  X(08)   VALUE '*'.        *
*        02  PRT-PWR-UINF              PIC  X(16)   VALUE SPACES.     *
*        02  PRT-PWR-TOKEN             PIC  X(08)   VALUE LOW-VALUES. *
*     *                                                               *
*        02  PRT-PCL-CONTROL           PIC  X(01)   VALUE 'E'.        *
*            88  PRT-PCL-DISPLAY-OFF                VALUE 'Z'.        *
*            88  PRT-PCL-DISPLAY-ON                 VALUE 'Y'.        *
*            88  PRT-PCL-RESET                      VALUE 'E'.        *
*        02  PRT-PCL-ORIENTATION       PIC  X(01)   VALUE '1'.        *
*            88  PRT-PCL-PORTRAIT                   VALUE '0'.        *
*            88  PRT-PCL-LANDSCAPE                  VALUE '1'.        *
*            88  PRT-PCL-REV-PORTRAIT               VALUE '2'.        *
*            88  PRT-PCL-REV-LANDSCAPE              VALUE '3'.        *
*        02  PRT-PCL-BOLD              PIC  X(01)   VALUE LOW-VALUES. *
*            88  PRT-PCL-BOLD-OFF                   VALUE '0'.        *
*            88  PRT-PCL-BOLD-ON1                   VALUE '1'.        *
*            88  PRT-PCL-BOLD-ON2                   VALUE '2'.        *
*            88  PRT-PCL-BOLD-ON3                   VALUE '3'.        *
*            88  PRT-PCL-BOLD-ON4                   VALUE '4'.        *
*            88  PRT-PCL-BOLD-ON5                   VALUE '5'.        *
*            88  PRT-PCL-BOLD-ON6                   VALUE '6'.        *
*            88  PRT-PCL-BOLD-ON7                   VALUE '7'.        *
*        02  PRT-PCL-ITALIC            PIC  X(01)   VALUE LOW-VALUES. *
*            88  PRT-PCL-ITALIC-OFF                 VALUE '0'.        *
*            88  PRT-PCL-ITALIC-ON                  VALUE '1'.        *
*        02  PRT-PCL-UNDERLINE         PIC  X(01)   VALUE LOW-VALUES. *
*            88  PRT-PCL-UNDERLINE-OFF              VALUE '@'.        *
*            88  PRT-PCL-UNDERLINE-ON-FIXED         VALUE '0'.        *
*            88  PRT-PCL-UNDERLINE-ON-FLOAT         VALUE '3'.        *
*        02  PRT-PCL-LINES-PER-INCH    PIC  9(01)   VALUE 8.          *
*            88  PRT-PCL-1LINES-PER-INCH            VALUE 1.          *
*            88  PRT-PCL-2LINES-PER-INCH            VALUE 2.          *
*            88  PRT-PCL-3LINES-PER-INCH            VALUE 3.          *
*            88  PRT-PCL-4LINES-PER-INCH            VALUE 4.          *
*            88  PRT-PCL-5LINES-PER-INCH            VALUE 5.          *
*            88  PRT-PCL-6LINES-PER-INCH            VALUE 6.          *
*            88  PRT-PCL-7LINES-PER-INCH            VALUE 7.          *
*            88  PRT-PCL-8LINES-PER-INCH            VALUE 8.          *
*            88  PRT-PCL-9LINES-PER-INCH            VALUE 9.          *
*        02  PRT-PCL-LINES-TOP-MARGIN  PIC S9(04)   COMP VALUE ZEROES.*
*        02  PRT-PCL-LINES-OF-TEXT     PIC S9(04)   COMP VALUE ZEROES.*
*        02  PRT-PCL-CHARS-PER-INCH    PIC S9(04)   COMP VALUE +13.   *
*        02  PRT-PCL-CHARS-LEFT-MARGIN PIC S9(04)   COMP VALUE ZEROES.*
*        02  PRT-PCL-CHARS-RGHT-MARGIN PIC S9(04)   COMP VALUE ZEROES.*
*     *                                                               *
*        02  PRT-BFR-PARM.                                            *
*          05  PRT-BFR-MAXL            PIC S9(04)   COMP VALUE +1920. *
*          05  PRT-BFR-LENG            PIC S9(04)   COMP VALUE ZEROES.*
*          05  PRT-BFR-DATA            PIC  X(4016).                  *
*     *                                                               *
*        02  PRT-STR-PARM.                                            *
*          05  PRT-STR-LENG            PIC S9(04)   COMP VALUE +132.  *
*          05  PRT-STR-DATA.                                          *
*            08  PRT-STR-BYTE          PIC  X(01)   OCCURS 132 TIMES  *
*                                      DEPENDING ON PRT-STR-LENG.     *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOKATION                              *
*                                                                     *
*      The subprogram is invoked in one of two ways (the first is for *
* normal print requests and the  second  forces the last print buffer *
* out to the printer):                                                *
*                                                                     *
*          MOVE your-prtrequ TO PRT-LNK-REQU                          *
*          MOVE your-prtleng TO PRT-STR-LENG                          *
*          MOVE your-prtline TO PRT-STR-DATA                          *
*          EXEC CICS LINK                                             *
*                    PROGRAM(PRT-PGM-NAME)                            *
*                    COMMAREA(PRT-PGM-COMM)                           *
*                    LENGTH(PRT-PGM-COML)                             *
*                    NOHANDLE                                         *
*                    END-EXEC                                         *
*          IF  EIBRESP NOT = DFHRESP(NORMAL)                          *
*     *        your-CICS-error-handling-code                          *
*          END-IF                                                     *
*          IF  NOT PRINT-COMPLETED                                    *
*     *        your-error-handling-code                               *
*          END-IF                                                     *
*                                                                     *
*                             --- and ---                             *
*                                                                     *
*          MOVE     'E'      TO PRT-LNK-REQU                          *
*          EXEC CICS LINK                                             *
*                    PROGRAM(PRT-PGM-NAME)                            *
*                    COMMAREA(PRT-PGM-COMM)                           *
*                    LENGTH(PRT-PGM-COML)                             *
*                    NOHANDLE                                         *
*                    END-EXEC                                         *
*          IF  EIBRESP NOT = DFHRESP(NORMAL)                          *
*     *        your-CICS-error-handling-code                          *
*          END-IF                                                     *
*          IF  NOT PRINT-COMPLETED                                    *
*     *        your-error-handling-code                               *
*          END-IF                                                     *
*                                                                     *
* Where: your-prtrequ is one of the codes shown under IMPLEMENTATION  *
*                     and selects the desired carriage control,       *
*        your-prtleng is the length of the passed print line, and     *
*        your-prtline is the actual print line to pass.               *
*                                                                     *
*      Except  for  VSE/POWER  requests,  characters  less  than  '¢' *
* (X'4A') or greater than '9' (X'F9') are removed from the end of the *
* passed print line  before  it  is  buffered.   This allows a single *
* buffer to contain as much significant data as possible before it is *
* sent to the printer.                                                *
*                                                                     *
*      Printer control  code  buffering  (as  selected  by the passed *
* request code) occurs in front of any passed print line data.   This *
* corresponds to the use of the COBOL "WRITE ...  AFTER ADVANCING..." *
* statement.                                                          *
*                                                                     *
*      Optionally,  the "NEWLINE"  request  code  can  include a line *
* count request ("PRT-LNK-LCNT")  which  is  greater than one.   This *
* causes multiple  printer  control  code  buffering  to occur before *
* buffering any passed print line data.                               *
*                                                                     *
*      Lastly,  PCL codes must  be  requested  separately -- in other *
* words,  no print data may be passed along with any requests for PCL *
* codes.   Note that this  program  will  reset  these  PCL fields to *
* binary zeroes, once they have been requested for insertion into the *
* print buffer,  in  order  to  prevent  these  PCL  codes from being *
* re-sent for each subsequent PCL request.                            *
*                                                                     *
* Notes for VSE/POWER queue requests:                                 *
*                                                                     *
*  1. Only the DELSPOOL, ENDOFMSG, and ASA carriage control requests  *
*     are valid;                                                      *
*  2. Only fixed data line lengths ('PRT-STR-LENG') are supported:    *
*          RDR = 80, PUN = 80, and LST = 132; and                     *
*  3. After an ENDOFMSG request, the queue selection ('PRT-PWR-JQUE') *
*          is cleared.                                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
COMMAREA DSECT                         CALLER INTERFACE
         USING *,R9,R10
* ------------------------------------------------------------------- *
CPGMNAME DS    CL8
CPGMCOML DS    H                       +4250
CLNKREQU DS    CL1
CLNKRETN DS    CL1
CLNKRESP DS    F
CLNKRSP2 DS    F
CLNKLCNT DS    H
CLNKTERM DS    2XL1
*
CPWRJQUE DS    CL3
CPWRJCLS DS    CL1
CPWRFNO  DS    CL4
CPWRJOBN DS    CL8
CPWRNODE DS    CL8
CPWRUSER DS    CL8
CPWRUINF DS    CL16
CPWRTOKN DS    XL8
*
CPCLCTL  DS    CL1
CPCLPOR  DS    CL1
CPCLBOL  DS    CL1
CPCLITA  DS    CL1
CPCLULI  DS    CL1
CPCLLPI  DS    CL1
CPCLTOP  DS    H
CPCLTXL  DS    H
CPCLCPI  DS    H
CPCLLFT  DS    H
CPCLRGT  DS    H
*
CBFRMAXL DS    H
CBFRLENG DS    H
CBFRDATA DS    4016CL1
*
CSTRLENG DS    H
CSTRDATA DS    132CL1
*
COMMALEN EQU   *-COMMAREA
* =================================================================== *
         EJECT
* =================================================================== *
DFHEISTG DSECT                         EXECUTE INTERFACE + USER STORAGE
* ------------------------------------------------------------------- *
         DFHEISTG
*
PDEC     DS    0PL8
         DS    XL1
PDVD     DS    0PL5
PQUO     DS    PL3
PREM     DS    PL2
PDIV     DS    PL2
EDIT     DS    XL8
*
PWROPTA  DS    A                       ADDRESS OF POWER OPTIONS AREA
PWROPTS  EQU   *                       START OF POWER OPTIONS AREA
PWRLENG  DS    F                       DUAL-PURPOSE FULLWORD LENGTH
PWROPTD  DS    CL46                    CHANGE LENGTH AS NEEDED
*
REPLY    DS    XL23                    MAXIMUM REPLY AREA
         ORG   REPLY
REPLYL   DS    H                       STRUCTURED FIELD LENGTH
         DS    X'81'                   QUERY REPLY
REPLYI   DS    X'81'                   REPLY ID (USABLE AREA)
         DS    X'00'
         DS    X'00'
         DS    XL2                     WIDTH
         DS    XL2                     DEPTH
         DS    X'00'
         DS    XL4
         DS    XL4
         DS    X'00'
         DS    X'00'
REPLYB   DS    XL2                     BUFFER SIZE
         ORG
*
         DFHEIEND
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTERS
* ------------------------------------------------------------------- *
R0       EQU   0                       PARM REG
R1       EQU   1                       PARM REG
R2       EQU   2                       WORK REG
R3       EQU   3                       LENGTH OF CC ORDERS
R4       EQU   4                       ADDRESS OF CC ENTRY
R5       EQU   5                       LENGTH OF EM ORDER
R6       EQU   6                       ADDRESS OF BUFFER-END + 1
R7       EQU   7
R8       EQU   8                       INTERNAL "BAS" ADDRESS
R9       EQU   9                       COMM AREA ADDR 1
R10      EQU   10                      COMM AREA ADDR 2
R11      EQU   11                      EIB DSECT ADDR
R12      EQU   12                      PGM CSECT ADDR
R13      EQU   13                      DATA/SAVEAREA REGISTER
R14      EQU   14                      BRANCH&LINK/RETURN REG
R15      EQU   15                      RETURN CODE/ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  IF
         COPY  SELECT
         COPY  WHILE
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
PRTMAN   DFHEIENT CODEREG=(R12)
* ------------------------------------------------------------------- *
         B     START                   BRANCH AROUND EYE CATCHER
*
PGMNAME  DC    CL8'PRTMAN '            PROGRAM
         DC    CL4'3.0 '               VERSION
         DC    CL4'DLC '               AUTHOR
         DC    CL8'(C)JAN91'           DATE
*
START    DS    0H
         L     R9,DFHEICAP             OBTAIN ADDR TO CALLER'S PARM
         LA    R2,4095                 SET CONSTANT FOR INDEXING
         LA    R10,1(R2,R9)            CALCULATE SECOND BASE REG
*
         LH    R2,=AL2(COMMALEN)
         IF    R2,(HT,CH),EIBCALEN,OR,CPGMNAME,(NE,CLC),PGMNAME
          EXEC CICS ABEND ABCODE('INIT') CANCEL
         ENDIF
*
         MVI   CLNKRETN,C' '           INITIALIZE RETURN CODE
*
* ------------------------------------------------------------------- *
* VSE/POWER INTERFACE
*
         IF    CPWRJQUE,(HT,CLC),=CL3' '
*
          IF   CPWRTOKN,(EQ,CLC),=XL8'00'
*          MVC PWROPTD+00(4),=C'JNM('  FOR LST AND PUN QUEUE OUTPUT,
*          MVC PWROPTD+04(8),CPWRJOBN   PLACE JOB NAME PARAMETER
*          MVC PWROPTD+12(2),=C') '      IN OUTPUT OPTION DATA AREA
*          MVC PWROPTD+14(4),=C'FNO('  FOR LST AND PUN QUEUE OUTPUT,
*          MVC PWROPTD+18(4),CPWRFNO    PLACE FORM NO. PARAMETER
*          MVC PWROPTD+22(2),=C') '      IN OUTPUT OPTION DATA AREA
*          MVC PWROPTD+24(5),=C'USER(' FOR LST AND PUN QUEUE OUTPUT,
*          MVC PWROPTD+29(16),CPWRUINF  PLACE USER INF PARAMETER
*          MVC PWROPTD+45(1),=C') '      IN OUTPUT OPTION DATA AREA
*          LA  R2,L'PWROPTD            SET LENGTH OF OPTION DATA AREA
*          ST  R2,PWRLENG              PUT IN PARM AREA
*          LA  R2,PWROPTS              GET ADDRESS OF PARM AREA
*          ST  R2,PWROPTA              PUT IN PARM POINTER
*          LA  R2,PWROPTA              GET ADDRESS OF PARM POINTER
           SELECT
           CASE CPWRJQUE,(EQ,CLC),=C'RDR'
            EXEC CICS SPOOLOPEN JCL REPORT(CPWRJOBN) TOKEN(CPWRTOKN)   X
               NOHANDLE
           CASE CPWRJQUE,(EQ,CLC),=C'LST'
*           EXEC CICS SPOOLOPEN OUTPUT PRINT ASA OUTDESCR(R2)          X
               CLASS(CPWRJCLS) NODE(CPWRNODE) USERID(CPWRUSER)         X
               TOKEN(CPWRTOKN) NOHANDLE
            EXEC CICS SPOOLOPEN REPORT(CPWRJOBN) ASA TOKEN(CPWRTOKN)   X
               CLASS(CPWRJCLS) NODE(CPWRNODE) USERID(CPWRUSER)         X
               FORMS(CPWRFNO) USERDATA(CPWRUINF) NOHANDLE
           CASE CPWRJQUE,(EQ,CLC),=C'PUN'
            EXEC CICS SPOOLOPEN OUTPUT PUNCH ASA                       X
               CLASS(CPWRJCLS) NODE(CPWRNODE) USERID(CPWRUSER)         X
               TOKEN(CPWRTOKN) NOHANDLE
           CASE ELSE
            MVI CLNKRETN,C'Q'          SIGNAL QUEUE REQUEST ERROR
            B  RETURN
           ENDSL
           MVC CLNKRESP,EIBRESP        GET RESPONSE CODE
           MVC CLNKRSP2,EIBRESP2       GET RESPONSE CODE 2
           IF  CLNKRESP,(NE,CLC),DFHRESP(NORMAL)
            MVI CLNKRETN,C'X'          SIGNAL UNEXPECTED ERROR
            B  RETURN
           ENDIF
          ENDIF
*
          IF   CLNKREQU,EQ,C'E',OR,CLNKREQU,EQ,C'D'
           IF  CPWRJQUE,(EQ,CLC),=C'PUN'
            IF CLNKREQU,EQ,C'D'
             EXEC CICS SPOOLCLOSE DELETE TOKEN(CPWRTOKN) NOHANDLE
            ELSE
             EXEC CICS SPOOLCLOSE KEEP TOKEN(CPWRTOKN) NOHANDLE
            ENDIF
           ELSE
            IF CLNKREQU,EQ,C'D'
             EXEC CICS SPOOLCLOSE REPORT(CPWRJOBN) DELETE              X
               TOKEN(CPWRTOKN) NOHANDLE
            ELSE
             EXEC CICS SPOOLCLOSE REPORT(CPWRJOBN) DISP('K')           X
               TOKEN(CPWRTOKN) NOHANDLE
            ENDIF
           ENDIF
           XC  CPWRJQUE,CPWRJQUE       RE-INITIALIZE FOR NEXT CALL
           XC  CPWRTOKN,CPWRTOKN       RE-INITIALIZE FOR NEXT CALL
          ELSE
           IF  CPWRJQUE,(EQ,CLC),=C'PUN'
            MVC CBFRDATA+0(L'CLNKREQU),CLNKREQU
            MVC CBFRDATA+1(80),CSTRDATA
            LA R2,81                   SET LENGTH OF PUN LINE
            ST R2,PWRLENG              PUT IN PARM AREA
            EXEC CICS SPOOLWRITE LINE TOKEN(CPWRTOKN)                  X
               FROM(CBFRDATA) FLENGTH(PWRLENG) NOHANDLE
           ELSE
            IF CPWRJQUE,(EQ,CLC),=C'RDR'
             MVC CBFRDATA(80),CSTRDATA
             LA R2,80                  SET LENGTH OF JCL LINE
            ELSE
             MVC CBFRDATA+0(L'CLNKREQU),CLNKREQU
             MVC CBFRDATA+1(132),CSTRDATA
             LA R2,133                 SET LENGTH OF LST LINE
            ENDIF
            ST R2,PWRLENG              PUT IN PARM AREA
            EXEC CICS SPOOLWRITE REPORT(CPWRJOBN) TOKEN(CPWRTOKN)      X
               FROM(CBFRDATA) FLENGTH(PWRLENG) NOHANDLE
           ENDIF
          ENDIF
          MVC  CLNKRESP,EIBRESP        GET RESPONSE CODE
          MVC  CLNKRSP2,EIBRESP2       GET RESPONSE CODE 2
          IF   CLNKRESP,(NE,CLC),DFHRESP(NORMAL)
           MVI CLNKRETN,C'S'           SIGNAL SPOOL ERROR
          ENDIF
*
          B    RETURN
*
         ENDIF
*
* ------------------------------------------------------------------- *
* CICS PRINTER INTERFACE
*
         IF    CLNKREQU,EQ,C'P'        IF PCL CODE REQUESTED
          XC   CLNKLCNT,CLNKLCNT        CLEAR LINE COUNT
          XC   CSTRLENG,CSTRLENG        CLEAR STRING LENGTH
          XC   CSTRDATA(132),CSTRDATA   CLEAR STRING DATA
          BAS  R8,BLDPCL                GO BUILD PCL CODES
          IF   CLNKRETN,NE,C' ',BC,RETURN
          ENDIF
         ELSE                          ELSE
          XR   R2,R2                    CLEAR FOR CARRIAGE CONTROL CODE
          IF   CLNKREQU,(ZERO,TRT),TRTABLE
           MVI CLNKRETN,C'E'             SIGNAL REQUEST ERROR
           B   RETURN                    AND RETURN TO CALLER
          ENDIF                         ENDIF
          BCTR R2,0                     REDUCE BY 1
          SLL  R2,2                     TIMES 4
          L    R4,CCTABLE(R2)           LOAD ADDRESS OF CC ENTRY
          XR   R3,R3                    CLEAR FOR LENGTH OF CC ORDERS
          IC   R3,0(R4)                 GET LENGTH OF CC ORDERS
          LH   R1,CLNKLCNT              GET LOOP (LINE) COUNT
          IF   R1,(PLUS,LTR),R1         IF COUNT > ZERO
          AND  CLNKREQU,NE,C'E'         AND NOT 'E' REQUEST
          AND  CLNKREQU,NE,C'R'         AND NOT 'R' REQUEST
*          CONTINUE                      CONTINUE
          ELSE                          ELSE
           LA  R1,1                      DEFAULT LINE COUNT TO 1
           STH R1,CLNKLCNT               TO USE ASA STANDARD SPACING
          ENDIF                         ENDIF
          IF   CSTRLENG,(LT,CLC),=H'0',OR,CSTRLENG,(HT,CLC),=H'132'
           MVI CLNKRETN,C'L'             SIGNAL LENGTH ERROR
           B   RETURN                    AND RETURN TO CALLER
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         IF    CLNKTERM,(NH,CLC),=X'90F0'  NEED TO CHECK PRINTER TYPE?
          EXEC CICS ASSIGN TERMCODE(CLNKTERM) NOHANDLE
          MVC  CLNKRESP,EIBRESP         GET RESPONSE CODE
          MVC  CLNKRSP2,EIBRESP2        GET RESPONSE CODE 2
          IF   CLNKRESP,(NE,CLC),DFHRESP(NORMAL)
           MVI CLNKRETN,C'X'             SIGNAL UNEXPECTED ERROR
           B   RETURN                    AND RETURN TO CALLER
          ENDIF                         ENDIF
          IF   CLNKTERM,NE,X'93',AND,CLNKTERM,NE,X'94'   RMT 3284/6 PRT
          AND  CLNKTERM,NE,X'9B',AND,CLNKTERM,NE,X'9C'   LCL 3284/6 PRT
          AND  CLNKTERM,NE,X'B6'                            SCS PRINTER
           XC  CLNKTERM,CLNKTERM         RE-INITIALIZE FOR NEXT CALL
           MVI CLNKRETN,C'P'             SIGNAL PRINTER ERROR
           B   RETURN                    AND RETURN TO CALLER
          ENDIF                         ENDIF
          IF   CLNKTERM,EQ,X'B6'        IF SCS PRINTER
           MVC CBFRMAXL,=H'4016'         USE MAXIMUM BUFFER SIZE
          ELSE                          ELSE
           EXEC CICS CONVERSE FROM(QUERY) INTO(REPLY) STRFIELD ASIS    X
               NOHANDLE
           IF  REPLYI,(EQ,CLC),QUERYI    IF EXPECTED REPLY ID
           AND REPLYB,(HT,CLC),=H'0'     AND BUFFER SIZE > ZERO
            IF REPLYB,(HT,CLC),=H'4016'   IF RETURNED SIZE > 4016
             MVC CBFRMAXL,=H'4016'         USE MAXIMUM BUFFER SIZE
            ELSE                          ELSE
             MVC CBFRMAXL,REPLYB           USE RETURNED BUFFER SIZE
            ENDIF                         ENDIF
           ELSE                          ELSE
            MVC CBFRMAXL,=H'1920'         USE MINIMUM BUFFER SIZE
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         IF    CLNKREQU,NE,C'E'        IF NOT EM REQUEST
          LH   R2,CSTRLENG              GET CURRENT STRING LENGTH
          LA   R1,CSTRDATA-1(R2)        CALC STRING-END ADDRESS
          WHILE R2,(PLUS,LTR),R2        LOOP ON LENGTH
           IF  0(R1),HT,X'49',AND,0(R1),LT,X'FA'
            WEXIT                        EXIT LOOP
           ENDIF
           BCTR R2,R0                    DECREMENT LENGTH
           BCTR R1,R0                    DECREMENT ADDRESS
          WEND                          LOOP ON LENGTH
          STH  R2,CSTRLENG              PUT STRING LENGTH
          AH   R2,CBFRLENG              ADD CURRENT BUFFER LENGTH
          AR   R2,R3                    PLUS LENGTH OF CC ORDER
          AH   R2,CLNKLCNT              ADD CURRENT LINE COUNTER
          BCTR R2,R0                    LESS 1 FOR CURRENT LINE
          IF   CLNKTERM,NE,X'B6'        IF NOT AN SCS PRINTER
           LA  R2,L'CCEM(,R2)            PLUS LENGTH OF EM ORDER
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         IF    CLNKREQU,EQ,C'E',OR,R2,(NL,CH),CBFRMAXL
          IF   CLNKREQU,EQ,C'E',AND,CPCLCTL,EQ,X'00'
           LH  R2,CBFRLENG              GET CURRENT BUFFER LENGTH
           LA  R6,CBFRDATA(R2)          CALC BUFFER-END + 1 ADDRESS
           MVC 0(2,R6),PCLESC           MOVE PCL RESET TO BUFFER
           LA  R2,2(,R2)                ADD TO BUFFER LENGTH
           STH R2,CBFRLENG              PUT UPDATED BUFFER LENGTH
          ENDIF
          IF   CLNKTERM,NE,X'B6'       IF NOT AN SCS PRINTER
           LH  R2,CBFRLENG              GET CURRENT BUFFER LENGTH
           LA  R6,CBFRDATA(R2)          CALC BUFFER-END + 1 ADDRESS
           XR  R5,R5                    CLEAR FOR LENGTH OF EM ORDER
           IC  R5,CCEML                 GET LENGTH OF EM ORDER
           BCTR R5,R0                   MINUS 1 FOR EXECUTE
           EX  R5,MVC#1                 MOVE EM ORDER TO BUFFER
           LA  R2,1(R5,R2)              ADD TO BUFFER LENGTH
           STH R2,CBFRLENG              PUT UPDATED BUFFER LENGTH
          ENDIF                        ENDIF
          EXEC CICS SEND FROM(CBFRDATA) LENGTH(CBFRLENG) CTLCHAR(C'H') X
               ERASE WAIT NOHANDLE
          MVC  CLNKRESP,EIBRESP         GET RESPONSE CODE
          MVC  CLNKRSP2,EIBRESP2        GET RESPONSE CODE 2
          IF   CLNKRESP,(NE,CLC),DFHRESP(NORMAL)
           MVI CLNKRETN,C'X'            SIGNAL UNEXPECTED ERROR
           B   RETURN                   AND RETURN TO CALLER
MVC#1    MVC   0(0,R6),CCEM            (SEE "EX" INSTRUCTION ABOVE)
          ELSE                         ELSE
           XC  CBFRLENG,CBFRLENG        ZERO BUFFER LENGTH
           IF  CLNKREQU,EQ,C'E'         IF EM REQUEST
            XC CLNKLCNT,CLNKLCNT         RESET LOOP (LINE) COUNTER
            XC CSTRLENG,CSTRLENG         ZERO STRING LENGTH
            B  RETURN                    AND RETURN TO CALLER
           ENDIF                        ENDIF
          ENDIF                        ENDIF
         ENDIF
*
         IF    CLNKREQU,NE,C'P'        IF NOT PCL REQUEST
         AND   CLNKREQU,NE,C'O'        AND NOT OMIT CC REQUEST
          SELECT                        SELECT
          CASE CLNKREQU,EQ,C'0'         FOR ASA DOUBLE SPACING
           LA  R1,2                      SET 2 LOOPS
          CASE CLNKREQU,EQ,C'-'         FOR ASA TRIPLE SPACING
           LA  R1,3                      SET 3 LOOPS
          CASE ELSE                     OTHERWISE
           LH  R1,CLNKLCNT               GET LOOP (LINE) COUNT
           IF  R1,(NPLUS,LTR),R1         IF NOT > ZERO
            LA R1,1                       DEFAULT TO 1 LOOP
           ENDIF                         ENDIF
          ENDSL                         END OF SELECT
          LH   R2,CBFRLENG              GET CURRENT BUFFER LENGTH
          LA   R6,CBFRDATA(R2)          CALC BUFFER-END + 1 ADDRESS
          BCTR R3,R0                    CC LEN MINUS 1 FOR EXECUTE
          WHILE R1,(PLUS,LTR),R1        LOOP ON COUNTER
           EX  R3,MVC#2                  MOVE CC ORDER TO BUFFER
           LA  R2,1(R3,R2)               ADD TO BUFFER LENGTH
           LA  R6,1(R3,R6)               ADD TO BUFFER ADDRESS
           BCTR R1,R0                    DECREMENT LOOP COUNTER
          WEND                          LOOP
          STH  R2,CBFRLENG              PUT UPDATED BUFFER LENGTH
          XC   CLNKLCNT,CLNKLCNT        RESET LOOP (LINE) COUNTER
         ENDIF                         ENDIF
*
         LH    R1,CSTRLENG             GET STRING LENGTH
         IF    R1,(PLUS,LTR),R1        IF > ZERO
          LH   R2,CBFRLENG              GET CURRENT BUFFER LENGTH
          LA   R6,CBFRDATA(R2)          CALC BUFFER-END + 1 ADDRESS
          BCTR R1,R0                    MINUS 1 FOR EXECUTE
          EX   R1,MVC#3                 MOVE STRING TO BUFFER
          LA   R2,1(R1,R2)              ADD TO BUFFER LENGTH
          STH  R2,CBFRLENG              PUT UPDATED BUFFER LENGTH
          XC   CSTRLENG,CSTRLENG        ZERO STRING LENGTH
         ENDIF                         ENDIF
*
         B     RETURN                  GO RETURN TO CALLER
*
MVC#2    MVC   0(0,R6),1(R4)           (SEE "EX" INSTRUCTION ABOVE)
MVC#3    MVC   0(0,R6),CSTRDATA        (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         SPACE 3
* =================================================================== *
* ROUTINE TO BUILD A STRING OF REQUESTED PCL CONTROL CODES            *
* ------------------------------------------------------------------- *
BLDPCL   DS    0H
*
         XR    R2,R2                    GET CURRENT STRING LENGTH
         LA    R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
*
         IF    CPCLCTL,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(1,R1),CPCLCTL          MOVE PCL PARM TO STRING
          LA   R2,2(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XC   CPCLCTL,CPCLCTL          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLPOR,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(2,R1),PCLPOR           MOVE PCL CODE TO STRING
          MVC  3(1,R1),CPCLPOR          MOVE PCL PARM TO STRING
          MVC  4(1,R1),PCLPOR+2         MOVE PCL CODE TO STRING
          LA   R2,5(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XC   CPCLPOR,CPCLPOR          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLBOL,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(2,R1),PCLBOLD          MOVE PCL CODE TO STRING
          MVC  3(1,R1),CPCLBOL          MOVE PCL PARM TO STRING
          MVC  4(1,R1),PCLBOLD+2        MOVE PCL CODE TO STRING
          LA   R2,5(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XC   CPCLBOL,CPCLBOL          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLITA,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(2,R1),PCLITAL          MOVE PCL CODE TO STRING
          MVC  3(1,R1),CPCLITA          MOVE PCL PARM TO STRING
          MVC  4(1,R1),PCLITAL+2        MOVE PCL CODE TO STRING
          LA   R2,5(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XC   CPCLITA,CPCLITA          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLULI,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(2,R1),PCLULIN          MOVE PCL CODE TO STRING
          MVC  3(1,R1),CPCLULI          MOVE PCL PARM TO STRING
          LA   R2,4(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          IF   CPCLULI,NE,C'@'          IF NOT DISABLE-CODE
           MVC 0(1,R1),PCLULIN+2         MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                 ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)           CALC STRING-END + 1 ADDRESS
          ENDIF                         ENDIF
          XC   CPCLULI,CPCLULI          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLLPI,HT,X'00'
          MVC  0(1,R1),PCLESC           SET ESCAPE CODE
          MVC  1(2,R1),PCLVMI           MOVE PCL CODE TO STRING
          LA   R2,3(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XR   R7,R7                    CLEAR WORK REGISTER
          XI   CPCLLPI,X'F0'            CLEAR PCL PARM ZONE BITS
          ICM  R7,B'0001',CPCLLPI       GET SINGLE DIGIT VALUE
          CVD  R7,PDEC                  CONVERT TO PACKED DECIMAL
          ZAP  PDVD,=P'4800'            48.00 IS THE DIVIDEND
          DP   PDVD,PDIV                DIVIDE BY LPI DIVISOR
          MVC  EDIT,=X'402020214B202060' PUT EDIT PATTERN
          ED   EDIT,PQUO                EDIT THE QUOTIENT
          IF   PQUO,(HT,CP),=P'999'     IF QUOTIENT > 9.99
           MVC 0(5,R1),EDIT+2            MOVE 5 BYTES (??.??) TO STRING
           LA  R2,5(,R2)                 ADD TO STRING LENGTH
          ELSE                          ELSE
           MVC 0(4,R1),EDIT+3            MOVE 4 BYTES (?.??) TO STRING
           LA  R2,4(,R2)                 ADD TO STRING LENGTH
          ENDIF                         ENDIF
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          MVC  0(1,R1),PCLVMI+2         MOVE PCL CODE TO STRING
          LA   R2,1(,R2)                ADD TO STRING LENGTH
          LA   R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
          XC   CPCLLPI,CPCLLPI          CLEAR PCL PARM
         ENDIF
*
         IF    CPCLTOP,(HT,CLC),=H'0'
          IF   CPCLTOP,(HT,CLC),=H'99'
           MVI CLNKRETN,C'I'            SIGNAL INVALID PCL
           BR  R8                       AND RETURN TO CALLER
          ELSE
           MVC 0(1,R1),PCLESC           SET ESCAPE CODE
           MVC 1(2,R1),PCLLTOP          MOVE PCL CODE TO STRING
           LA  R2,3(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XR  R7,R7                    CLEAR WORK REGISTER
           LH  R7,CPCLTOP               GET PCL PARM VALUE
           CVD R7,PDEC                  CONVERT TO PACKED DECIMAL
           UNPK PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL
           OI  PDEC+3,X'F0'             AND CLEAR ANY SIGN
           IF  CPCLTOP,(HT,CLC),=H'9'   IF PCL PARM VALUE > 9
            MVC 0(2,R1),PDEC+2           MOVE 2 BYTES TO STRING
            LA R2,2(,R2)                 ADD TO STRING LENGTH
           ELSE                         ELSE
            MVC 0(1,R1),PDEC+3           MOVE 1 BYTE TO STRING
            LA R2,1(,R2)                 ADD TO STRING LENGTH
           ENDIF                        ENDIF
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           MVC 0(1,R1),PCLLTOP+2        MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XC  CPCLTOP,CPCLTOP          CLEAR PCL PARM
          ENDIF
         ENDIF
*
         IF    CPCLTXL,(HT,CLC),=H'0'
          IF   CPCLTXL,(HT,CLC),=H'255'
           MVI CLNKRETN,C'I'            SIGNAL INVALID PCL
           BR  R8                       AND RETURN TO CALLER
          ELSE
           MVC 0(1,R1),PCLESC           SET ESCAPE CODE
           MVC 1(2,R1),PCLLTXT          MOVE PCL CODE TO STRING
           LA  R2,3(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XR  R7,R7                    CLEAR WORK REGISTER
           LH  R7,CPCLTXL               GET PCL PARM VALUE
           CVD R7,PDEC                  CONVERT TO PACKED DECIMAL
           UNPK PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL
           OI  PDEC+3,X'F0'             AND CLEAR ANY SIGN
           IF  CPCLTXL,(HT,CLC),=H'9'   IF PCL PARM VALUE > 9
            IF CPCLTXL,(HT,CLC),=H'99'   IF PCL PARM VALUE > 99
             MVC 0(3,R1),PDEC+1           MOVE 3 BYTES TO STRING
             LA R2,3(,R2)                 ADD TO STRING LENGTH
            ELSE                         ELSE
             MVC 0(2,R1),PDEC+2           MOVE 2 BYTES TO STRING
             LA R2,2(,R2)                 ADD TO STRING LENGTH
            ENDIF                        ENDIF
           ELSE                         ELSE
            MVC 0(1,R1),PDEC+3           MOVE 1 BYTE TO STRING
            LA R2,1(,R2)                 ADD TO STRING LENGTH
           ENDIF                        ENDIF
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           MVC 0(1,R1),PCLLTXT+2        MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XC  CPCLTXL,CPCLTXL          CLEAR PCL PARM
          ENDIF
         ENDIF
*
         IF    CPCLCPI,(HT,CLC),=H'0'
          IF   CPCLCPI,(HT,CLC),=H'24'
           MVI CLNKRETN,C'I'            SIGNAL INVALID PCL
           BR  R8                       AND RETURN TO CALLER
          ELSE
           MVC 0(1,R1),PCLESC           SET ESCAPE CODE
           MVC 1(2,R1),PCLCPI           MOVE PCL CODE TO STRING
           LA  R2,3(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XR  R7,R7                    CLEAR WORK REGISTER
           LH  R7,CPCLCPI               GET PCL PARM VALUE
           CVD R7,PDEC                  CONVERT TO PACKED DECIMAL
           UNPK PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL
           OI  PDEC+3,X'F0'             AND CLEAR ANY SIGN
           IF  CPCLCPI,(HT,CLC),=H'9'   IF PCL PARM VALUE > 9
            MVC 0(2,R1),PDEC+2           MOVE 2 BYTES TO STRING
            LA R2,2(,R2)                 ADD TO STRING LENGTH
           ELSE                         ELSE
            MVC 0(1,R1),PDEC+3           MOVE 1 BYTE TO STRING
            LA R2,1(,R2)                 ADD TO STRING LENGTH
           ENDIF                        ENDIF
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           MVC 0(1,R1),PCLCPI+2         MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XC  CPCLCPI,CPCLCPI          CLEAR PCL PARM
          ENDIF
         ENDIF
*
         IF    CPCLLFT,(HT,CLC),=H'0'
          IF   CPCLLFT,(HT,CLC),=H'99'
           MVI CLNKRETN,C'I'            SIGNAL INVALID PCL
           BR  R8                       AND RETURN TO CALLER
          ELSE
           MVC 0(1,R1),PCLESC           SET ESCAPE CODE
           MVC 1(2,R1),PCLCLFT          MOVE PCL CODE TO STRING
           LA  R2,3(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XR  R7,R7                    CLEAR WORK REGISTER
           LH  R7,CPCLLFT               GET PCL PARM VALUE
           CVD R7,PDEC                  CONVERT TO PACKED DECIMAL
           UNPK PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL
           OI  PDEC+3,X'F0'             AND CLEAR ANY SIGN
           IF  CPCLLFT,(HT,CLC),=H'9'   IF PCL PARM VALUE > 9
            MVC 0(2,R1),PDEC+2           MOVE 2 BYTES TO STRING
            LA R2,2(,R2)                 ADD TO STRING LENGTH
           ELSE                         ELSE
            MVC 0(1,R1),PDEC+3           MOVE 1 BYTE TO STRING
            LA R2,1(,R2)                 ADD TO STRING LENGTH
           ENDIF                        ENDIF
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           MVC 0(1,R1),PCLCLFT+2        MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XC  CPCLLFT,CPCLLFT          CLEAR PCL PARM
          ENDIF
         ENDIF
*
         IF    CPCLRGT,(HT,CLC),=H'0'
          IF   CPCLRGT,(HT,CLC),=H'255'
           MVI CLNKRETN,C'I'            SIGNAL INVALID PCL
           BR  R8                       AND RETURN TO CALLER
          ELSE
           MVC 0(1,R1),PCLESC           SET ESCAPE CODE
           MVC 1(2,R1),PCLCRGT          MOVE PCL CODE TO STRING
           LA  R2,3(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XR  R7,R7                    CLEAR WORK REGISTER
           LH  R7,CPCLRGT               GET PCL PARM VALUE
           CVD R7,PDEC                  CONVERT TO PACKED DECIMAL
           UNPK PDEC(4),PDEC+5(3)       THEN CHANGE TO ZONED DECIMAL
           OI  PDEC+3,X'F0'             AND CLEAR ANY SIGN
           IF  CPCLRGT,(HT,CLC),=H'9'   IF PCL PARM VALUE > 9
            IF CPCLRGT,(HT,CLC),=H'99'   IF PCL PARM VALUE > 99
             MVC 0(3,R1),PDEC+1           MOVE 3 BYTES TO STRING
             LA R2,3(,R2)                 ADD TO STRING LENGTH
            ELSE                         ELSE
             MVC 0(2,R1),PDEC+2           MOVE 2 BYTES TO STRING
             LA R2,2(,R2)                 ADD TO STRING LENGTH
            ENDIF                        ENDIF
           ELSE                         ELSE
            MVC 0(1,R1),PDEC+3           MOVE 1 BYTE TO STRING
            LA R2,1(,R2)                 ADD TO STRING LENGTH
           ENDIF                        ENDIF
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           MVC 0(1,R1),PCLCRGT+2        MOVE PCL CODE TO STRING
           LA  R2,1(,R2)                ADD TO STRING LENGTH
           LA  R1,CSTRDATA(R2)          CALC STRING-END + 1 ADDRESS
           XC  CPCLRGT,CPCLRGT          CLEAR PCL PARM
          ENDIF
         ENDIF
*
         STH   R2,CSTRLENG             PUT NEW STRING LENGTH
*
         BR    R8
* =================================================================== *
         SPACE 3
* =================================================================== *
RETURN   XC    EIBRCODE,EIBRCODE       CLEAR EIB RETURN/RESPONSE CODES
         XC    EIBRESP,EIBRESP         SO THAT THE LINKING PROGRAM
         XC    EIBRESP2,EIBRESP2       WON'T THINK THEY'RE FROM "LINK"
         DFHEIRET
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
TRTABLE  DC    256XL1'00'
*                       3270 CC ORDERS
         ORG   TRTABLE+C'E'            END-OF-MEDIUM
         DC    X'01'
         ORG   TRTABLE+C'F'            FORM-FEED
         DC    X'02'
         ORG   TRTABLE+C'N'            NEW-LINE
         DC    X'03'
         ORG   TRTABLE+C'O'            OMIT-CC
         DC    X'05'
         ORG   TRTABLE+C'P'            PCL CODE
         DC    X'05'
         ORG   TRTABLE+C'R'            CARRIAGE-RETURN
         DC    X'04'
*                       ASA CC ORDERS
         ORG   TRTABLE+C' '            SINGLE-SPACE
         DC    X'03'
         ORG   TRTABLE+C'0'            DOUBLE-SPACE
         DC    X'03'
         ORG   TRTABLE+C'-'            TRIPLE-SPACE
         DC    X'03'
         ORG   TRTABLE+C'+'            NO-SPACE
         DC    X'04'
         ORG   TRTABLE+C'1'            CHANNEL 1 (TOP-OF-PAGE)
         DC    X'02'
         DC    X'0303030303030303'     CHANNEL 2 TO 9
         ORG   TRTABLE+C'A' - C'C'
         DC    X'030303'               CHANNEL A TO C
         ORG
*
CCTABLE  DC    A(CCEML)                END-OF-MEDIUM
         DC    A(CCFFL)                FORM-FEED
         DC    A(CCNLL)                NEW-LINE
         DC    A(CCCRL)                CARRIAGE-RETURN
         DC    A(OMITL)                OMIT-CC
*
CCEML    DC    AL1(L'CCEM)
CCEM     DC    X'0D19'                 END-OF-MEDIUM
CCFFL    DC    AL1(L'CCFF)
CCFF     DC    X'0D0C0D'               FORM-FEED
CCNLL    DC    AL1(L'CCNL)
CCNL     DC    X'15'                   NEW-LINE
CCCRL    DC    AL1(L'CCCR)
CCCR     DC    X'0D'                   CARRIAGE-RETURN
OMITL    DC    AL1(L'OMIT)
OMIT     DC    X'0008'                 OMIT-CC
*
PCLESC   DC    X'27'                   ESCAPE CODE
PCLPOR   DC    X'5093',C'O'            C'&l#O' PAGE ORIENTATION
PCLBOLD  DC    X'4DA2',C'B'            C'(s#B' BOLD FORMATTING
PCLITAL  DC    X'4DA2',C'S'            C'(s#S' ITALIC FORMATTING
PCLULIN  DC    X'5084',C'D'            C'&d#D' UNDERLINE FORMATTING
PCLVMI   DC    X'5093',C'C'            C'&l#C' VERTICAL MOTION INDEX
PCLLPI   DC    X'5093',C'D'            C'&l#D' LINES PER INCH
PCLLTOP  DC    X'5093',C'E'            C'&l#E' LINES TOP MARGIN
PCLLTXT  DC    X'5093',C'F'            C'&l#F' LINES OF TEXT
PCLHMI   DC    X'5092',C'H'            C'&k#H' HORIZONTAL MOTION INDEX
PCLCPI   DC    X'4DA2',C'H'            C'(s#H' CHARS PER INCH
PCLCLFT  DC    X'5081',C'L'            C'&a#L' CHARS LEFT MARGIN
PCLCRGT  DC    X'5081',C'M'            C'&a#M' CHARS RIGHT MARGIN
*
QUERY    DS    XL7                     QUERY AREA
         ORG   QUERY
QUERYL   DC    AL2(L'QUERY)            STRUCTURED FIELD LENGTH
         DC    X'01'                   READ PARTITION
         DC    X'FF'                   PARTITION ID (X'FF' = "QUERY")
         DC    X'03'                   QUERY LIST
         DC    B'00000000'             QUERY TYPE (LIST ONLY)
QUERYI   DC    X'81'                   QUERY ID (USABLE AREA)
         ORG
*
         LTORG
* =================================================================== *
         END
