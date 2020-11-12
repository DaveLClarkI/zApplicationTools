LIBM     TITLE 'LIBMAN   - INVOKE VSE/LIBRARIAN FOR COBOL PROGRAMS'     00000100
         PRINT ON,GEN                                                   00000200
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
*       L I B R A R I A N   M A N A G E R   S U B R O U T I N E       * 00000500
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
*      This routine provides  the  COBOL  programmer  with a means of * 00001700
* easily gaining programatic access to  VSE/SP v2 (and up) libraries. * 00001800
* Additional information that is needed (but not supplied here) about * 00001900
* the Librarian CALL Interface may be found in the IBM manual "VSE/AF * 00002000
* Diagnosis Reference - Librarian".                                   * 00002100
*                                                                     * 00002200
* =================================================================== * 00002300
*                                                                     * 00002400
*                           IMPLEMENTATION                            * 00002500
*                                                                     * 00002600
*      The invoking COBOL program should  use the following data area * 00002700
* in its WORKING-STORAGE SECTION for passing parameters to the LIBMAN * 00002800
* subroutine:                                                         * 00002900
*                                                                     * 00003000
*      01  LIBMAN-PARMS.                                              * 00003100
*          05  LIB-IPT-EXIT            PIC  X(1)    VALUE 'N'.        * 00003200
*              88  LIB-SYSIPT-OMITTED               VALUE 'N'.        * 00003300
*              88  LIB-SYSIPT-INCLUDED              VALUE 'Y'.        * 00003400
*          05  LIB-LOG-EXIT            PIC  X(1)    VALUE 'N'.        * 00003500
*              88  LIB-SYSLOG-OMITTED               VALUE 'N'.        * 00003600
*              88  LIB-SYSLOG-INCLUDED              VALUE 'Y'.        * 00003700
*          05  LIB-LST-EXIT            PIC  X(1)    VALUE 'N'.        * 00003800
*              88  LIB-SYSLST-OMITTED               VALUE 'N'.        * 00003900
*              88  LIB-SYSLST-INCLUDED              VALUE 'Y'.        * 00004000
*          05  LIB-PCH-EXIT            PIC  X(1)    VALUE 'N'.        * 00004100
*              88  LIB-SYSPCH-OMITTED               VALUE 'N'.        * 00004200
*              88  LIB-SYSPCH-INCLUDED              VALUE 'Y'.        * 00004300
*          05  LIB-FMT-EXIT            PIC  X(1)    VALUE 'N'.        * 00004400
*              88  LIB-FORMAT-OMITTED               VALUE 'N'.        * 00004500
*              88  LIB-FORMAT-INCLUDED              VALUE 'Y'.        * 00004600
*          05  FILLER                  PIC  X(1)    VALUE SPACE.      * 00004700
*          05  LIB-RTN-CODE            PIC  9(4)    COMP VALUE ZEROES.* 00004800
*     *                                                               * 00004900
*          05  LIB-PGM-COMM.                                          * 00005000
*            08  FILLER                PIC  X(44)   VALUE LOW-VALUES. * 00005100
*            08  LIB-USR-COMM          PIC  X(04)   VALUE LOW-VALUES. * 00005200
*            08  FILLER                PIC  X(16)   VALUE LOW-VALUES. * 00005300
*                                                                     * 00005400
* In addition,  the COBOL program MUST contain at least a SYSIPT exit * 00005500
* routine entry point to pass standard LIBR-format statements and MAY * 00005600
* contain other exit routine entry points named as follows:           * 00005700
*                                                                     * 00005800
*      LIBMIPT  -  SYSIPT input exit routine                          * 00005900
*      LIBMLOG  -  SYSLOG output exit routine                         * 00006000
*      LIBMLST  -  SYSLST output exit routine                         * 00006100
*      LIBMPCH  -  SYSPCH output exit routine                         * 00006200
*      LIBMFMT  -  Formatted output exit routine                      * 00006300
*                                                                     * 00006400
* Each exit routine MUST be in a  COBOL SECTION by itself and have NO * 00006500
* PERFORMs or GO TOs  that  reference  paragraphs outside of the exit * 00006600
* routine's own SECTION.   Exit routines MAY,  however,  reference or * 00006700
* modify any items  in  the  WORKING-STORAGE  SECTION of the invoking * 00006800
* COBOL program.                                                      * 00006900
*      When the  exit  routine  receives  control  it  is  passed the * 00007000
* address of the LIB-PGM-COMM area  as passed to "LIBMAN" originally. * 00007100
* Hence,  the invoking COBOL program's LINKAGE SECTION should contain * 00007200
* the following data area:                                            * 00007300
*                                                                     * 00007400
*      01  LIBRCOMM-PARMS.                                            * 00007500
*          05  FILLER                  PIC  X(44).                    * 00007600
*          05  LIB-COM-USER            PIC  X(04).                    * 00007700
*          05  FILLER                  PIC  X(16).                    * 00007800
*                                                                     * 00007900
* And each exit routine (using appropriate unique paragraph and entry * 00008000
* point names) should be coded similar to the following:              * 00008100
*                                                                     * 00008200
*      100-SYSIPT-ROUTINE SECTION.                                    * 00008300
*          ENTRY  'LIBMIPT'  USING LIBRCOMM-PARMS.                    * 00008400
*      110-HANDLE-SYSIPT.                                             * 00008500
*           .                                                         * 00008600
*           .                                                         * 00008700
*           .                                                         * 00008800
*          (your exit routine statements)                             * 00008900
*           .                                                         * 00009000
*           .                                                         * 00009100
*           .                                                         * 00009200
*      190-SYSIPT-EXIT.                                               * 00009300
*          EXIT PROGRAM.                                              * 00009400
*                                                                     * 00009500
* =================================================================== * 00009600
*                                                                     * 00009700
*                             INVOKATION                              * 00009800
*                                                                     * 00009900
*      For each exit routine  you  wish  to  use,  you must supply an * 00010000
* appropriate I/O area in the WORKING-STORAGE SECTION of your program * 00010100
* similar to one or more of the following:                            * 00010200
*                                                                     * 00010300
*      01  SYSIPT-AREA.                                               * 00010400
*        02  SYSIPT-CTRL.                                             * 00010500
*          05  SYSIPT-LENG             PIC S9(04)   COMP VALUE +0088. * 00010600
*          05  FILLER                  PIC  X(06)   VALUE LOW-VALUES. * 00010700
*        02  SYSIPT-RECD.                                             * 00010800
*          05  SYSIPT-DATA             PIC  X(80)   VALUE SPACES.     * 00010900
*                                                                     * 00011000
*      01  SYSLOG-AREA.                                               * 00011100
*        02  SYSLOG-CTRL.                                             * 00011200
*          05  SYSLOG-LENG             PIC S9(04)   COMP VALUE +0255. * 00011300
*          05  FILLER                  PIC  X(06)   VALUE LOW-VALUES. * 00011400
*        02  SYSLOG-RECD.                                             * 00011500
*          05  SYSLOG-DATA             PIC  X(247)  VALUE SPACES.     * 00011600
*                                                                     * 00011700
*      01  SYSLST-AREA.                                               * 00011800
*        02  SYSLST-CTRL.                                             * 00011900
*          05  SYSLST-LENG             PIC S9(04)   COMP VALUE +0129. * 00012000
*          05  FILLER                  PIC  X(06)   VALUE LOW-VALUES. * 00012100
*        02  SYSLST-RECD.                                             * 00012200
*          05  SYSLST-VFC              PIC  X(01)   VALUE SPACES.     * 00012300
*          05  SYSLST-DATA             PIC  X(120)  VALUE SPACES.     * 00012400
*                                                                     * 00012500
*      01  SYSPCH-AREA.                                               * 00012600
*        02  SYSPCH-CTRL.                                             * 00012700
*          05  SYSPCH-LENG             PIC S9(04)   COMP VALUE +0088. * 00012800
*          05  FILLER                  PIC  X(06)   VALUE LOW-VALUES. * 00012900
*        02  SYSPCH-RECD.                                             * 00013000
*          05  SYSPCH-DATA             PIC  X(80)   VALUE SPACES.     * 00013100
*                                                                     * 00013200
*      01  FORMAT-AREA.                                               * 00013300
*        02  FORMAT-CTRL.                                             * 00013400
*          05  FORMAT-LENG             PIC S9(04)   COMP VALUE +0080. * 00013500
*          05  FILLER                  PIC  X(02)   VALUE LOW-VALUES. * 00013600
*        02  FORMAT-RECD.                                             * 00013700
*          05  FORMAT-DATA             PIC  X(76)   VALUE SPACES.     * 00013800
*                                                                     * 00013900
* Then you indicate which exit routines you are using and CALL LIBMAN * 00014000
* specifying its parameter area  and  each  of  the I/O areas you are * 00014100
* using.  For example:                                                * 00014200
*                                                                     * 00014300
*          MOVE     'Y'      TO LIB-IPT-EXIT                          * 00014400
*          MOVE     'N'      TO LIB-LOG-EXIT                          * 00014500
*          MOVE     'Y'      TO LIB-LST-EXIT                          * 00014600
*          MOVE     'Y'      TO LIB-PCH-EXIT                          * 00014700
*          MOVE     'N'      TO LIB-FMT-EXIT                          * 00014800
*          CALL   'LIBMAN'   USING LIBMAN-PARMS                       * 00014900
*                                  SYSIPT-AREA                        * 00015000
*                                  SYSLST-AREA                        * 00015100
*                                  SYSPCH-AREA                        * 00015200
*          IF  LIB-RTN-CODE > ZERO                                    * 00015300
*     *        your-error-handling-code                               * 00015400
*          END-IF                                                     * 00015500
*                                                                     * 00015600
* Note that in all cases,  the I/O  area names specified in the USING * 00015700
* clause MUST be in  the  following order (you can,  of course,  omit * 00015800
* those you are NOT using):                                           * 00015900
*                                                                     * 00016000
*      SYSIPT exit routine input area                                 * 00016100
*      SYSLOG exit routine output area                                * 00016200
*      SYSLST exit routine output area                                * 00016300
*      SYSPCH exit routine output area                                * 00016400
*      Formatted output exit routine output area                      * 00016500
*                                                                     * 00016600
* Finally,  in order for their to be  enough room in the partition to * 00016700
* load LIBR (and for  it  to  load  its modules),  execute your COBOL * 00016800
* program as follows:                                                 * 00016900
*                                                                     * 00017000
*    // EXEC   PGM=pgmname,SIZE=(pgmname,256K) ...                    * 00017100
*                                                                     * 00017200
* Note that other  special  considerations  may  be  required if your * 00017300
* COBOL program contains  an  internal  SORT  or CALLs other routines * 00017400
* requiring program storage in the partition.                         * 00017500
*                                                                     * 00017600
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00017700
         EJECT                                                          00017800
* =================================================================== * 00017900
LIBPARM  DSECT                         LIBMAN PARMS DUMMY SECTION       00018000
         USING *,R10                   ESTABLISH REG 10 AS BASE         00018100
* =================================================================== * 00018200
LIBIPT   DS    CL1                     SYSIPT EXIT ROUTINE              00018300
LIBLOG   DS    CL1                     SYSLOG EXIT ROUTINE              00018400
LIBLST   DS    CL1                     SYSLST EXIT ROUTINE              00018500
LIBPCH   DS    CL1                     SYSPCH EXIT ROUTINE              00018600
LIBFMT   DS    CL1                     FORMATTED EXIT ROUTINE           00018700
         DS    CL1                                                      00018800
LIBRTC   DS    H                       RETURN CODE                      00018900
*                                                                       00019000
INLCPARM DS    AL2                     CONTROL BLOCK LENGTH             00019100
         DS    XL2                     RESERVED                         00019200
INLCPCID DS    XL4                     CALLER IDENTIFICATION            00019300
SYSIPT   DS    AL4                     (EXIT ROUTINE ADDRESS)           00019400
         DS    AL4                     (INPUT AREA ADDRESS)             00019500
SYSLOG   DS    AL4                     (EXIT ROUTINE ADDRESS)           00019600
         DS    AL4                     (I-O AREA ADDRESS)               00019700
SYSLST   DS    AL4                     (EXIT ROUTINE ADDRESS)           00019800
         DS    AL4                     (OUTPUT AREA ADDRESS)            00019900
SYSPCH   DS    AL4                     (EXIT ROUTINE ADDRESS)           00020000
         DS    AL4                     (OUTPUT AREA ADDRESS)            00020100
         DS    XL1                     SYSLST LINE COUNT                00020200
FMTOPT   DS    XL1                     INTERNAL FUNCTION REQUEST NO     00020300
INLCPFB1 DS    XL1                     INTERNAL FLAG BYTE #1            00020400
INLCPFB2 DS    XL1                     INTERNAL FLAG BYTE #2            00020500
         DS    AL4                     USER AREA                        00020600
SYSFMT   DS    AL4                     (EXIT ROUTINE ADDRESS)           00020700
         DS    AL4                     (OUTPUT AREA ADDRESS)            00020800
         DS    CL8                     RESERVED                         00020900
INLCPLEN EQU   *-INLCPARM                                               00021000
* =================================================================== * 00021100
         EJECT                                                          00021200
* =================================================================== * 00021300
*        REGISTER USAGES                                                00021400
* =================================================================== * 00021500
R0       EQU   0                       PARM REG                         00021600
R1       EQU   1                       PARM REG                         00021700
R2       EQU   2                                                        00021800
R3       EQU   3                                                        00021900
R4       EQU   4                                                        00022000
R5       EQU   5                                                        00022100
R6       EQU   6                                                        00022200
R7       EQU   7                                                        00022300
R8       EQU   8                                                        00022400
R9       EQU   9                                                        00022500
R10      EQU   10                      LIBPARM DSECT BASE REG           00022600
R11      EQU   11                                                       00022700
R12      EQU   12                      LIBMAN CSECT BASE REG            00022800
R13      EQU   13                      REGISTER SAVEAREA                00022900
R14      EQU   14                      RETURN REG                       00023000
R15      EQU   15                      ENTRY REG                        00023100
* =================================================================== * 00023200
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         COPY  IF
* =================================================================== *
         EJECT                                                          00023300
* =================================================================== * 00023400
LIBMAN   CSECT                                                          00023500
         USING LIBMAN,15               ESTABLISH TEMP BASE REGISTER     00023600
         WXTRN LIBMIPT,LIBMLOG,LIBMLST,LIBMPCH,LIBMFMT                  00023700
* =================================================================== * 00023800
         B     START                   BRANCH AROUND CORE EYE-CATCHER   00023900
*                                                                       00024000
         DC    CL8'LIBMAN'             PROGRAM                          00024100
         DC    CL4'1.0'                VERSION                          00024200
         DC    CL4'DLC'                AUTHOR                           00024300
         DC    CL8'(C)MAY91'           DATE                             00024400
*                                                                       00024500
START    DS    0H                                                       00024600
         SAVE  (14,12)                 SAVE CALLER'S REGISTERS          00024700
         LR    R12,R15                 LOAD PROGRAM BASE REGISTER       00024800
         DROP  15                      DISESTABLISH TEMP BASE REGISTER  00024900
         USING LIBMAN,R12              ESTABLISH PERMANENT BASE REG     00025000
         XR    R15,R15                 CLEAR RETURN CODE REGISTER       00025100
         ST    R15,16(,R13)            PUT IN CALLER'S SAVEAREA         00025200
         LA    R15,SAVEAREA            LOAD ADDRESS OF MY SAVEAREA      00025300
         ST    R15,8(,R13)             SET NEXT SAVEAREA IN CALLER'S    00025400
         ST    R13,4(,R15)             SET PREV SAVEAREA IN MINE        00025500
         LR    R13,R15                 LOAD REG WITH MY SAVEAREA ADDR   00025600
         ST    R13,0(,R13)             AND STORE IT IN MY SAVEAREA      00025700
         XR    R15,R15                 CLEAR RETURN CODE REGISTER       00025800
*                                                                       00025900
         L     R10,0(,R1)              GET ADDRESS OF PARAMETER         00026000
         LA    R1,4(,R1)               INCREMENT TO NEXT PARM           00026100
         LA    R2,INLCPLEN             GET CONTROL BLOCK LENGTH         00026200
         STH   R2,INLCPARM             AND STORE IT IN CONTROL BLOCK    00026300
         LA    R2,0                    SET SPF CALLER IDENTIFICATION    00026400
         ST    R2,INLCPCID             AND STORE IT IN CONTROL BLOCK    00026500
*        OI    INLCPFB1,X'20'   ???    SET "MSHP" BYPASS ON             00026600
*                                                                       00026700
         IF    LIBIPT,EQ,C'Y'          IF SYSIPT EXIT ROUTINE SUPPLIED  00026800
          L    R2,=V(LIBMIPT)           GET EXIT RTN ADDRESS            00026900
          L    R3,0(,R1)                GET I/O AREA ADDRESS            00027000
          LA   R1,4(,R1)                INCREMENT TO NEXT PARM          00027100
         ELSE                          ELSE                             00027200
          LA   R2,0                     SET EXIT RTN ADDRESS TO ZERO    00027300
          LR   R3,R2                    SET I/O AREA ADDRESS TO ZERO    00027400
         ENDIF                         ENDIF                            00027500
         STM   R2,R3,SYSIPT            PUT ADDRESSES IN CONTROL BLOCK   00027600
*                                                                       00027700
         IF    LIBLOG,EQ,C'Y'          IF SYSLOG EXIT ROUTINE SUPPLIED  00027800
          L    R2,=V(LIBMLOG)           GET EXIT RTN ADDRESS            00027900
          L    R3,0(,R1)                GET I/O AREA ADDRESS            00028000
          LA   R1,4(,R1)                INCREMENT TO NEXT PARM          00028100
         ELSE                          ELSE                             00028200
          LA   R2,0                     SET EXIT RTN ADDRESS TO ZERO    00028300
          LR   R3,R2                    SET I/O AREA ADDRESS TO ZERO    00028400
         ENDIF                         ENDIF                            00028500
         STM   R2,R3,SYSLOG            PUT ADDRESSES IN CONTROL BLOCK   00028600
*                                                                       00028700
         IF    LIBLST,EQ,C'Y'          IF SYSLST EXIT ROUTINE SUPPLIED  00028800
          L    R2,=V(LIBMLST)           GET EXIT RTN ADDRESS            00028900
          L    R3,0(,R1)                GET I/O AREA ADDRESS            00029000
          LA   R1,4(,R1)                INCREMENT TO NEXT PARM          00029100
         ELSE                          ELSE                             00029200
          LA   R2,0                     SET EXIT RTN ADDRESS TO ZERO    00029300
          LR   R3,R2                    SET I/O AREA ADDRESS TO ZERO    00029400
         ENDIF                         ENDIF                            00029500
         STM   R2,R3,SYSLST            PUT ADDRESSES IN CONTROL BLOCK   00029600
*                                                                       00029700
         IF    LIBPCH,EQ,C'Y'          IF SYSPCH EXIT ROUTINE SUPPLIED  00029800
          L    R2,=V(LIBMPCH)           GET EXIT RTN ADDRESS            00029900
          L    R3,0(,R1)                GET I/O AREA ADDRESS            00030000
          LA   R1,4(,R1)                INCREMENT TO NEXT PARM          00030100
         ELSE                          ELSE                             00030200
          LA   R2,0                     SET EXIT RTN ADDRESS TO ZERO    00030300
          LR   R3,R2                    SET I/O AREA ADDRESS TO ZERO    00030400
         ENDIF                         ENDIF                            00030500
         STM   R2,R3,SYSPCH            PUT ADDRESSES IN CONTROL BLOCK   00030600
*                                                                       00030700
         IF    LIBFMT,EQ,C'Y'          IF FRMTTD EXIT ROUTINE SUPPLIED  00030800
          L    R2,=V(LIBMFMT)           GET EXIT RTN ADDRESS            00030900
          L    R3,0(,R1)                GET I/O AREA ADDRESS            00031000
          LA   R4,1                     SET FORMATTED OUTPUT OPTION     00031100
         ELSE                          ELSE                             00031200
          LA   R2,0                     SET EXIT RTN ADDRESS TO ZERO    00031300
          LR   R3,R2                    SET I/O AREA ADDRESS TO ZERO    00031400
          LR   R4,R2                    SET FORMATTED OUTPUT OPTION     00031500
         ENDIF                         ENDIF                            00031600
         STM   R2,R3,SYSFMT            PUT ADDRESSES IN CONTROL BLOCK   00031700
         STC   R4,FMTOPT               PUT FMT OPTION IN CONTROL BLOCK  00031800
*                                                                       00031900
         L     R11,SAVEAREA+4          GET ADDRESS OF CALLER'S SAVEAREA 00032000
         MVC   ORIGAREA(72),0(R11)     COPY IT TO PREVENT OVERLAY       00032100
         L     R15,LIBRADDR            CHECK LIBR ENTRY POINT           00032200
         IF    R15,(ZERO,LTR),R15      IF ZERO                          00032300
          COMRG REG=(R1)                GET ADDRESS OF COMREG IN R1     00032400
          L    R2,36(R1)                GET NEXT FREE ADDRESS           00032500
          LA   R2,8(,R2)                ADD X'08' TO THAT ADDRESS       00032600
          SRL  R2,3                     AND TRUNCATE LOW 3 BITS         00032700
          SLL  R2,3                     TO POINT TO NEXT DOUBLE-WORD    00032800
          LOAD LIBR,(R2),SYS=YES,RET=YES                                00032900
          IF   R15,(NZERO,LTR),R15      IF NOT LOADED OK                00033000
           AH  R15,=H'100'               ADD 100 FOR "LOAD" RET CODES   00033100
           B   FINISH                    GO RETURN TO CALLER            00033200
          ENDIF                         ENDIF                           00033300
          LR   R15,R1                   COPY ADDRESS TO ENTRY REG       00033400
          ST   R15,LIBRADDR             SAVE LIBR ENTRY POINT           00033500
         ENDIF                         ENDIF                            00033600
         LA    R1,INLCPARM             GET ADDRESS OF PARM AREA         00033700
         ST    R1,PARMLIST             PUT ADDRESS IN PARM LIST         00033800
         OI    PARMLIST,X'80'          SET LAST ADDRESS IN LIST         00033900
         LA    R1,PARMLIST             GET ADDRESS OF PARM LIST         00034000
         BALR  R14,R15                 INVOKE VSE LIBRARIAN             00034100
         MVC   0(72,R11),ORIGAREA      RESTORE CALLER'S ORIGINAL SVAREA 00034200
* ------------------------------------------------------------------- * 00034300
FINISH   DS    0H                                                       00034400
* ------------------------------------------------------------------- * 00034500
         STH   R15,LIBRTC              PASS BACK RETURN CODE            00034600
         L     R13,4(,R13)             GET ADDRESS OF CALLER'S SAVEAREA 00034700
         ST    R15,16(,R13)            SAVE RETURN CODE IN REG 15 LOC   00034800
         RETURN (14,12)                RETURN TO CALLER                 00034900
* =================================================================== * 00035000
         EJECT                                                          00035100
* =================================================================== * 00035200
STORAGE  DS    0D                                                       00035300
* =================================================================== * 00035400
SAVEAREA DC    18F'0'                                                   00035500
ORIGAREA DC    18F'0'                                                   00035600
LIBRADDR DC    A(0)                                                     00035700
PARMLIST DC    A(0)                                                     00035800
         LTORG                                                          00035900
* =================================================================== * 00036000
         END                                                            00036100
