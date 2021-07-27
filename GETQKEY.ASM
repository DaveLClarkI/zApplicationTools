QKEY     TITLE 'GET A KWIKKEY AIX ENTRY FROM TABLE'
         PRINT ON,GEN
GETQKEY  AMODE 31
GETQKEY  RMODE ANY
         SPACE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       K W I K K E Y   T A B L E   A C C E S S   R O U T I N E       *
*                                                                     *
*                                                                     *
* Winsupply Group Services                            Written by:     *
* 3110 Kettering Boulevard                         Dave Leonard Clark *
* Morraine, OH  45439  USA                           (937)751-3300    *
*                                                                     *
* No warranty is expressed  or  implied.   Neither the author nor the *
* company is responsible for  any  loss  or damage resulting from the *
* use of this code.  Source is provided on an as-is basis.            *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This routine will find  and  return  a KWIKKEY alternate index *
* definition entry from the internal program table.   Since a KWIKKEY *
* alternate index supports non-contiguous  data to build an alternate *
* key  (and  "sparse"  indexes),   the   alternate  index  cannot  be *
* automatically maintained by VSAM  and,  indeed,  cannot have a PATH *
* entry at all.   Thus,  this  routine  provides  a  means whereby an *
* application program can  obtain  the  dynamic information needed to *
* provide its own path between the KWIKKEY alternate index and a VSAM *
* base cluster.                                                       *
*                                                                     *
*      This routine may be used either  online or in batch because it *
* requires no CICS services.   In  either case,  however,  it must be *
* CALLed.   It cannot be LINKed to in CICS.   Thus, both in batch and *
* online you can do the following:                                    *
*                                                                     *
* CALL GETQKEY USING GETQKEY-PARMS.                                   *
*                                                                     *
*                                                                     *
*      To code the alternate index table, at the end of this routine, *
* you can specify up to nine  non-contiguous data areas from the base *
* record (to be concatenated,  in  order  from  left to right) as the *
* alternate  index  key  and  up  to  nine  either  INCLUDE  or  OMIT *
* conditions  (separated  by  {AND|OR}  articles).   The include/omit *
* conditions are useful when you don't want an alternate index record *
* for every record in the base cluster (i.e., a sparse versus a dense *
* index).   It is up to the  caller  whether to apply or ignore these *
* conditions when maintaining the associated alternate index.         *
*                                                                     *
*      Alternate index entry syntax is as follows (an upper-case word *
* is required  as-is,  a  lower-case  word  requires your replacement *
* value, commas/parentheses are required, and continuation characters *
* are in column 72):                                                  *
*                                                                     *
* altname ALTIDX BASELBL=basename,BASERECL=reclen,                  X *
*                BASEKEY=(offset,length),                           X *
*                AIXKEYS=(...)<,{INCLUDE|OMIT}=(...) >                *
*                                                                     *
*                                                                     *
*      Required alternate index key syntax  is as follows (spaces are *
* for readability -- i.e., do not code them in your entry):           *
*                                                                     *
* AIXKEYS=((offset,length) < ,(offset,length) < ... > > )             *
*                                                                     *
*                                                                     *
*      Optional include/omit selection  syntax  is as follows (spaces *
* are for readability -- i.e., do not code them in your entry):       *
*                                                                     *
* {INCLUDE|OMIT}=(condition < ,{AND|OR},condition < ... > > )         *
*                                                                     *
*                                                                     *
*      If coded, the include/omit "condition" syntax is as follows:   *
*                                                                     *
* (offset,length,operator,value)                                      *
*                                                                     *
*                                                                     *
* Where: <>        indicates optional operands;                       *
*        {|}       indicates mutually exclusive operands;             *
*        altname   is the alternate index label (up to 7 characters); *
*        basename  is the base cluster label (up to 7 characters);    *
*        reclen    is the base cluster record length (up to 32kb-10); *
*        offset    to the data area (zero-based) in the base record;  *
*        length    is the byte-length of that data area;              *
*        operator  must be one of {EQ|NE|LT|LE|GT|GE}; and,           *
*        value     is a self-defining term (whose data type and byte  *
*                  length matches the target data area for testing):  *
*                    C'aaaaaaaaaa'  (up to 10 characters/zoned data); *
*                    B'11111111'    (up to 8, 16, 24, or 32 bit data) *
*                    P'±1234567.89' (signed decimal, up to 19 digits) *
*                    H'±12345'      (signed integer, up to 32kb-1);   *
*                    F'±123456789'  (signed integer, up to  2gb-1);   *
*                    D'±1234567890' (signed integer, up to  8eb-1).   *
*                                                                     *
* NOTE:  There is no support for grouping conditions with parentheses *
* for the purpose of altering {AND|OR} precedence processing.   Thus, *
* simply code conditions  with  the  understanding  that  an AND will *
* accumulate as part of a single Boolean  test and an OR will start a *
* new, exclusive Boolean test.                                        *
*                                                                     *
* ALSO:  When using the NE (not-equal) operator, you can have trouble *
* with condition results if that operator is combined with the use of *
* the OR separator.   Meaning,  the Boolean results can end up always *
* being true on both sides of the OR.  The NE operator does work well *
* with the AND separator.  Just avoid it with the OR separator.       *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This program should be  compiled  as a standard batch program. *
* The following is an example  of  a  COBOL copybook that can be used *
* with this routine.                                                  *
*                                                                     *
* ---1----+----2----+----3----+----4----+----5----+----6----+----7--  *
*  01  GETQKEY-AREA.                                                  *
*    03  GETQKEY                   PIC  X(8)    VALUE 'GETQKEY'.      *
*    03  GETQKEY-PARMS.                                               *
*      05  QKEY-ALTLBL             PIC  X(8)    VALUE SPACES.         *
*      05  QKEY-SPECREQ            REDEFINES    QKEY-ALTLBL.          *
*        88  QKEY-TABLREQ                       VALUE '*ADDRESS'.     *
*        07  QKEY-SPECKEY          PIC  X(4).                         *
*          88  QKEY-TABLRET                     VALUE '*AD='.         *
*        07  QKEY-TABLADR                       POINTER.              *
*      05  QKEY-BASELBL            PIC  X(8)    VALUE SPACES.         *
* *                                                                   *
*      05  QKEY-ATTRS.                                                *
*        07  QKEY-BASERECL         PIC S9(4)    BINARY.               *
*        07  QKEY-BASEKEY.                                            *
*          09  QKEY-BASEKEYO       PIC S9(4)    BINARY.               *
*          09  QKEY-BASEKEYL       PIC S9(4)    BINARY.               *
*        07  QKEY-ALTKEYL          PIC S9(4)    BINARY.               *
*        07  QKEY-AIXKEYS          PIC S9(4)    BINARY.               *
*        07  QKEY-AIXKEY                        OCCURS 9 TIMES.       *
*          09  QKEY-AIXKEYO        PIC S9(4)    BINARY.               *
*          09  QKEY-AIXKEYL        PIC S9(4)    BINARY.               *
* *                                                                   *
*      05  QKEY-CONDS.                                                *
*        07  QKEY-AIXCONDS         PIC S9(4)    BINARY.               *
*        07  QKEY-AIXCONDE                      OCCURS 9 TIMES.       *
*          09  QKEY-AIXCONDC       PIC  X(2).                         *
*          09  QKEY-AIXCONDO       PIC S9(4)    BINARY.               *
*          09  QKEY-AIXCONDL       PIC S9(4)    BINARY.               *
*          09  QKEY-AIXCONDP       PIC  X(2).                         *
*          09  QKEY-AIXCONDV       PIC  X(12).                        *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      There are four different options  for making a request to this *
* routine -- depending upon what the caller needs to know.            *
*                                                                     *
*   1. Return the entry for a particular AIX label.                   *
*   2. Return the first AIX entry for a particular BASE label.        *
*   3. Return the next AIX entry for a particular BASE label.         *
*   4. Return the address of the KWIKKEY AIX table.                   *
*                                                                     *
*      For the first option,  only a single CALL/LINK is required and *
* the caller should pass  the  label  of  the desired alternate index *
* definition  (and  blanks  for  the  base  dataset  label)  as their *
* request.   On the other hand, the next two options are for when the *
* caller wants to know all  KWIKKEY alternate indexes defined for the *
* same  base  dataset.   In  which  case,  multiple  CALLs/LINKs  are *
* required as follows:                                                *
*                                                                     *
*   1. The first request should pass blanks for the alternate index   *
*      label and a specific base dataset label name.                  *
*   2. Subsequent requests should not modify either label that was    *
*      returned by the preceeding request.                            *
*   3. Repeat step 2, above, until the alternate index label comes    *
*      back as high values (indicating no more entries were found).   *
*                                                                     *
*      The fourth option is for  when  the caller needs access to the *
* entire KWIKKEY table for some more advanced purpose.   In this case *
* the caller should  pass  "*ADDRESS"  as  the alternate index label. *
* Then,  this routine will  pass  back  "*AD=xxxx"  where "xxxx" is a *
* 31-bit address pointing to the  beginning of the KWIKKEY AIX table. *
* The caller,  of course,  must be  aware of CICS vs.   batch storage *
* restrictions and handling methods to insure error-free processing.  *
*                                                                     *
*      Lastly, if an invalid request is made (such as both labels are *
* blank),  then a return code of 12 is issued.   If no matching entry *
* is found,  then a return code of  8 is issued (along with returning *
* high values for the alternate index label).                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
PARMS    DSECT
* ------------------------------------------------------------------- *
AIXLBL   DS    CL8
BASELBL  DS    CL8
RECLENG  DS    H
KEYOFFS  DS    H
KEYLENG  DS    H
ALTKEYL  DS    H
AIXKEYS  DS    H
AIXKEY1O DS    H
AIXKEY1L DS    H
AIXKEY2O DS    H
AIXKEY2L DS    H
AIXKEY3O DS    H
AIXKEY3L DS    H
AIXKEY4O DS    H
AIXKEY4L DS    H
AIXKEY5O DS    H
AIXKEY5L DS    H
AIXKEY6O DS    H
AIXKEY6L DS    H
AIXKEY7O DS    H
AIXKEY7L DS    H
AIXKEY8O DS    H
AIXKEY8L DS    H
AIXKEY9O DS    H
AIXKEY9L DS    H
AIXCONDS DS    H
AIXCOND1 DS    CL20
AIXCOND2 DS    CL20
AIXCOND3 DS    CL20
AIXCOND4 DS    CL20
AIXCOND5 DS    CL20
AIXCOND6 DS    CL20
AIXCOND7 DS    CL20
AIXCOND8 DS    CL20
AIXCOND9 DS    CL20
PARMSLEN EQU   *-PARMS
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
R0       EQU   0                       ON ENTRY, ...
R1       EQU   1                       ON ENTRY, PARMS LIST POINTER
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10                      DSECT BASE REG
R11      EQU   11                      CSECT BASE REG
R12      EQU   12                      RESERVED FOR LE/VSE
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* ------------------------------------------------------------------- *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
         COPY  ALTIDX                  AIX TABLE ENTRY DEFINITION
         COPY  AIXCOND                 AIX INCLUDE/OMIT CONDITIONS
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
GETQKEY  HEADER BASE=R11,VERSION='1.0',AUTHOR='DLC',DATE='(C)JUL21'
* ------------------------------------------------------------------- *
         L     R10,0(,R1)              GET ADDRESS OF PARMS BLOCK
         LA    R10,0(,R10)             CLEAR HIGH-ORDER PARM LIST FLAG
         USING PARMS,R10               ESTABLISH PARMS ADDRESSABILITY
*
         IF    AIXLBL,(NH,CLC),BLANKS,AND,  AIX NAME MISSING?          X
               BASELBL,(NH,CLC),BLANKS  AND BASE NAME MISSING?
          LA   R15,12                   INVALID REQUEST
          B    NOGOOD                   GO RETURN TO CALLER
         ENDIF
*
         LA    R9,QKEYTBL              POINT TO KWIKKEY TABLE
         IF    AIXLBL,(EQ,CLC),=CL8'*ADDRESS'  IF SPECIAL REQUEST
          MVC  AIXLBL(4),=CL4'*AD='     FLAG THE SPECIAL REQUEST
          ST   R9,AIXLBL+4              PASS BACK TABLE ADDRESS
          B    RETURN                   GO RETURN TO CALLER
         ENDIF
*
         SELECT
*
         CASE  BASELBL,(NH,CLC),BLANKS GET SINGLE AIX ENTRY
         WHILE AIXLBL,(NE,CLC),0(R9),  WHILE NOT REQUESTED AIX         X
               AND,0(R9),NE,X'FF'      AND NOT END-OF-TABLE
          LA   R9,PARMSLEN(,R9)         POINT TO NEXT ENTRY
         WEND                          LOOP
*
         CASE  AIXLBL,(NH,CLC),BLANKS  GET FIRST AIX ENTRY FOR BASE
         WHILE BASELBL,(NE,CLC),8(R9), WHILE NOT REQUESTED BASE        X
               AND,0(R9),NE,X'FF'      AND NOT END-OF-TABLE
          LA   R9,PARMSLEN(,R9)         POINT TO NEXT ENTRY
         WEND                          LOOP
*
         CASE  OTHER                   GET NEXT AIX ENTRY FOR BASE
         WHILE AIXLBL,(NE,CLC),0(R9),  WHILE NOT AIX LABEL             X
               AND,0(R9),NE,X'FF'      AND NOT END-OF-TABLE
          LA   R9,PARMSLEN(,R9)         POINT TO NEXT ENTRY
         WEND                          LOOP
         IF    0(R9),NE,X'FF'          IF NOT END-OF-TABLE
          LA   R9,PARMSLEN(,R9)         POINT TO NEXT ENTRY
         ENDIF
         WHILE BASELBL,(NE,CLC),8(R9), WHILE NOT BASE LABEL            X
               AND,0(R9),NE,X'FF'      AND NOT END-OF-TABLE
          LA   R9,PARMSLEN(,R9)         POINT TO NEXT ENTRY
         WEND                          LOOP
*
         ENDSL
*
         IF    0(R9),EQ,X'FF'          IF END-OF-TABLE
          MVC  AIXLBL,0(R9)             PASS BACK HIGH-VALUES
          LA   R15,8                    REQUEST NOT FOUND
          B    NOGOOD                   GO RETURN TO CALLER
         ENDIF
*
         MVC   PARMS(PARMSLEN),0(R9)   COPY TABLE ENTRY TO PARMS
* ------------------------------------------------------------------- *
RETURN   XR    R15,R15                 SET ZERO RETURN CODE
NOGOOD   TRAILER RETCODE=R15           PASS BACK RETURN CODE
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
BLANKS   DC    CL8' '
         LTORG
* =================================================================== *
* NOTE: SORTED ORDER NOT NEEDED -- ENTRIES ARE SEARCHED IN SEQUENCE.
*       ALL OF THE FOLLOWING KEY PAIRS ARE IN (OFFSET,LENGTH) ORDER.
*       AN "OFFSET" IS A ZERO-REFERENCE POSITION IN A RECORD LAYOUT.
*
        ALTIDX TYPE=BEGIN              REQUIRED BEGINNING OF TABLE
*
APAUAIX ALTIDX BASELBL=APAUDIT,BASERECL=255,BASEKEY=(0,11),            X
               AIXKEYS=((11,25),(46,2),(0,11))
APCHAI2 ALTIDX BASELBL=APCHEKS,BASERECL=163,BASEKEY=(0,9),             X
               AIXKEYS=((129,5),(0,9))
APCAI2  ALTIDX BASELBL=APCVND,BASERECL=418,BASEKEY=(0,13),             X
               AIXKEYS=((5,8),(0,5))
APNVFEI ALTIDX BASELBL=APNVEND,BASERECL=408,BASEKEY=(12,8),            X
               AIXKEYS=((267,5),(12,8))
APOAIX  ALTIDX BASELBL=APOINV,BASERECL=283,BASEKEY=(0,41),             X
               AIXKEYS=((0,5),(46,5),(5,36))
APOAI2  ALTIDX BASELBL=APOINV,BASERECL=283,BASEKEY=(0,41),             X
               AIXKEYS=((231,12),(0,41))
APPAIX  ALTIDX BASELBL=APPINV,BASERECL=283,BASEKEY=(0,41),             X
               AIXKEYS=((0,5),(188,5),(5,36))
APPAI2  ALTIDX BASELBL=APPINV,BASERECL=283,BASEKEY=(0,41),             X
               AIXKEYS=((231,12),(0,41))
APPAI3  ALTIDX BASELBL=APPINV,BASERECL=283,BASEKEY=(0,41),             X
               AIXKEYS=((254,9),(13,28))
CCHKAI2 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,6),(65,5),(16,14)),       BY CORP/CLEAR    X
               INCLUDE=((15,1,EQ,C'L'))               LOCAL BANK, ONLY
CCHKAI3 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((55,6),(16,14),(10,5)),       BY AMOUNT/DATE   X
               INCLUDE=((15,1,EQ,C'X'))               CONC. ACCT, ONLY
CCHKAI4 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((16,14),(10,5)),              BY DATE/CORP     X
               INCLUDE=((15,1,EQ,C'X'))               CONC. ACCT, ONLY
CCHKAI6 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,5),(16,14)),              BY CORP/DATE     X
               INCLUDE=((15,1,EQ,C'X'),AND,(65,5,EQ,P'0'))   UNCLEARED
CDEPAI2 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,6),(65,5),(16,14)),       BY CORP/CLEAR    X
               INCLUDE=((15,1,EQ,C'L'))               LOCAL BANK, ONLY
CDEPAI3 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((55,6),(16,14),(10,5)),       BY AMOUNT/DATE   X
               INCLUDE=((15,1,EQ,C'X'))               CONC. ACCT, ONLY
CDEPAI4 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((16,14),(10,5)),              BY DATE/CORP     X
               INCLUDE=((15,1,EQ,C'X'))               CONC. ACCT, ONLY
CDEPAI6 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,5),(16,14)),              BY CORP/DATE     X
               INCLUDE=((15,1,EQ,C'X'),AND,(65,5,EQ,P'0'))   UNCLEARED
PRHIST2 ALTIDX BASELBL=PRHIST,BASERECL=68,BASEKEY=(14,30),             X
               AIXKEYS=((24,11),(14,10),(35,9))
PRMAST2 ALTIDX BASELBL=PRMAST,BASERECL=629,BASEKEY=(9,10),             X
               AIXKEYS=((40,25),(19,20),(9,10))       BY LAST NAME
PRMAST3 ALTIDX BASELBL=PRMAST,BASERECL=629,BASEKEY=(9,10),             X
               AIXKEYS=((557,6),(9,10))               BY WINID
PSUPAI1 ALTIDX BASELBL=PSUPUPL,BASERECL=80,BASEKEY=(0,17),             X
               AIXKEYS=((8,4),(12,5),(6,2),(0,6))
PSUPAI2 ALTIDX BASELBL=PSUPUPL,BASERECL=80,BASEKEY=(0,17),             X
               AIXKEYS=((12,5),(8,4),(6,2),(0,6))
PSUPIA1 ALTIDX BASELBL=PSUPINV,BASERECL=120,BASEKEY=(0,29),            X
               AIXKEYS=((8,4),(12,5),(6,2),(0,6),(17,12))
SSOWIDX ALTIDX BASELBL=SSOWNER,BASERECL=227,BASEKEY=(15,11),           X
               AIXKEYS=((218,6),(15,11))              BY WINID
*
        ALTIDX TYPE=END                REQUIRED END OF TABLE
* =================================================================== *
         END
