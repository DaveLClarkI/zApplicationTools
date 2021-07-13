QKEY     TITLE 'GET A KWIKKEY AIX ENTRY FROM TABLE'
         PRINT ON,GEN
GETQKEY  AMODE 31
GETQKEY  RMODE ANY
         SPACE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     * 00000400
*       K W I K K E Y   T A B L E   A C C E S S   R O U T I N E       * 00000500
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
*      This routine will find  and  return  a KWIKKEY alternate index * 00002000
* definition entry from the internal program table.   Since a KWIKKEY * 00002100
* alternate index supports non-contiguous  data to build an alternate * 00002200
* key  (and  "sparse"  indexes),   the   alternate  index  cannot  be * 00002300
* automatically maintained by VSAM  and,  indeed,  cannot have a PATH * 00002400
* entry at all.   Thus,  this  routine  provides  a  means whereby an * 00002500
* application program can  obtain  the  dynamic information needed to * 00002500
* provide its own path between the KWIKKEY alternate index and a VSAM * 00002500
* base cluster.                                                       * 00002500
*                                                                     * 00002500
*      This routine may be used either  online or in batch because it * 00002500
* requires no CICS services and it accepts one or two address entries * 00002500
* in the parms list,  but only uses the last entry passed.   In CICS, * 00002500
* this  would  be  the  COMMAREA   (where  the  first  entry  is  the * 00002500
* auto-magically-passed execute interface block, or EIB).             * 00002500
*                                                                     * 00002500
*      Thus, in batch you can do this:                                * 00002500
*                                                                     * 00002500
* CALL GETQKEY USING GETQKEY-PARMS                                    * 00002500
*                                                                     * 00002500
*      And, in CICS you can do this:                                  * 00002500
*                                                                     * 00002500
* EXEC CICS LINK PROGRAM(GETQKEY) COMMAREA(GETQKEY-PARMS)             * 00002500
*                                                                     * 00002500
*                                                                     * 00002500
*      To code the alernate index table,  at the end of this routine, * 00002500
* you can specify up to  nine non-contiguous data areas from the base * 00002500
* record (to be concatenated,  in  order  from left to right,  as the * 00002500
* alternate  index  key)  and  up  to  nine  either  INCLUDE  or OMIT * 00002500
* conditions  (separated  by  AND/OR  articles).    The  include/omit * 00002500
* conditions are useful when  you  do  not  want to have an alternate * 00002500
* index record for every record  in the base cluster (i.e.,  a sparse * 00002500
* vs. a dense index).   It is up to  the  caller  whether to apply or * 00002500
* ignore  these  conditions  when  accessing  and/or  maintaining the * 00002500
* associated alternate index.                                         * 00002500
*                                                                     * 00002500
*      Alternate index key syntax is as follows:                      * 00002500
*                                                                     * 00002500
* AIXKEYS=((offset,length) < ,(offset,length) < ... > > )             * 00002500
*                                                                     * 00002500
*                                                                     * 00002500
*      Include/omit syntax is as follows:                             * 00002500
*                                                                     * 00002500
* {INCLUDE|OMIT}=(condition < ,{AND|OR},condition < ... > > )         * 00002500
*                                                                     * 00002500
*                                                                     * 00002500
*      The "condition" syntax is as follows:                          * 00002500
*                                                                     * 00002500
* (offset,length,operator,value)                                      * 00002500
*                                                                     * 00002500
*                                                                     * 00002500
* Where: offset    to the data area (zero-based) in the base record;  * 00002500
*        length    is the byte-length of that data area;              * 00002500
*        operator  must be one of {EQ|NE|LT|LE|GT|GE}; and,           * 00002500
*        value     must be a self-defining term (whose byte length    * 00002500
*                  matches the length of the data area) -- such as:   * 00002500
*                    C'aaaaaaaaaa'  (up to 10 characters)             * 00002500
*                    B'11111111'    (up to 8, 16, 24, or 32 bit data) * 00002500
*                    P'±1234567.89' (signed decimal, up to 19 digits) * 00002500
*                    H'±12345'      (signed integer, up to 5 digits)  * 00002500
*                    F'±123456789'  (signed integer, up to 10 digits) * 00002500
*                    D'±1234567890' (signed integer, up to 19 digits).* 00002500
*                                                                     * 00002500
* =================================================================== * 00002600
*                                                                     * 00002700
*                           IMPLEMENTATION                            * 00002800
*                                                                     * 00002900
*      This program should be  compiled  as a standard batch program. * 00003000
* The following is an example  of  a  COBOL copybook that can be used * 00002500
* with this routine.                                                  * 00002500
*                                                                     * 00002500
* ---1----+----2----+----3----+----4----+----5----+----6----+----7--  * 00002500
*  01  GETQKEY-AREA.                                                  * 00002500
*    03  GETQKEY                   PIC  X(8)    VALUE 'GETQKEY'.      * 00002500
*    03  GETQKEY-PARMS.                                               * 00002500
*      05  QKEY-ALTLBL             PIC  X(8)    VALUE SPACES.         * 00002500
*      05  QKEY-SPECREQ            REDEFINES    QKEY-AIXLBL.          * 00002500
*        88  QKEY-TABLREQ                       VALUE '*ADDRESS'.     * 00002500
*        07  QKEY-SPECKEY          PIC  X(4).                         * 00002500
*          88  QKEY-TABLRET                     VALUE '*AD='.         * 00002500
*        07  QKEY-TABLADR                       POINTER.              * 00002500
*      05  QKEY-BASELBL            PIC  X(8)    VALUE SPACES.         * 00002500
*      05  QKEY-ATTRS.                                                * 00002500
*        07  QKEY-BASERECL         PIC S9(4)    BINARY.               * 00002500
*        07  QKEY-BASEKEY.                                            * 00002500
*          09  QKEY-BASEKEYO       PIC S9(4)    BINARY.               * 00002500
*          09  QKEY-BASEKEYL       PIC S9(4)    BINARY.               * 00002500
*        07  QKEY-ALTKEYL          PIC S9(4)    BINARY.               * 00002500
*        07  QKEY-AIXKEYS          PIC S9(4)    BINARY.               * 00002500
*        07  QKEY-AIXKEY                        OCCURS 9 TIMES.       * 00002500
*          09  QKEY-AIXKEYO        PIC S9(4)    BINARY.               * 00002500
*          09  QKEY-AIXKEYL        PIC S9(4)    BINARY.               * 00002500
*      05  QKEY-CONDS.                                                * 00002500
*        07  QKEY-AIXCONDS         PIC S9(4)    BINARY.               * 00002500
*        07  QKEY-AIXCONDE                      OCCURS 9 TIMES.       * 00002500
*          09  QKEY-AIXCONDC       PIC  X(2).                         * 00002500
*          09  QKEY-AIXCONDO       PIC S9(4)    BINARY.               * 00002500
*          09  QKEY-AIXCONDL       PIC S9(4)    BINARY.               * 00002500
*          09  QKEY-AIXCONDP       PIC  X(2).                         * 00002500
*          09  QKEY-AIXCONDV       PIC  X(12).                        * 00002500
*                                                                     * 00002500
* =================================================================== * 00003800
*                                                                     * 00003900
*                             INVOCATION                              * 00004000
*                                                                     * 00004100
*      There are four different options  for making a request to this * 00002500
* routine -- depending upon what the caller needs to know.            * 00002500
*                                                                     * 00004100
*   1. Return the entry for a particular AIX label.                   * 00002500
*   2. Return the first AIX entry for a particular BASE label.        * 00002500
*   3. Return the next AIX entry for a particular BASE label.         * 00002500
*   4. Return the address of the KWIKKEY AIX table.                   * 00002500
*                                                                     * 00002500
*      For the first option,  only a single CALL/LINK is required and * 00002500
* the caller should pass  the  label  of  the desired alternate index * 00002500
* definition  (and  blanks  for  the  base  dataset  label)  as their * 00002500
* request.   On the other hand, the next two options are for when the * 00002500
* caller wants to know all  KWIKKEY alternate indexes defined for the * 00002500
* same  base  dataset.   In  which  case,  multiple  CALLs/LINKs  are * 00002500
* required as follows:                                                * 00002500
*                                                                     * 00002500
*   1. The first request should pass blanks for the alternate index   * 00002500
*      label and a specific base dataset label name.                  * 00002500
*   2. Subsequent requests should not modify either label that was    * 00002500
*      returned by the preceeding request.                            * 00002500
*   3. Repeat step 2, above, until the alternate index label comes    * 00002500
*      back as high values (indicating no more entries were found).   * 00002500
*                                                                     * 00002500
*      The fourth option is for  when  the caller needs access to the * 00002500
* entire KWIKKEY table for some more advanced purpose.   In this case * 00002500
* the caller should pass  "*ADDRESS"  in the first parameter.   Then, * 00002500
* this routine will pass  back  "*AD=xxxx"  where  "xxxx" is a 31-bit * 00002500
* address pointing to the  beginning  of the KWIKKEY AIX table.   The * 00002500
* caller,  of course,  must  be  aware  of  CICS  vs.   batch storage * 00002500
* restrictions and handling methods to insure error-free processing.  * 00002500
*                                                                     * 00002500
*      Lastly, if an invalid request is made (such as both labels are * 00005100
* blank),  then a return code of 12 is issued.   If no matching entry * 00005100
* is found,  then a return code of  8 is issued (along with returning * 00005100
* high values for the alternate index label).                         * 00005100
*                                                                     * 00005100
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
PARMSLST DSECT
* ------------------------------------------------------------------- *
PARM1    DS    F
PARM2    DS    F
* =================================================================== *
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
         PRINT ON
* ------------------------------------------------------------------- *
         MACRO
&AIXLBL  ALTIDX &BASELBL=,&BASERECL=,&BASEKEY=,&AIXKEYS=,              X
               &INCLUDE=,&OMIT=
         GBLA  MAX,RECL                                                 01800000
         LCLA  C,L,N,S                                                  01800000
         LCLC  STR                                                      01800000
&MAX     SETA  9
.*
         AIF   (T'&AIXLBL NE 'O').AIXLBL
         MNOTE 12,'AIX LABEL OMITTED'
         MEXIT
.AIXLBL  ANOP
&AIXLBL  DC    CL8'&AIXLBL'            AIX LABEL
.*
         AIF   (T'&BASELBL NE 'O').BASELBL
         MNOTE 12,'BASE LABEL OMITTED'
         MEXIT
.BASELBL ANOP
         DC    CL8'&BASELBL'           BASE LABEL
.*
         AIF   (T'&BASERECL EQ 'O').ERRRECL
         AIF   (&BASERECL LT 1).ERRRECR
         AIF   (&BASERECL LE 32767).BASERCL
.ERRRECR MNOTE 8,'BASE RECORD LENGTH < 1 OR > 32767'
         MEXIT
.ERRRECL MNOTE 12,'BASE RECORD LENGTH OMITTED'
         MEXIT
.*
.BASERCL ANOP
&RECL    SETA  &BASERECL
         DC    H'&RECL'                BASE RECORD LENGTH
.*
         AIF   (T'&BASEKEY EQ 'O').ERRBKEY
         AIF   (N'&BASEKEY NE 2).ERRBKYN
         AIF   (&BASEKEY(1) LT 0).ERRBKYO
         AIF   (&BASEKEY(1) GE &RECL).ERRBKYO
         AIF   (&BASEKEY(2) LT 1).ERRBKYL
         AIF   (&BASEKEY(2) GT 255).ERRBKYL
         AIF   (&BASEKEY(2) GT &RECL).ERRBKYR
         AIF   (&BASEKEY(1)+&BASEKEY(2) LE &RECL).BASEKEY
         MNOTE 8,'BASE KEY OFFSET+LENGTH > BASE RECORD LENGTH'
         MEXIT
.ERRBKYR MNOTE 8,'BASE KEY LENGTH > BASE RECORD LENGTH'
         MEXIT
.ERRBKYL MNOTE 8,'BASE KEY LENGTH < 1 OR > 255'
         MEXIT
.ERRBKYO MNOTE 8,'BASE KEY OFFSET < 0 OR >= BASE RECORD LENGTH'
         MEXIT
.ERRBKYN MNOTE 8,'BASE KEY NOT AN OFFSET,LENGTH PAIR'
         MEXIT
.ERRBKEY MNOTE 12,'BASE KEY OMITTED'
         MEXIT
.*
.BASEKEY ANOP
&STR     SETC  '              BASE KEY OFFSET/LENGTH'
         DC    H'&BASEKEY(1)',H'&BASEKEY(2)'&STR
.*
         AIF   (T'&AIXKEYS EQ 'O').ERRAKEY
         AIF   (N'&AIXKEYS LT 1).ERRAKYN
         AIF   (N'&AIXKEYS GT &MAX).ERRAKYN
&N       SETA  N'&AIXKEYS
         DC    H'0',H'&N'              NUMBER OF AIX KEYS
&N       SETA  1
&S       SETA  0
.CHKXKLP AIF   (&N GT N'&AIXKEYS).FILLUP
         AIF   (N'&AIXKEYS(&N) NE 2).ERRXKYN
         AIF   (&AIXKEYS(&N,1) LT 0).ERRXKYO
         AIF   (&AIXKEYS(&N,1) GE &RECL).ERRXKYO
         AIF   (&AIXKEYS(&N,2) LT 1).ERRXKYL
         AIF   (&AIXKEYS(&N,2) GT 255).ERRXKYL
         AIF   (&AIXKEYS(&N,2) GT &RECL).ERRXKYR
         AIF   (&AIXKEYS(&N,1)+&AIXKEYS(&N,2) GT &RECL).ERRXKYT
&S       SETA  &S+&AIXKEYS(&N,2)
         AIF   (&S GT 255).ERRXKYS
&STR     SETC  '              AIX KEY &N OFFSET/LENGTH'
         DC    H'&AIXKEYS(&N,1)',H'&AIXKEYS(&N,2)'&STR
&N       SETA  &N+1
         AGO   .CHKXKLP
.ERRXKYS MNOTE 8,'AIX KEYS TOTAL LENGTH > 255'
         MEXIT
.ERRXKYT MNOTE 8,'AIX KEY(&N) OFFSET+LENGTH > BASE RECORD LENGTH'
         MEXIT
.ERRXKYR MNOTE 8,'AIX KEY(&N) LENGTH > BASE RECORD LENGTH'
         MEXIT
.ERRXKYL MNOTE 8,'AIX KEY(&N) LENGTH < 1 OR > 255'
         MEXIT
.ERRXKYO MNOTE 8,'AIX KEY(&N) OFFSET < 0 OR >= BASE RECORD LENGTH'
         MEXIT
.ERRXKYN MNOTE 8,'AIX KEY(&N) NOT AN OFFSET,LENGTH PAIR'
         MEXIT
.ERRAKYN MNOTE 8,'AIX KEYS < 1 OR > &MAX'
         MEXIT
.ERRAKEY MNOTE 12,'AIX KEYS OMITTED'
         MEXIT
.*
.FILLUP  ANOP
         AIF   (&N GT &MAX).NEXT
&STR     SETC  '               AIX KEY &N OFFSET/LENGTH'
         DC    H'0',H'0'&STR
&N       SETA  &N+1
         AGO   .FILLUP
.NEXT    ANOP
         ORG   *-40
         DC    H'&S'                   AIX TOTAL KEY LENGTH
         ORG
.*
         AIF   (T'&INCLUDE NE 'O' AND T'&OMIT NE 'O').EXCLUDE
         AIF   (T'&INCLUDE NE 'O').INCLUDE
         AIF   (T'&OMIT NE 'O').OMIT
&STR     SETC  ''
         AIXCOND &STR
         AGO   .DONE
.EXCLSTS MNOTE 12,'INCLUDE/OMIT FIRST ELEMENT MUST BE A SUBLIST'
         MEXIT
.EXCLIST MNOTE 12,'INCLUDE/OMIT MUST BE SUBLISTED'
         MEXIT
.EXCLUDE MNOTE 12,'INCLUDE AND OMIT ARE MUTUALLY EXCLUSIVE PARAMETERS'
         MEXIT
.INCLUDE ANOP
         AIF   (N'&INCLUDE EQ 0).EXCLIST
         AIF   ('&INCLUDE'(1,1) NE '(').EXCLIST
         AIF   ('&INCLUDE(1)'(1,1) NE '(').EXCLSTS
         AIXCOND IN,&INCLUDE
         AGO   .DONE
.OMIT    ANOP
         AIF   (N'&OMIT EQ 0).EXCLIST
         AIF   ('&OMIT'(1,1) NE '(').EXCLIST
         AIF   ('&OMIT(1)'(1,1) NE '(').EXCLSTS
         AIXCOND OM,&OMIT
         AGO   .DONE
.*
.DONE    ANOP
         MEND
* ------------------------------------------------------------------- *
         MACRO
         AIXCOND &TYPE,&COND
         GBLA  MAX,RECL                                                 01800000
         LCLA  C,L,N,O,S                                                01800000
         LCLC  A,P,STR,T,V                                              01800000
&N       SETA  N'&COND
&C       SETA  (&N+1)/2
         AIF   (&C LE &MAX).READY
         MNOTE 8,'INCLUDE/OMIT CONDITIONS > &MAX'
         MEXIT
.READY   ANOP
         DC    H'&C'                   NUMBER OF CONDITIONS
.*
&C       SETA  0                       CONDITION NUMBER
&S       SETA  0                       SUBSCRIPT NUMBER
         AIF   ('&TYPE' EQ '').FILLUP
&A       SETC  '&TYPE'(1,2)
.CHKXKLP ANOP
&S       SETA  &S+1
         AIF   (&S GT &N).FILLUP
&C       SETA  &C+1
         AIF   (N'&COND(&S) NE 4).CONDKYN
         AIF   (&COND(&S,1) LT 0).CONDKYO
         AIF   (&COND(&S,1) GE &RECL).CONDKYO
         AIF   (&COND(&S,2) LT 1).CONDKYL
         AIF   (&COND(&S,2) GT 8).CONDKYL
         AIF   (&COND(&S,2) GT &RECL).CONDKYR
         AIF   (&COND(&S,1)+&COND(&S,2) GT &RECL).CONDKYT
&STR     SETC  '  C&C'
&O       SETA  &COND(&S,1)
&L       SETA  &COND(&S,2)
&P       SETC  '&COND(&S,3)'(1,2)
&T       SETC  '&COND(&S,4)'(1,1)
&V       SETC  '&COND(&S,4)'(2,*)
         AIF  ('&P' NE 'EQ' AND '&P' NE 'NE' AND                       X
               '&P' NE 'LT' AND '&P' NE 'LE' AND                       X
               '&P' NE 'GT' AND '&P' NE 'GE').CONDKYP
         AIF  ('&T' NE 'C' AND '&T' NE 'P' AND                         X
               '&T' NE 'B' AND '&T' NE 'H' AND                         X
               '&T' NE 'F' AND '&T' NE 'D').CONDKYX
         AIF   ('&T' EQ 'C').SETXKYC
         AIF   ('&T' EQ 'P').SETXKYP
         AIF   ('&T' EQ 'B').SETXKYB
         AIF   ('&T' EQ 'H').SETXKYH
         AIF   ('&T' EQ 'F').SETXKYF
         DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',DL8&V,XL2'0'
         AGO   .CHKNEXT
.SETXKYF DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',FL4&V,XL6'0'
         AGO   .CHKNEXT
.SETXKYH DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',HL2&V,XL8'0'
         AGO   .CHKNEXT
.SETXKYB AIF   (&L GT 3).SETXK4B
         AIF   (&L GT 2).SETXK3B
         AIF   (&L GT 1).SETXK2B
         DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',BL1&V,XL9'0'
         AGO   .CHKNEXT
.SETXK2B DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',BL2&V,XL8'0'
         AGO   .CHKNEXT
.SETXK3B DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',BL3&V,XL7'0'
         AGO   .CHKNEXT
.SETXK4B DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',BL4&V,XL6'0'
         AGO   .CHKNEXT
.SETXKYP AIF   (&L GT 9).SETXK0P
         AIF   (&L GT 8).SETXK9P
         AIF   (&L GT 7).SETXK8P
         AIF   (&L GT 6).SETXK7P
         AIF   (&L GT 5).SETXK6P
         AIF   (&L GT 4).SETXK5P
         AIF   (&L GT 3).SETXK4P
         AIF   (&L GT 2).SETXK3P
         AIF   (&L GT 1).SETXK2P
         DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL1&V,XL9'0'
         AGO   .CHKNEXT
.SETXK2P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL2&V,XL8'0'
         AGO   .CHKNEXT
.SETXK3P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL3&V,XL7'0'
         AGO   .CHKNEXT
.SETXK4P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL4&V,XL6'0'
         AGO   .CHKNEXT
.SETXK5P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL5&V,XL5'0'
         AGO   .CHKNEXT
.SETXK6P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL6&V,XL4'0'
         AGO   .CHKNEXT
.SETXK7P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL7&V,XL3'0'
         AGO   .CHKNEXT
.SETXK8P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL8&V,XL2'0'
         AGO   .CHKNEXT
.SETXK9P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL9&V,XL1'0'
         AGO   .CHKNEXT
.SETXK0P DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',PL10&V
         AGO   .CHKNEXT
.SETXKYC DC    C'&A',H'&O',H'&L',C'&P',CL2'&T:',CL10&V
         AGO   .CHKNEXT
.CHKNEXT ANOP
&S       SETA  &S+1
         AIF   (&S GT &N).FILLUP
&A       SETC  '&COND(&S)'
         AIF   ('&A' NE 'AND' AND '&A' NE 'OR').CONDART
&A       SETC  '&A'(1,2)
         AGO   .CHKXKLP
.CONDART MNOTE 8,'CONDITION(&C) EXPECTED ''AND'' OR ''OR'' (&A)'
         MEXIT
.CONDKYX MNOTE 8,'CONDITION(&C) UNKNOWN SELF-DEFINING TERM (&T)'
         MEXIT
.CONDKYP MNOTE 8,'CONDITION(&C) UNKNOWN OPERATOR (&P)'
         MEXIT
.CONDKYT MNOTE 8,'CONDITION(&C) OFFSET+LENGTH > BASE RECORD LENGTH'
         MEXIT
.CONDKYR MNOTE 8,'CONDITION(&C) LENGTH > BASE RECORD LENGTH'
         MEXIT
.CONDKYL MNOTE 8,'CONDITION(&C) LENGTH < 1 OR > 8'
         MEXIT
.CONDKYO MNOTE 8,'CONDITION(&C) OFFSET < 0 OR >= BASE RECORD LENGTH'
         MEXIT
.CONDKYN MNOTE 12,'CONDITION(&C) REQUIRES 4 ELEMENTS'
         MEXIT
.FILLUP  ANOP
         AIF   (&C GE &MAX).DONE
&C       SETA  &C+1
&STR     SETC  '     C&C'
         DC    C'  ',H'0',H'0',C'  ',CL12' '&STR
         AGO   .FILLUP
.DONE    ANOP
         MEND
* =================================================================== *
         EJECT
* =================================================================== *
GETQKEY  HEADER BASE=R11,VERSION='1.0',AUTHOR='DLC',DATE='(C)JUL21'
* ------------------------------------------------------------------- *
         USING PARMSLST,R1             SET ADDRESSABILITY TO PARMS LIST
         IF    PARM1,(ON,TM),X'80'     IF 1ST ENTRY IS LAST ENTRY
          L    R10,PARM1                USE 1ST ENTRY FOR PARMS
         ELSE                          ELSE
          L    R10,PARM2                USE 2ND ENTRY FOR PARMS
         ENDIF                         ENDIF
         DROP  R1                      REMOVE PARMS LIST ADDRESSABILITY
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
         WHILE BASELBL,(NE,CLC),0(R9), WHILE NOT REQUESTED BASE        X
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
         WHILE BASELBL,(NE,CLC),0(R9), WHILE NOT BASE LABEL            X
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
QKEYTBL  DS    0D
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
               AIXKEYS=((10,6),(65,5),(16,14)),                        X
               INCLUDE=((15,1,EQ,C'L'))
CCHKAI3 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((55,6),(16,14),(10,5)),                        X
               INCLUDE=((15,1,EQ,C'X'))
CCHKAI4 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((16,14),(10,5)),                               X
               INCLUDE=((15,1,EQ,C'X'))
CCHKAI6 ALTIDX BASELBL=CCHECK,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,5),(16,14)),                               X
               INCLUDE=((15,1,EQ,C'X'),AND,(65,5,EQ,P'0'))
CDEPAI2 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,6),(65,5),(16,14)),                        X
               INCLUDE=((15,1,EQ,C'L'))
CDEPAI3 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((55,6),(16,14),(10,5)),                        X
               INCLUDE=((15,1,EQ,C'X'))
CDEPAI4 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((16,14),(10,5)),                               X
               INCLUDE=((15,1,EQ,C'X'))
CDEPAI6 ALTIDX BASELBL=CDEPOS,BASERECL=120,BASEKEY=(10,20),            X
               AIXKEYS=((10,5),(16,14)),                               X
               INCLUDE=((15,1,EQ,C'X'),AND,(65,5,EQ,P'0'))
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
         DC    X'FFFFFFFFFFFFFFFF'     MARK END OF TABLE
* =================================================================== *
         END
