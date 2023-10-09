DMP      TITLE 'RXDMPVAR - REXX SUB TO DUMP ALL VARS OR RETURN LIST'
         PRINT ON,GEN
RXDMPVAR AMODE 31
RXDMPVAR RMODE ANY
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
*      This external  REXX  function  is  intended  for  use in those *
* situations where all of the needed potential variable names are not *
* known.   Thus,  this routine can either dump all variable names and *
* their contents (up to 250 bytes worth,  that is) or it can return a *
* list of all or selected variable names back to the caller.          *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This program is to be compiled as a standard batch phase.      *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX function  is  as easy as you would *
* invoke any other REXX  function.   The  following are the different *
* possible invocation formats (and some example code):                *
*                                                                     *
*     resp = RXDMPVAR()           /* 'Dump' is the default */         *
*     resp = RXDMPVAR('Dump')          /* Dumps via ARXSAY */         *
*     resp = RXDMPVAR('Dump','stemname.')  /* Dump to stem */         *
*     If resp ¬= 'OK' Then Do                                         *
*       Say ' Response = 'resp                                        *
*       Exit 4                                                        *
*     End                                                             *
*     Do n = 0 To stemname.0                                          *
*       Say ' stemname.'n' = 'stemname.n                              *
*     End                                                             *
*                                                                     *
*     str = RXDMPVAR('List')    /* list all variable names */         *
*     str = RXDMPVAR('List','prefix')    /* list by prefix */         *
*     Parse Var str resp list                                         *
*     If resp ¬= 'OK' Then Do                                         *
*       Say ' Response = 'str                                         *
*       Exit 4                                                        *
*     End                                                             *
*     list = SHAKESRT(list)    /* do a Shaker-Sort of list */         *
*     Do n = 1 To Words(list)                                         *
*       Say ' 'Word(list,n)' = 'Value(Word(list,n))                   *
*     End                                                             *
*                                                                     *
* Note that,  for the 'Dump'  argument,  the  optional stem name must *
* include the period and have a  maximum length of nine characters -- *
* including the period.  For the 'List' argument, the optional prefix *
* represents a generic selection  of  a group of variable names.   No *
* wild-card characters are supported.   When  selection is for a stem *
* variable name prefix,  be sure to include at least the first period *
* in order to insure selection of stem variable names only.           *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
*      The dumped variable  output  will  always  be in the following *
* format -- whether internally dumped  via  the REXX SAY statement or *
* returned in  a  stem  variable  (note  that  string  values are not *
* wrapped in quotes and  only  the  first  250  bytes of any variable *
* value is included):                                                 *
*                                                                     *
*     varname_1 = varvalue_1                                          *
*     varname_2 = varvalue_2                                          *
*     varname_3 = varvalue_3                                          *
*       ...                                                           *
*     varname_n = varvalue_n                                          *
*                                                                     *
*      The listed variable  output  is  returned as a space-delimited *
* list.   In the case of errors, only error diagnostics are returned. *
* If no errors,  the first word in the returned list will be 'OK' and *
* should not be  included  in  your  subsequent  list processing (see *
* Invocation,  above,  for an example).   Otherwise, the following is *
* the returned format in the case of no errors:                       *
*                                                                     *
*     "response varname_1 varname_2 varname_3 ... varname_n"          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PCOUNT   DS    PL8                     PACKED DOUBLE-WORD
EWORK    DS    CL7                     EDITED DOUBLE-WORD
         DS    C                       SLACK BYTE
*
SAYPARMB DS    3F                      ARXSAY PARAMETER BLOCK
SAYPARM1 DS    CL8                     'WRITE   '
SAYPARM2 DS    XL4                     LENGTH OF SAY OUTPUT
SAYPARM3 DS    XL4                     ADDRESS OF SAY OUTPUT
*
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
STEMLENG DS    F                       STEM NAME LENGTH
STEMNAME DS    CL9                     STEM NAME
OPT      DS    CL1                     OUTPUT OPTION
OPTDUMP  EQU   C'D'
OPTSTEM  EQU   C'S'
OPTLIST  EQU   C'L'
*
VARNAME  DS    CL250                   VARIABLE NAME
VARDATA  DS    CL250                   VARIABLE NAME VALUE
SAYDATA  DS    CL510                   SAY OUTPUT VALUE
*
VARSTOR  DS    CL64336                 STORAGE FOR STEM VAR. PROCESSING
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
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
R0       EQU   0                       REXX ENV BLOCK
R1       EQU   1                       REXX EFPL REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12                      RXDMPVAR CSECT BASE REG
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
* =================================================================== *
         EJECT
* =================================================================== *
RXDMPVAR HEADER VERSION='1.3',AUTHOR='DLC',DATE='(C)SEP03',            X
               BASE=R12,SVAREA=SALEN
* =================================================================== *
         USING DYNASTOR,R13
*
         XR    R5,R5
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RXDMPVAR: REQUIRES THE REXX ENVIRONMENT BLOCK'
          CANCEL ALL
         ENDIF
         XR    R6,R6
         L     R6,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R6
         DROP  R5
*
         XR    R7,R7
         LR    R7,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R7
         XR    R8,R8
         L     R8,EFPLEVAL             REXX EVALUATION BLOCK POINTER
         L     R8,0(,R8)               REXX EVALUATION BLOCK
         USING EVALBLOCK,R8
         XR    R9,R9
         L     R9,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R9
         XR    R10,R10
         LR    R10,R9                  REXX ARGUMENT STRING
         USING ARGSTRING,R10
         XR    R11,R11
         LA    R11,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R11              SHARED VARIABLES CONTROL BLOCK
*
         XR    R1,R1                   WORK/PARM REGISTER
         XR    R2,R2                   WORK REGISTER
         XR    R3,R3                   WORK REGISTER
         XR    R4,R4                   WORK REGISTER
         MVI   SAYDATA,X'40'           INIT FIRST BYTE OF SAY BUFFER
         MVI   EVALBLOCK_EVDATA,X'00'  INIT FIRST BYTE OF RESULTS BUF
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    OPT,OPT
         XC    STEMLENG,STEMLENG
         WHILE ARGTABLE_END,(NE,CLC),=XL8'FFFFFFFFFFFFFFFF'

         L     R2,ARGTABLE_ARGSTRING_LENGTH
         L     R4,ARGTABLE_ARGSTRING_PTR
         IF    OPT,EQ,X'00'
          IF   R2,(Z,LTR),R2
           MVI OPT,C'D'                DEFAULT
          ELSE
           MVC OPT,0(R4)               GET FIRST BYTE OF CALLER ARG
           TR  OPT,TRTBL                 AND TRANSLATE TO UPPER-CASE
          ENDIF
          IF   OPT,NE,OPTDUMP,AND,OPT,NE,OPTLIST
           MVC EVALBLOCK_EVDATA(23),=CL23'Invalid first argument.'
           LA  R2,23                   SET LENGTH OF RESULT MESSAGE
           B   FINISH                  GO RETURN MESSAGE
          ENDIF
         ELSE
          IF   STEMLENG,(NE,CLC),=F'0'
           MVC EVALBLOCK_EVDATA(22),=CL22'Invalid last argument.'
           LA  R2,22                   SET LENGTH OF RESULT MESSAGE
           B   FINISH                  GO RETURN MESSAGE
          ENDIF
          ST   R2,STEMLENG             SAVE STEM NAME LENGTH
          IF   OPT,EQ,OPTDUMP
           MVI OPT,C'S'
           MVC STEMNAME,0(R4)          SAVE STEM NAME
           LA  R3,STEMNAME-1           POINT TO LAST CHARACTER
           AR  R3,R2                     OF STEM NAME VALUE
           IF  STEMLENG,(H,CLC),=F'9',OR,0(R3),NE,C'.'
            MVC EVALBLOCK_EVDATA(22),=CL22'Invalid stem argument.'
            LA R2,22                   SET LENGTH OF RESULT MESSAGE
            B  FINISH                  GO RETURN MESSAGE
MVCSELN  MVC   SAYDATA(0),0(R4)        (SEE "EX" INSTRUCTION BELOW)
TRSELN   TR    SAYDATA(0),TRTBL        (SEE "EX" INSTRUCTION BELOW)
           ENDIF
          ELSE
           BCTR R2,0                   DECR. LENGTH FOR ZERO REF.
           EX  R2,MVCSELN              SAVE SELECTION NAME AND
           EX  R2,TRSELN                 TRANSLATE TO UPPER-CASE
          ENDIF
         ENDIF

         LA    R9,L'ARGTABLE_END(,R9)  ADVANCE TO NEXT ARGUMENT
         LR    R10,R9
         WEND

         IF    OPT,EQ,X'00'
          MVI  OPT,C'D'                DEFAULT
         ENDIF
*
* OUTPUT TO SYSLST -- ARXSAY
*
         IF    OPT,EQ,OPTDUMP
          LA   R1,SAYPARM1             GET ADDRESS OF PARM 1
          LA   R2,SAYPARM2             GET ADDRESS OF PARM 2
          LA   R3,SAYPARM3             GET ADDRESS OF PARM 3
          STM  R1,R3,SAYPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
          OI   SAYPARMB+(4*3-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
          MVC  SAYPARM1,=C'WRITE   '   SET PARM 1
          LA   R2,SAYDATA              GET ADDRESS FOR PARM 2
          LA   R3,L'SAYDATA            GET LENGTH FOR PARM 3
          STM  R2,R3,SAYPARM2          PUT IN PARMS 2 AND 3
         ENDIF
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R1,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R3,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R4,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R1,R4,SHVPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
         OI    SHVPARMB+(4*4-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         XC    VARNAME,VARNAME         CLEAR BUFFER FOR VAR. NAME
         XC    VARDATA,VARDATA         CLEAR BUFFER FOR VAR. VALUE
         MVI   SHVCODE,SHVNEXTV        SET REQUEST CODE TO 'N'
         LA    R1,VARNAME              GET ADDRESS FOR SHVNAMA
         LA    R2,L'VARNAME            GET LENGTH  FOR SHVNAML
         LA    R3,VARDATA              GET ADDRESS FOR SHVVALA
         LA    R4,L'VARDATA            GET LENGTH  FOR SHVVALL
         STM   R1,R4,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R2,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R4,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
         IF    OPT,EQ,OPTLIST
          MVC  VARSTOR(4),=C'OK '      PRESUME EVERYTHING WILL BE OK
          LA   R4,VARSTOR+3            PREPARE TO STORE VARIABLE NAMES
         ELSE
          LA   R4,VARSTOR              PREPARE TO STORE VARIABLE INFO
         ENDIF
*
* RETRIEVE AND PROCESS VARIABLES
*
         WHILE SHVRET,NE,SHVLVAR       LOOP UNTIL "LAST VAR" CONDITION

         LA    R1,SHVPARMB             GET ADDRESS OF SHV PARM BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,SHVRET,NE,SHVCLEAN
          IF   SHVRET,EQ,SHVLVAR       IF LAST VAR ALREADY RETURNED...
           WEXIT                         EXIT LOOP
          ENDIF
          IF   SHVRET,EQ,SHVTRUNC      IF TRUNCATION PROBLEM...
           MVC SHVVALL,SHVBUFL         SET VALUE LENGTH TO BUFFER LENG
          ELSE
           MVC EVALBLOCK_EVDATA(16),=CL16'ARXEXCOM failed.'
           LA  R2,16                   SET LENGTH OF RESULT MESSAGE
           IF  R15,(NZ,LTR),R15,BC,R15MSG  GO RETURN MESSAGE
           ENDIF
           B   RETMSG                  GO RETURN MESSAGE
MVCNAME  MVC   0(0,R3),VARNAME         (SEE "EX" INSTRUCTION BELOW)
MVCVALU  MVC   0(0,R3),VARDATA         (SEE "EX" INSTRUCTION BELOW)
          ENDIF
         ENDIF

         IF    OPT,NE,OPTLIST          FOR OPT=DUMP AND OPT=STEM...
          LA   R3,SAYDATA+1            START AT 2ND POSITION IN BUFFER
          L    R2,SHVNAML              GET LENGTH OF VARIABLE NAME
          BCTR R2,0                    DECR. LENGTH FOR ZERO REF.
          EX   R2,MVCNAME              MOVE VARIABLE NAME TO BUFFER
          AR   R3,R2                   ADD VARIABLE NAME LENGTH
          LA   R3,1(,R3)                 PLUS ONE TO BUFFER POSITION
          MVC  0(3,R3),=C' = '         MOVE LITERAL TO BUFFER
          LA   R3,3(,R3)               ADD LITERAL LENGTH TO BUF POS
          L    R2,SHVVALL              GET LENGTH OF VARIABLE VALUE
          BCTR R2,0                    DECR. LENGTH FOR ZERO REF.
          EX   R2,MVCVALU              MOVE VARIABLE VALUE TO BUFFER
          AR   R3,R2                   ADD VARIABLE VALUE LENGTH
          LA   R3,1(,R3)                 PLUS ONE TO BUFFER POSITION
          LA   R2,SAYDATA              COMPUTE LENGTH OF
          SR   R3,R2                     BUFFER VALUE CREATED
         ENDIF

         SELECT

         CASE  OPT,EQ,OPTSTEM          FOR OPT=STEM...
          BCTR R3,0                    DECR. LENGTH FOR FIRST BLANK
          LR   R1,R4                   STARTING AT CURRENT STORAGE POS
          LA   R1,2(,R1)                 IS THERE ROOM FOR LENGTH FIELD
          AR   R1,R3                     AND FOR THIS DATA
          LA   R2,VARSTOR                BETWEEN THE BEGINNING OF
          A    R2,=A(L'VARSTOR)            AND THE END OF STORAGE?
          IF   R1,(NL,CR),R2           IF NOT...
           MVC EVALBLOCK_EVDATA(46),=CL46'Internal storage exhausted --X
                use List option.'
           LA  R2,46                   SET LENGTH OF RESULT MESSAGE
           B   FINISH                  GO RETURN MESSAGE
          ENDIF
          LA   R2,SAYDATA+1            POINT TO THE DATA TO COPY FROM
          STH  R3,0(,R4)               PUT LENGTH IN STORAGE AND
          LA   R4,2(,R4)                 ADD TWO FOR LENGTH FIELD
          LR   R5,R3                   COPY LENGTH TO REG 5
          MVCL R4,R2                   COPY STRING DATA TO STORAGE

         CASE  OPT,EQ,OPTLIST          FOR OPT=LIST...
          L    R3,STEMLENG             GET LENGTH OF SELECTION NAME
          IF   R3,(NZ,LTR),R3          IF NOT ZERO...
           BCTR R3,0                   DECR LENGTH FOR ZERO REFERENCE
           IF  R3,(NE,EX),CLCPREFX     IF VARIABLE PREFIXES DON'T MATCH
            WLOOP                        LOOP
CLCPREFX CLC   SAYDATA(0),VARNAME      (SEE "IF/EX" INSTRUCTION ABOVE)
MVCSTOR  MVC   0(0,R4),VARNAME         (SEE "EX" INSTRUCTION BELOW)
           ENDIF
          ENDIF
          L    R3,SHVNAML              GET LENGTH OF SHV NAME
          BCTR R3,0                    DECR. LENGTH FOR ZERO REF.
          EX   R3,MVCSTOR              MOVE BUFFER TO STORAGE AND
          AR   R4,R3                     ADD THE BUFFER LENGTH
          MVI  1(R4),C' '              ADD LIST DELIMITER
          LA   R4,2(,R4)               PLUS TWO TO STORAGE POSITION

         CASE  ELSE                    FOR OPT=DUMP...
          ST   R3,SAYPARM3             PUT BUFFER LENGTH IN SAY PARM 3
          LA   R1,SAYPARMB             GET ADDRESS OF SAY PARM BLOCK
          L    R15,ARXSAY              POINT TO PGM TO CALL
          BASR R14,R15                 CALL ARXSAY
          IF   R15,(NZ,LTR),R15        IF CALL ERROR...
           MVC EVALBLOCK_EVDATA(14),=CL14'ARXSAY failed.'
           LA  R2,14                   SET LENGTH OF RESULT MESSAGE
           B   R15MSG                  GO RETURN MESSAGE
          ENDIF

         ENDSL
         WEND
*
* IF OPT=STEM, CREATE STEM VARIABLES
*
         IF    OPT,EQ,OPTSTEM          FOR OPT=STEM...
          MVC  0(2,R4),=X'EFFE'        FLAG THE END OF STORAGE
          MVI  SHVCODE,SHVSYSET        SET REQUEST CODE TO 's'
          MVC  VARNAME(9),STEMNAME     PUT STEM NAME IN VAR NAME AREA
          ZAP  PCOUNT,=P'0'            INITIALIZE VARIABLE COUNTER
          LA   R3,VARSTOR              PREPARE TO RETRIEVE VARB INFO
* set the STEM.n variables
          WHILE 0(2,R3),(NE,CLC),=X'EFFE'
           LA  R4,VARNAME              POINT TO FIRST POSITION
           A   R4,STEMLENG               AFTER STEM NAME AND PERIOD
           AP  PCOUNT,=P'1'            INCREMENT VARIABLE COUNTER
           MVC EWORK,=X'40202020202160'
           EDMK EWORK,PCOUNT+5         EDIT THE VARIABLE COUNTER
           LA  R2,EWORK+6              COMPUTE LENGTH OF
           SR  R2,R1                     REMAINING SIGNIFICANT DIGITS
           BCTR R2,0                   DECR. LENGTH FOR ZERO REF.
           EX  R2,MVCSTEM              MOVE TO STEM SUFFIX
           A   R2,STEMLENG             ADD LENGTH OF STEM NAME
           LA  R2,1(,R2)                 PLUS ONE 'CUZ OF ZERO REF.
           LH  R4,0(,R3)               GET LENGTH OF VARIABLE DATA
           LA  R3,2(,R3)               INCREMENT THE STORAGE POSITION
           STM R2,R4,SHVNAML           PUT IN SHV CONTROL BLOCK
           LA  R1,SHVPARMB             GET ADDRESS OF SHV PARM BLOCK
           L   R15,ARXEXCOM            POINT TO PGM TO CALL
           BASR R14,R15                CALL ARXEXCOM
           IF  R15,(NZ,LTR),R15,OR,SHVRET,NE,SHVCLEAN
            IF R15,(NZ,LTR),R15        IF CALL ERROR...
             MVC EVALBLOCK_EVDATA(16),=CL16'ARXEXCOM failed.'
             LA R2,16                  SET LENGTH OF RESULT MESSAGE
             B R15MSG                  GO RETURN MESSAGE
            ENDIF
            IF SHVRET,NE,SHVNEWV       IF NOT NEW VARIABLE...
             MVC EVALBLOCK_EVDATA(16),=CL16'ARXEXCOM failed.'
             LA R2,16                  SET LENGTH OF RESULT MESSAGE
             B RETMSG                  GO RETURN MESSAGE
MVCSTEM  MVC   0(0,R4),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
            ENDIF
           ENDIF
           AR  R3,R4                   ADD VARIABLE DATA LENGTH
          WEND
* set the STEM.0 variable
          IF   EVALBLOCK_EVDATA,EQ,X'00'
           L   R2,STEMLENG             GET LENGTH OF STEM NAME
           LA  R2,1(,R2)                 PLUS ONE FOR ZERO STEM SUFFIX
           LA  R3,VARNAME              POINT TO VAR NAME POSITION
           A   R3,STEMLENG               AFTER STEM NAME AND PERIOD
           MVI 0(R3),C'0'              SET ZERO STEM SUFFIX
           LA  R3,VARDATA              POINT TO VAR DATA
           MVC EWORK,=X'40202020202160'
           EDMK EWORK,PCOUNT+5         EDIT THE VARIABLE COUNTER
           LA  R4,EWORK+6              COMPUTE LENGTH OF
           SR  R4,R1                     REMAINING SIGNIFICANT DIGITS
           BCTR R4,0                   DECR. LENGTH FOR ZERO REF.
           EX  R4,MVCDATA              MOVE TO VAR DATA
           LA  R4,1(,R4)               ADD BACK THE ONE SUBTRACTED
           STM R2,R4,SHVNAML           PUT IN SHV CONTROL BLOCK
           LA  R1,SHVPARMB             GET ADDRESS OF SHV PARM BLOCK
           L   R15,ARXEXCOM            POINT TO PGM TO CALL
           BASR R14,R15                CALL ARXEXCOM
           IF  R15,(NZ,LTR),R15,OR,SHVRET,NE,SHVCLEAN
            IF R15,(NZ,LTR),R15        IF CALL ERROR...
             MVC EVALBLOCK_EVDATA(16),=CL16'ARXEXCOM failed.'
             LA R2,16                  SET LENGTH OF RESULT MESSAGE
             B R15MSG                  GO RETURN MESSAGE
            ENDIF
            IF SHVRET,NE,SHVNEWV       IF NOT NEW VARIABLE...
             MVC EVALBLOCK_EVDATA(16),=CL16'ARXEXCOM failed.'
             LA R2,16                  SET LENGTH OF RESULT MESSAGE
             B RETMSG                  GO RETURN MESSAGE
MVCDATA  MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
            ENDIF
           ENDIF
          ENDIF
         ENDIF
*
* IF OPT=LIST, MOVE STORED VARIABLE NAMES TO EVALUATION BLOCK
*
         IF    OPT,EQ,OPTLIST          FOR OPT=LIST...
          LA   R3,VARSTOR              COMPUTE THE LENGTH OF
          SR   R4,R3                     THE LIST TO BE RETURNED
          L    R3,EVALBLOCK_EVSIZE     GET DBLWRD SIZE OF EVAL BLOCK
          SLL  R3,3                    MULTIPLY BY 8 (3 ORDERS OF 2)
          S    R3,=F'16'               SUBTRACT EVAL HEADER LENGTH
          IF   R3,(NH,CR),R4           IF EVAL DATA AREA TOO SMALL
*                    USE THE ARXSAY PARM BLOCK AS THE ARXRLT PARM BLOCK
           LA  R1,SAYPARM1             GET ADDRESS OF PARM 1
           LA  R2,SAYPARM2             GET ADDRESS OF PARM 2
           LA  R3,SAYPARM3             GET ADDRESS OF PARM 3
           STM R1,R3,SAYPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
           OI  SAYPARMB+(4*3-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
           MVC SAYPARM1,=C'GETBLOCK'   GET LARGER EVALUATION BLOCK
           XC  SAYPARM2,SAYPARM2       CLEAR PARM 2 TO RECEIVE ADDRESS
           ST  R4,SAYPARM3             STORE LENGTH IN PARM 3
           LA  R1,SAYPARMB             GET ADDRESS OF RLT PARM BLOCK
           L   R15,ARXRLT              POINT TO PGM TO CALL
           BASR R14,R15                CALL ARXRLT
           IF  R15,(NZ,LTR),R15        IF CALL ERROR...
            MVC EVALBLOCK_EVDATA(14),=CL14'ARXRLT failed.'
            LA R2,14                   SET LENGTH OF RESULT MESSAGE
            B  R15MSG                  GO RETURN MESSAGE
           ENDIF
           L   R8,EFPLEVAL             EVALUATION BLOCK POINTER
           MVC 0(4,R8),SAYPARM2        SET ADDRESS OF NEW EVAL BLOCK
           L   R8,SAYPARM2             EVALUATION BLOCK
          ENDIF
          ST   R4,EVALBLOCK_EVLEN      STORE LENGTH OF RESULT MESSAGE
          LA   R2,EVALBLOCK_EVDATA     GET ADDRESS OF EVAL DATA AREA
          LR   R3,R4                   COPY RESULT MESSAGE LENGTH
          LA   R4,VARSTOR              GET ADDRESS OF STORED LIST
          LR   R5,R3                   COPY RESULT MESSAGE LENGTH
          MVCL R2,R4                   MOVE STORED LIST TO EVAL DATA
          B    RETURN                  GO RETURN RESULT TO CALLER
         ENDIF
*
* RETURN RESULT
*
         IF    EVALBLOCK_EVDATA,EQ,X'00'  IF NO ERRORS...
          MVC  EVALBLOCK_EVDATA(2),=C'OK'  SET DEFAULT MESSAGE
          LA   R2,2                    SET LENGTH OF RESULT MESSAGE
         ENDIF
         B     FINISH
* ------------------------------------------------------------------- *
RETMSG   DS    0H
         LA    R3,EVALBLOCK_EVDATA     POINT TO RESULT MESSAGE
         AR    R3,R2                     PLUS LENGTH OF MESSAGE
         BCTR  R3,0                      AND BACKUP TO PERIOD
         MVC   0(7,R3),=C', cc=x'''    ADD TO RESULT MESSAGE
         UNPK  7(3,R3),SHVRET(2)       UNPACK CHAR INTO HEX
         TR    7(2,R3),TRTBL           X'FAFBFCFDFEFF' TO C'ABCDEF'
         MVI   9(R3),C''''             ADD TO RESULT MESSAGE
         LA    R2,9(,R2)               ADD TO LENGTH OF MESSAGE
         B     FINISH                  GO RETURN MESSAGE
* ------------------------------------------------------------------- *
MVCMESG  MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION BELOW)
* ------------------------------------------------------------------- *
R15MSG   DS    0H
         LA    R3,EVALBLOCK_EVDATA     POINT TO RESULT MESSAGE
         AR    R3,R2                     PLUS LENGTH OF MESSAGE
         BCTR  R3,0                      AND BACKUP TO PERIOD
         MVC   0(5,R3),=C', rc='       ADD TO MESSAGE
         LA    R2,4(,R2)                 ADD TO LENGTH OF MESSAGE
         LA    R3,5(,R3)                 ADD TO ADDR OF NEXT MSG POS
         CVD   R15,PCOUNT              CONVERT REG15 TO DECIMAL
         MVC   EWORK,=X'40202020202160'  AND LOAD THE EDIT PATTERN
         EDMK  EWORK,PCOUNT+5          EDIT THE VARIABLE COUNTER
         LA    R4,EWORK+7              COMPUTE LENGTH OF
         SR    R4,R1                     REMAINING SIGNIFICANT DIGITS
         BCTR  R4,0                    DECR. LENGTH FOR ZERO REF.
         EX    R4,MVCMESG              MOVE TO RESULT MESSAGE
         AR    R2,R4                     ADD TO LENGTH OF MESSAGE
         LA    R2,1(,R2)                 PLUS ONE FOR ZER0 REF.
* ------------------------------------------------------------------- *
FINISH   ST    R2,EVALBLOCK_EVLEN      STORE LENGTH OF RESULT MESSAGE
RETURN   XR    R15,R15                 CLEAR REG15
         TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
TRTBL    EQU   *-64
         DC    192AL1(*-TRTBL)         Init for no translation, then
         ORG   TRTBL+C'a'              Translate lower-case
         DC    C'ABCDEFGHI'               to upper-case
         ORG   TRTBL+C'j'              Translate lower-case
         DC    C'JKLMNOPQR'               to upper-case
         ORG   TRTBL+C's'              Translate lower-case
         DC    C'STUVWXYZ'                to upper-case
         ORG   TRTBL+X'FA'             Translate unpacked data
         DC    C'ABCDEF'                  to valid hex
         ORG
*
SALEN    DC    A(DYNALENG)
         LTORG
* =================================================================== *
         END
