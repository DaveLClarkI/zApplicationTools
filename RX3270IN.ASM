3270     TITLE 'RX3270IN - REXX SUB TO CREATE 3270 CONTROL CHARS'
         PRINT ON,GEN
RX3270IN AMODE 24
RX3270IN RMODE 24
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
*      This  external  REXX  subroutine   is   intended  for  use  in *
* combination  with  the  RX3270IO  external  REXX  function.    This *
* subroutine creates the 3270 control  characters needed to work with *
* the input to, and the output from, the RX3270IO function.           *
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
* REXX "CALL" statement -- as follows:                                *
*                                                                     *
*             /* initialize REXX program with 3270 literals */        *
*   Call RX3270IN;                                                    *
*   If rc ¬= 0 Then Do                                                *
*     Say " RX3270IN call error: rc = "rc", result = "result;         *
*     Exit 4;                                                         *
*   End                                                               *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                INPUT                                *
*                                                                     *
* There is no input to this subroutine call.                          *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
* On output, the following values are returned:                       *
*                                                                     *
*   RX3270_CCC.LIT_LCL_WRITE           /* channel command codes */    *
*   RX3270_CCC.LIT_LCL_ERASE_WRITE                                    *
*   RX3270_CCC.LIT_LCL_ERASE_WRITE_ALTERNATE                          *
*   RX3270_CCC.LIT_LCL_ERASE_ALL_UNPROTECTED                          *
*   RX3270_CCC.LIT_LCL_WRITE_STRUCTURED                               *
*                                                                     *
*   RX3270_WCC.LIT_LEAVEKB             /* write control characters */ *
*   RX3270_WCC.LIT_LEAVEKB_FRSET                                      *
*   RX3270_WCC.LIT_FREEKB                                             *
*   RX3270_WCC.LIT_FREEKB_FRSET                                       *
*   RX3270_WCC.LIT_LEAVEKB_ALARM                                      *
*   RX3270_WCC.LIT_LEAVEKB_FRSET_ALARM                                *
*   RX3270_WCC.LIT_FREEKB_ALARM                                       *
*   RX3270_WCC.LIT_FREEKB_FRSET_ALARM                                 *
*                                                                     *
*   RX3270_DSO.LIT_PROGRAM_TAB         /* data stream orders */       *
*   RX3270_DSO.LIT_GRAPHIC_ESCAPE                                     *
*   RX3270_DSO.LIT_SET_BUFFER_ADDRESS                                 *
*   RX3270_DSO.LIT_ERASE_UNPROTECTED                                  *
*   RX3270_DSO.LIT_INSERT_CURSOR                                      *
*   RX3270_DSO.LIT_START_FIELD                                        *
*   RX3270_DSO.LIT_SET_ATTRIBUTE                                      *
*   RX3270_DSO.LIT_START_FIELD_EXTENDED                               *
*   RX3270_DSO.LIT_MODIFY_FIELD                                       *
*   RX3270_DSO.LIT_REPEAT_TO_ADDRESS                                  *
*                                                                     *
*   RX3270_TYP.LIT_RESET               /* extended attrib. types */   *
*   RX3270_TYP.LIT_FIELD_ATTRIBUTE                                    *
*   RX3270_TYP.LIT_FIELD_VALIDATION                                   *
*   RX3270_TYP.LIT_FIELD_OUTLINING                                    *
*   RX3270_TYP.LIT_EXTENDED_HILIGHT                                   *
*   RX3270_TYP.LIT_EXTENDED_COLOR                                     *
*   RX3270_TYP.LIT_CHARACTER_SET                                      *
*   RX3270_TYP.LIT_BACKGROUND_COLOR                                   *
*   RX3270_TYP.LIT_TRANSPARENCY                                       *
*                                                                     *
*   RX3270_AID.LIT_NO_AID              /* attention identifier char*/ *
*   RX3270_AID.LIT_CLEAR_KEY                                          *
*   RX3270_AID.LIT_ENTER_KEY                                          *
*   RX3270_AID.LIT_CURSOR_SELECT                                      *
*   RX3270_AID.LIT_PA1_KEY                                            *
*   RX3270_AID.LIT_PA2_KEY                                            *
*   RX3270_AID.LIT_PA3_KEY                                            *
*   RX3270_AID.LIT_PF1_KEY                                            *
*   RX3270_AID.LIT_PF2_KEY                                            *
*   RX3270_AID.LIT_PF3_KEY                                            *
*   RX3270_AID.LIT_PF4_KEY                                            *
*   RX3270_AID.LIT_PF5_KEY                                            *
*   RX3270_AID.LIT_PF6_KEY                                            *
*   RX3270_AID.LIT_PF7_KEY                                            *
*   RX3270_AID.LIT_PF8_KEY                                            *
*   RX3270_AID.LIT_PF9_KEY                                            *
*   RX3270_AID.LIT_PF10_KEY                                           *
*   RX3270_AID.LIT_PF11_KEY                                           *
*   RX3270_AID.LIT_PF12_KEY                                           *
*   RX3270_AID.LIT_PF13_KEY                                           *
*   RX3270_AID.LIT_PF14_KEY                                           *
*   RX3270_AID.LIT_PF15_KEY                                           *
*   RX3270_AID.LIT_PF16_KEY                                           *
*   RX3270_AID.LIT_PF17_KEY                                           *
*   RX3270_AID.LIT_PF18_KEY                                           *
*   RX3270_AID.LIT_PF19_KEY                                           *
*   RX3270_AID.LIT_PF20_KEY                                           *
*   RX3270_AID.LIT_PF21_KEY                                           *
*   RX3270_AID.LIT_PF22_KEY                                           *
*   RX3270_AID.LIT_PF23_KEY                                           *
*   RX3270_AID.LIT_PF24_KEY                                           *
*                                                                     *
*   RX3270_ATR.LIT_UNPROT_NORM         /* field attribute chars */    *
*   RX3270_ATR.LIT_UNPROT_NORM_FSET                                   *
*   RX3270_ATR.LIT_UNPROT_NORM_DET                                    *
*   RX3270_ATR.LIT_UNPROT_NORM_DET_FSET                               *
*   RX3270_ATR.LIT_UNPROT_BRT                                         *
*   RX3270_ATR.LIT_UNPROT_BRT_FSET                                    *
*   RX3270_ATR.LIT_UNPROT_DRK                                         *
*   RX3270_ATR.LIT_UNPROT_DRK_FSET                                    *
*   RX3270_ATR.LIT_UNPROT_NORM_NUM                                    *
*   RX3270_ATR.LIT_UNPROT_NORM_NUM_FSET                               *
*   RX3270_ATR.LIT_UNPROT_NORM_NUM_DET                                *
*   RX3270_ATR.LIT_UNPROT_NORM_NUM_DET_FSET                           *
*   RX3270_ATR.LIT_UNPROT_BRT_NUM                                     *
*   RX3270_ATR.LIT_UNPROT_BRT_NUM_FSET                                *
*   RX3270_ATR.LIT_UNPROT_DRK_NUM                                     *
*   RX3270_ATR.LIT_UNPROT_DRK_NUM_FSET                                *
*   RX3270_ATR.LIT_PROT_NORM                                          *
*   RX3270_ATR.LIT_PROT_NORM_FSET                                     *
*   RX3270_ATR.LIT_PROT_NORM_DET                                      *
*   RX3270_ATR.LIT_PROT_NORM_DET_FSET                                 *
*   RX3270_ATR.LIT_PROT_BRT                                           *
*   RX3270_ATR.LIT_PROT_BRT_FSET                                      *
*   RX3270_ATR.LIT_PROT_DRK                                           *
*   RX3270_ATR.LIT_PROT_DRK_FSET                                      *
*   RX3270_ATR.LIT_ASKIP_NORM                                         *
*   RX3270_ATR.LIT_ASKIP_NORM_FSET                                    *
*   RX3270_ATR.LIT_ASKIP_NORM_DET                                     *
*   RX3270_ATR.LIT_ASKIP_NORM_DET_FSET                                *
*   RX3270_ATR.LIT_ASKIP_BRT                                          *
*   RX3270_ATR.LIT_ASKIP_BRT_FSET                                     *
*   RX3270_ATR.LIT_ASKIP_DRK                                          *
*   RX3270_ATR.LIT_ASKIP_DRK_FSET                                     *
*                                                                     *
*   RX3270_CSR.LIT_NO                  /* insert cursor characters */ *
*   RX3270_CSR.LIT_YES                                                *
*                                                                     *
*   RX3270_EXC.LIT_DEFAULT             /* extended color characters*/ *
*   RX3270_EXC.LIT_BLUE                                               *
*   RX3270_EXC.LIT_RED                                                *
*   RX3270_EXC.LIT_PINK                                               *
*   RX3270_EXC.LIT_GREEN                                              *
*   RX3270_EXC.LIT_TURQUOISE                                          *
*   RX3270_EXC.LIT_YELLOW                                             *
*   RX3270_EXC.LIT_WHITE                                              *
*                                                                     *
*   RX3270_EXH.LIT_OFF                 /* extended hilite chars */    *
*   RX3270_EXH.LIT_BLINK                                              *
*   RX3270_EXH.LIT_REVERSE                                            *
*   RX3270_EXH.LIT_UNDERLINE                                          *
*                                                                     *
* In addition,  the RX3270IN subroutine  call  will return one of the *
* following return codes in the REXX "RC" special variable.           *
*                                                                     *
*    0   no errors occurred                                           *
*    8   internal REXX function failed                                *
*          also, the REXX function's register 15 value is             *
*          returned in the REXX "RESULT" special variable             *
*   12   REXX environment not present                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PWORK8   DS    PL8                     PACKED DOUBLE-WORD
EWORK8   DS    CL8                     EDITED WORK FIELD
*
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
VARNAME  DS    CL50                    VARIABLE NAME
VARDATA  DS    CL10                    VARIABLE NAME VALUE
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
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK
R1       EQU   1                       ON ENTRY, REXX EFPL REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                       REXX EXTERNAL ENTRY-POINT VECTOR
R9       EQU   9                       EXTERNAL FUNCTION PARAMETER LIST
R10      EQU   10                      REXX EVALUATION BLOCK
R11      EQU   11                      SHARED VARIABLES CONTROL BLOCK
R12      EQU   12                      RX3270IN CSECT BASE REG
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
RX3270IN HEADER VERSION='1.1',AUTHOR='DLC',DATE='(C)MAR04',            X
               BASE=R12,SVAREA=SALEN
* =================================================================== *
         USING DYNASTOR,R13
*
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RX3270IN: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LA   R15,12                  SET ABNORMAL RETURN CODE
          B    RETR15                  GO RETURN ERROR
         ENDIF
         L     R8,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R8
         DROP  R5
*
         LR    R9,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R9
         L     R10,EFPLEVAL            REXX EVALUATION BLOCK POINTER
         L     R10,0(,R10)             REXX EVALUATION BLOCK
         USING EVALBLOCK,R10
         MVI   EVALBLOCK_EVDATA,X'00'  INIT FIRST BYTE OF RESULTS BUF
*
         LA    R11,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R11              SHARED VARIABLES CONTROL BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R0,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R1,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R3,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R0,R3,SHVPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
         OI    SHVPARMB+(4*4-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         XC    VARNAME,VARNAME         CLEAR BUFFER FOR VAR. NAME
         XC    VARDATA,VARDATA         CLEAR BUFFER FOR VAR. VALUE
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         LA    R0,VARNAME              GET ADDRESS FOR SHVNAMA
         LA    R1,L'VARNAME            GET LENGTH  FOR SHVNAML
         LA    R2,VARDATA              GET ADDRESS FOR SHVVALA
         LA    R3,L'VARDATA            GET LENGTH  FOR SHVVALL
         STM   R0,R3,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R1,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R3,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
* CREATE 3270 CONTROL CHARACTER VARIABLES
*
         LA    R2,1                    SET LENGTH OF DATA VALUE
         ST    R2,SHVVALL                AND PUT IN CONTROL BLOCK
         MVC   VARNAME(L'STEM),STEM
*
* ...CHANNEL COMMAND CODES
         MVC   VARNAME+L'STEM(L'CCC),CCC
         MVC   VARNAME+L'STEM+L'CCC(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'CCC+L'LIT
*
     LA R3,L'STEM+L'CCC+L'LIT+L'CCCWRITE
         MVC   0(L'CCCWRITE,R4),CCCWRITE
         MVI   VARDATA,X'01'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'CCC+L'LIT+L'CCCERWRT
         MVC   0(L'CCCERWRT,R4),CCCERWRT
         MVI   VARDATA,X'05'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'CCC+L'LIT+L'CCCERWRA
         MVC   0(L'CCCERWRA,R4),CCCERWRA
         MVI   VARDATA,X'0D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'CCC+L'LIT+L'CCCERAUP
         MVC   0(L'CCCERAUP,R4),CCCERAUP
         MVI   VARDATA,X'0F'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'CCC+L'LIT+L'CCCWRSTR
         MVC   0(L'CCCWRSTR,R4),CCCWRSTR
         MVI   VARDATA,X'11'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...WRITE CONTROL CHARACTERS
         MVC   VARNAME+L'STEM(L'WCC),WCC
         MVC   VARNAME+L'STEM+L'WCC(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'WCC+L'LIT
*
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCLKFST
         MVC   0(L'WCCLKFST,R4),WCCLKFST
         MVI   VARDATA,X'40'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCLKFRS
         MVC   0(L'WCCLKFRS,R4),WCCLKFRS
         MVI   VARDATA,X'C1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCFKFST
         MVC   0(L'WCCFKFST,R4),WCCFKFST
         MVI   VARDATA,X'C2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCFKFRS
         MVC   0(L'WCCFKFRS,R4),WCCFKFRS
         MVI   VARDATA,X'C3'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCLKFSA
         MVC   0(L'WCCLKFSA,R4),WCCLKFSA
         MVI   VARDATA,X'C4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCLKFRA
         MVC   0(L'WCCLKFRA,R4),WCCLKFRA
         MVI   VARDATA,X'C5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCFKFSA
         MVC   0(L'WCCFKFSA,R4),WCCFKFSA
         MVI   VARDATA,X'C6'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'WCC+L'LIT+L'WCCFKFRA
         MVC   0(L'WCCFKFRA,R4),WCCFKFRA
         MVI   VARDATA,X'C7'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...DATA STREAM ORDERS
         MVC   VARNAME+L'STEM(L'DSO),DSO
         MVC   VARNAME+L'STEM+L'DSO(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'DSO+L'LIT
*
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOPGTAB
         MVC   0(L'DSOPGTAB,R4),DSOPGTAB
         MVI   VARDATA,X'05'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOGRESC
         MVC   0(L'DSOGRESC,R4),DSOGRESC
         MVI   VARDATA,X'08'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOSETBA
         MVC   0(L'DSOSETBA,R4),DSOSETBA
         MVI   VARDATA,X'11'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOERASE
         MVC   0(L'DSOERASE,R4),DSOERASE
         MVI   VARDATA,X'12'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOCURSR
         MVC   0(L'DSOCURSR,R4),DSOCURSR
         MVI   VARDATA,X'13'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOSTFLD
         MVC   0(L'DSOSTFLD,R4),DSOSTFLD
         MVI   VARDATA,X'1D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOSETAT
         MVC   0(L'DSOSETAT,R4),DSOSETAT
         MVI   VARDATA,X'28'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOSTFEX
         MVC   0(L'DSOSTFEX,R4),DSOSTFEX
         MVI   VARDATA,X'29'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOMODFY
         MVC   0(L'DSOMODFY,R4),DSOMODFY
         MVI   VARDATA,X'2C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'DSO+L'LIT+L'DSOREPTA
         MVC   0(L'DSOREPTA,R4),DSOREPTA
         MVI   VARDATA,X'3C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...EXTENDED FIELD AND CHARACTER ATTRIBUTE TYPES
         MVC   VARNAME+L'STEM(L'TYP),TYP
         MVC   VARNAME+L'STEM+L'TYP(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'TYP+L'LIT
*
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPRESET
         MVC   0(L'TYPRESET,R4),TYPRESET
         MVI   VARDATA,X'00'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPFIELD
         MVC   0(L'TYPFIELD,R4),TYPFIELD
         MVI   VARDATA,X'C0'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPVALDN
         MVC   0(L'TYPVALDN,R4),TYPVALDN
         MVI   VARDATA,X'C1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPOUTLN
         MVC   0(L'TYPOUTLN,R4),TYPOUTLN
         MVI   VARDATA,X'C2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPHILIT
         MVC   0(L'TYPHILIT,R4),TYPHILIT
         MVI   VARDATA,X'41'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPCOLOR
         MVC   0(L'TYPCOLOR,R4),TYPCOLOR
         MVI   VARDATA,X'42'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPPSYMB
         MVC   0(L'TYPPSYMB,R4),TYPPSYMB
         MVI   VARDATA,X'43'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPBGCLR
         MVC   0(L'TYPBGCLR,R4),TYPBGCLR
         MVI   VARDATA,X'45'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'TYP+L'LIT+L'TYPTRANS
         MVC   0(L'TYPTRANS,R4),TYPTRANS
         MVI   VARDATA,X'46'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...KEYBOARD ATTENTION IDENTIFIERS
         MVC   VARNAME+L'STEM(L'AID),AID
         MVC   VARNAME+L'STEM+L'AID(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'AID+L'LIT
*
     LA R3,L'STEM+L'AID+L'LIT+L'AIDNOAID
         MVC   0(L'AIDNOAID,R4),AIDNOAID
         MVI   VARDATA,X'60'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPAK03
         MVC   0(L'AIDPAK03,R4),AIDPAK03
         MVI   VARDATA,X'6B'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPAK01
         MVC   0(L'AIDPAK01,R4),AIDPAK01
         MVI   VARDATA,X'6C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDCLEAR
         MVC   0(L'AIDCLEAR,R4),AIDCLEAR
         MVI   VARDATA,X'6D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPAK02
         MVC   0(L'AIDPAK02,R4),AIDPAK02
         MVI   VARDATA,X'6E'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK01
         MVC   0(L'AIDPFK01,R4),AIDPFK01
         MVI   VARDATA,X'F1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK02
         MVC   0(L'AIDPFK02,R4),AIDPFK02
         MVI   VARDATA,X'F2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK03
         MVC   0(L'AIDPFK03,R4),AIDPFK03
         MVI   VARDATA,X'F3'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK04
         MVC   0(L'AIDPFK04,R4),AIDPFK04
         MVI   VARDATA,X'F4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK05
         MVC   0(L'AIDPFK05,R4),AIDPFK05
         MVI   VARDATA,X'F5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK06
         MVC   0(L'AIDPFK06,R4),AIDPFK06
         MVI   VARDATA,X'F6'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK07
         MVC   0(L'AIDPFK07,R4),AIDPFK07
         MVI   VARDATA,X'F7'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK08
         MVC   0(L'AIDPFK08,R4),AIDPFK08
         MVI   VARDATA,X'F8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK09
         MVC   0(L'AIDPFK09,R4),AIDPFK09
         MVI   VARDATA,X'F9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK10
         MVC   0(L'AIDPFK10,R4),AIDPFK10
         MVI   VARDATA,X'7A'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK11
         MVC   0(L'AIDPFK11,R4),AIDPFK11
         MVI   VARDATA,X'7B'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK12
         MVC   0(L'AIDPFK12,R4),AIDPFK12
         MVI   VARDATA,X'7C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDENTER
         MVC   0(L'AIDENTER,R4),AIDENTER
         MVI   VARDATA,X'7D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDCURSR
         MVC   0(L'AIDCURSR,R4),AIDCURSR
         MVI   VARDATA,X'7E'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK13
         MVC   0(L'AIDPFK13,R4),AIDPFK13
         MVI   VARDATA,X'C1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK14
         MVC   0(L'AIDPFK14,R4),AIDPFK14
         MVI   VARDATA,X'C2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK15
         MVC   0(L'AIDPFK15,R4),AIDPFK15
         MVI   VARDATA,X'C3'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK16
         MVC   0(L'AIDPFK16,R4),AIDPFK16
         MVI   VARDATA,X'C4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK17
         MVC   0(L'AIDPFK17,R4),AIDPFK17
         MVI   VARDATA,X'C5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK18
         MVC   0(L'AIDPFK18,R4),AIDPFK18
         MVI   VARDATA,X'C6'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK19
         MVC   0(L'AIDPFK19,R4),AIDPFK19
         MVI   VARDATA,X'C7'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK20
         MVC   0(L'AIDPFK20,R4),AIDPFK20
         MVI   VARDATA,X'C8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK21
         MVC   0(L'AIDPFK21,R4),AIDPFK21
         MVI   VARDATA,X'C9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK22
         MVC   0(L'AIDPFK22,R4),AIDPFK22
         MVI   VARDATA,X'4A'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK23
         MVC   0(L'AIDPFK23,R4),AIDPFK23
         MVI   VARDATA,X'4B'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'AID+L'LIT+L'AIDPFK24
         MVC   0(L'AIDPFK24,R4),AIDPFK24
         MVI   VARDATA,X'4C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...STANDARD FIELD ATTRIBUTES
         MVC   VARNAME+L'STEM(L'ATR),ATR
         MVC   VARNAME+L'STEM+L'ATR(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'ATR+L'LIT
*
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNPRO
         MVC   0(L'ATRUNPRO,R4),ATRUNPRO
         MVI   VARDATA,X'40'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNFST
         MVC   0(L'ATRUNFST,R4),ATRUNFST
         MVI   VARDATA,X'C1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDET
         MVC   0(L'ATRUNDET,R4),ATRUNDET
         MVI   VARDATA,X'C4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDTF
         MVC   0(L'ATRUNDTF,R4),ATRUNDTF
         MVI   VARDATA,X'C5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNBRT
         MVC   0(L'ATRUNBRT,R4),ATRUNBRT
         MVI   VARDATA,X'C8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNBRF
         MVC   0(L'ATRUNBRF,R4),ATRUNBRF
         MVI   VARDATA,X'C9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDRK
         MVC   0(L'ATRUNDRK,R4),ATRUNDRK
         MVI   VARDATA,X'4C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDKF
         MVC   0(L'ATRUNDKF,R4),ATRUNDKF
         MVI   VARDATA,X'4D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNNUM
         MVC   0(L'ATRUNNUM,R4),ATRUNNUM
         MVI   VARDATA,X'50'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNNMF
         MVC   0(L'ATRUNNMF,R4),ATRUNNMF
         MVI   VARDATA,X'D1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNNMD
         MVC   0(L'ATRUNNMD,R4),ATRUNNMD
         MVI   VARDATA,X'D4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNNDF
         MVC   0(L'ATRUNNDF,R4),ATRUNNDF
         MVI   VARDATA,X'D5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNBRN
         MVC   0(L'ATRUNBRN,R4),ATRUNBRN
         MVI   VARDATA,X'D8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNBNF
         MVC   0(L'ATRUNBNF,R4),ATRUNBNF
         MVI   VARDATA,X'D9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDKN
         MVC   0(L'ATRUNDKN,R4),ATRUNDKN
         MVI   VARDATA,X'5C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRUNDNF
         MVC   0(L'ATRUNDNF,R4),ATRUNDNF
         MVI   VARDATA,X'5D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
    LA R3,L'STEM+L'ATR+L'LIT+L'ATRPROT
         MVC   0(L'ATRPROT,R4),ATRPROT
         MVI   VARDATA,X'60'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRFST
         MVC   0(L'ATRPRFST,R4),ATRPRFST
         MVI   VARDATA,X'61'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRDET
         MVC   0(L'ATRPRDET,R4),ATRPRDET
         MVI   VARDATA,X'E4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRDTF
         MVC   0(L'ATRPRDTF,R4),ATRPRDTF
         MVI   VARDATA,X'E5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRBRT
         MVC   0(L'ATRPRBRT,R4),ATRPRBRT
         MVI   VARDATA,X'E8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRBRF
         MVC   0(L'ATRPRBRF,R4),ATRPRBRF
         MVI   VARDATA,X'E9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRDRK
         MVC   0(L'ATRPRDRK,R4),ATRPRDRK
         MVI   VARDATA,X'6C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRPRDKF
         MVC   0(L'ATRPRDKF,R4),ATRPRDKF
         MVI   VARDATA,X'6D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASKIP
         MVC   0(L'ATRASKIP,R4),ATRASKIP
         MVI   VARDATA,X'F0'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASFST
         MVC   0(L'ATRASFST,R4),ATRASFST
         MVI   VARDATA,X'F1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASDET
         MVC   0(L'ATRASDET,R4),ATRASDET
         MVI   VARDATA,X'F4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASDTF
         MVC   0(L'ATRASDTF,R4),ATRASDTF
         MVI   VARDATA,X'F5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASBRT
         MVC   0(L'ATRASBRT,R4),ATRASBRT
         MVI   VARDATA,X'F8'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASBRF
         MVC   0(L'ATRASBRF,R4),ATRASBRF
         MVI   VARDATA,X'F9'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASDRK
         MVC   0(L'ATRASDRK,R4),ATRASDRK
         MVI   VARDATA,X'7C'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'ATR+L'LIT+L'ATRASDKF
         MVC   0(L'ATRASDKF,R4),ATRASDKF
         MVI   VARDATA,X'7D'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...INSERT CURSOR LITERALS
         MVC   VARNAME+L'STEM(L'CSR),CSR
         MVC   VARNAME+L'STEM+L'CSR(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'CSR+L'LIT
*
  LA R3,L'STEM+L'CSR+L'LIT+L'CSRNO
         MVC   0(L'CSRNO,R4),CSRNO
         MVI   VARDATA,X'00'
         BAS   R5,SETVAR               CREATE THE VARIABLE
   LA R3,L'STEM+L'CSR+L'LIT+L'CSRYES
         MVC   0(L'CSRYES,R4),CSRYES
         MVI   VARDATA,X'13'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...EXTENDED COLOR ATTRIBUTES
         MVC   VARNAME+L'STEM(L'EXC),EXC
         MVC   VARNAME+L'STEM+L'EXC(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'EXC+L'LIT
*
     LA R3,L'STEM+L'EXC+L'LIT+L'EXCDEFLT
         MVC   0(L'EXCDEFLT,R4),EXCDEFLT
         MVI   VARDATA,X'00'
         BAS   R5,SETVAR               CREATE THE VARIABLE
    LA R3,L'STEM+L'EXC+L'LIT+L'EXCBLUE
         MVC   0(L'EXCBLUE,R4),EXCBLUE
         MVI   VARDATA,X'F1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
   LA R3,L'STEM+L'EXC+L'LIT+L'EXCRED
         MVC   0(L'EXCRED,R4),EXCRED
         MVI   VARDATA,X'F2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
    LA R3,L'STEM+L'EXC+L'LIT+L'EXCPINK
         MVC   0(L'EXCPINK,R4),EXCPINK
         MVI   VARDATA,X'F3'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXC+L'LIT+L'EXCGREEN
         MVC   0(L'EXCGREEN,R4),EXCGREEN
         MVI   VARDATA,X'F4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXC+L'LIT+L'EXCTURQU
         MVC   0(L'EXCTURQU,R4),EXCTURQU
         MVI   VARDATA,X'F5'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXC+L'LIT+L'EXCYELLW
         MVC   0(L'EXCYELLW,R4),EXCYELLW
         MVI   VARDATA,X'F6'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXC+L'LIT+L'EXCWHITE
         MVC   0(L'EXCWHITE,R4),EXCWHITE
         MVI   VARDATA,X'F7'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* ...EXTENDED HILITE ATTRIBUTES
         MVC   VARNAME+L'STEM(L'EXH),EXH
         MVC   VARNAME+L'STEM+L'EXH(L'LIT),LIT
         LA R4,VARNAME+L'STEM+L'EXH+L'LIT
*
   LA R3,L'STEM+L'EXH+L'LIT+L'EXHOFF
         MVC   0(L'EXHOFF,R4),EXHOFF
         MVI   VARDATA,X'00'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXH+L'LIT+L'EXHBLINK
         MVC   0(L'EXHBLINK,R4),EXHBLINK
         MVI   VARDATA,X'F1'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXH+L'LIT+L'EXHREVRS
         MVC   0(L'EXHREVRS,R4),EXHREVRS
         MVI   VARDATA,X'F2'
         BAS   R5,SETVAR               CREATE THE VARIABLE
     LA R3,L'STEM+L'EXH+L'LIT+L'EXHULINE
         MVC   0(L'EXHULINE,R4),EXHULINE
         MVI   VARDATA,X'F4'
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   LA    R3,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         CVD   R15,PWORK8              CONVERT REG15 TO PACKED-DECIMAL
         BAS   R15,PAK2ZON             CONVERT THAT TO ZONED-DECIMAL
         ST    R2,EVALBLOCK_EVLEN      LENGTH OF CONVERTED RESULT
*
         LA    R3,VARDATA              POINT TO VARIABLE DATA AREA
         CVD   R5,PWORK8               CONVERT REG5 TO PACKED-DECIMAL
         BAS   R15,PAK2ZON             CONVERT THAT TO ZONED-DECIMAL
         ST    R2,SHVVALL              LENGTH OF CONVERTED RESULT
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
PAK2ZON  DS    0H
*
         MVC   EWORK8(7),=X'40202020212060'  LOAD EDIT PATTERN
         LA    R1,EWORK8+5             POINT TO SIGNIFICANCE BYTE
         LA    R2,EWORK8+6             POINT TO SIGN BYTE (& IGNORE)
         IF    EWORK8(7),(M,EDMK),PWORK8+5   IF NEGATIVE NUMBER
          BCTR R1,0                    DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'                FLOAT A LEFT MINUS SIGN
         ENDIF
         SR    R2,R1                   CALC NO. OF CHARS TO MOVE
         BCTR  R2,0                    DECR. LEN FOR ZERO REF.
         EX    R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
*
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
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
STORAGE  DS    0D
* =================================================================== *
SALEN    DC    A(DYNALENG)
         LTORG
STEM     DC    C'RX3270_'
LIT      DC    C'.LIT_'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        LOCAL CCW COMMAND CODES                                      *
* ******************************************************************* *
CCC      DC    C'CCC'
CCCWRITE DC    C'LCL_WRITE'
CCCERWRT DC    C'LCL_ERASE_WRITE'
CCCERWRA DC    C'LCL_ERASE_WRITE_ALTERNATE'
CCCERAUP DC    C'LCL_ERASE_ALL_UNPROTECTED'
CCCWRSTR DC    C'LCL_WRITE_STRUCTURED'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        WRITE CONTROL CHARACTER LITERALS                             *
* ******************************************************************* *
WCC      DC    C'WCC'
WCCLKFST DC    C'LEAVEKB'
WCCLKFRS DC    C'LEAVEKB_FRSET'
WCCFKFST DC    C'FREEKB'
WCCFKFRS DC    C'FREEKB_FRSET'
WCCLKFSA DC    C'LEAVEKB_ALARM'
WCCLKFRA DC    C'LEAVEKB_FRSET_ALARM'
WCCFKFSA DC    C'FREEKB_ALARM'
WCCFKFRA DC    C'FREEKB_FRSET_ALARM'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        DATA STREAM ORDERS LITERALS                                  *
* ******************************************************************* *
DSO      DC    C'DSO'
DSOPGTAB DC    C'PROGRAM_TAB'
DSOGRESC DC    C'GRAPHIC_ESCAPE'
DSOSETBA DC    C'SET_BUFFER_ADDRESS'
DSOERASE DC    C'ERASE_UNPROTECTED'
DSOCURSR DC    C'INSERT_CURSOR'
DSOSTFLD DC    C'START_FIELD'
DSOSETAT DC    C'SET_ATTRIBUTE'
DSOSTFEX DC    C'START_FIELD_EXTENDED'
DSOMODFY DC    C'MODIFY_FIELD'
DSOREPTA DC    C'REPEAT_TO_ADDRESS'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        EXTENDED FIELD AND CHARACTER ATTRIBUTE TYPES                 *
* ******************************************************************* *
TYP      DC    C'TYP'
TYPRESET DC    C'RESET'
TYPFIELD DC    C'FIELD_ATTRIBUTE'
TYPVALDN DC    C'FIELD_VALIDATION'
TYPOUTLN DC    C'FIELD_OUTLINING'
TYPHILIT DC    C'EXTENDED_HILIGHT'
TYPCOLOR DC    C'EXTENDED_COLOR'
TYPPSYMB DC    C'CHARACTER_SET'
TYPBGCLR DC    C'BACKGROUND_COLOR'
TYPTRANS DC    C'TRANSPARENCY'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        ATTENTION IDENTIFIER LITERALS                                *
* ******************************************************************* *
AID      DC    C'AID'
AIDNOAID DC    C'NO_AID'
AIDPAK03 DC    C'PA3_KEY'
AIDPAK01 DC    C'PA1_KEY'
AIDCLEAR DC    C'CLEAR_KEY'
AIDPAK02 DC    C'PA2_KEY'
AIDPFK01 DC    C'PF1_KEY'
AIDPFK02 DC    C'PF2_KEY'
AIDPFK03 DC    C'PF3_KEY'
AIDPFK04 DC    C'PF4_KEY'
AIDPFK05 DC    C'PF5_KEY'
AIDPFK06 DC    C'PF6_KEY'
AIDPFK07 DC    C'PF7_KEY'
AIDPFK08 DC    C'PF8_KEY'
AIDPFK09 DC    C'PF9_KEY'
AIDPFK10 DC    C'PF10_KEY'
AIDPFK11 DC    C'PF11_KEY'
AIDPFK12 DC    C'PF12_KEY'
AIDENTER DC    C'ENTER_KEY'
AIDCURSR DC    C'CURSOR_SELECT'        (OR LIGHT PEN)
AIDPFK13 DC    C'PF13_KEY'
AIDPFK14 DC    C'PF14_KEY'
AIDPFK15 DC    C'PF15_KEY'
AIDPFK16 DC    C'PF16_KEY'
AIDPFK17 DC    C'PF17_KEY'
AIDPFK18 DC    C'PF18_KEY'
AIDPFK19 DC    C'PF19_KEY'
AIDPFK20 DC    C'PF20_KEY'
AIDPFK21 DC    C'PF21_KEY'
AIDPFK22 DC    C'PF22_KEY'
AIDPFK23 DC    C'PF23_KEY'
AIDPFK24 DC    C'PF24_KEY'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        STANDARD FIELD ATTRIBUTE LITERALS                            *
* ******************************************************************* *
ATR      DC    C'ATR'
ATRUNPRO DC    C'UNPROT_NORM'
ATRUNFST DC    C'UNPROT_NORM_FSET'
ATRUNDET DC    C'UNPROT_NORM_DET'
ATRUNDTF DC    C'UNPROT_NORM_DET_FSET'
ATRUNBRT DC    C'UNPROT_BRT'
ATRUNBRF DC    C'UNPROT_BRT_FSET'
ATRUNDRK DC    C'UNPROT_DRK'
ATRUNDKF DC    C'UNPROT_DRK_FSET'
ATRUNNUM DC    C'UNPROT_NORM_NUM'
ATRUNNMF DC    C'UNPROT_NORM_NUM_FSET'
ATRUNNMD DC    C'UNPROT_NORM_NUM_DET'
ATRUNNDF DC    C'UNPROT_NORM_NUM_DET_FSET'
ATRUNBRN DC    C'UNPROT_BRT_NUM'
ATRUNBNF DC    C'UNPROT_BRT_NUM_FSET'
ATRUNDKN DC    C'UNPROT_DRK_NUM'
ATRUNDNF DC    C'UNPROT_DRK_NUM_FSET'
ATRPROT  DC    C'PROT_NORM'
ATRPRFST DC    C'PROT_NORM_FSET'
ATRPRDET DC    C'PROT_NORM_DET'
ATRPRDTF DC    C'PROT_NORM_DET_FSET'
ATRPRBRT DC    C'PROT_BRT'
ATRPRBRF DC    C'PROT_BRT_FSET'
ATRPRDRK DC    C'PROT_DRK'
ATRPRDKF DC    C'PROT_DRK_FSET'
ATRASKIP DC    C'ASKIP_NORM'
ATRASFST DC    C'ASKIP_NORM_FSET'
ATRASDET DC    C'ASKIP_NORM_DET'
ATRASDTF DC    C'ASKIP_NORM_DET_FSET'
ATRASBRT DC    C'ASKIP_BRT'
ATRASBRF DC    C'ASKIP_BRT_FSET'
ATRASDRK DC    C'ASKIP_DRK'
ATRASDKF DC    C'ASKIP_DRK_FSET'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        INSERT CURSOR LITERALS                                       *
* ******************************************************************* *
CSR      DC    C'CSR'
CSRNO    DC    C'NO'
CSRYES   DC    C'YES'
* ******************************************************************* *
         SPACE 3
* ******************************************************************* *
*        EXTENDED FIELD ATTRIBUTE LITERALS                            *
* ******************************************************************* *
EXC      DC    C'EXC'
EXCDEFLT DC    C'DEFAULT'              (GREEN)
EXCBLUE  DC    C'BLUE'
EXCRED   DC    C'RED'
EXCPINK  DC    C'PINK'
EXCGREEN DC    C'GREEN'
EXCTURQU DC    C'TURQUOISE'
EXCYELLW DC    C'YELLOW'
EXCWHITE DC    C'WHITE'                (NEUTRAL)
*
EXH      DC    C'EXH'
EXHOFF   DC    C'OFF'
EXHBLINK DC    C'BLINK'
EXHREVRS DC    C'REVERSE'
EXHULINE DC    C'UNDERLINE'
* ******************************************************************* *
         SPACE 3
* =================================================================== *
         END
