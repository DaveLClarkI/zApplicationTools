WILD     TITLE 'WILDCOMP - WILD-CARD COMPARISON BETWEEN TWO STRINGS'    00000100
         PRINT ON,GEN                                                   00000200
WILDCOMP AMODE 31
WILDCOMP RMODE ANY
         EJECT                                                          00010500
* =================================================================== * 00010600
WILDPARM DSECT                                                          00010700
         USING WILDPARM,R10            ESTABLISH REG 10 AS BASE         00010800
* ------------------------------------------------------------------- * 00014800
WILDCTL  DS    CL8                     PROGRAM NAME                     00011000
WILDRES  DS    PL2                     RETURN RESULT                    00011100
WILDONE  DS    CL1                     MATCH ON ONE CHARACTER           00011300
WILDALL  DS    CL1                     MATCH ON ALL REMAINING CHARS     00011400
WILDLEN  DS    H                       COMPARE LENGTH OF BOTH STRINGS   00011600
WILDSTR1 DS    CL40                    FIRST STRING (WITH WILD CHARS)   00011700
WILDSTR2 DS    CL40                    SECOND STRING (TO COMPARE TO)    00011800
* =================================================================== * 00012200
         EJECT                                                          00012300
* =================================================================== * 00012400
*        REGISTER USAGE                                                 00012500
* ------------------------------------------------------------------- * 00014800
R0       EQU   0                                                        00012700
R1       EQU   1                                                        00012800
R2       EQU   2                                                        00012900
R3       EQU   3                                                        00013000
R4       EQU   4                                                        00013100
R5       EQU   5                                                        00013200
R6       EQU   6                                                        00013300
R7       EQU   7                                                        00013400
R8       EQU   8                                                        00013500
R9       EQU   9                                                        00013600
R10      EQU   10                      DSECT BASE REG (WILDPARM)        00013700
R11      EQU   11                      CSECT BASE REG (WILDCOMP)        00013800
R12      EQU   12                      RESERVED FOR LE/VSE              00013900
R13      EQU   13                      REGS SAVE AREA                   00014000
R14      EQU   14                      RETURN REG                       00014100
R15      EQU   15                      ENTRY REG                        00014200
* =================================================================== * 00014300
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* ------------------------------------------------------------------- * 00014800
         PRINT OFF
         COPY  IF
         COPY  WHILE
         PRINT ON
* =================================================================== * 00012800
         EJECT                                                          00014400
* =================================================================== * 00014500
WILDCOMP CSECT                                                          00014600
         USING WILDCOMP,R15       ESTABLISH ENTRY-POINT REG AS BASE     00014700
* ------------------------------------------------------------------- * 00014800
         B     START              BRANCH AROUND CORE EYE-CATCHER        00014900
*                                                                       00015000
THISPGM  DC    C'WILDCOMP'        PROGRAM                               00015100
         DC    C'1.0 '            VERSION                               00015200
         DC    C'DLC '            AUTHOR                                00015300
         DC    C'(C)APR08'        DATE                                  00015400
*                                                                       00015500
START    DS    0H                                                       00015600
         SAVE  (14,12)            SAVE CALLER'S REGISTERS               00015700
         LR    R11,R15            INITIALIZE ROUTINE BASE REGISTER      00015800
         DROP  R15                DISESTABLISH TEMP BASE REGISTER       00015900
         USING WILDCOMP,R11       ESTABLISH PERM BASE REGISTER          00016000
*                                                                       00016100
         L     R10,0(,R1)         GET ADDRESS OF PARAMETERS             00016200
         IF    WILDCTL,(NE,CLC),THISPGM
          ZAP  WILDRES,=PL2'11'   SIGNAL PARM AREA FORMAT ISSUES
          B    RETURN             GO RETURN TO CALLER
         ENDIF
         IF    WILDONE,NH,C' '    IF NO ONE-CHAR MATCH SPECIFIED
          MVI  WILDONE,C'?'         SET DEFAULT TO A QUESTION MARK
         ENDIF
         IF    WILDALL,NH,C' '    IF NO ALL-CHAR MATCH SPECIFIED
          MVI  WILDALL,C'*'         SET DEFAULT TO AN ASTERISK
         ENDIF
         IF    WILDONE,(EQ,CLC),WILDALL
          ZAP  WILDRES,=PL2'12'   SIGNAL WILD CARD CHAR ISSUES
          B    RETURN             GO RETURN TO CALLER
         ENDIF
         LH    R5,WILDLEN         GET LENGTH OF COMPARE STRINGS
         IF    R5,(LT,CH),=H'1',OR,                                    +
               R5,(HT,CH),=YL2(L'WILDSTR1)
          ZAP  WILDRES,=PL2'13'   SIGNAL STRING LENGTH ISSUES
          B    RETURN             GO RETURN TO CALLER
         ENDIF
         ZAP   WILDRES,=PL2'0'    INITIALIZE RESULT VALUE               00016300
         LA    R3,WILDSTR1
         LA    R4,WILDSTR2
*                                                                       00016400
         WHILE R5,(NZ,LTR),R5                                           00016500
          IF   0(1,R3),(EQ,CLC),WILDALL                                 00016600
           WEXIT
          ENDIF
          IF   0(1,R3),(NE,CLC),WILDONE                                 00016600
          AND  0(1,R3),(NE,CLC),0(R4)                                   00016600
           IF 0(1,R3),(LT,CLC),0(R4)                                    00016600
            ZAP WILDRES,=PL2'-1'  SIGNAL STR1 LT STR2                   00016300
           ELSE
            ZAP WILDRES,=PL2'+1'  SIGNAL STR1 GT STR2                   00016300
           ENDIF
           WEXIT
          ENDIF
          LA   R3,1(,R3)
          LA   R4,1(,R4)
          BCTR R5,0                                                     00016700
         WEND                                                           00016800
*                                                                       00016900
RETURN   RETURN (14,12),RC=0      RETURN TO CALLER                      00024000
* =================================================================== * 00024100
         EJECT                                                          00024200
* =================================================================== * 00024300
STORAGE  DS    0D                                                       00024400
* ------------------------------------------------------------------- * 00014800
         LTORG                                                          00024600
* =================================================================== * 00024700
         END                                                            00024800
