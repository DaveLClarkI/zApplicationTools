WILD     TITLE 'WILDCOMP - WILD-CARD COMPARISON BETWEEN TWO STRINGS'    00000100
         PRINT ON,GEN                                                   00000200
WILDCOMP AMODE 31
WILDCOMP RMODE ANY

* CHANGE HISTORY  -----------------------------------------------------
* 04/01/2008 DLC  ORIGINAL DESIGN.
* 09/01/2023 DLC0 ALLOW ENDING WILD-CARD ASTERISK TO BE OMITTED, BUT
*                 WITH THE SAME RESULT AS IF IT WERE THERE.
* 11/29/2023 DLC1 ADD BLANK-MATCHING CHARACTER SUPPORT.
* END OF HISTORY  -----------------------------------------------------

         EJECT                                                          00010500
* =================================================================== * 00010600
WILDPARM DSECT                                                          00010700
         USING *,R10              ESTABLISH REG 10 AS DSECT BASE        00010800
* ------------------------------------------------------------------- * 00014800
WILDCTL  DS    CL8                PROGRAM NAME                          00011000
WILDRES  DS    PL2                RETURN RESULT                         00011100
WILDONE  DS    CL1                MATCH ON ANY ONE CHARACTER            00011300
WILDALL  DS    CL1                MATCH ON ALL REMAINING CHARS          00011400
WILDBLNK DS    CL1                MATCH ON BLANK CHARACTER              00011400
WILDRSVD DS    CL1                RESERVED FOR FUTURE USE               00011400
WILDLEN  DS    H                  COMPARE LENGTH OF BOTH STRINGS        00011600
WILDSTR1 DS    CL40               FIRST STRING (W/WO WILD CHARS)        00011700
WILDSTR2 DS    CL40               SECOND STRING (TO COMPARE TO)         00011800
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
R10      EQU   10                 DSECT BASE REG (WILDPARM)             00013700
R11      EQU   11                 CSECT BASE REG (WILDCOMP)             00013800
R12      EQU   12                 RESERVED FOR LE/VSE                   00013900
R13      EQU   13                 REGISTER SAVE AREA                    00014000
R14      EQU   14                 RETURN REG                            00014100
R15      EQU   15                 ENTRY REG                             00014200
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
WILDCOMP HEADER VERSION='1.2',AUTHOR='DLC',DATE='(C)APR08',            X00014600
               BASE=R11           NO SAVE AREA NEEDED                   00014600
* ------------------------------------------------------------------- * 00014800
         L     R10,0(,R1)         LOAD DSECT BASE REGISTER              00016200
         IF    WILDCTL,(NE,CLC),PGMNAME
          ZAP  WILDRES,=PL2'11'   SIGNAL PARM AREA FORMAT ISSUES
          J    RETURN             GO RETURN TO CALLER
         ENDIF
         IF    WILDONE,NH,X'40'   IF NO ONE-CHAR MATCH SPECIFIED
          MVI  WILDONE,C'?'         SET DEFAULT TO A QUESTION MARK
         ENDIF
         IF    WILDALL,NH,X'40'   IF NO ALL-CHAR MATCH SPECIFIED
          MVI  WILDALL,C'*'         SET DEFAULT TO AN ASTERISK
         ENDIF
         IF    WILDBLNK,NH,X'40'  IF NO BLANK-CHAR MATCH SPECIFIED DLC1
          MVI  WILDBLNK,C'¬'        SET DEFAULT TO A NOT-SIGN      DLC1
         ENDIF                                                     DLC1
         IF    WILDONE,(NE,CLC),WILDALL
         AND   WILDONE,(NE,CLC),WILDBLNK                           DLC1
         AND   WILDALL,(NE,CLC),WILDBLNK                           DLC1
         ELSE
          ZAP  WILDRES,=PL2'12'   SIGNAL WILD CARD CHAR ISSUES
          J    RETURN             GO RETURN TO CALLER
         ENDIF
         LH    R3,WILDLEN         GET LENGTH OF COMPARE STRINGS
         IF    R3,(LT,CH),=H'1',OR,                                    +
               R3,(HT,CH),=YL2(L'WILDSTR1)
          ZAP  WILDRES,=PL2'13'   SIGNAL STRING LENGTH ISSUES
          J    RETURN             GO RETURN TO CALLER
         ENDIF
*
         ZAP   WILDRES,=PL2'0'    INITIALIZE RESULT VALUE               00016300
         LA    R1,WILDSTR1        COMPARE STR1 FROM LEFT-TO-RIGHT
         LA    R2,WILDSTR2           WITH STR2
*                  allow ending wild-card asterisk to be omitted   DLC0 00016400
         LA    R4,WILDSTR1+L'WILDSTR1-1     POINT TO END OF STR1   DLC0
         WHILE 0(R4),NH,X'40',AND,R4,(HT,CR),R1   LAST NON-BLANK   DLC0 00016500
          BCTR R4,0                                                DLC0 00016700
         WEND                                                      DLC0 00016800
         IF    R4,(EQ,CR),R1,AND,0(R4),EQ,X'40'   STR1 IS BLANK?   DLC0 00016500
          LA   R4,WILDSTR1+L'WILDSTR1-1     POINT TO END OF STR1   DLC0 00016700
         ENDIF                                                     DLC0 00016800
*                                                                       00016400
*DLC0    WHILE R3,(NZ,LTR),R3                                      DLC0 00016500
         WHILE R3,(NZ,LTR),R3,AND,R1,(NH,CR),R4                    DLC0 00016500
          IF   0(1,R1),(EQ,CLC),WILDALL                                 00016600
           WEXIT
          ENDIF
          IF   0(R2),EQ,X'40'                                      DLC1 00016600
           IF  0(1,R1),(NE,CLC),0(R2)                              DLC1 00016600
           AND 0(1,R1),(NE,CLC),WILDONE                            DLC1 00016600
           AND 0(1,R1),(NE,CLC),WILDBLNK                           DLC1 00016600
            ZAP WILDRES,=PL2'+1'   SIGNAL STR1 GT STR2             DLC1 00016300
            WEXIT                                                  DLC1
           ENDIF                                                   DLC1
          ELSE                                                     DLC1
           IF  0(1,R1),(NE,CLC),0(R2)                                   00016600
           AND 0(1,R1),(NE,CLC),WILDONE                                 00016600
            IF 0(1,R1),(LT,CLC),0(R2)                                   00016600
             ZAP WILDRES,=PL2'-1'  SIGNAL STR1 LT STR2                  00016300
            ELSE
             ZAP WILDRES,=PL2'+1'  SIGNAL STR1 GT STR2                  00016300
            ENDIF
            WEXIT
           ENDIF
          ENDIF                                                    DLC1
          LA   R1,1(,R1)
          LA   R2,1(,R2)
          BCTR R3,0                                                     00016700
         WEND                                                           00016800
* ------------------------------------------------------------------- * 00014800
RETURN   DS    0H
         XR    R15,R15            ZERO RETURN CODE
         TRAILER RETCODE=R15      RETURN TO CALLER
* =================================================================== * 00024100
         CORG  0,256
         EJECT                                                          00024200
DATA     LOCTR
* =================================================================== * 00024300
STORAGE  DS    0D                                                       00024400
* ------------------------------------------------------------------- * 00014800
         LTORG                                                          00024600
         CORG  0,256
* =================================================================== * 00024700
         END                                                            00024800
