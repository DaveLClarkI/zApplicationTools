3270     TITLE 'RX3270AP - 3270 Channel Appendage Routine (non-SNA)'
         PRINT ON,GEN
RX3270AP AMODE 24
RX3270AP RMODE 24
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Dapsco Information Systems                          Written by:     *
* 3110 Kettering Boulevard                         Dave Leonard Clark *
* Dayton, OH  45439                                  (937)294-5331    *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* No warranty is expressed  or  implied.   Neither the author nor the *
* company is responsible for  any  loss  or damage resulting from the *
* use of this code.  Source is provided on an as-is basis.            *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
         MAPCCB
* =================================================================== *
         EJECT
* =================================================================== *
         MAPPUB
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* =================================================================== *
R0       EQU   0
R1       EQU   1                       CCB ADDR
R2       EQU   2                       CHANNEL AND DEVICE NO.
R3       EQU   3                       PUB ADDR
R4       EQU   4                       CHANNEL QUE ENTRY ADDR
R5       EQU   5                       LUBID ENTRY ADDR
R6       EQU   6                       WORK REG
R7       EQU   7                       RETURN TO SUPERVISOR
R8       EQU   8                       BASE FOR APPENDAGE ROUTINE
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
RX3270AP CSECT 0
         USING *,R8
         USING CCBADR,R1
         USING PUBADR,R3
* =================================================================== *
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
*
         ST    R6,SUPVR6                    SAVE SUPVR. WORK REG.       00001430
         XR    R6,R6                        CLEAR WORK REGISTER
         ICM   R6,B'0111',X'41'(R0)         GET PENDING CCW ADDR.
*
         SELECT                             SELECT CONDITION
         WHEN  x'44'(R0),(ON,TM),x'80'      WHEN INTERRUPT IS ATTN
          MVC  x'44'(2,R0),=x'0C00'          SET CE+DE AND DEQUE
         WHEN  X'44'(R0),EQ,X'08'           WHEN INTERRUPT IS CH END
          NOPR 0                             DO NOTHING AND DEQUE
         WHEN  X'44'(R0),EQ,X'04',AND,      WHEN INTERRUPT IS DEVEND   *
               R6,(Z,LTR),R6                AND NO PENDING CCW
          MVC  x'44'(2,R0),=x'0600'          SET DE+UC AND DEQUE
         WHEN  x'44'(R0),EQ,x'0C',AND,      WHEN INTERRUPT IS CE+DE    *
               R6,(Z,LTR),R6                AND NO PENDING CCW
          MVC  x'44'(2,R0),=x'0F00'          SET CE+DE+UC+UE AND DEQUE
         WHEN  OTHER                        WHEN CE+DE OR JUST DE
*                                           AND HAS PENDING CCW
          SL   R6,=F'8'                      GET PREV. CCW ADDR.        00001438
          IF   0(R6),EQ,x'03'                IF CCW IS MY NOOP
           IF  0(R1),(NOT,TM),x'80'           IF CCB NOT FLAGGED
            MVC x'44'(2,R0),=x'0080'           CHANGE TO PCI
            OI 0(R1),x'80'                     FLAG THE CCB
           ELSE                               ELSE
            NI 0(R1),x'FF'-x'80'               REMOVE CCB FLAG
           ENDIF                              ENDIF
          ENDIF                              ENDIF
*
         ENDSL                              END SELECT
*
         L     R6,SUPVR6                    RESTORE SUPVR. WORK REG.    00001444
         B     4(,R7)                       DEQUEUE AND CONTINUE
*
         POP   ACONTROL
* ------------------------------------------------------------------- *
SUPVR6   DS    F                                                        00001446
         LTORG
* =================================================================== *
         END   RX3270AP
