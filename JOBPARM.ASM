JPM      TITLE 'JOBPARM  - SET PARTITION JOB PARMS TO SELECTED VALUES'
         PRINT ON,GEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*           J O B   P A R M   S E T T I N G   R O U T I N E           *
*                                                                     *
*                                                                     *
* DAPSCO Information Systems                          Written by:     *
* 3110 Kettering Blvd.                             Dave Leonard Clark *
* Dayton, Ohio  45439                                (937)294-5331    *
*                                                                     *
* No warranty is expressed  or  implied.   Neither the author nor the *
* company is responsible for  any  loss  or damage resulting from the *
* use of this code.  Source is provided on an as-is basis.            *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This  routine  allows  a   program   to  setup  partition  job *
* parameters without requiring JCL "// SETPARM" cards.  Although this *
* routine may be executed from JCL, it's greatest power is when it is *
* executed  as  a  subroutine   of   some  other  program  which  has *
* intelligently decided what the parameters should be.                *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      This routine  does  not  validate  passed  parameter  names or *
* values.   The invoking COBOL program  should use the following data *
* area for passing parameters:                                        *
*                                                                     *
*      01  JOBPARM-PARMS.                                             *
*          05  JOB-PLEN                PIC  9(03)   COMP VALUE ZEROS. *
*          05  JOB-PARM                PIC  X(100)  VALUE SPACES.     *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      The subroutine is invoked as follows:                          *
*                                                                     *
*     * SET PARTITION JOB PARAMETERS                                  *
*          MOVE total-length TO JOB-PLEN                              *
*          MOVE  'name=value,name=value,...' TO JOB-PARM              *
*          CALL  'JOBPARM'   USING JOBPARM-PARMS                      *
*                                                                     *
* The parm "name" and "value"  shown  above fall under the same rules *
* as the "SETPARM" JCL statement,  except the "value" may not contain *
* a comma and does not  have to be enclosed in quotes.   Furthermore, *
* you may be more  likely  to  use  the  STRING statement to fill the *
* parameter buffer rather than the MOVE statement.                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
JPMPARM  DSECT                    JOBPARM PARMS DUMMY SECTION
         USING *,R10              ESTABLISH REG 10 AS BASE
* =================================================================== *
JPMLENG  DS    H                  LENGTH OF REQUEST JOB PARM
JPMREQU  DS    CL100              REQUEST JOB PARM
* =================================================================== *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D
PARMAREA DS    3D
DATAAREA DS    CL278
DATALENG EQU   *-DATAAREA
         DS    CL2
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES                                              *
* =================================================================== *
R0       EQU   0                  PARM REG
R1       EQU   1                  PARM REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10                 JPMPARM DSECT BASE REG
R11      EQU   11                 JOBPARM CSECT BASE REG
R12      EQU   12                 LE RESERVED REG
R13      EQU   13                 REGISTER SAVEAREA
R14      EQU   14                 RETURN REG
R15      EQU   15                 ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF
         COPY  WHILE
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
JOBPARM  HEADER VERSION='1.1',AUTHOR='DLC',DATE='(C)DEC91',            X
               BASE=R11,SVAREA=SALEN
* =================================================================== *
         USING DYNASTOR,R13
*
         L     R10,0(,R1)         GET ADDRESS OF PARAMETER
         IF    R1,(EQ,CR),R12,OR,JPMLENG,(EQ,CLC),=H'0'
          LA   R15,8               SET "NOPARM" RETURN CODE
         ELSE                     ELSE
*
* INITIALIZE DYNAMIC STORAGE
         XC    PARMAREA(3*L'PARMAREA),PARMAREA
         MVI   DATAAREA,C' '
         MVC   DATAAREA+1(256),DATAAREA
         MVC   DATAAREA+256(L'DATAAREA-256),DATAAREA
*
         LA    R0,DATALENG        GET LENGTH OF DATA AREA
         LA    R1,DATAAREA        GET ADDRESS OF DATA AREA
         LH    R2,JPMLENG         GET LENGTH OF PARM STRING
         LA    R3,JPMREQU         GET ADDRESS OF PARM STRING
*
         WHILE R0,(NL,CH),=H'11',AND,R2,(P,LTR),R2
*
         LR    R5,R3              SET STARTING ADDRESS OF PARM NAME
         WHILE 0(R3),NE,C'=',AND,R2,(P,LTR),R2
          BCTR R2,0               DECREMENT LENGTH VALUE
          LA   R3,1(,R3)          INCREMENT ADDRESS VALUE
         WEND
         LR    R4,R3              CALCULATE LENGTH AS DELIMITER ADDRESS
         SR    R4,R5                             MINUS STARTING ADDRESS
*
         IF    R2,(P,LTR),R2
          BCTR  R2,0              DECREMENT LENGTH VALUE FOR DELIMITER
          LA    R3,1(,R3)         INCREMENT ADDRESS VALUE FOR DELIMITER
         ENDIF
*
         LR    R7,R3              SET STARTING ADDRESS OF PARM DATA
         WHILE 0(R3),NE,C',',AND,R2,(P,LTR),R2
          BCTR R2,0               DECREMENT LENGTH VALUE
          LA   R3,1(,R3)          INCREMENT ADDRESS VALUE
         WEND
         LR    R6,R3              CALCULATE LENGTH AS DELIMITER ADDRESS
         SR    R6,R7                             MINUS STARTING ADDRESS
*
         IF    R2,(P,LTR),R2
          BCTR  R2,0              DECREMENT LENGTH VALUE FOR DELIMITER
          LA    R3,1(,R3)         INCREMENT ADDRESS VALUE FOR DELIMITER
         ENDIF
*
         SH    R0,=H'10'          SUBTRACT HEADER LENGTH FROM DATA AREA
         IF    R0,(M,SR),R6       IF PARM DATA WON'T FIT IN DATA AREA
          WEXIT                    THEN GO WITH THE PARMS WE HAVE
MVCNAME  MVC   3(0,R1),0(R5)      (SEE "EXECUTE" INSTRUCTION BELOW)
MVCDATA  MVC   10(0,R1),0(R7)     (SEE "EXECUTE" INSTRUCTION BELOW)
         ENDIF
*
         STCM  R6,B'0011',0(R1)   PUT LENGTH OF DATA IN BUFFER
         XC    2(1,R1),2(R1)      SET INDICATOR BYTE IN BUFFER
         BCTR  R4,0               DECREMENT FOR EXECUTE
         EX    R4,MVCNAME         SET PARM NAME IN BUFFER
         BCTR  R6,0               DECREMENT FOR EXECUTE
         EX    R6,MVCDATA         SET PARM DATA IN BUFFER
*
         AH    R1,=H'11'          ADD LENGTH OF HEADER + 1
         AR    R1,R6              ADD LENGTH OF PARM DATA - 1
*
         WEND
*
         MVC   0(2,R1),=X'FFFF'   FLAG THE END OF THE BUFFER
*
         LOAD  $IJBPROC,SYS=YES   LOCATE SYSTEM ROUTINE
         LR    R15,R1             GET ENTRY POINT ADDRESS
         LA    R1,PARMAREA        GET ADDRESS OF PARM AREA
         MVI   0(R1),8            SELECT "SETPAR" FUNCTION
         LA    R0,DATAAREA        GET ADDRESS OF DATA AREA
         ST    R0,16(R1)          PUT IN PARM AREA FOR ADDRESSABILITY
         SR    R0,R0              CLEAR PARM REG
         BALR  R14,R15            INVOKE SYSTEM ROUTINE
*
         ENDIF                    ENDIF
* ------------------------------------------------------------------- *
FINISH   DS    0H
* ------------------------------------------------------------------- *
         TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
SALEN    DC    A(DYNALENG)
         LTORG
* =================================================================== *
         END
