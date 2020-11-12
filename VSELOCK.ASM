LOCK     TITLE 'VSELOCK  - MANAGE A VSE BATCH RESOURCE LOCK'                    
         PRINT ON,GEN                                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* WinWholesale Group Services                         Written by:     *         
* 3110 Kettering Boulevard                         Dave Leonard Clark *         
* Dayton, OH  45439                                  (937)294-5331    *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                               PURPOSE                               *         
*                                                                     *         
*      This assembler  subroutine  allows  a  user-written process to *         
* manage a batch resource locking  mechanism using the VSE Lock File. *         
* Note that this subroutine's parameter  list is set up to facilitate *         
* ease of invoking this  program from any standard-linkage interface; *         
* including REXX and directly  from  VSE  batch  JCL as a stand-alone *         
* program.   This is accomplished by  using  a single parameter block *         
* and supporting either a zoned or binary wait time specification.    *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                           IMPLEMENTATION                            *         
*                                                                     *         
*      Assemble this  program  as  either  an  object module,  a load *         
* module, or both -- depending upon your needs for either a static or *         
* a  dynamic  CALL  or  direct  EXEC  from  batch  JCL.   The calling *         
* assembler program may use  the  same  parameter  area DSECT used by *         
* this  subroutine.    The  calling  COBOL  program  should  use  the *         
* following data area for passing parameters:                         *         
*                                                                     *         
*      01  VSELOCK-AREA.                                              *         
*        03  VSELOCK                   PIC  X(8)    VALUE 'VSELOCK '. *         
*          88  VSELOCK-PGMNAME                      VALUE 'VSELOCK '. *         
*        03  VSELOCK-PARMS.                                           *         
*          05  LOCK-REMLENG            PIC S9(4)    BINARY VALUE +16. *         
*          05  LOCK-REQU               PIC  X(1)    VALUE 'A'.        *         
*            88  LOCK-ALLOCATE                      VALUE 'A'.        *         
*            88  LOCK-RELEASE                       VALUE 'R'.        *         
*          05  LOCK-TIMEOUT            PIC  9(3)    VALUE ZEROES.     *         
*          05                          REDEFINES    LOCK-TIMEOUT.     *         
*            07  LOCK-BINTYPE          PIC  X(1).                     *         
*              88  LOCK-BINTYPE-WAIT                VALUE LOW-VALUE.  *         
*            07  LOCK-BINWAIT          PIC S9(4)    BINARY.           *         
*          05  LOCK-RESNAME            PIC  X(12)   VALUE SPACES.     *         
*                                                                     *         
*      The following REXX code  demonstrates  two  ways to set up the *         
* parameter block for acquiring a resource  lock -- one using a zoned *         
* wait time and the other using a  binary wait time -- and one way to *         
* set up the parameter block for releasing a resource lock.           *         
*                                                                     *         
* lockparm = 'A',                      /* acquire lock */             *         
*          ||Right(120,3,'0'),         /* wait 2 minutes  */          *         
*          ||Left('TESTLOCK',12,' ');                                 *         
* ...or...                                                            *         
* lockparm = 'A',                      /* acquire lock */             *         
*          ||'00'x||D2C(1200,2),       /* wait 20 minutes */          *         
*          ||Left('TESTLOCK',12,' ');                                 *         
* ...or...                                                            *         
* lockparm = 'R',                      /* release lock */             *         
*          ||'000',                                                   *         
*          ||Left('TESTLOCK',12,' ');                                 *         
*                                                                     *         
* Address LINK "VSELOCK" lockparm;                                    *         
* If rc <> 0 Then Do                                                  *         
*   If rc = 10 Then Do                                                *         
*     Say 'VSELOCK timeout';                                          *         
*     Exit rc;                                                        *         
*   End;                                                              *         
*   Say 'VSELOCK rc='rc;                                              *         
*   Exit 12;                                                          *         
* End;                                                                *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                          BATCH INVOCATION                           *         
*                                                                     *         
*      Calling this program is  not a complicated thing.   An example *         
* of calling this program via JCL EXEC is probably in order,  though. *         
* Note that the lock resource name must be padded with blanks.        *         
*                                                                     *         
* // JOB    VSELOCK   MANAGE A VSE BATCH RESOURCE LOCK                *         
* // EXEC   PGM=VSELOCK,PARM='A030TESTLOCK    '                       *         
* /* EOD                                                              *         
* // IF     $RC > 0 THEN                                              *         
* // GOTO   EXIT                                                      *         
* // PAUSE  REPLY EOB TO RELEASE THE LOCK                             *         
* // EXEC   PGM=VSELOCK,PARM='R000TESTLOCK    '                       *         
* /* EOD                                                              *         
* /. EXIT                                                             *         
* /& EOJ                                                              *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                                INPUT                                *         
*                                                                     *         
*      On input, the parameter block must contain these values:       *         
*                                                                     *         
*   PARMLEN    is the length of the remainder of the parameter block. *         
*                This should be 16 bytes.  This field is included     *         
*                because it is present in the JCL EXEC PARM area and  *         
*                when invoked from REXX via Address LINK.             *         
*                                                                     *         
*   LOCKOPT    is the action to take on the lock resource specified.  *         
*                Where: 'A' will attempt to acquire the lock; and     *         
*                       'R' will release the lock.                    *         
*                                                                     *         
*   MAXWAIT    is an unsigned 3-digit zoned number of seconds to wait *         
*                while attempting to acquire a resource lock.         *         
*                                                                     *         
*   MAXWAITO   is an option to indicate that a binary wait time is    *         
*   MAXWAITB     specified.  This byte should be x'00' (LOW-VALUE in  *         
*                COBOL) to indicate that the next field (MAXWAITB)    *         
*                contains this binary wait time.  Note that these two *         
*                fields redefine the same storage as MAXWAIT.         *         
*                                                                     *         
*   RESNAME    is the 12-byte resource name associated with the lock. *         
*                The resource name  must  start  with  a  letter or a *         
*                national character;  but may not start with 'A' thru *         
*                'I' or the  letter  'V',  as  IBM has reserved these *         
*                letters for its own use.                             *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                               OUTPUT                                *         
*                                                                     *         
*      On output,  just be aware  that  this program does convert any *         
* zoned wait time value to a binary wait time value for internal use. *         
* Otherwise,  the following are  the  return  codes from this program *         
* (other return codes are from either the LOCK or UNLOCK macros):     *         
*                                                                     *         
*    0   The requested lock action was successful.                    *         
*   10   A timeout occurred while attempting to acquire the lock.     *         
*   91   PARMLEN does not contain a binary half-word value of 16.     *         
*   92   LOCKOPT is not either an 'A' or an 'R' value.                *         
*   93   MAXWAIT is not unsigned zoned no. or MAXWAITB is negative.   *         
*   94   RESNAME does not start with an acceptable value.             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
         COPY  VSELOCK                                                          
         EJECT                                                                  
* =================================================================== *         
DYNASTOR DSECT                                                                  
         USING *,R13                                                            
* =================================================================== *         
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA              
PWORK8   DS    PL8                     PACKED DOUBLE-WORD                       
*                                                                               
MYTECB   TECB                                                                   
MYLOCK   DTL   NAME=RESOURCENAME,CONTROL=E,LOCKOPT=1,                  X        
               KEEP=YES,OWNER=PARTITION,SCOPE=EXT                               
MYLOCKL  EQU   *-MYLOCK                                                         
*                                                                               
DYNALENG EQU   *-DYNASTOR              LENGTH OF DYNAMIC STORAGE                
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
*        REGISTER USAGES                                                        
* =================================================================== *         
R0       EQU   0                                                                
R1       EQU   1                       ENTRY PARMS REG                          
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                       VSELOCK PARMS BASE REG                   
R10      EQU   10                      VSELOCK CSECT BASE REG                   
R11      EQU   11                                                               
R12      EQU   12                                                               
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
VSELOCK  HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)AUG09',            X        
               BASE=R10,SVAREA=SALEN                                            
* =================================================================== *         
         L     R9,0(,R1)               GET ADDR OF PARM AREA                    
*                                                                               
* VALIDATE PARM AREA ENTRIES                                                    
*                                                                               
         LA    R15,91                  SET POSSIBLE ERROR CODE                  
         IF    PARMLEN,(NE,CLC),=H'16', IF REMAINING PARMAREA TOO SHORTX        
               BC,RETR15                ERROR CODE IN REG 15                    
         ENDIF                         ENDIF                                    
*                                                                               
         LA    R15,93                  SET POSSIBLE ERROR CODE                  
         IF    MAXWAITO,EQ,X'00'       IF BINARY WAIT TIME SUPPLIED             
          IF   MAXWAITB,(ON,TM),X'80',  IF NEGATIVE, RETURN ERROR      X        
               BC,RETR15                 ERROR CODE IN REG 15                   
          ENDIF                         ENDIF                                   
         ELSE                          ELSE ZONED WAIT TIME SUPPLIED            
          MVC  PWORK8(3),=C'000'        SET MASK VALUE                          
          MVZ  PWORK8(3),MAXWAIT        CHECK ZONES OF WAIT TIME VALUE          
          IF   =C'000',(NE,CLC),PWORK8, IF DIFFERENT, RETURN ERROR     X        
               BC,RETR15                 ERROR CODE IN REG 15                   
          ENDIF                         ENDIF                                   
          PACK PWORK8,MAXWAIT           CHECK DIGITS OF WAIT TIME VALUE         
          IF   PWORK8,(NZ,TP),$NOOP,    IF NOT VALID, RETURN ERROR     X        
               BC,RETR15                 ERROR CODE IN REG 15                   
          ENDIF                         ENDIF                                   
          CVB  R3,PWORK8                CONVERT IT TO A BINARY VALUE            
          STCM R3,B'0111',MAXWAIT       STORE FILL BYTE + HALF-WORD             
         ENDIF                         ENDIF                                    
*                                                                               
         LA    R15,94                  SET POSSIBLE ERROR CODE                  
         IF    RESNAME(1),(NZ,TRT),NJS, IF NOT VALID FIRST CHARACTER   X        
               BC,RETR15                ERROR CODE IN REG 15                    
         ENDIF                         ENDIF                                    
*                                                                               
* BEGIN PROCESSING...                                                           
*                                                                               
         XC    MYLOCK(MYLOCKL),MYLOCK  INITIALIZE THE LOCK STORAGE              
         GENDTL ADDR=(S,MYLOCK),CONTROL=E,LOCKOPT=1,   SETUP THE       X        
               KEEP=YES,OWNER=PARTITION,SCOPE=EXT      LOCK TYPE                
         MODDTL ADDR=(S,MYLOCK),NAME=(S,RESNAME)       NAME THE LOCK            
*                                                                               
         SELECT                                                                 
*                                                                               
         CASE  LOCKOPT,EQ,C'A'         WHEN 'ACQUIRE'                           
* ...ACQUIRE A RESOURCE LOCK                                                    
          LH   R3,MAXWAITB              GET BINARY WAIT COUNT                   
          LA   R3,1(,R3)                CHANGE ZERO-BASED TO 1-BASED            
          WHILE R3,(NZ,LTR),R3          LOOP ON WAIT COUNT                      
           LOCK (S,MYLOCK),FAIL=RETURN   TRY TO LOCK THE RESOURCE               
           IF  R15,(Z,LTR),R15           IF SUCCESSFUL                          
            WEXIT                         EXIT THE LOOP                         
           ENDIF                         ENDIF                                  
           IF  R15,(NE,CH),=H'4',BC,RETR15                                      
           ENDIF                         ENDIF                                  
           BCTR R3,0                     DECREMENT WAIT COUNT                   
           IF  R3,(Z,LTR),R3             IF WAIT TIMEOUT                        
            LA R15,10                     SET RETURN CODE                       
            B  RETR15                     RETURN TO CALLER                      
           ENDIF                         ENDIF                                  
           XR  R0,R0                     CLEAR PARM REG                         
           LA  R1,1                      SET 1-SECOND TIMER                     
           LA  R2,MYTECB                 SET ADDRESS OF TECB                    
           SETIME (1),(2)                SETUP TIMER                            
           WAIT (1)                      WAIT FOR TIMER INTERRUPT               
          WEND                          END LOOP                                
*                                                                               
         CASE  LOCKOPT,EQ,C'R'         WHEN 'RELEASE'                           
* ...RELEASE A RESOURCE LOCK                                                    
          UNLOCK (S,MYLOCK)             UNLOCK THE RESOURCE                     
          IF   R15,(NZ,LTR),R15,BC,RETR15                                       
          ENDIF                         ENDIF                                   
*                                                                               
         CASE  ELSE                    WHEN OTHER                               
* ...ELSE INVALID OPTION                                                        
          LA   R15,92                   SET ERROR CODE                          
          B    RETR15                   RETURN ERROR                            
*                                                                               
         ENDSL                         END-SELECT                               
*                                                                               
* RETURN RESULT                                                                 
*                                                                               
RETURN   XR    R15,R15                 CLEAR REG15 FOR ZERO RETCODE             
RETR15   TRAILER RETCODE=R15,SVAREA=SALEN                                       
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
STORAGE  DS    0D                                                               
* =================================================================== *         
NJS      DC    256X'FF'                                                         
         ORG   NJS+C'$'                                                         
         DC    X'00'                   ALLOW THE                                
         ORG   NJS+C'#'                                                         
         DC    X'00'                   NATIONAL                                 
         ORG   NJS+C'@'                                                         
         DC    X'00'                   CHARACTERS                               
         ORG   NJS+C'J'                                                         
         DC    X'000000000000000000'   ALLOW 'J' THRU 'R'                       
         ORG   NJS+C'S'                                                         
         DC    X'000000FF00000000'     ALLOW 'S' THRU 'Z' EXCEPT 'V'            
         ORG                                                                    
SALEN    DC    A(DYNALENG)                                                      
         LTORG                                                                  
* =================================================================== *         
         END                                                                    
