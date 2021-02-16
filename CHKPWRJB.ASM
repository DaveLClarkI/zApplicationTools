CHK      TITLE 'CHKPWRJB - CHECK POWER JOB STATUS'                              
         PRINT ON,GEN                                                           
CHKPWRJB AMODE 31                                                               
CHKPWRJB RMODE ANY                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*             C H E C K   P O W E R   J O B   S T A T U S             *         
*                                                                     *         
*                                                                     *         
* Winsupply Group Services                            Written by:     *         
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
*      This subroutine allows a batch  or online program to determine *         
* the status of a  VSE/POWER  job  in another partition.   The caller *         
* does not need to know the partition.  Alternatively, the caller can *         
* ask what VSE/POWER job is active in a particular partition -- i.e., *         
* the caller does not  need  to  know  the  other VSE/POWER job name. *         
* However,  the  caller  must  supply  either  a  partition  id  or a *         
* VSE/POWER job name to check  on.   In addition to the status,  this *         
* subroutine can return either the  partition id or the VSE/POWER job *         
* name along with the current program  step executing for that job -- *         
* which might be "NO NAME".                                           *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                           IMPLEMENTATION                            *         
*                                                                     *         
*      If supplied,  this subroutine  will  validate any partition id *         
* specified.   The invoking COBOL  program  should  use the following *         
* data area for passing parameters:                                   *         
*                                                                     *         
*      01  CHKPWRJB-PARMS.                                            *         
*        03  CHK-STATUS                PIC  X(02)   VALUE SPACES.     *         
*        03  CHK-PARTID                PIC  X(02)   VALUE SPACES.     *         
*        03  CHK-PWRJOB                PIC  X(08)   VALUE SPACES.     *         
*        03  CHK-CURPGM                PIC  X(08)   VALUE SPACES.     *         
*        03  CHK-RESRVD                PIC  X(108)  VALUE LOW-VALUES. *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                             INVOCATION                              *         
*                                                                     *         
*      The subroutine is invoked in one of three ways:                *         
*                                                                     *         
*     * check a particular job's status in a particular partition     *         
*          MOVE 'F7'          TO CHK-PARTID                           *         
*          MOVE 'WEBACKUP'    TO CHK-PWRJOB                           *         
*          CALL 'CHKPWRJB' USING CHKPWRJB-PARMS                       *         
*                                                                     *         
*     * check a particular job's status in any partition              *         
*          MOVE SPACES        TO CHK-PARTID                           *         
*          MOVE 'WEBACKUP'    TO CHK-PWRJOB                           *         
*          CALL 'CHKPWRJB' USING CHKPWRJB-PARMS                       *         
*                                                                     *         
*     * check a particular partition for an active job                *         
*          MOVE 'F7'          TO CHK-PARTID                           *         
*          MOVE SPACES        TO CHK-PWRJOB                           *         
*          CALL 'CHKPWRJB' USING CHKPWRJB-PARMS                       *         
*                                                                     *         
*                                                                     *         
*      On return, if no errors:                                       * 00014200
*                                                                     *         
* CHK-STATUS   will be 'A' for active or 'I' for inactive;            * 00014300
* CHK-PARTID   will reflect the VSE/POWER job's partition if active   * 00014400
*                and the partition id not supplied in the call;       * 00014500
* CHK-PWRJOB   will reflect the VSE/POWER job's name if partition is  * 00014600
*                active and the job name not supplied in the call;    * 00014700
* CHK-CURPGM   will reflect the current job step if a job is active.  * 00014800
*                                                                     * 00014900
*      Otherwise, if errors:                                          * 00015000
*                                                                     * 00015100
* CHK-STATUS   will be 'EI' for a partition id error                  * 00014300
*                              or partition id not allocated,         * 00014300
*                   or 'EJ' for a missing job name parameter in       * 00014300
*                              the case that PARTID not supplied.     * 00014300
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
* =================================================================== *         
CHKPARMS DSECT                    CHKPWRJB PARMS DUMMY SECTION                  
* =================================================================== *         
CHKSTAT  DS    CL2                STATUS                                        
CHKPID   DS    CL2                PARTITION ID                                  
CHKJOB   DS    CL8                POWER JOB NAME                                
CHKPGM   DS    CL8                CURRENT PROGRAM NAME                          
         DS    CL4                FOR DOUBLEWORD ALIGNMENT                      
CHKSAVE  DS    9D                 SAVE AREA                                     
CHKDATA  DS    CL32               DATA AREA                                     
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
*        REGISTER USAGES                                              *         
* =================================================================== *         
R0       EQU   0                  PARM REG                                      
R1       EQU   1                  PARM REG OR POWJB                             
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                  PIK                                           
R5       EQU   5                  PCB/PCE                                       
R6       EQU   6                  COMREG                                        
R7       EQU   7                  OTHER COMREG                                  
R8       EQU   8                  PCBATAB                                       
R9       EQU   9                                                                
R10      EQU   10                 CHKPARM DSECT BASE REG                        
R11      EQU   11                 CHKPWRJB CSECT BASE REG                       
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
         COPY  IF                      STRUCTURED CONSTRUCT                     
         COPY  WHILE                   STRUCTURED CONSTRUCT                     
         PRINT ON                                                               
*                                                                               
*        SGLOWC DSECT=YES              LOW CORE LAYOUT                          
         MAPPCB ,                      PCB LAYOUT                               
         MAPCOMR ,                     COMREG LAYOUT                            
         MAPPOWJB ,                    POWER JOB INFORMATION                    
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
CHKPWRJB HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)FEB21',            X        
               BASE=R11                                                         
* =================================================================== *         
         L     R10,0(,R1)         GET ADDRESS OF PARAMETER                      
         USING CHKPARMS,R10       ADDRESSABILITY FOR PARAMETER                  
*                                                                               
         LA    R15,CHKSAVE        LOAD ADDRESS OF MY SAVEAREA                   
         ST    R15,8(,R13)        SET NEXT SAVEAREA IN CALLER'S                 
         ST    R13,4(,R15)        SET PREV SAVEAREA IN MINE                     
         LR    R13,R15            GET ADDRESS OF MY SAVEAREA                    
         XR    R15,R15            CLEAR RETURN CODE REGISTER                    
*                                                                               
* INITIALIZE PASSED STORAGE                                                     
         XC    CHKDATA,CHKDATA                                                  
*                                                                               
* VALIDATE PARAMETERS                                                           
         IF    CHKPID,(NH,CLC),BLANKS                                           
         AND   CHKJOB,(NH,CLC),BLANKS                                           
          MVC  CHKSTAT,=CL2'EJ'   MUST HAVE ONE OR THE OTHER                    
          B    FINISH             GO RETURN TO CALLER                           
         ENDIF                                                                  
*                                                                               
         IF    CHKPID,(HT,CLC),BLANKS  SPECIFIC PARTITION CHECK?                
          GETFLD FIELD=PIK,LOGID=CHKPID,RETURN=YES                              
          IF   R15,(NZ,LTR),R15   IF NON-ZERO RETURN CODE                       
           MVC CHKSTAT,=CL2'EI'   INVALID PARTITION ID SPECIFIED                
           B   FINISH             GO RETURN TO CALLER                           
          ENDIF                                                                 
          LR   R4,R1              SAVE PIK                                      
          MVC  CHKSTAT,=CL2'I '   JOB IS INACTIVE BY DEFAULT                    
          GETFLD FIELD=PCBPTR,PART=(4),RETURN=YES                               
          IF   R15,(NZ,LTR),R15,                                       X        
               BC,FINISH          PARTITION NOT ACTIVE                          
          ENDIF                                                                 
          LR   R5,R1              GET ADDRESS OF PCB/PCE                        
          USING PCBADR,R5         ADDRESSABILITY FOR PCB/PCE                    
          L    R6,PCECOMRA        GET ADDRESS OF COMREG                         
          USING COMREG,R6         ADDRESSABILITY FOR COMREG                     
          L    R1,PCEPOWJB        GET ADDRESS OF POWJB INFO                     
          USING PJBADR,R1         ADDRESSABILITY FOR POWJB INFO                 
          IF   PJBPNAME,(NH,CLC),BLANKS,                               X        
               BC,FINISH          NO JOB ACTIVE                                 
          ENDIF                                                                 
          IF   CHKJOB,(HT,CLC),BLANKS                                           
          AND  PJBPNAME,(NE,CLC),CHKJOB                                         
           B   FINISH             JOB NOT ACTIVE                                
          ENDIF                                                                 
          MVC  CHKSTAT,=CL2'A '   JOB IS ACTIVE                                 
          MVC  CHKJOB,PJBPNAME    CAPTURE POWER JOB NAME                        
          MVC  CHKPGM,IJBPHNAM    CAPTURE PROGRAM NAME                          
          DROP R5,R6,R1                                                         
          B    FINISH             GO RETURN TO CALLER                           
         ENDIF                                                                  
*                                                                               
* PREPARE TO SEARCH ALL PARTITIONS                                              
         XR    R0,R0              POINT TO LOW CORE                             
*        L     R7,CRADDR          (FROM SGLOWC MACRO)                           
         L     R7,x'14'           GET MY COMREG ADDRESS FROM LOW CORE           
*        L     R8,APCBATAB        (FROM SGLOWC MACRO)                           
         L     R8,x'2C4'          GET ADDRESS OF PCBATAB FROM LOW CORE          
         LA    R8,4(,R8)          SKIP SYSTEM PCB ENTRY                         
*                                                                               
         MVC   CHKSTAT,=CL2'I '   JOB IS INACTIVE BY DEFAULT                    
*                                                                               
         WHILE =F'-1',(NE,CLC),0(R8) IF NOT END OF TABLE                        
          IF   =F'0',(NE,CLC),0(R8) IF NOT INACTIVE ENTRY                       
           L   R5,0(R8)           GET ADDRESS OF PCB/PCE                        
           USING PCBADR,R5        ADDRESSABILITY FOR PCB/PCE                    
           L   R6,PCECOMRA        GET ADDRESS OF COMREG                         
           USING COMREG,R6        ADDRESSABILITY FOR COMREG                     
           IF  R6,(NE,CR),R7      IF NOT MY COMREG                              
            L  R1,PCEPOWJB        GET ADDRESS OF POWJB INFO                     
            USING PJBADR,R1       ADDRESSABILITY FOR POWJB INFO                 
            IF PJBPNAME,(EQ,CLC),CHKJOB                                         
             MVC CHKSTAT,=CL2'A ' JOB IS ACTIVE                                 
             MVC CHKPID,PCELID    CAPTURE PARTITION ID                          
             MVC CHKPGM,IJBPHNAM  CAPTURE PROGRAM NAME                          
             WEXIT                EXIT LOOP                                     
            ENDIF                                                               
           ENDIF                                                                
           DROP R5,R6,R1                                                        
          ENDIF                                                                 
          LA   R8,4(,R8)          POINT TO NEXT PCBATAB ENTRY                   
         WEND                     LOOP UNTIL END OF TABLE                       
* ------------------------------------------------------------------- *         
FINISH   DS    0H                                                               
         TRAILER RETCODE=R15,SVAREA=CHKSAVE                                     
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
STORAGE  DS    0D                                                               
* =================================================================== *         
BLANKS   DC    CL8' '                                                           
         LTORG                                                                  
* =================================================================== *         
         END                                                                    
