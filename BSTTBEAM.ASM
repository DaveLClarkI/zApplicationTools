BEAM     TITLE 'BSTTBEAM - BIM-EDIT ACCESS METHOD DRIVER FOR BSI FTP'           
         PRINT ON,GEN                                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* WinWholesale Group Services                        Written by:      *         
* 3110 Kettering Boulevard                        Dave Leonard Clark  *         
* Dayton, OH  45439                                 (937)294-5331     *         
*                                                                     *         
*      No warranty is expressed  or implied.   Neither the author nor *         
* the company is responsible  for  any  loss or damage resulting from *         
* the use of this code.  Source is provided on an as-is basis.        *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*                               PURPOSE                               *         
*                                                                     *         
*      This program provides BIM-EDIT application programming inter-  *         
* face (API) support for the Barnard Software (BSI) FTP server and    *         
* batch FTP client.  In addition to simple member I/O (RETR & STOR),  *         
* with the potential for creating new members during OUTPUT           *         
* processing, this driver also provides support for directory         *         
* listings (LIST), creation (MKD) and deletion (RMD) of libraries,    *         
* rename (RNFR & RNTO) and delete (DELE) of members, and remote       *         
* execution (SITE BIMCMD) of other BIM-EDIT commands and procedures.  *         
* Security is based on normal BIM-EDIT LOGON processing.              *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*                           IMPLEMENTATION                            *         
*                                                                     *         
*      Assemble this program  as  a  standard  assembler batch phase. *         
* Note that the BIM-EDIT API  (BIUAPMD) must be SVA-resident in order *         
* for this interface to function.                                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*                             INVOCATION                              *         
*                                                                     *         
*      The following is  an  example  of  the  command  format an FTP *         
* client would use for  the  Structure Mount (SMNT) command supported *         
* by the BSI FTP Server.                                              *         
*                                                                     *         
*   QUOTE SMNT BSTTBEAM applid usrid pswd                             *         
*                                                                     *         
*      The following is an example of using the BSI batch FTP client  *         
* for simple I/O using this BIM-EDIT Access Method driver.            *         
*                                                                     *         
*   // JOB    BSTTFTPC   BSI FTP CLIENT JOB                           *         
*   // LIBDEF *,SEARCH=(PRD2.CONFIG,ESP.BSI),TEMP                     *         
*   // EXEC   PGM=BSTTFTPC,SIZE=(BSTTFTPC,512K)                       *         
*   ID 00                                                             *         
*   OPEN server                                                       *         
*   USER ftpuser                                                      *         
*   PASS ftppwrd                                                      *         
*   SYST                                                              *         
*   CWD  \folder\subfolder\                                           *         
*   TYPE A                                                            *         
*   CRLF ON                                                           *         
*   *                                                                 *         
*   * Define BIM-EDIT INPUT and Store data at the FTP Server          *         
*   *                                                                 *         
*   INPUT EXIT BSTTBEAM applid usrid pswd library member              *         
*   STOR filename.ext                                                 *         
*   *                                                                 *         
*   * Define BIM-EDIT OUTPUT and Retrieve data via the FTP Server     *         
*   *                                                                 *         
*   OUTPUT EXIT BSTTBEAM applid usrid pswd library member type        *         
*   RETR filename.ext                                                 *         
*   *                                                                 *         
*   QUIT                                                              *         
*   /* EOD                                                            *         
*   /& EOJ                                                            *         
*                                                                     *         
* Where:                                                              *         
*   applid  - specifies the BIM-EDIT VTAM applid (this may be an      *         
*             asterisk and, in this case, the BIM-EDIT installation   *         
*             default of "BIMEDIT" is used);                          *         
*   usrid   - is the BIM-EDIT user's id for LOGON processing;         *         
*   pswrd   - is the BIM-EDIT user's password for LOGON processing;   *         
*   library - is the BIM-EDIT origin or destination library name to   *         
*             use for subsequent I/O processing (this may be an       *         
*             asterisk if the specified user has a defined "home"     *         
*             library and this "home" library is to be used for FTP   *         
*             processing);                                            *         
*   member  - is the BIM-EDIT source or target member name to use for *         
*             subsequent I/O processing.  On OUTPUT, an attempt is    *         
*             made to define a new member.  Existing members are NOT  *         
*             replaced.  Variable-length records are supported by     *         
*             creating x'0D25'-delimited records during INPUT         *         
*             processing and extracting x'0D25'-delimited records     *         
*             during OUTPUT processing.  Thus, the associated FTP     *         
*             process must use ASCII mode and have CR/LF turned ON;   *         
*   type    - is the BIM-EDIT target member type to use for OUTPUT    *         
*             processing (if omitted, or specified as an asterisk, a  *         
*             default member type of "TEXT" is used).                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BRANCH ROUTINES:                                                    *         
* ---------------                                                     *         
*   MEMNAME             MEMBER "DELETE" AND "RENAME" PROCESSING       *         
*   LIBNAME             LIBRARY "CREATE" AND "DELETE" PROCESSING      *         
*   SENDCMD             "SEND" COMMAND TO BIM-EDIT PROCESSING         *         
*   BIMCLS              ALL "CLOSE" PROCESSING                        *         
*                                                                     *         
* FIRST-LEVEL SUBROUTINES:                                            *         
* -----------------------                                             *         
*   MEMOPN    R9        "OPEN" PROCESSING FOR MEMBER INPUT OR OUTPUT  *         
*   DIROPN    R9        "OPEN" PROCESSING FOR A DIRECTORY LISTING     *         
*   BIMGET    R9        ALL "READ" PROCESSING (MEMBER & DIRECTORY)    *         
*   BIMPUT    R9        "WRITE" PROCESSING FOR MEMBER OUTPUT          *         
*                                                                     *         
* SECOND-LEVEL SUBROUTINES:                                           *         
* ------------------------                                            *         
*   OPENAPI   R9        OPEN THE BIM-EDIT API                         *         
*   CLOSEAPI  R9        CLOSE THE BIM-EDIT API                        *         
*                                                                     *         
* GENERAL SUBROUTINES:                                                *         
* -------------------                                                 *         
*   SENDBIM   R8        SEND MESSAGE DATA TO BIM-EDIT                 *         
*   RECVBIM   R8        RECEIVE MESSAGE DATA FROM BIM-EDIT            *         
*   LOGMSG    R7        CONSTRUCT AND LOG A MESSAGE ON SYSLOG         *         
*   PRTMSG    R7        CONSTRUCT AND PRINT A MESSAGE ON SYSLST       *         
*   APPEND    R4        APPEND DATA TO A BUFFER                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
* =================================================================== *         
DYNASTOR DSECT                                                                  
* ------------------------------------------------------------------- *         
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA              
PWORK8   DS    PL8                     PACKED DOUBLE-WORD                       
EWORK8   DS    CL8                     EDITED WORK FIELD                        
*                                                                               
WORDS    DS    0XL(4*6)                                                         
CURA     DS    F                       CURRENT BUFFER ADDRESS                   
CURL     DS    F                       CURRENT BUFFER LENGTH                    
BUFEND   DS    F                       CALCULATED ADDRESS BUFL+1                
LINK1    DS    F                       LINK ADDRESS A                           
LINK2    DS    F                       LINK ADDRESS B                           
TEMPA    DS    F                       TEMPORARY SAVE ADDRESS                   
*                                                                               
BIMPARMS DS    4F                      PARAMETER BLOCK                          
         ORG   BIMPARMS                                                         
LINEDA   DS    F                         LINE DATA ADDRESS                      
LINELA   DS    F                         LINE LENGTH ADDRESS                    
STATUSA  DS    F                         STATUS ADDRESS                         
WORKAA   DS    F                         WORK AREA ADDRESS                      
         ORG                                                                    
*                                                                               
LINED    DS    CL256                   BIM-EDIT DATA                            
LINEL    DS    H                       BIM-EDIT LENGTH                          
STATUS   DS    CL2                     BIM-EDIT STATUS                          
WORKA    DS    CL1024                  BIM-EDIT WORK AREA                       
*                                                                               
         DS    0H                                                               
SFUNC    DS    C                       ONE-LETTER FUNCTION PREFIX CODE          
OPT1     DS    X                       OPTION BITS                              
APIOPEN  EQU   B'10000000'             API OPEN FLAG                            
OPTERR   EQU   B'01000000'             ERROR FLAG                               
OPTEOF   EQU   B'00100000'             EOF FLAG                                 
MSGSENT  EQU   B'00010000'             MESSAGE SENT PREFIX FLAG                 
MSGSTAT  EQU   B'00001000'             BIM-EDIT MESSAGE STATUS FLAG             
BIMSEND  EQU   B'00000100'             BIM-EDIT API ON=SEND, OFF=RECV           
PRTSENT  EQU   B'00000010'             PRINT DATA SENT                          
PRTRECV  EQU   B'00000001'             PRINT DATA RECEIVED                      
OPT2     DS    X                       OPTION BITS                              
APISESS  EQU   B'10000000'             API SESSION OPEN FLAG                    
PBUF     DS    CL133                   PRINT BUFFER                             
*                                                                               
         DS    0D                      ALIGNMENT FOR TRANSLATE TABLE            
TRTBL    DS    CL256                   TEMPORARY TRANSLATE TABLE                
*                                                                               
DYNALENG EQU   *-DYNASTOR              LENGTH OF DYNAMIC STORAGE                
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
PARMBLK  DSECT                                                                  
         USING *,R10         FOR PERMANENT ADDRESSIBILITY                       
* ------------------------------------------------------------------- *         
FUNC     DS    F       FUNCTION                                                 
BUFA     DS    F       BUFFER ADDRESS                                           
BUFL     DS    F       BUFFER LENGTH                                            
USRA     DS    F       USER FIELD (SET ON OPEN, CLEARED ON CLOSE)               
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
PARSBLK  DSECT                                                                  
         USING *,R9          FOR TEMPORARY ADDRESSIBILITY                       
* ------------------------------------------------------------------- *         
PARSDATA DS    CL24    PARAMETER STRING                                         
PARSDLEN DS    F       PARAMETER LENGTH (ZERO RELATIVE)                         
PARSDVAL DS    F       PARAMETER VALUE  (IF NUMERIC)                            
PARSDADR DS    F       PARAMETER ADDRESS (WITHIN CARD IMAGE TEXT)               
*                                                                               
PARSLENG EQU   *-PARSBLK               LENGTH OF PARSE AREA                     
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
*        REGISTER USAGES                                                        
* ------------------------------------------------------------------- *         
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK                 
R1       EQU   1                       ON ENTRY, REXX EFPL REG                  
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                       REXX EXTERNAL ENTRY-POINT VECTOR         
R10      EQU   10                      REXX EVALUATION BLOCK                    
R11      EQU   11                      SHARED VARIABLES CONTROL BLOCK           
R12      EQU   12                      BSTTBEAM CSECT BASE REG                  
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
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
BSTTBEAM HEADER VERSION=' 2.2',AUTHOR=' DLC',DATE='(C)OCT05',          X        
               BASE=(R11,R12)                                                   
* ------------------------------------------------------------------- *         
*                                 point to arguments                            
         LR    R10,R1                  SET UP REG 10 FOR ARGUMENTS              
*                                                                               
         IF    FUNC,(NE,CLC),KMEMOPN   IF  NOT MEMBER OPEN                      
         AND   FUNC,(NE,CLC),KDIROPN   AND NOT DIRECTORY OPEN                   
         AND   FUNC,(NE,CLC),KMEMDEL   AND NOT MEMBER DELETE                    
         AND   FUNC,(NE,CLC),KMEMRNM   AND NOT MEMBER RENAME                    
         AND   FUNC,(NE,CLC),KLIBCRE   AND NOT LIBRARY CREATE                   
         AND   FUNC,(NE,CLC),KLIBDEL   AND NOT LIBRARY DELETE                   
         AND   FUNC,(NE,CLC),KSNDCMD   AND NOT SEND COMMAND                     
*                                 member or directory continuation              
          IF   R1,(ZERO,ICM,15),USRA    IF USER AREA ADDRESS IS ZERO            
           LA  R1,L'CALLERR              SET LENGTH OF MESSAGE                  
           LA  R2,CALLERR                GET ADDRESS OF MESSAGE                 
           B   CANCEL                    GO LOG MSG & CANCEL THE JOB            
          ELSE                          ELSE                                    
           USING DYNASTOR,R1             ESTABLISH TEMP ADDRESSIBILITY          
           IF  OPT1,(NOT,TM),APIOPEN     IF THE API IS NOT OPEN...              
            LA R1,L'APIERR                SET LENGTH OF MESSAGE                 
            LA R2,APIERR                  GET ADDRESS OF MESSAGE                
            B  CANCEL                     GO LOG MSG & CANCEL THE JOB           
           ENDIF                         ENDIF                                  
           DROP R1                       REMOVE TEMP ADDRESSIBILITY             
          ENDIF                         ENDIF                                   
*                                                                               
         ELSE                          ELSE                                     
*                                 allocate getvis and initialize                
          L    R0,SALEN                 GET MY SAVEAREA LENGTH                  
          GETVIS SVA=NO                 ACQUIRE GETVIS STORAGE                  
          IF   R15,(NZERO,LTR),R15      IF UNABLE TO OBTAIN STORAGE             
           LA  R1,L'VISERR               SET LENGTH OF MESSAGE                  
           LA  R2,VISERR                 GET ADDRESS OF MESSAGE                 
           B   CANCEL                    GO LOG MSG & CANCEL THE JOB            
          ENDIF                         ENDIF                                   
          ST   R1,USRA                  SAVE ADDRESS IN PARM BLOCK              
          USING DYNASTOR,R1             ESTABLISH TEMP ADDRESSIBILITY           
          XC   WORDS,WORDS              CLEAR SAVE WORDS                        
          LA   R2,LINED                 GET ADDRES FOR LINE DATA                
          LA   R3,LINEL                 GET ADDRESS FOR LINE LENGTH             
          LA   R4,STATUS                GET ADDRESS FOR STATUS                  
          LA   R5,WORKA                 GET ADDRESS FOR WORK AREA               
          STM  R2,R5,BIMPARMS           PUT IN ARGUMENT LIST                    
          XC   LINED,LINED              CLEAR MESSAGE DATA                      
          XC   LINEL,LINEL              CLEAR MESSAGE LENGTH                    
          XC   STATUS,STATUS            CLEAR STATUS CODE                       
          XC   SFUNC,SFUNC              CLEAR SAVE FUNCTION CODE                
          XC   OPT1,OPT1                CLEAR ALL OPTION BITS                   
          DROP R1                       REMOVE TEMP ADDRESSIBILITY              
*                                                                               
         ENDIF                         ENDIF                                    
*                                                                               
*                                 point to dynamic storage                      
         ST    R1,8(,R13)              SET NEXT SAVEAREA IN CALLER'S            
         ST    R13,4(,R1)              SET PREV SAVEAREA IN MINE                
         LR    R13,R1                  SET UP REG 13 FOR MY SAVEAREA            
         USING DYNASTOR,R13       FOR PERMANENT ADDRESSIBILITY                  
*                                                                               
* SELECT THE REQUESTED FUNCTION                                                 
* -----------------------------                                                 
*                                                                               
         SELECT                                                                 
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMOPN   IF MEMBER OPEN FUNC REQUESTED            
          BAL  R9,MEMOPN                EXECUTE MEMBER OPEN FUNCTION            
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   INPUT EXIT BSTTBEAM applid usrid pswd library member                        
*   OUTPUT EXIT BSTTBEAM applid usrid pswd library member <type>                
*                                                                               
*      The first letter of the first parsed value is used as the SFUNC          
* code in subsequent processing.  The second and third parsed values            
* are always ignored.  See the OPENAPI routine for its handling of the          
* next four parsed values.  Then, the eighth value (the member name) is         
* examined by this member open routine.  This value is required and may         
* not be omitted.  For processing, this routine procedes to issue               
* either a BIM-EDIT SEND or DEFINE command -- depending upon the SFUNC          
* code value ("I" or "O", respectively).  For DEFINE processing, an             
* optional ninth value specifies the member type for the new member.            
* If omitted, this routine supplies "TEXT" as the default member type.          
* DEFINE processing is also followed by EDIT and INSERTF commands.              
* ------------------------------------------------------------------- *         
                                                                                
         CASE  FUNC,(EQ,CLC),KDIROPN   IF DIRECTORY OPEN FUNC REQUESTED         
          MVI  SFUNC,C'L'               SAVE AS DIR. "L"IST FUNCTION            
          BAL  R9,DIROPN                EXECUTE DIRECTORY OPEN FUNCTION         
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   applid usrid pswd <*|library>                                               
*                                                                               
*      See the OPENAPI routine for its handling of the first four               
* parsed values.  Then, the fourth value (the library name) is also             
* examined by this directory open routine.  If this value is omitted,           
* i.e., the value is not greater than blanks, then this routine                 
* produces a list of all BIM-EDIT libraries to which this usrid has at          
* least EXEC-level access (i.e., using standard BIM-EDIT security).             
* Otherwise, if this usrid has at least LIST-level access to the                
* currently-attached library (see the OPENAPI routine for how this              
* attachment takes place), then this routine produces a list of all             
* members in that library.                                                      
* ------------------------------------------------------------------- *         
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMDEL   IF MEMBER DELETE FUNC REQUESTED          
          MVI  SFUNC,C'D'               SAVE AS "D"ELETE FUNCTION               
          B    MEMNAME                  EXECUTE MEMBER NAME FUNCTION            
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMRNM   IF MEMBER RENAME FUNC REQUESTED          
          MVI  SFUNC,C'N'               SAVE AS RE"N"AME FUNCTION               
          B    MEMNAME                  EXECUTE MEMBER NAME FUNCTION            
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   applid usrid pswd library member <as-member>                                
*                                                                               
*      See the OPENAPI routine for its handling of the first four               
* parsed values.  Then, the fifth value (the member name) is examined           
* by this member processing routine.  This value is required and may            
* not be omitted.  For processing, this routine procedes to issue               
* either a BIM-EDIT PURGE or RENAME command -- depending upon the SFUNC         
* code value passed from the entry selection code at the beginning of           
* this program.  For RENAME processing, the sixth value is required for         
* the rename "as" name.                                                         
* ------------------------------------------------------------------- *         
                                                                                
         CASE  FUNC,(EQ,CLC),KLIBCRE   IF LIBRARY CREATE FUNC REQUESTED         
          MVI  SFUNC,C'M'               SAVE AS "M"AKE FUNCTION                 
          B    LIBNAME                  EXECUTE LIBRARY CREATE FUNCTION         
                                                                                
         CASE  FUNC,(EQ,CLC),KLIBDEL   IF LIBRARY DELETE FUNC REQUESTED         
          MVI  SFUNC,C'R'               SAVE AS "R"EMOVE FUNCTION               
          B    LIBNAME                  EXECUTE LIBRARY DELETE FUNCTION         
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   applid usrid pswd library                                                   
*                                                                               
*      See the OPENAPI routine for its handling of the first four               
* parsed values.  Then, the fourth value (the library name) is also             
* examined by this library processing routine.  This value is required          
* and may not be omitted.  For processing, this routine procedes to             
* issue either a BIM-EDIT PURGEL or DEFINEL command -- depending upon           
* the SFUNC code value passed from the entry selection code at the              
* beginning of this program.  For DEFINEL processing, this routine              
* supplies a default library description.                                       
* ------------------------------------------------------------------- *         
                                                                                
         CASE  FUNC,(EQ,CLC),KSNDCMD   IF SEND COMMAND FUNC REQUESTED           
          MVI  SFUNC,C'S'               SAVE AS "S"END FUNCTION                 
          B    SENDCMD                  EXECUTE SEND COMMAND FUNCTION           
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   applid usrid pswd library command cmdparm1 cmdparm2 ...                     
*                                                                               
*      See the OPENAPI routine for its handling of the first four               
* parsed values.  Then, beginning with the fifth value, this send               
* command routine sends all remaining parsed values, verbatim, as a             
* command and its parameters to BIM-EDIT.                                       
* ------------------------------------------------------------------- *         
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMINP   IF MEMBER INPUT FUNC REQUESTED           
          BAL  R9,BIMGET                EXECUTE BIM-EDIT GET FUNCTION           
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMOUT   IF MEMBER OUTPUT FUNC REQUESTED          
          BAL  R9,BIMPUT                EXECUTE BIM-EDIT PUT FUNCTION           
                                                                                
         CASE  FUNC,(EQ,CLC),KMEMCLS   IF MEMBER CLOSE FUNC REQUESTED           
          B    BIMCLS                   EXECUTE BIM-EDIT CLOSE FUNCTION         
                                                                                
         CASE  FUNC,(EQ,CLC),KDIRLST   IF DIRECTORY LIST FUNC REQUESTED         
          BAL  R9,BIMGET                EXECUTE BIM-EDIT GET FUNCTION           
                                                                                
         CASE  FUNC,(EQ,CLC),KDIRCLS   IF DIRECTORY CLOSE FUNC REQUESTD         
          B    BIMCLS                   EXECUTE BIM-EDIT CLOSE FUNCTION         
                                                                                
         CASE  ELSE                    ELSE                                     
          LA   R1,L'INVFUNC             GET LENGTH OF ERROR MESSAGE             
          LA   R2,INVFUNC               GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          XR   R0,R0                    CLEAR RETURN LENGTH                     
          LA   R15,8                    SET ERROR CODE                          
         ENDSL                         END SELECT                               
*                                                                               
* RETURN RESULT                                                                 
*                                                                               
RETURN   DS    0H                                                               
         TRAILER RETCODE=(R15,R0),SVAREA=SAVEAREA   KEEP GETVIS                 
*                                                                               
CANCEL   DS    0H                                                               
         BAL   R7,LOGMSG               GO LOG MSG ON CONSOLE                    
         IF    R1,(NZ,ICM,15),USRA     IF USER AREA ADDRESS NOT ZERO            
          L    R0,SALEN                 GET MY SAVEAREA LENGTH                  
          FREEVIS SVA=NO                RELEASE GETVIS STORAGE                  
          IF   R15,(NZERO,LTR),R15      IF UNABLE TO FREE STORAGE               
           LA  R1,L'VISREL               SET LENGTH OF MESSAGE                  
           LA  R2,VISREL                 GET ADDRESS OF MESSAGE                 
           BAL R7,LOGMSG                 GO LOG MSG ON CONSOLE                  
          ENDIF                         ENDIF                                   
         ENDIF                         ENDIF                                    
         XC    USRA,USRA               CLEAR USER ADDR IN ARG LIST              
         XR    R0,R0                   CLEAR RETURN LENGTH                      
         LA    R15,16                  SET SEVERE ERROR                         
         B     RETURN                  GO RETURN TO CALLER                      
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
APPEND   DS    0H       APPEND DATA TO A BUFFER                                 
*                                 R0 = length accumulator                       
*                                 R1 = current data length (max 256)            
*                                 R2 = current data value                       
*                                 R3 = data accumulator                         
*                                 R4 = return register                          
* ------------------------------------------------------------------- *         
         BCTR  R1,0                    DECR. FOR 0-BASED LENGTH                 
         EX    R1,APPENDX              MOVE TO BUFFER                           
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         AR    R0,R1                   ADD TO BUFFER LENGTH                     
         AR    R3,R1                   AND TO BUFFER POINTER                    
*                                                                               
         BR    R4                      RETURN TO CALLER                         
APPENDX  MVC   0(0,R3),0(R2)           (SEE "EX" INSTRUCTION ABOVE)             
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
BIMCLS   DS    0H       ALL "CLOSE" PROCESSING                                  
* ------------------------------------------------------------------- *         
*                                 close the application interface               
         IF    OPT1,(ON,TM),APIOPEN    IF THE API IS OPEN...                    
          BAL  R9,CLOSEAPI              CLOSE THE BIM-EDIT API                  
         ENDIF                         ENDIF                                    
*                                 signal successful completion                  
         XR    R0,R0                   CLEAR RETURN LENGTH                      
         IF    OPT1,(ON,TM),OPTERR     IF AN ERROR OCCURRED PREVIOUSLY          
          LA   R15,8                    SET ERROR CODE                          
         ELSE                          ELSE                                     
          XR   R15,R15                  SET RETURN CODE TO ZERO                 
         ENDIF                         ENDIF                                    
*                                                                               
         XC    USRA,USRA                CLEAR USER ADDR IN ARG LIST             
         TRAILER RETCODE=(R15,R0),SVAREA=SALEN       FREE GETVIS                
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
BIMGET   DS    0H       ALL "READ" PROCESSING (MEMBER & DIRECTORY)              
*                                 R9 = return register                          
* ------------------------------------------------------------------- *         
         NI    OPT1,X'FF'-PRTRECV      SET OFF PRINT FLAG                       
*                                                                               
         XR    R0,R0                   CLEAR RETURN LENGTH REGISTER             
         L     R3,BUFA                 POINT TO RETURN BUFFER                   
         IF    SFUNC,EQ,C'L'           IF INPUT FROM DIRECTORY SESSION          
          ZAP  PWORK8,=PL1'0'           EACH BUFFER GETS OWN COUNT              
         ENDIF                         ENDIF                                    
*                                 load up the return buffer with input          
         WHILE OPT1,(NOT,TM),OPTEOF    WHILE NOT EOF                            
          BAL  R8,RECVBIM               RECEIVE BIMEDIT RESPONSE                
          IF   STATUS,(NE,CLC),=C'OK'                                           
           IF  STATUS,(NE,CLC),=C'EF',BC,BIMGETX                                
           ENDIF                                                                
           OI  OPT1,OPTEOF              SET ON EOF FLAG                         
           WEXIT                        EXIT LOOP                               
          ENDIF                                                                 
          IF   R0,(ZERO,LTR),R0         IF STARTING A NEW BUFFER                
           LA  R1,L'BUFINP               GET LENGTH OF MESSAGE                  
           LA  R2,BUFINP                 GET ADDRESS OF MESSAGE                 
           BAL R7,PRTMSG                 GO PRINT MESSAGE ON SYSLST             
           XR  R0,R0                     CLEAR RETURN LENGTH REGISTER           
           L   R3,BUFA                   POINT TO RETURN BUFFER                 
          ENDIF                         ENDIF                                   
          XR   R1,R1                    CLEAR REGISTER FOR ICM                  
          IF   R1,(HTZ,ICM,B'0011'),LINEL   IF LENGTH HIGHER THAN ZERO          
           LA  R2,LINED                  GET ADDRESS OF DATA                    
           BAL R4,APPEND                 PUT DATA IN BUFFER                     
          ENDIF                         ENDIF                                   
          IF   SFUNC,EQ,C'I'            IF INPUT FROM MEMBER                    
           LA  R1,L'CRLF                 GET LENGTH OF DELIMITER                
           LA  R2,CRLF                   GET ADDRESS OF DELIMITER               
           BAL R4,APPEND                 PUT DELIMITER IN BUFFER                
          ENDIF                         ENDIF                                   
          AP   PWORK8,=PL1'1'           INCREMENT RECORD COUNTER                
          L    R5,BUFL                  GET LENGTH OF RETURN BUFFER             
          SR   R5,R0                    SUBTRACT ACCUMULATED LENGTH             
          IF   R5,(LT,CH),=H'255'       IF BUFFER IS FULL                       
           WEXIT                         EXIT LOOP                              
          ENDIF                         ENDIF                                   
         WEND                          WEND                                     
*                                 set up for delayed eof response               
         IF    OPT1,(ON,TM),OPTEOF     IF EOF FLAGGED                           
         AND   R0,(ZERO,LTR),R0        AND NO DATA TO RETURN                    
          LA   R1,L'EOFINP              GET LENGTH OF MESSAGE                   
          LA   R2,EOFINP                GET ADDRESS OF MESSAGE                  
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          IF   SFUNC,EQ,C'I'            IF INPUT FROM MEMBER                    
           MVC LINED(L'CNTMSG),CNTMSG    MOVE MESSAGE TO OUTPUT AREA            
           ED  LINED+CNTMSGE(L'CNTMSGF),PWORK8+3                                
           LA  R1,L'CNTMSG               GET LENGTH OF MESSAGE                  
           LA  R2,LINED                  GET ADDRESS OF MESSAGE                 
           BAL R7,LOGMSG                 GO LOG MSG ON CONSOLE                  
          ENDIF                         ENDIF                                   
          XR   R0,R0                    CLEAR RETURN LENGTH                     
          LA   R15,4                    SET EOF RETURN CODE                     
         ELSE                          ELSE                                     
          IF   SFUNC,EQ,C'L'            IF INPUT FROM DIRECTORY SESSION         
           CVB R0,PWORK8                 PUT COUNT IN REG 0, NOT LENGTH         
          ENDIF                         ENDIF                                   
          XR   R15,R15                  SET RETURN CODE TO ZERO                 
         ENDIF                         ENDIF                                    
*                                                                               
BIMGETX  DS    0H                                                               
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
BIMPUT   DS    0H       "WRITE" PROCESSING FOR MEMBER OUTPUT                    
*                                 R9 = return register                          
* ------------------------------------------------------------------- *         
         NI    OPT1,X'FF'-PRTSENT      SET OFF PRINT FLAG                       
*                                                                               
         LA    R1,L'BUFOUT             GET LENGTH OF MESSAGE                    
         LA    R2,BUFOUT               GET ADDRESS OF MESSAGE                   
         BAL   R7,PRTMSG               GO PRINT MESSAGE ON SYSLST               
         L     R0,BUFL                 GET BUFFER LENGTH                        
         IF    BUFA,(ON,TM),X'80'      IF LAST BUFFER TO RECEIVE                
          OI   OPT1,OPTEOF              SET ON EOF FLAG                         
          NI   BUFA,X'FF'-X'80'         CLEAR BIT FROM BUFFER ADDRESS   BST01680
         ENDIF                         ENDIF                                    
         L     R3,BUFA                 POINT TO RECEIVED BUFFER                 
         LR    R1,R3                   BEGINNING OF BUFFER                      
         AR    R1,R0                    PLUS BUFFER LENGTH                      
         ST    R1,BUFEND                 EQUALS ENDING POSITION                 
*                                                                               
         WHILE R3,(LT,C),BUFEND                                                 
*                                 find record delimiter                         
         LA    R1,253                  SET LENGTH-1 TO SEARCH                   
         IF    R1,(Z,EX),FNDCRLF       IF RECORD DELIMITER NOT FOUND            
          L    R1,BUFEND                POINT TO END OF DATA                    
         ELSE                          ELSE                                     
          IF   R1,(NL,C),BUFEND         IF PAST END OF DATA                     
           L   R1,BUFEND                 POINT TO END OF DATA                   
          ENDIF                         ENDIF                                   
         ENDIF                         ENDIF                                    
*                                 calculate length of record                    
         LR    R5,R1                   COPY FUNCTION ADDRESS                    
         SR    R5,R3                   CALC LENGTH OF DATA                      
         IF    R5,(HT,CH),=H'253'      IF HIGHER THAN 253                       
          OI   OPT1,OPTERR+OPTEOF       SET ON ERROR & EOF FLAGS                
          MVC  LINED(L'RECLMSG),RECLMSG MOVE MESSAGE TO WORK AREA               
          ED   LINED+RECLMSGE(L'RECLMSGF),PWORK8+3                              
          LA   R1,L'RECLMSG             GET LENGTH OF ERROR MESSAGE             
          LA   R2,LINED                 GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          LA   R1,L'INVLENG             GET LENGTH OF ERROR MESSAGE             
          LA   R2,INVLENG               GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          WEXIT                         TERMINATE DATA LOOP                     
FNDCRLF  TRT   0(0,R3),TRTBL           (SEE "EX" INSTRUCTION ABOVE)     00032200
         ENDIF                         ENDIF                                    
*                                 check if full record is in the buffer         
         IF    R1,(EQ,CL),BUFEND       IF SEARCHED PAST END OF BUFFER           
         AND   OPT1,(NOT,TM),OPTEOF    AND EOF NOT FLAGGED                      
          LR   R0,R5                    INDICATE REMAINING DATA LENGTH          
          L    R6,BUFA                  POINT TO BEGINNING OF BUFFER            
          BCTR R5,0                     DECR. FOR 0-BASED LENGTH                
          EX   R5,PUSHUP                COPY RECORD TO BIM-EDIT PARM            
          WEXIT                         GO GET MORE DATA                        
PUSHUP   MVC   0(0,R6),0(R3)           (SEE "EX" INSTRUCTION ABOVE)     00032200
PUTLINE  MVC   LINED(0),0(R3)          (SEE "EX" INSTRUCTION BELOW)     00032200
         ENDIF                         ENDIF                                    
*                                 send record to bim-edit                       
         LR    R1,R5                   COPY LENGTH OF RECORD                    
         STH   R1,LINEL                PUT IN BIM-EDIT PARAMETER                
         BCTR  R1,0                    DECR. FOR 0-BASED LENGTH                 
         EX    R1,PUTLINE              COPY RECORD TO BIM-EDIT PARM             
         BAL   R8,SENDBIM              SEND DATA TO BIMEDIT                     
         IF    STATUS,(NE,CLC),=C'OK',BC,BIMPUTX                                
         ENDIF                                                                  
         AP    PWORK8,=PL1'1'          INCREMENT RECORD COUNTER                 
*                                 point to next record                          
         AR    R3,R5                   INCR. FOR RECORD LENGTH                  
         IF    0(L'CRLF,R3),(EQ,CLC),CRLF   IF BOTH CR & LF PRESENT             
          LA   R3,L'CRLF(,R3)           INCR. FOR 2-BYTE DELIMITER              
         ELSE                          ELSE                                     
          LA   R3,1(,R3)                INCR. FOR 1-BYTE DELIMITER              
         ENDIF                         ENDIF                                    
*                                                                               
         WEND                                                                   
*                                                                               
         IF    OPT1,(ON,TM),OPTEOF     IF EOF FLAGGED                           
          XC   LINEL,LINEL              SEND ZERO-LENGTH RECORD                 
          MVC  STATUS,=C'EF'            FLAGGED AS END-OF-FILE                  
          BAL  R8,SENDBIM               SEND DATA TO BIMEDIT                    
          IF   STATUS,(NE,CLC),=C'OK',BC,BIMPUTX                                
          ENDIF                                                                 
          IF   OPT1,(ON,TM),OPTERR      IF AN ERROR OCCURRED                    
           XR  R0,R0                     CLEAR RETURN LENGTH                    
           LA  R15,8                     SET ERROR CODE                         
           B   BIMPUTX                   GO RETURN TO CALLER                    
          ENDIF                         ENDIF                                   
          LA   R1,L'EOFOUT              GET LENGTH OF MESSAGE                   
          LA   R2,EOFOUT                GET ADDRESS OF MESSAGE                  
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          MVC  LINED(L'CNTMSG),CNTMSG   MOVE MESSAGE TO OUTPUT AREA             
          ED   LINED+CNTMSGE(L'CNTMSGF),PWORK8+3                                
          LA   R1,L'CNTMSG              GET LENGTH OF MESSAGE                   
          LA   R2,LINED                 GET ADDRESS OF MESSAGE                  
          BAL  R7,LOGMSG                GO LOG MSG ON CONSOLE                   
          XR   R0,R0                    CLEAR RETURN LENGTH                     
         ENDIF                         ENDIF                                    
         XR    R15,R15                 SET RETURN CODE TO ZERO                  
*                                                                               
BIMPUTX  DS    0H                                                               
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
CLOSEAPI DS    0H       CLOSE THE BIM-EDIT API                                  
*                                 R9 = return register                          
* ------------------------------------------------------------------- *         
         OI    OPT1,PRTSENT+PRTRECV    SET ON PRINT FLAGS                       
*                                 build session quit command                    
         IF    OPT2,(ON,TM),APISESS    IF A SESSION IS OPEN                     
          NI   OPT2,X'FF'-APISESS       FLAG SESSION AS CLOSED                  
          MVC  LINED(L'QUIT),QUIT       PUT IN BIMEDIT LINE                     
          LA   R1,L'QUIT                GET LENGTH OF DATA                      
          STH  R1,LINEL                 PUT LENGTH IN PARM                      
          BAL  R8,SENDBIM               SEND COMMAND STRING TO BIMEDIT          
          BAL  R8,RECVBIM               RECEIVE BIMEDIT RESPONSE                
         ENDIF                         ENDIF                                    
*                                 build logoff command string                   
         MVC   LINED(L'LOGOFF),LOGOFF  PUT IN BIMEDIT LINE                      
         LA    R1,L'LOGOFF             GET LENGTH OF DATA                       
         STH   R1,LINEL                PUT LENGTH IN PARM                       
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
*                                 close the application interface               
         NI    OPT1,X'FF'-APIOPEN      FLAG API AS CLOSED                       
         XC    LINEL,LINEL             CLEAR LINE LENGTH                        
         LA    R1,BIMPARMS             GET ADDRESS OF PARM BLOCK                
         L     R15,=V(BIUAPCL)         GET ADDRESS OF API                       
         BALR  R14,R15                 CLOSE BIM-EDIT API                       
         IF    STATUS,(NE,CLC),=C'OK'  IF CLOSE ERROR...                        
          OI   OPT1,OPTERR+MSGSTAT      SET ON ERROR & MSG API STATUS           
          LA   R1,L'AMERROR             GET LENGTH OF ERROR MESSAGE             
          LA   R2,AMERROR               GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          LA   R15,8                    SET ERROR CODE                          
          B    CLOSAPIX                 GO RETURN TO CALLER                     
         ENDIF                         ENDIF                                    
*                                 signal successful completion                  
         LA    R1,L'APICLS             GET LENGTH OF MESSAGE                    
         LA    R2,APICLS               GET ADDRESS OF MESSAGE                   
         BAL   R7,PRTMSG               GO PRINT MESSAGE ON SYSLST               
*                                                                               
CLOSAPIX DS    0H                                                               
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
DIROPN   DS    0H       "OPEN" PROCESSING FOR A DIRECTORY LISTING               
*                                 R9 = return register                          
* ------------------------------------------------------------------- *         
         ST    R9,LINK1                SAVE LINK LEVEL 1 ADDRESS                
         L     R9,BUFA                 SET UP REG 9 FOR PARSED VALUES           
*                                 open the application interface                
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         BAL   R9,OPENAPI              OPEN THE BIM-EDIT API                    
         IF    STATUS,(NE,CLC),=C'OK',BC,DIROPNX                                
         ENDIF                                                                  
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
*                                 build directory processing command            
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         LA    R1,L'EXEC               SET LENGTH OF DATA                       
         LA    R2,EXEC                 GET ADDRESS OF DATA                      
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R1,L'BSTTLIBL           SET LENGTH OF DATA                       
         IF    PARSDATA,NH,C' '        IF LIBRARY OMITTED                       
          LA   R2,BSTTLIBL              GET ADDRESS OF DATA                     
         ELSE                          ELSE                                     
          LA   R2,BSTTMEML              GET ADDRESS OF DATA                     
         ENDIF                         ENDIF                                    
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,DIROPNX                                
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,DIROPNX                                
         ENDIF                                                                  
         OI    OPT2,APISESS            SET ON SESSION OPEN FLAG                 
*                                 build send current session command            
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         LA    R1,L'SEND               SET LENGTH OF DATA                       
         LA    R2,SEND                 GET ADDRESS OF DATA                      
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R1,L'SLASH              SET LENGTH OF DATA                       
         LA    R2,SLASH                GET ADDRESS OF DATA                      
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,DIROPNX                                
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,DIROPNX                                
         ENDIF                                                                  
*                                 signal successful completion                  
         XR    R0,R0                   CLEAR RETURN LENGTH                      
         XR    R15,R15                 SET RETURN CODE TO ZERO                  
*                                                                               
DIROPNX  DS    0H                                                               
         L     R9,LINK1                RESTORE LINK LEVEL 1 ADDRESS             
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
LIBNAME  DS    0H       "CREATE" & "DELETE" PROCESSING FOR LIBRARIES            
* ------------------------------------------------------------------- *         
         L     R9,BUFA                 SET UP REG 9 FOR PARSED VALUES           
*                                 open the application interface                
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         BAL   R9,OPENAPI              OPEN THE BIM-EDIT API                    
         IF    STATUS,(NE,CLC),=C'OK',BC,LIBNAMEX                               
         ENDIF                                                                  
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
*                                 build library processing command              
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         IF    SFUNC,EQ,C'R'           IF LIBRARY "R"EMOVE                      
          LA   R1,L'PURGEL              SET LENGTH OF DATA                      
          LA   R2,PURGEL                GET ADDRESS OF DATA                     
         ELSE                          ELSE                                     
          LA   R1,L'DEFINEL             SET LENGTH OF DATA                      
          LA   R2,DEFINEL               GET ADDRESS OF DATA                     
         ENDIF                         ENDIF                                    
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         IF    PARSDATA,NH,C' '        IF LIBRARY NAME OMITTED                  
          OI   OPT1,OPTERR              SET ON ERROR FLAG                       
          LA   R1,L'LIBREQ              GET LENGTH OF ERROR MESSAGE             
          LA   R2,LIBREQ                GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          B    LIBNAMEX                 GO CLOSE API AND RETURN ERROR           
         ENDIF                         ENDIF                                    
         L     R1,PARSDLEN             GET 0-BASED LEN. OF PARSED DATA          
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         IF    SFUNC,EQ,C'M'           IF LIBRARY "M"AKE                        
          LA   R1,L'TITLE               SET LENGTH OF DATA                      
          LA   R2,TITLE                 GET ADDRESS OF DATA                     
          BAL  R4,APPEND                PUT IN BIMEDIT LINE                     
         ENDIF                         ENDIF                                    
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,LIBNAMEX                               
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,LIBNAMEX                               
         ENDIF                                                                  
*                                                                               
LIBNAMEX DS    0H                                                               
         B     BIMCLS                  EXECUTE BIM-EDIT CLOSE FUNCTION          
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
LOGMSG   DS    0H       CONSTRUCT AND LOG A MESSAGE ON SYSLOG                   
*                                 R1 = msg length                               
*                                 R2 = msg value                                
*                                 R7 = return register                          
* ------------------------------------------------------------------- *         
         LA    R0,L'PGMNAME            SET INITIAL BUFFER LENGTH                
         LA    R3,PBUF                 GET MESSAGE BUFFER POINTER               
         MVC   0(L'PGMNAME,R3),PGMNAME START MESSAGE WITH PGMNAME               
         AR    R3,R0                   ADD TO MESSAGE BUFFER POINTER            
         BAL   R4,APPEND               PUT REST OF MESSAGE IN BUFFER            
*                                                                               
         MVC   OUTPUT(60),=CL256' '    CLEAR SYSLOG LINE                        
         LR    R1,R0                   GET MESSAGE LENGTH                       
         BCTR  R1,0                    DECR. FOR 0-BASED LENGTH                 
         EX    R1,LOGMSGX              MOVE TO I/O AREA                         
         PUT   CONSOLE                 PUT MESSAGE ON CONSOLE                   
*                                                                               
         BR    R7                      RETURN TO CALLER                         
LOGMSGX  MVC   OUTPUT(0),PBUF          (SEE "EX" INSTRUCTION ABOVE)             
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
MEMNAME  DS    0H       "DELETE" & "RENAME" PROCESSING FOR MEMBERS              
* ------------------------------------------------------------------- *         
         L     R9,BUFA                 SET UP REG 9 FOR PARSED VALUES           
*                                 open the application interface                
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         BAL   R9,OPENAPI              OPEN THE BIM-EDIT API                    
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMNAMEX                               
         ENDIF                                                                  
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
*                                 build member processing command               
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         IF    SFUNC,EQ,C'D'           IF MEMBER "D"ELETE                       
          LA   R1,L'PURGE               SET LENGTH OF DATA                      
          LA   R2,PURGE                 GET ADDRESS OF DATA                     
         ELSE                          ELSE                                     
          LA   R1,L'RENAME              SET LENGTH OF DATA                      
          LA   R2,RENAME                GET ADDRESS OF DATA                     
         ENDIF                         ENDIF                                    
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO MEMBER_NAME                          
         IF    PARSDATA,NH,C' '        IF MEMBER NAME OMITTED                   
          OI   OPT1,OPTERR              SET ON ERROR FLAG                       
          LA   R1,L'MEMREQ              GET LENGTH OF ERROR MESSAGE             
          LA   R2,MEMREQ                GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          B    MEMNAMEX                 GO CLOSE API AND RETURN ERROR           
         ENDIF                         ENDIF                                    
         L     R1,PARSDLEN             GET 0-BASED LEN. OF PARSED DATA          
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         IF    SFUNC,EQ,C'N'           IF MEMBER RE"N"AME                       
          LA   R9,PARSLENG(,R9)   POINT TO AS MEMBER_NAME                       
          IF   PARSDATA,NH,C' '         IF AS MEMBER NAME OMITTED               
           OI  OPT1,OPTERR               SET ON ERROR FLAG                      
           LA  R1,L'RNMREQ               GET LENGTH OF ERROR MESSAGE            
           LA  R2,RNMREQ                 GET ADDRESS OF ERROR MESSAGE           
           BAL R7,PRTMSG                 GO PRINT MESSAGE ON SYSLST             
           B   MEMNAMEX                  GO CLOSE API AND RETURN ERROR          
          ENDIF                         ENDIF                                   
          LA   R1,L'COMMA               SET LENGTH OF DATA                      
          LA   R2,COMMA                 GET ADDRESS OF DATA                     
          BAL  R4,APPEND                PUT IN BIMEDIT LINE                     
          L    R1,PARSDLEN              GET 0-BASED LEN. OF PARSED DATA         
          LA   R1,1(,R1)                INCR. FOR 1-BASED LENGTH                
          LA   R2,PARSDATA              GET ADDRESS OF PARSED DATA              
          BAL  R4,APPEND                PUT IN BIMEDIT LINE                     
         ENDIF                         ENDIF                                    
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMNAMEX                               
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMNAMEX                               
         ENDIF                                                                  
*                                                                               
MEMNAMEX DS    0H                                                               
         B     BIMCLS                  EXECUTE BIM-EDIT CLOSE FUNCTION          
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
MEMOPN   DS    0H       "OPEN" PROCESSING FOR MEMBER INPUT OR OUTPUT            
*                                 R9 = return register                          
* ------------------------------------------------------------------- *         
         ST    R9,LINK1                SAVE LINK LEVEL 1 ADDRESS                
         L     R9,BUFA                 SET UP REG 9 FOR PARSED VALUES           
         MVC   SFUNC,PARSDATA          SAVE "I"NPUT OR "O"OUTPUT                
         LA    R9,PARSLENG(,R9)        POINT TO "EXIT"                          
         LA    R9,PARSLENG(,R9)        POINT TO "BSTTBEAM"                      
         LA    R9,PARSLENG(,R9)        POINT TO APPL ID                         
*                                 open the application interface                
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         BAL   R9,OPENAPI              OPEN THE BIM-EDIT API                    
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
         ENDIF                                                                  
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
*                                 build member processing command               
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         IF    SFUNC,EQ,C'O'           IF OUTPUT TO MEMBER                      
          LA   R1,L'DEFINE              SET LENGTH OF DATA                      
          LA   R2,DEFINE                GET ADDRESS OF DATA                     
         ELSE                          ELSE                                     
          LA   R1,L'SEND                SET LENGTH OF DATA                      
          LA   R2,SEND                  GET ADDRESS OF DATA                     
         ENDIF                         ENDIF                                    
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO MEMBER_NAME                          
         L     R1,PARSDLEN             GET 0-BASED LEN. OF PARSED DATA          
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         IF    SFUNC,EQ,C'O'           IF OUTPUT TO MEMBER                      
          LA   R9,PARSLENG(,R9)   POINT TO MEMBER TYPE                          
          IF   PARSDATA,EQ,C'*',OR,PARSDATA,NH,C' '   IF TYPE OMITTED           
           LA  R1,L'TEXT                 SET LENGTH OF DATA                     
           LA  R2,TEXT                   GET ADDRESS OF DATA                    
          ELSE                          ELSE                                    
           LA  R1,L'COMMA                SET LENGTH OF DATA                     
           LA  R2,COMMA                  GET ADDRESS OF DATA                    
           BAL R4,APPEND                 PUT IN BIMEDIT LINE                    
           L   R1,PARSDLEN               GET 0-BASED LEN OF PARSED DATA         
           LA  R1,1(,R1)                 INCR. FOR 1-BASED LENGTH               
           LA  R2,PARSDATA               GET ADDRESS OF PARSED DATA             
          ENDIF                         ENDIF                                   
          BAL  R4,APPEND                PUT IN BIMEDIT LINE                     
          LA   R1,L'TITLE               SET LENGTH OF DATA                      
          LA   R2,TITLE                 GET ADDRESS OF DATA                     
          BAL  R4,APPEND                PUT IN BIMEDIT LINE                     
         ENDIF                         ENDIF                                    
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
         ENDIF                                                                  
*                                 build member edit command                     
         IF    SFUNC,EQ,C'O'           IF OUTPUT TO MEMBER                      
          MVC  LINED(L'EDIT),EDIT       PUT IN BIMEDIT LINE                     
          LA   R1,L'EDIT                GET LENGTH OF DATA                      
          STH  R1,LINEL                 PUT LENGTH IN PARM                      
          BAL  R8,SENDBIM               SEND COMMAND STRING TO BIMEDIT          
          IF   STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
          ENDIF                                                                 
          BAL  R8,RECVBIM               RECEIVE BIMEDIT RESPONSE                
          IF   STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
          ENDIF                                                                 
          OI   OPT2,APISESS             SET ON SESSION OPEN FLAG                
*                                 build insert-following command                
          MVC  LINED(L'INSERTF),INSERTF PUT IN BIMEDIT LINE                     
          LA   R1,L'INSERTF             GET LENGTH OF DATA                      
          STH  R1,LINEL                 PUT LENGTH IN PARM                      
          BAL  R8,SENDBIM               SEND COMMAND STRING TO BIMEDIT          
          IF   STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
          ENDIF                                                                 
          BAL  R8,RECVBIM               RECEIVE BIMEDIT RESPONSE                
          IF   STATUS,(NE,CLC),=C'OK',BC,MEMOPNX                                
          ENDIF                                                                 
*                                 set up for extracting of output               
          XC   TRTBL,TRTBL              INITIALIZE TRANSLATE TABLE              
          MVI  TRTBL+CR,CR              TRAP FOR CARRIAGE-RETURN                
          MVI  TRTBL+LF,LF              TRAP FOR LINE-FEED                      
         ENDIF                         ENDIF                                    
*                                                                               
         ZAP   PWORK8,=PL1'0'          PREPARE RECORD COUNTER                   
*                                 signal successful completion                  
         XR    R0,R0                   CLEAR RETURN LENGTH                      
         XR    R15,R15                 SET RETURN CODE TO ZERO                  
*                                                                               
MEMOPNX  DS    0H                                                               
         L     R9,LINK1                RESTORE LINK LEVEL 1 ADDRESS             
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
OPENAPI  DS    0H       OPEN THE BIM-EDIT API                                   
*                                 R9 = return register                          
*                                                                               
* PASSED PARSED VALUES ARE:                                                     
*                                                                               
*   ... {*|applid} usrid pswd {*|library} ...                                   
*                                                                               
*      This routine processes up to four parsed values -- no matter how         
* many parsed values were passed.  When this routine receives control,          
* it expects TEMPA to point to the first of these four parsed values.           
* This first value specifies the BIM-EDIT VTAM Application ID to which          
* an API connection is to be made by this routine.  This value is               
* required and may not be omitted.  However, if specified as an                 
* asterisk, this routine supplies the BIM-EDIT installation default             
* application id of "BIMEDIT" for the API connection.  Then, the next           
* two values are used as the user id and password, respectively, for            
* subsequent BIM-EDIT "LOGON" processing.  Finally, this routine uses           
* the fourth of these parsed values (the library name) to perform a             
* library "ATTACH" command.  This value is required and may not be              
* omitted.  However, if specified as an asterisk, this routine attempts         
* to attach the user's default "HOME" library.  On return, this routine         
* saves a pointer to this fourth value back into the TEMPA field -- so          
* that the calling routine may continue in the parsed values list where         
* this routine has left off.                                                    
* ------------------------------------------------------------------- *         
         ST    R9,LINK2                SAVE LINK LEVEL 2 ADDRESS                
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
         OI    OPT1,PRTSENT+PRTRECV    SET ON PRINT FLAGS                       
         NI    OPT1,X'FF'-APIOPEN-OPTEOF  SET OFF API OPEN & EOF FLAG           
*                                 open the application interface                
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         IF    PARSDATA,EQ,C'*'        IF ASTERISK SUPPLIED...                  
          LA   R2,DEFAPPL               GET ADDRESS OF DATA                     
         ELSE                          ELSE                                     
          LA   R2,PARSDATA              GET ADDRESS OF PARSED DATA              
         ENDIF                         ENDIF                                    
         LA    R1,L'DEFAPPL            SET 1-BASED LEN. OF DATA                 
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         LA    R1,BIMPARMS             GET ADDRESS OF PARM BLOCK                
         L     R15,=V(BIUAPOP)         GET ADDRESS OF API                       
         BALR  R14,R15                 OPEN BIM-EDIT API                        
         IF    STATUS,(NE,CLC),=C'OK'  IF OPEN ERROR...                         
          OI   OPT1,OPTERR+MSGSTAT      SET ON ERROR & MSG API STATUS           
          LA   R1,L'AMERROR             GET LENGTH OF ERROR MESSAGE             
          LA   R2,AMERROR               GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          LA   R15,8                    SET ERROR CODE                          
          B    OPENAPIX                 GO RETURN TO CALLER                     
         ENDIF                         ENDIF                                    
         OI    OPT1,APIOPEN            FLAG API AS OPEN                         
         LA    R1,L'APIOPN             GET LENGTH OF MESSAGE                    
         LA    R2,APIOPN               GET ADDRESS OF MESSAGE                   
         BAL   R7,PRTMSG               GO PRINT MESSAGE ON SYSLST               
*                                 build logon command string                    
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         LA    R1,L'LOGON              SET LENGTH OF DATA                       
         LA    R2,LOGON                GET ADDRESS OF DATA                      
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO USER ID                              
         L     R1,PARSDLEN             GET 0-BASED LEN. OF PARSED DATA          
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R1,L'COMMA              SET LENGTH OF DATA                       
         LA    R2,COMMA                GET ADDRESS OF DATA                      
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO PASSWORD                             
         L     R1,PARSDLEN             GET 0-BASED LEN. OF PARSED DATA          
         LA    R1,1(,R1)               INCR. FOR 1-BASED LENGTH                 
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
         ENDIF                                                                  
*                                 turn on error trapping                        
         MVC   LINED(L'PPDCOND),PPDCOND PUT IN BIMEDIT LINE                     
         LA    R1,L'PPDCOND            GET LENGTH OF DATA                       
         STH   R1,LINEL                PUT LENGTH IN PARM                       
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
         ENDIF                                                                  
*                                                                               
         LA    R9,PARSLENG(,R9)   POINT TO LIBRARY.NAME                         
*                                 build attach command string                   
         IF    SFUNC,NE,C'M'           IF NOT LIBRARY "M"AKE FUNCTION           
          XR   R0,R0                   CLEAR LENGTH ACCUMULATOR                 
          LA   R3,LINED                SET ADDRESS OF BIMEDIT LINE              
          LA   R1,L'ATTACH             SET LENGTH OF DATA                       
          LA   R2,ATTACH               GET ADDRESS OF DATA                      
          BAL  R4,APPEND               PUT IN BIMEDIT LINE                      
          IF   PARSDATA,EQ,C'*',OR,PARSDATA,NH,C' '   IF ATTACH HOME...         
           LA  R1,L'HOME                SET LENGTH OF DATA                      
           LA  R2,HOME                  GET ADDRESS OF DATA                     
          ELSE                         ELSE                                     
           L   R1,PARSDLEN              GET 0-BASED LEN. OF PARSED DATA         
           LA  R1,1(,R1)                INCR. FOR 1-BASED LENGTH                
           LA  R2,PARSDATA              GET ADDRESS OF PARSED DATA              
          ENDIF                        ENDIF                                    
          BAL  R4,APPEND               PUT IN BIMEDIT LINE                      
          STH  R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
          BAL  R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
          IF   STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
          ENDIF                                                                 
          BAL  R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
          IF   STATUS,(NE,CLC),=C'OK',BC,OPENAPIX                               
          ENDIF                                                                 
         ENDIF                                                                  
*                                                                               
OPENAPIX DS    0H                                                               
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         L     R9,LINK2                RESTORE LINK LEVEL 2 ADDRESS             
         BR    R9                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
PRTMSG   DS    0H       CONSTRUCT AND PRINT A MESSAGE ON SYSLST                 
*                                 R1 = msg length                               
*                                 R2 = msg value                                
*                                 R7 = return register                          
* ------------------------------------------------------------------- *         
         LA    R0,L'BUFPFX             SET INITIAL BUFFER LENGTH                
         LA    R3,PBUF                 GET MESSAGE BUFFER POINTER               
         IF    OPT1,(ON,TM),MSGSENT    IF MESSAGE SENT PFX REQUESTED            
          MVC  0(L'BUFPFX,R3),BUFPFX    SET MESSAGE SENT PREFIX                 
          NI   OPT1,X'FF'-MSGSENT       SET OFF MSG SENT FLAG                   
         ELSE                          ELSE                                     
          MVC  0(L'BUFPFX,R3),=CL256' ' CLEAR MESSAGE BUFFER PREFIX             
         ENDIF                         ENDIF                                    
         AR    R3,R0                   ADD TO MESSAGE BUFFER POINTER            
         BAL   R4,APPEND               PUT REST OF MESSAGE IN BUFFER            
*                                                                               
         IF    OPT1,(ON,TM),MSGSTAT    IF MESSAGE STATUS REQUESTED              
          SELECT                                                                
          CASE FUNC,(EQ,CLC),KMEMOPN,OR,FUNC,(EQ,CLC),KDIROPN                   
           MVC PBUF+L'BUFPFX+AMERRORF(L'AMERRORF),AMOPEN                        
          CASE FUNC,(EQ,CLC),KMEMCLS,OR,FUNC,(EQ,CLC),KDIRCLS                   
           MVC PBUF+L'BUFPFX+AMERRORF(L'AMERRORF),AMCLOSE                       
          CASE OPT1,(ON,TM),BIMSEND                                             
           MVC PBUF+L'BUFPFX+AMERRORF(L'AMERRORF),AMSEND                        
          CASE ELSE                                                             
           MVC PBUF+L'BUFPFX+AMERRORF(L'AMERRORF),AMRECV                        
          ENDSL                                                                 
          MVC  PBUF+L'BUFPFX+AMERRORS(L'AMERRORS),STATUS                        
          NI   OPT1,X'FF'-MSGSTAT       SET OFF MESSAGE STATUS FLAG             
         ENDIF                         ENDIF                                    
*                                                                               
         MVC   OUTPUT,=CL256' '        CLEAR PRINT LINE                         
         LR    R1,R0                   GET MESSAGE LENGTH                       
         BCTR  R1,0                    DECR. FOR 0-BASED LENGTH                 
         EX    R1,PRTMSGX              MOVE TO I/O AREA                         
         PUT   PRINTER                 PRINT LINE                               
*                                                                               
         BR    R7                      RETURN TO CALLER                         
PRTMSGX  MVC   OUTPUT(0),PBUF          (SEE "EX" INSTRUCTION ABOVE)             
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
RECVBIM  DS    0H       RECEIVE MESSAGE DATA FROM BIM-EDIT                      
*                                 R8 = return register                          
* ------------------------------------------------------------------- *         
         ST    R0,CURL                 SAVE LENGTH OF ACCUMULATED DATA          
         ST    R3,CURA                 SAVE ADDRESS OF ACCUMULATED DATA         
*                                 receive data from bim-edit                    
         LA    R1,BIMPARMS             GET ADDRESS OF PARM BLOCK                
         L     R15,=V(BIUAPRC)         GET ADDRESS OF API                       
         BALR  R14,R15                 RECEIVE VIA BIM-EDIT API                 
*                                 print received data for debugging             
         IF    OPT1,(ON,TM),PRTRECV    IF PRINT FLAG ON                         
          XR   R1,R1                    CLEAR REGISTER FOR ICM                  
          IF   R1,(HTZ,ICM,B'0011'),LINEL   IF LENGTH HIGHER THAN ZERO          
           LA  R2,LINED                 GET ADDRESS OF MESSAGE                  
           BAL R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          ENDIF                                                                 
         ENDIF                         ENDIF                                    
*                                 check returned status code                    
         IF    STATUS,(NE,CLC),=C'OK'                                           
          IF   STATUS,(EQ,CLC),=C'EF'                                           
           LA  R1,L'EOFSTAT            GET LENGTH OF MESSAGE                    
           LA  R2,EOFSTAT              GET ADDRESS OF MESSAGE                   
          ELSE                                                                  
           OI  OPT1,OPTERR+MSGSTAT     SET ON ERROR & MSG API STATUS            
           LA  R1,L'AMERROR            GET LENGTH OF ERROR MESSAGE              
           LA  R2,AMERROR              GET ADDRESS OF ERROR MESSAGE             
          ENDIF                                                                 
          BAL  R7,PRTMSG               GO PRINT MESSAGE ON SYSLST               
          LA   R15,8                   SET ERROR CODE                           
         ENDIF                                                                  
*                                                                               
         L     R0,CURL                 GET LENGTH OF ACCUMULATED DATA           
         L     R3,CURA                 GET ADDRESS OF ACCUMULATED DATA          
         BR    R8                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
SENDBIM  DS    0H       SEND MESSAGE DATA TO BIM-EDIT                           
*                                 R8 = return register                          
* ------------------------------------------------------------------- *         
         OI    OPT1,BIMSEND            SET ON BIM SEND API FLAG                 
         ST    R0,CURL                 SAVE LENGTH OF EXTRACTED DATA            
         ST    R3,CURA                 SAVE ADDRESS OF EXTRACTED DATA           
*                                 print send data for debugging                 
         IF    OPT1,(ON,TM),PRTSENT    IF PRINT FLAG ON                         
          XR   R1,R1                    CLEAR REGISTER FOR ICM                  
          IF   R1,(HTZ,ICM,B'0011'),LINEL   IF LENGTH HIGHER THEN ZERO          
           OI  OPT1,MSGSENT             SET ON MSG SENT FLAG                    
           LA  R2,LINED                 GET ADDRESS OF MESSAGE                  
           BAL R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          ENDIF                                                                 
         ENDIF                         ENDIF                                    
*                                 send data to bim-edit                         
         LA    R1,BIMPARMS             GET ADDRESS OF PARM BLOCK                
         L     R15,=V(BIUAPSD)         GET ADDRESS OF API                       
         BALR  R14,R15                 SEND VIA BIM-EDIT API                    
*                                 check returned status code                    
         IF    STATUS,(NE,CLC),=C'OK'                                           
          OI   OPT1,OPTERR+MSGSTAT     SET ON ERROR & MSG API STATUS            
          LA   R1,L'AMERROR            GET LENGTH OF ERROR MESSAGE              
          LA   R2,AMERROR              GET ADDRESS OF ERROR MESSAGE             
          BAL  R7,PRTMSG               GO PRINT MESSAGE ON SYSLST               
          LA   R15,8                   SET ERROR CODE                           
         ENDIF                                                                  
*                                                                               
         L     R0,CURL                 GET LENGTH OF EXTRACTED DATA             
         L     R3,CURA                 GET ADDRESS OF EXTRACTED DATA            
         NI    OPT1,X'FF'-BIMSEND      SET OFF BIM SEND API FLAG                
         BR    R8                      RETURN TO CALLER                         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
SENDCMD  DS    0H       "SEND" COMMAND TO BIM-EDIT PROCESSING                   
* ------------------------------------------------------------------- *         
         L     R9,BUFA                 SET UP REG 9 FOR PARSED VALUES           
*                                 OPEN THE APPLICATION INTERFACE                
         ST    R9,TEMPA                SAVE DSECT ADDRESS                       
         BAL   R9,OPENAPI              OPEN THE BIM-EDIT API                    
         IF    STATUS,(NE,CLC),=C'OK',BC,LIBNAMEX                               
         ENDIF                                                                  
         L     R9,TEMPA                RESTORE DSECT ADDRESS                    
*                                 build command to send to bim-edit             
         XR    R0,R0                   CLEAR LENGTH ACCUMULATOR                 
         LA    R3,LINED                SET ADDRESS OF BIMEDIT LINE              
         LA    R9,PARSLENG(,R9)   POINT TO FIRST PART OF COMMAND                
         IF    PARSDATA,NH,C' ',OR,    IF COMMAND OMITTED              X        
               PARSDATA,NL,X'FF'                                                
          OI   OPT1,OPTERR              SET ON ERROR FLAG                       
          LA   R1,L'SNDREQ              GET LENGTH OF ERROR MESSAGE             
          LA   R2,SNDREQ                GET ADDRESS OF ERROR MESSAGE            
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
          B    SENDCMDX                 GO CLOSE API AND RETURN ERROR           
         ENDIF                         ENDIF                                    
         LA    R1,L'PARSDATA           GET LENGTH OF PARSED DATA                
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO SECOND PART OF COMMAND               
         LA    R1,L'PARSDATA           GET LENGTH OF PARSED DATA                
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         LA    R9,PARSLENG(,R9)   POINT TO THIRD PART OF COMMAND                
         LA    R1,L'PARSDATA           GET LENGTH OF PARSED DATA                
         LA    R2,PARSDATA             GET ADDRESS OF PARSED DATA               
         BAL   R4,APPEND               PUT IN BIMEDIT LINE                      
         STH   R0,LINEL                PUT ACCUMULATED LENGTH IN PARM           
         BAL   R8,SENDBIM              SEND COMMAND STRING TO BIMEDIT           
         IF    STATUS,(NE,CLC),=C'OK',BC,SENDCMDX                               
         ENDIF                                                                  
         BAL   R8,RECVBIM              RECEIVE BIMEDIT RESPONSE                 
         IF    STATUS,(NE,CLC),=C'OK',BC,SENDCMDX                               
         ENDIF                                                                  
*                                 make sure a confirmation msg is sent          
         IF    R1,(Z,ICM,B'0011'),LINEL   IF NO MESSAGE FROM BIM-EDIT           
          LA   R1,L'SENDOK              GET LENGTH OF MESSAGE                   
          LA   R2,SENDOK                GET ADDRESS OF MESSAGE                  
          BAL  R7,PRTMSG                GO PRINT MESSAGE ON SYSLST              
         ENDIF                         ENDIF                                    
*                                                                               
SENDCMDX DS    0H                                                               
         B     BIMCLS                  EXECUTE BIM-EDIT CLOSE FUNCTION          
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
STORAGE  DS    0D                                                               
* ------------------------------------------------------------------- *         
SALEN    DC    A(DYNALENG)                                                      
*                                 FUNCTIONS...                                  
KMEMOPN  DC    F'0'                    MEMBER OPEN                              
KMEMCLS  DC    F'4'                    MEMBER CLOSE                             
KMEMINP  DC    F'8'                    MEMBER "I"NPUT (READ)                    
KMEMOUT  DC    F'12'                   MEMBER "O"UTPUT (WRITE)                  
KDIROPN  DC    F'16'                   DIRECTORY OPEN                           
KDIRLST  DC    F'20'                   DIRECTORY "L"IST (READ)                  
KDIRCLS  DC    F'24'                   DIRECTORY CLOSE                          
KMEMDEL  DC    F'28'                   "D"ELETE MEMBER                          
KMEMRNM  DC    F'32'                   RE"N"AME MEMBER                          
KLIBCRE  DC    F'36'                   "M"AKE LIBRARY                           
KLIBDEL  DC    F'40'                   "R"EMOVE LIBRARY                         
KSNDCMD  DC    F'44'                   "S"END COMMAND                           
         LTORG                                                                  
*                                 API COMMANDS...                               
DEFAPPL  DC    CL8'BIMEDIT'                                                     
COMMA    DC    C','                                                             
LOGON    DC    C'LOGON '                                                        
PPDCOND  DC    C'SET PPDCOND 2'                                                 
ATTACH   DC    C'ATTACH '                                                       
HOME     DC    C',HOME'                                                         
SEND     DC    C'SEND '                                                         
SLASH    DC    C'/'                                                             
DEFINE   DC    C'DEFINE '                                                       
TEXT     DC    C',(TEXT)'                                                       
TITLE    DC    C',"CREATED VIA FTP"'                                            
EDIT     DC    C'EDIT'                                                          
INSERTF  DC    C'INSERTF'                                                       
EXEC     DC    C'EXEC $SIT.PROC.'                                               
BSTTLIBL DC    C'BSTTLIBL'                                                      
BSTTMEML DC    C'BSTTMEML'                                                      
PURGE    DC    C'PURGE '                                                        
RENAME   DC    C'RENAME '                                                       
DEFINEL  DC    C'DEFINEL '                                                      
PURGEL   DC    C'PURGEL '                                                       
QUIT     DC    C'QUIT'                                                          
LOGOFF   DC    C'LOGOFF'                                                        
*                                 RECORD DELIMITERS...                          
CR       EQU   X'0D'                                                            
LF       EQU   X'25'                                                            
CRB      DC    YL1(CR)                                                          
         DC    YL1(LF)                                                          
CRLF     EQU   CRB,*-CRB,C'X'                                                   
*                                 API ERROR MESSAGE                             
BUFPFX   DC    C'                    ==> '                                      
AMOPEN   DC    CL5'OPEN'                                                        
AMCLOSE  DC    CL5'CLOSE'                                                       
AMRECV   DC    CL5'RECV'                                                        
AMSEND   DC    CL5'SEND'                                                        
AMERRORB DC    C'ACCESS METHOD '                                                
AMERRORF EQU   *-AMERRORB,L'AMCLOSE,C'C'                                        
         DC    C'?????'                                                         
         DC    C' ERROR = '                                                     
AMERRORS EQU   *-AMERRORB,L'STATUS,C'C'                                         
         DC    C'??'                                                            
AMERROR  EQU   AMERRORB,*-AMERRORB,C'C'                                         
*                                 OTHER MESSAGES...                             
INVFUNC  DC    C'INVALID OR UNSUPPORTED FUNCTION'                               
CALLERR  DC    C': CALLING SEQUENCE ERROR'                                      
APIERR   DC    C': API SEQUENCE ERROR'                                          
VISERR   DC    C': GETVIS ACQUISITION ERROR'                                    
VISREL   DC    C': GETVIS RELEASE ERROR'                                        
APIOPN   DC    C'BIM-EDIT API OPENED'                                           
EOFSTAT  DC    C'*EOF* STATUS RECEIVED'                                         
BUFINP   DC    C'LOADING INPUT BUFFER...'                                       
EOFINP   DC    C'-- END OF INPUT --'                                            
BUFOUT   DC    C'EXTRACTING OUTPUT BUFFER...'                                   
EOFOUT   DC    C'-- END OF OUTPUT --'                                           
MEMREQ   DC    C'REQUIRED MEMBER NAME MISSING'                                  
RNMREQ   DC    C'REQUIRED TO MEMBER NAME MISSING'                               
LIBREQ   DC    C'REQUIRED LIBRARY NAME MISSING'                                 
SNDREQ   DC    C'REQUIRED SEND COMMAND MISSING'                                 
SENDOK   DC    C'COMMAND SENT SUCCESSFULLY'                                     
APICLS   DC    C'BIM-EDIT API CLOSED'                                           
*                                 RECORD LENGTH ERROR MESSAGES                  
RECLMSGB DC    C'RECORD AFTER '                                                 
RECLMSGE EQU   *-RECLMSGB                                                       
RECLMSGF DC    X'4020202020202020212060'                                        
         DC    C'TOO LONG'                                                      
RECLMSG  EQU   RECLMSGB,*-RECLMSGB,C'C'                                         
INVLENG  DC    C'MAX BIM-EDIT RECORD LENGTH = 253'                              
*                                 RECORD COUNT MESSAGE                          
CNTMSGB  DC    C' '                                                             
CNTMSGE  EQU   *-CNTMSGB                                                        
CNTMSGF  DC    X'4020202020202020212060'                                        
         DC    C'RECORDS PROCESSED'                                             
CNTMSG   EQU   CNTMSGB,*-CNTMSGB,C'C'                                           
*                                 LIOCS I/O AREA                                
IOAREA   DS    0CL121                                                           
CTLCHR   DC    CL1' '                                                           
OUTPUT   DC    CL120' '                                                         
*                                 LIOCS INTERFACES                              
CONSOLE  DTFCN DEVADDR=SYSLOG,IOAREA1=OUTPUT,BLKSIZE=60                         
PRINTER  DTFPR DEVADDR=SYSLST,IOAREA1=IOAREA,BLKSIZE=121,CTLCHR=ASA             
* =================================================================== *         
         END                                                                    
