      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSAMSTAT.                                                 
       AUTHOR.        DAVE L CLARK.                                             
       DATE-WRITTEN.  AUG 2005.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  DAPSCO INFORMATION SYSTEMS.                               
      *REMARKS.       VSAM STATUS DISPLAY.                                      
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 08/29/2005 DLC ORIGINAL PROGRAM                                         
      * END OF HISTORY ------------------------------------------------         
                                                                                
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
      *                                                               *         
      *     V S A M   S T A T U S   D I S P L A Y   P R O G R A M     *         
      *                                                               *         
      *                                                               *         
      * DAPSCO Information Systems                    Written by:     *         
      * 3110 Kettering Blvd.                       Dave Leonard Clark *         
      * Dayton, OH  45439                            (937)294-5331    *         
      *                                                               *         
      * No warranty is expressed or implied.   Neither the author nor *         
      * the company is responsible  for  any loss or damage resulting *         
      * from the use of this  code.   Source  is provided on an as-is *         
      * basis.                                                        *         
      *                                                               *         
      * ============================================================= *         
      *                                                               *         
      *                            PURPOSE                            *         
      *                                                               *         
      *      This program merely  provides  a  standardized method of *         
      * displaying VSAM status  information  on  both  SYSLST and the *         
      * system console.                                               *         
      *                                                               *         
      * ============================================================= *         
      *                                                               *         
      *                        IMPLEMENTATION                         *         
      *                                                               *         
      *      At least three and up  to  five data areas may be passed *         
      * to this program.   The following sample COBOL copybook layout *         
      * demonstrates these areas.                                     *         
      *                                                               *         
      *    01  VSAMSTAT-AREAS.                                        *         
      *        05  VSAMSTAT            PIC  X(8)    VALUE 'VSAMSTAT'. *         
      *        05  VSAMSTAT-FILENAME   PIC  X(8)    VALUE SPACES.     *         
      *        05  VSAMSTAT-VERBNAME   PIC  X(8)    VALUE SPACES.     *         
      *        05  VSAMSTAT-STATUS     PIC  X(2)    VALUE SPACES.     *         
      *    01  VSAMSTAT-KEYAREA.                                      *         
      *        05  VSAMSTAT-KEYLENG    PIC S9(4)    BINARY VALUE ZERO.*         
      *        05  VSAMSTAT-KEYDATA    PIC  X(98)   VALUE SPACES.     *         
      *    01  VSAMSTAT-FEEDBACK.                                     *         
      *        05  VSAMSTAT-RC         PIC S9(4)    BINARY.           *         
      *        05  VSAMSTAT-FC         PIC S9(4)    BINARY.           *         
      *        05  VSAMSTAT-EC         PIC S9(4)    BINARY.           *         
      *                                                               *         
      * The exact structure  of  the  first  area  is not required as *         
      * shown -- as the sub  areas  must be passed separately to this *         
      * program anyway (see the  next section).   The exact structure *         
      * of the next two areas, however, is required exactly as shown. *         
      *                                                               *         
      * ============================================================= *         
      *                                                               *         
      *                          INVOCATION                           *         
      *                                                               *         
      *      This program is  invoked  from  another COBOL program in *         
      * various  ways  and  with   a   varying  number  of  arguments *         
      * (depending upon the  desired  result).   Up to five arguments *         
      * are supported.  The first three arguments are required -- but *         
      * may be passed  either  by  reference  or  by content (as this *         
      * program does not modify any of its arguments).   The last two *         
      * arguments are  optional  and,  if  omitted,  merely  serve to *         
      * reduce the amount of diagnostic information displayed.        *         
      *                                                               *         
      *      The following shows a few  of the main ways for invoking *         
      * this program.  Feel free to mix-n-match as suits your needs.  *         
      *                                                               *         
      *    CALL VSAMSTAT USING BY CONTENT                             *         
      *                        'MYFILE  '                             *         
      *                        'READNEXT'                             *         
      *                        BY REFERENCE                           *         
      *                        VSAMSTAT-STATUS.                       *         
      *                                                               *         
      *                            - or -                             *         
      *                                                               *         
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *         
      *                        VSAMSTAT-VERBNAME                      *         
      *                        VSAMSTAT-STATUS.                       *         
      *                                                               *         
      *                            - or -                             *         
      *                                                               *         
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *         
      *                        VSAMSTAT-VERBNAME                      *         
      *                        VSAMSTAT-STATUS                        *         
      *                        VSAMSTAT-KEYAREA.                      *         
      *                                                               *         
      *                            - or -                             *         
      *                                                               *         
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *         
      *                        VSAMSTAT-VERBNAME                      *         
      *                        VSAMSTAT-STATUS                        *         
      *                        OMITTED                                *         
      *                        VSAMSTAT-FEEDBACK.                     *         
      *                                                               *         
      *                            - or -                             *         
      *                                                               *         
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *         
      *                        VSAMSTAT-VERBNAME                      *         
      *                        VSAMSTAT-STATUS                        *         
      *                        VSAMSTAT-KEYAREA                       *         
      *                        VSAMSTAT-FEEDBACK.                     *         
      *                                                               *         
      *                           - etc. -                            *         
      *                                                               *         
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    ENVIRONMENT DIVISION                                        *        
      *                                                                *        
      ******************************************************************        
       ENVIRONMENT DIVISION.                                                    
                                                                                
      ******************************************************************        
      *    CONFIGURATION SECTION                                       *        
      ******************************************************************        
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-2086-A04-140.                                       
       OBJECT-COMPUTER. IBM-2086-A04-140.                                       
                                                                                
       SPECIAL-NAMES.                                                           
           SYSLST IS PRINTER.                                                   
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    DATA DIVISION                                               *        
      *                                                                *        
      ******************************************************************        
       DATA DIVISION.                                                           
                                                                                
      /*****************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       77                              PIC  X(8)    VALUE '===WS==>'.           
       77  MSG-LENG                    PIC S9(4)    BINARY.                     
       77  MSG-AREA                    PIC  X(100)  VALUE SPACES.               
       77  SUB                         PIC  9       VALUE ZEROES.               
       77  DEC-OUTP                    PIC  999     VALUE ZEROES.               
       77  PERIODS-65                  PIC  X(65)   VALUE ALL '.'.              
       77  NON-DISPLAY                 PIC  X(65)   VALUE                       
                                      X'000102030405060708090A0B0C0D0E0F        
      -                                '101112131415161718191A1B1C1D1E1F        
      -                                '202122232425262728292A2B2C2D2E2F        
      -                                '303132333435363738393A3B3C3D3E3F        
      -                                'FF'.                                    
                                                                                
       01  FUNC-DATA.                                                           
         03  PIC S9(4) VALUE +15 BINARY.                                        
         03  PIC X(40) VALUE 'in base access.'.                                 
         03  PIC S9(4) VALUE +39 BINARY.                                        
         03  PIC X(40) VALUE 'in base access -- error in upgrade set.'.         
         03  PIC S9(4) VALUE +14 BINARY.                                        
         03  PIC X(40) VALUE 'in aix access.'.                                  
         03  PIC S9(4) VALUE +38 BINARY.                                        
         03  PIC X(40) VALUE 'in aix access -- error in upgrade set.'.          
         03  PIC S9(4) VALUE +40 BINARY.                                        
         03  PIC X(40) VALUE 'during upgrade -- no err in upgrade set.'.        
         03  PIC S9(4) VALUE +39 BINARY.                                        
         03  PIC X(40) VALUE 'during upgrade -- error in upgrade set.'.         
       01  FUNC-TABLE        REDEFINES FUNC-DATA.                               
         03  FUNC-ENTRY                             OCCURS 6.                   
           05  FUNC-LEN                PIC S9(4)    BINARY.                     
           05  FUNC-MSG                PIC  X(40).                              
                                                                                
       01  DOUBLE-WORD                 PIC S9(18)   BINARY.                     
       01  EIGHT-BYTES       REDEFINES DOUBLE-WORD.                             
         03  FILLER                    PIC  X(4).                               
         03  MEMORY-ADDRESS                         POINTER.                    
         03  FULL-WORD       REDEFINES MEMORY-ADDRESS                           
                                       PIC S9(9)    BINARY.                     
         03  FOUR-BYTES      REDEFINES FULL-WORD.                               
           05  HI-MSB                  PIC  X.                                  
           05  THREE-BYTES.                                                     
             07  HI-LSB                PIC  X.                                  
             07  HALF-WORD             PIC S9(4)    BINARY.                     
             07  TWO-BYTES   REDEFINES HALF-WORD.                               
               09  LO-MSB              PIC  X.                                  
               09  ONE-BYTE            PIC  X.                                  
                                                                                
       COPY HEXMAN.                                                             
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
       01  VSAMSTAT-FILENAME           PIC  X(8).                               
       01  VSAMSTAT-VERBNAME           PIC  X(8).                               
       01  VSAMSTAT-STATUS             PIC  X(2).                               
       01  VSAMSTAT-KEYAREA.                                                    
           05  VSAMSTAT-KEYLENG        PIC S9(4)    BINARY.                     
           05  VSAMSTAT-KEYDATA        PIC  X(98).                              
       01  VSAMSTAT-FEEDBACK.                                                   
           05  VSAMSTAT-RC             PIC  XX.                                 
           05  VSAMSTAT-FC             PIC S9(4)    BINARY.                     
           05  VSAMSTAT-EC             PIC  XX.                                 
                                                                                
       01  LOW-MEMORY.                                                          
           05  FILLER                  PIC  X(20).                              
           05  COMREG-POINTER          POINTER.                                 
                                                                                
       01  COMREG-BLOCK.                                                        
      *  JOB DATE WITH CENTURY                                                  
           05  COMREG-JOBDATWC.                                                 
      *    JOB DATE                                                             
             07  COMREG-JOBDATE        PIC  X(8).                               
      *    CENTURY                                                              
             07  FILLER                PIC  X(3).                               
           05  FILLER                  PIC  X(1).                               
      *  USER SCRATCH AREA                                                      
           05  COMREG-COMUSCR          PIC  X(11).                              
      *  UPSI BYTE                                                              
           05  COMREG-UPSI             PIC  X(1).                               
      *  VSE JOB NAME                                                           
           05  COMREG-COMNAME          PIC  X(8).                               
      *  HIGHEST STORAGE ADDR OF PARTITION                                      
           05  COMREG-PPEND            PIC  X(4).                               
      *  END ADDR OF LAST PHASE LOADED                                          
           05  COMREG-HIPHAS           PIC  X(4).                               
      *  END ADDR OF LONGEST PHASE LOADED                                       
           05  COMREG-HIPROG           PIC  X(4).                               
      *  LENGTH OF PP LABEL AREA                                                
           05  COMREG-LABLEN           PIC S9(4)    COMP.                       
      *  PROGRAM INTERRUPT KEY (PIK)                                            
           05  COMREG-PID              PIC S9(4)    COMP.                       
      *  END OF VIRTUAL STORAGE (LAST BYTE IN ADDRESS SPACE)                    
           05  COMREG-EOCADR           PIC  X(4).                               
      *  MACHINE CONFIGURATION BYTE                                             
           05  COMREG-CONFIG           PIC  X(1).                               
      *  SYSTEM CONFIGURATION BYTE                                              
           05  COMREG-LTACT            PIC  X(1).                               
      *  STDJC SWITCH 1                                                         
           05  COMREG-SOB1             PIC  X(1).                               
      *  STDJC SWITCH 2                                                         
           05  COMREG-SOB2             PIC  X(1).                               
      *  JOB CONTROL SWITCHES                                                   
           05  COMREG-JCSW1            PIC  X(1).                               
      *  LINKAGE CONTROL BYTE                                                   
           05  COMREG-JCSW2            PIC  X(1).                               
      *  LANGUAGE TRANSLATOR CONTROL BYTE                                       
           05  COMREG-JCSW3            PIC  X(1).                               
      *  JOB DURATION INDICATOR BYTE                                            
           05  COMREG-JCSW4            PIC  X(1).                               
      *  JOB CONTROL SWITCH                                                     
           05  COMREG-JCSW7            PIC  X(1).                               
      *  PROCEDURE NESTING LEVEL                                                
           05  COMREG-NSTLEVEL         PIC  X(1).                               
      *  JOB CONTROL FLAGS                                                      
           05  COMREG-JCSW8            PIC  X(1).                               
           05  FILLER                  PIC  X(1).                               
      *  ADDRESS OF PUB TABLE                                                   
           05  COMREG-PUBPT            PIC S9(4)    COMP.                       
      *  JOB START TIME                                                         
           05  COMREG-IJBJOBST         PIC  X(6).                               
      *  ADDRESS OF FICL                                                        
           05  COMREG-FICLPT           PIC S9(4)    COMP.                       
      *  ADDRESS OF NICL                                                        
           05  COMREG-NICLPT           PIC S9(4)    COMP.                       
      *  ADDRESS OF LUB TABLE                                                   
           05  COMREG-LUBPT            PIC S9(4)    COMP.                       
      *  SYSLST LINE COUNT                                                      
           05  COMREG-SYSLINE          PIC  X(1).                               
      *  SYSTEM DATE                                                            
           05  COMREG-SYSDATE.                                                  
      *    MMDD OR DDMM                                                         
             07  COMREG-MMDD           PIC  9(4).                               
      *    YYDDD PORTION OF DATE                                                
             07  COMREG-YYDDD          PIC  9(5).                               
      *  LIOCS COMMUNICATIONS BYTES                                             
           05  COMREG-LIOCSCOM.                                                 
      *    LIOCS SWITCH 1                                                       
             07  COMREG-LIOCSSW1       PIC  X(1).                               
      *    LIOCS SWITCH 2                                                       
             07  COMREG-LIOCSSW2       PIC  X(1).                               
      *  ADDRESS OF PIB TABLE                                                   
           05  COMREG-PIBPT            PIC S9(4)    COMP.                       
      *  LAST CHECKPOINT NUMBER                                                 
           05  COMREG-CHKPTID          PIC S9(4)    COMP.                       
      *  JOB ZONE IN MINUTES                                                    
           05  COMREG-JOBZON           PIC S9(4)    COMP.                       
      *  BACKGROUND DIB POINTER                                                 
           05  COMREG-DIBPT            PIC S9(4)    COMP.                       
      *  DEVICE FLAG                                                            
           05  COMREG-DEVFLG1          PIC  X(1).                               
      *  FLAG BYTE                                                              
           05  COMREG-OPNBYT2          PIC  X(1).                               
      *  CENTURY OF SYSDATE                                                     
           05  COMREG-SYSCENT          PIC S9(4)    COMP.                       
           05  FILLER                  PIC  X(4).                               
      *  KEY OF PROGRAM WITH TI SUPPORT                                         
           05  COMREG-PWTIMS           PIC S9(4)    COMP.                       
      *  ACTIVE SPACE ID OF PART.                                               
           05  COMREG-IJBASID          PIC XX.                                  
      *  LOGICAL TRANSIENT KEY                                                  
           05  COMREG-LTK              PIC S9(4)    COMP.                       
      *  ADDRESS OF SYSPARM FIELD                                               
           05  COMREG-SYSPARM          POINTER.                                 
      *  POINTER TO JA PARTITION TABLE                                          
           05  COMREG-JAPART           POINTER.                                 
      *  ADDR OF TOD CLOCK COMM AREA                                            
           05  COMREG-TODCOM           POINTER.                                 
      *  ADDRESS OF PIB EXTENSION                                               
           05  COMREG-PIB2PTR          PIC S9(4)    COMP.                       
      *  ADDR OF MICR DTF TABLE                                                 
           05  COMREG-PDTABB           PIC S9(4)    COMP.                       
      *  ADDR OF LABEL SPACE                                                    
           05  COMREG-LABELPTR         POINTER.                                 
      *  RESERVED                                                               
           05  COMREG-BGCOMPT          PIC S9(4)    COMP.                       
      *  OPTION INDICATOR BYTE                                                  
           05  COMREG-OPTNBYTE         PIC  X(1).                               
      *  SYSTEM CONFIG BYTE 2                                                   
           05  COMREG-RMSROPEN         PIC  X(1).                               
      *  A(JOB CONTROL WORK AREA)                                               
           05  COMREG-IJBJCWA          POINTER.                                 
      *  STANDARD OPTIONS                                                       
           05  COMREG-STDOPT           PIC  X(1).                               
      *  TEMPORARY OPTIONS                                                      
           05  COMREG-TEMOPT           PIC  X(1).                               
      *  DISK CONFIG BYTE                                                       
           05  COMREG-DISKCONF         PIC  X(1).                               
      *  PROCEDURE NAME VB                                                      
           05  COMREG-PROCNAM          PIC  X(8).                               
      *  INTERFACE FLAGS FOR CAT. PROC. VB                                      
           05  COMREG-PSWTCH           PIC  X(1).                               
      *  STATEMENT NAME VB                                                      
           05  COMREG-POVNAM           PIC  X(7).                               
      *  INDICATOR 81 BYTE                                                      
           05  COMREG-INSIZE           PIC  X(1).                               
      *  ADDR OF PART CONT BLOCK                                                
           05  COMREG-POWPCB           POINTER.                                 
      *  POWER/VS FLAG BYTE 1                                                   
           05  COMREG-POWFLG1          PIC  X(1).                               
      *  POWER/VS FLAG BYTE 2                                                   
           05  COMREG-POWFLG2          PIC  X(1).                               
      *  VSAM SNAP FUNCTION BYTES                                               
           05  COMREG-IJBVSSNP         PIC  X(2).                               
      *  ADDR OF LUB TABLE EXTENS                                               
           05  COMREG-LUBEXT           POINTER.                                 
      *  JOB CONTROL SWITCH 5                                                   
           05  COMREG-JCSW5            PIC  X(1).                               
      *  JOB CONTROL SWITCH 6                                                   
           05  COMREG-JCSW6            PIC  X(1).                               
      *  STANDARD OPTIONS 2                                                     
           05  COMREG-STDOPT2          PIC  X(1).                               
      *  TEMPORARY OPTIONS 2                                                    
           05  COMREG-TEMOPT2          PIC  X(1).                               
      *  ADDR OF JPL OF PARTITION                                               
           05  COMREG-IJBJPL           POINTER.                                 
      *  SLOT FOR CICS                                                          
           05  COMREG-IJBAFCB          POINTER.                                 
      *  ADDR.FETCH/LOAD TRACE TAB                                              
           05  COMREG-IJBPHLST         POINTER.                                 
      *  ADDR.OF LAST HC JOBSTMNT                                               
           05  COMREG-IJBJOBLG.                                                 
      *    CYCLE BYTE OF JOBSTMNT                                               
             07  COMREG-IJBDSKCB       PIC  X(1).                               
      *    DISK ADDRESS OF JOBSTMNT                                             
             07  COMREG-IJBDSKAD       PIC  X(5).                               
      *    LOGICAL REC. OF JOBSTMNT                                             
             07  COMREG-IJBDSKLR       PIC  X(2).                               
      *  A(SPF CONTROL AREA)                                                    
           05  COMREG-IJBASPF          POINTER.                                 
      *  ADDR.OF GETVIS CNTL.INF.                                               
           05  COMREG-IJBGVCTL         POINTER.                                 
      *  A(TAPE FILE CNTL BLOCKS)                                               
           05  COMREG-IJBIJJT          POINTER.                                 
      *  SYSTEM GETVIS PART.POOL                                                
           05  COMREG-IJBJOBLG.                                                 
      *    FIXED PART OF THE NAME                                               
             07  COMREG-IJBSPFIX       PIC  X(4).                               
      *    VARIABLE PART IN SUBPOOL NAME (SYSLOG ID)                            
             07  COMREG-IJBSPNLI       PIC  X(2).                               
      *    SUBPOOL INDEX                                                        
             07  COMREG-IJBSPIND       PIC  X(2).                               
      *  EXEC PHASE NAME                                                        
           05  COMREG-IJBPHNAM         PIC  X(8).                               
      *  MIRROR DE ENTRY CHAIN                                                  
           05  COMREG-IJBDECPY         POINTER.                                 
      *  PARTITION UNBATCH ECB                                                  
           05  COMREG-IJBECBU.                                                  
      *    USED BY POWER                                                        
             07  COMREG-IJBECBLN       PIC  X(2).                               
      *    ECB BYTE                                                             
             07  COMREG-IJBECBUB       PIC  X(1).                               
      *    USED BY POWER                                                        
             07  COMREG-IJBECBU2       PIC  X(1).                               
      *  JCL SWITCH 9                                                           
           05  COMREG-JCSW9            PIC  X(1).                               
      *  MISC. SWITCH BYTE                                                      
           05  COMREG-IJBMSWB1         PIC  X(1).                               
      *  IND.FOR VSAM SNAP DUMP O.                                              
           05  COMREG-IJBVSMLU         PIC  X(1).                               
           05  FILLER                  PIC  X(1).                               
      *  JCL BASE VECTOR                                                        
           05  COMREG-IJBJCLBV         POINTER.                                 
      *  ADDRESS OF PART.PCE                                                    
           05  COMREG-IJBPCEPT         POINTER.                                 
      *  END ADDRESS OF HIGHEST LOADED PHASE                                    
           05  COMREG-IJBHPHLA         PIC  X(4).                               
      *  VMCF USERID                                                            
           05  COMREG-IJBOWNID         PIC  X(8).                               
      *  VMCF PROCESSING FLAG 1                                                 
           05  COMREG-IJBVMCF1         PIC  X(1).                               
      *  TEMPORARY ECHO LEVEL                                                   
           05  COMREG-IJBTELVL         PIC  X(1).                               
      *  POWER FLAG BYTE 3                                                      
           05  COMREG-POWFLG3          PIC  X(1).                               
      *  JCL STANDARD OPTIONS 3                                                 
           05  COMREG-IJBSTOP3         PIC  X(1).                               
      *  JCL TEMP. OPTIONS 3                                                    
           05  COMREG-IJBTMOP3         PIC  X(1).                               
      *  REXX FLAG BYTE                                                         
           05  COMREG-IJBRXFLG         PIC  X(1).                               
           05  FILLER                  PIC  X(2).                               
      *  PERM. PFIX LIMIT BELOW                                                 
           05  COMREG-IJBPFLBP         PIC  X(4).                               
      *  PERM. PFIX LIMIT ABOVE                                                 
           05  COMREG-IJBPFLAP         PIC  X(4).                               
      *  USERID OF POWER JOB                                                    
           05  COMREG-IJBPOWUI         PIC  X(8).                               
      *  JCL ENTRY POINT (REXX)                                                 
           05  COMREG-IJBRXJCL         POINTER.                                 
      *  UTILITY RC (SA ENV.)                                                   
           05  COMREG-IJBSARC          PIC  X(4).                               
      *  SA UTILITY CB                                                          
           05  COMREG-IJBSAUCB         POINTER.                                 
           05  FILLER                  PIC  X(8).                               
                                                                                
       01  SYSPARM-BLOCK.                                                       
           05  SYSPARM-LENGTH          PIC  X(1).                               
           05  SYSPARM-VALUE           PIC  X(8).                               
                                                                                
       01  TODCOM-BLOCK.                                                        
           05  TODCOM-CURDATE          PIC  9(6).                               
           05  FILLER                  PIC  X(4).                               
           05  TODCOM-ZONEOFS          PIC S9(4)    COMP.                       
           05  TODCOM-CENTURY          PIC  9(2).                               
           05  TODCOM-ZONEID           PIC  X(3).                               
           05  FILLER                  PIC  X(3).                               
                                                                                
       01  PCE-BLOCK.                                                           
      *  ABNORMAL TERMINATION ECB TO POST POWER FOR DEALLOCATION                
           05  PCE-PCELEN.                                                      
      *    LENGTH OF PCE                                                        
             07  PCE-PCELEN            PIC S9(4)    COMP.                       
      *    FLAG BYTE                                                            
             07  PCE-PCEFLAG           PIC  X(1).                               
      *    FLAG BYTE                                                            
             07  PCE-PCEFLAG1          PIC  X(1).                               
      *  PARTITION'S STORAGE KEY                                                
           05  PCE-PCEKEY              PIC  X(1).                               
      *  DYNCLASS ID=HOLD COUNT                                                 
           05  PCE-PCEHCNT             PIC  X(1).                               
           05  FILLER                  PIC  X(2).                               
      *  PARTITION'S IDENTIFICATION KEY                                         
           05  PCE-PCEPIK              PIC S9(4)    COMP.                       
      *  PARTITION'S SYSLOG ID                                                  
           05  PCE-PCELID              PIC  X(2).                               
      *  PARTITION'S COMREG ADDRESS                                             
           05  PCE-PCECOMRA            POINTER.                                 
      *  PARTITION'S LUBTAB ADDRESS                                             
           05  PCE-PCEALUB             POINTER.                                 
      *  PARTITION'S DIB ADDRESS                                                
           05  PCE-PCEADIB             POINTER.                                 
      *  PARTITION'S PIB ADDRESS                                                
           05  PCE-PCEPIB              POINTER.                                 
      *  PARTITION'S PIB2 ADDRESS                                               
           05  PCE-PCEPIB2             POINTER.                                 
      *  PARTITION'S CLASS INFORMATION                                          
           05  PCE-PCECLASS            POINTER.                                 
      *  VSE/POWER JOB INFORMATION ADDRESS                                      
           05  PCE-PCEPOWJB            POINTER.                                 
                                                                                
       01  PJB-BLOCK.                                                           
      *  VSE/POWER JOB NAME                                                     
           05  PJB-PJBPNAME            PIC  X(8).                               
      *  VSE/POWER JOB START TIME                                               
           05  PJB-PJBPTIME            PIC  X(8).                               
      *  VSE/AF JOB STEP START (// EXEC)                                        
           05  PJB-PJBJTIME            PIC  X(8).                               
      *  VSE/AF JOB - MAX. RETURN CODE                                          
           05  PJB-PJBJRET             PIC S9(9)    COMP.                       
      *  VSE/POWER JOB NUMBER                                                   
           05  PJB-PJBPNUM             PIC  X(2).                               
           05  FILLER                  PIC  X(2).                               
      *  ADDRESS OF USER TOKEN                                                  
           05  PJB-PJBRUTKN            POINTER.                                 
      *  VSE/POWER JOB: FROM USERID                                             
           05  PJB-PJBPUSER            PIC  X(8).                               
      *  VSE/POWER JOB: USER INFORMATION                                        
           05  PJB-PJBPUINF            PIC  X(16).                              
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION                                                       
           USING VSAMSTAT-FILENAME                                              
                 VSAMSTAT-VERBNAME                                              
                 VSAMSTAT-STATUS                                                
                 VSAMSTAT-KEYAREA                                               
                 VSAMSTAT-FEEDBACK.                                             
                                                                                
       A00-MAINLINE-ROUTINE.                                                    
                                                                                
      * First, point to low memory at x'00000000' and,                          
           SET ADDRESS OF LOW-MEMORY   TO NULL.                                 
                                                                                
      * from there, offset x'14'(020) is the pointer to COMREG.                 
           SET ADDRESS OF COMREG-BLOCK TO COMREG-POINTER.                       
                                                                                
      *   At COMREG offset x'70'(112) is the pointer to SYSPARM.                
           SET ADDRESS OF SYSPARM-BLOCK TO COMREG-SYSPARM.                      
                                                                                
      *   At COMREG offset x'78'(120) is the pointer to TODCOM.                 
           SET ADDRESS OF TODCOM-BLOCK TO COMREG-TODCOM.                        
                                                                                
      *   At COMREG offset x'F0'(240) is the pointer to PCE.                    
           SET ADDRESS OF PCE-BLOCK    TO COMREG-IJBPCEPT.                      
                                                                                
      *  At the PCE offset x'24'(036) is the pointer to PJB.                    
           SET ADDRESS OF PJB-BLOCK    TO PCE-PCEPOWJB.                         
                                                                                
           MOVE 1                      TO MSG-LENG.                             
           STRING COMREG-IJBPHNAM         DELIMITED BY SPACE                    
                  ': '                    DELIMITED BY SIZE                     
                  VSAMSTAT-FILENAME       DELIMITED BY SPACE                    
                  ' '                     DELIMITED BY SIZE                     
                  VSAMSTAT-VERBNAME       DELIMITED BY SPACE                    
                  ' ERROR; VSAMSTAT='                                           
                  VSAMSTAT-STATUS         DELIMITED BY SIZE                     
               INTO MSG-AREA         WITH POINTER MSG-LENG.                     
           PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT.                           
                                                                                
           IF  ADDRESS OF VSAMSTAT-KEYAREA NOT = NULL                           
             IF  VSAMSTAT-KEYLENG > ZERO                                        
             AND VSAMSTAT-VERBNAME NOT = 'OPEN'                                 
             AND VSAMSTAT-VERBNAME NOT = 'CLOSED'                               
               MOVE VSAMSTAT-KEYLENG   TO HEX-LENG                              
               MOVE VSAMSTAT-KEYDATA   TO HEX-CHRT                              
               MOVE 1                  TO MSG-LENG                              
               STRING COMREG-IJBPHNAM     DELIMITED BY SPACE                    
                      ': KEY="'                                                 
                      HEX-CHRT '"'        DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               IF  MSG-LENG > LENGTH OF MSG-AREA                                
                   SUBTRACT 3        FROM LENGTH OF MSG-AREA                    
                                   GIVING MSG-LENG                              
                   STRING '..."'          DELIMITED BY SIZE                     
                       INTO MSG-AREA WITH POINTER MSG-LENG                      
               END-IF                                                           
               PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT                        
             END-IF                                                             
           END-IF.                                                              
                                                                                
           IF  ADDRESS OF VSAMSTAT-FEEDBACK NOT = NULL                          
             IF  VSAMSTAT-FEEDBACK > LOW-VALUES                                 
               MOVE 1                  TO MSG-LENG                              
               STRING COMREG-IJBPHNAM     DELIMITED BY SPACE                    
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               EVALUATE VSAMSTAT-VERBNAME                                       
               WHEN 'OPEN'                                                      
                 STRING ': VSAMOPEN'      DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               WHEN 'CLOSE'                                                     
                 STRING ': VSAMCLOS'      DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               WHEN OTHER                                                       
                 STRING ': VSAMREQU'      DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               END-EVALUATE                                                     
               SET HEX-DECODE-FROM-CHAR TO TRUE                                 
               MOVE 1                  TO HEX-LENG                              
               MOVE VSAMSTAT-RC        TO TWO-BYTES                             
               MOVE HALF-WORD          TO DEC-OUTP                              
               MOVE ONE-BYTE           TO HEX-CHRT                              
               CALL HEXMAN          USING HEXMAN-PARMS                          
               STRING ' RC=x''' HEX-HEXT                                        
                      '''(' DEC-OUTP ')'  DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               MOVE VSAMSTAT-EC        TO TWO-BYTES                             
               MOVE HALF-WORD          TO DEC-OUTP                              
               MOVE ONE-BYTE           TO HEX-CHRT                              
               CALL HEXMAN          USING HEXMAN-PARMS                          
               STRING ', EC=x''' HEX-HEXT                                       
                      '''(' DEC-OUTP ')'  DELIMITED BY SIZE                     
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               ADD  1                  TO VSAMSTAT-FC                           
                                   GIVING SUB                                   
               STRING ' '                                                       
                      FUNC-MSG(SUB)(1:FUNC-LEN(SUB)) DELIMITED BY SIZE          
                   INTO MSG-AREA     WITH POINTER MSG-LENG                      
               PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT                        
             END-IF                                                             
           END-IF.                                                              
                                                                                
       A90-MAINLINE-EXIT.                                                       
                                                                                
           MOVE ZEROES                 TO RETURN-CODE.                          
           GOBACK.                                                              
                                                                                
       B00-DISPLAY-ROUTINE.                                                     
           SUBTRACT 1                FROM MSG-LENG.                             
           IF  MSG-LENG > LENGTH OF MSG-AREA                                    
               MOVE LENGTH OF MSG-AREA TO MSG-LENG                              
           END-IF.                                                              
           DISPLAY MSG-AREA(1:MSG-LENG) UPON PRINTER.                           
           INSPECT MSG-AREA(1:MSG-LENG)                                         
               CONVERTING NON-DISPLAY  TO PERIODS-65.                           
           DISPLAY MSG-AREA(1:MSG-LENG) UPON CONSOLE.                           
       B90-EXIT.                                                                
           EXIT.                                                                
                                                                                
