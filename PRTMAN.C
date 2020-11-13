      *                                                                         
      * CICS PRINT MANAGER SUBROUTINE DATA STRUCTURE                            
      *                                                                         
       01  PRT-PGM-COMM.                                                        
         02  PRT-PGM-NAME              PIC  X(08)   VALUE 'PRTMAN  '.           
         02  PRT-PGM-COML              PIC S9(04)   COMP VALUE +4250.           
         02  PRT-LNK-REQU              PIC  X(01)   VALUE SPACES.               
             88  PRINT-ASA-CC                       VALUES ' ' '-' '+',         
                                                          '0' THRU '9',         
                                                          'A' THRU 'C'.         
             88  PRINT-DELSPOOL                     VALUE 'D'.                  
             88  PRINT-ENDOFMSG                     VALUE 'E'.                  
             88  PRINT-FORMFEED                     VALUE 'F'.                  
             88  PRINT-NEWLINE                      VALUE 'N'.                  
             88  PRINT-OMIT-CC                      VALUE 'O'.                  
             88  PRINT-PCL-CODE                     VALUE 'P'.                  
             88  PRINT-RETURN                       VALUE 'R'.                  
         02  PRT-LNK-RETN              PIC  X(01)   VALUE SPACES.               
             88  PRINT-COMPLETED                    VALUE ' '.                  
             88  PRINT-REQU-ERROR                   VALUE 'E'.                  
             88  PRINT-PCL-INVALID                  VALUE 'I'.                  
             88  PRINT-LENG-ERROR                   VALUE 'L'.                  
             88  PRINT-PRTR-ERROR                   VALUE 'P'.                  
             88  PRINT-QUEUE-ERROR                  VALUE 'Q'.                  
             88  PRINT-SPOOL-ERROR                  VALUE 'S'.                  
             88  PRINT-UNEX-ERROR                   VALUE 'X'.                  
         02  PRT-LNK-RESP              PIC S9(08)   COMP VALUE ZEROES.          
         02  PRT-LNK-RSP2              PIC S9(08)   COMP VALUE ZEROES.          
         02  PRT-LNK-LCNT              PIC S9(04)   COMP VALUE +0001.           
         02  PRT-LNK-TERM              PIC  X(02)   VALUE LOW-VALUES.           
      *                                                                         
         02  PRT-PWR-JQUE              PIC  X(03)   VALUE LOW-VALUES.           
           88  PRT-PWR-NONE                         VALUE LOW-VALUES.           
           88  PRT-PWR-LSTQ                         VALUE 'LST'.                
           88  PRT-PWR-PUNQ                         VALUE 'PUN'.                
           88  PRT-PWR-RDRQ                         VALUE 'RDR'.                
         02  PRT-PWR-JCLS              PIC  X(01)   VALUE 'A'.                  
         02  PRT-PWR-FNO               PIC  X(04)   VALUE SPACES.               
         02  PRT-PWR-JOBN              PIC  X(08)   VALUE SPACES.               
         02  PRT-PWR-NODE              PIC  X(08)   VALUE '*'.                  
         02  PRT-PWR-USER              PIC  X(08)   VALUE '*'.                  
         02  PRT-PWR-UINF              PIC  X(16)   VALUE SPACES.               
         02  PRT-PWR-TOKEN             PIC  X(08)   VALUE LOW-VALUES.           
      *                                                                         
         02  PRT-PCL-CONTROL           PIC  X(01)   VALUE 'E'.                  
             88  PRT-PCL-DISPLAY-OFF                VALUE 'Z'.                  
             88  PRT-PCL-DISPLAY-ON                 VALUE 'Y'.                  
             88  PRT-PCL-RESET                      VALUE 'E'.                  
         02  PRT-PCL-ORIENTATION       PIC  X(01)   VALUE '1'.                  
             88  PRT-PCL-PORTRAIT                   VALUE '0'.                  
             88  PRT-PCL-LANDSCAPE                  VALUE '1'.                  
             88  PRT-PCL-REV-PORTRAIT               VALUE '2'.                  
             88  PRT-PCL-REV-LANDSCAPE              VALUE '3'.                  
         02  PRT-PCL-BOLD              PIC  X(01)   VALUE LOW-VALUES.           
             88  PRT-PCL-BOLD-OFF                   VALUE '0'.                  
             88  PRT-PCL-BOLD-ON1                   VALUE '1'.                  
             88  PRT-PCL-BOLD-ON2                   VALUE '2'.                  
             88  PRT-PCL-BOLD-ON3                   VALUE '3'.                  
             88  PRT-PCL-BOLD-ON4                   VALUE '4'.                  
             88  PRT-PCL-BOLD-ON5                   VALUE '5'.                  
             88  PRT-PCL-BOLD-ON6                   VALUE '6'.                  
             88  PRT-PCL-BOLD-ON7                   VALUE '7'.                  
         02  PRT-PCL-ITALIC            PIC  X(01)   VALUE LOW-VALUES.           
             88  PRT-PCL-ITALIC-OFF                 VALUE '0'.                  
             88  PRT-PCL-ITALIC-ON                  VALUE '1'.                  
         02  PRT-PCL-UNDERLINE         PIC  X(01)   VALUE LOW-VALUES.           
             88  PRT-PCL-UNDERLINE-OFF              VALUE '@'.                  
             88  PRT-PCL-UNDERLINE-ON-FIXED         VALUE '0'.                  
             88  PRT-PCL-UNDERLINE-ON-FLOAT         VALUE '3'.                  
         02  PRT-PCL-LINES-PER-INCH    PIC  9(01)   VALUE 8.                    
             88  PRT-PCL-1LINES-PER-INCH            VALUE 1.                    
             88  PRT-PCL-2LINES-PER-INCH            VALUE 2.                    
             88  PRT-PCL-3LINES-PER-INCH            VALUE 3.                    
             88  PRT-PCL-4LINES-PER-INCH            VALUE 4.                    
             88  PRT-PCL-5LINES-PER-INCH            VALUE 5.                    
             88  PRT-PCL-6LINES-PER-INCH            VALUE 6.                    
             88  PRT-PCL-7LINES-PER-INCH            VALUE 7.                    
             88  PRT-PCL-8LINES-PER-INCH            VALUE 8.                    
             88  PRT-PCL-9LINES-PER-INCH            VALUE 9.                    
         02  PRT-PCL-LINES-TOP-MARGIN  PIC S9(04)   COMP VALUE ZEROES.          
         02  PRT-PCL-LINES-OF-TEXT     PIC S9(04)   COMP VALUE ZEROES.          
         02  PRT-PCL-CHARS-PER-INCH    PIC S9(04)   COMP VALUE +13.             
         02  PRT-PCL-CHARS-LEFT-MARGIN PIC S9(04)   COMP VALUE ZEROES.          
         02  PRT-PCL-CHARS-RGHT-MARGIN PIC S9(04)   COMP VALUE ZEROES.          
      *                                                                         
         02  PRT-BFR-PARM.                                                      
           05  PRT-BFR-MAXL            PIC S9(04)   COMP VALUE +1920.           
           05  PRT-BFR-LENG            PIC S9(04)   COMP VALUE ZEROES.          
           05  PRT-BFR-DATA            PIC  X(4016).                            
      *                                                                         
         02  PRT-STR-PARM.                                                      
           05  PRT-STR-LENG            PIC S9(04)   COMP VALUE +132.            
           05  PRT-STR-DATA.                                                    
             08  PRT-STR-BYTE          PIC  X(01)   OCCURS 132 TIMES            
                                       DEPENDING ON PRT-STR-LENG.               
