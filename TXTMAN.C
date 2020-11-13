      *                                                                         
      * TEXT MANIPULATION SUBROUTINE DATA STRUCTURE                             
      *                                                                         
       01  TXTMAN-AREAS.                                                        
         03  TXTMAN                    PIC  X(08)   VALUE 'TXTMAN'.             
         03  TXTMAN-PARMS.                                                      
           05  TXT-REQU                PIC  X(01)   VALUE SPACES.               
               88  TXT-REQUEST-TOASCII              VALUE 'A'.          00003100
               88  TXT-REQUEST-COPY                 VALUE 'C'.                  
               88  TXT-REQUEST-DELETE               VALUE 'D'.                  
               88  TXT-REQUEST-TOEBCDIC             VALUE 'E'.          00003100
               88  TXT-REQUEST-FIND                 VALUE 'F'.                  
               88  TXT-REQUEST-INSERT               VALUE 'I'.                  
               88  TXT-REQUEST-JUSTIFY-L            VALUE 'J'.          00003600
               88  TXT-REQUEST-PRINTMASK            VALUE 'K'.          00003600
               88  TXT-REQUEST-LOWERCASE            VALUE 'L'.          00003600
               88  TXT-REQUEST-JUSTIFY-M            VALUE 'M'.          00003600
               88  TXT-REQUEST-OVERLAY              VALUE 'O'.                  
               88  TXT-REQUEST-PARSE                VALUE 'P'.                  
               88  TXT-REQUEST-JUSTIFY-R            VALUE 'R'.          00003600
               88  TXT-REQUEST-UPPERCASE            VALUE 'U'.          00003600
               88  TXT-REQUEST-WORDCAPS             VALUE 'W'.          00003600
           05  TXT-RETN                PIC  X(01)   VALUE SPACES.               
               88  TXT-REQUEST-COMPLETED            VALUE ' '.                  
               88  TXT-STRING-NOT-FOUND             VALUE 'N'.                  
               88  TXT-STRING-WAS-FOUND             VALUE 'Y'.                  
               88  TXT-PARSED-AT-BUFEND             VALUE LOW-VALUE.            
               88  TXT-INVALID-REQU                 VALUE 'E'.                  
               88  TXT-INVALID-PNTR                 VALUE 'P'.                  
               88  TXT-INVALID-BUFLEN               VALUE 'B'.                  
               88  TXT-INVALID-STRLEN               VALUE 'S'.                  
           05  TXT-PNTR                PIC  9(04)   COMP VALUE ZEROES.          
      * THESE DELIMITERS MAY BE CHANGED FOR THE TEXT "PARSE" REQUEST            
           05  TXT-DLMS.                                                        
               10  TXT-DLM1            PIC  X(01)   VALUE '('.                  
               10  TXT-DLM2            PIC  X(01)   VALUE ')'.                  
               10  TXT-DLM3            PIC  X(01)   VALUE ','.                  
               10  TXT-DLM4            PIC  X(01)   VALUE QUOTE.                
               10  TXT-DLM5            PIC  X(01)   VALUE '='.                  
               10  TXT-DLM6            PIC  X(01)   VALUE SPACE.                
               10  TXT-DLM7            PIC  X(01)   VALUE SPACE.                
               10  TXT-DLM8            PIC  X(01)   VALUE SPACE.                
      *                                                                         
           05  TXT-RESERVED            PIC  X(260)  VALUE LOW-VALUES.           
                                                                                
       01  TXTMAN-BUFFER.                                                       
           05  TXT-MSTR-BUFLEN         PIC  9(04)   COMP VALUE 9998.            
           05  TXT-MSTR-BUFFER.                                                 
               10  TXT-MSTR-BYTE       PIC  X(01)                               
                           OCCURS 9998 TIMES                                    
                           DEPENDING   TXT-MSTR-BUFLEN.                         
                                                                                
       01  TXTMAN-STRING.                                                       
           05  TXT-PARM-STRLEN         PIC  9(04)   COMP VALUE 256.             
           05  TXT-PARM-STRING.                                                 
               10  TXT-PARM-BYTE       PIC  X(01)                               
                           OCCURS 0256 TIMES                                    
                           DEPENDING   TXT-PARM-STRLEN.                         
