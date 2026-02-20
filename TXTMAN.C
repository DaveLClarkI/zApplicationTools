      *                                                                         
      * TEXT MANIPULATION SUBROUTINE DATA STRUCTURE                             
      *                                                                         
       01  TXTMAN-AREAS.                                                        
         03  TXTMAN                    PIC  X(08)   VALUE 'TXTMAN'.             
         03  TXTMAN-PARMS.                                                      
           05  TXT-REQU                PIC  X(01)   VALUE SPACES.               
               88  TXT-REQUEST-SCRIPTING            VALUE 'S'.          00003600
      * legacy requests dependent on a master buffer to manipulate              
               88  TXT-REQUEST-COPY                 VALUE 'C'.                  
               88  TXT-REQUEST-DELETE               VALUE 'D'.                  
               88  TXT-REQUEST-FIND                 VALUE 'F'.                  
               88  TXT-REQUEST-INSERT               VALUE 'I'.                  
               88  TXT-REQUEST-OVERLAY              VALUE 'O'.                  
               88  TXT-REQUEST-PARSE                VALUE 'P'.                  
      * legacy requests that transform only the string parameter(s)             
               88  TXT-REQUEST-JUSTIFY-L            VALUE 'J'.          00003600
               88  TXT-REQUEST-JUSTIFY-M            VALUE 'M'.          00003600
               88  TXT-REQUEST-JUSTIFY-R            VALUE 'R'.          00003600
               88  TXT-REQUEST-TRIM-R               VALUE 'T'.                  
      * legacy requests that translate only the string parameter(s)             
               88  TXT-REQUEST-TOASCII              VALUE 'A'.          00003100
               88  TXT-REQUEST-TOEBCDIC             VALUE 'E'.          00003100
               88  TXT-REQUEST-PRINTMASK            VALUE 'K'.          00003600
               88  TXT-REQUEST-LOWERCASE            VALUE 'L'.          00003600
               88  TXT-REQUEST-UPPERCASE            VALUE 'U'.          00003600
               88  TXT-REQUEST-WORDCAPS             VALUE 'W'.          00003600
      * new requests to convert string parm(s) into the output buffer           
               88  TXT-REQUEST-BINTOB64             VALUE 'B'.          00003100
               88  TXT-REQUEST-B64TOBIN             VALUE 'X'.          00003100
      * note: B64-TO-BIN requires string lengths evenly divisible by 4          
      *            (the last quadruplet may be padded with '=' signs).          
           05  TXT-RETN                PIC  X(01)   VALUE SPACES.               
               88  TXT-REQUEST-COMPLETED            VALUE ' '.                  
               88  TXT-REQUEST-PTR-RESET            VALUE 'R'.                  
               88  TXT-STRING-NOT-FOUND             VALUE 'N'.                  
               88  TXT-STRING-WAS-FOUND             VALUE 'Y'.                  
               88  TXT-BUFFER-IS-FULL               VALUE 'F'.                  
               88  TXT-PARSED-AT-BUFEND             VALUE LOW-VALUE.            
               88  TXT-INVALID-REQU                 VALUE 'E'.                  
               88  TXT-INVALID-PARMLIST             VALUE 'L'.                  
               88  TXT-INVALID-PNTR                 VALUE 'P'.                  
               88  TXT-INVALID-BUFLEN               VALUE 'B'.                  
               88  TXT-INVALID-STRLEN               VALUE 'S'.                  
               88  TXT-SCRIPT-TERMINATED            VALUE 'T'.                  
           05  TXT-PNTR                PIC  9(04)   COMP VALUE ZEROES.          
      * these delimiters may be changed for the text "PARSE" request            
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
                                                                                
      * The following is the legacy layout for the string parameter(s).         
                                                                                
       01  TXTMAN-STRING.                                                       
           05  TXT-PARM-STRLEN         PIC  9(04)   COMP VALUE 256.             
           05  TXT-PARM-STRING.                                                 
               10  TXT-PARM-BYTE       PIC  X(01)                               
                           OCCURS 0256 TIMES                                    
                           DEPENDING   TXT-PARM-STRLEN.                         
                                                                                
      * the scripting layout--then the CALL statement will list each of         
      * the array elements that you are actually using in your "script".        
                                                                                
       01  TXTMAN-SCRIPT.                                                       
      * the array is not limited to 99, as there is no technical limit--        
      * only whatever COBOL allows for the total size of an 01-level.           
         03  TXTMAN-S-ENTRY                         OCCURS 99.                  
           05  TXT-S-PARM-REQU         PIC  X(01).                              
      * scripting requests dependent on a master buffer to manipulate           
               88  TXT-SCRIPT-COPY                  VALUE 'C'.          00003100
               88  TXT-SCRIPT-DELETE                VALUE 'D'.                  
               88  TXT-SCRIPT-FIND                  VALUE 'F'.          00003100
               88  TXT-SCRIPT-INSERT                VALUE 'I'.                  
               88  TXT-SCRIPT-OVERLAY               VALUE 'O'.          00003600
               88  TXT-SCRIPT-PARSE                 VALUE 'P'.                  
      * scripting requests that transform only the string parameter             
               88  TXT-SCRIPT-JUSTIFY-L             VALUE 'J'.                  
               88  TXT-SCRIPT-JUSTIFY-M             VALUE 'M'.          00003600
               88  TXT-SCRIPT-JUSTIFY-R             VALUE 'R'.                  
               88  TXT-SCRIPT-TRIM-R                VALUE 'T'.          00003600
      * scripting requests that translate only the string parameter             
               88  TXT-SCRIPT-TOASCII               VALUE 'A'.          00003100
               88  TXT-SCRIPT-TOEBCDIC              VALUE 'E'.                  
               88  TXT-SCRIPT-PRINTMASK             VALUE 'K'.          00003600
               88  TXT-SCRIPT-LOWERCASE             VALUE 'L'.          00003600
               88  TXT-SCRIPT-UPPERCASE             VALUE 'U'.          00003600
               88  TXT-SCRIPT-WORDCAPS              VALUE 'W'.                  
           05  TXT-S-PARM-RETN         PIC  X(01).                              
               88  TXT-S-STRING-COMPLETED           VALUE ' '.                  
               88  TXT-S-STRING-INHERIT             VALUE 'I'.                  
               88  TXT-S-BUFFER-PTR-RESET           VALUE 'R'.                  
               88  TXT-S-STRING-NOT-FOUND           VALUE 'N'.                  
               88  TXT-S-STRING-WAS-FOUND           VALUE 'Y'.                  
               88  TXT-S-PARSED-AT-BUFEND           VALUE LOW-VALUE.            
               88  TXT-S-INVALID-REQU               VALUE 'E'.                  
               88  TXT-S-INVALID-INHERIT            VALUE 'F'.                  
               88  TXT-S-INVALID-STRLEN             VALUE 'S'.                  
           05  TXT-S-PARM-STRLEN       PIC  9(04)   BINARY.                     
           05  TXT-S-PARM-STRING.                                               
               10  TXT-S-PARM-BYTE     PIC  X(01)   OCCURS 256.                 
                                                                                
