      *                                                                         
      * HEX MANIPULATION SUBROUTINE DATA STRUCTURE                              
      *                                                                         
       01  HEXMAN-AREA.                                                         
         03  HEXMAN                    PIC  X(8)    VALUE 'HEXMAN  '.           
         03  HEXMAN-PARMS.                                                      
           05  HEX-REQU                PIC  X(1)    VALUE 'D'.                  
               88  HEX-DECODE-FROM-CHAR             VALUE 'D'.                  
               88  HEX-ENCODE-FROM-HEX              VALUE 'E'.                  
           05  HEX-RETN                PIC  X(1)    VALUE SPACES.               
               88  HEX-REQUEST-COMPLETED            VALUE ' '.                  
               88  HEX-INVALID-REQU                 VALUE 'E'.                  
               88  HEX-INVALID-LENG                 VALUE 'L'.                  
               88  HEX-INVALID-HEXE                 VALUE 'X'.                  
           05  HEX-LENG                PIC  9(4)    COMP VALUE ZEROES.          
           05  HEX-CHRT.                                                        
             10  HEX-CHRE              PIC  X(1)                                
                           OCCURS 0256 TIMES                                    
                           DEPENDING   HEX-LENG.                                
           05  FILLER                  PIC  X(1).                               
           05  HEX-HEXT.                                                        
             10  HEX-HEXE              PIC  X(2)                                
                           OCCURS 0256 TIMES                                    
                           DEPENDING   HEX-LENG.                                
           05  FILLER                  PIC  X(1).                               
