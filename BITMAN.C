      *                                                                         
      * BIT MANIPULATION SUBROUTINE DATA STRUCTURE                              
      *                                                                         
       01  BITMAN-AREA.                                                         
         03  BITMAN                    PIC  X(8)    VALUE 'BITMAN  '.           
         03  BITMAN-PARMS.                                                      
           05  BIT-REQU                PIC  X(1)    VALUE 'D'.                  
               88  BIT-DECODE-FROM-BYTE             VALUE 'D'.                  
               88  BIT-ENCODE-FROM-BITS             VALUE 'E'.                  
           05  BIT-RETN                PIC  X(1)    VALUE SPACES.               
               88  BIT-REQUEST-COMPLETED            VALUE ' '.                  
               88  BIT-INVALID-REQU                 VALUE 'E'.                  
           05  FILLER                  PIC  X(1)    VALUE SPACES.               
           05  BIT-BYTE                PIC  X(1)    VALUE LOW-VALUE.            
           05  BIT-DATA.                                                        
               10  BIT-0               PIC  9(1)    VALUE 0.                    
                   88  BIT-0-IS-OFF                 VALUE 0.                    
                   88  BIT-0-IS-ON                  VALUE 1.                    
               10  BIT-1               PIC  9(1)    VALUE 0.                    
                   88  BIT-1-IS-OFF                 VALUE 0.                    
                   88  BIT-1-IS-ON                  VALUE 1.                    
               10  BIT-2               PIC  9(1)    VALUE 0.                    
                   88  BIT-2-IS-OFF                 VALUE 0.                    
                   88  BIT-2-IS-ON                  VALUE 1.                    
               10  BIT-3               PIC  9(1)    VALUE 0.                    
                   88  BIT-3-IS-OFF                 VALUE 0.                    
                   88  BIT-3-IS-ON                  VALUE 1.                    
               10  BIT-4               PIC  9(1)    VALUE 0.                    
                   88  BIT-4-IS-OFF                 VALUE 0.                    
                   88  BIT-4-IS-ON                  VALUE 1.                    
               10  BIT-5               PIC  9(1)    VALUE 0.                    
                   88  BIT-5-IS-OFF                 VALUE 0.                    
                   88  BIT-5-IS-ON                  VALUE 1.                    
               10  BIT-6               PIC  9(1)    VALUE 0.                    
                   88  BIT-6-IS-OFF                 VALUE 0.                    
                   88  BIT-6-IS-ON                  VALUE 1.                    
               10  BIT-7               PIC  9(1)    VALUE 0.                    
                   88  BIT-7-IS-OFF                 VALUE 0.                    
                   88  BIT-7-IS-ON                  VALUE 1.                    
           05  BIT-TABLE   REDEFINES   BIT-DATA.                                
               10  BIT     OCCURS 08   PIC  9(1).                               
                   88  BIT-IS-OFF                   VALUE 0.                    
                   88  BIT-IS-ON                    VALUE 1.                    
