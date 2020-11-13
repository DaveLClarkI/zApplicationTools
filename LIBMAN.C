      *                                                                         
      * VSE/LIBRARIAN MANAGER SUBROUTINE DATA STRUCTURE                         
      *                                                                         
       01  LIBMAN-PARMS.                                                        
           05  LIB-IPT-EXIT            PIC  X(1)    VALUE 'N'.                  
               88  LIB-SYSIPT-OMITTED               VALUE 'N'.                  
               88  LIB-SYSIPT-INCLUDED              VALUE 'Y'.                  
           05  LIB-LOG-EXIT            PIC  X(1)    VALUE 'N'.                  
               88  LIB-SYSLOG-OMITTED               VALUE 'N'.                  
               88  LIB-SYSLOG-INCLUDED              VALUE 'Y'.                  
           05  LIB-LST-EXIT            PIC  X(1)    VALUE 'N'.                  
               88  LIB-SYSLST-OMITTED               VALUE 'N'.                  
               88  LIB-SYSLST-INCLUDED              VALUE 'Y'.                  
           05  LIB-PCH-EXIT            PIC  X(1)    VALUE 'N'.                  
               88  LIB-SYSPCH-OMITTED               VALUE 'N'.                  
               88  LIB-SYSPCH-INCLUDED              VALUE 'Y'.                  
           05  LIB-FMT-EXIT            PIC  X(1)    VALUE 'N'.                  
               88  LIB-FORMAT-OMITTED               VALUE 'N'.                  
               88  LIB-FORMAT-INCLUDED              VALUE 'Y'.                  
           05  FILLER                  PIC  X(1)    VALUE SPACE.                
           05  LIB-RTN-CODE            PIC  9(4)    COMP VALUE ZEROES.          
      *                                                                         
           05  LIB-PGM-COMM.                                                    
             08  FILLER                PIC  X(44)   VALUE LOW-VALUES.           
             08  LIB-USR-COMM          PIC  X(04)   VALUE LOW-VALUES.           
             08  FILLER                PIC  X(16)   VALUE LOW-VALUES.           
