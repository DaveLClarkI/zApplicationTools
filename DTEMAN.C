      *                                                                         
      * DATE MANIPULATION SUBROUTINE DATA STRUCTURE                             
      *                                                                         
       01  DTEMAN-AREAS.                                                        
         03  DTEMAN                    PIC  X(08)   VALUE 'DTEMAN'.             
         03  DTEMAN-PARMS.                                                      
           05  DTE-REQU                PIC  X(01)   VALUE 'G'.                  
               88  DTE-REQUEST-CURRENT-DATE         VALUE 'C'.          00005450
               88  DTE-REQUEST-GREG-EDIT            VALUE 'G'.                  
               88  DTE-REQUEST-JULN-EDIT            VALUE 'J'.                  
               88  DTE-REQUEST-TOTL-DAYS            VALUE 'T'.                  
           05  DTE-RETN                PIC  X(01)   VALUE SPACES.               
               88  DTE-REQUEST-COMPLETED            VALUE ' '.                  
               88  DTE-INVALID-CLOCK                VALUE 'C'.          00005950
               88  DTE-INVALID-REQU                 VALUE 'E'.                  
               88  DTE-INVALID-GREG                 VALUE 'G'.                  
               88  DTE-INVALID-JULN                 VALUE 'J'.                  
               88  DTE-INVALID-TOTD                 VALUE 'T'.                  
               88  DTE-INVALID-YEAR                 VALUE 'Y'.                  
               88  DTE-INVALID-MONTH                VALUE 'M'.                  
               88  DTE-INVALID-DAY                  VALUE 'D'.                  
      *                                                                         
           05  DTE-GREG                             VALUE SPACES.               
             07                        PIC  XX.                                 
             07  DTE-GRYR              PIC  9999.                               
             07              REDEFINES DTE-GRYR.                                
               09                      PIC  XX.                                 
               09  DTE-GRYY            PIC  99.                                 
             07  DTE-GRMO              PIC  99.                                 
             07  DTE-GRDA              PIC  99.                                 
           05  DTE-GNUM      REDEFINES DTE-GREG                                 
                                       PIC  9(10).                              
           05                REDEFINES DTE-GREG.                                
             07                        PIC  XX.                                 
             07  DTE-CCYYMMDD          PIC  9(08).                              
             07              REDEFINES DTE-CCYYMMDD.                            
               09  DTE-CCYYMM          PIC  9(06).                              
               09                      PIC  XX.                                 
             07              REDEFINES DTE-CCYYMMDD.                            
               09                      PIC  XX.                                 
               09  DTE-YYMMDD          PIC  9(06).                              
      *                                                                         
           05  DTE-JULN                             VALUE SPACES.               
             07                        PIC  X.                                  
             07  DTE-JUYR              PIC  9999.                               
             07              REDEFINES DTE-JUYR.                                
               09                      PIC  XX.                                 
               09  DTE-JUYY            PIC  99.                                 
             07  DTE-JUDA              PIC  999.                                
           05  DTE-JNUM      REDEFINES DTE-JULN                                 
                                       PIC  9(08).                              
           05                REDEFINES DTE-JULN.                                
             07                        PIC  X.                                  
             07  DTE-CCYYDDD           PIC  9(07).                              
             07              REDEFINES DTE-CCYYDDD.                             
               09                      PIC  XX.                                 
               09  DTE-YYDDD           PIC  9(05).                              
      *                                                                         
           05  DTE-TOTD                PIC  9(07)   COMP-3 VALUE ZERO.          
           05  DTE-MNTH                PIC  9(03)   COMP-3 VALUE ZERO.          
               88  DTE-JANUARY                      VALUE 1.                    
               88  DTE-FEBRUARY                     VALUE 2.                    
               88  DTE-MARCH                        VALUE 3.                    
               88  DTE-APRIL                        VALUE 4.                    
               88  DTE-MAY                          VALUE 5.                    
               88  DTE-JUNE                         VALUE 6.                    
               88  DTE-JULY                         VALUE 7.                    
               88  DTE-AUGUST                       VALUE 8.                    
               88  DTE-SEPTEMBER                    VALUE 9.                    
               88  DTE-OCTOBER                      VALUE 10.                   
               88  DTE-NOVEMBER                     VALUE 11.                   
               88  DTE-DECEMBER                     VALUE 12.                   
           05  DTE-DAYN                PIC  9(01)   COMP-3 VALUE ZERO.          
               88  DTE-SUNDAY                       VALUE 1.                    
               88  DTE-MONDAY                       VALUE 2.                    
               88  DTE-TUESDAY                      VALUE 3.                    
               88  DTE-WEDNESDAY                    VALUE 4.                    
               88  DTE-THURSDAY                     VALUE 5.                    
               88  DTE-FRIDAY                       VALUE 6.                    
               88  DTE-SATURDAY                     VALUE 7.                    
           05  DTE-LEAP                PIC  X(01)   VALUE SPACES.               
               88  DTE-LEAP-YEAR                    VALUE 'L'.                  
               88  DTE-NONLEAP-YEAR                 VALUE 'N'.                  
      *                                                                         
           05  DTE-RESERVED            PIC  X(04)   VALUE LOW-VALUES.           
