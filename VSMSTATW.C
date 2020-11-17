      *                                                                         
      * VSAM STATUS DISPLAY PROGRAM DATA AREAS                                  
      *                                                                         
       01  VSAMSTAT-AREAS.                                                      
         03  VSAMSTAT                  PIC  X(8)    VALUE 'VSAMSTAT'.           
         03  VSAMSTAT-FILENAME         PIC  X(8)    VALUE SPACES.               
         03  VSAMSTAT-VERBNAME         PIC  X(8)    VALUE SPACES.               
         03  VSAMSTAT-STATUS           PIC  X(2)    VALUE SPACES.               
       01  VSAMSTAT-KEYAREA.                                                    
         03  VSAMSTAT-KEYLENG          PIC S9(4)    BINARY VALUE ZERO.          
         03  VSAMSTAT-KEYDATA          PIC  X(98)   VALUE SPACES.               
       01  VSAMSTAT-FEEDBACK.                                                   
         03  VSAMSTAT-RC               PIC S9(4)    BINARY.                     
         03  VSAMSTAT-FC               PIC S9(4)    BINARY.                     
         03  VSAMSTAT-EC               PIC S9(4)    BINARY.                     
       01  VSAMSTAT-CONTROLS.                                                   
         03  VSUB                      PIC S9(04)   BINARY.                     
         03  STAT-TOTL                 PIC S9(04)   BINARY.                     
         03  STAT-DATA.                                                         
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE1-STAT            PIC  XX      VALUE '00'.                 
             07  FILE1-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE2-STAT            PIC  XX      VALUE '00'.                 
             07  FILE2-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE3-STAT            PIC  XX      VALUE '00'.                 
             07  FILE3-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE4-STAT            PIC  XX      VALUE '00'.                 
             07  FILE4-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE5-STAT            PIC  XX      VALUE '00'.                 
             07  FILE5-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE6-STAT            PIC  XX      VALUE '00'.                 
             07  FILE6-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE7-STAT            PIC  XX      VALUE '00'.                 
             07  FILE7-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE8-STAT            PIC  XX      VALUE '00'.                 
             07  FILE8-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE9-STAT            PIC  XX      VALUE '00'.                 
             07  FILE9-FDBK                         VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE10-STAT           PIC  XX      VALUE '00'.                 
             07  FILE10-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE11-STAT           PIC  XX      VALUE '00'.                 
             07  FILE11-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE12-STAT           PIC  XX      VALUE '00'.                 
             07  FILE12-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE13-STAT           PIC  XX      VALUE '00'.                 
             07  FILE13-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE14-STAT           PIC  XX      VALUE '00'.                 
             07  FILE14-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE15-STAT           PIC  XX      VALUE '00'.                 
             07  FILE15-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
             07                        PIC  X(116)  VALUE LOW-VALUES.           
             07  FILE16-STAT           PIC  XX      VALUE '00'.                 
             07  FILE16-FDBK                        VALUE LOW-VALUES.           
               09                      PIC  X(06).                              
         03  STAT-TABLE                             REDEFINES STAT-DATA.        
           05  STAT-ENTRY                           OCCURS 16.                  
             07  VSAM-FILE             PIC  X(08).                              
             07  VSAM-FUNC             PIC  X(08).                              
               88  FUNC-OPEN                        VALUE 'OPEN    '.           
               88  FUNC-START                       VALUE 'START   '.           
               88  FUNC-READ                        VALUE 'READ    '.           
               88  FUNC-READUPD                     VALUE 'READUPD '.           
               88  FUNC-READNEXT                    VALUE 'READNEXT'.           
               88  FUNC-READPREV                    VALUE 'READPREV'.           
               88  FUNC-WRITE                       VALUE 'WRITE   '.           
               88  FUNC-REWRITE                     VALUE 'REWRITE '.           
               88  FUNC-DELETE                      VALUE 'DELETE  '.           
               88  FUNC-CLOSE                       VALUE 'CLOSE   '.           
             07  VSAM-KEYA.                                                     
               09  VSAM-KEYL           PIC S9(4)    COMP.                       
               09  VSAM-KEYD           PIC  X(98).                              
             07  VSAM-STAT             PIC  XX.                                 
               88  STAT-NORMAL                      VALUE '00'.                 
               88  STAT-DUPKEY                      VALUE '02'.                 
               88  STAT-EOFILE                      VALUE '10'.                 
               88  STAT-DUPREC                      VALUE '22'.                 
               88  STAT-NOTFND                      VALUE '23'.                 
             07  VSAM-FDBK.                                                     
               09  VSAM-FDBK-RC        PIC S9(04)   BINARY.                     
               09  VSAM-FDBK-FC        PIC S9(04)   BINARY.                     
               09  VSAM-FDBK-EC        PIC S9(04)   BINARY.                     
