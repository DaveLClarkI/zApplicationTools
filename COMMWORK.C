         03  THIS-TRN                  PIC  X(04)   VALUE 'TRAN'.               
         03  THIS-WRK.                                                          
           05  LOVALUE                 PIC  X(01)   VALUE LOW-VALUE.            
           05  OPT-BYTE                PIC  X(01)   VALUE SPACES.               
           05  OPT-PAIR                PIC  X(02)   VALUE SPACES.               
         03  THIS-PGM                  PIC  X(08)   VALUE 'PROGRAM'.            
         03  THIS-QUE                  PIC  X(16)   VALUE SPACES.               
         03  LINK-PGM                  PIC  X(08)   VALUE 'LINKPGM'.            
         03  DAPSYSF                   PIC  X(08)   VALUE 'DAPSYSF'.            
         03  ALT2-TRN                  PIC  X(04)   VALUE 'ALT2'.               
         03  INIT-FLAG                 PIC  X(04)   VALUE HIGH-VALUES.          
         03  MAPTITLE-KEY              PIC  X(20)   VALUE 'MAPTITLE'.           
         03  MAPTITLE                  PIC  X(40)   VALUE SPACES.               
         03  OPT-LEN                   PIC S9(04)   BINARY VALUE ZEROES.        
         03  LINK-LEN                  PIC S9(04)   BINARY VALUE ZEROES.        
         03  SCRNHGHT                  PIC S9(04)   BINARY VALUE +24.           
         03  SCRNWDTH                  PIC S9(04)   BINARY VALUE +80.           
