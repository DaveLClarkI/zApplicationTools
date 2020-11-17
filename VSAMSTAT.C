      *                                                                         
      * VSAM STATUS DISPLAY PROGRAM DATA AREAS                                  
      *                                                                         
       01  VSAMSTAT-AREAS.                                                      
           05  VSAMSTAT                PIC  X(8)    VALUE 'VSAMSTAT'.           
           05  VSAMSTAT-FILENAME       PIC  X(8)    VALUE SPACES.               
           05  VSAMSTAT-VERBNAME       PIC  X(8)    VALUE SPACES.               
           05  VSAMSTAT-STATUS         PIC  X(2)    VALUE SPACES.               
       01  VSAMSTAT-KEYAREA.                                                    
           05  VSAMSTAT-KEYLENG        PIC S9(4)    BINARY VALUE ZERO.          
           05  VSAMSTAT-KEYDATA        PIC  X(84)   VALUE SPACES.               
       01  VSAMSTAT-FEEDBACK.                                                   
           05  VSAMSTAT-RC             PIC S9(4)    BINARY.                     
           05  VSAMSTAT-FC             PIC S9(4)    BINARY.                     
           05  VSAMSTAT-EC             PIC S9(4)    BINARY.                     
