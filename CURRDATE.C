       01  WS-CURRENT-DATE-AREA.                                                
         03  WS-CURRENT-DATE           PIC  9(8).                               
         03                            REDEFINES    WS-CURRENT-DATE.            
           05  WS-CURRENT-YEAR         PIC  9(4).                               
           05  WS-CURRENT-MONTH        PIC  9(2).                               
           05  WS-CURRENT-DAY          PIC  9(2).                               
         03  WS-CURRENT-TIME           PIC  9(6)V99.                            
         03                            REDEFINES    WS-CURRENT-TIME.            
           05  WS-CURRENT-HOUR         PIC  9(2).                               
           05  WS-CURRENT-MINUTE       PIC  9(2).                               
           05  WS-CURRENT-SECOND       PIC  9(2).                               
           05  WS-CURRENT-HUNDREDTH    PIC  9(2).                               
         03  WS-CURRENT-OFFSET         PIC +9(4).                               
         03                            REDEFINES    WS-CURRENT-OFFSET.          
           05  WS-OFFSET-DIRECTION     PIC  X(1).                               
             88  WS-OFFSET-EAST                     VALUE '+'.                  
             88  WS-OFFSET-WEST                     VALUE '-'.                  
           05  WS-OFFSET-HOURS         PIC  9(2).                               
           05  WS-OFFSET-MINUTES       PIC  9(2).                               
