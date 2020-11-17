       01  VUSERID-RECORD.                                                      
         03  VUSER-KEY                 PIC  X(8).                               
         03  VUSER-OPID                PIC  X(4).                               
         03  VUSER-NAME                PIC  X(30).                              
         03  VUSER-CLASS               PIC  XX.                                 
         03  VUSER-DAPSCO              PIC  X.                                  
           88  VUSER-DAPSCO-CODES                   VALUE '0' THRU '9'.         
           88  VUSER-AREA-LEADER                    VALUE 'A'.                  
           88  VUSER-WISE-USER-OR-AC                VALUE 'W'.                  
           88  VUSER-EXTERNAL-USER                  VALUE 'X'.                  
         03  VUSER-PASSWORD            PIC  X(8).                               
      *                                                                         
         03  VUSER-DFQ-SELECT          PIC  X(8).                               
         03  VUSER-DFQ-BASE.                                                    
           05  VUSER-DFQ-BASEN         PIC  99.                                 
         03  VUSER-DFQ-REPT.                                                    
           05  VUSER-DFQ-REPTN         PIC  99.                                 
         03  VUSER-DFQ-SORT            PIC  X(3).                               
      *                                                                         
         03  VUSER-IND-AUTH            PIC  X(8).                               
         03  VUSER-IND-LIST            PIC  X(8).                               
         03  VUSER-NEWC-DAYX.                                                   
           05  VUSER-NEWC-DAYS         PIC  999.                                
         03  VUSER-NEWC-SORT           PIC  X.                                  
      *                                                                         
         03  VUSER-WIN-UNIT            PIC  X(10).                              
         03  VUSER-DEPT-NAME           PIC  X(30).                              
         03  VUSER-PRIMARY-CO          PIC  X(5).                               
         03  VUSER-PRIMARY-IND         PIC  X(1).                               
         03  VUSER-WIN-ORG             PIC  X(5).                               
         03  VUSER-WIN-FUNCTION        PIC  X(3).                               
         03  VUSER-WIN-DIRECTOR        PIC  X(1).                               
         03  VUSER-WIN-POSITION        PIC  X(5).                               
      *                                                                         
         03  VUSER-SVC-REGION          PIC  XX.                                 
         03  VUSER-ALT-REGION          PIC  XX.                                 
         03  VUSER-RPT-REMOTE          PIC  XXX.                                
         03  VUSER-ALT-REMOTE          PIC  XXX.                                
         03  VUSER-DFT-PRINTER         PIC  X(4).                               
      *                                                                         
         03  FILLER                    PIC  X(10).                              
         03  VUSER-LOCK-FLAG           PIC  X(1).                               
         03  VUSER-AD-REFRESH          PIC  X(1).                               
           88  VUSER-NO-REFRESH                     VALUE 'N'.                  
           88  VUSER-DO-REFRESH                     VALUE 'Y', ' '.             
