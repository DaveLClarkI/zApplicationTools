      *  WORK AREAS FOR DATE/TIME MANIPULATIONS                                 
      *                                                                         
      *                                                                         
      * Can use the following in CICS:                                          
      *    EXEC CICS ASKTIME ABSTIME(WRK-ABSTIME) END-EXEC.                     
      * Then, in CICS, use FORMATTIME for further information.                  
      *                                                                         
      * Can use the following in batch:                                         
      *    CALL ASKTIME USING WRK-ABSTIME.                                      
      * Then, in batch, see ABSTIME or ISOTSTMP for further information.        
      *                                                                         
       01  WRK-ABSTIME                 PIC S9(15)   PACKED-DECIMAL.             
      *                                                                         
      *                                                                         
      * Can use the following in COBOL (batch or online):                       
      *    MOVE FUNCTION CURRENT-DATE  TO WRK-DATETIME.                         
      *                                                                         
       01  WRK-DATETIME.                                                        
         03  ISO-NUM-DATE              PIC  9(8).                               
         03  ISO-DATE        REDEFINES ISO-NUM-DATE.                            
           05  DTE-GRYR                PIC  9999.                               
           05  DTE-GRMO                PIC  99.                                 
           05  DTE-GRDA                PIC  99.                                 
         03  WRK-TIME.                                                          
           05  NUM-TIME                PIC  9(6).                               
           05  NUM-TIME-X    REDEFINES NUM-TIME.                                
             07  NUM-TIME-HH           PIC  99.                                 
             07  NUM-TIME-MM           PIC  99.                                 
             07  NUM-TIME-SS           PIC  99.                                 
           05  NUM-TIME-UU             PIC  99.                                 
         03  WRK-ZONE-OFFSET           PIC +9(4).                               
         03                            REDEFINES    WRK-ZONE-OFFSET.            
           05  WRK-ZONE-DIRECTION      PIC  X(1).                               
             88  WRK-ZONE-EAST                      VALUE '+'.                  
             88  WRK-ZONE-WEST                      VALUE '-'.                  
           05  WRK-ZONE-HOURS          PIC  9(2).                               
           05  WRK-ZONE-MINUTES        PIC  9(2).                               
      *                                                                         
      *                                                                         
      * The following two sections are for reformatting the above               
      * or, in CICS, for output areas used with FORMATTIME.                     
      *                                                                         
       01  NUM-DATETIME.                                                        
         03  NUM-MMDDYY                PIC  9(6).                               
         03  NUM-YYMMDD                PIC  9(6).                               
         03  JUL7-DATE                 PIC  9(7).                               
         03  JUL7-DATE-X     REDEFINES JUL7-DATE.                               
           05  DTE-JUYR                PIC  9999.                               
           05                          PIC  XXX.                                
         03                  REDEFINES JUL7-DATE.                               
           05                          PIC  XX.                                 
           05  JUL-DATE                PIC  9(5).                               
           05  JUL-DATE-X    REDEFINES JUL-DATE.                                
             07  DTE-JUYY              PIC  99.                                 
             07  DTE-JUDA              PIC  999.                                
         03  NUM-DATE                  PIC  9(8).                               
         03  USA-DATE        REDEFINES NUM-DATE.                                
           05  DTE-GRMO                PIC  99.                                 
           05  DTE-GRDA                PIC  99.                                 
           05  DTE-GRYR                PIC  9999.                               
      *                                                                         
       01  DSP-DATETIME.                                                        
         03  ISO-DSP-DATE              PIC  9999B99B99.                         
         03  USA-DSP-DATE              PIC  99/99/9999.                         
         03  WRK-DSP-DATE    REDEFINES USA-DSP-DATE.                            
           05  WRK-DSP-MM              PIC  9(02).                              
           05                          PIC  X.                                  
           05  WRK-DSP-DD              PIC  9(02).                              
           05                          PIC  X.                                  
           05  WRK-DSP-CY              PIC  9(04).                              
         03  ISO-DSP-TIME              PIC  99B99B99.                           
         03  WRK-DSP-TIME    REDEFINES ISO-DSP-TIME.                            
           05  WRK-DSP-HH              PIC  9(02).                              
           05                          PIC  X(03).                              
           05  WRK-DSP-AM-PM           PIC  X(03).                              
