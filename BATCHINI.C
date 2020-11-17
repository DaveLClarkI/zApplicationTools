           PERFORM B80-CALL-RTCMAN THRU B85-EXIT.                               
           IF  RTC-CODE NOT = ZERO                                              
           OR  NOT RTC-REQUEST-COMPLETED                                        
               IF  NOT RTC-REQUEST-COMPLETED                                    
                   DISPLAY THIS-PGM ': RTCMAN ERROR - ' RTC-RETN                
                       UPON CONSOLE                                             
               ELSE                                                             
                   DISPLAY THIS-PGM ': ABORTED DUE TO PRIOR RETURN CODE'        
                       UPON CONSOLE                                             
               END-IF                                                           
               MOVE +16                TO RTC-CODE                              
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT                            
           END-IF.                                                              
                                                                                
           COMPUTE STAT-TOTL            = LENGTH OF STAT-DATA                   
                                        / LENGTH OF STAT-ENTRY.                 
