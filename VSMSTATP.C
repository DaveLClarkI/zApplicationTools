       B80-CALL-RTCMAN.                                                         
           CALL RTCMAN              USING RTCMAN-PARMS.                         
       B85-EXIT.                                                                
           EXIT.                                                                
                                                                                
       B90-CHECK-STATUS.                                                        
           IF  NOT STAT-NORMAL(VSUB)                                            
               CALL VSAMSTAT        USING VSAM-FILE(VSUB)                       
                                          VSAM-FUNC(VSUB)                       
                                          VSAM-STAT(VSUB)                       
                                          VSAM-KEYA(VSUB)                       
                                          VSAM-FDBK(VSUB)                       
           END-IF.                                                              
           IF  FUNC-OPEN(VSUB)                                                  
           AND VSAM-STAT(VSUB) = '90'                                           
           AND VSAM-FDBK-RC(VSUB) = 4                                           
           AND VSAM-FDBK-EC(VSUB) = 116                                         
               CONTINUE                                                         
           ELSE                                                                 
           IF  NOT STAT-NORMAL(VSUB)                                            
               EVALUATE TRUE                                                    
               WHEN FUNC-CLOSE(VSUB)                                            
               WHEN STAT-DUPKEY(VSUB)     MOVE  +4 TO RTC-CODE                  
               WHEN STAT-DUPREC(VSUB)                                           
               WHEN STAT-NOTFND(VSUB)     MOVE  +8 TO RTC-CODE                  
               WHEN STAT-EOFILE(VSUB)                                           
               WHEN FUNC-OPEN(VSUB)       MOVE +12 TO RTC-CODE                  
               WHEN OTHER                                                       
                   IF  VSAM-STAT(VSUB) < '10'                                   
                                          MOVE  +8 TO RTC-CODE                  
                   ELSE                                                         
                                          MOVE +16 TO RTC-CODE                  
                   END-IF                                                       
               END-EVALUATE                                                     
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT                            
           END-IF.                                                              
       B95-EXIT-CHECK.                                                          
           EXIT.                                                                
