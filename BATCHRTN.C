           MOVE RTC-CODE               TO RETURN-CODE.                          
           IF  RTC-CODE > +8                                                    
             IF  RTC-CODE < +16                                                 
               SET RTC-REQUEST-CANCEL  TO TRUE                                  
             ELSE                                                               
               SET RTC-REQUEST-DUMP    TO TRUE                                  
             END-IF                                                             
           ELSE                                                                 
               SET RTC-REQUEST-FINISH  TO TRUE                                  
           END-IF.                                                              
                                                                                
           PERFORM B80-CALL-RTCMAN THRU B85-EXIT.                               
           MOVE RTC-CODE               TO RETURN-CODE.                          
