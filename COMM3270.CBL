           EXEC CICS LOAD                                                       
                     PROGRAM  ('IBM3270')                                       
                     SET      (ADDRESS OF IBM3270)                              
           END-EXEC.                                                            
           IF  NOT IBM3270-LOADED-OK                                            
               EXEC CICS ABEND                                                  
                         ABCODE   ('3270')                                      
               END-EXEC                                                         
           END-IF.                                                              
