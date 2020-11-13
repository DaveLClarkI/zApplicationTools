           IF  COMM-CURRRTN NOT = 'RETN'                                        
                              AND 'EXIT'                                        
               MOVE SPACES             TO COMM-CURRRTN                          
           END-IF.                                                              
           IF  COMM-CURRPGM > SPACES                                            
           AND COMM-CURRRTN = 'EXIT'                                            
               MOVE COMM-CURRPGM       TO COMM-LINKPGM                          
           ELSE                                                                 
               IF  COMM-CURRRTN NOT = 'RETN'                                    
                   MOVE  SPACES        TO COMM-LINKPGM                          
               END-IF                                                           
           END-IF.                                                              
           MOVE   THIS-PGM             TO COMM-CURRPGM.                         
