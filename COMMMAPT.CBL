           EXEC CICS READ                                                       
                     DATASET  (DAPSYSF)                                         
                     SET      (ADDRESS OF MAPTITLE-RECORD)                      
                     RIDFLD   (MAPTITLE-KEY)                                    
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               MOVE MAPTITLE-VALUE     TO MAPTITLE                              
           END-IF.                                                              
                                                                                
           EXEC CICS ASSIGN                                                     
                     SCRNHT   (SCRNHGHT)                                        
                     SCRNWD   (SCRNWDTH)                                        
                     NOHANDLE                                                   
           END-EXEC.                                                            
           EXEC CICS HANDLE CONDITION                                           
                     INVMPSZ  (X00-UNEX-ERR)                                    
           END-EXEC.                                                            
