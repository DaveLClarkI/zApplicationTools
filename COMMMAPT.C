      *EXEC CICS READ                                                           
      *          DATASET  (DAPSYSF)                                             
      *          SET      (ADDRESS OF MAPTITLE-RECORD)                          
      *          RIDFLD   (MAPTITLE-KEY)                                        
      *          EQUAL                                                          
      *          NOHANDLE                                                       
      *END-EXEC.                                                                
            MOVE '�}  � 00001   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0  DAPSYSF ADDRESS OF                      
           MAPTITLE-RECORD DFHDUMMY MAPTITLE-KEY.                               
                                                                                
                                                                                
                                                                                
                                                                                
      * DFHRESP(NORMAL) = 0                       INSERTED BY TRANSLATOR        
           IF  EIBRESP = 0                                                      
               MOVE MAPTITLE-VALUE     TO MAPTITLE                              
           END-IF.                                                              
                                                                                
      *EXEC CICS ASSIGN                                                         
      *          SCRNHT   (SCRNHGHT)                                            
      *          SCRNWD   (SCRNWDTH)                                            
      *          NOHANDLE                                                       
      *END-EXEC.                                                                
            MOVE '�{ ��               00012   ' TO DFHEIV0                    
           CALL 'DFHEI1' USING DFHEIV0  SCRNHGHT SCRNWDTH.                      
                                                                                
                                                                                
                                                                                
      *EXEC CICS HANDLE CONDITION                                               
      *          INVMPSZ  (X00-UNEX-ERR)                                        
      *END-EXEC.                                                                
            MOVE '��                 00017   ' TO DFHEIV0                    
           CALL 'DFHEI1' USING DFHEIV0                                          
            SERVICE LABEL                                                       
            GO TO  X00-UNEX-ERR DEPENDING ON DFHEIGDI.                          
