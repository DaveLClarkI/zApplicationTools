      *EXEC CICS LOAD                                                           
      *          PROGRAM  ('IBM3270')                                           
      *          SET      (ADDRESS OF IBM3270)                                  
      *END-EXEC.                                                                
            MOVE '�{   � 00001   ' TO DFHEIV0                                 
            MOVE 'IBM3270' TO DFHC0080                                          
           CALL 'DFHEI1' USING DFHEIV0  DFHC0080 ADDRESS OF IBM3270.            
                                                                                
           IF  NOT IBM3270-LOADED-OK                                            
      *EXEC CICS ABEND                                                          
      *          ABCODE   ('3270')                                              
      *END-EXEC                                                                 
                MOVE '�   - 00006   ' TO DFHEIV0                             
                MOVE '3270' TO DFHC0040                                         
               CALL 'DFHEI1' USING DFHEIV0  DFHC0040                            
           END-IF.                                                              
