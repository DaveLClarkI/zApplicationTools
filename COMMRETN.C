      * issue explicit syncpoint                                                
      *EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                                   
            MOVE '     00002   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0.                                         
                                                                                
      * return control to cics, but with a return to this transaction           
      *EXEC CICS RETURN                                                         
      *          TRANSID  (THIS-TRN)                                            
      *          COMMAREA (DFHCOMMAREA)                                         
      *END-EXEC.                                                                
            MOVE '�\    00005   ' TO DFHEIV0                                 
            MOVE LENGTH OF DFHCOMMAREA TO DFHB0020                              
           CALL 'DFHEI1' USING DFHEIV0  THIS-TRN DFHCOMMAREA DFHB0020.          
                                                                                
