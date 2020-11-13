      * issue explicit syncpoint                                                
           EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                               
                                                                                
      * return control to cics, but with a return to this transaction           
           EXEC CICS RETURN                                                     
                     TRANSID  (THIS-TRN)                                        
                     COMMAREA (DFHCOMMAREA)                                     
           END-EXEC.                                                            
