      * issue explicit syncpoint                                                
           EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                               
                                                                                
      * set next program (JOBS)                                                 
           MOVE 'JOBSSELT'             TO DFHCOMMAREA(1:8).                     
                                                                                
      * go display standard NOT OPEN screen                                     
           EXEC CICS XCTL                                                       
                     PROGRAM  ('DAPNTOPN')                                      
                     COMMAREA (DFHCOMMAREA)                                     
                     LENGTH   (+8)                                              
           END-EXEC.                                                            
