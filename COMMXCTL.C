      * issue explicit syncpoint                                                
      *EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                                   
            MOVE '     00002   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0.                                         
                                                                                
      * set next program (JOBS)                                                 
           MOVE 'JOBSSELT'             TO DFHCOMMAREA(1:8).                     
                                                                                
      * go display standard NOT OPEN screen                                     
      *EXEC CICS XCTL                                                           
      *          PROGRAM  ('DAPNTOPN')                                          
      *          COMMAREA (DFHCOMMAREA)                                         
      *          LENGTH   (+8)                                                  
      *END-EXEC.                                                                
            MOVE '�\    00008   ' TO DFHEIV0                                 
            MOVE 'DAPNTOPN' TO DFHC0080                                         
            MOVE +8 TO DFHB0020                                                 
           CALL 'DFHEI1' USING DFHEIV0  DFHC0080 DFHCOMMAREA DFHB0020.          
                                                                                
