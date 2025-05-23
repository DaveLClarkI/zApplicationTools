      * if invoked by another transaction or program...                         
           IF  EIBCALEN > ZERO                                                  
           AND COMM-LINKPGM > SPACES                                            
      * then, return to that transaction or program                             
               MOVE 'RETN'             TO COMM-TRANSID                          
               IF  LINK-LEN NOT > ZERO                                          
                   MOVE LENGTH OF DFHCOMMAREA                                   
                                       TO LINK-LEN                              
               END-IF                                                           
               EVALUATE TRUE                                                    
               WHEN COMM-LINKTRN = 'ECUE'                                       
                   COMPUTE LINK-LEN    =  LINK-LEN                              
                                       -  LENGTH OF COMM-HEADER                 
                                       -  LENGTH OF COMM-SHARED                 
      *EXEC CICS START                                                          
      *          TRANSID  (COMM-LINKTRN)                                        
      *          FROM     (COMM-DETAIL)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          TERMID   (EIBTRMID)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                    MOVE '��   � 00015   ' TO DFHEIV0                         
                   CALL 'DFHEI1' USING DFHEIV0  DFHEID0 DFHDUMMY                
           COMM-LINKTRN COMM-DETAIL LINK-LEN EIBTRMID                           
                                                                                
                                                                                
                                                                                
                                                                                
               WHEN COMM-LINKTRN = 'JOBS'                                       
               OR   COMM-LINKSFX = 'PGM'                                        
               OR   COMM-LINKSFX NOT > SPACES                                   
      *EXEC CICS START                                                          
      *          TRANSID  (COMM-LINKTRN)                                        
      *          FROM     (DFHCOMMAREA)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          TERMID   (EIBTRMID)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                    MOVE '��   � 00025   ' TO DFHEIV0                         
                   CALL 'DFHEI1' USING DFHEIV0  DFHEID0 DFHDUMMY                
           COMM-LINKTRN DFHCOMMAREA LINK-LEN EIBTRMID                           
                                                                                
                                                                                
                                                                                
                                                                                
               WHEN OTHER                                                       
      *EXEC CICS XCTL                                                           
      *          PROGRAM  (COMM-LINKPGM)                                        
      *          COMMAREA (DFHCOMMAREA)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                    MOVE '�\    00033   ' TO DFHEIV0                         
                   CALL 'DFHEI1' USING DFHEIV0  COMM-LINKPGM DFHCOMMAREA        
            LINK-LEN                                                            
                                                                                
                                                                                
                                                                                
               END-EVALUATE                                                     
           ELSE                                                                 
      * else, if attached to a terminal                                         
               IF  EIBTRMID > SPACES AND NOT = 'CNSL'                           
      * ... then, if originally invoked from a JOBS menu ...                    
                   MOVE 'DATA'         TO THIS-WRK                              
                   MOVE 'JOBS'         TO THIS-QUE(1:4)                         
                   MOVE EIBTRMID       TO THIS-QUE(5:4)                         
      *EXEC CICS READQ TS                                                       
      *          QUEUE    (THIS-QUE)                                            
      *          SYSID    (THIS-WRK)                                            
      *          SET      (ADDRESS OF COMM-HOLDAREA)                            
      *          LENGTH   (LENGTH OF COMM-HOLDAREA)                             
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                    MOVE '��S  a 00047   ' TO DFHEIV0                         
                    MOVE LENGTH OF COMM-HOLDAREA TO DFHB0020                    
                   CALL 'DFHEI1' USING DFHEIV0  THIS-QUE ADDRESS OF             
           COMM-HOLDAREA DFHB0020 DFHDUMMY DFHDUMMY DFHDUMMY THIS-WRK           
                                                                                
                                                                                
                                                                                
      * DFHRESP(NORMAL) = 0                       INSERTED BY TRANSLATOR        
                   IF  EIBRESP = 0                                              
      * DFHRESP(LENGERR) = 22                     INSERTED BY TRANSLATOR        
                   OR  EIBRESP = 22                                             
      * ... ... then, return to the previous JOBS menu                          
                       MOVE 'RETN'     TO COMM-TRANSID                          
                       MOVE 'JOBS'     TO COMM-LINKPGM                          
      *EXEC CICS START                                                          
      *          TRANSID  (COMM-LINKTRN)                                        
      *          FROM     (COMM-TRANSID)                                        
      *          TERMID   (EIBTRMID)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                        MOVE '��   � 00059   ' TO DFHEIV0                     
                        MOVE LENGTH OF COMM-TRANSID TO DFHB0020                 
                       CALL 'DFHEI1' USING DFHEIV0  DFHEID0 DFHDUMMY            
           COMM-LINKTRN COMM-TRANSID DFHB0020 EIBTRMID                          
                                                                                
                                                                                
                   ELSE                                                         
      * ... ... else, return to a blank cics screen                             
      *EXEC CICS SEND CONTROL                                                   
      *          ERASE DEFAULT                                                  
      *          FREEKB                                                         
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                        MOVE '  �B  ��  �00067   ' TO DFHEIV0               
                       CALL 'DFHEI1' USING DFHEIV0                              
                                                                                
                                                                                
                                                                                
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      * report errors, if any                                                   
      * DFHRESP(NORMAL) = 0                       INSERTED BY TRANSLATOR        
           IF  EIBRESP NOT = 0                                                  
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
       Z95-CICS-RETURN.                                                         
      * issue explicit syncpoint                                                
      *EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                                   
            MOVE '     00081   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0.                                         
      * retun control to cics                                                   
      *EXEC CICS RETURN END-EXEC.                                               
            MOVE '�     00083   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0.                                         
           GOBACK.                                                              
