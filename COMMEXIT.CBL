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
                   EXEC CICS START                                              
                             TRANSID  (COMM-LINKTRN)                            
                             FROM     (COMM-DETAIL)                             
                             LENGTH   (LINK-LEN)                                
                             TERMID   (EIBTRMID)                                
                             NOHANDLE                                           
                   END-EXEC                                                     
               WHEN COMM-LINKTRN = 'JOBS'                                       
               OR   COMM-LINKSFX = 'PGM'                                        
               OR   COMM-LINKSFX NOT > SPACES                                   
                   EXEC CICS START                                              
                             TRANSID  (COMM-LINKTRN)                            
                             FROM     (DFHCOMMAREA)                             
                             LENGTH   (LINK-LEN)                                
                             TERMID   (EIBTRMID)                                
                             NOHANDLE                                           
                   END-EXEC                                                     
               WHEN OTHER                                                       
                   EXEC CICS XCTL                                               
                             PROGRAM  (COMM-LINKPGM)                            
                             COMMAREA (DFHCOMMAREA)                             
                             LENGTH   (LINK-LEN)                                
                             NOHANDLE                                           
                   END-EXEC                                                     
               END-EVALUATE                                                     
           ELSE                                                                 
      * else, if attached to a terminal                                         
               IF  EIBTRMID > SPACES AND NOT = 'CNSL'                           
      * ... then, if originally invoked from a JOBS menu ...                    
                   MOVE 'DATA'         TO THIS-WRK                              
                   MOVE 'JOBS'         TO THIS-QUE(1:4)                         
                   MOVE EIBTRMID       TO THIS-QUE(5:4)                         
                   EXEC CICS READQ TS                                           
                             QUEUE    (THIS-QUE)                                
                             SYSID    (THIS-WRK)                                
                             SET      (ADDRESS OF COMM-HOLDAREA)                
                             LENGTH   (LENGTH OF COMM-HOLDAREA)                 
                             NOHANDLE                                           
                   END-EXEC                                                     
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   OR  EIBRESP = DFHRESP(LENGERR)                               
      * ... ... then, return to the previous JOBS menu                          
                       MOVE 'RETN'     TO COMM-TRANSID                          
                       MOVE 'JOBS'     TO COMM-LINKPGM                          
                       EXEC CICS START                                          
                                 TRANSID  (COMM-LINKTRN)                        
                                 FROM     (COMM-TRANSID)                        
                                 TERMID   (EIBTRMID)                            
                                 NOHANDLE                                       
                       END-EXEC                                                 
                   ELSE                                                         
      * ... ... else, return to a blank cics screen                             
                       EXEC CICS SEND CONTROL                                   
                                 ERASE DEFAULT                                  
                                 FREEKB                                         
                                 NOHANDLE                                       
                       END-EXEC                                                 
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      * report errors, if any                                                   
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
       Z95-CICS-RETURN.                                                         
      * issue explicit syncpoint                                                
           EXEC CICS SYNCPOINT NOHANDLE END-EXEC.                               
      * retun control to cics                                                   
           EXEC CICS RETURN END-EXEC.                                           
           GOBACK.                                                              
