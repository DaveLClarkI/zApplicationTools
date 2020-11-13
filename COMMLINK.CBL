       T00-LINK-TRANSFER.                                                       
           IF  COMM-TRANSID     = 'EXIT'                                        
           OR  COMM-LINKPGM NOT > SPACES                                        
           OR  COMM-LINKPGM     = THIS-PGM                                      
           OR  COMM-LINKTRN     = THIS-TRN                                      
               MOVE THIS-TRN           TO COMM-TRANSID                          
               IF  LINK-PGM = 'ECUE'                                            
               OR  COMM-SAVE-FUNC > SPACES                                      
                   MOVE COMM-SAVE-FUNC TO THIS-WRK                              
                   MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC                        
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT                      
                   MOVE THIS-WRK       TO COMM-SAVE-FUNC                        
               ELSE                                                             
                   IF  COMM-CURRPGM > SPACES                                    
                     MOVE COMM-CURRRTN TO COMM-SAVE-FUNC                        
                     PERFORM T40-SAVE-COMMAREA THRU T45-EXIT                    
                   END-IF                                                       
               END-IF                                                           
               MOVE 'EXIT'             TO COMM-TRANSID                          
               MOVE LINK-PGM           TO COMM-LINKPGM                          
           ELSE                                                                 
               MOVE 'RETN'             TO COMM-TRANSID                          
           END-IF.                                                              
                                                                                
       T02-LINK-CONTINUE.                                                       
           EVALUATE TRUE                                                        
           WHEN COMM-LINKTRN = 'ECUE'                                           
               IF  LINK-LEN NOT > ZEROES                                        
                   COMPUTE LINK-LEN     = LENGTH OF DFHCOMMAREA                 
                                        - LENGTH OF COMM-HEADER                 
                                        - LENGTH OF COMM-SHARED                 
               END-IF                                                           
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
               IF  LINK-LEN NOT > ZEROES                                        
                   MOVE LENGTH OF DFHCOMMAREA TO LINK-LEN                       
               END-IF                                                           
               EXEC CICS START                                                  
                         TRANSID  (COMM-LINKTRN)                                
                         FROM     (DFHCOMMAREA)                                 
                         LENGTH   (LINK-LEN)                                    
                         TERMID   (EIBTRMID)                                    
                         NOHANDLE                                               
               END-EXEC                                                         
           WHEN OTHER                                                           
               IF  LINK-LEN NOT > ZEROES                                        
                   MOVE LENGTH OF DFHCOMMAREA TO LINK-LEN                       
               END-IF                                                           
               EXEC CICS XCTL                                                   
                         PROGRAM  (COMM-LINKPGM)                                
                         COMMAREA (DFHCOMMAREA)                                 
                         LENGTH   (LINK-LEN)                                    
                         NOHANDLE                                               
               END-EXEC                                                         
           END-EVALUATE.                                                        
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
                                                                                
           GO TO Z95-CICS-RETURN.                                               
                                                                                
       T40-SAVE-COMMAREA.                                                       
           MOVE THIS-TRN           TO THIS-QUE(1:4).                            
           MOVE EIBTRMID           TO THIS-QUE(5:4).                            
           MOVE 'COMMAREA'         TO THIS-QUE(9:8).                            
           EXEC CICS DELETEQ TS                                                 
                     QNAME    (THIS-QUE)                                        
                     NOHANDLE                                                   
           END-EXEC.                                                            
           EXEC CICS WRITEQ TS                                                  
                     QNAME    (THIS-QUE)                                        
                     FROM     (DFHCOMMAREA)                                     
                     AUXILIARY                                                  
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
       T45-EXIT.                                                                
           EXIT.                                                                
