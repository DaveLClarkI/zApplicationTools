           IF  EIBCALEN     NOT = LENGTH OF DFHCOMMAREA                         
           OR  COMM-TRANSID NOT = THIS-TRN                                      
                                                                                
      * allocate temporary storage for this program's commarea                  
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF COMM-TEMPAREA)                    
                         LENGTH   (LENGTH OF DFHCOMMAREA)                       
                         INITIMG  (LOVALUE)                                     
               END-EXEC                                                         
                                                                                
      * if a commarea is present...                                             
               IF  EIBCALEN > ZEROES                                            
                   MOVE COMM-HEADER    TO COMM-TEMPHDR                          
      * ... then, if returning from another program...                          
                   IF  COMM-TRANSID = 'RETN'                                    
                   OR  COMM-CURRRTN = 'RETN'                                    
                       MOVE SPACES     TO COMM-TEMPLNK                          
      * ... ... then, get our commarea from temp storage into temp area         
                       MOVE THIS-TRN   TO THIS-QUE(1:4)                         
                       MOVE EIBTRMID   TO THIS-QUE(5:4)                         
                       MOVE 'COMMAREA' TO THIS-QUE(9:8)                         
                       EXEC CICS READQ TS                                       
                                 QNAME    (THIS-QUE)                            
                                 INTO     (COMM-TEMPAREA)                       
                                 LENGTH   (LENGTH OF DFHCOMMAREA)               
                                 ITEM     (1)                                   
                                 NOHANDLE                                       
                       END-EXEC                                                 
                       IF  EIBRESP = DFHRESP(NORMAL)                            
                                  OR DFHRESP(LENGERR)                           
                           EXEC CICS DELETEQ TS                                 
                                     QNAME    (THIS-QUE)                        
                                     NOHANDLE                                   
                           END-EXEC                                             
                       END-IF                                                   
                       MOVE 'RETN'     TO COMM-TEMPTRN                          
                       MOVE SPACES     TO COMM-TEMPPGM                          
                   END-IF                                                       
      * ... ... overlay current commarea into allocated temp commarea           
                   IF  LENGTH OF DFHCOMMAREA < EIBCALEN                         
                   OR  LENGTH OF COMM-TEMPAREA < EIBCALEN                       
                     IF  LENGTH OF DFHCOMMAREA < LENGTH OF COMM-TEMPAREA        
                       MOVE LENGTH OF DFHCOMMAREA TO OPT-LEN                    
                     ELSE                                                       
                       MOVE LENGTH OF COMM-TEMPAREA TO OPT-LEN                  
                     END-IF                                                     
                   ELSE                                                         
                       MOVE EIBCALEN   TO OPT-LEN                               
                   END-IF                                                       
                   COMPUTE OPT-LEN     =  OPT-LEN                               
                                       -  LENGTH OF COMM-HEADER                 
                                       -  LENGTH OF COMM-SHARED                 
                   IF  OPT-LEN > ZERO                                           
                       MOVE COMM-DETAIL(1:OPT-LEN)                              
                                       TO COMM-TEMPDTL(1:OPT-LEN)               
                   END-IF                                                       
               ELSE                                                             
      * ... else, see if data was passed via START FROM                         
                   MOVE LENGTH OF DFHCOMMAREA TO OPT-LEN                        
                   EXEC CICS RETRIEVE                                           
                             INTO     (COMM-TEMPAREA)                           
                             LENGTH   (OPT-LEN)                                 
                             NOHANDLE                                           
                   END-EXEC                                                     
                   IF  EIBRESP = DFHRESP(ENDDATA)                               
                       CONTINUE                                                 
                   ELSE                                                         
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                                     AND DFHRESP(LENGERR)                       
                           GO TO X00-UNEX-ERR                                   
                       END-IF                                                   
      * ... ... then, if returning from another program...                      
                       IF  COMM-TEMPTRN = 'RETN'                                
                       OR  COMM-TEMPRTN = 'RETN'                                
                           MOVE SPACES TO COMM-TEMPLNK                          
      * ... ... ... then, retrieve our commarea from temp storage               
                           MOVE THIS-TRN   TO THIS-QUE(1:4)                     
                           MOVE EIBTRMID   TO THIS-QUE(5:4)                     
                           MOVE 'COMMAREA' TO THIS-QUE(9:8)                     
                           MOVE LENGTH OF COMM-HOLDAREA TO LINK-LEN             
                           EXEC CICS READQ TS                                   
                                     QNAME    (THIS-QUE)                        
                                     SET      (ADDRESS OF COMM-HOLDAREA)        
                                     LENGTH   (LINK-LEN)                        
                                     ITEM     (1)                               
                                     NOHANDLE                                   
                           END-EXEC                                             
                           IF  EIBRESP = DFHRESP(NORMAL)                        
                                      OR DFHRESP(LENGERR)                       
                             MOVE COMM-HOLDAREA(1:LENGTH OF COMM-HEADER)        
                                       TO COMM-TEMPHDR                          
                             MOVE COMM-HOLDAREA(LENGTH OF COMM-HEADER +         
                                                1:LENGTH OF COMM-SHARED)        
                                       TO COMM-TEMPSHR                          
                             IF  OPT-LEN < (LENGTH OF COMM-HEADER               
                                          + LENGTH OF COMM-SHARED)              
                               COMPUTE OPT-LEN = LENGTH OF COMM-HEADER          
                                               + LENGTH OF COMM-SHARED          
                             END-IF                                             
                             SUBTRACT OPT-LEN FROM LINK-LEN                     
                             IF  LINK-LEN > ZERO                                
                               ADD  1  TO OPT-LEN                               
                               MOVE COMM-HOLDAREA(OPT-LEN:LINK-LEN)             
                                      TO COMM-TEMPAREA(OPT-LEN:LINK-LEN)        
                             END-IF                                             
                             EXEC CICS DELETEQ TS                               
                                       QNAME    (THIS-QUE)                      
                                       NOHANDLE                                 
                             END-EXEC                                           
                           END-IF                                               
                           MOVE 'RETN' TO COMM-TEMPTRN                          
                           MOVE SPACES TO COMM-TEMPPGM                          
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
                                                                                
      * setup header of newly allocated commarea                                
               IF  COMM-TEMPTRN = 'EXIT' OR 'RETN'                              
                   MOVE COMM-TEMPTRN   TO COMM-TEMPRTN                          
               END-IF                                                           
               MOVE THIS-TRN           TO COMM-TEMPTRN                          
                                                                                
      * reinvoke this program with newly allocated commarea                     
               IF  EIBTRNID = THIS-TRN                                          
                   EXEC CICS XCTL                                               
                             PROGRAM  (THIS-PGM)                                
                             COMMAREA (COMM-TEMPAREA)                           
                             LENGTH   (LENGTH OF DFHCOMMAREA)                   
                             NOHANDLE                                           
                   END-EXEC                                                     
               ELSE                                                             
                   IF  COMM-TEMPPGM NOT > SPACES                                
                   AND COMM-TEMPRTN     = 'EXIT'                                
                       EVALUATE EIBTRNID                                        
                       WHEN 'JOBX' MOVE 'JOBSRETN' TO COMM-TEMPPGM              
                       WHEN OTHER  MOVE EIBTRNID   TO COMM-TEMPPGM              
                       END-EVALUATE                                             
                   END-IF                                                       
                   EXEC CICS START                                              
                             TRANSID  (THIS-TRN)                                
                             FROM     (COMM-TEMPAREA)                           
                             LENGTH   (LENGTH OF DFHCOMMAREA)                   
                             TERMID   (EIBTRMID)                                
                             NOHANDLE                                           
                   END-EXEC                                                     
               END-IF                                                           
                                                                                
      * report errors, if any                                                   
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO X00-UNEX-ERR                                           
               END-IF                                                           
      * return to cics, if needed                                               
               EXEC CICS RETURN END-EXEC                                        
                                                                                
           END-IF.                                                              
