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
                                                                                
           EVALUATE TRUE                                                        
           WHEN COMM-LINKTRN = 'ECUE'                                           
               IF  LINK-LEN NOT > ZEROES                                        
                   COMPUTE LINK-LEN     = LENGTH OF DFHCOMMAREA                 
                                        - LENGTH OF COMM-HEADER                 
               END-IF                                                           
      *EXEC CICS START                                                          
      *          TRANSID  (COMM-LINKTRN)                                        
      *          FROM     (COMM-DETAIL)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          TERMID   (EIBTRMID)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                MOVE '��   � 00031   ' TO DFHEIV0                             
               CALL 'DFHEI1' USING DFHEIV0  DFHEID0 DFHDUMMY                    
           COMM-LINKTRN COMM-DETAIL LINK-LEN EIBTRMID                           
                                                                                
                                                                                
                                                                                
                                                                                
           WHEN COMM-LINKTRN = 'JOBS'                                           
           OR   COMM-LINKSFX = 'PGM'                                            
           OR   COMM-LINKSFX NOT > SPACES                                       
               IF  LINK-LEN NOT > ZEROES                                        
                   MOVE LENGTH OF DFHCOMMAREA TO LINK-LEN                       
               END-IF                                                           
      *EXEC CICS START                                                          
      *          TRANSID  (COMM-LINKTRN)                                        
      *          FROM     (DFHCOMMAREA)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          TERMID   (EIBTRMID)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                MOVE '��   � 00044   ' TO DFHEIV0                             
               CALL 'DFHEI1' USING DFHEIV0  DFHEID0 DFHDUMMY                    
           COMM-LINKTRN DFHCOMMAREA LINK-LEN EIBTRMID                           
                                                                                
                                                                                
                                                                                
                                                                                
           WHEN OTHER                                                           
               IF  LINK-LEN NOT > ZEROES                                        
                   MOVE LENGTH OF DFHCOMMAREA TO LINK-LEN                       
               END-IF                                                           
      *EXEC CICS XCTL                                                           
      *          PROGRAM  (COMM-LINKPGM)                                        
      *          COMMAREA (DFHCOMMAREA)                                         
      *          LENGTH   (LINK-LEN)                                            
      *          NOHANDLE                                                       
      *END-EXEC                                                                 
                MOVE '�\    00055   ' TO DFHEIV0                             
               CALL 'DFHEI1' USING DFHEIV0  COMM-LINKPGM DFHCOMMAREA            
           LINK-LEN                                                             
                                                                                
                                                                                
                                                                                
           END-EVALUATE.                                                        
                                                                                
      * DFHRESP(NORMAL) = 0                       INSERTED BY TRANSLATOR        
           IF  EIBRESP NOT = 0                                                  
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
                                                                                
           GO TO Z95-CICS-RETURN.                                               
                                                                                
       T40-SAVE-COMMAREA.                                                       
           MOVE EIBTRMID           TO THIS-QUE(1:4).                            
           MOVE THIS-TRN           TO THIS-QUE(5:4).                            
      *EXEC CICS DELETEQ TS                                                     
      *          QUEUE    (THIS-QUE)                                            
      *          NOHANDLE                                                       
      *END-EXEC.                                                                
            MOVE '���   � 00072   ' TO DFHEIV0                                 
           CALL 'DFHEI1' USING DFHEIV0  THIS-QUE.                               
                                                                                
                                                                                
      *EXEC CICS WRITEQ TS                                                      
      *          QUEUE    (THIS-QUE)                                            
      *          FROM     (DFHCOMMAREA)                                         
      *          AUXILIARY                                                      
      *          NOHANDLE                                                       
      *END-EXEC.                                                                
            MOVE '�\   � 00076   ' TO DFHEIV0                                 
            MOVE LENGTH OF DFHCOMMAREA TO DFHB0020                              
           CALL 'DFHEI1' USING DFHEIV0  THIS-QUE DFHCOMMAREA DFHB0020.          
                                                                                
                                                                                
                                                                                
      * DFHRESP(NORMAL) = 0                       INSERTED BY TRANSLATOR        
           IF  EIBRESP NOT = 0                                                  
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
       T45-EXIT.                                                                
           EXIT.                                                                
