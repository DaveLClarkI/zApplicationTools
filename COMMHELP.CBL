       T50-HELP-TRANSFER.                                                       
           MOVE 'HELP'                 TO COMM-HELPAREA.                        
           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.                       
           PERFORM T40-SAVE-COMMAREA THRU T45-EXIT.                             
           MOVE SPACES                 TO COMM-HELPAREA.                        
                                                                                
       T51-HELP-SAVE-DONE.                                                      
           MOVE THIS-TRN               TO COMM-HELPTRAN.                        
           IF  COMM-CURRRTN > SPACES                                            
               MOVE COMM-CURRRTN       TO COMM-HELPNAME                         
           ELSE                                                                 
               MOVE COMM-CURRPGM       TO COMM-HELPNAME                         
           END-IF.                                                              
                                                                                
       T52-HELP-SETUP-DONE.                                                     
           MOVE 'EXIT'                 TO COMM-TRANSID.                         
           MOVE 'HELP'                 TO COMM-LINKPGM.                         
                                                                                
           COMPUTE LINK-LEN            =  LENGTH OF COMM-HEADER                 
                                       +  LENGTH OF COMM-HELPAREA.              
           EXEC CICS START                                                      
                     TRANSID  (COMM-LINKTRN)                                    
                     FROM     (DFHCOMMAREA)                                     
                     LENGTH   (LINK-LEN)                                        
                     TERMID   (EIBTRMID)                                        
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO X00-UNEX-ERR                                               
           END-IF.                                                              
                                                                                
           GO TO Z95-CICS-RETURN.                                               
