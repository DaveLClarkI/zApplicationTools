       01  COMM-TEMPAREA.                                                       
         03  COMM-TEMPHDR.                                                      
           05  COMM-TEMPTRN            PIC  X(4).                               
           05  COMM-TEMPPGM            PIC  X(8).                               
           05  COMM-TEMPRTN            PIC  X(4).                               
           05  COMM-TEMPLNK            PIC  X(8).                               
         03  COMM-TEMPSHR.                                                      
           05                          PIC  X(20).                              
         03  COMM-TEMPDTL.                                                      
           05                          PIC  X(9950).                            
                                                                                
       01  COMM-HOLDAREA.                                                       
           05                          PIC  X(9994).                            
                                                                                
       01  MAPTITLE-RECORD.                                                     
         03                            PIC  X(20).                              
         03  MAPTITLE-VALUE            PIC  X(40).                              
         03                            PIC  X(190).                             
