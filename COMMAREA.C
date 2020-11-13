         03  COMM-HEADER.                                                       
           05  COMM-TRANSID            PIC  X(4).                               
           05  COMM-CURRPGM.                                                    
             07  COMM-CURRTRN          PIC  X(4).                               
             07  COMM-CURRSFX          PIC  X(4).                               
           05  COMM-CURRRTN            PIC  X(4).                               
           05  COMM-LINKPGM.                                                    
             07  COMM-LINKTRN          PIC  X(4).                               
             07  COMM-LINKSFX          PIC  X(4).                               
         03  COMM-SHARED.                                                       
           05  COMM-HELPAREA.                                                   
             07                        PIC  X(4).                               
             07  COMM-HELPTRAN         PIC  X(4).                               
             07  COMM-HELPNAME         PIC  X(8).                               
           05  COMM-SAVE-FUNC          PIC  X(4).                               
         03  COMM-DETAIL.                                                       
