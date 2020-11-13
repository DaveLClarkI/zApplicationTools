         03  CICS-APPLID               PIC  X(8).                               
         03  CICS-SYSID                PIC  X(4).                               
         03  CICS-PRINSYSID            PIC  X(4).                               
         03  CICS-RELEASE              PIC  X(4).                               
         03  CICS-STATUS               PIC S9(9)    BINARY.                     
         03  CICS-STARTCODE            PIC  X(2).                               
           88  CICS-FROM-TDQUE                      VALUE 'QD'.                 
           88  CICS-FROM-START                      VALUE 'S '.                 
           88  CICS-FROM-STARTD                     VALUE 'SD'.                 
           88  CICS-FROM-TERMINAL                   VALUE 'TD'.                 
           88  CICS-FROM-USER                       VALUE 'U '.                 
         03  CICS-FILEDATE.                                                     
           05  CICS-CCYYMMDD           PIC  9(8).                               
           05                          REDEFINES    CICS-CCYYMMDD.              
             07  CICS-CCYYMM           PIC  9(6).                               
             07                        PIC  9(2).                               
           05                          REDEFINES    CICS-CCYYMMDD.              
             07  CICS-CCYY             PIC  9(4).                               
             07  CICS-MM               PIC  9(2).                               
             07  CICS-DD               PIC  9(2).                               
           05                          PIC  XX.                                 
         03  CICS-FILETIME.                                                     
           05  CICS-HHMMSS             PIC  9(6).                               
           05                          REDEFINES    CICS-HHMMSS.                
             07  CICS-HHMM             PIC  9(4).                               
             07  CICS-SS               PIC  9(2).                               
           05                          PIC  XX.                                 
         03  CICS-FULLDATE             PIC  X(10).                              
         03  CICS-FULLTIME             PIC  X(8).                               
         03  CICS-OPID                 PIC  X(4).                               
         03  CICS-USERID               PIC  X(8).                               
         03  CICS-INVOKEDBY            PIC  X(8).                               
         03  CICS-NETNAME              PIC  X(8).                               
