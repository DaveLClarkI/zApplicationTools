       01  PCE-BLOCK.
      *  ABNORMAL TERMINATION ECB TO POST POWER FOR DEALLOCATION
           05  PCE-PCELEN.
      *    LENGTH OF PCE
             07  PCE-PCELEN            PIC S9(4)    COMP.
      *    FLAG BYTE
             07  PCE-PCEFLAG           PIC  X(1).
      *    FLAG BYTE
             07  PCE-PCEFLAG1          PIC  X(1).
      *  PARTITION'S STORAGE KEY
           05  PCE-PCEKEY              PIC  X(1).
      *  DYNCLASS ID=HOLD COUNT
           05  PCE-PCEHCNT             PIC  X(1).
           05                          PIC  X(2).
      *  PARTITION'S IDENTIFICATION KEY
           05  PCE-PCEPIK              PIC S9(4)    COMP.
      *  PARTITION'S SYSLOG ID
           05  PCE-PCELID              PIC  X(2).
      *  PARTITION'S COMREG ADDRESS
           05  PCE-PCECOMRA            POINTER.
      *  PARTITION'S LUBTAB ADDRESS
           05  PCE-PCEALUB             POINTER.
      *  PARTITION'S DIB ADDRESS
           05  PCE-PCEADIB             POINTER.
      *  PARTITION'S PIB ADDRESS
           05  PCE-PCEPIB              POINTER.
      *  PARTITION'S PIB2 ADDRESS
           05  PCE-PCEPIB2             POINTER.
      *  PARTITION'S CLASS INFORMATION
           05  PCE-PCECLASS            POINTER.
      *  VSE/POWER JOB INFORMATION ADDRESS
           05  PCE-PCEPOWJB            POINTER.
