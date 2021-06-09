       01  COMREG-BLOCK.
      *  JOB DATE WITH CENTURY
           05  COMREG-JOBDATWC.
      *    JOB DATE
             07  COMREG-JOBDATE        PIC  X(8).
      *    CENTURY
             07  FILLER                PIC  X(3).
           05  FILLER                  PIC  X(1).
      *  USER SCRATCH AREA
           05  COMREG-COMUSCR          PIC  X(11).
      *  UPSI BYTE
           05  COMREG-UPSI             PIC  X(1).
      *  VSE JOB NAME
           05  COMREG-COMNAME          PIC  X(8).
      *  HIGHEST STORAGE ADDR OF PARTITION
           05  COMREG-PPEND            PIC  X(4).
      *  END ADDR OF LAST PHASE LOADED
           05  COMREG-HIPHAS           PIC  X(4).
      *  END ADDR OF LONGEST PHASE LOADED
           05  COMREG-HIPROG           PIC  X(4).
      *  LENGTH OF PP LABEL AREA
           05  COMREG-LABLEN           PIC S9(4)    COMP.
      *  PROGRAM INTERRUPT KEY (PIK)
           05  COMREG-PID              PIC S9(4)    COMP.
      *  END OF VIRTUAL STORAGE (LAST BYTE IN ADDRESS SPACE)
           05  COMREG-EOCADR           PIC  X(4).
      *  MACHINE CONFIGURATION BYTE
           05  COMREG-CONFIG           PIC  X(1).
      *  SYSTEM CONFIGURATION BYTE
           05  COMREG-LTACT            PIC  X(1).
      *  STDJC SWITCH 1
           05  COMREG-SOB1             PIC  X(1).
      *  STDJC SWITCH 2
           05  COMREG-SOB2             PIC  X(1).
      *  JOB CONTROL SWITCHES
           05  COMREG-JCSW1            PIC  X(1).
      *  LINKAGE CONTROL BYTE
           05  COMREG-JCSW2            PIC  X(1).
      *  LANGUAGE TRANSLATOR CONTROL BYTE
           05  COMREG-JCSW3            PIC  X(1).
      *  JOB DURATION INDICATOR BYTE
           05  COMREG-JCSW4            PIC  X(1).
      *  JOB CONTROL SWITCH
           05  COMREG-JCSW7            PIC  X(1).
      *  PROCEDURE NESTING LEVEL
           05  COMREG-NSTLEVEL         PIC  X(1).
      *  JOB CONTROL FLAGS
           05  COMREG-JCSW8            PIC  X(1).
           05  FILLER                  PIC  X(1).
      *  ADDRESS OF PUB TABLE
           05  COMREG-PUBPT            PIC S9(4)    COMP.
      *  JOB START TIME
           05  COMREG-IJBJOBST         PIC  X(6).
      *  ADDRESS OF FICL
           05  COMREG-FICLPT           PIC S9(4)    COMP.
      *  ADDRESS OF NICL
           05  COMREG-NICLPT           PIC S9(4)    COMP.
      *  ADDRESS OF LUB TABLE
           05  COMREG-LUBPT            PIC S9(4)    COMP.
      *  SYSLST LINE COUNT
           05  COMREG-SYSLINE          PIC  X(1).
      *  SYSTEM DATE
           05  COMREG-SYSDATE.
      *    MMDD OR DDMM
             07  COMREG-MMDD           PIC  9(4).
      *    YYDDD PORTION OF DATE
             07  COMREG-YYDDD          PIC  9(5).
      *  LIOCS COMMUNICATIONS BYTES
           05  COMREG-LIOCSCOM.
      *    LIOCS SWITCH 1
             07  COMREG-LIOCSSW1       PIC  X(1).
      *    LIOCS SWITCH 2
             07  COMREG-LIOCSSW2       PIC  X(1).
      *  ADDRESS OF PIB TABLE
           05  COMREG-PIBPT            PIC S9(4)    COMP.
      *  LAST CHECKPOINT NUMBER
           05  COMREG-CHKPTID          PIC S9(4)    COMP.
      *  JOB ZONE IN MINUTES
           05  COMREG-JOBZON           PIC S9(4)    COMP.
      *  BACKGROUND DIB POINTER
           05  COMREG-DIBPT            PIC S9(4)    COMP.
      *  DEVICE FLAG
           05  COMREG-DEVFLG1          PIC  X(1).
      *  FLAG BYTE
           05  COMREG-OPNBYT2          PIC  X(1).
      *  CENTURY OF SYSDATE
           05  COMREG-SYSCENT          PIC S9(4)    COMP.
           05  FILLER                  PIC  X(4).
      *  KEY OF PROGRAM WITH TI SUPPORT
           05  COMREG-PWTIMS           PIC S9(4)    COMP.
      *  ACTIVE SPACE ID OF PART.
           05  COMREG-IJBASID          PIC XX.
      *  LOGICAL TRANSIENT KEY
           05  COMREG-LTK              PIC S9(4)    COMP.
      *  ADDRESS OF SYSPARM FIELD
           05  COMREG-SYSPARM          POINTER.
      *  POINTER TO JA PARTITION TABLE
           05  COMREG-JAPART           POINTER.
      *  ADDR OF TOD CLOCK COMM AREA
           05  COMREG-TODCOM           POINTER.
      *  ADDRESS OF PIB EXTENSION
           05  COMREG-PIB2PTR          PIC S9(4)    COMP.
      *  ADDR OF MICR DTF TABLE
           05  COMREG-PDTABB           PIC S9(4)    COMP.
      *  ADDR OF LABEL SPACE
           05  COMREG-LABELPTR         POINTER.
      *  RESERVED
           05  COMREG-BGCOMPT          PIC S9(4)    COMP.
      *  OPTION INDICATOR BYTE
           05  COMREG-OPTNBYTE         PIC  X(1).
      *  SYSTEM CONFIG BYTE 2
           05  COMREG-RMSROPEN         PIC  X(1).
      *  A(JOB CONTROL WORK AREA)
           05  COMREG-IJBJCWA          POINTER.
      *  STANDARD OPTIONS
           05  COMREG-STDOPT           PIC  X(1).
      *  TEMPORARY OPTIONS
           05  COMREG-TEMOPT           PIC  X(1).
      *  DISK CONFIG BYTE
           05  COMREG-DISKCONF         PIC  X(1).
      *  PROCEDURE NAME VB
           05  COMREG-PROCNAM          PIC  X(8).
      *  INTERFACE FLAGS FOR CAT. PROC. VB
           05  COMREG-PSWTCH           PIC  X(1).
      *  STATEMENT NAME VB
           05  COMREG-POVNAM           PIC  X(7).
      *  INDICATOR 81 BYTE
           05  COMREG-INSIZE           PIC  X(1).
      *  ADDR OF PART CONT BLOCK
           05  COMREG-POWPCB           POINTER.
      *  POWER/VS FLAG BYTE 1
           05  COMREG-POWFLG1          PIC  X(1).
      *  POWER/VS FLAG BYTE 2
           05  COMREG-POWFLG2          PIC  X(1).
      *  VSAM SNAP FUNCTION BYTES
           05  COMREG-IJBVSSNP         PIC  X(2).
      *  ADDR OF LUB TABLE EXTENS
           05  COMREG-LUBEXT           POINTER.
      *  JOB CONTROL SWITCH 5
           05  COMREG-JCSW5            PIC  X(1).
      *  JOB CONTROL SWITCH 6
           05  COMREG-JCSW6            PIC  X(1).
      *  STANDARD OPTIONS 2
           05  COMREG-STDOPT2          PIC  X(1).
      *  TEMPORARY OPTIONS 2
           05  COMREG-TEMOPT2          PIC  X(1).
      *  ADDR OF JPL OF PARTITION
           05  COMREG-IJBJPL           POINTER.
      *  SLOT FOR CICS
           05  COMREG-IJBAFCB          POINTER.
      *  ADDR.FETCH/LOAD TRACE TAB
           05  COMREG-IJBPHLST         POINTER.
      *  ADDR.OF LAST HC JOBSTMNT
           05  COMREG-IJBJOBLG.
      *    CYCLE BYTE OF JOBSTMNT
             07  COMREG-IJBDSKCB       PIC  X(1).
      *    DISK ADDRESS OF JOBSTMNT
             07  COMREG-IJBDSKAD       PIC  X(5).
      *    LOGICAL REC. OF JOBSTMNT
             07  COMREG-IJBDSKLR       PIC  X(2).
      *  A(SPF CONTROL AREA)
           05  COMREG-IJBASPF          POINTER.
      *  ADDR.OF GETVIS CNTL.INF.
           05  COMREG-IJBGVCTL         POINTER.
      *  A(TAPE FILE CNTL BLOCKS)
           05  COMREG-IJBIJJT          POINTER.
      *  SYSTEM GETVIS PART.POOL
           05  COMREG-IJBJOBLG.
      *    FIXED PART OF THE NAME
             07  COMREG-IJBSPFIX       PIC  X(4).
      *    VARIABLE PART IN SUBPOOL NAME (SYSLOG ID)
             07  COMREG-IJBSPNLI       PIC  X(2).
      *    SUBPOOL INDEX
             07  COMREG-IJBSPIND       PIC  X(2).
      *  EXEC PHASE NAME
           05  COMREG-IJBPHNAM         PIC  X(8).
      *  MIRROR DE ENTRY CHAIN
           05  COMREG-IJBDECPY         POINTER.
      *  PARTITION UNBATCH ECB
           05  COMREG-IJBECBU.
      *    USED BY POWER
             07  COMREG-IJBECBLN       PIC  X(2).
      *    ECB BYTE
             07  COMREG-IJBECBUB       PIC  X(1).
      *    USED BY POWER
             07  COMREG-IJBECBU2       PIC  X(1).
      *  JCL SWITCH 9
           05  COMREG-JCSW9            PIC  X(1).
      *  MISC. SWITCH BYTE
           05  COMREG-IJBMSWB1         PIC  X(1).
      *  IND.FOR VSAM SNAP DUMP O.
           05  COMREG-IJBVSMLU         PIC  X(1).
           05  FILLER                  PIC  X(1).
      *  JCL BASE VECTOR
           05  COMREG-IJBJCLBV         POINTER.
      *  ADDRESS OF PART.PCE
           05  COMREG-IJBPCEPT         POINTER.
      *  END ADDRESS OF HIGHEST LOADED PHASE
           05  COMREG-IJBHPHLA         PIC  X(4).
      *  VMCF USERID
           05  COMREG-IJBOWNID         PIC  X(8).
      *  VMCF PROCESSING FLAG 1
           05  COMREG-IJBVMCF1         PIC  X(1).
      *  TEMPORARY ECHO LEVEL
           05  COMREG-IJBTELVL         PIC  X(1).
      *  POWER FLAG BYTE 3
           05  COMREG-POWFLG3          PIC  X(1).
      *  JCL STANDARD OPTIONS 3
           05  COMREG-IJBSTOP3         PIC  X(1).
      *  JCL TEMP. OPTIONS 3
           05  COMREG-IJBTMOP3         PIC  X(1).
      *  REXX FLAG BYTE
           05  COMREG-IJBRXFLG         PIC  X(1).
           05  FILLER                  PIC  X(2).
      *  PERM. PFIX LIMIT BELOW
           05  COMREG-IJBPFLBP         PIC  X(4).
      *  PERM. PFIX LIMIT ABOVE
           05  COMREG-IJBPFLAP         PIC  X(4).
      *  USERID OF POWER JOB
           05  COMREG-IJBPOWUI         PIC  X(8).
      *  JCL ENTRY POINT (REXX)
           05  COMREG-IJBRXJCL         POINTER.
      *  UTILITY RC (SA ENV.)
           05  COMREG-IJBSARC          PIC  X(4).
      *  SA UTILITY CB
           05  COMREG-IJBSAUCB         POINTER.
      *  AUTH. PROG. FAC. PTR
           05  COMREG-IJBAPFP          POINTER.
      *  SECURITY CB
           05  COMREG-IJBSENV          POINTER.
      *  JOB // ID USER
           05  COMREG-IJBDUCPS         PIC  X(8).
      *  reserved
           05                          PIC  X(12).
