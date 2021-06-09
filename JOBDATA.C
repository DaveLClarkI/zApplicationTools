       01  JOBDATA-PARMS.
      * POWER JOB NAME
         03  JOBD-PWR-NAME             PIC  X(8).
      * POWER JOB NUMBER
         03  JOBD-PWR-NUMB             PIC  X(6).
      * SYSLOG PARTITION ID (DYNAMIC ONES, TOO)
         03  JOBD-PART-ID              PIC  X(2).
      * POWER JOB START TIME STAMP
         03  JOBD-PWR-STIM             PIC  X(8).
      * POWER FROM USER
         03  JOBD-PWR-FUSR             PIC  X(8).
      * POWER USER INFORMATION
         03  JOBD-PWR-UINF             PIC  X(16).
      * VSE SECURITY USER ID
         03  JOBD-SEC-USER             PIC  X(8).
      * VSE JOB DATE (PARTITION)
         03  JOBD-JOB-DATE             PIC  X(8).
      * VSE JOB NAME
         03  JOBD-JOB-NAME             PIC  X(8).
      * MAIN PROGRAM NAME
         03  JOBD-EXEC-PGM             PIC  X(8).
      * PROGRAM START TIME STAMP
         03  JOBD-PGM-STIM             PIC  X(8).
      * VSE SYSPARM VALUE
         03  JOBD-SYS-PARM             PIC  X(8).
