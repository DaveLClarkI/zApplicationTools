       01  PJB-BLOCK.
      *  VSE/POWER JOB NAME
           05  PJB-PJBPNAME            PIC  X(8).
      *  VSE/POWER JOB START TIME
           05  PJB-PJBPTIME            PIC  X(8).
      *  VSE/AF JOB STEP START (// EXEC)
           05  PJB-PJBJTIME            PIC  X(8).
      *  VSE/AF JOB - MAX. RETURN CODE
           05  PJB-PJBJRET             PIC S9(9)    COMP.
      *  VSE/POWER JOB NUMBER
           05  PJB-PJBPNUM             PIC  X(2).
      *  VSE/POWER JOB CLASS
           05  PJB-PJBPCLS             PIC  X(1).
           05                          PIC  X(1).
      *  ADDRESS OF USER TOKEN
           05  PJB-PJBRUTKN            POINTER.
      *  VSE/POWER JOB: FROM USERID
           05  PJB-PJBPUSER            PIC  X(8).
      *  VSE/POWER JOB: USER INFORMATION
           05  PJB-PJBPUINF            PIC  X(16).
