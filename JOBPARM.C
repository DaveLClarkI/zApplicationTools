      *
      * JOB PARM SETTING SUBROUTINE DATA STRUCTURE
      *
       01  JOBPARM-AREA.
         03  JOBPARM                   PIC  X(8)    VALUE 'JOBPARM '.
         03  JOBPARM-PARMS.
           05  JOB-PLEN                PIC  9(03)   BINARY VALUE ZEROES.
           05  JOB-PARM                PIC  X(100)  VALUE SPACES.
