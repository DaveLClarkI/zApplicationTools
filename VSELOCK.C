       01  VSELOCK-AREA.
         03  VSELOCK                   PIC  X(8)    VALUE 'VSELOCK '.
           88  VSELOCK-PGMNAME                      VALUE 'VSELOCK '.
         03  VSELOCK-PARMS.
           05  LOCK-REMLENG            PIC S9(4)    BINARY VALUE +16.
           05  LOCK-REQU               PIC  X(1)    VALUE 'A'.
             88  LOCK-ALLOCATE                      VALUE 'A'.
             88  LOCK-RELEASE                       VALUE 'R'.
           05  LOCK-TIMEOUT            PIC  9(3)    VALUE ZEROES.
           05                          REDEFINES    LOCK-TIMEOUT.
             07  LOCK-BINTYPE          PIC  X(1).
               88  LOCK-BINTYPE-WAIT                VALUE LOW-VALUE.
             07  LOCK-BINWAIT          PIC S9(4)    BINARY.
           05  LOCK-RESNAME            PIC  X(12)   VALUE SPACES.
