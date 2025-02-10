       01  WINWAITO-CONTROLS.
         03  WINWAITO                  PIC  X(08)   VALUE 'WINWAIT0'.
         03  WINWAITO-PARMS.
           05  WINWAITO-LEN            PIC S9(04)   BINARY.
             88  WINWAITO-BYPID                     VALUE 2.
             88  WINWAITO-BYJOB                     VALUE 8.
             88  WINWAITO-BYJOB-WAIT                VALUE 9.
             88  WINWAITO-BYPID-WAIT                VALUE 9.
           05  WINWAITO-DATA.
             07  WINWAITO-JOB-OR-PID   PIC  X(08)   VALUE SPACES.
             07  WINWAITO-WAIT-OPT     PIC  X(01)   VALUE SPACES.
               88  WINWAITO-WAIT                    VALUE 'W'.
