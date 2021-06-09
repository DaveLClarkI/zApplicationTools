      *
      * DECODE CICS ABSTIME INTO DDDDDDDHHMMSSUU
      *
       01  ABSTIME-PARMS.                                               00005300
           05  ABS-INPUT               PIC S9(15)   COMP-3.             00008700
           05  ABS-DAYS                PIC S9(11)   COMP-3.             00008800
           05  ABS-HOURS               PIC S9(03)   COMP-3.             00008800
           05  ABS-MINUTES             PIC S9(03)   COMP-3.             00008800
           05  ABS-SECONDS             PIC S9(03)   COMP-3.             00008800
           05  ABS-UNITS               PIC S9(07)   COMP-3.             00008800
           05                          PIC  X(20).                      00008800
