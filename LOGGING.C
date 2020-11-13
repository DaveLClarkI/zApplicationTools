       01  LOGF-AREA.
         03  LOGF-QUEUE                PIC  X(04)   VALUE SPACES.
           88  DAPL-LOGGING                         VALUE 'DAPL'.
           88  DTLG-LOGGING                         VALUE 'DTLG'.
         03  LOGF-RECORD.
           05  LOGF-DEST               PIC  X(1)    VALUE SPACES.
             88  LOG-TO-CONSOLE                     VALUE 'C'.
             88  LOG-TO-LOGFILE                     VALUE 'L'.
             88  LOG-TO-BOTH                        VALUE '*'.
           05  LOGF-PROG               PIC  X(9)    VALUE SPACES.
           05  LOGF-MESG                            VALUE SPACES.
             07  LOGF-LENDSC           PIC  ZZZZZZ9.
             07  LOGF-TEXT             PIC  X(63).
