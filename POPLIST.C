         03  POPLIST-CONTROL.
           05  POPLIST-CTL             PIC  X(8).
             88  POPLIST-PGM                        VALUE 'POPLIST'.
           05  POPLIST-RTC             PIC  X(4).
             88  POPLIST-ITEM-PICKED                VALUE 'OKAY'.
             88  POPLIST-NO-ITEM-PICKED             VALUE 'EXIT'.
             88  POPLIST-READ-TIMEOUT               VALUE 'TIME'.
             88  POPLIST-COMM-ERROR                 VALUE 'ECOM'.
             88  POPLIST-ROW-ERROR                  VALUE 'EROW'.
             88  POPLIST-COL-ERROR                  VALUE 'ECOL'.
             88  POPLIST-CNT-ERROR                  VALUE 'ECNT'.
           05  POPLIST-PICK            PIC S9(4)    BINARY.
           05  POPLIST-ROW             PIC S9(4)    BINARY.
           05  POPLIST-COL             PIC S9(4)    BINARY.
           05  POPLIST-CNT             PIC S9(4)    BINARY.
           05  POPLIST-BDR             PIC  X(1).
             88  POPLIST-BDR-BLUE                   VALUE '1'.
             88  POPLIST-BDR-RED                    VALUE '2'.
             88  POPLIST-BDR-PINK                   VALUE '3'.
             88  POPLIST-BDR-GREEN                  VALUE '4'.
             88  POPLIST-BDR-TURQU                  VALUE '5'.
             88  POPLIST-BDR-YELLOW                 VALUE '6'.
             88  POPLIST-BDR-WHITE                  VALUE '7'.
           05  POPLIST-OPT             PIC  X(1).
             88  POPLIST-OPT-NORM                   VALUE '0'.
             88  POPLIST-OPT-BRT                    VALUE '8'.
             88  POPLIST-OPT-DARK                   VALUE '@'.
/DLC0/*    05  POPLIST-HDR             PIC  X(35).
/DLC0/     05  POPLIST-HDR             PIC  X(40).
/DLC0/*    05  POPLIST-TLR             PIC  X(35).
/DLC0/     05  POPLIST-TLR             PIC  X(40).
         03  POPLIST-TABLE.
           05  POPLIST-ITM                          OCCURS 1 TO 99
                                                    DEPENDING ON
                                                        POPLIST-CNT.
             07  POPLIST-KEY           PIC  X(6).
/DLC0/*      07  POPLIST-DSC           PIC  X(20).
/DLC0/       07  POPLIST-DSC           PIC  X(25).
