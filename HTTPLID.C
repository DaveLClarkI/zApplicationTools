      * HTTP TEMPLATE ID FILE RECORD
       01  HTTPLID-RECORD.
         03  TPID-KEY.
           05  TPID-NAME               PIC  X(12).
           05  TPID-TYPE               PIC  X(1).
             88  TPID-REQU                          VALUE 'Q'.
             88  TPID-RESP                          VALUE 'R'.
         03  TPID-DATA.
           05  TPID-FMT                PIC  X(1).
             88  TPID-JSON                          VALUE 'J'.
             88  TPID-XML                           VALUE 'X'.
           05  TPID-DESCRIPTION        PIC  X(60).
           05  TPID-CREATED-BY         PIC  X(8).
           05  TPID-CREATED-TIMESTAMP  PIC S9(15)   PACKED-DECIMAL.
           05  TPID-UPDATED-BY         PIC  X(8).
           05  TPID-UPDATED-TIMESTAMP  PIC S9(15)   PACKED-DECIMAL.
           05                          PIC  X(15).
