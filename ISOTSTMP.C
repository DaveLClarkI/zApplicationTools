       01  ISOTSTMP-PARMS.
         03  ISO-OPTION                PIC  A(1).
           88  ISO-FORMAT                           VALUE 'I'.
           88  NUM-FORMAT                           VALUE 'N'.
         03  ISO-TIMESTAMP.
           05  ISO-DATE-FORMAT         PIC  X(10).
           05  ISO-DATE-DELIM          PIC  X(1).
           05  ISO-TIME-FORMAT         PIC  X(8).
           05  ISO-TIME-DELIM          PIC  X(1).
           05  ISO-MILLISECONDS        PIC  9(6).
         03  NUM-TIMESTAMP             REDEFINES    ISO-TIMESTAMP.
           05  NUM-DATE-FORMAT         PIC  9(8).
           05  NUM-TIME-FORMAT         PIC  9(6).
           05  NUM-MILLISECONDS        PIC  9(6).
