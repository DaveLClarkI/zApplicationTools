      *  WORK AREA FOR BYTE-ENCODED NUMBERS
       01  DOUBLE-WORD                 PIC S9(18)   BINARY VALUE ZEROES.
       01  EIGHT-BYTES                 REDEFINES    DOUBLE-WORD.
         03  FILLER                    PIC  X(4).
         03  FULL-WORD                 PIC S9(9)    BINARY.
         03  FOUR-BYTES                REDEFINES    FULL-WORD.
           05  HI-MSB                  PIC  X.
           05  THREE-BYTES.
             07  HI-LSB                PIC  X.
             07  HALF-WORD             PIC S9(4)    BINARY.
             07  TWO-BYTES             REDEFINES    HALF-WORD.
               09  LO-MSB              PIC  X.
               09  ONE-BYTE            PIC  X.
