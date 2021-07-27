       01  GETQKEY-AREA.
         03  GETQKEY                   PIC  X(8)    VALUE 'GETQKEY'.
         03  GETQKEY-PARMS.
           05  QKEY-ALTLBL             PIC  X(8)    VALUE SPACES.
           05  QKEY-SPECREQ            REDEFINES    QKEY-ALTLBL.
             88  QKEY-TABLREQ                       VALUE '*ADDRESS'.
             07  QKEY-SPECKEY          PIC  X(4).
               88  QKEY-TABLRET                     VALUE '*AD='.
             07  QKEY-TABLADR                       POINTER.
           05  QKEY-BASELBL            PIC  X(8)    VALUE SPACES.
      *
           05  QKEY-ATTRS.
             07  QKEY-BASERECL         PIC S9(4)    BINARY.
             07  QKEY-BASEKEY.
               09  QKEY-BASEKEYO       PIC S9(4)    BINARY.
               09  QKEY-BASEKEYL       PIC S9(4)    BINARY.
             07  QKEY-ALTKEYL          PIC S9(4)    BINARY.
             07  QKEY-AIXKEYS          PIC S9(4)    BINARY.
             07  QKEY-AIXKEY                        OCCURS 9 TIMES.
               09  QKEY-AIXKEYO        PIC S9(4)    BINARY.
               09  QKEY-AIXKEYL        PIC S9(4)    BINARY.
      *
           05  QKEY-CONDS.
             07  QKEY-AIXCONDS         PIC S9(4)    BINARY.
             07  QKEY-AIXCONDE                      OCCURS 9 TIMES.
               09  QKEY-AIXCONDC       PIC  X(2).
               09  QKEY-AIXCONDO       PIC S9(4)    BINARY.
               09  QKEY-AIXCONDL       PIC S9(4)    BINARY.
               09  QKEY-AIXCONDP       PIC  X(2).
               09  QKEY-AIXCONDV       PIC  X(12).
