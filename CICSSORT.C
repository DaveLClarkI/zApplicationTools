       01  CICSSORT-CONTROL.                                            00001000
         03  CICSSORT                  PIC  X(8)    VALUE 'CICSSORT'.   00091000
         03  CICSSORT-ONAME            PIC  X(8).                       00091000
       01  CICSSORT-PARMS.                                              00001000
         03  CICSSORT-QNAME            PIC  X(8).                       00091000
         03  CICSSORT-RECCNT           PIC  9(4)    COMP.               00094000
         03  CICSSORT-RECLEN           PIC  9(4)    COMP.               00095000
         03  CICSSORT-KEYOFF           PIC  9(4)    COMP.               00096000
         03  CICSSORT-KEYLEN           PIC  9(4)    COMP.               00097000
         03  CICSSORT-SRTDIR           PIC  X(1)    VALUE 'A'.          00098000
           88  CICSSORT-ASCENDING                   VALUE 'A'.          00102000
           88  CICSSORT-DESCENDING                  VALUE 'D'.          00103000
         03  FILLER                    PIC  X(1).                       00099000
         03  CICSSORT-RC               PIC  9(4)    COMP.               00100000
           88  CICSSORT-OK                          VALUE 0.            00101000
           88  CICSSORT-BAD-NAME                    VALUE 1.            00102000
           88  CICSSORT-BAD-QUEUE                   VALUE 2.            00103000
           88  CICSSORT-BAD-NUMBER                  VALUE 3.            00104000
           88  CICSSORT-BAD-SRTTYP                  VALUE 4.            00105000
           88  CICSSORT-BAD-RECLEN                  VALUE 5.            00106000
           88  CICSSORT-BAD-KEYLEN                  VALUE 6.            00107000
           88  CICSSORT-BAD-GETMAIN                 VALUE 7.            00108000
           88  CICSSORT-UNKNOWN-ERROR               VALUE 10.           00109000
