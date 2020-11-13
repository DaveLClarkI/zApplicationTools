       01  UNEX-CONTROLS.
         03  UNEX-WORKAREA.
           05  UNEX-FN                 PIC  X(04)   VALUE SPACES.
           05  UNEX-RCODE              PIC  X(12)   VALUE SPACES.
           05  UNEX-RESP               PIC  Z(6)9-.
           05  UNEX-RESP2              PIC  Z(6)9-.
           05  UNEX-DS                 PIC  X(08).
           05  UNEX-RESET              PIC  X(01)   VALUE 'B'.
           05  UNEX-IC                 PIC  X(01)   VALUE X'13'.
           05  UNEX-PAD                PIC  X(01)   VALUE SPACE.
           05  UNEX-POSN               PIC S9(04)   BINARY.
           05  UNEX-INDX               PIC S9(04)   BINARY.
           05  UNEX-LENG               PIC S9(04)   BINARY.
           05  RESPX-RESP2             PIC S9(09)   BINARY.
           05  UNEX-VARIABLE           PIC  X(80).
         03  DFHFUNCX                  PIC  X(08)   VALUE 'DFHFUNCX'.
         03  DFHFUNCX-COMMAREA.
           05  FUNCX-FUNC              PIC  X(2)    VALUE LOW-VALUES.
           05  FUNCX-TEXL              PIC S9(04)   BINARY.
           05  FUNCX-TEXT              PIC  X(20)   VALUE SPACES.
         03  DFHRESPX                  PIC  X(08)   VALUE 'DFHRESPX'.
         03  DFHRESPX-COMMAREA.
           05  RESPX-RESP              PIC S9(09)   BINARY.
           05  RESPX-TEXT              PIC  X(12)   VALUE SPACES.
           05  RESPX-TEXL              PIC S9(04)   BINARY.
      *
      * UNEXPECTED CICS ERROR HANDLING DATA STRUCTURE
      *
       01  UNEX-ERR.
           05  UNEX-MSGL               PIC S9(04)   COMP VALUE +0080.
           05  UNEX-MSG                PIC  X(80)   VALUE SPACES.
