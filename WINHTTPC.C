       01  WINHTTPC-PARMS.
         03  HTTP-TEMPLATE-ID          PIC  X(12).
           88  HTTP-NO-TEMPLATE                     VALUE '**NONE**'.
         03  HTTP-REQUEST              PIC  X(10).
           88  HTTP-REQ-GET                         VALUE 'GET       '.
           88  HTTP-REQ-GET-TEXT                    VALUE 'GET-TEXT  '.
           88  HTTP-REQ-GET-BINARY                  VALUE 'GET-BIN   '.
           88  HTTP-REQ-POST                        VALUE 'POST      '.
           88  HTTP-REQ-POST-TEXT                   VALUE 'POST-TEXT '.
           88  HTTP-REQ-POST-BINARY                 VALUE 'POST-BIN  '.
           88  HTTP-REQ-PUT                         VALUE 'PUT       '.
           88  HTTP-REQ-PUT-TEXT                    VALUE 'PUT-TEXT  '.
           88  HTTP-REQ-PUT-BINARY                  VALUE 'PUT-BIN   '.
           88  HTTP-REQ-PATCH                       VALUE 'PATCH     '.
           88  HTTP-REQ-PATCH-TEXT                  VALUE 'PATCH-TEXT'.
           88  HTTP-REQ-PATCH-BINARY                VALUE 'PATCH-BIN '.
           88  HTTP-REQ-DELETE                      VALUE 'DELETE    '.
           88  HTTP-REQ-CLOSE                       VALUE 'CLOSE     '.
         03  HTTP-RTNCODE              PIC S9(9)    BINARY.
         03  HTTP-URL-ADR                           POINTER.
         03  HTTP-URL-LEN              PIC S9(9)    BINARY.
         03  HTTPS-CERT-MEMBER         PIC  X(8).
         03  HTTP-HEADERS-ADR                       POINTER.
         03  HTTP-HEADERS-LEN          PIC S9(9)    BINARY.
         03  HTTP-SEND-DATA-ADR                     POINTER.
         03  HTTP-SEND-DATA-LEN        PIC S9(9)    BINARY.
         03  HTTP-SEND-CONTENT-ADR                  POINTER.
         03  HTTP-SEND-CONTENT-LEN     PIC S9(9)    BINARY.
         03  HTTP-RECV-DATA-ADR                     POINTER.
         03  HTTP-RECV-DATA-LEN        PIC S9(9)    BINARY.
         03  HTTP-RECV-CONTENT-ADR                  POINTER.
         03  HTTP-RECV-CONTENT-LEN     PIC S9(9)    BINARY.
         03  HTTP-RESPONSE-ADR                      POINTER.
         03  HTTP-RESPONSE-LEN         PIC S9(9)    BINARY.
