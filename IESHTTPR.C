      ******************************************************************
      * IBM HTTP Proxy Client (IESHTTPB and IESHTTPC) request block    *
      ******************************************************************
       01 HTTP-REQ.
         02  HTTP-REQ-LEN              PIC  9(9)    BINARY.
         02  HTTP-URL-ADR                           POINTER.
         02  HTTP-URL-LEN              PIC  9(9)    BINARY.
      *
      * default is to use HTTP 1.1 (see optional overrides at bottom)
         02  HTTP-REQUEST              PIC  X(4).
           88  HTTP-GET                             VALUE x'00000001'.
           88  HTTP-POST                            VALUE x'00000002'.
           88  HTTP-GET-BINARY                      VALUE x'00000003'.
           88  HTTP-POST-BINARY                     VALUE x'00000004'.
      * added at about VSE v5
           88  HTTP-GET-TEXT                        VALUE x'00000005'.
           88  HTTP-POST-TEXT                       VALUE x'00000006'.
      * added at about VSE v6
           88  HTTP-PUT                             VALUE x'00000007'.
           88  HTTP-PUT-BINARY                      VALUE x'00000008'.
           88  HTTP-PUT-TEXT                        VALUE x'00000009'.
           88  HTTP-HEAD                            VALUE x'0000000A'.
           88  HTTP-DELETE                          VALUE x'0000000B'.
           88  HTTP-OPTIONS                         VALUE x'0000000C'.
           88  HTTP-PATCH                           VALUE x'0000000D'.
           88  HTTP-PATCH-BINARY                    VALUE x'0000000E'.
           88  HTTP-PATCH-TEXT                      VALUE x'0000000F'.
      *
         02  HTTP-USER-AGENT-ADR                    POINTER.
         02  HTTP-USER-AGENT-LEN       PIC  9(9)    BINARY.
         02  HTTP-ACCEPT-ADR                        POINTER.
         02  HTTP-ACCEPT-LEN           PIC  9(9)    BINARY.
      * user status/private data area
         02  HTTP-USER-DATA-ADR                     POINTER.
      *
      * data to send for post processing
         02  HTTP-POST-HANDLER         PIC  9(9)    BINARY.
           88  HTTP-POST-NO-HANDLER                 VALUE 0.
           88  HTTP-POST-BUFFER                     VALUE 1.
           88  HTTP-POST-FUNCTION                   VALUE 2.
           88  HTTP-POST-PROGRAM                    VALUE 3.
         02  HTTP-POST-DATA-ADR                     POINTER.
         02  HTTP-POST-DATA-LEN        PIC  9(9)    BINARY.
         02  HTTP-POST-CONTENT-ADR                  POINTER.
         02  HTTP-POST-CONTENT-LEN     PIC  9(9)    BINARY.
      *
      * data received from get/post processing
         02  HTTP-GET-HANDLER          PIC  9(9)    BINARY.
           88  HTTP-GET-BUFFER                      VALUE 1.
           88  HTTP-GET-FUNCTION                    VALUE 2.
           88  HTTP-GET-PROGRAM                     VALUE 3.
           88  HTTP-GET-CONTAINER                   VALUE 4.
         02  HTTP-GET-DATA-ADR                      POINTER.
         02  HTTP-GET-DATA-LEN         PIC  9(9)    BINARY.
         02  HTTP-GET-CONTENT-ADR                   POINTER.
         02  HTTP-GET-CONTENT-LEN      PIC  9(9)    BINARY.
      *
      * this is the HTTP response -- not the proxy return code
         02  HTTP-RESPONSE-ADR                      POINTER.
         02  HTTP-RESPONSE-LEN         PIC  9(9)    BINARY.
      *
      * proxy/socks settings
         02  HTTP-PROXY-TYPE           PIC  9(9)    BINARY.
           88  HTTP-TYPE-DIRECT                     VALUE 0.
           88  HTTP-TYPE-PROXY                      VALUE 1.
           88  HTTP-TYPE-SOCKS4                     VALUE 2.
           88  HTTP-TYPE-SOCKS5                     VALUE 3.
         02  HTTP-PROXY-URL-ADR                     POINTER.
         02  HTTP-PROXY-URL-LEN        PIC  9(9)    BINARY.
         02  HTTP-PROXY-PORT           PIC  9(9)    BINARY.
         02  HTTP-USER-ADR                          POINTER.
         02  HTTP-USER-LEN             PIC  9(9)    BINARY.
         02  HTTP-PASSWORD-ADR                      POINTER.
         02  HTTP-PASSWORD-LEN         PIC  9(9)    BINARY.
      *
      * code page overrides
         02  HTTP-ASCII-CPID-ADR                    POINTER.
         02  HTTP-ASCII-CPID-LEN       PIC  9(9)    BINARY.
         02  HTTP-EBCDIC-CPID-ADR                   POINTER.
         02  HTTP-EBCDIC-CPID-LEN      PIC  9(9)    BINARY.
      * optional additional header line(s)
         02  HTTP-HDR-LINE-ADR                      POINTER.
         02  HTTP-HDR-LINE-LEN         PIC  9(9)    BINARY.
      * optional new location (for 301/302 response)
         02  HTTP-NEW-LOCATION-ADR                  POINTER.
         02  HTTP-NEW-LOCATION-LEN     PIC  9(9)    BINARY.
      *
      * SSL settings for HTTPS
         02  HTTP-KEY-RING-LIB-ADR                  POINTER.
         02  HTTP-KEY-RING-LIB-LEN     PIC  9(9)    BINARY.
         02  HTTP-KEY-NAME-ADR                      POINTER.
         02  HTTP-KEY-NAME-LEN         PIC  9(9)    BINARY.
         02  HTTP-CIPER-SPECS-ADR                   POINTER.
         02  HTTP-CIPER-SPECS-LEN      PIC  9(9)    BINARY.
         02  HTTP-SESS-TIMEOUT         PIC  9(9)    BINARY.
      *
      * HTTP authentication
         02  HTTP-AUTH-USER-ADR                     POINTER.
         02  HTTP-AUTH-USER-LEN        PIC  9(9)    BINARY.
         02  HTTP-AUTH-PWD-ADR                      POINTER.
         02  HTTP-AUTH-PWD-LEN         PIC  9(9)    BINARY.
      *
      * additional SSL settings for HTTPS (VSE v5+, only)
         02  HTTP-SSL-TYPE-ADR                      POINTER.
         02  HTTP-SSL-TYPE-LEN         PIC  9(9)    BINARY.
      *
      * send/receive additional HTTP headers (VSE v6+, only)
         02  HTTP-HDR-HANDLER          PIC  9(9)    BINARY.
           88  HTTP-HDR-NO-HANDLER                  VALUE 0.
           88  HTTP-HDR-FUNCTION                    VALUE 1.
           88  HTTP-HDR-PROGRAM                     VALUE 2.
         02  HTTP-HDR-HANDLER-ADR                   POINTER.

      *
      * possible R15 return codes (or RESP2 in CICS)
       01  HTTP-RETURN-CODES.
         02  HTTP-RC-NO-ERROR          PIC  9(9)    BINARY VALUE 0.
         02  HTTP-RC-INVALID-PARM      PIC  9(9)    BINARY VALUE 1.
         02  HTTP-RC-NULL-POINTER      PIC  9(9)    BINARY VALUE 2.
         02  HTTP-RC-NETWORK-ERR       PIC  9(9)    BINARY VALUE 3.
         02  HTTP-RC-URL-ERROR         PIC  9(9)    BINARY VALUE 4.
         02  HTTP-RC-UNKNOWN-HOST      PIC  9(9)    BINARY VALUE 5.
         02  HTTP-RC-CONNECT-FAILED    PIC  9(9)    BINARY VALUE 6.
         02  HTTP-RC-CONNECTION-BROKEN PIC  9(9)    BINARY VALUE 7.
         02  HTTP-RC-CONNECTION-CLOSED PIC  9(9)    BINARY VALUE 8.
         02  HTTP-RC-INVALID-RESP      PIC  9(9)    BINARY VALUE 9.
         02  HTTP-RC-NOT-ALLOWED       PIC  9(9)    BINARY VALUE 10.
         02  HTTP-RC-CODEPAGE          PIC  9(9)    BINARY VALUE 11.
         02  HTTP-RC-SSL-INIT          PIC  9(9)    BINARY VALUE 12.
         02  HTTP-RC-SSL-HANDSHAKE     PIC  9(9)    BINARY VALUE 13.
         02  HTTP-RC-NO-MEMORY         PIC  9(9)    BINARY VALUE 14.
         02  HTTP-RC-COMMAREA-LEN      PIC  9(9)    BINARY VALUE 15.
         02  HTTP-RC-PARAM-LEN         PIC  9(9)    BINARY VALUE 16.
      * added at about VSE v6
         02  HTTP-RC-PROGRAM-ERROR     PIC  9(9)    BINARY VALUE 17.
         02  HTTP-RC-CONTAINER-ERROR   PIC  9(9)    BINARY VALUE 18.
      * added at about VSE v6.3
         02  HTTP-RC-HOSTNAME-CHANGED  PIC  9(9)    BINARY VALUE 19.
         02  HTTP-RC-MP-LENGTH-ERROR   PIC  9(9)    BINARY VALUE 20.
         02  HTTP-RC-MP-BNDRY-NOTFND   PIC  9(9)    BINARY VALUE 21.
         02  HTTP-RC-MP-BNDRY-INVALID  PIC  9(9)    BINARY VALUE 22.
         02  HTTP-RC-MP-EOHDR-NOTFND   PIC  9(9)    BINARY VALUE 23.
         02  HTTP-RC-MP-CDISP-NOTFND   PIC  9(9)    BINARY VALUE 24.
         02  HTTP-RC-PERSIST-TIMEOUT   PIC  9(9)    BINARY VALUE 25.
      *
      * optional overrides to HTTP-REQUEST (or'd after setting it first)
         02  HTTP-TRACE-TO-SYSLOG      PIC  X(4)    VALUE x'01000000'.
         02  HTTP-TRACE-TO-SYSLST      PIC  X(4)    VALUE x'02000000'.
      * added at about VSE v6
         02  HTTP-FORCE-HTTP-1-0       PIC  X(4)    VALUE x'10000000'.
      * added at about VSE v6.3
         02  HTTP-TRACE-MULTIPART      PIC  X(4)    VALUE x'04000000'.
         02  HTTP-FORCE-USE-SNI        PIC  X(4)    VALUE x'20000000'.
         02  HTTP-USE-PERS-CONN        PIC  X(4)    VALUE x'40000000'.
         02  HTTP-CLOSE-CONNECTION     PIC  X(4)    VALUE x'80000000'.
