      *
      * UNEXPECTED CICS ERROR HANDLING PROCEDURE STATEMENTS
      *
       X00-UNEX-ERR.

           MOVE EIBFN                  TO FUNCX-FUNC.
           MOVE EIBRESP                TO RESPX-RESP
                                          UNEX-RESP.
           MOVE EIBRESP2               TO RESPX-RESP2
                                          UNEX-RESP2.
           MOVE EIBDS                  TO UNEX-DS.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE LENGTH OF EIBFN        TO HEX-LENG.
           MOVE EIBFN                  TO HEX-CHRT.
           CALL 'HEXMAN'            USING HEXMAN-PARMS.
           MOVE HEX-HEXT               TO UNEX-FN.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE LENGTH OF EIBRCODE     TO HEX-LENG.
           MOVE EIBRCODE               TO HEX-CHRT.
           CALL 'HEXMAN'            USING HEXMAN-PARMS.
           MOVE HEX-HEXT               TO UNEX-RCODE.

           EXEC CICS LINK
                     PROGRAM(DFHFUNCX)
                     COMMAREA(DFHFUNCX-COMMAREA)
           END-EXEC.

           IF  RESPX-RESP = DFHRESP(NORMAL)
               MOVE 'NORMAL'           TO RESPX-TEXT
               MOVE +6                 TO RESPX-TEXL
           ELSE
               EXEC CICS LINK
                         PROGRAM(DFHRESPX)
                         COMMAREA(DFHRESPX-COMMAREA)
               END-EXEC
           END-IF.

           MOVE HEX-CHRT               TO EIBRCODE.
           MOVE RESPX-RESP             TO EIBRESP.
           MOVE RESPX-RESP2            TO EIBRESP2.

           MOVE SPACES                 TO UNEX-MSG.
           MOVE 1                      TO UNEX-MSGL.

           EVALUATE RESPX-TEXT
           WHEN 'INVMPSZ'
               STRING 'Please call Support to help you reconfigure '
                      'your screen to a size of 27x132.'
                 DELIMITED BY SIZE INTO UNEX-MSG WITH POINTER UNEX-MSGL
           WHEN OTHER
               STRING EIBTRNID ' failed- '
                      RESPX-TEXT(1:RESPX-TEXL) ' error in '
                      FUNCX-TEXT(1:FUNCX-TEXL) ' command-'
                      ' RC=x''' UNEX-RCODE(1:6) ''','
                      ' DS=' UNEX-DS
                 DELIMITED BY SIZE INTO UNEX-MSG WITH POINTER UNEX-MSGL
           END-EVALUATE.

       X40-UNEX-IC.

           MOVE UNEX-IC                TO UNEX-MSG(UNEX-MSGL:1).

       X50-SEND-MSG.

           IF  EIBTRMID > SPACES
               EXEC CICS SEND CONTROL
                         ERASE DEFAULT
                         NOHANDLE
               END-EXEC
               EXEC CICS SEND
                         FROM(UNEX-MSG)
                         LENGTH(UNEX-MSGL)
                         CTLCHAR(UNEX-RESET)
                         WAIT
                         NOHANDLE
               END-EXEC
           END-IF.

           EXEC CICS RETURN END-EXEC.

       X99-EXIT-ERR.
           EXIT.
