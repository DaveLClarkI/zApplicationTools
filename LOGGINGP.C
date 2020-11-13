       Q100-LOGIT.
           IF  LOGF-MESG > SPACES
               IF  LOGF-QUEUE NOT > SPACES
                   SET  DTLG-LOGGING   TO TRUE
               END-IF
               IF  LOGF-DEST  NOT > SPACES
                   SET  LOG-TO-LOGFILE TO TRUE
               END-IF
               IF  LOGF-PROG  NOT > SPACES
                   MOVE THIS-PGM       TO LOGF-PROG
               END-IF
               EXEC CICS WRITEQ TD
                         QUEUE(LOGF-QUEUE)
                         FROM(LOGF-RECORD)
               END-EXEC
               MOVE SPACES             TO LOGF-MESG
           END-IF.
       Q199-EXIT.
           EXIT.

       Q200-UNEX-LOG.
           PERFORM X00-UNEX-ERR.
           IF  LOGF-MESG > SPACES
               PERFORM Q100-LOGIT    THRU Q199-EXIT
           END-IF.
           MOVE 1                      TO UNEX-MSGL.
           STRING 'UNEX '  DELIMITED BY SIZE
                  'FN=x''' UNEX-FN         ''''
                 ',RC=x''' UNEX-RCODE(1:6) ''''
                 ',RS='    RESPX-TEXT(1:RESPX-TEXL)
               DELIMITED BY SPACE    INTO LOGF-MESG
                                     WITH POINTER UNEX-MSGL.
           MOVE UNEX-RESP2             TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
           STRING ',R2='   UNEX-VARIABLE
                 ',DS='    UNEX-DS
               DELIMITED BY SPACE    INTO LOGF-MESG
                                     WITH POINTER UNEX-MSGL.
           PERFORM Q100-LOGIT        THRU Q199-EXIT.

           MOVE HEX-CHRT               TO EIBRCODE.
           MOVE RESPX-RESP             TO EIBRESP.
           MOVE RESPX-RESP2            TO EIBRESP2.
       Q201-EXIT.
           GO TO Q205-EXIT.

       Q202-SET-LENGTH.
           IF  UNEX-VARIABLE > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING UNEX-LENG FROM LENGTH OF UNEX-VARIABLE BY -1
                   UNTIL UNEX-LENG < 1
                      OR UNEX-VARIABLE(UNEX-LENG:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO UNEX-LENG
               MOVE SPACES             TO UNEX-VARIABLE
           END-IF.

       Q203-LEFT-JUSTIFY.
           IF  UNEX-VARIABLE > SPACES
               PERFORM Q202-SET-LENGTH
               PERFORM WITH TEST BEFORE
                 VARYING UNEX-LENG FROM UNEX-LENG BY -1
                   UNTIL UNEX-LENG < 1
                      OR UNEX-VARIABLE(1:1) > SPACE
                   MOVE UNEX-VARIABLE(2:UNEX-LENG - 1)
                                       TO UNEX-VARIABLE(1:)
               END-PERFORM
           ELSE
               MOVE ZEROES             TO UNEX-LENG
               MOVE SPACES             TO UNEX-VARIABLE
           END-IF.

       Q204-RIGHT-JUSTIFY.
           PERFORM WITH TEST BEFORE
             VARYING UNEX-INDX FROM UNEX-LENG BY -1
               UNTIL UNEX-INDX < 1
                  OR UNEX-VARIABLE(UNEX-LENG:1) > SPACE
               PERFORM WITH TEST BEFORE
                 VARYING UNEX-POSN FROM UNEX-LENG BY -1
                   UNTIL UNEX-POSN <= 1
                 MOVE UNEX-VARIABLE(UNEX-POSN - 1:1)
                                       TO UNEX-VARIABLE(UNEX-POSN:1)
               END-PERFORM
               MOVE UNEX-PAD           TO UNEX-VARIABLE(1:1)
           END-PERFORM.

       Q205-EXIT.
           EXIT.

       Q300-UNEX-LOG.
           PERFORM X00-UNEX-ERR.
           IF  LOGF-MESG > SPACES
               PERFORM Q100-LOGIT    THRU Q199-EXIT
           END-IF.
           MOVE 'Detected '            TO LOGF-MESG.
           MOVE UNEX-MSG(14:)          TO LOGF-MESG(10:).
           PERFORM Q100-LOGIT        THRU Q199-EXIT.
           MOVE HEX-CHRT               TO EIBRCODE.
           MOVE RESPX-RESP             TO EIBRESP.
           MOVE RESPX-RESP2            TO EIBRESP2.
       Q305-EXIT.
           EXIT.
