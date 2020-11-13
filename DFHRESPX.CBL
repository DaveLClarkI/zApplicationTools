      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    DFHRESPX.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  APR 2006.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       TRANSLATE EIBRESP TO EQUIVALENT TEXT STRING.

      * CHANGE HISTORY ------------------------------------------------
      * 04/18/2006 DLC ORIGINAL PROGRAM.
/DLC0/* 11/24/2009 DLC ALSO RETURN LENGTH OF TRANSLATED TEXT.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-7060-H30.
       OBJECT-COMPUTER. IBM-7060-H30.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       77  FILLER                      PIC  X(11)   VALUE '**STORAGE**'.
       77  LOWVALUE                    PIC  X(01)   VALUE LOW-VALUE.
       77  LN                          PIC S9(04)   BINARY.
       77  SB                          PIC S9(04)   BINARY.
      *
       77  THIS-PGM                    PIC  X(08)   VALUE 'DFHRESPX'.
       77  DFHEITAB                    PIC  X(08)   VALUE 'DFHEITAB'.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
/DLC0/   03  COMM-HEADER.
           05  COMM-RESP               PIC S9(09)   BINARY.
           05  COMM-RESPX              PIC  X(12).
/DLC0/   03  COMM-RESPL                PIC S9(04)   BINARY.

      * CICS EI LITERALS TABLE
       01  DFHEITAB-AREA.
         03                            PIC  X(24).
         03  RESPTAB-ADDR                           POINTER.
         03  RESPTAB-COUNT             PIC S9(09)   BINARY.

      * EIBRESP LITERALS TABLE
       01  EIBRESP-TABLE.
         03  EIBRESP-ENTRY             OCCURS 1 TO 1500
                                       DEPENDING ON RESPTAB-COUNT.
           05  EIBRESP-TEXT            PIC  X(12).
           05  EIBRESP-FLAGS           PIC  X(02).
             88  EIBRESP-TYPE                       VALUE X'50C0'.
           05                          PIC  X(04).
           05  EIBRESP-VALUE           PIC S9(04)   BINARY.
           05                          PIC  X(02).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

/DLC0/*    IF  EIBCALEN < LENGTH OF DFHCOMMAREA
/DLC0/     IF  EIBCALEN < LENGTH OF COMM-HEADER
               IF  EIBCALEN > LENGTH OF COMM-RESP
                   COMPUTE LN          =  EIBCALEN
                                       -  LENGTH OF COMM-RESP
                   MOVE '#CALERR'      TO COMM-RESPX(1:LN)
               ELSE
                   EXEC CICS ABEND
                             ABCODE('CALN')
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS LOAD
                         PROGRAM(DFHEITAB)
                         SET(ADDRESS OF DFHEITAB-AREA)
                         HOLD
               END-EXEC
               SET ADDRESS OF EIBRESP-TABLE TO RESPTAB-ADDR
               PERFORM WITH TEST BEFORE
                 VARYING SB FROM 1 BY 1
                   UNTIL SB > RESPTAB-COUNT
                      OR EIBRESP-TYPE(SB)
                     AND EIBRESP-VALUE(SB) = COMM-RESP
               END-PERFORM
               IF  SB > RESPTAB-COUNT
                   MOVE '@NOTFND'      TO COMM-RESPX
/DLC0/             IF  EIBCALEN >= LENGTH OF DFHCOMMAREA
/DLC0/                 MOVE +7         TO COMM-RESPL
/DLC0/             END-IF
               ELSE
/DLC0/             IF  EIBCALEN < LENGTH OF DFHCOMMAREA
                       MOVE EIBRESP-TEXT(SB) TO COMM-RESPX
/DLC0/             ELSE
/DLC0/                 MOVE SPACES     TO COMM-RESPX
/DLC0/                 MOVE 1          TO LN
/DLC0/                 STRING EIBRESP-TEXT(SB) DELIMITED BY SPACE
/DLC0/                   INTO COMM-RESPX WITH POINTER LN
/DLC0/                 SUBTRACT 1 FROM LN GIVING COMM-RESPL
/DLC0/             END-IF
               END-IF
               EXEC CICS RELEASE
                         PROGRAM(DFHEITAB)
               END-EXEC
           END-IF.

           EXEC CICS RETURN END-EXEC.
           GOBACK.
