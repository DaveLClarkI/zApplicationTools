 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    CICSINFO.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  FEB 2011.
       DATE-COMPILED.
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       CICS INFO AND TIMESTAMP SUBROUTINE.

      * CHANGE HISTORY ------------------------------------------------
      * 02/24/2011 DLC ORIGINAL PROGRAM.
/DLC0/* 11/22/2019 DLC ADD NETNAME AS OPTIONAL FIELD IN COMMAREA.
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

       SOURCE-COMPUTER. IBM-2086-A04-140.
       OBJECT-COMPUTER. IBM-2096-N03.

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

       01  CONTROL-FIELDS.
         03  WS-ABSTIME                PIC S9(15)   PACKED-DECIMAL.
         03  PTR1                                   POINTER.
         03  NUM1      REDEFINES PTR1  PIC  9(09)   BINARY.
         03  PTR2                                   POINTER.
         03  NUM2      REDEFINES PTR2  PIC  9(09)   BINARY.
         03  LEN                       PIC S9(04)   BINARY.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
         COPY CICSINFO.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

           MOVE SPACES                 TO CICS-OPID.

           EXEC CICS ASSIGN
                     APPLID      (CICS-APPLID   )
                     INVOKINGPROG(CICS-INVOKEDBY)
                     OPID        (CICS-OPID     )
                     PRINSYSID   (CICS-PRINSYSID)
                     STARTCODE   (CICS-STARTCODE)
                     SYSID       (CICS-SYSID    )
                     USERID      (CICS-USERID   )
                     NOHANDLE
           END-EXEC.

           SET  PTR1                   TO ADDRESS OF DFHCOMMAREA.
           SET  PTR2                   TO ADDRESS OF CICS-NETNAME.
           SUBTRACT NUM1             FROM NUM2
                                   GIVING LEN.
           IF  EIBCALEN > LEN
               EXEC CICS ASSIGN
                         NETNAME     (CICS-NETNAME  )
                         NOHANDLE
               END-EXEC
           END-IF.

           EVALUATE TRUE
           WHEN CICS-OPID NOT > SPACES
               MOVE CICS-USERID(1:3)   TO CICS-OPID
           WHEN CICS-USERID NOT > SPACES
               MOVE CICS-OPID          TO CICS-USERID
           END-EVALUATE.

           EXEC CICS INQUIRE SYSTEM
                     CICSSTATUS  (CICS-STATUS   )
                     RELEASE     (CICS-RELEASE  )
                     NOHANDLE
           END-EXEC.

           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD    (CICS-FILEDATE )
                     TIME        (CICS-FILETIME )
                     NOHANDLE
           END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     FULLDATE    (CICS-FULLDATE )   DATESEP
                     TIME        (CICS-FULLTIME )   TIMESEP
                     NOHANDLE
           END-EXEC.

           EXEC CICS RETURN END-EXEC.
           GOBACK.

