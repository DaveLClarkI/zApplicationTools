      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    VSAMSTAT.
       AUTHOR.        DAVE L CLARK.
       DATE-WRITTEN.  AUG 2005.
       DATE-COMPILED.
       INSTALLATION.  DAPSCO INFORMATION SYSTEMS.
      *REMARKS.       VSAM STATUS DISPLAY.

      * CHANGE HISTORY ------------------------------------------------
      * 08/29/2005 DLC ORIGINAL PROGRAM
      * END OF HISTORY ------------------------------------------------

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *     V S A M   S T A T U S   D I S P L A Y   P R O G R A M     *
      *                                                               *
      *                                                               *
      * DAPSCO Information Systems                    Written by:     *
      * 3110 Kettering Blvd.                       Dave Leonard Clark *
      * Dayton, OH  45439                            (937)294-5331    *
      *                                                               *
      * No warranty is expressed or implied.   Neither the author nor *
      * the company is responsible  for  any loss or damage resulting *
      * from the use of this  code.   Source  is provided on an as-is *
      * basis.                                                        *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                            PURPOSE                            *
      *                                                               *
      *      This program merely  provides  a  standardized method of *
      * displaying VSAM status  information  on  both  SYSLST and the *
      * system console.                                               *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                        IMPLEMENTATION                         *
      *                                                               *
      *      At least three and up  to  five data areas may be passed *
      * to this program.   The following sample COBOL copybook layout *
      * demonstrates these areas.                                     *
      *                                                               *
      *    01  VSAMSTAT-AREAS.                                        *
      *        05  VSAMSTAT            PIC  X(8)    VALUE 'VSAMSTAT'. *
      *        05  VSAMSTAT-FILENAME   PIC  X(8)    VALUE SPACES.     *
      *        05  VSAMSTAT-VERBNAME   PIC  X(8)    VALUE SPACES.     *
      *        05  VSAMSTAT-STATUS     PIC  X(2)    VALUE SPACES.     *
      *    01  VSAMSTAT-KEYAREA.                                      *
      *        05  VSAMSTAT-KEYLENG    PIC S9(4)    BINARY VALUE ZERO.*
      *        05  VSAMSTAT-KEYDATA    PIC  X(98)   VALUE SPACES.     *
      *    01  VSAMSTAT-FEEDBACK.                                     *
      *        05  VSAMSTAT-RC         PIC S9(4)    BINARY.           *
      *        05  VSAMSTAT-FC         PIC S9(4)    BINARY.           *
      *        05  VSAMSTAT-EC         PIC S9(4)    BINARY.           *
      *                                                               *
      * The exact structure  of  the  first  area  is not required as *
      * shown -- as the sub  areas  must be passed separately to this *
      * program anyway (see the  next section).   The exact structure *
      * of the next two areas, however, is required exactly as shown. *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                          INVOCATION                           *
      *                                                               *
      *      This program is  invoked  from  another COBOL program in *
      * various  ways  and  with   a   varying  number  of  arguments *
      * (depending upon the  desired  result).   Up to five arguments *
      * are supported.  The first three arguments are required -- but *
      * may be passed  either  by  reference  or  by content (as this *
      * program does not modify any of its arguments).   The last two *
      * arguments are  optional  and,  if  omitted,  merely  serve to *
      * reduce the amount of diagnostic information displayed.        *
      *                                                               *
      *      The following shows a few  of the main ways for invoking *
      * this program.  Feel free to mix-n-match as suits your needs.  *
      *                                                               *
      *    CALL VSAMSTAT USING BY CONTENT                             *
      *                        'MYFILE  '                             *
      *                        'READNEXT'                             *
      *                        BY REFERENCE                           *
      *                        VSAMSTAT-STATUS.                       *
      *                                                               *
      *                            - or -                             *
      *                                                               *
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *
      *                        VSAMSTAT-VERBNAME                      *
      *                        VSAMSTAT-STATUS.                       *
      *                                                               *
      *                            - or -                             *
      *                                                               *
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *
      *                        VSAMSTAT-VERBNAME                      *
      *                        VSAMSTAT-STATUS                        *
      *                        VSAMSTAT-KEYAREA.                      *
      *                                                               *
      *                            - or -                             *
      *                                                               *
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *
      *                        VSAMSTAT-VERBNAME                      *
      *                        VSAMSTAT-STATUS                        *
      *                        OMITTED                                *
      *                        VSAMSTAT-FEEDBACK.                     *
      *                                                               *
      *                            - or -                             *
      *                                                               *
      *    CALL VSAMSTAT USING VSAMSTAT-FILENAME                      *
      *                        VSAMSTAT-VERBNAME                      *
      *                        VSAMSTAT-STATUS                        *
      *                        VSAMSTAT-KEYAREA                       *
      *                        VSAMSTAT-FEEDBACK.                     *
      *                                                               *
      *                           - etc. -                            *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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
       OBJECT-COMPUTER. IBM-2086-A04-140.

       SPECIAL-NAMES.
           SYSLST IS PRINTER.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      /*****************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       77                              PIC  X(8)    VALUE '===WS==>'.
       77  MSG-LENG                    PIC S9(4)    BINARY.
       77  MSG-AREA                    PIC  X(100)  VALUE SPACES.
       77  SUB                         PIC  9       VALUE ZEROES.
       77  DEC-OUTP                    PIC  999     VALUE ZEROES.
       77  PERIODS-65                  PIC  X(65)   VALUE ALL '.'.
       77  NON-DISPLAY                 PIC  X(65)   VALUE
                                      X'000102030405060708090A0B0C0D0E0F
      -                                '101112131415161718191A1B1C1D1E1F
      -                                '202122232425262728292A2B2C2D2E2F
      -                                '303132333435363738393A3B3C3D3E3F
      -                                'FF'.

       01  FUNC-DATA.
         03  PIC S9(4) VALUE +15 BINARY.
         03  PIC X(40) VALUE 'in base access.'.
         03  PIC S9(4) VALUE +39 BINARY.
         03  PIC X(40) VALUE 'in base access -- error in upgrade set.'.
         03  PIC S9(4) VALUE +14 BINARY.
         03  PIC X(40) VALUE 'in aix access.'.
         03  PIC S9(4) VALUE +38 BINARY.
         03  PIC X(40) VALUE 'in aix access -- error in upgrade set.'.
         03  PIC S9(4) VALUE +40 BINARY.
         03  PIC X(40) VALUE 'during upgrade -- no err in upgrade set.'.
         03  PIC S9(4) VALUE +39 BINARY.
         03  PIC X(40) VALUE 'during upgrade -- error in upgrade set.'.
       01  FUNC-TABLE        REDEFINES FUNC-DATA.
         03  FUNC-ENTRY                             OCCURS 6.
           05  FUNC-LEN                PIC S9(4)    BINARY.
           05  FUNC-MSG                PIC  X(40).

       01  DOUBLE-WORD                 PIC S9(18)   BINARY.
       01  EIGHT-BYTES       REDEFINES DOUBLE-WORD.
         03  FILLER                    PIC  X(4).
         03  MEMORY-ADDRESS                         POINTER.
         03  FULL-WORD       REDEFINES MEMORY-ADDRESS
                                       PIC S9(9)    BINARY.
         03  FOUR-BYTES      REDEFINES FULL-WORD.
           05  HI-MSB                  PIC  X.
           05  THREE-BYTES.
             07  HI-LSB                PIC  X.
             07  HALF-WORD             PIC S9(4)    BINARY.
             07  TWO-BYTES   REDEFINES HALF-WORD.
               09  LO-MSB              PIC  X.
               09  ONE-BYTE            PIC  X.

       COPY HEXMAN.

       01  JOBDATA                     PIC  X(8)    VALUE 'JOBDATA'.
       COPY JOBDATA.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  VSAMSTAT-FILENAME           PIC  X(8).
       01  VSAMSTAT-VERBNAME           PIC  X(8).
       01  VSAMSTAT-STATUS             PIC  X(2).
       01  VSAMSTAT-KEYAREA.
           05  VSAMSTAT-KEYLENG        PIC S9(4)    BINARY.
           05  VSAMSTAT-KEYDATA        PIC  X(98).
       01  VSAMSTAT-FEEDBACK.
           05  VSAMSTAT-RC             PIC  XX.
           05  VSAMSTAT-FC             PIC S9(4)    BINARY.
           05  VSAMSTAT-EC             PIC  XX.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION
           USING VSAMSTAT-FILENAME
                 VSAMSTAT-VERBNAME
                 VSAMSTAT-STATUS
                 VSAMSTAT-KEYAREA
                 VSAMSTAT-FEEDBACK.

       A00-MAINLINE-ROUTINE.

           CALL JOBDATA             USING JOBDATA-PARMS.

           MOVE 1                      TO MSG-LENG.
           STRING JOBD-EXEC-PGM           DELIMITED BY SPACE
                  ': '                    DELIMITED BY SIZE
                  VSAMSTAT-FILENAME       DELIMITED BY SPACE
                  ' '                     DELIMITED BY SIZE
                  VSAMSTAT-VERBNAME       DELIMITED BY SPACE
                  ' ERROR; VSAMSTAT='
                  VSAMSTAT-STATUS         DELIMITED BY SIZE
               INTO MSG-AREA         WITH POINTER MSG-LENG.
           PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT.

           IF  ADDRESS OF VSAMSTAT-KEYAREA NOT = NULL
             IF  VSAMSTAT-KEYLENG > ZERO
             AND VSAMSTAT-VERBNAME NOT = 'OPEN'
             AND VSAMSTAT-VERBNAME NOT = 'CLOSED'
               MOVE VSAMSTAT-KEYLENG   TO HEX-LENG
               MOVE VSAMSTAT-KEYDATA   TO HEX-CHRT
               MOVE 1                  TO MSG-LENG
               STRING JOBD-EXEC-PGM       DELIMITED BY SPACE
                      ': KEY="'
                      HEX-CHRT '"'        DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               IF  MSG-LENG > LENGTH OF MSG-AREA
                   SUBTRACT 3        FROM LENGTH OF MSG-AREA
                                   GIVING MSG-LENG
                   STRING '..."'          DELIMITED BY SIZE
                       INTO MSG-AREA WITH POINTER MSG-LENG
               END-IF
               PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT
             END-IF
           END-IF.

           IF  ADDRESS OF VSAMSTAT-FEEDBACK NOT = NULL
             IF  VSAMSTAT-FEEDBACK > LOW-VALUES
               MOVE 1                  TO MSG-LENG
               STRING JOBD-EXEC-PGM       DELIMITED BY SPACE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               EVALUATE VSAMSTAT-VERBNAME
               WHEN 'OPEN'
                 STRING ': VSAMOPEN'      DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               WHEN 'CLOSE'
                 STRING ': VSAMCLOS'      DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               WHEN OTHER
                 STRING ': VSAMREQU'      DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               END-EVALUATE
               SET HEX-DECODE-FROM-CHAR TO TRUE
               MOVE 1                  TO HEX-LENG
               MOVE VSAMSTAT-RC        TO TWO-BYTES
               MOVE HALF-WORD          TO DEC-OUTP
               MOVE ONE-BYTE           TO HEX-CHRT
               CALL HEXMAN          USING HEXMAN-PARMS
               STRING ' RC=x''' HEX-HEXT
                      '''(' DEC-OUTP ')'  DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               MOVE VSAMSTAT-EC        TO TWO-BYTES
               MOVE HALF-WORD          TO DEC-OUTP
               MOVE ONE-BYTE           TO HEX-CHRT
               CALL HEXMAN          USING HEXMAN-PARMS
               STRING ', EC=x''' HEX-HEXT
                      '''(' DEC-OUTP ')'  DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               ADD  1                  TO VSAMSTAT-FC
                                   GIVING SUB
               STRING ' '
                      FUNC-MSG(SUB)(1:FUNC-LEN(SUB)) DELIMITED BY SIZE
                   INTO MSG-AREA     WITH POINTER MSG-LENG
               PERFORM B00-DISPLAY-ROUTINE THRU B90-EXIT
             END-IF
           END-IF.

       A90-MAINLINE-EXIT.

           MOVE ZEROES                 TO RETURN-CODE.
           GOBACK.

       B00-DISPLAY-ROUTINE.
           SUBTRACT 1                FROM MSG-LENG.
           IF  MSG-LENG > LENGTH OF MSG-AREA
               MOVE LENGTH OF MSG-AREA TO MSG-LENG
           END-IF.
           DISPLAY MSG-AREA(1:MSG-LENG) UPON PRINTER.
           INSPECT MSG-AREA(1:MSG-LENG)
               CONVERTING NON-DISPLAY  TO PERIODS-65.
           DISPLAY MSG-AREA(1:MSG-LENG) UPON CONSOLE.
       B90-EXIT.
           EXIT.

