 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    VCATPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  APR 2002.
       DATE-COMPILED.
       INSTALLATION.  DAPSCO INFORMATION SYSTEMS.
       SECURITY.      NONE.
      *REMARKS.       CICS ON-LINE VSAM CATALOG VIEWER.

      * CHANGE HISTORY ------------------------------------------------
      * 01/09/2008 DLC IMPLEMENT COMMON CICS INTERFACE COPYBOOKS.
/DLC1/* 03/06/2008 DLC CHANGE TO "QUERY SECURITY" FOR INTERNAL SECURITY.
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

       01  CONTROL-FIELDS.
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='VCAT'==
                                       =='PROGRAM'== BY =='VCATPGM'==.
         03  VCATMS                    PIC  X(08)   VALUE 'VCATMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  LISTXXX                   PIC  X(08)   VALUE 'LISTXXX '.
         03  GENRWIN                   PIC  X(08)   VALUE 'GENRWIN '.
         03  VCATWIN                   PIC  X(08)   VALUE 'VCATWIN '.
         03  VCATERR                   PIC  X(08)   VALUE 'VCATERR '.
         03  ALT2TRN                   PIC  X(04)   VALUE 'VCA2'.
         03  VCA2MS                    PIC  X(08)   VALUE 'VCA2MS  '.
         03  DETLMAP                   PIC  X(08)   VALUE 'DETLMAP '.
         03  GEN2WIN                   PIC  X(08)   VALUE 'GEN2WIN '.
         03  EDITPGM                   PIC  X(08)   VALUE 'DPHEXT  '.

         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  CURSOR-SWITCH             PIC S9(01)   VALUE ZEROES.
           88  CURSOR-IS-SET                        VALUE ZEROES.
           88  CURSOR-NOT-SET                       VALUE -1.
         03  EOD-SWITCH                PIC S9(01)   VALUE ZEROES.
           88  EOD-MARKED                           VALUE ZEROES.
           88  EOD-NOT-MARKED                       VALUE -1.
           88  PROCESSING-DATA-COMPONENT            VALUE +2.
           88  PROCESSING-INDEX-COMPONENT           VALUE -2.
         03  PURGE-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  PURGE-WAS-REQUESTED                  VALUE ZEROES.
           88  PURGE-NOT-REQUESTED                  VALUE -1.
           88  IS-ALLOCATION-DATASET                VALUE +2.
           88  NO-ALLOCATION-DATASET                VALUE -2.
         03  DATA-SWITCH               PIC S9(01)   VALUE ZEROES.
           88  DATA-WAS-LISTED                      VALUE ZEROES.
           88  DATA-NOT-LISTED                      VALUE -1.
         03  INDEX-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  INDEX-WAS-LISTED                     VALUE ZEROES.
           88  INDEX-NOT-LISTED                     VALUE -1.
           88  INDEX-ASSOC-WAS-FOUND                VALUE +2.
           88  INDEX-ASSOC-NOT-FOUND                VALUE -2.
         03  SORT-SWITCH               PIC S9(01)   VALUE ZEROES.
           88  NO-ITEM-SWAPPED                      VALUE ZEROES.
           88  AN-ITEM-SWAPPED                      VALUE -1.
         03  DEVTYPE                   PIC  X(04)   VALUE LOW-VALUE.
         03  SAVE-EXTREC               PIC  X(05)   VALUE LOW-VALUE.
         03  SAVE-CAT-KEY              PIC  X(44)   VALUE SPACES.
         03  DIGIT                     PIC  9(01).
         03  SAVE-YEAR                 PIC  9(02).
         03  X1                        PIC S9(04)   COMP VALUE ZEROES.
         03  X2                        PIC S9(04)   COMP VALUE ZEROES.
         03  X3                        PIC S9(04)   COMP VALUE ZEROES.
         03  P1                        PIC S9(04)   COMP VALUE ZEROES.
         03  P2                        PIC S9(04)   COMP VALUE ZEROES.
         03  P3                        PIC S9(04)   COMP VALUE ZEROES.
         03  LN                        PIC S9(04)   COMP VALUE ZEROES.
         03  L1                        PIC S9(04)   COMP VALUE ZEROES.
         03  L2                        PIC S9(04)   COMP VALUE ZEROES.
         03  L3                        PIC S9(04)   COMP VALUE ZEROES.
         03  SQ                        PIC S9(04)   COMP VALUE ZEROES.
         03  ROW                       PIC S9(04)   COMP VALUE ZEROES.
         03  COL                       PIC S9(04)   COMP VALUE ZEROES.
         03  XX-ITEM                   PIC S9(04)   COMP VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   COMP VALUE +6.
         03  LISTROWS                  PIC S9(04)   COMP VALUE +15.
         03  VWINHEAD                  PIC S9(04)   COMP VALUE +5.
         03  VWINROWS                  PIC S9(04)   COMP VALUE +6.
         03  DETLHEAD                  PIC S9(04)   COMP VALUE +4.
         03  DETLROWS                  PIC S9(04)   COMP VALUE +21.
         03  TABLE-ENTRIES             PIC S9(04)   COMP VALUE +128.
         03  ZONE-OFFSET               PIC S9(04)   COMP VALUE ZEROES.
         03  DATASETS-ON-VOL           PIC S9(04)   COMP VALUE ZEROES.
         03  DATASPACES-ON-VOL         PIC S9(04)   COMP VALUE ZEROES.
         03  EXTENT-COUNT              PIC S9(04)   COMP VALUE ZEROES.
         03  ADJUSTMENT-DAYS           PIC S9(09)   COMP VALUE ZEROES.
         03  OPEN-STATUS               PIC S9(09)   COMP VALUE ZEROES.
         03  ENABLE-STATUS             PIC S9(09)   COMP VALUE ZEROES.
         03  LINE-COUNT                PIC S9(09)   COMP VALUE ZEROES.
         03  MAX-ALLOC                 PIC S9(09)   COMP VALUE ZEROES.
         03  HIGH-ALLOC-RBA            PIC S9(15)V999 COMP-3 VALUE +0.
         03  EDITTED-DATE              PIC  99/99/9999.
         03  EDITTED-TIME              PIC  99B99B99.
         03  CHARACTER-TIME  REDEFINES EDITTED-TIME PIC X(8).

      *  TEMP-STORAGE RECORD LAYOUTS
       01  TS-RECD.
           05  TS-ENTNAME              PIC  X(44).
           05  TS-CINO                 PIC  X(3).
           05  TS-RECTYPE              PIC  X(1).
           05  TS-EXTENTS              PIC S9(4)    COMP.
           05  TS-SELECT               PIC  X(1).
           05  TS-ENTTYPE              PIC  X(4).
           05  TS-LASTUPD              PIC  9(8).
           05  TS-LASTUPT              PIC  9(4).
           05  TS-RECOUNT              PIC S9(15)   COMP-3.
       01  T2-DETL.
           05  T2-DETLINE              PIC  X(100).
           05  T2-LNCOUNT              PIC S9(9)    COMP.
       01  S2-DETL.
           05  S2-DETLINE              PIC  X(100).
           05  S2-LNCOUNT              PIC S9(9)    COMP.

      *  WORK AREA FOR BYTE-ENCODED NUMBERS
       01  DOUBLE-WORD                 PIC S9(18)   COMP.
       01  EIGHT-BYTES     REDEFINES   DOUBLE-WORD.
         03  FILLER                    PIC  X(4).
         03  FULL-WORD                 PIC S9(9)    COMP.
         03  FOUR-BYTES    REDEFINES   FULL-WORD.
           05  HI-MSB                  PIC  X.
           05  THREE-BYTES.
             07  HI-LSB                PIC  X.
             07  HALF-WORD             PIC S9(4)    COMP.
             07  TWO-BYTES REDEFINES   HALF-WORD.
               09  LO-MSB              PIC  X.
               09  ONE-BYTE            PIC  X.

      *  ATTRIBUTES TABLE
       01  ATTRS-TABLE.
           05  ATTRS-ENTRY             PIC  X(12)   OCCURS 16.

      *  GROUP OCCURRENCE POINTER TABLE
       01  GOP-TABLE.
         03  GO-POINTER                OCCURS 128.
           05  GOP-CINO.
             07  FILLER                PIC  X.
             07  GOP-DISP              PIC  9(4)    COMP.
           05  GOP-TYPE                PIC  X.
             88  AMDSB-GOP-INLINE                   VALUE X'01'.
             88  ASSOC-GOP-INLINE                   VALUE X'02'.
             88  VOLUM-GOP-INLINE                   VALUE X'03'.
             88  PSWRD-GOP-INLINE                   VALUE X'04'.
             88  SPMAP-GOP-INLINE                   VALUE X'05'.
             88  SPACE-GOP-INLINE                   VALUE X'06'.
             88  DSDIR-GOP-INLINE                   VALUE X'08'.
             88  AMDSB-GOP-EXTREC                   VALUE X'81'.
             88  ASSOC-GOP-EXTREC                   VALUE X'82'.
             88  VOLUM-GOP-EXTREC                   VALUE X'83'.
             88  PSWRD-GOP-EXTREC                   VALUE X'84'.
             88  SPMAP-GOP-EXTREC                   VALUE X'85'.
             88  SPACE-GOP-EXTREC                   VALUE X'86'.
             88  DSDIR-GOP-EXTREC                   VALUE X'88'.
           05  GOP-W-SEQU              PIC  9(4)    COMP.
           05  FILLER      REDEFINES   GOP-W-SEQU.
             07  GOP-SEQU              PIC  X.
             07  FILLER                PIC  X.

      *  GROUP OCCURRENCE TABLE
       01  GO-TABLE.
         03  G-OCCURRENCE              OCCURS 128.
           05  GO-LENG                 PIC  9(4)    COMP.
           05  GO-TYPE                 PIC  X.
           05  GO-DATA                 PIC  X(7).
           05  ASSOC-GO    REDEFINES   GO-DATA.
             07  ASSOC-CINO            PIC  X(3).
             07  ASSOC-TYPE2           PIC  X.
             07  ASSOC-CINO2           PIC  X(3).
           05  DSDIR-GO    REDEFINES   GO-DATA.
             07  DSDIR-VCNT            PIC  X.
             07  DSDIR-VOFF            PIC  X.
             07  DSDIR-DSIDNO          PIC  X(3).

      *  DATASET STATISTICS GROUP OCCURENCE
       01  AMDSB-GO.
           05  AMDSB-VCNT              PIC  X.
           05  AMDSB-VOFF              PIC  X.
           05  AMDSB-ID                PIC  X.
           05  AMDSB-ATTR1             PIC  X.
           05  AMDSB-LEN               PIC S9(4)    COMP.
           05  AMDSB-AXRKP             PIC S9(4)    COMP.
           05  AMDSB-NEST   REDEFINES  AMDSB-AXRKP
                                       PIC S9(4)    COMP.
           05  AMDSB-RKP               PIC S9(4)    COMP.
           05  AMDSB-KEYLN             PIC S9(4)    COMP.
           05  AMDSB-PCTCA             PIC  X.
           05  AMDSB-RCFRM  REDEFINES  AMDSB-PCTCA
                                       PIC  X.
           05  AMDSB-PCTCI             PIC  X.
           05  AMDSB-CIPCA             PIC S9(4)    COMP.
           05  AMDSB-FSCA              PIC S9(4)    COMP.
           05  AMDSB-FSCI              PIC S9(9)    COMP.
           05  AMDSB-CINV              PIC S9(9)    COMP.
           05  AMDSB-LRECL             PIC S9(9)    COMP.
           05  AMDSB-NSLOT             PIC S9(9)    COMP.
           05  AMDSB-HLRBA  REDEFINES  AMDSB-NSLOT
                                       PIC  9(9)    COMP.
           05  AMDSB-BLREC  REDEFINES  AMDSB-NSLOT
                                       PIC S9(9)    COMP.
           05  AMDSB-MAXRR             PIC S9(9)    COMP.
           05  AMDSB-SSRBA  REDEFINES  AMDSB-MAXRR
                                       PIC  9(9)    COMP.
           05  AMDSB-PARDB             PIC S9(9)    COMP.
           05  AMDSB-ATTR3             PIC  X.
           05  FILLER                  PIC  X.
           05  AMDSB-CPHDL             PIC  X.
           05  AMDSB-OLIL              PIC  X.
           05  AMDSB-OLHRB             PIC  X(4).
           05  AMDSB-STMST             PIC  X(8).
           05  AMDSB-NIL               PIC S9(4)    COMP.
           05  AMDSB-NEXT              PIC S9(4)    COMP.
           05  AMDSB-NEDB   REDEFINES  AMDSB-NEXT
                                       PIC S9(4)    COMP.
           05  AMDSB-NLR               PIC S9(9)    COMP.
           05  AMDSB-DELR              PIC S9(9)    COMP.
           05  AMDSB-IREC              PIC S9(9)    COMP.
           05  AMDSB-UPR               PIC S9(9)    COMP.
           05  AMDSB-RETR              PIC S9(9)    COMP.
           05  AMDSB-ASPA              PIC  X(4).
           05  AMDSB-NCIS              PIC S9(9)    COMP.
           05  AMDSB-NCAS              PIC S9(9)    COMP.
           05  AMDSB-EXCP              PIC S9(9)    COMP.

      *  DATASET VOLUME INFORMATION GROUP OCCURENCE
       01  VOLUM-GO.
           05  VOLUM-VCNT              PIC  X.
           05  VOLUM-VOFF              PIC  X.
           05  VOLUM-DEVTYP            PIC  X(4).
           05  VOLUM-VOLSER            PIC  X(6).
           05  VOLUM-FILESEQ           PIC S9(4)    COMP.
           05  VOLUM-VOLFLG            PIC  X.
           05  VOLUM-NOEXTNT           PIC  X.
           05  VOLUM-HKRBA             PIC  X(4).
           05  VOLUM-HURBA             PIC  X(4).
           05  VOLUM-HARBA             PIC  X(4).
           05  VOLUM-PHYBLKSZ          PIC S9(9)    COMP.
           05  VOLUM-NOBLKTRK          PIC S9(4)    COMP.
           05  VOLUM-NOTRKAU           PIC S9(4)    COMP.
           05  VOLUM-ITYPEXT           PIC  X.
           05  VOLUM-DSDIRSN           PIC S9(4)    COMP.
           05  VOLUM-VARDTA            PIC  X(406).

      *  DATASET PASSWORD GROUP OCCURENCE
       01  PSWRD-GO.
           05  PSWRD-VCNT              PIC  X.
           05  PSWRD-VOFF              PIC  X.
           05  PSWRD-MASTER            PIC  X(8).
           05  PSWRD-CTLINT            PIC  X(8).
           05  PSWRD-UPDATE            PIC  X(8).
           05  PSWRD-READ              PIC  X(8).
           05  PSWRD-PASSPRMT          PIC  X(8).
           05  PSWRD-PASSATMP          PIC S9(4)    COMP.
           05  PSWRD-USVRMDUL          PIC  X(8).
           05  PSWRD-USERALEN          PIC S9(4)    COMP.
           05  PSWRD-USERAREC          PIC  X(256).

      *  VOLUME SPACE MAP GROUP OCCURENCE
       01  SPMAP-GO.
           05  SPMAP-VCNT              PIC  X.
           05  SPMAP-VOFF              PIC  X.
           05  SPMAP-LEN               PIC S9(4)    COMP.
           05  SPMAP-VARDTA            PIC  X(440).

      *  DATA SPACE GROUP OCCURENCE
       01  SPACE-GO.
           05  SPACE-VCNT              PIC  X.
           05  SPACE-VOFF              PIC  X.
           05  SPACE-DSCBTS            PIC  X(8).
           05  SPACE-DSCBPTR           PIC  X(5).
           05  SPACE-SPHDFLG           PIC  X.
           05  SPACE-NODSPEXT          PIC  X.
           05  SPACE-DSPSOPT           PIC  X.
           05  SPACE-DSPSSQ            PIC  X(3).
           05  SPACE-SPEXTENT          OCCURS 16.
             07  SPACE-SPEXTT          PIC  X(2).
             07  SPACE-SPEXNN          PIC  X(2).

       01  WS-ABSTIME                  PIC S9(15)   COMP-3.
       01  WS-WRKTIME.
           05  WS-HRSTIME              PIC  9(02).
           05  FILLER                  PIC  X(03).
           05  WS-XXXTIME              PIC  X(03).

       01  NBR-DATE                    PIC  9(08).
       01  WRK-DATE        REDEFINES   NBR-DATE.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYR                PIC  9999.

       01  NBR-TIME                    PIC  9(06).
       01  WRK-TIME        REDEFINES   NBR-TIME.
         03  NBR-SHORT                 PIC  9(04).
         03  SHORT-TIME    REDEFINES   NBR-SHORT.
           05  WRK-HOUR                PIC  99.
           05  WRK-MINS                PIC  99.
         03  WRK-SECS                  PIC  99.

       01  EDITTED-NUMBER              PIC BZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-.
       01  WRK-AREA        REDEFINES   EDITTED-NUMBER.
           05  WRK-FIELD               PIC  X(25).

      * THE FOLLOWING AREAS ARE FOR THE SUBMISSION ROUTINE
       01  JOB-GROUP.
           05  SUBMIT-TOKEN            PIC  X(08).
           05  JOB-NAME                PIC  X(08)   VALUE 'VCATDEL '.
           05  JOB-LENGTH              PIC S9(09)   COMP VALUE +800.
           05  JCL-SUB                 PIC S9(04)   COMP VALUE ZEROES.
           05  JOB-TOTAL               PIC S9(04)   COMP VALUE +10.
           05  JOB-COUNT               PIC  9(04)   VALUE ZEROES.
       01  VAR-GROUP.
           05  SAVE-FILE               PIC  X(8)    VALUE SPACES.
           05  SAVE-CODE               PIC  X(2)    VALUE SPACES.
           05  SAVE-FILE2              PIC  X(8)    VALUE SPACES.
           05  SAVE-CODE2              PIC  X(2)    VALUE SPACES.
           05  VAR-NAME                PIC  X(5)    VALUE SPACES.
           05  VAR-TEMP                PIC  X(8)    VALUE SPACES.
           05  VAR-NUMB                PIC  Z(4).
           05  VAR-LENG                PIC S9(4)    COMP VALUE +44.
           05  VAR-DATA.
               10  VAR-BYTE            PIC  X       OCCURS 1 TO 44
                                       DEPENDING ON VAR-LENG.

      * JCL TEMPLATE TABLE
       01  JCL-DATA.
         03  FILLER  PIC X(60)  VALUE
           '* $$ JOB JNM=VCATDEL,CLASS=C,DISP=D,USER=<USR>          '.
         03  FILLER  PIC X(60)  VALUE
           '* $$ LST LST=SYSLST,CLASS=Z,DISP=D                      '.
         03  FILLER  PIC X(60)  VALUE
           '// JOB    VCATDEL   PURGE VSAM DATASET (ONLINE SUBMISSION)'.
         03  FILLER  PIC X(60)  VALUE '// EXEC   PGM=IDCAMS,SIZE=AUTO '.
         03  FILLER  PIC X(60)  VALUE '   DELETE (<DSN>) -            '.
         03  FILLER  PIC X(60)  VALUE '          <OPT>  PURGE  -      '.
         03  FILLER  PIC X(60)  VALUE '          CATALOG(<CAT>)       '.
         03  FILLER  PIC X(60)  VALUE '/* EOD                         '.
         03  FILLER  PIC X(60)  VALUE '/& EOJ                         '.
         03  FILLER  PIC X(60)  VALUE '* $$ EOJ                       '.
       01  JCL-TABLE       REDEFINES   JCL-DATA.
         03  JCL-TEMPLATE              PIC  X(60)   OCCURS 10.

      * JOB CONTROL TABLE
       01  JOB-TABLE.
         03  JCL-CARD                  PIC  X(80)   OCCURS 10.

/DLC1/* APPLICATION SECURITY CONTROL AREAS
/DLC1/ 01  VSECSERV-CONTROL.
/DLC1/   03  VSECSERV                  PIC  X(8)    VALUE 'VSECSERV'.
/DLC1/   03  VSECSERV-RESOURCE         PIC  X(39)   VALUE
/DLC1/                        'WINAPPS.SY.VCAT.PURGE'.
/DLC1/ 01  VSECSERV-COMMAREA.
/DLC1/   COPY VSECSERV.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       COPY BITMAN.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY NUMMAN.

       COPY SBA3270.

       COPY TXTMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG  REDEFINES  UNEX-MSG     PIC  X(79).

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * VCAT CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  VCAT-SUBRRTN            PIC  X(4).
           05  VCAT-PROMPT             PIC  X(8).
           05  VCAT-CATALOG            PIC  X(8).
           05  VCAT-ENTNAME            PIC  X(44).
           05  VCAT-CATNAME            PIC  X(44).
/DLC1/     05  VCAT-PURGE              PIC  X(1).
/DLC1/       88  VCAT-PURGEABLE                     VALUE 'Y'.
/DLC1/       88  VCAT-NOTPURGEABLE                  VALUE 'N'.
           05  TS-QUEUE.
               10  TS-TRAN             PIC  X(04).
               10  TS-TERM             PIC  X(04).
           05  TS-ITEM                 PIC S9(04)   COMP.
           05  TS-TOTL                 PIC S9(04)   COMP.
           05  T2-QUEUE.
               10  T2-TRAN             PIC  X(04).
               10  T2-TERM             PIC  X(04).
           05  T2-ITEM                 PIC S9(04)   COMP.
           05  T2-TOTL                 PIC S9(04)   COMP.
           05  VCAT-SAVEIOA            PIC  X(1881).

      * VCAT TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * This is a triple overlay structure for addressing low storage.
      * COMREG pointer is at offset x'14' (20) from address x'00000000'
      * TODCOM pointer is at offset x'78' (120) in the partition COMREG
      * The Time-of-Day Clock COMmunication block (TODCOM) is 20 bytes.
       01  LOW-MEMORY.
           05  TODCOM.
               10  TODCOM-CURDATE      PIC  9(6).
               10  FILLER              PIC  X(4).
               10  TODCOM-ZONEOFS      PIC S9(4) COMP.
               10  TODCOM-CENTURY      PIC  9(2).
               10  TODCOM-ZONEID       PIC  X(3).
               10  FILLER              PIC  X(3).
           05  COMREG-POINTER          POINTER.
           05  FILLER                  PIC  X(96).
           05  TODCOM-POINTER          POINTER.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * CATALOG RECORD I/O AREA
       01  CATALOG-RECORD.
         03  CATALOG-KEY               PIC X(44).
         03  CAT-LO-KEY    REDEFINES   CATALOG-KEY.
           05  CAT-LO-BYT1             PIC X.
           05  CAT-LO-CINO             PIC X(3).
           05  CAT-LO-VERS             PIC X.
           05  FILLER                  PIC X(39).
         03  CAT-HI-KEY    REDEFINES   CATALOG-KEY.
           05  CAT-HI-PFX              PIC X(8).
           05  CAT-HI-TAG              PIC X(13).
           05  CAT-HI-FIL              PIC X(15).
           05  CAT-HI-SFX              PIC X(8).
         03  CATALOG-DATA.
           05  CAT-HI-CINO             PIC X(3).
           05  FILLER                  PIC X(458).
         03  CAT-C-REC     REDEFINES   CATALOG-DATA.
           05  CAT-C-TYPE              PIC X.
           05  CAT-C-LEN               PIC S9(4) COMP.
           05  CAT-C-DLEN              PIC S9(4) COMP.
           05  CAT-C-ENTNAME           PIC X(44).
           05  CAT-C-OWNERID           PIC X(8).
           05  CAT-C-DSETCRDT          PIC S9(5) COMP-3.
           05  CAT-C-DSETEXDT          PIC S9(5) COMP-3.
           05  CAT-C-CATTR             PIC X.
           05  CAT-C-EXTREC            PIC X(5).
           05  CAT-C-GOPCNT            PIC X.
           05  CAT-C-VARDTA            PIC X(391).
         03  CAT-D-REC     REDEFINES   CATALOG-DATA.
           05  CAT-D-TYPE              PIC X.
           05  CAT-D-LEN               PIC S9(4) COMP.
           05  CAT-D-DLEN              PIC S9(4) COMP.
           05  CAT-D-ENTNAME           PIC X(44).
           05  CAT-D-OWNERID           PIC X(8).
           05  CAT-D-DSETCRDT          PIC S9(5) COMP-3.
           05  CAT-D-DSETEXDT          PIC S9(5) COMP-3.
           05  CAT-D-ATTR1             PIC X.
           05  CAT-D-ATTR2             PIC X.
           05  CAT-D-OPENIND           PIC X.
           05  CAT-D-BUFSIZE           PIC S9(9) COMP.
           05  CAT-D-PRIMSPAC          PIC X(3).
           05  CAT-D-SCONSPAC          PIC X(3).
           05  CAT-D-SPACOPTN          PIC X.
           05  CAT-D-HURBADS           PIC X(4).
           05  CAT-D-HARBADS           PIC X(4).
           05  CAT-D-LRECL             PIC S9(9) COMP.
           05  CAT-D-USERINFO          PIC XX.
           05  CAT-D-EXCPEXIT          PIC X(8).
           05  CAT-D-EXTREC            PIC X(5).
           05  CAT-D-GOPCNT            PIC X.
           05  CAT-D-VARDTA            PIC X(356).
         03  CAT-E-REC     REDEFINES   CATALOG-DATA.
           05  CAT-E-TYPE              PIC X.
           05  CAT-E-LEN               PIC S9(4) COMP.
           05  CAT-E-DLEN              PIC S9(4) COMP.
           05  CAT-E-EXTREC            PIC X(5).
           05  CAT-E-GOPCNT            PIC X.
           05  CAT-E-VARDTA            PIC X(450).
         03  CAT-G-REC     REDEFINES   CATALOG-DATA.
           05  CAT-G-TYPE              PIC X.
           05  CAT-G-LEN               PIC S9(4) COMP.
           05  CAT-G-DLEN              PIC S9(4) COMP.
           05  CAT-G-ENTNAME           PIC X(44).
           05  CAT-G-OWNERID           PIC X(8).
           05  CAT-G-DSETCRDT          PIC S9(5) COMP-3.
           05  CAT-G-DSETEXDT          PIC S9(5) COMP-3.
           05  CAT-G-RGATTR            PIC X.
           05  CAT-G-EXTREC            PIC X(5).
           05  CAT-G-GOPCNT            PIC X.
           05  CAT-G-VARDTA            PIC X(391).
         03  CAT-R-REC     REDEFINES   CATALOG-DATA.
           05  CAT-R-TYPE              PIC X.
           05  CAT-R-LEN               PIC S9(4) COMP.
           05  CAT-R-DLEN              PIC S9(4) COMP.
           05  CAT-R-ENTNAME           PIC X(44).
           05  CAT-R-OWNERID           PIC X(8).
           05  CAT-R-DSETCRDT          PIC S9(5) COMP-3.
           05  CAT-R-DSETEXDT          PIC S9(5) COMP-3.
           05  CAT-R-RGATTR            PIC X.
           05  CAT-R-EXTREC            PIC X(5).
           05  CAT-R-GOPCNT            PIC X.
           05  CAT-R-VARDTA            PIC X(391).
         03  CAT-U-REC     REDEFINES   CATALOG-DATA.
           05  CAT-U-TYPE              PIC X.
           05  CAT-U-LEN               PIC S9(4) COMP.
           05  CAT-U-DLEN              PIC S9(4) COMP.
           05  CAT-U-ENTNAME           PIC X(44).
           05  CAT-U-EXTREC            PIC X(5).
           05  CAT-U-GOPCNT            PIC X.
           05  CAT-U-VARDTA            PIC X(406).
         03  CAT-V-REC     REDEFINES   CATALOG-DATA.
           05  CAT-V-TYPE              PIC X.
           05  CAT-V-LEN               PIC S9(4) COMP.
           05  CAT-V-DLEN              PIC S9(4) COMP.
           05  CAT-V-ENTNAME           PIC X(44).
           05  CAT-V-VOLTSTMP          PIC X(8).
           05  CAT-V-VOLDVCHR.
             07  CAT-V-DEVTYPE         PIC X(4).
             07  CAT-V-MAXBLKSZ        PIC S9(9) COMP.
             07  CAT-V-MAXCYLS         PIC S9(4) COMP.
             07  CAT-V-TRKPCYL         PIC S9(4) COMP.
             07  CAT-V-BYTPTRK         PIC X(2).
             07  CAT-V-GAPBYTS         PIC X.
             07  CAT-V-GAPBLST         PIC X.
             07  CAT-V-UNKBYTS         PIC X.
             07  CAT-V-DEVFLAG         PIC X.
             07  CAT-V-TOLFACT.
               09  CAT-V-SEGBYTS       PIC X.
               09  CAT-V-DEVRESV       PIC X.
           05  CAT-V-VOLRFLG           PIC X.
           05  CAT-V-SYSEXTDS          PIC X.
           05  CAT-V-DERVNOS           PIC S9(4) COMP.
           05  CAT-V-DERVNOD           PIC S9(4) COMP.
           05  CAT-V-EXTREC            PIC X(5).
           05  CAT-V-GOPCNT            PIC X.
           05  CAT-V-VARDTA            PIC X(372).

      * DAPSYSF RECORD I/O AREA
       01  DAPSYSF-REC.
         03  DAPSYSF-KEY.
           05  SYSF-PRIME-KEY          PIC  X(12).
             88  SYSF-VSAM-CATALOG             VALUE 'VSMCATALOG=>'.
           05  SYSF-CATFILE            PIC  X(08).
         03  DAPSYSF-DATA.
           05  SYSF-CATNAME            PIC  X(44).
           05  FILLER                  PIC  X(186).

      * BMS TERMINAL I/O BUFFERS
       COPY VCATMS.
       COPY VCA2MS.

      * SAVE AREAS
       01  REC-SAVE                    PIC X(505).
       01  REC-SAVE2                   PIC X(505).
       01  GOP-SAVE                    PIC X(768).
       01  GOP-SAVE2                   PIC X(768).
       01  GO-SAVE                     PIC X(1280).
       01  GO-SAVE2                    PIC X(1280).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-VCAT-MAINLINE.

           IF  EIBTRNID = ALT2TRN
               GO TO A00-VCAT-CONTINUE
           END-IF.

           COPY COMMENTR.

       A00-VCAT-CONTINUE.

           COPY COMMMAPT.

      /*****************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       B00-VCAT-INITIALIZATION.

           SET DTE-REQUEST-GREG-EDIT TO TRUE.
           MOVE   19000101   TO DTE-GNUM.
           CALL   'DTEMAN'   USING DTEMAN-PARMS.
           MOVE   DTE-TOTD   TO ADJUSTMENT-DAYS.

      * First, point to low memory at x'00000000' and,
           SET ADDRESS OF LOW-MEMORY TO NULL.
      * from there, offset x'14'(020) points to COMREG
           SET ADDRESS OF LOW-MEMORY TO COMREG-POINTER.
      *  and COMREG offset x'78'(120) points to TODCOM.
           SET ADDRESS OF LOW-MEMORY TO TODCOM-POINTER.
      * Now we are addressing the 20-byte TODCOM block.
           COMPUTE ZONE-OFFSET = TODCOM-ZONEOFS / 60.

           IF  COMM-CURRPGM = THIS-PGM
               EXEC CICS LOAD
                         PROGRAM('IBM3270')
                         SET(ADDRESS OF IBM3270)
               END-EXEC
               IF  NOT IBM3270-LOADED-OK
                   EXEC CICS ABEND
                             ABCODE('3270')
                   END-EXEC
               ELSE
               IF  COMM-TRANSID = ALT2TRN
                   GO TO E00-DETL-ROUTINE
               ELSE
               IF  COMM-CURRRTN = 'VCAT'
                   GO TO D00-VCAT-PROMPT
               ELSE
               GO TO C00-LIST-ROUTINE
           END-IF.

           COPY COMMINIT.

/DLC1/     MOVE SPACES                 TO VSECSERV-HEADER.
/DLC1/     SET  VSECSERV-TRAN          TO TRUE.
/DLC1/     SET  VSECSERV-CHK-SECURITY  TO TRUE.
/DLC1/     MOVE VSECSERV-RESOURCE      TO VSECSERV-CGI-REQU.
/DLC1/     EXEC CICS LINK
/DLC1/               PROGRAM(VSECSERV)
/DLC1/               COMMAREA(VSECSERV-HEADER)
/DLC1/               NOHANDLE
/DLC1/     END-EXEC.
/DLC1/     IF  EIBRESP    = DFHRESP(NORMAL)
/DLC1/     AND VSECSERV-SEC-ALTERABLE
/DLC1/         SET  VCAT-PURGEABLE     TO TRUE
/DLC1/     ELSE
/DLC1/         SET  VCAT-NOTPURGEABLE  TO TRUE
/DLC1/     END-IF.

           GO TO B00-VCAT-INITIALIZATION.

      /*****************************************************************
      *    ROUTINE TO MANAGE THE ENTRY "LIST" SCREEN BY CATALOG        *
      ******************************************************************
       C00-LIST-ROUTINE.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF DAPSYSF-REC)
                     LENGTH(LENGTH OF DAPSYSF-REC)
           END-EXEC.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF LISTMAPI)
                     LENGTH(LENGTH OF LISTMAPI)
                     INITIMG(LOVALUE)
           END-EXEC.

           IF  COMM-CURRRTN NOT = 'LIST'
               SET NO-ERRORS-FOUND TO TRUE
               IF  COMM-CURRRTN = 'VCAT'
                   MOVE VCAT-SAVEIOA TO LISTMAPO
                   EXEC CICS SEND
                             MAP(LISTMAP)
                             FROM(LISTMAPI)
                             MAPSET(VCATMS)
                             TERMINAL
                             ERASE
                             NOHANDLE
                   END-EXEC
                   IF  VCAT-PROMPT > SPACES
                       MOVE VCAT-PROMPT TO LIST-CATNAME-O
                   END-IF
                   MOVE SPACES TO VCAT-PROMPT
                   GO TO C60-LIST-DISPLAY
               ELSE
               IF  COMM-CURRRTN = 'EDIT'
                   MOVE VCAT-SAVEIOA TO LISTMAPO
                   EXEC CICS SEND
                             MAP(LISTMAP)
                             FROM(LISTMAPI)
                             MAPSET(VCATMS)
                             TERMINAL
                             ERASE
                             NOHANDLE
                   END-EXEC
                   MOVE SPACES TO VCAT-PROMPT
                   GO TO C60-LIST-DISPLAY
               ELSE
               IF  COMM-CURRRTN = 'DETL'
                   MOVE VCAT-SAVEIOA TO LISTMAPO
                   EXEC CICS SEND
                             MAP(LISTMAP)
                             FROM(LISTMAPI)
                             MAPSET(VCATMS)
                             TERMINAL
                             ERASE
                             NOHANDLE
                   END-EXEC
                   MOVE SPACES TO VCAT-PROMPT
                   GO TO C60-LIST-DISPLAY
               ELSE
               MOVE  EIBTRNID  TO TS-TRAN
               MOVE  EIBTRMID  TO TS-TERM
               MOVE   ZEROES   TO TS-ITEM
                                  TS-TOTL
               EXEC CICS SEND
                         MAP(LISTMAP)
                         MAPONLY
                         MAPSET(VCATMS)
                         TERMINAL
                         ERASE
                         NOHANDLE
               END-EXEC
               GO TO C60-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP(LISTMAP)
                     INTO(LISTMAPI)
                     MAPSET(VCATMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP = DFHRESP(MAPFAIL)
               NEXT SENTENCE
           ELSE
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -1    TO ERROR-SWITCH
                                LIST-CATNAME-L
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO C60-LIST-DISPLAY
           END-IF.

       C20-LIST-KEYS.

           IF  EIBAID = AIDCURSR OR AIDPAK01 OR AIDPAK02 OR AIDPAK03
               GO TO C60-LIST-DISPLAY
           ELSE
           IF  EIBAID = AIDENTER OR AIDPFK04 OR AIDPFK05
                     OR AIDPFK06 OR AIDPFK07 OR AIDPFK08 OR AIDPFK09
               GO TO C30-LIST-PROCESS
           ELSE
           IF  EIBAID = AIDCLEAR OR AIDPFK03
               EXEC CICS DELETEQ TS
                         QUEUE(TS-QUEUE)
                         NOHANDLE
               END-EXEC
               GO TO Z00-VCAT-TERMINATION
           END-IF.

           MOVE      -1      TO ERROR-SWITCH
                                LIST-CATNAME-L.
           MOVE   EXHREVRS   TO LIST-MESSAGE-H.
           MOVE 'That Program Function key is not shown, and serves no p
      -         'urpose, on this screen.' TO LIST-MESSAGE-O.

           GO TO C60-LIST-DISPLAY.

       C30-LIST-PROCESS.

           IF  EIBAID = AIDPFK04
               DIVIDE EIBCPOSN BY 80 GIVING ROW REMAINDER COL
               ADD     1     TO ROW COL
               IF  ROW NOT = 4
               OR  NOT (COL >= 11 AND <= 54)
                   MOVE 'To prompt, the cursor must be in the Catalog Na
      -              'me field.                      ' TO LIST-MESSAGE-O
               ELSE
               MOVE LISTMAPI TO VCAT-SAVEIOA
               MOVE VCAT-CATALOG TO VCAT-PROMPT
               GO TO D00-VCAT-PROMPT
           END-IF.

           IF  EIBAID = AIDPFK05
               MOVE SPACES TO LIST-OLDNAME-O
           END-IF.

           IF  LIST-CATNAME-I NOT = LIST-OLDNAME-I
               GO TO C60-LIST-DISPLAY
           END-IF.

           PERFORM WITH TEST BEFORE
             VARYING ROW FROM 1 BY 1
               UNTIL ROW > LISTROWS
                  OR LIST-SELECT-I(ROW) = 'B' OR 'D' OR 'M' OR 'E'
           END-PERFORM.
           IF  ROW <= LISTROWS
               COMPUTE XX-ITEM = TS-ITEM + (ROW - 1)
               EXEC CICS READQ TS
                         QUEUE(TS-QUEUE)
                         ITEM(XX-ITEM)
                         INTO(TS-RECD)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE   -1   TO ERROR-SWITCH
                                  LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   PERFORM X00-UNEX-ERR
                   MOVE UNEX-MSG TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
               END-IF
               EVALUATE LIST-SELECT-I(ROW)
               WHEN 'M'
                 IF TS-ENTTYPE = 'VSPC'
                   MOVE 'SMAP' TO VCAT-SUBRRTN
                 ELSE
                   MOVE    -1    TO ERROR-SWITCH
                                    LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   MOVE 'The Space Map option is only supported for VSAM
      -                ' Spaces.  Please try again.  ' TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
                 END-IF
               WHEN 'E'
                 IF TS-ENTTYPE = 'KSDS' OR 'XLDS' OR 'ESDS' OR 'RRDS'
                              OR 'AIX ' OR 'PATH' OR 'SAME' OR 'SAMI'
                   MOVE 'EDIT' TO VCAT-SUBRRTN
                 ELSE
                   MOVE    -1    TO ERROR-SWITCH
                                    LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   MOVE 'The Edit Dataset option is not supported for th
      -                'is entry type.  Try again.   ' TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
                 END-IF
               WHEN 'D'
                 IF TS-ENTTYPE NOT = 'PATH' AND 'UCAT'
                   MOVE 'DETL' TO VCAT-SUBRRTN
                 ELSE
                   MOVE    -1    TO ERROR-SWITCH
                                    LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   MOVE 'The Details option does not apply to Path or Us
      -              'er Catalogs.  Please try again.' TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
                 END-IF
               WHEN OTHER
                   MOVE 'BASI' TO VCAT-SUBRRTN
               END-EVALUATE
               MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
               MOVE  TS-ITEM   TO T2-ITEM
               MOVE  XX-ITEM   TO TS-ITEM
               MOVE TS-ENTNAME TO VCAT-ENTNAME
               MOVE  LISTMAPI  TO VCAT-SAVEIOA
               IF  VCAT-SUBRRTN = 'EDIT'
                   MOVE VCAT-SUBRRTN TO COMM-CURRRTN
                   EXEC CICS XCTL
                             PROGRAM(EDITPGM)
                             COMMAREA(DFHCOMMAREA)
                             LENGTH(EIBCALEN)
                             NOHANDLE
                   END-EXEC
                   MOVE T2-ITEM TO TS-ITEM
                   MOVE 'LIST' TO COMM-CURRRTN
                   MOVE   -1   TO ERROR-SWITCH
                                  LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   MOVE 'The Edit routine is not currently available.  P
      -                 'lease try again.         ' TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
               END-IF
               MOVE  ALT2TRN   TO COMM-TRANSID
               EXEC CICS START
                         TRANSID(ALT2TRN)
                         FROM(DFHCOMMAREA)
                         LENGTH(EIBCALEN)
                         TERMID(EIBTRMID)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE   -1   TO ERROR-SWITCH
                                  LIST-SELECT-L(ROW)
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   PERFORM X00-UNEX-ERR
                   MOVE UNEX-MSG TO LIST-MESSAGE-O
                   GO TO C80-LIST-OUTPUT
               END-IF
               GO TO C90-LIST-RETURN
           END-IF.

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   MOVE   1  TO TS-ITEM
               ELSE
                   COMPUTE TS-ITEM = TS-TOTL - (LISTROWS - 1)
               END-IF
               GO TO C60-LIST-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK07 OR AIDPFK08
               DIVIDE EIBCPOSN BY 80 GIVING ROW REMAINDER COL
               ADD     1     TO ROW COL
               SUBTRACT LISTHEAD FROM ROW
               IF  EIBAID = AIDPFK07
               AND (ROW < 1 OR >= LISTROWS)
                   MOVE   0  TO ROW
               END-IF
               IF  EIBAID = AIDPFK08
               AND (ROW <= 1 OR > LISTROWS)
                   COMPUTE ROW = LISTROWS + 1
               END-IF
               IF  EIBAID = AIDPFK07
                   COMPUTE TS-ITEM = TS-ITEM - (LISTROWS - ROW)
               ELSE
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
               END-IF
               IF  TS-ITEM < 1
                   MOVE  +1  TO TS-ITEM
                   MOVE 'This is the first Entry in this Catalog.
      -                 '                    ' TO LIST-MESSAGE-O
               END-IF
               EXEC CICS READQ TS
                         QUEUE(TS-QUEUE)
                         ITEM(TS-ITEM)
                         INTO(TS-RECD)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP = DFHRESP(ITEMERR)
                   MOVE TS-TOTL TO TS-ITEM
                   MOVE 'This is the last Entry in this Catalog.
      -                 '                    ' TO LIST-MESSAGE-O
               ELSE
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                   AND EIBRESP NOT = DFHRESP(QIDERR)
                       MOVE   -1   TO ERROR-SWITCH
                                      LIST-SELECT-L(1)
                       MOVE EXHREVRS TO LIST-MESSAGE-H
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG TO LIST-MESSAGE-O
                       GO TO C80-LIST-OUTPUT
                   END-IF
               END-IF
               GO TO C60-LIST-DISPLAY
           END-IF.

       C60-LIST-DISPLAY.

           MOVE    'LIST'    TO COMM-CURRRTN.

           IF  LIST-CATNAME-I NOT > SPACES
               MOVE  SPACES  TO DAPSYSF-KEY
               SET SYSF-VSAM-CATALOG TO TRUE
               EXEC CICS READ
                         DATASET(DAPSYSF)
                         INTO(DAPSYSF-REC)
                         RIDFLD(DAPSYSF-KEY)
                         GTEQ
                         NOHANDLE
               END-EXEC
               IF  NOT SYSF-VSAM-CATALOG
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE  SPACES  TO SYSF-CATNAME
                   MOVE    -1    TO ERROR-SWITCH
                                    LIST-CATNAME-L
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG TO LIST-MESSAGE-O
                   ELSE
                       MOVE 'There are no Catalog Names defined in the D
      -                    'APSYSF file.             ' TO LIST-MESSAGE-O
                   END-IF
                   GO TO C80-LIST-OUTPUT
               END-IF
               MOVE SYSF-CATNAME TO LIST-CATNAME-O
               MOVE    SPACES    TO LIST-OLDNAME-O
           END-IF.

           IF  LIST-CATNAME-I NOT = LIST-OLDNAME-I
               MOVE  ZEROES  TO TS-ITEM
                                TS-TOTL
               GO TO C70-LIST-DETAIL
           END-IF.

           IF  EIBAID = AIDENTER
             IF  LIST-CATNAME-I > SPACES
             AND VCAT-CATALOG > SPACES
               EXEC CICS ASSIGN
                         OPID(USER-ID-O)
                         NOEDF
                         NOHANDLE
               END-EXEC
               MOVE DFHRESP(NORMAL) TO EIBRESP
               SET NO-ERRORS-FOUND TO TRUE
               PERFORM WITH TEST BEFORE
                 VARYING ROW FROM 1 BY 1
                   UNTIL ROW > LISTROWS
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                      OR ERRORS-FOUND
                   IF  LIST-SELECT-I(ROW) = 'P'
/DLC1/*            AND (USER-ID-I = 'DLC' OR 'JCD' OR 'WCW' OR
/DLC1/*                             'JPD' OR 'DRR' OR 'RAH')
/DLC1/             AND VCAT-PURGEABLE
                       COMPUTE XX-ITEM = TS-ITEM + (ROW - 1)
                       EXEC CICS READQ TS
                                 QUEUE(TS-QUEUE)
                                 ITEM(XX-ITEM)
                                 INTO(TS-RECD)
                                 NOHANDLE
                       END-EXEC
                       IF (TS-ENTTYPE = 'KSDS' OR 'XLDS'
                                     OR 'ESDS' OR 'RRDS'
                                     OR 'SAME' OR 'SAMI'
                                     OR 'AIX ' OR 'PATH')
                       AND TS-ENTNAME NOT = LIST-CATNAME-I
                           COMPUTE EIBCPOSN = (ROW + 5) * 80 + 1
                           EXEC CICS SEND CONTROL
                                     CURSOR(EIBCPOSN)
                           END-EXEC
                           MOVE '     Press PF2 to confirm SUBMIT of PUR
      -                         'GE job, else press ENTER to cancel it.'
                               TO CONFIRM-MSG
                           PERFORM V00-VALIDATE THRU V90-EXIT-VALIDATE
                           IF  EIBAID NOT = AIDPFK02
                               MOVE  SPACES  TO LIST-SELECT-I(ROW)
                               MOVE EXHULINE TO LIST-SELECT-H(ROW)
                           ELSE
                               MOVE   '*'    TO TS-SELECT
                                                LIST-SELECT-I(ROW)
                               MOVE FLDASBRT TO LIST-SELECT-A(ROW)
                               MOVE EXCNEUTR TO LIST-SELECT-C(ROW)
                               MOVE EXHDEFLT TO LIST-SELECT-H(ROW)
                               MOVE TS-ENTNAME TO VCAT-ENTNAME
                               EXEC CICS WRITEQ TS
                                         QUEUE(TS-QUEUE)
                                         ITEM(XX-ITEM)
                                         FROM(TS-RECD)
                                         REWRITE
                                         MAIN
                                         NOHANDLE
                               END-EXEC
                               PERFORM F00-SUBMIT THRU F90-EXIT
                           END-IF
                       ELSE
                           MOVE  SPACES  TO LIST-SELECT-I(ROW)
                           MOVE EXHULINE TO LIST-SELECT-H(ROW)
                       END-IF
                   ELSE
                       IF  LIST-SELECT-I(ROW) NOT = '*'
                           MOVE  SPACES  TO LIST-SELECT-I(ROW)
                           MOVE EXHULINE TO LIST-SELECT-H(ROW)
                       END-IF
                   END-IF
               END-PERFORM
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C80-LIST-OUTPUT
               END-IF
             ELSE
               MOVE    -1    TO ERROR-SWITCH
                                LIST-CATNAME-L
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE 'Missing information (at cursor) or validation requi
      -             'red first (press ENTER).   ' TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
             END-IF
           END-IF.

       C70-LIST-DETAIL.

           IF  TS-ITEM NOT > ZERO
               PERFORM P30-TS-LIST-BUILD THRU P35-EXIT-LIST-BUILD
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE    -1    TO ERROR-SWITCH
                                        LIST-CATNAME-L
                       MOVE EXHREVRS TO LIST-MESSAGE-H
                       MOVE 'That Catalog Name is not defined in the DAP
      -                    'SYSF dataset.            ' TO LIST-MESSAGE-O
                   END-IF
                   GO TO C80-LIST-OUTPUT
               END-IF
               MOVE LIST-CATNAME-I TO LIST-OLDNAME-O
           END-IF.

           MOVE      1       TO X1.
           MOVE   TS-ITEM    TO XX-ITEM.
           MOVE DFHRESP(NORMAL) TO EIBRESP.

           IF  TS-TOTL > ZERO
               PERFORM WITH TEST BEFORE
                   VARYING X1 FROM 1 BY 1
                     UNTIL X1 > LISTROWS
                        OR XX-ITEM > TS-TOTL
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   EXEC CICS READQ TS
                             QUEUE(TS-QUEUE)
                             INTO(TS-RECD)
                             ITEM(XX-ITEM)
                             NOHANDLE
                   END-EXEC
                   IF  EIBRESP = DFHRESP(NORMAL)
                       MOVE TS-SELECT   TO LIST-SELECT-O(X1)
                       IF  TS-SELECT = '*'
                           MOVE  FLDASBRT  TO LIST-SELECT-A(X1)
                           MOVE  EXCNEUTR  TO LIST-SELECT-C(X1)
                           MOVE  EXHDEFLT  TO LIST-SELECT-H(X1)
                       ELSE
                           MOVE LOW-VALUES TO LIST-SELECT-A(X1)
                           MOVE LOW-VALUES TO LIST-SELECT-C(X1)
                           MOVE  EXHULINE  TO LIST-SELECT-H(X1)
                       END-IF
                       MOVE LOW-VALUES  TO LIST-ENTNAME-C(X1)
                       MOVE TS-ENTNAME  TO LIST-ENTNAME-O(X1)
                       EVALUATE TS-ENTTYPE ALSO TRUE
                       WHEN 'AIX ' ALSO TS-EXTENTS > 98
                       WHEN 'KSDS' ALSO TS-EXTENTS > 98
                       WHEN 'XLDS' ALSO TS-EXTENTS > 98
                           MOVE EXCRED   TO LIST-ENTNAME-C(X1)
                       WHEN 'ESDS' ALSO TS-EXTENTS > 50
                           MOVE EXCRED   TO LIST-ENTNAME-C(X1)
                       WHEN 'RRDS' ALSO TS-EXTENTS > 12
                       WHEN 'SAME' ALSO TS-EXTENTS > 12
                       WHEN 'SAMI' ALSO TS-EXTENTS > 12
                           MOVE EXCRED   TO LIST-ENTNAME-C(X1)
                       END-EVALUATE
                       MOVE TS-ENTTYPE  TO LIST-ENTTYPE-O(X1)
                       IF  TS-LASTUPD > ZERO
                           MOVE TS-LASTUPD  TO LIST-LASTUPD-O(X1)
                           MOVE TS-LASTUPT  TO LIST-LASTUPT-O(X1)
                           INSPECT LIST-LASTUPT-O(X1)
                               REPLACING ALL SPACES BY ':'
                       ELSE
                           MOVE LOW-VALUES TO LIST-LASTUPD-I(X1)
                           MOVE LOW-VALUES TO LIST-LASTUPT-I(X1)
                       END-IF
                       IF  TS-RECOUNT < 9999999
                           MOVE TS-RECOUNT  TO LIST-RECOUNT-O(X1)
                       ELSE
                           MOVE LENGTH OF LIST-RECOUNT-I TO P2
                           IF  TS-RECOUNT < 9999999999
                               COMPUTE LIST-RECOUNT-O(X1) =
                                   TS-RECOUNT / 1000
                               MOVE 'K' TO LIST-RECOUNT-I(X1)(P2:1)
                           ELSE
                               COMPUTE LIST-RECOUNT-O(X1) =
                                   TS-RECOUNT / 1000000
                               MOVE 'M' TO LIST-RECOUNT-I(X1)(P2:1)
                           END-IF
                       END-IF
                       ADD   1   TO XX-ITEM
                   END-IF
               END-PERFORM
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(QIDERR)
               GO TO X00-UNEX-ERR
           END-IF.

           PERFORM WITH TEST BEFORE
               VARYING X1 FROM X1 BY 1
                 UNTIL X1 > LISTROWS
               MOVE FLDASDRK TO LIST-SELECT-A(X1)
               MOVE LOW-VALUES TO LIST-SELECT-O(X1)
               MOVE LOW-VALUES TO LIST-ENTNAME-O(X1)
               MOVE LOW-VALUES TO LIST-ENTTYPE-O(X1)
               MOVE LOW-VALUES TO LIST-LASTUPD-I(X1)
               MOVE LOW-VALUES TO LIST-LASTUPT-I(X1)
               MOVE LOW-VALUES TO LIST-RECOUNT-I(X1)
           END-PERFORM.

       C80-LIST-OUTPUT.

           MOVE MAPTITLE               TO LIST-COMPANY-O.

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     FULLDATE(LIST-SYSDATE-O)
                     DATESEP('/')
                     TIME(WS-WRKTIME)
                     TIMESEP(':')
           END-EXEC.
           IF  WS-HRSTIME > 12
               MOVE  ' PM'   TO WS-XXXTIME
               SUBTRACT  12  FROM WS-HRSTIME
           ELSE
           IF  WS-HRSTIME = 12
               MOVE  ' PM'   TO WS-XXXTIME
           ELSE
           MOVE    ' AM'     TO WS-XXXTIME
           IF  WS-HRSTIME = 00
               MOVE    12    TO WS-HRSTIME
           END-IF.
           MOVE  WS-WRKTIME  TO LIST-SYSTIME-O.

           IF  ERRORS-FOUND
               EXEC CICS SEND
                         MAP(LISTMAP)
                         DATAONLY
                         FROM(LISTMAPO)
                         MAPSET(VCATMS)
                         TERMINAL
                         FREEKB
                         ALARM
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP(LISTMAP)
                         FROM(LISTMAPO)
                         MAPSET(VCATMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA(LISTMAPI)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(DAPSYSF-REC)
           END-EXEC.

           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(DFHCOMMAREA)
                     LENGTH(EIBCALEN)
           END-EXEC.

      /*****************************************************************
      *    PROMPT FOR A CATALOG NAME                                   *
      ******************************************************************
       D00-VCAT-PROMPT.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF DAPSYSF-REC)
                     LENGTH(LENGTH OF DAPSYSF-REC)
           END-EXEC.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF VCATWINI)
                     LENGTH(LENGTH OF VCATWINI)
                     INITIMG(LOVALUE)
           END-EXEC.

           IF  COMM-CURRRTN NOT = 'VCAT'
               EXEC CICS SEND
                         MAP(LISTXXX)
                         MAPSET(VCATMS)
                         MAPONLY
                         TERMINAL
                         NOHANDLE
               END-EXEC
               GO TO D50-VCAT-DISPLAY
           END-IF.

       D10-VCAT-RECEIVE.

           EXEC CICS RECEIVE
                     MAP(VCATWIN)
                     INTO(VCATWINI)
                     MAPSET(VCATMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(MAPFAIL) AND DFHRESP(NORMAL)
               GO TO X00-UNEX-ERR
           END-IF.

       D20-VCAT-KEYS.

           IF  EIBAID = AIDCURSR OR AIDPAK01 OR AIDPAK02 OR AIDPAK03
                     OR AIDPFK07 OR AIDPFK08
               GO TO D50-VCAT-DISPLAY
           ELSE
           IF  EIBAID = AIDENTER
               GO TO D30-VCAT-PROCESS
           ELSE
           IF  EIBAID = AIDCLEAR OR AIDPFK03
               MOVE  SPACES  TO VCAT-PROMPT
               GO TO C00-LIST-ROUTINE
           END-IF.

           EXEC CICS SEND
                     MAP(VCATERR)
                     MAPSET(VCATMS)
                     MAPONLY
                     TERMINAL
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS DELAY
                     INTERVAL(1)
                     NOHANDLE
           END-EXEC.

           GO TO D50-VCAT-DISPLAY.

       D30-VCAT-PROCESS.

           DIVIDE EIBCPOSN BY 80 GIVING ROW REMAINDER COL.
           ADD       1       TO ROW COL.
           SUBTRACT VWINHEAD FROM ROW.

           IF  ROW >= 1 AND ROW <= VWINROWS
               IF  VCAT-FILE-I(ROW) > SPACES
                   MOVE VCAT-FILE-I(ROW) TO VCAT-PROMPT
                   GO TO C00-LIST-ROUTINE
               END-IF
           END-IF.

           EXEC CICS SEND
                     MAP(VCATERR)
                     MAPSET(VCATMS)
                     MAPONLY
                     TERMINAL
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS DELAY
                     INTERVAL(1)
                     NOHANDLE
           END-EXEC.

           GO TO D50-VCAT-DISPLAY.

       D50-VCAT-DISPLAY.

           MOVE    'VCAT'    TO COMM-CURRRTN.

           IF  VCAT-FILE-I(1) > SPACES
           AND EIBAID NOT = AIDPFK07 AND AIDPFK08
               GO TO D80-VCAT-OUTPUT
           END-IF.

           IF  VCAT-FILE-I(1) NOT > SPACES
               MOVE VCAT-PROMPT TO VCAT-FILE-I(1)
           END-IF.

           MOVE    SPACES    TO DAPSYSF-KEY.
           SET SYSF-VSAM-CATALOG TO TRUE.
           MOVE VCAT-FILE-I(1) TO SYSF-CATFILE.

           EXEC CICS STARTBR
                     DATASET(DAPSYSF)
                     RIDFLD(DAPSYSF-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO X00-UNEX-ERR
           END-IF.

       D60-VCAT-SCROLL.

           IF  EIBAID NOT = AIDPFK07 AND AIDPFK08
               GO TO D70-VCAT-DETAILS
           END-IF.

           IF EIBAID = AIDPFK07
               PERFORM WITH TEST AFTER
                   VARYING X1 FROM 1 BY 1
                     UNTIL X1 > VWINROWS
                        OR NOT SYSF-VSAM-CATALOG
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   EXEC CICS READPREV
                             DATASET(DAPSYSF)
                             INTO(DAPSYSF-REC)
                             RIDFLD(DAPSYSF-KEY)
                             NOHANDLE
                   END-EXEC
               END-PERFORM
               IF  EIBRESP NOT = DFHRESP(ENDFILE)
               AND EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO X00-UNEX-ERR
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
               AND NOT SYSF-VSAM-CATALOG
                   EXEC CICS READNEXT
                             DATASET(DAPSYSF)
                             INTO(DAPSYSF-REC)
                             RIDFLD(DAPSYSF-KEY)
                             NOHANDLE
                   END-EXEC
               END-IF
           END-IF.

           IF  EIBAID = AIDPFK08
               PERFORM WITH TEST AFTER
                   VARYING X1 FROM 1 BY 1
                     UNTIL X1 = VWINROWS
                        OR NOT SYSF-VSAM-CATALOG
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   EXEC CICS READNEXT
                             DATASET(DAPSYSF)
                             INTO(DAPSYSF-REC)
                             RIDFLD(DAPSYSF-KEY)
                             NOHANDLE
                   END-EXEC
               END-PERFORM
               IF  EIBRESP NOT = DFHRESP(ENDFILE)
               AND EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO X00-UNEX-ERR
               END-IF
               IF  EIBRESP = DFHRESP(ENDFILE)
               OR  NOT SYSF-VSAM-CATALOG
                   IF  EIBRESP = DFHRESP(NORMAL)
                   AND NOT SYSF-VSAM-CATALOG
                       EXEC CICS READPREV
                                 DATASET(DAPSYSF)
                                 INTO(DAPSYSF-REC)
                                 RIDFLD(DAPSYSF-KEY)
                                 NOHANDLE
                       END-EXEC
                   END-IF
                   PERFORM WITH TEST AFTER
                       VARYING X1 FROM 1 BY 1
                         UNTIL X1 = VWINROWS
                            OR NOT SYSF-VSAM-CATALOG
                            OR EIBRESP NOT = DFHRESP(NORMAL)
                       EXEC CICS READPREV
                                 DATASET(DAPSYSF)
                                 INTO(DAPSYSF-REC)
                                 RIDFLD(DAPSYSF-KEY)
                                 NOHANDLE
                       END-EXEC
                   END-PERFORM
                   IF  EIBRESP = DFHRESP(NORMAL)
                   AND NOT SYSF-VSAM-CATALOG
                       EXEC CICS READNEXT
                                 DATASET(DAPSYSF)
                                 INTO(DAPSYSF-REC)
                                 RIDFLD(DAPSYSF-KEY)
                                 NOHANDLE
                       END-EXEC
                   END-IF
               END-IF
           END-IF.

       D70-VCAT-DETAILS.

           PERFORM WITH TEST AFTER
               VARYING X1 FROM 1 BY 1
                 UNTIL X1 = VWINROWS
                    OR NOT SYSF-VSAM-CATALOG
                    OR EIBRESP NOT = DFHRESP(NORMAL)
               EXEC CICS READNEXT
                         DATASET(DAPSYSF)
                         INTO(DAPSYSF-REC)
                         RIDFLD(DAPSYSF-KEY)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP = DFHRESP(NORMAL)
               AND SYSF-VSAM-CATALOG
                   MOVE SYSF-CATFILE TO VCAT-FILE-O(X1)
                   MOVE SYSF-CATNAME TO VCAT-NAME-O(X1)
               ELSE
                   SUBTRACT 1 FROM X1
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO X00-UNEX-ERR
           END-IF.

           EXEC CICS ENDBR
                     DATASET(DAPSYSF)
                     NOHANDLE
           END-EXEC.

           IF  X1 > ZERO
               ADD     1     TO X1
               PERFORM WITH TEST BEFORE
                   VARYING X1 FROM X1 BY 1
                     UNTIL X1 > VWINROWS
                   MOVE FLDASDRK TO VCAT-NAME-A(X1)
                   MOVE SPACES   TO VCAT-NAME-O(X1)
                   MOVE SPACES   TO VCAT-FILE-O(X1)
               END-PERFORM
           END-IF.

       D80-VCAT-OUTPUT.

           MOVE      -1      TO VCAT-NAME-L(1)
           EXEC CICS SEND
                     MAP(VCATWIN)
                     FROM(VCATWINO)
                     MAPSET(VCATMS)
                     TERMINAL
                     FREEKB
                     CURSOR
           END-EXEC.

       D90-VCAT-RETURN.

           EXEC CICS FREEMAIN
                     DATA(VCATWINI)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(DAPSYSF-REC)
           END-EXEC.

           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(DFHCOMMAREA)
                     LENGTH(EIBCALEN)
           END-EXEC.

      /*****************************************************************
      *    ROUTINE TO MANAGE THE "DETAILS" SCREEN                      *
      ******************************************************************
       E00-DETL-ROUTINE.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF DETLMAPI)
                     LENGTH(LENGTH OF DETLMAPI)
                     INITIMG(LOVALUE)
           END-EXEC.

           EXEC CICS ASSIGN
                     SCRNHT(SBA-HGHT)
                     SCRNWD(SBA-WDTH)
                     NOHANDLE
           END-EXEC.

           IF  COMM-CURRRTN NOT = 'DETL'
               COMPUTE EIBCPOSN = (DETLHEAD * SBA-WDTH) + 1
               MOVE  EIBTRNID  TO T2-TRAN
               MOVE  EIBTRMID  TO T2-TERM
               MOVE   ZEROES   TO T2-ITEM
                                  T2-TOTL
               EXEC CICS SEND
                         MAP(DETLMAP)
                         MAPONLY
                         MAPSET(VCA2MS)
                         TERMINAL
                         ERASE
                         NOHANDLE
               END-EXEC
               EXEC CICS SEND CONTROL
                         CURSOR(EIBCPOSN)
                         NOHANDLE
               END-EXEC
               GO TO E60-DETL-DISPLAY
           END-IF.

       E10-DETL-RECEIVE.

           EXEC CICS RECEIVE
                     MAP(DETLMAP)
                     INTO(DETLMAPI)
                     MAPSET(VCA2MS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP = DFHRESP(MAPFAIL)
               MOVE AIDPFK03 TO EIBAID
               GO TO E20-DETL-KEYS
           ELSE
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO DETL-MESSAGE-H
               MOVE UNEX-MSG TO DETL-MESSAGE-O
               GO TO E60-DETL-DISPLAY
           END-IF.

       E20-DETL-KEYS.

           IF  EIBAID = AIDCURSR OR AIDPAK01 OR AIDPAK02 OR AIDPAK03
               GO TO E60-DETL-DISPLAY
           ELSE
           IF  EIBAID = AIDENTER OR AIDPFK05
                     OR AIDPFK06 OR AIDPFK07 OR AIDPFK08 OR AIDPFK09
               GO TO E30-DETL-PROCESS
           ELSE
           IF  EIBAID = AIDPFK03
               EXEC CICS DELETEQ TS
                         QUEUE(T2-QUEUE)
                         NOHANDLE
               END-EXEC
               MOVE  SPACES  TO VCAT-PROMPT
               MOVE THIS-TRN  TO COMM-TRANSID
               EXEC CICS START
                         TRANSID(THIS-TRN)
                         FROM(DFHCOMMAREA)
                         LENGTH(EIBCALEN)
                         TERMID(EIBTRMID)
               END-EXEC
               GO TO E90-DETL-RETURN
           END-IF.

           MOVE      -2      TO ERROR-SWITCH.
           MOVE   EXHREVRS   TO DETL-MESSAGE-H.
           MOVE 'That Program Function key is not shown, and serves no p
      -         'urpose, on this screen.' TO DETL-MESSAGE-O.

           GO TO E60-DETL-DISPLAY.

       E30-DETL-PROCESS.

           IF  EIBAID = AIDPFK05
               MOVE   ZEROES   TO T2-ITEM
                                  T2-TOTL
               GO TO E60-DETL-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               COMPUTE EIBCPOSN = (DETLHEAD * SBA-WDTH) + 1
               IF  EIBAID = AIDPFK06
                   MOVE   1  TO T2-ITEM
               ELSE
                   COMPUTE T2-ITEM = (T2-TOTL + 1) - (DETLROWS - 1)
               END-IF
               GO TO E60-DETL-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK07 OR AIDPFK08
               DIVIDE EIBCPOSN BY SBA-WDTH GIVING ROW REMAINDER COL
               ADD     1     TO ROW COL
               SUBTRACT DETLHEAD FROM ROW
               IF  EIBAID = AIDPFK07
               AND (ROW < 1 OR >= DETLROWS)
                   MOVE   0  TO ROW
               END-IF
               IF  EIBAID = AIDPFK08
               AND (ROW <= 1 OR > DETLROWS)
                   COMPUTE ROW = DETLROWS + 1
               END-IF
               MOVE T2-ITEM  TO XX-ITEM
               IF  EIBAID = AIDPFK07
                   COMPUTE T2-ITEM = T2-ITEM - (DETLROWS - ROW)
               ELSE
                   COMPUTE T2-ITEM = T2-ITEM + (ROW - 1)
               END-IF
               IF  T2-ITEM < 1
                   MOVE  +1  TO T2-ITEM
                                XX-ITEM
                   COMPUTE EIBCPOSN = (DETLHEAD * SBA-WDTH) + 1
                   MOVE 'This is the first Line available for display.
      -                 '                    ' TO DETL-MESSAGE-O
               END-IF
               EXEC CICS READQ TS
                         QUEUE(T2-QUEUE)
                         ITEM(T2-ITEM)
                         INTO(T2-DETL)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP = DFHRESP(ITEMERR)
                   MOVE T2-TOTL TO T2-ITEM
                   MOVE 'This is the last Line available for display.
      -                 '                    ' TO DETL-MESSAGE-O
               ELSE
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                   AND EIBRESP NOT = DFHRESP(QIDERR)
                       MOVE   -1   TO ERROR-SWITCH
                       MOVE EXHREVRS TO DETL-MESSAGE-H
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG TO DETL-MESSAGE-O
                       GO TO E80-DETL-OUTPUT
                   END-IF
               END-IF
               IF  EIBAID = AIDPFK07
               AND EIBCPOSN > (DETLHEAD * SBA-WDTH)
                        AND < ((DETLHEAD + DETLROWS - 1) * SBA-WDTH + 1)
               OR  EIBAID = AIDPFK08
               AND EIBCPOSN > ((DETLHEAD + 1) * SBA-WDTH - 2)
                        AND < ((DETLHEAD + DETLROWS) * SBA-WDTH - 1)
                   COMPUTE EIBCPOSN = EIBCPOSN +
                       ((XX-ITEM - T2-ITEM) * SBA-WDTH)
               END-IF
               GO TO E60-DETL-DISPLAY
           END-IF.

       E60-DETL-DISPLAY.

           MOVE    'DETL'    TO COMM-CURRRTN.

       E70-DETL-DETAIL.

           IF  T2-ITEM NOT > ZERO
               PERFORM P40-T2-DETL-BUILD THRU P45-EXIT-DETL-BUILD
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E80-DETL-OUTPUT
               END-IF
           END-IF.

           MOVE      1       TO X1.
           MOVE   T2-ITEM    TO XX-ITEM.
           MOVE DFHRESP(NORMAL) TO EIBRESP.

           MOVE   T2-ITEM    TO DETL-CURLINE-O.
           MOVE   T2-TOTL    TO DETL-TOTLINE-O.

           IF  T2-TOTL > ZERO
               PERFORM WITH TEST BEFORE
                   VARYING X1 FROM 1 BY 1
                     UNTIL X1 > DETLROWS
                        OR XX-ITEM > T2-TOTL
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   EXEC CICS READQ TS
                             QUEUE(T2-QUEUE)
                             INTO(T2-DETL)
                             ITEM(XX-ITEM)
                             NOHANDLE
                   END-EXEC
                   IF  EIBRESP = DFHRESP(NORMAL)
                       MOVE T2-DETLINE TO DETL-LINE-O(X1)
                       IF  T2-LNCOUNT = ZERO
                           MOVE   SPACES   TO DETL-COUNT-I(X1)
                       ELSE
                           MOVE T2-LNCOUNT TO DETL-COUNT-O(X1)
                       END-IF
                       ADD   1   TO XX-ITEM
                   END-IF
               END-PERFORM
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(QIDERR)
               GO TO X00-UNEX-ERR
           END-IF.

           SET EOD-NOT-MARKED TO TRUE.

           PERFORM WITH TEST BEFORE
               VARYING X1 FROM X1 BY 1
                 UNTIL X1 > DETLROWS
               MOVE EXCBLUE  TO DETL-LINE-C(X1)
               IF  EOD-NOT-MARKED
                   SET EOD-MARKED TO TRUE
                   MOVE ' -- END OF DATA -- ' TO DETL-LINE-O(X1)
                   MOVE   SPACES   TO DETL-COUNT-I(X1)
               ELSE
                   MOVE LOW-VALUES TO DETL-LINE-O(X1)
                                      DETL-COUNT-I(X1)
               END-IF
           END-PERFORM.

       E80-DETL-OUTPUT.

           MOVE MAPTITLE               TO DETL-COMPANY-O.

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     FULLDATE(DETL-SYSDATE-O)
                     DATESEP('/')
                     TIME(WS-WRKTIME)
                     TIMESEP(':')
           END-EXEC.
           IF  WS-HRSTIME > 12
               MOVE  ' PM'   TO WS-XXXTIME
               SUBTRACT  12  FROM WS-HRSTIME
           ELSE
           IF  WS-HRSTIME = 12
               MOVE  ' PM'   TO WS-XXXTIME
           ELSE
           MOVE    ' AM'     TO WS-XXXTIME
           IF  WS-HRSTIME = 00
               MOVE    12    TO WS-HRSTIME
           END-IF.
           MOVE  WS-WRKTIME  TO DETL-SYSTIME-O.

           EXEC CICS READQ TS
                     QUEUE(TS-QUEUE)
                     ITEM(TS-ITEM)
                     INTO(TS-RECD)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO TS-RECD
           END-IF.

           EVALUATE VCAT-SUBRRTN
           WHEN 'SMAP'
               MOVE '             VSAM Space Map             '
                   TO DETL-TITLE-O
           WHEN 'DETL'
               MOVE '          VSAM Details Listing          '
                   TO DETL-TITLE-O
           WHEN OTHER
               MOVE '         VSAM Basic Information         '
                   TO DETL-TITLE-O
           END-EVALUATE.

           IF  ERRORS-FOUND
               EXEC CICS SEND
                         MAP(DETLMAP)
                         DATAONLY
                         FROM(DETLMAPO)
                         MAPSET(VCA2MS)
                         TERMINAL
                         FREEKB
                         ALARM
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP(DETLMAP)
                         FROM(DETLMAPO)
                         MAPSET(VCA2MS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR(EIBCPOSN)
               END-EXEC
           END-IF.

       E90-DETL-RETURN.

           EXEC CICS FREEMAIN
                     DATA(DETLMAPI)
           END-EXEC.

           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(DFHCOMMAREA)
                     LENGTH(EIBCALEN)
           END-EXEC.

      /*****************************************************************
      *    SUBMIT DATASET PURGE JOB TO BATCH                           *
      ******************************************************************
       F00-SUBMIT.

           PERFORM WITH TEST BEFORE
               VARYING JCL-SUB FROM 1 BY 1
                 UNTIL JCL-SUB > JOB-TOTAL
                    OR ERRORS-FOUND
               MOVE  ZEROES  TO TXT-PNTR
               MOVE    80    TO TXT-MSTR-BUFLEN
               MOVE JCL-TEMPLATE(JCL-SUB) TO TXT-MSTR-BUFFER
               PERFORM F50-PROCESS-SUBSTITUTION THRU F60-EXIT
               MOVE TXT-MSTR-BUFFER TO JCL-CARD(JCL-SUB)
           END-PERFORM.
           IF  ERRORS-FOUND
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE  SPACES  TO LIST-MESSAGE-O
               STRING 'Internal substitution error.  rc='''
                   TXT-RETN ''' on rq=''' TXT-REQU ''''
                   DELIMITED BY SIZE INTO LIST-MESSAGE-O
               GO TO F90-EXIT
           END-IF.

           EXEC CICS SPOOLOPEN JCL
                     REPORT(JOB-NAME)
                     TOKEN(SUBMIT-TOKEN)
                     LOGICAL
                     NOHANDLE
           END-EXEC.

           IF EIBRESP = DFHRESP(NORMAL)
               EXEC CICS SPOOLWRITE
                         FROM(JOB-TABLE)
                         FLENGTH(JOB-LENGTH)
                         REPORT(JOB-NAME)
                         TOKEN(SUBMIT-TOKEN)
                         NOHANDLE
               END-EXEC
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -1    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               EXEC CICS SPOOLCLOSE
                         REPORT(JOB-NAME)
                         TOKEN(SUBMIT-TOKEN)
                         DELETE
                         NOHANDLE
               END-EXEC
               GO TO F90-EXIT
           END-IF.

           EXEC CICS SPOOLCLOSE
                     REPORT(JOB-NAME)
                     TOKEN(SUBMIT-TOKEN)
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE 'Purge job(s) submitted.' TO LIST-MESSAGE-O
           ELSE
               MOVE    -1    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
           END-IF.

           GO TO F90-EXIT.

       F50-PROCESS-SUBSTITUTION.

           MOVE   '<USR>'    TO VAR-NAME.
           MOVE      +8      TO VAR-LENG.
           MOVE  USER-ID-I   TO VAR-TEMP.
           MOVE    ZEROES    TO TALLY.
           INSPECT VAR-TEMP TALLYING TALLY
               FOR CHARACTERS BEFORE INITIAL SPACE.
           MOVE     TALLY    TO VAR-LENG.
           MOVE   VAR-TEMP   TO VAR-DATA.
           PERFORM F70-VARIABLE-SUBSTITUTION THRU F80-EXIT.
           IF  ERRORS-FOUND  GO TO F60-EXIT.

           MOVE   '<DSN>'    TO VAR-NAME.
           MOVE     +44      TO VAR-LENG.
           MOVE    ZEROES    TO TALLY.
           INSPECT VCAT-ENTNAME TALLYING TALLY
               FOR CHARACTERS BEFORE INITIAL SPACE.
           MOVE     TALLY    TO VAR-LENG.
           MOVE VCAT-ENTNAME TO VAR-DATA.
           PERFORM F70-VARIABLE-SUBSTITUTION THRU F80-EXIT.
           IF  ERRORS-FOUND  GO TO F60-EXIT.

           MOVE   '<OPT>'    TO VAR-NAME.
           EVALUATE TS-ENTTYPE
           WHEN 'AIX'
               MOVE   +16    TO VAR-LENG
               MOVE 'AIX      NOERASE' TO VAR-DATA
           WHEN 'PATH'
               MOVE    +7    TO VAR-LENG
               MOVE  'PATH'  TO VAR-DATA
           WHEN OTHER
               MOVE   +16    TO VAR-LENG
               MOVE 'CLUSTER  NOERASE' TO VAR-DATA
           END-EVALUATE.
           PERFORM F70-VARIABLE-SUBSTITUTION THRU F80-EXIT.
           IF  ERRORS-FOUND  GO TO F60-EXIT.

           MOVE   '<CAT>'    TO VAR-NAME.
           MOVE     +44      TO VAR-LENG.
           MOVE    ZEROES    TO TALLY.
           INSPECT LIST-CATNAME-I TALLYING TALLY
               FOR CHARACTERS BEFORE INITIAL SPACE.
           MOVE     TALLY    TO VAR-LENG.
           MOVE LIST-CATNAME-I TO VAR-DATA.
           PERFORM F70-VARIABLE-SUBSTITUTION THRU F80-EXIT.
           IF  ERRORS-FOUND  GO TO F60-EXIT.

       F60-EXIT.
           EXIT.

       F70-VARIABLE-SUBSTITUTION.

           MOVE     'F'      TO TXT-REQU.
           MOVE      05      TO TXT-PARM-STRLEN.
           MOVE   VAR-NAME   TO TXT-PARM-STRING.
           CALL   'TXTMAN'   USING TXTMAN-PARMS
                                   TXTMAN-BUFFER
                                   TXTMAN-STRING.
           IF  TXT-STRING-NOT-FOUND
               MOVE  ZEROES  TO TXT-PNTR
               GO TO F80-EXIT
           END-IF.
           IF  NOT TXT-STRING-WAS-FOUND
               MOVE    -1    TO ERROR-SWITCH
               GO TO F80-EXIT
           END-IF.

           MOVE     'D'      TO TXT-REQU.
           MOVE    SPACES    TO TXT-PARM-STRING.
           CALL   'TXTMAN'   USING TXTMAN-PARMS
                                   TXTMAN-BUFFER
                                   TXTMAN-STRING.
           IF  NOT TXT-REQUEST-COMPLETED
               MOVE    -1    TO ERROR-SWITCH
               GO TO F80-EXIT
           END-IF.

           MOVE     'I'      TO TXT-REQU.
           MOVE   VAR-LENG   TO TXT-PARM-STRLEN.
           MOVE   VAR-DATA   TO TXT-PARM-STRING.
           CALL   'TXTMAN'   USING TXTMAN-PARMS
                                   TXTMAN-BUFFER
                                   TXTMAN-STRING.
           IF NOT TXT-REQUEST-COMPLETED
               MOVE    -1    TO ERROR-SWITCH
               GO TO F80-EXIT
           END-IF.

           GO TO F70-VARIABLE-SUBSTITUTION.

       F80-EXIT.
           EXIT.

       F90-EXIT.
           EXIT.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-JUSTIFY-LEFT.

           IF  HEX-LENG > 1 AND HEX-CHRT > SPACES
               PERFORM WITH TEST BEFORE
                 UNTIL HEX-CHRE(1) > SPACE
                   PERFORM WITH TEST BEFORE
                     VARYING X2 FROM 1 BY +1 UNTIL X2 = HEX-LENG
                       MOVE HEX-CHRE(X2 + 1) TO HEX-CHRE(X2)
                   END-PERFORM
                   MOVE SPACE TO HEX-CHRE(HEX-LENG)
               END-PERFORM
           END-IF.

       P15-EXIT-JUSTIFY.
           EXIT.

       P20-JUSTIFY-RIGHT.

           IF  HEX-LENG > 1 AND HEX-CHRT > SPACES
               PERFORM WITH TEST BEFORE
                 UNTIL HEX-CHRE(HEX-LENG) > SPACE
                   PERFORM WITH TEST BEFORE
                     VARYING X2 FROM HEX-LENG BY -1 UNTIL X2 = 1
                       MOVE HEX-CHRE(X2 - 1) TO HEX-CHRE(X2)
                   END-PERFORM
                   MOVE SPACE TO HEX-CHRE(1)
               END-PERFORM
           END-IF.

       P25-EXIT-JUSTIFY.
           EXIT.

       P30-TS-LIST-BUILD.

           EXEC CICS SEND
                     MAP(GENRWIN)
                     MAPSET(VCATMS)
                     MAPONLY
                     TERMINAL
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS DELETEQ TS
                     QUEUE(TS-QUEUE)
                     NOHANDLE
           END-EXEC.

           IF  VCAT-CATALOG > SPACES
               PERFORM P90-CLOSE-CATALOG THRU P95-EXIT-CLOSE
               MOVE  SPACES  TO VCAT-CATALOG
           END-IF.

           PERFORM P70-IDENTIFY-CATALOG THRU P75-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO P35-EXIT-LIST-BUILD
           END-IF.

           PERFORM P80-OPEN-CATALOG THRU P85-EXIT-OPEN.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO P35-EXIT-LIST-BUILD
           END-IF.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF CATALOG-RECORD)
                     LENGTH(LENGTH OF CATALOG-RECORD)
                     INITIMG(LOVALUE)
           END-EXEC.

           MOVE    SPACES    TO CAT-HI-KEY.
           EXEC CICS STARTBR
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO P34-FREEMAIN
           END-IF.

           MOVE    SPACES    TO SAVE-CAT-KEY.
           PERFORM WITH TEST AFTER
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)
               EXEC CICS READNEXT
                         DATASET(VCAT-CATALOG)
                         INTO(CATALOG-RECORD)
                         RIDFLD(CATALOG-KEY)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP = DFHRESP(NORMAL)
               AND CAT-HI-KEY NOT = SAVE-CAT-KEY
               AND NOT (CAT-HI-TAG = '.VSAMDSET.DFD' AND
                        CAT-HI-PFX = CAT-HI-SFX)
                   MOVE CAT-HI-KEY TO SAVE-CAT-KEY
                   PERFORM P32-WRITE-TS-RECD THRU P33-EXIT-WRITE
               END-IF
           END-PERFORM.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO P34-FREEMAIN
           END-IF.

           EXEC CICS ENDBR
                     DATASET(VCAT-CATALOG)
                     NOHANDLE
           END-EXEC.

           MOVE      +1      TO TS-ITEM.
           EXEC CICS READQ TS
                     QUEUE(TS-QUEUE)
                     ITEM(TS-ITEM)
                     INTO(TS-RECD)
                     NUMITEMS(TS-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE  ZEROES  TO TS-ITEM
               MOVE  ZEROES  TO TS-TOTL
               IF  EIBRESP = DFHRESP(QIDERR)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
               END-IF
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO P34-FREEMAIN
           END-IF.

           GO TO P34-FREEMAIN.

       P32-WRITE-TS-RECD.

           MOVE CAT-HI-KEY   TO TS-ENTNAME.
           MOVE CAT-HI-CINO  TO TS-CINO.

           MOVE LOW-VALUES   TO CAT-LO-KEY.
           MOVE TS-CINO      TO CAT-LO-CINO.
           EXEC CICS READ
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     INTO(CATALOG-RECORD)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  CAT-LO-CINO NOT = TS-CINO
               IF  CAT-LO-CINO NOT = TS-CINO
                   MOVE DFHRESP(NOTFND) TO EIBRESP
               END-IF
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO LIST-MESSAGE-H
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO P33-EXIT-WRITE
           END-IF.

           MOVE CAT-C-TYPE   TO TS-RECTYPE.
           EVALUATE TS-RECTYPE
           WHEN 'C'
               SET INDEX-ASSOC-NOT-FOUND TO TRUE
               PERFORM T00-PARSE-C-GOPS THRU T09-EXIT-PARSE
               IF  INDEX-ASSOC-WAS-FOUND
                   MOVE 'KSDS' TO TS-ENTTYPE
               ELSE
                   MOVE 'ESDS' TO TS-ENTTYPE
               END-IF
           WHEN 'G'
               MOVE  'AIX '  TO TS-ENTTYPE
               PERFORM T30-PARSE-G-GOPS THRU T39-EXIT-PARSE
           WHEN 'R'
               MOVE  'PATH'  TO TS-ENTTYPE
               PERFORM T40-PARSE-R-GOPS THRU T49-EXIT-PARSE
           WHEN 'U'
               MOVE  'UCAT'  TO TS-ENTTYPE
               PERFORM T50-PARSE-U-GOPS THRU T59-EXIT-PARSE
           WHEN 'V'
               MOVE  'VSPC'  TO TS-ENTTYPE
               PERFORM T60-PARSE-V-GOPS THRU T69-EXIT-PARSE
           WHEN OTHER
               GO TO P33-EXIT-WRITE
           END-EVALUATE.

           MOVE ZEROES       TO TS-EXTENTS.
           MOVE SPACES       TO TS-SELECT.
           MOVE ZEROES       TO TS-LASTUPD.
           MOVE ZEROES       TO TS-LASTUPT.
           MOVE ZEROES       TO TS-RECOUNT.

           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR (ASSOC-GOP-INLINE(X1) AND GO-TYPE(X1) = 'D')
           END-PERFORM.

           IF  X1 <= L1 AND TABLE-ENTRIES
           AND ASSOC-GOP-INLINE(X1)
           AND GO-TYPE(X1) = 'D'
               MOVE LOW-VALUES   TO CAT-LO-KEY
               MOVE ASSOC-CINO(X1) TO CAT-LO-CINO
               EXEC CICS READ
                         DATASET(VCAT-CATALOG)
                         RIDFLD(CATALOG-KEY)
                         INTO(CATALOG-RECORD)
                         GTEQ
                         NOHANDLE
               END-EXEC
               IF  EIBRESP NOT = DFHRESP(NORMAL)
               OR  CAT-LO-CINO NOT = ASSOC-CINO(X1)
                   IF  CAT-LO-CINO NOT = ASSOC-CINO(X1)
                       MOVE DFHRESP(NOTFND) TO EIBRESP
                   END-IF
                   MOVE    -2    TO ERROR-SWITCH
                   PERFORM X00-UNEX-ERR
                   MOVE EXHREVRS TO LIST-MESSAGE-H
                   MOVE UNEX-MSG TO LIST-MESSAGE-O
                   GO TO P33-EXIT-WRITE
               ELSE
                   PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                   PERFORM WITH TEST BEFORE
                     VARYING X1 FROM 1 BY 1
                       UNTIL X1 > L1 OR TABLE-ENTRIES
                          OR AMDSB-GOP-INLINE(X1)
                   END-PERFORM
                   IF  X1 <= L1 AND TABLE-ENTRIES
                   AND AMDSB-GOP-INLINE(X1)
                       IF  TS-ENTTYPE = 'ESDS'
                           IF  AMDSB-MAXRR > ZERO
                               MOVE 'RRDS' TO TS-ENTTYPE
                           ELSE
                               IF AMDSB-RCFRM NOT = X'00'
                                   SET BIT-DECODE-FROM-BYTE TO TRUE
                                   MOVE AMDSB-RCFRM TO BIT-BYTE
                                   CALL 'BITMAN' USING BITMAN-PARMS
                                   IF  BIT-0-IS-OFF
                                       MOVE 'SAME' TO TS-ENTTYPE
                                   ELSE
                                       MOVE 'SAMI' TO TS-ENTTYPE
                                   END-IF
                               END-IF
                           END-IF
                       ELSE
                           IF  TS-ENTTYPE = 'KSDS'
                               SET BIT-DECODE-FROM-BYTE TO TRUE
                               MOVE AMDSB-ATTR3  TO BIT-BYTE
                               CALL   'BITMAN'   USING BITMAN-PARMS
                               IF  BIT-2-IS-ON
                                   MOVE  'XLDS'  TO TS-ENTTYPE
                               END-IF
                           END-IF
                       END-IF
                       MOVE AMDSB-NEXT TO TS-EXTENTS
                       IF  AMDSB-STMST NOT = LOW-VALUES
                           MOVE LOW-VALUES TO EIGHT-BYTES
      * ignore unused rightmost 8 bits
                           MOVE AMDSB-STMST(1:7)
                                  TO EIGHT-BYTES(2:7)
                           PERFORM P60-DECODE-TIMESTAMP
                               THRU P65-EXIT-DECODE
                           MOVE NBR-DATE  TO TS-LASTUPD
                           MOVE NBR-SHORT TO TS-LASTUPT
                       END-IF
                       IF  AMDSB-NLR > ZERO
                           MOVE AMDSB-NLR TO TS-RECOUNT
                       ELSE
                           IF (TS-ENTTYPE = 'SAME' OR 'SAMI')
                           AND AMDSB-LRECL > ZERO
                               MOVE LOW-VALUES TO EIGHT-BYTES
                               MOVE CAT-D-HURBADS TO FOUR-BYTES
                               COMPUTE TS-RECOUNT = DOUBLE-WORD /
                                                    AMDSB-LRECL
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

           EXEC CICS WRITEQ TS
                     QUEUE(TS-QUEUE)
                     FROM(TS-RECD)
                     MAIN
           END-EXEC.

       P33-EXIT-WRITE.

           MOVE SAVE-CAT-KEY TO CAT-HI-KEY.

           EXEC CICS READ
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     INTO(CATALOG-RECORD)
                     NOHANDLE
           END-EXEC.

       P34-FREEMAIN.

           EXEC CICS FREEMAIN
                     DATA(CATALOG-RECORD)
           END-EXEC.

           PERFORM P90-CLOSE-CATALOG THRU P95-EXIT-CLOSE.

       P35-EXIT-LIST-BUILD.
           EXIT.

       P40-T2-DETL-BUILD.

           EXEC CICS SEND
                     MAP(GEN2WIN)
                     MAPSET(VCA2MS)
                     MAPONLY
                     TERMINAL
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS DELETEQ TS
                     QUEUE(T2-QUEUE)
                     NOHANDLE
           END-EXEC.

           EXEC CICS READQ TS
                     QUEUE(TS-QUEUE)
                     ITEM(TS-ITEM)
                     INTO(TS-RECD)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO DETL-MESSAGE-H
               MOVE UNEX-MSG TO DETL-MESSAGE-O
               GO TO P45-EXIT-DETL-BUILD
           END-IF.

           PERFORM P80-OPEN-CATALOG THRU P85-EXIT-OPEN.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO DETL-MESSAGE-H
               MOVE UNEX-MSG TO DETL-MESSAGE-O
               GO TO P45-EXIT-DETL-BUILD
           END-IF.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF CATALOG-RECORD)
                     LENGTH(LENGTH OF CATALOG-RECORD)
                     INITIMG(LOVALUE)
           END-EXEC.

           MOVE LOW-VALUES   TO CAT-LO-KEY.
           MOVE TS-CINO      TO CAT-LO-CINO.
           EXEC CICS READ
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     INTO(CATALOG-RECORD)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  CAT-LO-CINO NOT = TS-CINO
               IF  CAT-LO-CINO NOT = TS-CINO
                   MOVE DFHRESP(NOTFND) TO EIBRESP
               END-IF
               MOVE    -2    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR
               MOVE EXHREVRS TO DETL-MESSAGE-H
               MOVE UNEX-MSG TO DETL-MESSAGE-O
               GO TO P44-FREEMAIN
           END-IF.

           EVALUATE TS-RECTYPE
           WHEN 'C'
               PERFORM T00-PARSE-C-GOPS THRU T09-EXIT-PARSE
           WHEN 'G'
               PERFORM T30-PARSE-G-GOPS THRU T39-EXIT-PARSE
           WHEN 'R'
               PERFORM T40-PARSE-R-GOPS THRU T49-EXIT-PARSE
           WHEN 'U'
               PERFORM T50-PARSE-U-GOPS THRU T59-EXIT-PARSE
           WHEN 'V'
               PERFORM T60-PARSE-V-GOPS THRU T69-EXIT-PARSE
           END-EVALUATE.

           INITIALIZE T2-DETL.
           MOVE 'Listing from Catalog -- ' TO T2-DETLINE(17:24).
           MOVE VCAT-CATNAME TO T2-DETLINE(41:44).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           EVALUATE TS-ENTTYPE
           WHEN 'VSPC'
               IF  CAT-V-DEVTYPE = X'60082100'
                   MOVE 'FBAV'        TO DEVTYPE
               ELSE
                   MOVE '3390'        TO DEVTYPE
               END-IF
               MOVE 'Volume --------' TO T2-DETLINE
           WHEN 'UCAT'
               MOVE 'UserCatalog ---' TO T2-DETLINE
           WHEN 'PATH'
               MOVE 'Path ----------' TO T2-DETLINE
           WHEN 'AIX '
               MOVE 'AIX -----------' TO T2-DETLINE
           WHEN OTHER
               MOVE 'Cluster -------' TO T2-DETLINE
           END-EVALUATE.
           MOVE TS-ENTNAME TO T2-DETLINE(17:44).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF GOP-SAVE)
                     LENGTH(LENGTH OF GOP-TABLE)
                     INITIMG(LOVALUE)
           END-EXEC.
           MOVE  GOP-TABLE   TO GOP-SAVE.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF GO-SAVE)
                     LENGTH(LENGTH OF GO-TABLE)
                     INITIMG(LOVALUE)
           END-EXEC.
           MOVE   GO-TABLE   TO GO-SAVE.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF REC-SAVE)
                     LENGTH(LENGTH OF CATALOG-RECORD)
                     INITIMG(LOVALUE)
           END-EXEC.
           MOVE CATALOG-RECORD TO REC-SAVE.

           EVALUATE VCAT-SUBRRTN
           WHEN 'SMAP'
               PERFORM S00-OUTPUT-SPACE-MAP THRU S90-EXIT-OUTPUT
           WHEN 'DETL'
               PERFORM R00-OUTPUT-DETAILS-LIST THRU R90-EXIT-OUTPUT
           WHEN OTHER
               PERFORM Q00-OUTPUT-BASIC-INFORMATION THRU Q90-EXIT-OUTPUT
           END-EVALUATE.

           EXEC CICS FREEMAIN
                     DATA(REC-SAVE)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(GO-SAVE)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(GOP-SAVE)
           END-EXEC.

           MOVE      +1      TO T2-ITEM.
           EXEC CICS READQ TS
                     QUEUE(T2-QUEUE)
                     ITEM(T2-ITEM)
                     INTO(T2-DETL)
                     NUMITEMS(T2-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE  ZEROES  TO T2-ITEM
               MOVE  ZEROES  TO T2-TOTL
               IF  EIBRESP = DFHRESP(QIDERR)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
               END-IF
               GO TO P44-FREEMAIN
           END-IF.

           GO TO P44-FREEMAIN.

       P42-WRITE-T2-DETL.

           EXEC CICS WRITEQ TS
                     QUEUE(T2-QUEUE)
                     FROM(T2-DETL)
                     MAIN
           END-EXEC.

           INITIALIZE T2-DETL.

       P43-EXIT-WRITE.
           EXIT.

       P44-FREEMAIN.

           EXEC CICS FREEMAIN
                     DATA(CATALOG-RECORD)
           END-EXEC.

           PERFORM P90-CLOSE-CATALOG THRU P95-EXIT-CLOSE.

       P45-EXIT-DETL-BUILD.
           EXIT.

       P50-DECODE-JULIAN-DATE.

           SET DTE-REQUEST-JULN-EDIT TO TRUE.
           IF  SAVE-YEAR < 50
               ADD 2000 TO DTE-JUYR
           ELSE
               ADD 1900 TO DTE-JUYR
           END-IF.
           CALL  'DTEMAN'  USING DTEMAN-PARMS.
           MOVE CORR DTE-GREG TO WRK-DATE.
           MOVE      NBR-DATE TO EDITTED-DATE.

       P55-EXIT-DECODE.
           EXIT.

       P60-DECODE-TIMESTAMP.

      * shift out unused rightmost 4 bits
           DIVIDE 16 INTO DOUBLE-WORD.
      * compute msec and drop them
           DIVIDE 1000000 INTO DOUBLE-WORD.
      * compute zone offset
           COMPUTE DOUBLE-WORD = DOUBLE-WORD +
               (ZONE-OFFSET * 60 * 60).
      * compute days = rem / (24*60*60)
           DIVIDE DOUBLE-WORD BY 86400
               GIVING DTE-TOTD REMAINDER FULL-WORD.
      * compute hour = rem / (60*60)
           DIVIDE FULL-WORD   BY 3600
               GIVING WRK-HOUR REMAINDER HALF-WORD.
      * compute mins = rem / (60)
           DIVIDE HALF-WORD   BY 60
               GIVING WRK-MINS REMAINDER WRK-SECS.
      * compute secs = rem
           SET DTE-REQUEST-TOTL-DAYS TO TRUE.
           ADD ADJUSTMENT-DAYS TO DTE-TOTD.
           CALL 'DTEMAN' USING DTEMAN-PARMS.
           MOVE CORR DTE-GREG TO WRK-DATE.

       P65-EXIT-DECODE.
           EXIT.

       P70-IDENTIFY-CATALOG.

           SET SYSF-VSAM-CATALOG TO TRUE.
           MOVE    SPACES    TO SYSF-CATFILE.
           EXEC CICS STARTBR
                     DATASET(DAPSYSF)
                     RIDFLD(DAPSYSF-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P75-EXIT
           END-IF.

           PERFORM WITH TEST AFTER
             UNTIL SYSF-CATFILE = LIST-CATNAME-I
                OR SYSF-CATNAME = LIST-CATNAME-I
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR NOT SYSF-VSAM-CATALOG
               EXEC CICS READNEXT
                         DATASET(DAPSYSF)
                         INTO(DAPSYSF-REC)
                         RIDFLD(DAPSYSF-KEY)
                         NOHANDLE
               END-EXEC
           END-PERFORM.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT SYSF-VSAM-CATALOG
               IF  NOT SYSF-VSAM-CATALOG
                   MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
               GO TO P75-EXIT
           END-IF.

           MOVE SYSF-CATFILE TO VCAT-CATALOG.
           MOVE SYSF-CATNAME TO VCAT-CATNAME
                                LIST-CATNAME-O.

           EXEC CICS ENDBR
                     DATASET(DAPSYSF)
                     NOHANDLE
           END-EXEC.

       P75-EXIT.
           EXIT.

       P80-OPEN-CATALOG.

           EXEC CICS INQUIRE
                     DATASET(VCAT-CATALOG)
                     OPENSTATUS(OPEN-STATUS)
                     ENABLESTATUS(ENABLE-STATUS)
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND (OPEN-STATUS NOT = DFHVALUE(OPEN)
           OR ENABLE-STATUS NOT = DFHVALUE(ENABLED))
               EXEC CICS SET
                         DATASET(VCAT-CATALOG)
                         ENABLED
                         OPEN
                         WAIT
                         NOHANDLE
               END-EXEC
           END-IF.

       P85-EXIT-OPEN.
           EXIT.

       P90-CLOSE-CATALOG.

           EXEC CICS SET
                     DATASET(VCAT-CATALOG)
                     ENABLED
                     CLOSED
                     WAIT
                     NOHANDLE
           END-EXEC.

           MOVE DFHRESP(NORMAL) TO EIBRESP.

       P95-EXIT-CLOSE.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-OUTPUT-BASIC-INFORMATION.

           MOVE      L1      TO L2.
           MOVE 'History:'   TO T2-DETLINE(6:8).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           EVALUATE TS-ENTTYPE

           WHEN 'VSPC'
               PERFORM U40-COUNT-V-GOPS THRU U49-EXIT-COUNT

               IF  CAT-LO-VERS = X'01'
                   MOVE 'Release----------------2' TO T2-DETLINE(8:25)
               ELSE
                   MOVE 'Release----------------1' TO T2-DETLINE(8:25)
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE 'Characteristics:'   TO T2-DETLINE(6:16)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE  LOW-VALUES  TO FOUR-BYTES
               MOVE CAT-V-BYTPTRK TO TWO-BYTES
               MOVE  FULL-WORD   TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               IF  DEVTYPE = 'FBAV'
                   MOVE 'Blks/Min-CA'    TO WRK-FIELD(1:11)
               ELSE
                   MOVE 'Bytes/Trk'      TO WRK-FIELD(1:9)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(8:25)

               MOVE 'DevType-------------' TO WRK-FIELD
               IF  DEVTYPE = 'FBAV'
                   MOVE 'FBAV'           TO WRK-FIELD(21:4)
               ELSE
                   MOVE '3390'           TO WRK-FIELD(21:4)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)

               IF  DEVTYPE NOT = 'FBAV'
                   MOVE CAT-V-MAXBLKSZ   TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'Max-PhyRec-Sz'  TO WRK-FIELD(1:13)
                   MOVE WRK-FIELD        TO T2-DETLINE(68:25)
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               IF  DEVTYPE = 'FBAV'
                   MULTIPLY CAT-V-TRKPCYL BY FULL-WORD
                   MOVE FULL-WORD TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'Blks/Max-CA'    TO WRK-FIELD(1:11)
               ELSE
                   MOVE CAT-V-TRKPCYL TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'Trks/Cyl'       TO WRK-FIELD(1:8)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(8:25)

               MOVE 'Volume-TimeStamp:'  TO WRK-FIELD
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)

               MOVE LOW-VALUES      TO TWO-BYTES
               MOVE CAT-V-SYSEXTDS  TO ONE-BYTE
               MOVE HALF-WORD       TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'Max-Ext/Alloc' TO WRK-FIELD(1:13)
               MOVE WRK-FIELD            TO T2-DETLINE(68:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               IF  DEVTYPE = 'FBAV'
                   MOVE  LOW-VALUES  TO FOUR-BYTES
                   MOVE CAT-V-BYTPTRK TO TWO-BYTES
                   MULTIPLY CAT-V-TRKPCYL BY FULL-WORD
                   MULTIPLY CAT-V-MAXCYLS BY FULL-WORD
                   MOVE FULL-WORD TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'Blks/Vol'       TO WRK-FIELD(1:8)
               ELSE
                   MOVE CAT-V-MAXCYLS TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'Cyls/Vol'       TO WRK-FIELD(1:8)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(8:25)

               MOVE  SPACES  TO WRK-FIELD
               MOVE LOW-VALUES TO EIGHT-BYTES
      * ignore unused rightmost 8 bits
               MOVE CAT-V-VOLTSTMP(1:7) TO EIGHT-BYTES(2:7)
               PERFORM P60-DECODE-TIMESTAMP THRU P65-EXIT-DECODE
               MOVE NBR-DATE  TO EDITTED-DATE
               MOVE NBR-SHORT TO TS-LASTUPT
               MOVE EDITTED-DATE TO WRK-FIELD(6:10)
               MOVE NBR-TIME  TO EDITTED-TIME
               INSPECT CHARACTER-TIME REPLACING ALL SPACES BY ':'
               MOVE EDITTED-TIME TO WRK-FIELD(17:8)
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)

               MOVE DATASETS-ON-VOL      TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'DataSets-on-Vol'    TO WRK-FIELD(1:15)
               MOVE WRK-FIELD            TO T2-DETLINE(68:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE  SPACES  TO WRK-FIELD
               SET HEX-DECODE-FROM-CHAR TO TRUE
               MOVE    +8    TO HEX-LENG
               MOVE CAT-V-VOLTSTMP TO HEX-CHRT
               CALL  'HEXMAN'  USING HEXMAN-PARMS
               STRING '     x''' HEX-HEXT ''''
                   DELIMITED BY SIZE INTO WRK-FIELD
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)

               MOVE DATASPACES-ON-VOL    TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'DataSpcs-on-Vol'    TO WRK-FIELD(1:15)
               MOVE WRK-FIELD            TO T2-DETLINE(68:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE LENGTH OF GO-POINTER TO LN
               PERFORM WITH TEST AFTER
                 VARYING X2 FROM 1 BY 1
                   UNTIL X2 > L2 OR TABLE-ENTRIES
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  X2 <= L2 AND TABLE-ENTRIES
                   AND (SPACE-GOP-INLINE(X2) OR SPACE-GOP-EXTREC(X2))
                       IF  SPACE-GOP-INLINE(X2)
                           COMPUTE P2 = L2 * LN + 1 + GOP-DISP(X2)
                           MOVE CAT-V-VARDTA(P2:LENGTH OF SPACE-GO)
                               TO SPACE-GO
                       ELSE
                           MOVE GOP-W-SEQU(X2) TO SQ
                           IF  GOP-CINO(X2) NOT = CAT-LO-CINO
                               MOVE LOW-VALUES   TO CAT-LO-KEY
                               MOVE GOP-CINO(X2) TO CAT-LO-CINO
                               EXEC CICS READ
                                         DATASET(VCAT-CATALOG)
                                         RIDFLD(CATALOG-KEY)
                                         INTO(CATALOG-RECORD)
                                         GTEQ
                               END-EXEC
                           END-IF
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE CAT-E-GOPCNT TO ONE-BYTE
                           MOVE  HALF-WORD   TO L1
                           PERFORM WITH TEST AFTER
                             VARYING X1 FROM 1 BY 1
                               UNTIL X1 > L1 OR TABLE-ENTRIES
                                  OR SPACE-GOP-INLINE(X1) AND
                                     GOP-W-SEQU(X1) = SQ
                               IF  X1 <= L1 AND TABLE-ENTRIES
                                   COMPUTE P1 = (X1 - 1) * LN + 1
                                   MOVE CAT-E-VARDTA(P1:LN)
                                       TO GO-POINTER(X1)
                               END-IF
                           END-PERFORM
                           COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                           MOVE CAT-E-VARDTA(P1:LENGTH OF SPACE-GO)
                               TO SPACE-GO
                       END-IF
                       PERFORM U50-PRINT-V-DETAILS THRU U59-EXIT
                       MOVE GOP-SAVE TO GOP-TABLE
                       MOVE GO-SAVE  TO GO-TABLE
                   END-IF
               END-PERFORM

           WHEN 'UCAT'
               IF  CAT-LO-VERS = X'01'
                   MOVE 'Release----------------2' TO T2-DETLINE(8:25)
               ELSE
                   MOVE 'Release----------------1' TO T2-DETLINE(8:25)
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE 'Volumes:' TO T2-DETLINE(6:8)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               PERFORM WITH TEST AFTER
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1
                      OR X1 > TABLE-ENTRIES
                   IF  X1 <= L1 AND TABLE-ENTRIES
                   AND (VOLUM-GOP-INLINE(X1) OR VOLUM-GOP-EXTREC(X1))
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       MOVE CAT-U-VARDTA(P1:LENGTH OF VOLUM-GO)
                           TO VOLUM-GO
                       IF  VOLUM-GOP-INLINE(X1)
                           MOVE 'VolSer------------' TO WRK-FIELD
                           MOVE VOLUM-VOLSER         TO WRK-FIELD(19:6)
                           MOVE WRK-FIELD            TO T2-DETLINE(8:25)
                           MOVE 'DevType-------------' TO WRK-FIELD
                           IF  VOLUM-DEVTYP = X'60082100'
                               MOVE 'FBAV'           TO WRK-FIELD(21:4)
                           ELSE
                               MOVE '3390'           TO WRK-FIELD(21:4)
                           END-IF
                           MOVE WRK-FIELD           TO T2-DETLINE(38:25)
                       ELSE
                           MOVE 'VolSer------------' TO WRK-FIELD
                           MOVE '(unkn)'             TO WRK-FIELD(19:6)
                           MOVE WRK-FIELD            TO T2-DETLINE(8:25)
                           MOVE 'DevType-------------' TO WRK-FIELD
                           MOVE '(unkn)'             TO WRK-FIELD(19:6)
                           MOVE WRK-FIELD           TO T2-DETLINE(38:25)
                       END-IF
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                   END-IF
               END-PERFORM

           WHEN 'PATH'
               PERFORM U30-PRINT-R-ATTRIBS THRU U39-EXIT

           WHEN 'AIX '
               MOVE 'Owner-Ident-------' TO WRK-FIELD
               IF  CAT-G-OWNERID = HIGH-VALUES
                   MOVE '(null)'         TO WRK-FIELD(19:6)
               ELSE
                   MOVE CAT-G-OWNERID    TO WRK-FIELD(17:8)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(8:25)
               MOVE 'Creation------'     TO WRK-FIELD
               MOVE SPACES               TO DTE-JULN
               MOVE ZEROES               TO DTE-JUYR
               MOVE CAT-G-DSETCRDT       TO DTE-YYDDD
               MOVE DTE-JUYR             TO SAVE-YEAR
               PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE
               MOVE EDITTED-DATE         TO WRK-FIELD(15:10)
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               IF  CAT-LO-VERS = X'01'
                   MOVE 'Release----------------2' TO T2-DETLINE(8:25)
               ELSE
                   MOVE 'Release----------------1' TO T2-DETLINE(8:25)
               END-IF
               MOVE 'Expiration----'     TO WRK-FIELD
               MOVE SPACES               TO DTE-JULN
               MOVE ZEROES               TO DTE-JUYR
               MOVE CAT-G-DSETEXDT       TO DTE-YYDDD
               PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE
               MOVE EDITTED-DATE         TO WRK-FIELD(15:10)
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               PERFORM WITH TEST BEFORE
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1 OR TABLE-ENTRIES
                      OR PSWRD-GOP-INLINE(X1)
                      OR PSWRD-GOP-EXTREC(X1)
               END-PERFORM

               MOVE 'Protection-----------' TO T2-DETLINE(6:25)
               IF  X1 > L1 OR TABLE-ENTRIES
                   MOVE '(null)'            TO T2-DETLINE(26:6)
               ELSE
                   IF  PSWRD-GOP-EXTREC(X1)
                       MOVE '(unkn)'        TO T2-DETLINE(26:6)
                   ELSE
                       MOVE '(yes)'         TO T2-DETLINE(27:5)
                   END-IF
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE CAT-G-RGATTR TO BIT-BYTE
               PERFORM T70-WRITE-ASSOCIATIONS THRU T79-EXIT-WRITE

               MOVE 'Attributes:'        TO T2-DETLINE(6:11)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               SET BIT-DECODE-FROM-BYTE TO TRUE
               CALL  'BITMAN'  USING BITMAN-PARMS
               IF  BIT-0-IS-ON
                   MOVE 'Upgrade'        TO T2-DETLINE(8:7)
               ELSE
                   MOVE 'NoUpgrade'      TO T2-DETLINE(8:9)
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

           WHEN OTHER
               MOVE 'Owner-Ident-------' TO WRK-FIELD
               IF  CAT-C-OWNERID = HIGH-VALUES
                   MOVE '(null)'         TO WRK-FIELD(19:6)
               ELSE
                   MOVE CAT-C-OWNERID    TO WRK-FIELD(17:8)
               END-IF
               MOVE WRK-FIELD            TO T2-DETLINE(8:25)
               MOVE 'Creation------'     TO WRK-FIELD
               MOVE SPACES               TO DTE-JULN
               MOVE ZEROES               TO DTE-JUYR
               MOVE CAT-C-DSETCRDT       TO DTE-YYDDD
               MOVE DTE-JUYR             TO SAVE-YEAR
               PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE
               MOVE EDITTED-DATE         TO WRK-FIELD(15:10)
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               IF  CAT-LO-VERS = X'01'
                   MOVE 'Release----------------2' TO T2-DETLINE(8:25)
               ELSE
                   MOVE 'Release----------------1' TO T2-DETLINE(8:25)
               END-IF
               MOVE 'Expiration----'     TO WRK-FIELD
               MOVE SPACES               TO DTE-JULN
               MOVE ZEROES               TO DTE-JUYR
               MOVE CAT-C-DSETEXDT       TO DTE-YYDDD
               PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE
               MOVE EDITTED-DATE         TO WRK-FIELD(15:10)
               MOVE WRK-FIELD            TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               PERFORM WITH TEST BEFORE
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1 OR TABLE-ENTRIES
                      OR PSWRD-GOP-INLINE(X1)
                      OR PSWRD-GOP-EXTREC(X1)
               END-PERFORM

               MOVE 'Protection-----------' TO T2-DETLINE(6:25)
               IF  X1 > L1 OR TABLE-ENTRIES
                   MOVE '(null)'            TO T2-DETLINE(26:6)
               ELSE
                   IF  PSWRD-GOP-EXTREC(X1)
                       MOVE '(unkn)'        TO T2-DETLINE(26:6)
                   ELSE
                       MOVE '(yes)'         TO T2-DETLINE(27:5)
                   END-IF
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               PERFORM T70-WRITE-ASSOCIATIONS THRU T79-EXIT-WRITE

           END-EVALUATE.

           IF  TS-ENTTYPE = 'KSDS' OR 'XLDS'
                         OR 'ESDS' OR 'RRDS'
                         OR 'SAME' OR 'SAMI'
                         OR 'AIX '
               SET DATA-NOT-LISTED TO TRUE
               SET INDEX-NOT-LISTED TO TRUE
               PERFORM WITH TEST AFTER
                 VARYING X2 FROM 1 BY 1
                   UNTIL X2 > L2 OR TABLE-ENTRIES
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  X2 <= L2 AND TABLE-ENTRIES
                   AND ASSOC-GOP-INLINE(X2)
                   AND ((GO-TYPE(X2) = 'D' AND DATA-NOT-LISTED)
                    OR  (GO-TYPE(X2) = 'I' AND INDEX-NOT-LISTED)
                    OR  (GO-TYPE(X2) = 'R'))
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                       EVALUATE GO-TYPE(X2)
                       WHEN 'D'
                           SET DATA-WAS-LISTED TO TRUE
                           MOVE 'Data ------- ' TO T2-DETLINE(4:13)
                       WHEN 'I'
                           SET INDEX-WAS-LISTED TO TRUE
                           MOVE 'Index ------ ' TO T2-DETLINE(4:13)
                       WHEN 'R'
                           MOVE 'Path ------- ' TO T2-DETLINE(4:13)
                       END-EVALUATE
                       MOVE LOW-VALUES   TO CAT-LO-KEY
                       MOVE ASSOC-CINO(X2) TO CAT-LO-CINO
                       EXEC CICS READ
                                 DATASET(VCAT-CATALOG)
                                 RIDFLD(CATALOG-KEY)
                                 INTO(CATALOG-RECORD)
                                 GTEQ
                       END-EXEC
                       MOVE CAT-C-ENTNAME TO T2-DETLINE(17:44)
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                       EVALUATE GO-TYPE(X2)
                       WHEN 'D'
                           PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                           PERFORM U00-PRINT-D-ATTRIBS THRU U09-EXIT
                       WHEN 'I'
                           PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                           PERFORM U20-PRINT-I-ATTRIBS THRU U29-EXIT
                       WHEN 'R'
                           MOVE 'History:'   TO T2-DETLINE(6:8)
                           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                           PERFORM T40-PARSE-R-GOPS THRU T49-EXIT-PARSE
                           PERFORM U30-PRINT-R-ATTRIBS THRU U39-EXIT
                       END-EVALUATE
                       MOVE GOP-SAVE TO GOP-TABLE
                       MOVE GO-SAVE  TO GO-TABLE
                   END-IF
               END-PERFORM
           END-IF.

       Q90-EXIT-OUTPUT.
           EXIT.

       R00-OUTPUT-DETAILS-LIST.

           EXEC CICS GETMAIN
                     SET(ADDRESS OF GOP-SAVE2)
                     LENGTH(LENGTH OF GOP-TABLE)
                     INITIMG(LOVALUE)
           END-EXEC.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF GO-SAVE2)
                     LENGTH(LENGTH OF GO-TABLE)
                     INITIMG(LOVALUE)
           END-EXEC.
           EXEC CICS GETMAIN
                     SET(ADDRESS OF REC-SAVE2)
                     LENGTH(LENGTH OF CATALOG-RECORD)
                     INITIMG(LOVALUE)
           END-EXEC.

           MOVE      L1      TO L2.

           EVALUATE TS-ENTTYPE

           WHEN 'VSPC'
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
               MOVE 'DataSet-Directory:' TO T2-DETLINE(6:18)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               PERFORM WITH TEST AFTER
                 UNTIL L2 = ZEROES

               MOVE CATALOG-RECORD TO REC-SAVE

               PERFORM WITH TEST AFTER
                 VARYING X2 FROM 1 BY 1
                   UNTIL X2 > L2 OR TABLE-ENTRIES
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  X2 <= L2 AND TABLE-ENTRIES
                   AND (DSDIR-GOP-INLINE(X2)
                    OR  DSDIR-GOP-EXTREC(X2))
                       PERFORM R95-RESOLVE-GOP-REFERENCE THRU R99-EXIT
                       MOVE LOW-VALUES   TO CAT-LO-KEY
                       MOVE DSDIR-DSIDNO(X1) TO CAT-LO-CINO
                       EXEC CICS READ
                                 DATASET(VCAT-CATALOG)
                                 RIDFLD(CATALOG-KEY)
                                 INTO(CATALOG-RECORD)
                                 GTEQ
                       END-EXEC
                       IF  CAT-C-TYPE = 'C'
                           PERFORM T00-PARSE-C-GOPS THRU T09-EXIT-PARSE
                           PERFORM WITH TEST BEFORE
                             VARYING X1 FROM 1 BY 1
                               UNTIL X1 > L1 OR TABLE-ENTRIES
                                  OR ASSOC-GOP-INLINE(X1)
                                 AND GO-TYPE(X1) = 'D'
                           END-PERFORM
                           MOVE LOW-VALUES TO CAT-LO-KEY
                           MOVE ASSOC-CINO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                           END-EXEC
                           PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                           PERFORM U60-COUNT-EXTENTS THRU U69-EXIT-COUNT
                           MOVE EXTENT-COUNT  TO EDITTED-NUMBER
                           INSPECT WRK-FIELD
                               REPLACING LEADING SPACES BY '-'
                           MOVE 'Extents'     TO WRK-FIELD(1:7)
                           MOVE WRK-FIELD     TO T2-DETLINE(68:25)
                           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                           MOVE GOP-SAVE TO GOP-TABLE
                           MOVE GO-SAVE  TO GO-TABLE
                           MOVE REC-SAVE TO CATALOG-RECORD
                           PERFORM R95-RESOLVE-GOP-REFERENCE
                               THRU R99-EXIT
                           MOVE LOW-VALUES   TO CAT-LO-KEY
                           MOVE DSDIR-DSIDNO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                           END-EXEC
                           PERFORM T00-PARSE-C-GOPS THRU T09-EXIT-PARSE
                           PERFORM WITH TEST BEFORE
                             VARYING X1 FROM 1 BY 1
                               UNTIL X1 > L1 OR TABLE-ENTRIES
                                  OR ASSOC-GOP-INLINE(X1)
                                 AND GO-TYPE(X1) = 'I'
                           END-PERFORM
                           MOVE LOW-VALUES TO CAT-LO-KEY
                           MOVE ASSOC-CINO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                           END-EXEC
                       END-IF
                       PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                       PERFORM U60-COUNT-EXTENTS THRU U69-EXIT-COUNT
                       MOVE EXTENT-COUNT  TO EDITTED-NUMBER
                       INSPECT WRK-FIELD
                           REPLACING LEADING SPACES BY '-'
                       MOVE 'Extents'     TO WRK-FIELD(1:7)
                       MOVE WRK-FIELD     TO T2-DETLINE(68:25)
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                       MOVE GOP-SAVE TO GOP-TABLE
                       MOVE GO-SAVE  TO GO-TABLE
                   END-IF
               END-PERFORM

               MOVE REC-SAVE TO CATALOG-RECORD

               IF  CAT-V-TYPE = 'V' AND CAT-V-EXTREC NOT = LOW-VALUES
               OR  CAT-E-TYPE = 'W' AND CAT-E-EXTREC NOT = LOW-VALUES
                   MOVE LOW-VALUES TO CAT-LO-KEY
                   IF  CAT-V-TYPE = 'V'
                       MOVE CAT-V-EXTREC TO CAT-LO-CINO
                   ELSE
                       MOVE CAT-E-EXTREC TO CAT-LO-CINO
                   END-IF
                   EXEC CICS READ
                             DATASET(VCAT-CATALOG)
                             RIDFLD(CATALOG-KEY)
                             INTO(CATALOG-RECORD)
                             GTEQ
                   END-EXEC
                   PERFORM T20-PARSE-E-GOPS THRU T29-EXIT-PARSE
                   MOVE GOP-TABLE  TO GOP-SAVE
                   MOVE  GO-TABLE  TO GO-SAVE
                   MOVE     L1     TO L2
               ELSE
                   MOVE   ZEROES   TO L2
               END-IF

               END-PERFORM

               PERFORM WITH TEST AFTER
                 UNTIL NO-ITEM-SWAPPED
                    OR EIBRESP NOT = DFHRESP(NORMAL)

                   MOVE    +6    TO T2-ITEM
                   EXEC CICS READQ
                             QUEUE(T2-QUEUE)
                             ITEM(T2-ITEM)
                             INTO(S2-DETL)
                             NUMITEMS(T2-TOTL)
                             NOHANDLE
                   END-EXEC
                   ADD     1     TO T2-ITEM
                   SET NO-ITEM-SWAPPED TO TRUE

                   PERFORM WITH TEST BEFORE
                     VARYING T2-ITEM FROM T2-ITEM BY 1
                       UNTIL T2-ITEM > T2-TOTL
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                       EXEC CICS READQ
                                 QUEUE(T2-QUEUE)
                                 ITEM(T2-ITEM)
                                 INTO(T2-DETL)
                                 NOHANDLE
                       END-EXEC
                       IF  EIBRESP = DFHRESP(NORMAL)
                           IF  T2-DETLINE(17:48) NOT < S2-DETLINE(17:48)
                               MOVE T2-DETL TO S2-DETL
                           ELSE
                               SUBTRACT 1 FROM T2-ITEM GIVING SQ
                               EXEC CICS WRITEQ TS
                                         QUEUE(T2-QUEUE)
                                         ITEM(SQ)
                                         FROM(T2-DETL)
                                         REWRITE
                                         MAIN
                               END-EXEC
                               EXEC CICS WRITEQ TS
                                         QUEUE(T2-QUEUE)
                                         ITEM(T2-ITEM)
                                         FROM(S2-DETL)
                                         REWRITE
                                         MAIN
                               END-EXEC
                               SET AN-ITEM-SWAPPED TO TRUE
                           END-IF
                       END-IF
                   END-PERFORM

               END-PERFORM

           WHEN OTHER
               SET DATA-NOT-LISTED TO TRUE
               SET INDEX-NOT-LISTED TO TRUE
               PERFORM WITH TEST AFTER
                 VARYING X2 FROM 1 BY 1
                   UNTIL X2 > L2 OR TABLE-ENTRIES
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  X2 <= L2 AND TABLE-ENTRIES
                   AND ASSOC-GOP-INLINE(X2)
                   AND ((GO-TYPE(X2) = 'D' AND DATA-NOT-LISTED)
                    OR  (GO-TYPE(X2) = 'I' AND INDEX-NOT-LISTED))
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                       MOVE LOW-VALUES   TO CAT-LO-KEY
                       MOVE ASSOC-CINO(X2) TO CAT-LO-CINO
                       EXEC CICS READ
                                 DATASET(VCAT-CATALOG)
                                 RIDFLD(CATALOG-KEY)
                                 INTO(CATALOG-RECORD)
                                 GTEQ
                       END-EXEC
                       EVALUATE GO-TYPE(X2)
                       WHEN 'D'
                           SET DATA-WAS-LISTED TO TRUE
                           SET PROCESSING-DATA-COMPONENT TO TRUE
                           MOVE 'Data ------- ' TO T2-DETLINE(4:13)
                       WHEN 'I'
                           SET INDEX-WAS-LISTED TO TRUE
                           SET PROCESSING-INDEX-COMPONENT TO TRUE
                           MOVE 'Index ------ ' TO T2-DETLINE(4:13)
                       END-EVALUATE
                       MOVE CAT-D-ENTNAME TO T2-DETLINE(17:44)
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                       PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE
                       PERFORM U70-PRINT-D-DETAILS THRU U79-EXIT
                       MOVE GOP-SAVE TO GOP-TABLE
                       MOVE GO-SAVE  TO GO-TABLE
                       MOVE REC-SAVE TO CATALOG-RECORD
                   END-IF
               END-PERFORM

           END-EVALUATE.

           EXEC CICS FREEMAIN
                     DATA(REC-SAVE2)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(GO-SAVE2)
           END-EXEC.
           EXEC CICS FREEMAIN
                     DATA(GOP-SAVE2)
           END-EXEC.

       R90-EXIT-OUTPUT.
           EXIT.

       R95-RESOLVE-GOP-REFERENCE.
           IF  DSDIR-GOP-INLINE(X2)
               MOVE REC-SAVE TO CATALOG-RECORD
               MOVE X2 TO X1
           ELSE
               MOVE GOP-W-SEQU(X2) TO SQ
               MOVE LOW-VALUES   TO CAT-LO-KEY
               MOVE GOP-CINO(X2) TO CAT-LO-CINO
               EXEC CICS READ
                         DATASET(VCAT-CATALOG)
                         RIDFLD(CATALOG-KEY)
                         INTO(CATALOG-RECORD)
                         GTEQ
               END-EXEC
               MOVE LENGTH OF GO-POINTER TO LN
               MOVE  LOW-VALUES  TO TWO-BYTES
               MOVE CAT-E-GOPCNT TO ONE-BYTE
               MOVE  HALF-WORD   TO L1
               PERFORM WITH TEST AFTER
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1 OR TABLE-ENTRIES
                      OR DSDIR-GOP-INLINE(X1) AND
                         GOP-W-SEQU(X1) = SQ
                   IF  X1 <= L1 AND TABLE-ENTRIES
                       COMPUTE P1 = (X1 - 1) * LN + 1
                       MOVE CAT-E-VARDTA(P1:LN)
                           TO GO-POINTER(X1)
                   END-IF
               END-PERFORM
               COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
               MOVE  '8'   TO GO-TYPE(X1)
               MOVE CAT-E-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                   TO GO-DATA(X1)
               MOVE LOW-VALUES TO TWO-BYTES
               MOVE DSDIR-VOFF(X1) TO ONE-BYTE
               COMPUTE GO-LENG(X1) = HALF-WORD + 3
           END-IF.
       R99-EXIT.
           EXIT.

       S00-OUTPUT-SPACE-MAP.

           MOVE     L1      TO L2.
           MOVE 'Space Map: ' TO T2-DETLINE(6:11).
           MOVE '(Legend: X = used, . = free, each ' TO T2-DETLINE(20:).
           MOVE 'position = 1 CKD-track / 64 FBA-blocks.)'
                                                     TO T2-DETLINE(54:).
           MOVE     'X'      TO T2-DETLINE(29:1).
           MOVE    X'B3'     TO T2-DETLINE(39:1).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           COMPUTE MAX-ALLOC = CAT-V-MAXCYLS * CAT-V-TRKPCYL.
           MOVE    ZEROES    TO P3
                                LINE-COUNT.

           PERFORM WITH TEST AFTER
             VARYING X2 FROM 1 BY 1
               UNTIL X2 > L2 OR TABLE-ENTRIES
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               IF  X2 <= L2 AND TABLE-ENTRIES
               AND SPMAP-GOP-EXTREC(X2)
                   MOVE LOW-VALUES   TO CAT-LO-KEY
                   MOVE GOP-CINO(X2) TO CAT-LO-CINO
                   EXEC CICS READ
                             DATASET(VCAT-CATALOG)
                             RIDFLD(CATALOG-KEY)
                             INTO(CATALOG-RECORD)
                             GTEQ
                   END-EXEC
                   PERFORM T20-PARSE-E-GOPS THRU T29-EXIT-PARSE
                   PERFORM WITH TEST AFTER
                     VARYING X1 FROM 1 BY 1
                       UNTIL X1 > L1 OR TABLE-ENTRIES
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                       IF  SPMAP-GOP-INLINE(X1)
                           COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                           MOVE CAT-E-VARDTA(P1:LENGTH OF SPMAP-GO)
                               TO SPMAP-GO
                           PERFORM S60-OUTPUT-BIT-INFO THRU S69-EXIT
                       END-IF
                   END-PERFORM
                   MOVE GOP-SAVE TO GOP-TABLE
                   MOVE GO-SAVE  TO GO-TABLE
               END-IF
           END-PERFORM.

           IF  P3 > ZERO
               ADD    P3    TO LINE-COUNT
               MOVE LINE-COUNT TO T2-LNCOUNT
               IF  P3 < 100
                   ADD     1     TO P3
                   MOVE  X'B6'   TO T2-DETLINE(P3:1)
               END-IF
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
               MOVE  ZEROES  TO P3
                                LINE-COUNT
           END-IF.

           GO TO S90-EXIT-OUTPUT.

       S60-OUTPUT-BIT-INFO.

           SET BIT-DECODE-FROM-BYTE TO TRUE.

           PERFORM WITH TEST AFTER
             VARYING ROW FROM 1 BY 1
               UNTIL ROW > SPMAP-LEN
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               IF  ROW NOT > SPMAP-LEN
                   MOVE SPMAP-VARDTA(ROW:1) TO BIT-BYTE
                   CALL 'BITMAN' USING BITMAN-PARMS
                   PERFORM WITH TEST AFTER
                     VARYING COL FROM 1 BY 1
                       UNTIL COL > 8
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                       IF  COL NOT > 8
                       AND (P3 + LINE-COUNT) < MAX-ALLOC
                           ADD 1 TO P3
                           IF  BIT-IS-OFF(COL)
                               MOVE  'X'  TO T2-DETLINE(P3:1)
                           ELSE
                               MOVE X'B3' TO T2-DETLINE(P3:1)
                           END-IF
                           IF  P3 = 100
                               ADD P3 TO LINE-COUNT
                               MOVE LINE-COUNT TO T2-LNCOUNT
                               PERFORM P42-WRITE-T2-DETL
                                   THRU P43-EXIT-WRITE
                               MOVE  ZEROES  TO P3
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       S69-EXIT.
           EXIT.

       S90-EXIT-OUTPUT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       T00-PARSE-C-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-C-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-C-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-C-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN OTHER
                           MOVE CAT-C-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                           IF  GO-TYPE(X1) = 'I'
                               SET INDEX-ASSOC-WAS-FOUND TO TRUE
                           END-IF
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T09-EXIT-PARSE.
           EXIT.

       T10-PARSE-D-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-D-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-D-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN AMDSB-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF AMDSB-GO + 3
                           MOVE '1' TO GO-TYPE(X1)
                           MOVE CAT-D-VARDTA(P1:LENGTH OF AMDSB-GO)
                               TO AMDSB-GO
                       WHEN VOLUM-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF VOLUM-GO + 3
                           MOVE '3' TO GO-TYPE(X1)
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-D-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN OTHER
                           MOVE CAT-D-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T19-EXIT-PARSE.
           EXIT.

       T20-PARSE-E-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-E-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           IF  CAT-E-TYPE = 'W'
               MOVE LENGTH OF GO-POINTER TO LN
           ELSE
               MOVE    5     TO LN
           END-IF.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-E-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN AMDSB-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF AMDSB-GO + 3
                           MOVE '1' TO GO-TYPE(X1)
                           MOVE CAT-E-VARDTA(P1:LENGTH OF AMDSB-GO)
                               TO AMDSB-GO
                       WHEN VOLUM-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF VOLUM-GO + 3
                           MOVE '3' TO GO-TYPE(X1)
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-E-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN SPMAP-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF SPMAP-GO + 3
                           MOVE '5' TO GO-TYPE(X1)
                       WHEN SPACE-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF SPACE-GO + 3
                           MOVE '6' TO GO-TYPE(X1)
                       WHEN DSDIR-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF GO-DATA  + 3
                           MOVE '8' TO GO-TYPE(X1)
                           MOVE CAT-E-VARDTA(P1:LENGTH OF GO-DATA)
                                    TO GO-DATA(X1)
                       WHEN OTHER
                           MOVE CAT-E-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T29-EXIT-PARSE.
           EXIT.

       T30-PARSE-G-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-G-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-G-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-G-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN OTHER
                           MOVE CAT-G-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T39-EXIT-PARSE.
           EXIT.

       T40-PARSE-R-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-R-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-R-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-R-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN OTHER
                           MOVE CAT-R-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T49-EXIT-PARSE.
           EXIT.

       T50-PARSE-U-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-U-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-U-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN VOLUM-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF VOLUM-GO + 3
                           MOVE '3' TO GO-TYPE(X1)
                           MOVE CAT-U-VARDTA(P1:LENGTH OF VOLUM-GO)
                               TO VOLUM-GO
                       WHEN PSWRD-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF PSWRD-GO + 3
                           MOVE '4' TO GO-TYPE(X1)
                           MOVE CAT-U-VARDTA(P1:LENGTH OF PSWRD-GO)
                               TO PSWRD-GO
                       WHEN OTHER
                           MOVE CAT-U-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T59-EXIT-PARSE.
           EXIT.

       T60-PARSE-V-GOPS.

           MOVE  LOW-VALUES  TO GOP-TABLE.
           MOVE  LOW-VALUES  TO GO-TABLE.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-V-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           MOVE LENGTH OF GO-POINTER TO LN.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR X1 > TABLE-ENTRIES
               IF  X1 <= L1 AND TABLE-ENTRIES
                   COMPUTE P1 = (X1 - 1) * LN + 1
                   MOVE CAT-V-VARDTA(P1:LN) TO GO-POINTER(X1)
                   IF  GOP-TYPE(X1) < X'40'
                       COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       EVALUATE TRUE
                       WHEN SPMAP-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF SPMAP-GO + 3
                           MOVE '5' TO GO-TYPE(X1)
                       WHEN SPACE-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF SPACE-GO + 3
                           MOVE '6' TO GO-TYPE(X1)
                       WHEN DSDIR-GOP-INLINE(X1)
                           COMPUTE GO-LENG(X1) = LENGTH OF GO-DATA  + 3
                           MOVE '8' TO GO-TYPE(X1)
                           MOVE CAT-V-VARDTA(P1:LENGTH OF GO-DATA)
                                    TO GO-DATA(X1)
                       WHEN OTHER
                           MOVE CAT-V-VARDTA(P1:LENGTH OF G-OCCURRENCE)
                               TO G-OCCURRENCE(X1)
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       T69-EXIT-PARSE.
           EXIT.

       T70-WRITE-ASSOCIATIONS.

           MOVE 'Associations:'         TO T2-DETLINE(6:13).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           PERFORM WITH TEST AFTER
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR EIBRESP NOT = DFHRESP(NORMAL)
               IF  X1 <= L1 AND TABLE-ENTRIES
               AND (ASSOC-GOP-INLINE(X1) OR ASSOC-GOP-EXTREC(X1))
                   IF  ASSOC-GOP-INLINE(X1)
                       IF  GO-TYPE(X1) NOT = 'Y'
                           EVALUATE GO-TYPE(X1)
                           WHEN 'G'
                               MOVE 'AIX------' TO T2-DETLINE(8:9)
                           WHEN 'C'
                               MOVE 'Cluster--' TO T2-DETLINE(8:9)
                           WHEN 'D'
                               MOVE 'Data-----' TO T2-DETLINE(8:9)
                           WHEN 'I'
                               MOVE 'Index----' TO T2-DETLINE(8:9)
                           WHEN 'R'
                               MOVE 'Path-----' TO T2-DETLINE(8:9)
                           WHEN OTHER
                               MOVE 'Other----' TO T2-DETLINE(8:9)
                           END-EVALUATE
                           MOVE LOW-VALUES   TO CAT-LO-KEY
                           MOVE ASSOC-CINO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                                     NOHANDLE
                           END-EXEC
                           MOVE CAT-C-ENTNAME TO T2-DETLINE(17:44)
                       END-IF
                   ELSE
                       MOVE 'ExtRec---' TO T2-DETLINE(8:9)
                   END-IF
                   PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
               END-IF
           END-PERFORM.

       T79-EXIT-WRITE.
           EXIT.

       T80-WRITE-STATISTICS.

           MOVE 'Statistics:' TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-NLR    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Rec-Total'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE AMDSB-NCIS   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Splits-CI'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-EXCP   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'EXCPs'      TO WRK-FIELD(1:5).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-DELR   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Rec-Deleted' TO WRK-FIELD(1:11).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE AMDSB-NCAS   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Splits-CA'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-NEXT   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Extents'    TO WRK-FIELD(1:7).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-IREC   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Rec-Inserted' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE AMDSB-PCTCI  TO ONE-BYTE.
           MOVE HALF-WORD    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Freespace-%CI' TO WRK-FIELD(1:13).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE 'System-TimeStamp:' TO WRK-FIELD.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-UPR    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Rec-Updated' TO WRK-FIELD(1:11).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE  LOW-VALUES  TO TWO-BYTES.
           IF  TS-ENTTYPE NOT = 'SAME' AND 'SAMI'
               MOVE AMDSB-PCTCA TO ONE-BYTE
           END-IF.
           MOVE HALF-WORD    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Freespace-%CA' TO WRK-FIELD(1:13).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE    SPACES    TO WRK-FIELD.
           IF  AMDSB-STMST NOT = LOW-VALUES
               MOVE LOW-VALUES TO EIGHT-BYTES
      * ignore unused rightmost 8 bits
               MOVE AMDSB-STMST(1:7) TO EIGHT-BYTES(2:7)
               PERFORM P60-DECODE-TIMESTAMP THRU P65-EXIT-DECODE
           ELSE
               MOVE  ZEROES  TO NBR-DATE
               MOVE  ZEROES  TO NBR-TIME
           END-IF.
           MOVE NBR-DATE     TO EDITTED-DATE.
           MOVE EDITTED-DATE TO WRK-FIELD(6:10).
           MOVE NBR-TIME     TO EDITTED-TIME.
           INSPECT CHARACTER-TIME REPLACING ALL SPACES BY ':'.
           MOVE EDITTED-TIME TO WRK-FIELD(17:8).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-RETR   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Rec-Retrievd' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE  LOW-VALUES  TO EIGHT-BYTES.
           MOVE AMDSB-ASPA   TO FOUR-BYTES.
           MOVE DOUBLE-WORD  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Freespace'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE    SPACES    TO WRK-FIELD.
           SET HEX-DECODE-FROM-CHAR TO TRUE.
           MOVE      +8      TO HEX-LENG.
           MOVE AMDSB-STMST  TO HEX-CHRT.
           CALL   'HEXMAN'   USING HEXMAN-PARMS.
           STRING '     x''' HEX-HEXT ''''
               DELIMITED BY SIZE INTO WRK-FIELD.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

       T89-EXIT-WRITE.
           EXIT.

       T90-WRITE-ALLOCATION.

      *    SET BIT-DECODE-FROM-BYTE TO TRUE.
      *    MOVE AMDSB-ATTR3  TO BIT-BYTE.
      *    CALL   'BITMAN'   USING BITMAN-PARMS.
      *    IF  BIT-2-IS-ON
      *        SET IS-EXTRALARGE-DATASET TO TRUE
      *    ELSE
      *        SET IS-NORMALVSAM-DATASET TO TRUE
      *    END-IF.

           MOVE 'Allocation:'     TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-SPACOPTN TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           MOVE    SPACES    TO WRK-FIELD.
           EVALUATE TRUE     ALSO     TRUE
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-ON
               MOVE 'Cylinder' TO WRK-FIELD(17:8)
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-OFF
               MOVE 'Track'    TO WRK-FIELD(20:5)
           WHEN OTHER
               MOVE 'Block'    TO WRK-FIELD(20:5)
           END-EVALUATE.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Space-Type' TO WRK-FIELD(1:10).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE  LOW-VALUES  TO FOUR-BYTES.
           MOVE CAT-D-PRIMSPAC TO THREE-BYTES.
           MOVE  FULL-WORD   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Space-Pri'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           SET BIT-ENCODE-FROM-BITS TO TRUE.
           MOVE '00000'      TO BIT-DATA(1:5).
           CALL   'BITMAN'   USING BITMAN-PARMS.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE   BIT-BYTE   TO ONE-BYTE.
           MOVE  HALF-WORD   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'UseClass-Pri' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE  LOW-VALUES  TO EIGHT-BYTES.
           MOVE CAT-D-HARBADS TO FOUR-BYTES.
           MOVE DOUBLE-WORD  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
      *    IF  IS-EXTRALARGE-DATASET
           IF  TS-ENTTYPE = 'XLDS'
           AND CAT-D-TYPE = 'D'
               MOVE 'HiAlc-CI' TO WRK-FIELD(1:8)
           ELSE
               MOVE 'HiAlcRBA' TO WRK-FIELD(1:8)
           END-IF.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE  LOW-VALUES  TO FOUR-BYTES.
           MOVE CAT-D-SCONSPAC TO THREE-BYTES.
           MOVE  FULL-WORD   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Space-Sec'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-SPACOPTN TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           IF  BIT-3-IS-ON
               MOVE  ZEROES  TO EDITTED-NUMBER
           ELSE
               SET BIT-ENCODE-FROM-BITS TO TRUE
               MOVE '00000'    TO BIT-DATA(1:5)
               CALL  'BITMAN'  USING BITMAN-PARMS
               MOVE LOW-VALUES TO TWO-BYTES
               MOVE  BIT-BYTE  TO ONE-BYTE
               MOVE HALF-WORD  TO EDITTED-NUMBER
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'UseClass-Sec' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE  LOW-VALUES  TO EIGHT-BYTES.
           MOVE CAT-D-HURBADS TO FOUR-BYTES.
           MOVE DOUBLE-WORD  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
      *    IF  IS-EXTRALARGE-DATASET
           IF  TS-ENTTYPE = 'XLDS'
           AND CAT-D-TYPE = 'D'
               MOVE 'HiUse-CI' TO WRK-FIELD(1:8)
           ELSE
               MOVE 'HiUseRBA' TO WRK-FIELD(1:8)
           END-IF.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).

           IF  CAT-D-HARBADS = LOW-VALUES
               MOVE  ZEROES  TO EDITTED-NUMBER
           ELSE
               MOVE  LOW-VALUES  TO EIGHT-BYTES
               MOVE CAT-D-HARBADS TO FOUR-BYTES
               MOVE DOUBLE-WORD  TO HIGH-ALLOC-RBA
               MOVE  LOW-VALUES  TO EIGHT-BYTES
               MOVE CAT-D-HURBADS TO FOUR-BYTES
               COMPUTE EDITTED-NUMBER ROUNDED =
                   ((DOUBLE-WORD / HIGH-ALLOC-RBA) * 100.00)
           END-IF.
           MOVE    ZEROES    TO TALLY.
           INSPECT WRK-FIELD TALLYING TALLY
               FOR LEADING SPACES.
           ADD       1       TO TALLY.
           SUBTRACT TALLY    FROM 25 GIVING LN.
           STRING ' (' WRK-FIELD(TALLY:LN) '%)'
               DELIMITED BY SIZE INTO WRK-FIELD.
           MOVE WRK-FIELD    TO T2-DETLINE(92:6).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

       T99-EXIT-WRITE.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       U00-PRINT-D-ATTRIBS.

           MOVE 'History:'   TO T2-DETLINE(6:8).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE 'Owner-Ident-------' TO WRK-FIELD.
           IF  CAT-D-OWNERID = HIGH-VALUES
               MOVE '(null)'         TO WRK-FIELD(19:6)
           ELSE
               MOVE CAT-D-OWNERID    TO WRK-FIELD(17:8)
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).
           MOVE 'Creation------'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-D-DSETCRDT       TO DTE-YYDDD.
           MOVE DTE-JUYR             TO SAVE-YEAR.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           IF  CAT-LO-VERS = X'01'
               MOVE 'Release----------------2' TO T2-DETLINE(8:25)
           ELSE
               MOVE 'Release----------------1' TO T2-DETLINE(8:25)
           END-IF.
           MOVE 'Expiration----'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-D-DSETEXDT       TO DTE-YYDDD.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-D-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1
                                LN.
           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR PSWRD-GOP-INLINE(X1)
                  OR PSWRD-GOP-EXTREC(X1)
           END-PERFORM.

           MOVE 'Protection-----------' TO T2-DETLINE(6:25).
           IF  X1 > L1 OR TABLE-ENTRIES
               MOVE '(null)'            TO T2-DETLINE(26:6)
           ELSE
               IF  PSWRD-GOP-EXTREC(X1)
                   MOVE '(unkn)'        TO T2-DETLINE(26:6)
               ELSE
                   MOVE '(yes)'         TO T2-DETLINE(27:5)
               END-IF
           END-IF.
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           PERFORM T70-WRITE-ASSOCIATIONS THRU T79-EXIT-WRITE.

           MOVE   GOP-SAVE   TO GOP-TABLE.
           MOVE   GO-SAVE    TO GO-TABLE.
           MOVE LOW-VALUES   TO CAT-LO-KEY.
           MOVE ASSOC-CINO(X2) TO CAT-LO-CINO.
           EXEC CICS READ
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     INTO(CATALOG-RECORD)
                     GTEQ
           END-EXEC.

           PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE.

           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR AMDSB-GOP-INLINE(X1)
                  OR AMDSB-GOP-EXTREC(X1)
           END-PERFORM.

           MOVE 'Attributes:'        TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-KEYLN  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'KeyLen'     TO WRK-FIELD(1:6).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE CAT-D-LRECL  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'AvgLRecL'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE CAT-D-BUFSIZE TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'BufSpace'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-RKP    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'RKP'        TO WRK-FIELD(1:3).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE AMDSB-LRECL  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'MaxLRecL'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE SPACES TO WRK-FIELD.
           IF  CAT-D-EXCPEXIT = HIGH-VALUES
               MOVE '(null)' TO WRK-FIELD(19:6)
           ELSE
               MOVE CAT-D-EXCPEXIT TO WRK-FIELD(17:8)
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'EXCPExit'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           IF  TS-ENTTYPE = 'SAME' OR 'SAMI'
               MOVE AMDSB-BLREC  TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'SAMLRecL'   TO WRK-FIELD(1:8)
               MOVE WRK-FIELD    TO T2-DETLINE(8:25)
           ELSE
               IF  AMDSB-AXRKP > ZERO
                   MOVE AMDSB-AXRKP  TO EDITTED-NUMBER
                   INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
                   MOVE 'AXRKP'      TO WRK-FIELD(1:5)
                   MOVE WRK-FIELD    TO T2-DETLINE(8:25)
               END-IF
           END-IF.

           MOVE AMDSB-CINV   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'CISize'     TO WRK-FIELD(1:6).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-CIPCA  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'CI/CA'      TO WRK-FIELD(1:5).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           IF  TS-ENTTYPE = 'SAME' OR 'SAMI'
               MOVE  SPACES  TO WRK-FIELD
               SET BIT-DECODE-FROM-BYTE TO TRUE
               MOVE AMDSB-RCFRM      TO BIT-BYTE
               CALL  'BITMAN'  USING BITMAN-PARMS
               IF  BIT-6-IS-ON
                   IF  BIT-5-IS-ON
                       MOVE 'VarBlk' TO WRK-FIELD(19:6)
                   ELSE
                       MOVE 'VarUnb' TO WRK-FIELD(19:6)
                   END-IF
               ELSE
                   IF  BIT-7-IS-ON
                       IF  BIT-5-IS-ON
                           MOVE 'FixBlk' TO WRK-FIELD(19:6)
                       ELSE
                           MOVE 'FixUnb' TO WRK-FIELD(19:6)
                       END-IF
                   ELSE
                       MOVE 'Undef' TO WRK-FIELD(20:5)
                   END-IF
               END-IF
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'RecFormat'  TO WRK-FIELD(1:9)
               MOVE WRK-FIELD    TO T2-DETLINE(8:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
           END-IF.

           MOVE    ZEROES    TO X3.
           MOVE HIGH-VALUES  TO ATTRS-TABLE.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-ATTR2       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           MOVE 'ShrOptn(x,x)'    TO ATTRS-ENTRY(X3).
           EVALUATE TRUE     ALSO     TRUE
           WHEN BIT-0-IS-OFF ALSO BIT-1-IS-OFF
               MOVE '1' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-OFF ALSO BIT-1-IS-ON
               MOVE '2' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-OFF
               MOVE '3' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-ON
               MOVE '4' TO ATTRS-ENTRY(X3)(9:1)
           END-EVALUATE.
           EVALUATE TRUE     ALSO     TRUE
           WHEN BIT-2-IS-OFF ALSO BIT-3-IS-OFF
               MOVE '1' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-OFF ALSO BIT-3-IS-ON
               MOVE '2' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-ON  ALSO BIT-3-IS-OFF
               MOVE '3' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-ON  ALSO BIT-3-IS-ON
               MOVE '4' TO ATTRS-ENTRY(X3)(11:1)
           END-EVALUATE.

           IF  BIT-4-IS-ON
               SET NO-ALLOCATION-DATASET TO TRUE
           ELSE
               SET IS-ALLOCATION-DATASET TO TRUE
           END-IF.

           MOVE CAT-D-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           IF  BIT-0-IS-ON
               MOVE 'Speed'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'Recovery'    TO ATTRS-ENTRY(X3)
           END-IF.
           ADD       1       TO X3.
           IF  NO-ALLOCATION-DATASET
               MOVE 'NoAlloc'     TO ATTRS-ENTRY(X3)
           ELSE
           IF  BIT-1-IS-ON
               MOVE 'Unique'      TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'SubAlloc'    TO ATTRS-ENTRY(X3)
           END-IF.
           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Erase'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoErase'     TO ATTRS-ENTRY(X3)
           END-IF.

           MOVE AMDSB-ATTR3       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Compress'    TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoCompress'  TO ATTRS-ENTRY(X3)
           END-IF.
           IF  BIT-2-IS-ON
               ADD       1        TO X3
               MOVE 'ExtraLarge'  TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  TS-ENTTYPE = 'KSDS' OR 'XLDS' OR 'AIX '
               MOVE 'Indexed'     TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NonIndexed'  TO ATTRS-ENTRY(X3)
               IF  TS-ENTTYPE = 'SAME' OR 'SAMI'
                   ADD       1       TO X3
                   MOVE 'SAMDataset' TO ATTRS-ENTRY(X3)
                   ADD       1       TO X3
                   IF  TS-ENTTYPE = 'SAME'
                       MOVE 'Exp-Define' TO ATTRS-ENTRY(X3)
                   ELSE
                       MOVE 'Imp-Define' TO ATTRS-ENTRY(X3)
                   END-IF
               ELSE
                   ADD       1       TO X3
                   MOVE 'VSAMDataset' TO ATTRS-ENTRY(X3)
               END-IF
           END-IF.

           MOVE AMDSB-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           IF  BIT-1-IS-ON
               MOVE 'WriteChk'    TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoWriteChk'  TO ATTRS-ENTRY(X3)
           END-IF.
           ADD       1       TO X3.
           IF  BIT-2-IS-ON
               MOVE 'Imbed'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoImbed'     TO ATTRS-ENTRY(X3)
           END-IF.
           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Replicate'   TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoReplicate' TO ATTRS-ENTRY(X3)
           END-IF.
           ADD       1       TO X3.
           IF  BIT-4-IS-ON
               MOVE 'Ordered'     TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'UnOrdered'   TO ATTRS-ENTRY(X3)
           END-IF.

           MOVE CAT-D-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           IF  BIT-2-IS-ON
               MOVE 'Reuse'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoReuse'     TO ATTRS-ENTRY(X3)
           END-IF.

           MOVE AMDSB-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           ADD       1       TO X3.
           IF  BIT-7-IS-ON
               MOVE 'Spanned'     TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NonSpanned'  TO ATTRS-ENTRY(X3)
           END-IF.

           IF  TS-ENTTYPE = 'AIX '
               ADD       1        TO X3
               MOVE AMDSB-ATTR3       TO BIT-BYTE
               CALL   'BITMAN'   USING BITMAN-PARMS
               IF  BIT-0-IS-ON
                   MOVE 'NonUniqKey'  TO ATTRS-ENTRY(X3)
               ELSE
                   MOVE 'UniqueKey'   TO ATTRS-ENTRY(X3)
               END-IF
           END-IF.

           IF  CAT-D-DSETEXDT = 99365
               ADD       1        TO X3
               MOVE 'NeverExpire' TO ATTRS-ENTRY(X3)
           END-IF.

           PERFORM U10-WRITE-ATTRS-TABLE THRU U19-EXIT-WRITE.
           PERFORM T80-WRITE-STATISTICS THRU T89-EXIT-WRITE.
           PERFORM T90-WRITE-ALLOCATION THRU T99-EXIT-WRITE.

       U09-EXIT.
           EXIT.

       U10-WRITE-ATTRS-TABLE.

           MOVE    SPACES    TO WRK-FIELD.

           PERFORM WITH TEST BEFORE
             VARYING X3 FROM 1 BY 1
               UNTIL X3 > 16
                  OR ATTRS-ENTRY(X3) = HIGH-VALUES
               IF  WRK-FIELD = SPACES
                   MOVE ATTRS-ENTRY(X3) TO WRK-FIELD
               ELSE
                   MOVE  +12 TO HEX-LENG
                   MOVE ATTRS-ENTRY(X3) TO HEX-CHRT
                   PERFORM P20-JUSTIFY-RIGHT THRU P25-EXIT-JUSTIFY
                   MOVE HEX-CHRT         TO WRK-FIELD(13:12)
                   EVALUATE TRUE
                   WHEN T2-DETLINE(8:1) = SPACE
                       MOVE WRK-FIELD    TO T2-DETLINE(8:25)
                   WHEN T2-DETLINE(38:1) = SPACE
                       MOVE WRK-FIELD    TO T2-DETLINE(38:25)
                   WHEN OTHER
                       MOVE WRK-FIELD    TO T2-DETLINE(68:25)
                       PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
                   END-EVALUATE
                   MOVE SPACES TO WRK-FIELD
               END-IF
           END-PERFORM.

           IF  WRK-FIELD > SPACES
               EVALUATE TRUE
               WHEN T2-DETLINE(8:1) = SPACE
                   MOVE WRK-FIELD    TO T2-DETLINE(8:25)
               WHEN T2-DETLINE(38:1) = SPACE
                   MOVE WRK-FIELD    TO T2-DETLINE(38:25)
               WHEN OTHER
                   MOVE WRK-FIELD    TO T2-DETLINE(68:25)
               END-EVALUATE
           END-IF.

           IF  T2-DETLINE > SPACES
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE
           END-IF.

       U19-EXIT-WRITE.
           EXIT.

       U20-PRINT-I-ATTRIBS.

           MOVE 'History:'   TO T2-DETLINE(6:8).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE 'Owner-Ident-------' TO WRK-FIELD.
           IF  CAT-D-OWNERID = HIGH-VALUES
               MOVE '(null)'         TO WRK-FIELD(19:6)
           ELSE
               MOVE CAT-D-OWNERID    TO WRK-FIELD(17:8)
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).
           MOVE 'Creation------'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-D-DSETCRDT       TO DTE-YYDDD.
           MOVE DTE-JUYR             TO SAVE-YEAR.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           IF  CAT-LO-VERS = X'01'
               MOVE 'Release----------------2' TO T2-DETLINE(8:25)
           ELSE
               MOVE 'Release----------------1' TO T2-DETLINE(8:25)
           END-IF.
           MOVE 'Expiration----'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-D-DSETEXDT       TO DTE-YYDDD.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-D-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1
                                LN.
           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR PSWRD-GOP-INLINE(X1)
                  OR PSWRD-GOP-EXTREC(X1)
           END-PERFORM.

           MOVE 'Protection-----------' TO T2-DETLINE(6:25).
           IF  X1 > L1 OR TABLE-ENTRIES
               MOVE '(null)'            TO T2-DETLINE(26:6)
           ELSE
               IF  PSWRD-GOP-EXTREC(X1)
                   MOVE '(unkn)'        TO T2-DETLINE(26:6)
               ELSE
                   MOVE '(yes)'         TO T2-DETLINE(27:5)
               END-IF
           END-IF.
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           PERFORM T70-WRITE-ASSOCIATIONS THRU T79-EXIT-WRITE.

           MOVE   GOP-SAVE   TO GOP-TABLE.
           MOVE   GO-SAVE    TO GO-TABLE.
           MOVE LOW-VALUES   TO CAT-LO-KEY.
           MOVE ASSOC-CINO(X2) TO CAT-LO-CINO.
           EXEC CICS READ
                     DATASET(VCAT-CATALOG)
                     RIDFLD(CATALOG-KEY)
                     INTO(CATALOG-RECORD)
                     GTEQ
           END-EXEC.

           PERFORM T10-PARSE-D-GOPS THRU T19-EXIT-PARSE.

           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR AMDSB-GOP-INLINE(X1)
                  OR AMDSB-GOP-EXTREC(X1)
           END-PERFORM.

           MOVE 'Attributes:'        TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-KEYLN  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'KeyLen'     TO WRK-FIELD(1:6).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           IF  CAT-D-LRECL = -1
               MOVE  ZEROES  TO EDITTED-NUMBER
           ELSE
               MOVE CAT-D-LRECL TO EDITTED-NUMBER
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'AvgLRecL'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           IF  CAT-D-BUFSIZE = -1
               MOVE  ZEROES  TO EDITTED-NUMBER
           ELSE
               MOVE CAT-D-BUFSIZE TO EDITTED-NUMBER
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'BufSpace'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-RKP    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'RKP'        TO WRK-FIELD(1:3).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE AMDSB-LRECL  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'MaxLRecL'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE SPACES TO WRK-FIELD.
           IF  CAT-D-EXCPEXIT = HIGH-VALUES
               MOVE '(null)' TO WRK-FIELD(19:6)
           ELSE
               MOVE CAT-D-EXCPEXIT TO WRK-FIELD(17:8)
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'EXCPExit'   TO WRK-FIELD(1:8).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-CINV   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'CISize'     TO WRK-FIELD(1:6).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-CIPCA  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'CI/CA'      TO WRK-FIELD(1:5).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    ZEROES    TO X3.
           MOVE HIGH-VALUES  TO ATTRS-TABLE.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-ATTR2       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           ADD       1       TO X3.
           MOVE 'ShrOptn(x,x)'    TO ATTRS-ENTRY(X3).
           EVALUATE TRUE     ALSO     TRUE
           WHEN BIT-0-IS-OFF ALSO BIT-1-IS-OFF
               MOVE '1' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-OFF ALSO BIT-1-IS-ON
               MOVE '2' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-OFF
               MOVE '3' TO ATTRS-ENTRY(X3)(9:1)
           WHEN BIT-0-IS-ON  ALSO BIT-1-IS-ON
               MOVE '4' TO ATTRS-ENTRY(X3)(9:1)
           END-EVALUATE.
           EVALUATE TRUE     ALSO     TRUE
           WHEN BIT-2-IS-OFF ALSO BIT-3-IS-OFF
               MOVE '1' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-OFF ALSO BIT-3-IS-ON
               MOVE '2' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-ON  ALSO BIT-3-IS-OFF
               MOVE '3' TO ATTRS-ENTRY(X3)(11:1)
           WHEN BIT-2-IS-ON  ALSO BIT-3-IS-ON
               MOVE '4' TO ATTRS-ENTRY(X3)(11:1)
           END-EVALUATE.

           IF  BIT-4-IS-ON
               SET NO-ALLOCATION-DATASET TO TRUE
           ELSE
               SET IS-ALLOCATION-DATASET TO TRUE
           END-IF.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           ADD       1       TO X3.
           IF  BIT-0-IS-ON
               MOVE 'Speed'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'Recovery'    TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  NO-ALLOCATION-DATASET
               MOVE 'NoAlloc'     TO ATTRS-ENTRY(X3)
           ELSE
           IF  BIT-1-IS-ON
               MOVE 'Unique'      TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'SubAlloc'    TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Erase'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoErase'     TO ATTRS-ENTRY(X3)
           END-IF.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE AMDSB-ATTR3       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Compress'    TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoCompress'  TO ATTRS-ENTRY(X3)
           END-IF.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE AMDSB-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           ADD       1       TO X3.
           IF  BIT-1-IS-ON
               MOVE 'WriteChk'    TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoWriteChk'  TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  BIT-2-IS-ON
               MOVE 'Imbed'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoImbed'     TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  BIT-3-IS-ON
               MOVE 'Replicate'   TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoReplicate' TO ATTRS-ENTRY(X3)
           END-IF.

           ADD       1       TO X3.
           IF  BIT-4-IS-ON
               MOVE 'Ordered'     TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'UnOrdered'   TO ATTRS-ENTRY(X3)
           END-IF.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE CAT-D-ATTR1       TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.

           ADD       1       TO X3.
           IF  BIT-2-IS-ON
               MOVE 'Reuse'       TO ATTRS-ENTRY(X3)
           ELSE
               MOVE 'NoReuse'     TO ATTRS-ENTRY(X3)
           END-IF.

           IF  CAT-D-DSETEXDT = 99365
               ADD       1        TO X3
               MOVE 'NeverExpire' TO ATTRS-ENTRY(X3)
           END-IF.

           PERFORM U10-WRITE-ATTRS-TABLE THRU U19-EXIT-WRITE.
           PERFORM T80-WRITE-STATISTICS THRU T89-EXIT-WRITE.

           MOVE AMDSB-NIL    TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Index Levels' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-SSRBA  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Seq-Set-RBA' TO WRK-FIELD(1:11).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE AMDSB-NEST   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Entries/Sect' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE AMDSB-HLRBA  TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Hi-Level-RBA' TO WRK-FIELD(1:12).
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           PERFORM T90-WRITE-ALLOCATION THRU T99-EXIT-WRITE.

       U29-EXIT.
           EXIT.

       U30-PRINT-R-ATTRIBS.

           MOVE 'Owner-Ident-------' TO WRK-FIELD.
           IF  CAT-R-OWNERID = HIGH-VALUES
               MOVE '(null)'         TO WRK-FIELD(19:6)
           ELSE
               MOVE CAT-R-OWNERID    TO WRK-FIELD(17:8)
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).
           MOVE 'Creation------'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-R-DSETCRDT       TO DTE-YYDDD.
           MOVE DTE-JUYR             TO SAVE-YEAR.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           IF  CAT-LO-VERS = X'01'
               MOVE 'Release----------------2' TO T2-DETLINE(8:25)
           ELSE
               MOVE 'Release----------------1' TO T2-DETLINE(8:25)
           END-IF.
           MOVE 'Expiration----'     TO WRK-FIELD.
           MOVE SPACES               TO DTE-JULN.
           MOVE ZEROES               TO DTE-JUYR.
           MOVE CAT-R-DSETEXDT       TO DTE-YYDDD.
           PERFORM P50-DECODE-JULIAN-DATE THRU P55-EXIT-DECODE.
           MOVE EDITTED-DATE         TO WRK-FIELD(15:10).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE CAT-R-GOPCNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO L1.
           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR PSWRD-GOP-INLINE(X1)
                  OR PSWRD-GOP-EXTREC(X1)
           END-PERFORM.

           MOVE 'Protection-----------' TO T2-DETLINE(6:25).
           IF  X1 > L1 OR TABLE-ENTRIES
               MOVE '(null)'            TO T2-DETLINE(26:6)
           ELSE
               IF  PSWRD-GOP-EXTREC(X1)
                   MOVE '(unkn)'        TO T2-DETLINE(26:6)
               ELSE
                   MOVE '(yes)'         TO T2-DETLINE(27:5)
               END-IF
           END-IF.
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE CAT-R-RGATTR TO BIT-BYTE.
           PERFORM T70-WRITE-ASSOCIATIONS THRU T79-EXIT-WRITE.

           MOVE 'Attributes:'        TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           SET BIT-DECODE-FROM-BYTE TO TRUE.
           CALL  'BITMAN'  USING BITMAN-PARMS.
           IF  BIT-0-IS-ON
               MOVE 'Update'         TO T2-DETLINE(8:6)
           ELSE
               MOVE 'NoUpdate'       TO T2-DETLINE(8:8)
           END-IF.
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

       U39-EXIT.
           EXIT.

       U40-COUNT-V-GOPS.

           MOVE    ZEROES    TO DATASETS-ON-VOL.
           MOVE    ZEROES    TO DATASPACES-ON-VOL.

       U42-COUNT-EM.

           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1
                  OR TABLE-ENTRIES
               IF  DSDIR-GOP-INLINE(X1) OR DSDIR-GOP-EXTREC(X1)
                   ADD   1   TO DATASETS-ON-VOL
               END-IF
               IF  SPACE-GOP-INLINE(X1) OR SPACE-GOP-EXTREC(X1)
                   ADD   1   TO DATASPACES-ON-VOL
               END-IF
           END-PERFORM.

           IF  CAT-V-TYPE = 'V' AND CAT-V-EXTREC NOT = LOW-VALUES
           OR  CAT-E-TYPE = 'W' AND CAT-E-EXTREC NOT = LOW-VALUES
               MOVE  LOW-VALUES  TO CAT-LO-KEY
               IF  CAT-V-TYPE = 'V'
                   MOVE CAT-V-EXTREC TO CAT-LO-CINO
               ELSE
                   MOVE CAT-E-EXTREC TO CAT-LO-CINO
               END-IF
               EXEC CICS READ
                         DATASET(VCAT-CATALOG)
                         RIDFLD(CATALOG-KEY)
                         INTO(CATALOG-RECORD)
                         GTEQ
               END-EXEC
               PERFORM T20-PARSE-E-GOPS THRU T29-EXIT-PARSE
               GO TO U42-COUNT-EM
           END-IF.

           IF  CAT-V-TYPE NOT = 'V'
               MOVE  LOW-VALUES  TO CAT-LO-KEY
               MOVE   TS-CINO    TO CAT-LO-CINO
               EXEC CICS READ
                         DATASET(VCAT-CATALOG)
                         RIDFLD(CATALOG-KEY)
                         INTO(CATALOG-RECORD)
                         GTEQ
               END-EXEC
               PERFORM T60-PARSE-V-GOPS THRU T69-EXIT-PARSE
           END-IF.

       U49-EXIT-COUNT.
           EXIT.

       U50-PRINT-V-DETAILS.

           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.
           MOVE 'Data-Space:'        TO T2-DETLINE(6:11).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           SET BIT-DECODE-FROM-BYTE  TO TRUE.
           MOVE SPACE-SPHDFLG        TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           IF  BIT-4-IS-ON
               MOVE 1                TO EDITTED-NUMBER
           ELSE
               MOVE DATASETS-ON-VOL  TO EDITTED-NUMBER
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Datasets'           TO WRK-FIELD(1:8).
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).

           MOVE 'Format-1-Label:'    TO WRK-FIELD.
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).

           MOVE 'Attributes:'        TO WRK-FIELD.
           MOVE WRK-FIELD            TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE SPACE-NODSPEXT       TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Extents'            TO WRK-FIELD(1:7).
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).

           IF  DEVTYPE = 'FBAV'
               MOVE LOW-VALUES       TO TWO-BYTES
               MOVE SPACE-DSCBPTR(5:1) TO ONE-BYTE
               MOVE HALF-WORD        TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               MOVE 'RRnum'          TO WRK-FIELD(1:5)
           ELSE
               SET HEX-DECODE-FROM-CHAR TO TRUE
               MOVE      +5      TO HEX-LENG
               MOVE SPACE-DSCBPTR TO HEX-CHRT
               CALL   'HEXMAN'   USING HEXMAN-PARMS
               STRING 'CCHHR------x''' HEX-HEXT ''''
                   DELIMITED BY SIZE INTO WRK-FIELD
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).

           IF  BIT-0-IS-ON
               MOVE 'Unique'         TO WRK-FIELD
           ELSE
               MOVE 'SubAlloc'       TO WRK-FIELD
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE LOW-VALUES           TO FOUR-BYTES.
           MOVE SPACE-DSPSSQ         TO THREE-BYTES.
           MOVE FULL-WORD            TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Sec-Alloc'          TO WRK-FIELD(1:9).
           MOVE WRK-FIELD            TO T2-DETLINE(8:25).

           MOVE 'TimeStamp:'         TO WRK-FIELD.
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).

           IF  BIT-1-IS-ON
               MOVE 'Implicit'       TO WRK-FIELD
           ELSE
               MOVE 'Explicit'       TO WRK-FIELD
           END-IF.
           MOVE WRK-FIELD            TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    SPACES    TO WRK-FIELD.
           MOVE  LOW-VALUES  TO EIGHT-BYTES.
      * ignore unused rightmost 8 bits
           MOVE SPACE-DSCBTS(1:7) TO EIGHT-BYTES(2:7).
           PERFORM P60-DECODE-TIMESTAMP THRU P65-EXIT-DECODE.
           MOVE   NBR-DATE   TO EDITTED-DATE.
           MOVE EDITTED-DATE TO WRK-FIELD(6:10).
           MOVE   NBR-TIME   TO EDITTED-TIME.
           INSPECT CHARACTER-TIME REPLACING ALL SPACES BY ':'.
           MOVE EDITTED-TIME TO WRK-FIELD(17:8).
           MOVE WRK-FIELD            TO T2-DETLINE(38:25).

           EVALUATE TRUE
           WHEN BIT-2-IS-ON
               MOVE 'UserCat'        TO WRK-FIELD
           WHEN BIT-3-IS-ON
               MOVE 'MasterCat'      TO WRK-FIELD
           WHEN OTHER
               MOVE SPACES           TO WRK-FIELD
               STRING 'b''' BIT-DATA ''''
                   DELIMITED BY SIZE INTO WRK-FIELD
           END-EVALUATE.
           MOVE WRK-FIELD            TO T2-DETLINE(68:25).

           MOVE 'Type--------------------' TO WRK-FIELD.
           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE SPACE-DSPSOPT TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           IF  DEVTYPE = 'FBAV'
               MOVE 'Block'  TO WRK-FIELD(20:5)
           ELSE
               IF  BIT-1-IS-ON
                   MOVE 'Cyl'    TO WRK-FIELD(22:3)
               ELSE
                   MOVE 'Track'  TO WRK-FIELD(20:5)
               END-IF
           END-IF.
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           SET BIT-ENCODE-FROM-BITS TO TRUE.
           MOVE '00000'      TO BIT-DATA(1:5).
           CALL   'BITMAN'   USING BITMAN-PARMS.
           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE   BIT-BYTE   TO ONE-BYTE.
           MOVE  HALF-WORD   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Class'      TO WRK-FIELD(1:5).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE    SPACES    TO WRK-FIELD.
           SET HEX-DECODE-FROM-CHAR TO TRUE.
           MOVE      +8      TO HEX-LENG.
           MOVE SPACE-DSCBTS TO HEX-CHRT.
           CALL   'HEXMAN'   USING HEXMAN-PARMS.
           STRING '     x''' HEX-HEXT ''''
               DELIMITED BY SIZE INTO WRK-FIELD.
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE 'Extent-Descriptor:' TO T2-DETLINE(8:18).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > 16
                  OR SPACE-SPEXTENT(X1) = LOW-VALUES

               MOVE LOW-VALUES         TO FOUR-BYTES
               MOVE SPACE-SPEXNN(X1)  TO TWO-BYTES
               MOVE FULL-WORD          TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               IF  DEVTYPE = 'FBAV'
                   MOVE 'Blocks-Total' TO WRK-FIELD(1:12)
               ELSE
                   MOVE 'Tracks-Total' TO WRK-FIELD(1:12)
               END-IF
               MOVE WRK-FIELD          TO T2-DETLINE(8:25)

               MOVE LOW-VALUES         TO FOUR-BYTES
               MOVE SPACE-SPEXTT(X1)  TO TWO-BYTES
               MOVE FULL-WORD          TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               IF  DEVTYPE = 'FBAV'
                   MOVE 'Beg-Block'    TO WRK-FIELD(1:9)
               ELSE
                   MOVE 'Beg-Track'    TO WRK-FIELD(1:9)
               END-IF
               MOVE WRK-FIELD          TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

               MOVE ZERO               TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               IF  DEVTYPE = 'FBAV'
                   MOVE 'Blocks-Used'  TO WRK-FIELD(1:11)
               ELSE
                   MOVE 'Tracks-Used'  TO WRK-FIELD(1:11)
               END-IF
               MOVE WRK-FIELD          TO T2-DETLINE(8:25)

               MOVE ZERO               TO EDITTED-NUMBER
               INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'
               IF  DEVTYPE = 'FBAV'
                   MOVE 'Blocks-%Used' TO WRK-FIELD(1:12)
               ELSE
                   MOVE 'Tracks-%Used' TO WRK-FIELD(1:12)
               END-IF
               MOVE WRK-FIELD          TO T2-DETLINE(38:25)
               PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE

           END-PERFORM.

       U59-EXIT.
           EXIT.

       U60-COUNT-EXTENTS.

           MOVE  GOP-TABLE   TO GOP-SAVE2.
           MOVE   GO-TABLE   TO GO-SAVE2.
           MOVE CATALOG-RECORD TO REC-SAVE2.

           MOVE      +8      TO P1.
           STRING 'DSN ---- ' DELIMITED BY SIZE
               INTO T2-DETLINE WITH POINTER P1.
           PERFORM WITH TEST BEFORE
             VARYING X1 FROM 1 BY 1
               UNTIL X1 > L1 OR TABLE-ENTRIES
                  OR ASSOC-GOP-INLINE(X1)
                 AND GO-TYPE(X1) = 'C' OR 'G'
           END-PERFORM.
           IF  X1 <= L1 AND TABLE-ENTRIES
           AND ASSOC-GOP-INLINE(X1)
           AND GO-TYPE(X1) = 'C' OR 'G'
               MOVE LOW-VALUES   TO CAT-LO-KEY
               MOVE ASSOC-CINO(X1) TO CAT-LO-CINO
               EXEC CICS READ
                         DATASET(VCAT-CATALOG)
                         RIDFLD(CATALOG-KEY)
                         INTO(CATALOG-RECORD)
                         GTEQ
                         NOHANDLE
               END-EXEC
               STRING CAT-C-ENTNAME DELIMITED BY SPACE
                   INTO T2-DETLINE WITH POINTER P1
               MOVE REC-SAVE2 TO CATALOG-RECORD
               STRING ' (' CAT-D-TYPE ')' DELIMITED BY SIZE
                   INTO T2-DETLINE WITH POINTER P1
           ELSE
               MOVE REC-SAVE2 TO CATALOG-RECORD
               STRING CAT-D-ENTNAME DELIMITED BY SPACE
                   INTO T2-DETLINE WITH POINTER P1
           END-IF.

           MOVE    ZEROES    TO EXTENT-COUNT.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             UNTIL L1 = ZEROES

               MOVE CATALOG-RECORD TO REC-SAVE2

               PERFORM WITH TEST BEFORE
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1
                      OR TABLE-ENTRIES
                   IF  X1 <= L1 AND TABLE-ENTRIES
                   AND (VOLUM-GOP-INLINE(X1) OR  VOLUM-GOP-EXTREC(X1))
                       IF  VOLUM-GOP-INLINE(X1)
                           MOVE REC-SAVE2 TO CATALOG-RECORD
                           COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       ELSE
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE GOP-SEQU(X1) TO ONE-BYTE
                           MOVE  HALF-WORD   TO SQ
                           MOVE  LOW-VALUES  TO CAT-LO-KEY
                           MOVE GOP-CINO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                           END-EXEC
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE CAT-E-GOPCNT TO ONE-BYTE
                           MOVE  HALF-WORD   TO L3
                           PERFORM WITH TEST AFTER
                             VARYING X3 FROM 1 BY 1
                               UNTIL X3 > L3 OR TABLE-ENTRIES
                                  OR VOLUM-GOP-INLINE(X3) AND
                                     HALF-WORD = SQ
                               IF  X3 <= L3 AND TABLE-ENTRIES
                                   COMPUTE P3 = (X3 - 1) * LN + 1
                                   MOVE CAT-E-VARDTA(P3:LN)
                                       TO GO-POINTER(X3)
                                   MOVE  LOW-VALUES  TO TWO-BYTES
                                   MOVE GOP-SEQU(X3) TO ONE-BYTE
                               END-IF
                           END-PERFORM
                           COMPUTE P1 = L3 * LN + 1 + GOP-DISP(X3)
                       END-IF
                       IF  CAT-D-TYPE = 'D' OR 'I'
                           MOVE CAT-D-VARDTA(P1:LENGTH OF VOLUM-GO)
                               TO VOLUM-GO
                       ELSE
                           MOVE CAT-E-VARDTA(P1:LENGTH OF VOLUM-GO)
                               TO VOLUM-GO
                       END-IF
                       IF  VOLUM-VOLSER = TS-ENTNAME(1:6)
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE VOLUM-NOEXTNT TO ONE-BYTE
                           SET BIT-DECODE-FROM-BYTE TO TRUE
                           MOVE VOLUM-VOLFLG TO BIT-BYTE
                           CALL  'BITMAN'  USING BITMAN-PARMS
                           IF  BIT-1-IS-OFF
                           OR  BIT-1-IS-ON
                           AND HALF-WORD > ZERO
                               ADD   HALF-WORD   TO EXTENT-COUNT
                           END-IF
                       END-IF
                       MOVE GOP-SAVE2 TO GOP-TABLE
                       MOVE GO-SAVE2  TO GO-TABLE
                   END-IF
               END-PERFORM

               MOVE REC-SAVE2 TO CATALOG-RECORD

               IF (CAT-D-TYPE = 'D' OR 'I')
                                    AND CAT-D-EXTREC NOT = LOW-VALUES
               OR  CAT-E-TYPE = 'E' AND CAT-E-EXTREC NOT = LOW-VALUES
                   MOVE  LOW-VALUES  TO CAT-LO-KEY
                   IF  CAT-D-TYPE = 'D' OR 'I'
                       MOVE CAT-D-EXTREC TO CAT-LO-CINO
                   ELSE
                       MOVE CAT-E-EXTREC TO CAT-LO-CINO
                   END-IF
                   EXEC CICS READ
                             DATASET(VCAT-CATALOG)
                             RIDFLD(CATALOG-KEY)
                             INTO(CATALOG-RECORD)
                             GTEQ
                   END-EXEC
                   PERFORM T20-PARSE-E-GOPS THRU T29-EXIT-PARSE
                   MOVE GOP-TABLE TO GOP-SAVE2
                   MOVE GO-TABLE  TO GO-SAVE2
               ELSE
                   MOVE   ZEROES   TO L1
               END-IF

           END-PERFORM.

       U69-EXIT-COUNT.
           EXIT.

       U70-PRINT-D-DETAILS.

           MOVE  GOP-TABLE   TO GOP-SAVE2.
           MOVE   GO-TABLE   TO GO-SAVE2.
           MOVE      5       TO LN.

           PERFORM WITH TEST AFTER
             UNTIL L1 = ZEROES

               MOVE CATALOG-RECORD TO REC-SAVE2

               PERFORM WITH TEST BEFORE
                 VARYING X1 FROM 1 BY 1
                   UNTIL X1 > L1
                      OR TABLE-ENTRIES
                   IF  X1 <= L1 AND TABLE-ENTRIES
                   AND (VOLUM-GOP-INLINE(X1) OR  VOLUM-GOP-EXTREC(X1))
                       IF  VOLUM-GOP-INLINE(X1)
                           MOVE REC-SAVE2 TO CATALOG-RECORD
                           COMPUTE P1 = L1 * LN + 1 + GOP-DISP(X1)
                       ELSE
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE GOP-SEQU(X1) TO ONE-BYTE
                           MOVE  HALF-WORD   TO SQ
                           MOVE  LOW-VALUES  TO CAT-LO-KEY
                           MOVE GOP-CINO(X1) TO CAT-LO-CINO
                           EXEC CICS READ
                                     DATASET(VCAT-CATALOG)
                                     RIDFLD(CATALOG-KEY)
                                     INTO(CATALOG-RECORD)
                                     GTEQ
                           END-EXEC
                           MOVE  LOW-VALUES  TO TWO-BYTES
                           MOVE CAT-E-GOPCNT TO ONE-BYTE
                           MOVE  HALF-WORD   TO L3
                           PERFORM WITH TEST AFTER
                             VARYING X3 FROM 1 BY 1
                               UNTIL X3 > L3 OR TABLE-ENTRIES
                                  OR VOLUM-GOP-INLINE(X3) AND
                                     HALF-WORD = SQ
                               IF  X3 <= L3 AND TABLE-ENTRIES
                                   COMPUTE P3 = (X3 - 1) * LN + 1
                                   MOVE CAT-E-VARDTA(P3:LN)
                                       TO GO-POINTER(X3)
                                   MOVE  LOW-VALUES  TO TWO-BYTES
                                   MOVE GOP-SEQU(X3) TO ONE-BYTE
                               END-IF
                           END-PERFORM
                           COMPUTE P1 = L3 * LN + 1 + GOP-DISP(X3)
                       END-IF
                       IF  CAT-D-TYPE = 'D' OR 'I'
                           MOVE CAT-D-VARDTA(P1:LENGTH OF VOLUM-GO)
                               TO VOLUM-GO
                       ELSE
                           MOVE CAT-E-VARDTA(P1:LENGTH OF VOLUM-GO)
                               TO VOLUM-GO
                       END-IF
                       PERFORM U80-PRINT-D-VOLUMES THRU U89-EXIT
                       MOVE GOP-SAVE2 TO GOP-TABLE
                       MOVE GO-SAVE2  TO GO-TABLE
                       MOVE     5     TO LN
                   END-IF
               END-PERFORM

               MOVE REC-SAVE2 TO CATALOG-RECORD

               IF (CAT-D-TYPE = 'D' OR 'I')
                                    AND CAT-D-EXTREC NOT = LOW-VALUES
               OR  CAT-E-TYPE = 'E' AND CAT-E-EXTREC NOT = LOW-VALUES
                   MOVE  LOW-VALUES  TO CAT-LO-KEY
                   IF  CAT-D-TYPE = 'D' OR 'I'
                       MOVE CAT-D-EXTREC TO CAT-LO-CINO
                   ELSE
                       MOVE CAT-E-EXTREC TO CAT-LO-CINO
                   END-IF
                   EXEC CICS READ
                             DATASET(VCAT-CATALOG)
                             RIDFLD(CATALOG-KEY)
                             INTO(CATALOG-RECORD)
                             GTEQ
                   END-EXEC
                   PERFORM T20-PARSE-E-GOPS THRU T29-EXIT-PARSE
                   MOVE GOP-TABLE TO GOP-SAVE2
                   MOVE GO-TABLE  TO GO-SAVE2
               ELSE
                   MOVE   ZEROES   TO L1
               END-IF

           END-PERFORM.

       U79-EXIT.
           EXIT.

       U80-PRINT-D-VOLUMES.

           IF  VOLUM-DEVTYP = X'60082100'
               MOVE 'FBAV'   TO DEVTYPE
           ELSE
               MOVE '3390'   TO DEVTYPE
           END-IF.

           MOVE 'Volumes:'   TO T2-DETLINE(6:8).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    SPACES    TO WRK-FIELD.
           MOVE VOLUM-VOLSER TO WRK-FIELD(19:6)
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'VolSer'     TO WRK-FIELD(1:6).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE VOLUM-PHYBLKSZ TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'PhyRec-Size' TO WRK-FIELD(1:11).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE  LOW-VALUES  TO EIGHT-BYTES.
           MOVE VOLUM-HARBA  TO FOUR-BYTES.
           MOVE DOUBLE-WORD  TO EDITTED-NUMBER
                                HIGH-ALLOC-RBA.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
      *    IF  IS-EXTRALARGE-DATASET
           IF  TS-ENTTYPE = 'XLDS'
           AND PROCESSING-DATA-COMPONENT
               MOVE 'HiAlc-CI' TO WRK-FIELD(1:8)
           ELSE
               MOVE 'HiAlcRBA' TO WRK-FIELD(1:8)
           END-IF.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    SPACES    TO WRK-FIELD.
           MOVE   DEVTYPE    TO WRK-FIELD(21:4)
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'DevType'    TO WRK-FIELD(1:7).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE VOLUM-NOBLKTRK TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'PhyRecs/Trk' TO WRK-FIELD(1:11).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE  LOW-VALUES  TO EIGHT-BYTES.
           MOVE VOLUM-HURBA  TO FOUR-BYTES.
           IF  DOUBLE-WORD > HIGH-ALLOC-RBA
               MOVE HIGH-ALLOC-RBA TO EDITTED-NUMBER
                                      DOUBLE-WORD
           ELSE
               MOVE DOUBLE-WORD    TO EDITTED-NUMBER
           END-IF.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
      *    IF  IS-EXTRALARGE-DATASET
           IF  TS-ENTTYPE = 'XLDS'
           AND PROCESSING-DATA-COMPONENT
               MOVE 'HiUse-CI' TO WRK-FIELD(1:8)
           ELSE
               MOVE 'HiUseRBA' TO WRK-FIELD(1:8)
           END-IF.
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).

           IF  VOLUM-HARBA = LOW-VALUES
               MOVE  ZEROES  TO EDITTED-NUMBER
           ELSE
               COMPUTE EDITTED-NUMBER ROUNDED =
                   ((DOUBLE-WORD / HIGH-ALLOC-RBA) * 100.00)
           END-IF.
           MOVE    ZEROES    TO TALLY.
           INSPECT WRK-FIELD TALLYING TALLY
               FOR LEADING SPACES.
           ADD       1       TO TALLY.
           SUBTRACT TALLY    FROM 25 GIVING LN.
           STRING ' (' WRK-FIELD(TALLY:LN) '%)'
               DELIMITED BY SIZE INTO WRK-FIELD.
           MOVE WRK-FIELD    TO T2-DETLINE(92:6).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    SPACES    TO WRK-FIELD.
           SET BIT-DECODE-FROM-BYTE TO TRUE.
           MOVE VOLUM-VOLFLG TO BIT-BYTE.
           CALL   'BITMAN'   USING BITMAN-PARMS.
           EVALUATE TRUE
           WHEN BIT-0-IS-ON
               MOVE 'Prime'  TO WRK-FIELD(20:5)
           WHEN BIT-1-IS-ON
               MOVE 'Candidate' TO WRK-FIELD(16:9)
           WHEN BIT-2-IS-ON
               MOVE 'Overflow' TO WRK-FIELD(17:8)
           END-EVALUATE.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'VolFlag'    TO WRK-FIELD(1:7).
           MOVE WRK-FIELD    TO T2-DETLINE(8:25).

           MOVE VOLUM-NOTRKAU TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Tracks/CA'  TO WRK-FIELD(1:9).
           MOVE WRK-FIELD    TO T2-DETLINE(38:25).

           MOVE  LOW-VALUES  TO TWO-BYTES.
           MOVE VOLUM-NOEXTNT TO ONE-BYTE.
           MOVE  HALF-WORD   TO EDITTED-NUMBER.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Extent-Count' TO WRK-FIELD(1:12)
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE    SPACES    TO WRK-FIELD.
           SET HEX-DECODE-FROM-CHAR TO TRUE
           MOVE      +1      TO HEX-LENG.
           MOVE VOLUM-ITYPEXT TO HEX-CHRT.
           CALL  'HEXMAN'  USING HEXMAN-PARMS.
           MOVE      20      TO P1.
           STRING 'x''' HEX-HEXT '''' DELIMITED BY SIZE
               INTO WRK-FIELD WITH POINTER P1.
           INSPECT WRK-FIELD REPLACING LEADING SPACES BY '-'.
           MOVE 'Extent-Type' TO WRK-FIELD(1:11)
           MOVE WRK-FIELD    TO T2-DETLINE(68:25).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

           MOVE 'Extents:'   TO T2-DETLINE(8:8).
           PERFORM P42-WRITE-T2-DETL THRU P43-EXIT-WRITE.

       U89-EXIT.
           EXIT.

      /*****************************************************************
      *    VALIDATION ROUTINE                                          *
      ******************************************************************
       V00-VALIDATE.

           MOVE   AIDENTER   TO EIBAID.

           MOVE     'E'      TO SBA-REQU.
           MOVE    ZEROES    TO SBA-POSN.
           MOVE      24      TO SBA-ROWN.
           MOVE      01      TO SBA-COLN.
           CALL  'SBA3270'   USING SBA3270-PARMS.

           MOVE      79      TO UNEX-MSGL.
           STRING SCRSETBA SBA-ADDR SCRSTFLD FLDASBRT
               DELIMITED BY SIZE INTO CONFIRM-MSG.

           EXEC CICS SEND
                     FROM(CONFIRM-MSG)
                     LENGTH(UNEX-MSGL)
                     CTLCHAR(WRTFKFSA)
                     WAIT
                     NOHANDLE
                     END-EXEC.

           IF EIBRESP NOT = DFHRESP(NORMAL)
               MOVE    -1    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR THRU X99-EXIT-ERR
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO V90-EXIT-VALIDATE
           END-IF.

           EXEC CICS RECEIVE
                     NOHANDLE
                     END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           AND EIBRESP NOT = DFHRESP(EOC)
               MOVE    -1    TO ERROR-SWITCH
               PERFORM X00-UNEX-ERR THRU X99-EXIT-ERR
               MOVE UNEX-MSG TO LIST-MESSAGE-O
               GO TO V90-EXIT-VALIDATE
           END-IF.

       V90-EXIT-VALIDATE.
           EXIT.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.

      /*****************************************************************
      *    PROGRAM TERMINATION (NORMAL)                                *
      ******************************************************************
       Z00-VCAT-TERMINATION.

           PERFORM P90-CLOSE-CATALOG THRU P95-EXIT-CLOSE.

           COPY COMMEXIT.

