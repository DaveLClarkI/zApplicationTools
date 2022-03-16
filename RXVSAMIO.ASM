VSM      TITLE 'RXVSAMIO - REXX FUNCTION TO ACCESS VSAM FILES && STATS'
         PRINT ON,GEN
RXVSAMIO AMODE 24
RXVSAMIO RMODE 24
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*             V S A M   F I L E   I / O   f o r   R E X X             *
*                                                                     *
*                                                                     *
* No warranty is expressed or implied.       Written by Dave L Clark  *
* Neither the author nor the company is      Phone: (937)751-3300     *
* responsible for any loss or damage         Winsupply Group Services *
* resulting from the use of this code.       3110 Kettering Blvd.     *
* If provided, source on as-is basis.        Dayton, OH  45439        *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This external REXX function allows  a REXX program to access a *
* VSAM Dataset of either a 'KSDS',  'ESDS',  'PATH',  'RRDS', 'VRDS', *
* 'SAME',  'SAMI',  or 'AIX' dataset  type.   Passed information is a *
* required DLBL name and  the  access request to be performed.   Some *
* access requests have  additional,  optional parameters with implied *
* defaults.  This routine cannot be used to access NOCIFORMAT files.  *
*                                                                     *
*      The passed DLBL name must  also  refer to a REXX stem variable *
* of the same  name.   The  elements  of  that  stem variable provide *
* additional information for  this  external  REXX  function and this *
* external REXX function will  add  or  update  elements of that stem *
* variable as needed to provide additional information to the caller. *
* Some of the other things  that stem variable will then contain,  on *
* request, is returned dataset attributes and statistics.             *
*                                                                     *
* NOTE: There is only one  access  string  and  one record layout per *
*       file.   So,  if more than one  position  in  the same file is *
*       needed or different record  layouts  are  needed for a multi- *
*       format file,  then separate  DLBL  statements are required in *
*       order to handle either of these situations.   Up to nine file *
*       definitions at a time are accepted by this function.          *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                USAGE                                *
*                                                                     *
*      The information passed on the  function call has the following *
* general syntax:                                                     *
*                                                                     *
* Call RXVSAMIO ddname, request <, request_arg2 <, request_arg3 > >;  *
*                                                                     *
*                                                                     *
*      And the following  are  all  of  the  possible request formats *
* (note that the request arguments may be spelled out, of course, but *
* only as few letters for uniqueness are actually checked):           *
*                                                                     *
* Call RXVSAMIO ddname, 'ATTRS';                                      *
*        Where: no OPEN or CLOSE required for attrs & stats but, if   *
*               already open, the dataset is automatically closed.    *
*                                                                     *
* Call RXVSAMIO ddname, 'OPEN'                                        *
*                       <, 'INPUT'|'UPDATE'|'OUTPUT'|'APPEND'|'RESET' *
*                               <, 'SEQUENCE'|'RANDOM'|'DYNAMIC' > >; *
*        Where: 'INPUT' and 'SEQUENCE' are the defaults,              *
*               'RESET' is the same as 'OUTPUT' but for REUSE, and    *
*               'DYNAMIC' enables skip-sequential processing.         *
*                                                                     *
* Call RXVSAMIO ddname, { 'FWDPOS' | 'BWDPOS' }                       *
*                               <, 'KEY' <, 'GTEQ'|'EQUAL' > |'RBA'>; *
*        Where: 'KEY' and 'GTEQ' are the defaults, and                *
*               this performs VSAM POINT function processing -- only  *
*               setting file position and direction (no record read,  *
*               but 'GTEQ' is ignored for backward positioning).      *
*                                                                     *
* Call RXVSAMIO ddname, 'READ' <, 'NEXT'|'PREV'                       *
*                                 |'KEY' <, 'EQUAL'|'GTEQ' > |'RBA'>; *
*        Where: 'NEXT' is the default (or,                            *
*               if 'KEY' is specified, then 'EQUAL' is the default)   *
*               and, if opened for UPDATE, always causes a CI lock.   *
*                                                                     *
* Call RXVSAMIO ddname, 'WRITE';                                      *
*        Where: on write, automatically releases any prior locks.     *
*                                                                     *
* Call RXVSAMIO ddname, 'UPDATE';                                     *
*        Where: a prior successful READ is required, first.           *
*                                                                     *
* Call RXVSAMIO ddname, 'DELETE';                                     *
*        Where: a prior successful READ is required, first.           *
*                                                                     *
* Call RXVSAMIO ddname, 'CLOSE';                                      *
*                                                                     *
*                                                                     *
* Where: { }      encloses required request arguments;                *
*        < >      encloses optional request arguments;                *
*         |       separates mutually exclusive options; and,          *
*         ,       commas are required to separate arguments.          *
*                                                                     *
*                                                                     *
*      Otherwise,  the stem variable  mentioned  is where you provide *
* the data definitions (see INPUT, below) that allow this function to *
* parse the VSAM records,  after  retrieval,  so that the caller does *
* not have to do this  themselves.   The parsed (and converted) field *
* data is also returned in the  stem variable -- a separate entry for *
* each  field  definition  given.   At  WRITE  or  UPDATE time,  this *
* function also uses  the  provided  data  definitions to rebuild the *
* complete VSAM record from  the  individual  field data found in the *
* stem variable.                                                      *
*                                                                     *
*      The caller must provide  two  sets  of data definitions in the *
* stem variable -- one set  for  the  key  layout and one set for the *
* record layout.   This is documented under INPUT, below.  These data *
* definitions provide information  for  up  to  seven different field *
* data types -- as follows:                                           *
*                                                                     *
*   CHAR    is string data up to 256 bytes in length that does not    *
*              require conversion.  For a key field, of course, this  *
*              is limited to 255 bytes.  The COBOL equivalent is a    *
*              PIC A or PIC X field and USAGE IS DISPLAY.             *
*                                                                     *
*   HEX     is string data up to 128 bytes in length that is          *
*              automatically converted to 2-byte (per character)      *
*              hexadecimal format when returned to the caller and     *
*              automatically converted back to 1-byte character       *
*              format in the VSAM record.  The COBOL equivalent is    *
*              a PIC X field and USAGE IS DISPLAY.                    *
*                                                                     *
*   ZONED   is a signed, zoned-decimal number up to 31 bytes long     *
*              that is automatically converted to REXX format when    *
*              returned to the caller and automatically converted     *
*              back to signed, zoned-decimal format in the VSAM       *
*              record.  The COBOL equivalent is a PIC S9 field with   *
*              or without a V position and USAGE IS DISPLAY.          *
*                                                                     *
*   IZONED  is an unsigned, zoned-decimal integer up to 31 bytes long *
*              that is automatically converted to REXX format (with   *
*              left-zero padding, as needed) when returned to the     *
*              caller and automatically converted back to unsigned,   *
*              zoned-decimal format in the VSAM record.  For example, *
*              this type is useful for zoned date and time fields.    *
*              Packed date and time fields must use the "PACKED" data *
*              type.  The COBOL equivalent is a PIC 9 field without   *
*              a V position and USAGE IS DISPLAY.                     *
*                                                                     *
*   PACKED  is a signed, packed-decimal number up to 16 bytes long    *
*              (31 digits plus a sign) that is automatically          *
*              converted to REXX format when returned to the caller   *
*              and automatically converted back to signed,            *
*              packed-decimal format in the VSAM record.  The COBOL   *
*              equivalent is a PIC S9 field with or without a V       *
*              position and USAGE IS COMP-3 or PACKED-DECIMAL.        *
*                                                                     *
*   BINARY  is a signed, binary integer 2-, 4-, or 8-bytes in length  *
*              that is automatically converted to REXX format when    *
*              returned to the caller and automatically converted     *
*              back to signed, binary format in the VSAM record:      *
*                 2-bytes is up to 32,767 max,                        *
*                 4-bytes is up to 2,147,483,647 max, and             *
*                 8-bytes is up to 9,223,372,036,854,775,807 max.     *
*              The COBOL equivalent is a PIC S9 field without a V     *
*              position and USAGE IS COMP or USAGE IS BINARY.         *
*                                                                     *
*   UBIN    is an unsigned, binary integer 1-, 2-, or 4-bytes in      *
*              length that is automatically converted to REXX         *
*              format when returned to the caller and automatically   *
*              converted back to unsigned, binary format in the       *
*              VSAM record:                                           *
*                 1-byte  is up to 255 max,                           *
*                 2-bytes is up to 65,535 max, and                    *
*                 4-bytes is up to 4,294,967,295 max.                 *
*              There is no COBOL equivalent for this because COBOL's  *
*              idea of no sign is to force the sign to be positive.   *
*              But, e.g., VSAM RBAs are 4-byte, unsigned binary       *
*              and VSAM record lengths are 2-byte, unsigned binary.   *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      Create the five object decks,  first, as standard 24-bit batch *
* subroutines (actually, four subroutines and one main program -- but *
* as an object deck).   Then,  link  this  program's object deck as a *
* standard,  24-bit batch main  program  (even  though it switches to *
* 31-bit addressing mode, internally).                                *
*                                                                     *
*      RXVSAMBK     RXVSAMIO -- Build Key Buffer Subroutine           *
*      RXVSAMBR     RXVSAMIO -- Build Record Buffer Subroutine        *
*      RXVSAMXA     RXVSAMIO -- Extract Dataset Attrs Subroutine      *
*      RXVSAMXR     RXVSAMIO -- Extract Record Fields Subroutine      *
*      RXVSAMIO     REXX Function For VSAM File I/O & Statistics      *
*                                                                     *
* This main program will not, however, then be available for just any *
* program to call.   Only a REXX  procedure  will be able to call it. *
* Furthermore,  do not distribute the linked main program because, in *
* addition to its own subroutines,  it also  links in a couple of IBM *
* subroutines that are not licensed for distribution.                 *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                INPUT                                *
*                                                                     *
*      On input, the following stem variable elements are used:       *
*                                                                     *
*   ..._rln     value for the record length to be used for variable-  *
*                 length record processing (_rmx is the default);     *
*   ..._rmx     value for the maximum record length (at OPEN time,    *
*                 this will be supplied automatically as a lookup     *
*                 from the associated catalog -- it is highly         *
*                 inadvisable that this value should be changed);     *
*                                                                     *
*   ..._kln     value for the key length to be used for generic       *
*                 point or read processing (_kmx is the default);     *
*   ..._kmx     value for the maximum key length (at OPEN time,       *
*                 this will be supplied automatically as a lookup     *
*                 from the associated catalog -- it is highly         *
*                 inadvisable that this value should be changed);     *
*                                                                     *
*   ..._key.0   value specifies the total number of contiguous key    *
*                 fields that make up a full key for this file        *
*                 (specify a value of 1 for RBA or RRN access);       *
*   ..._key.n   where n is from 1 to the value of _key.0, in sequence *
*                 and without gaps,  that specifies one or more field *
*                 definitions to make up  a  full  key for this file; *
*                 where the 5-word value is:                          *
*                   word1 is the actual name of this key field        *
*                           (assembler conventions up to 32 chars),   *
*                   word2 is data type of the field in the record,    *
*                   word3 is the key zero-offset of this key field,   *
*                   word4 is the byte length of this key field,       *
*                   word5 is the number of decimal digits (up to 15)  *
*                           for 'PACKED' or 'ZONED' key fields (must  *
*                           be zero for 'BINARY'/'IZONED' key fields) *
*                 (define a single 'UBIN'-type, 0-offset, 4-byte,     *
*                 0-decimal key field for RBA or RRN access);         *
*   ..._key.key_name                                                  *
*               contains the character or decimal value for the       *
*                 associated key field (where "key_name" is one of    *
*                 those found in word1 of the _key.n definitions      *
*                 given, undefined "key_name" fields default to all   *
*                 binary zeroes, and supplied "key_name" values are   *
*                 automatically converted by this function before     *
*                 storing in the file and before storing in the       *
*                 associated stem variables -- such that the caller   *
*                 only has to deal with them in character and decimal *
*                 formats, as appropriate);                           *
*                                                                     *
*   ..._fld.0   value specifies the number of contiguous fields that  *
*                 make up a full record for this file;                *
*   ..._fld.n   where n is from 1 to the value of _fld.0, in sequence *
*                 and without gaps,  that specifies one or more field *
*                 definitions to make up a full record for this file; *
*                 where the 5-word value is:                          *
*                   word1 is the actual name of this record field     *
*                           (assembler conventions up to 32 chars),   *
*                   word2 is data type of the field in the record,    *
*                   word3 is the record zero-offset of this field,    *
*                   word4 is the byte length of this record field,    *
*                   word5 is the number of decimal digits (up to 15)  *
*                           for 'PACKED' or 'ZONED' fields (must be   *
*                           zero for 'BINARY' and 'IZONED' fields);   *
*   ..._fld.fld_name                                                  *
*               contains the character or decimal value for the       *
*                 associated record field (where "fld_name" is one of *
*                 those found in word1 of the _fld.n definitions      *
*                 given and values are automatically converted by     *
*                 this function before storing in the file and before *
*                 storing in the associated stem variables -- such    *
*                 that the caller only has to deal with them in       *
*                 character and decimal formats, as appropriate).     *
*                                                                     *
* Note that if a non-binary  field  value  on disk is low-values (all *
* binary zeroes), then this function returns the *NULL keyword as the *
* associated variable value.   Signed binary field values, of course, *
* return  a  zero  (0)  as  the  associated  variable  value  in this *
* situation.  Conversely, the caller may pass the *NULL keyword, as a *
* variable value,  to set  the  associated  field  on  disk to binary *
* low-values regardless of the field's data type.                     *
*                                                                     *
* For complete functionality,  then,  if a  non-binary field value on *
* disk is high-values (all  binary ones),  then this function returns *
* the *HIGH keyword as the associated variable value.   Signed binary *
* field values,  of course, return a minus one (-1) as the associated *
* variable value in this situation.   Conversely, the caller may pass *
* the *HIGH keyword, as a variable value, to set the associated field *
* on disk to binary high-values regardless of the field's data type.  *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
*      After every request,  the following stem variable elements are *
* returned -- as available;                                           *
*                                                                     *
*   ..._ddn     the data label name for the file (variable form);     *
*   ..._lfn     the name of the last access function requested;       *
*   ..._lfm     the name of the last failed macro, if any;            *
*   ..._rc      the VSAM Return Code (in decimal);                    *
*   ..._ec      the VSAM Error Code (in decimal) or, if a catalog     *
*                 management error, then this is two words for the    *
*                 decimal Reason Code and the module id;              *
*   ..._fc      the VSAM Feedback Function Code (in decimal).         *
*                                                                     *
*                                                                     *
*      After  a  successful  OPEN,   the  following  additional  stem *
* variable elements are returned:                                     *
*                                                                     *
*   ..._dsn     the data set name for the file;                       *
*   ..._typ     the file type: KSDS, XLDS, ESDS, PATH, RRDS, VRDS,    *
*                                          SAME, SAMI, or AIX;        *
*   ..._rmx     the maximum record length for the file;               *
*   ..._kmx     the maximum key length for the file;                  *
*   ..._rkp     the zero-offset relative key position in the record   *
*                 (for RBA and RRN files this is always zero).        *
*                                                                     *
*                                                                     *
*      After a successful  READ,  the  following additional stem var- *
* iable elements are  returned  (the  _key.key_name  values are never *
* changed by this function):                                          *
*                                                                     *
*   ..._fld.fld_name                                                  *
*               contains the character or decimal value for the       *
*                 associated record field (the "fld_name" is as found *
*                 in word1 of the _fld.n definitions given and values *
*                 are automatically converted by this function as     *
*                 outlined, above);                                   *
*                                                                     *
*   ..._rba     the record's relative byte address (in decimal);      *
*   ..._rrn     the record's relative number (for RRDS, in decimal);  *
*   ..._rln     the length of the (possibly variable-length) record   *
*                 just retrieved.                                     *
*                                                                     *
*                                                                     *
*      After the ATTRS request,  all  of  the following stem variable *
* elements are returned:                                              *
*                                                                     *
*   ..._ddn     the data label name for the file;                     *
*   ..._dsn     the data set name for the file;                       *
*   ..._catlbl  the catalog data label name for the file;             *
*   ..._catdsn  the catalog dataset name for the file;                *
*   ..._typ     the file type: KSDS, XLDS, ESDS, PATH, RRDS, VRDS,    *
*                                          SAME, SAMI, or AIX;        *
*   ..._rmx     the maximum record length for the file                *
*                   (or the maximum block size for MSAM files);       *
*   ..._rav     the average record length for the file                *
*                   (or the logical record length for MSAM files);    *
*   ..._kmx     the maximum key length for the file;                  *
*   ..._rkp     the zero-offset relative key position in the record;  *
*                                                                     *
*   ..._crea    the creation date for the file;                       *
*   ..._expd    the expiration date for the file;                     *
*   ..._tstp    the last updated timestamp for the file (two words);  *
*   ..._shro    the share options for the file (two words);           *
*                                                                     *
*   ..._data    the DATA component name;                              *
*   ..._dcsz    the DATA control interval (CI) size;                  *
*   ..._dexc    the number of DATA EXCPs;                             *
*   ..._dext    the number of DATA extents;                           *
*   ..._dcis    the number of DATA CI splits;                         *
*   ..._dcas    the number of DATA CA splits;                         *
*   ..._dfsp    the DATA CI/CA freespace requested (two words);       *
*   ..._dhal    the hi-allocated DATA RBA or XXL CI;                  *
*   ..._dhus    the hi-used DATA RBA or XXL CI;                       *
*   ..._dtal    the type of DATA allocation (CYL, TRK, or BLK);       *
*   ..._dpal    the primary DATA allocation;                          *
*   ..._dsal    the secondary DATA allocation;                        *
*   ..._dvol    the list of DATA volume(s);                           *
*                                                                     *
*   ..._indx    the INDEX component name (if any);                    *
*   ..._icsz    the INDEX control interval (CI) size;                 *
*   ..._iexc    the number of INDEX EXCPs;                            *
*   ..._iext    the number of INDEX extents;                          *
*   ..._ilvl    the number of INDEX levels;                           *
*   ..._issr    the INDEX sequence set RBA;                           *
*   ..._ihlr    the INDEX high-level RBA;                             *
*   ..._ihal    the hi-allocated INDEX RBA;                           *
*   ..._ihus    the hi-used INDEX RBA;                                *
*   ..._ital    the type of INDEX allocation (CYL, TRK, or BLK);      *
*   ..._ipal    the primary INDEX allocation;                         *
*   ..._isal    the secondary INDEX allocation;                       *
*   ..._ivol    the list of INDEX volume(s);                          *
*                                                                     *
*   ..._comp    a Boolean value indicating a COMPRESSed dataset;      *
*   ..._eras    a Boolean value indicating the ERASE setting;         *
*   ..._imbd    a Boolean value indicating the IMBED setting;         *
*   ..._ordr    a Boolean value indicating the ORDERED setting;       *
*   ..._rcvr    a Boolean value indicating the RECOVERY setting;      *
*   ..._repl    a Boolean value indicating the REPLICATE setting;     *
*   ..._reus    a Boolean value indicating the REUSE setting;         *
*   ..._span    a Boolean value indicating the SPANNED setting;       *
*   ..._ukey    a Boolean value for the UNIQUEKEY setting (AIX-only); *
*   ..._upgr    a Boolean value for the UPGRADE setting (AIX-only);   *
*   ..._updt    a Boolean value for the UPDATE setting (PATH-only);   *
*   ..._wchk    a Boolean value indicating the WRITECHECK setting;    *
*                                                                     *
*   ..._recs    the number of records in the file;                    *
*   ..._dels    the number of deletes issued against the file;        *
*   ..._adds    the number of inserts issued against the file;        *
*   ..._upds    the number of updates issued against the file;        *
*   ..._gets    the number of retrievals issued against the file.     *
*                                                                     *
*                                                                     *
*      In addition,  the RXVSAMIO  function  also  sets the REXX "RC" *
* special variable with a value  indicating the success or failure of *
* the requested operation.   You  should  analyze the "RC" variable's *
* value for errors.  Those values are as follows:                     *
*                                                                     *
*   -9   Record length error in stem (missing or out of range);       *
*   -8   Key length error in stem (missing or out of range);          *
*   -7   One or more of the _fld elements are invalid;                *
*   -6   One or more of the _key elements are invalid;                *
*   -5   Specified file label is for a NOCIFORMAT file;               *
*   -4   Specified file label is missing a data component;            *
*   -3   Specified file label is not a cluster, aix, or path entry;   *
*   -2   A maximum of 9 files have already been used;                 *
*   -1   VSAM end-of-file or top-of-file occurred;                    *
*    0   no errors occurred;                                          *
*    1   request argument 1 is invalid;                               *
*    2   request argument 2 is invalid;                               *
*    3   request argument 3 is invalid;                               *
*    4   argument error -- missing, too many, or too long;            *
*    8   internal REXX function or system macro failed:               *
*          also, the failing function's/macro's register 15 value     *
*          is returned in the REXX "RESULT" special variable;         *
*   12   REXX environment not present; and,                           *
*   16   a VSAM error occurred (as noted under "OUTPUT" above).       *
*                                                                     *
* Note that if a subroutine  of RXVSAMIO produces an error,  then the *
* variable "SRC" may be created to provide additional information for *
* diagnostic purposes.   For example,  if "RC" is -6 or -7 then "SRC" *
* is the _key.n  or  _fld.n element number,  respectively,  where the *
* error was detected.                                                 *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX function  is  as easy as you would *
* invoke any other REXX  function.   Like other REXX functions,  this *
* can take  the  form  of  either  an  invoked  function  or a called *
* subroutine.  For example, the following are exactly equivalent:     *
*                                                                     *
*   result = SLEEP(1); /* invoked as a function  */                   *
*   Call SLEEP 1;      /* called as a subroutine */                   *
*                                                                     *
* Meaning: When called as a subroutine, the returned result from a    *
*          REXX function  is  not  discarded  but is automatically    *
*          assigned to the REXX "RESULT" special variable.            *
*                                                                     *
*      That said, the following is an example of sequentially reading *
* an entire file  and  displaying  the  fields (notice that,  in this *
* case,  the key field names  match  the  record  field names but the *
* offsets are different because they are relative to the beginning of *
* the key and to the beginning of the record, respectively):          *
*                                                                     *
*   myfile. = '';            /* initialize stem variable */           *
*                                                                     *
*   myfile._key.0 = 2;       /* define key layout */                  *
*   myfile._key.1 = 'FIELD_NAME_2' 'CHAR'    0 10;                    *
*   myfile._key.2 = 'FIELD_NAME_3' 'ZONED'  10  8  0;                 *
*                                                                     *
*   myfile._fld.0 = 6;       /* define record layout */               *
*   myfile._fld.1 = 'FIELD_NAME_1' 'BINARY'  0  8  0;                 *
*   myfile._fld.2 = 'FIELD_NAME_2' 'CHAR'    8 10;                    *
*   myfile._fld.3 = 'FIELD_NAME_3' 'ZONED'  18  8  0;                 *
*   myfile._fld.4 = 'FIELD_NAME_4' 'CHAR'   26 30;                    *
*   myfile._fld.5 = 'FIELD_NAME_5' 'PACKED' 56  7  2;                 *
*   myfile._fld.6 = 'FIELD_NAME_6' 'BINARY' 63  4  0;                 *
*                                                                     *
*                            /* open the dataset */                   *
*   Call RXVSAMIO 'MYFILE', 'OPEN';                                   *
*                                                                     *
*   If rc == 0 Then Do       /* begin dataset retrieval */            *
*     Say "List of file fields:";                                     *
*     Say " ";                                                        *
*     r = 0;                                                          *
*     Do Until rc <> 0       /* rc of -1 indicates end-of-file */     *
*       Call RXVSAMIO 'MYFILE', 'READ';                               *
*       If rc == 0 Then Do                                            *
*         r = r + 1;                                                  *
*         Do i = 1 To myfile._fld.0                                   *
*           pfx = 'Record' r', field' i',' Word(myfile._fld.i,1);     *
*           val = Value('myfile._fld.'Word(myfile._fld.i,1));         *
*           typ = Word(myfile._fld.i,2);                              *
*           Select                                                    *
*             When val == '*NULL',                                    *
*             |    val == '*HIGH' Then Say pfx "=" val;               *
*             When typ == 'HEX'   Then Say pfx "= x'"val"'";          *
*             When typ == 'CHAR'  Then Say pfx "= '"val"'";           *
*             Otherwise                Say pfx "=" val;               *
*           End                                                       *
*         End                                                         *
*         Say " ";                                                    *
*       End                                                           *
*     End                                                             *
*   End                                                               *
*                            /* if no errors, close the dataset */    *
*   If rc == 0 | rc == -1 Then Do                                     *
*     Call RXVSAMIO 'MYFILE', 'CLOSE';                                *
*   End                                                               *
*                            /* report any errors */                  *
*   If rc <> 0 Then Do                                                *
*     If rc <> 16 Then                                                *
*       Say "RXVSAMIO Call error: rc="rc", result="result",",         *
*                                "ddname="myfile._ddn",",             *
*                                "request="myfile._lfn",",            *
*                                "macro="myfile._lfm;                 *
*     Else Do                                                         *
*       Say "RXVSAMIO" myfile._lfn "error --" myfile._ddn "--",       *
*                     "rc="myfile._rc", ec="myfile._ec"," myfile._lfm;*
*     End                                                             *
*     Exit 4;                                                         *
*   End                                                               *
*                                                                     *
*                                                                     *
*      ...and,  using the above key & field definitions and the error *
* reporting, the following is an example of checking to see if a rec- *
* ord exists before trying to delete it.                              *
*                                                                     *
*                            /* open the dataset for update */        *
*   CALL RXVSAMIO 'MYFILE', 'OPEN', 'update', 'random';               *
*                                                                     *
*   If rc == 0 Then Do       /* random retrieval of record */         *
*     myfile._key.FIELD_NAME_2 = "5712434-10"; /* order number */     *
*     myfile._key.FIELD_NAME_3 = 12;           /* line number */      *
*     CALL RXVSAMIO 'MYFILE', 'READ', 'key';                          *
*   End                                                               *
*                                                                     *
*   If rc == 16,                                                      *
*   &  myfile._rc == 8,      /* not found? */                         *
*   &  myfile._ec == 16 Then                                          *
*     rc = 0;                /* allow "not found" */                  *
*   Else Do                                                           *
*     If rc == 0,            /* correct record key? */                *
*     &  myfile._fld.FIELD_NAME_2 == myfile._key.FIELD_NAME_2,        *
*     &  myfile._fld.FIELD_NAME_3 == myfile._key.FIELD_NAME_3,        *
*     Then Do                /* delete the record */                  *
*       CALL RXVSAMIO 'MYFILE', 'DELETE';                             *
*     End                                                             *
*   End                                                               *
*                            /* if no errors, close the dataset */    *
*   If rc == 0 | rc == -1 Then Do                                     *
*     Call RXVSAMIO 'MYFILE', 'CLOSE';                                *
*   End                                                               *
*                                                                     *
*                                                                     *
*      ...and,  again (using the  original  field definitions and the *
* error reporting,  but with an RBA Key definition), the following is *
* an example of  using  an  RBA  to  begin forward record processing. *
* (Note that this same scenario would work for an RRDS dataset except *
* that you would use an RRN key instead of an RBA address.)           *
*                                                                     *
*   myfile. = '';            /* initialize stem variable */           *
*                                                                     *
*   myfile._key.0 = 1;       /* define the RBA address key */         *
*   myfile._key.1 = 'MYFILE_RBA_ADR'  'UBIN'  0  4  0;                *
*                                                                     *
*   /* same record layout as before */                                *
*                                                                     *
*                            /* open the dataset for input */         *
*   CALL RXVSAMIO 'MYFILE', 'OPEN', 'input', 'dynamic';               *
*                                                                     *
*   If rc == 0 Then Do       /* position for forward processing */    *
*     myfile._key.MYFILE_RBA_ADR = 7504;   /* RBA in decimal */       *
*     CALL RXVSAMIO 'MYFILE', 'FWDPOS', 'rba';                        *
*   End                                                               *
*                                                                     *
*   If rc == 0 Then Do       /* begin dataset retrieval */            *
*     Say "List of file keys:";                                       *
*     Say " ";                                                        *
*     r = 0;                                                          *
*     Do Until rc <> 0       /* rc of -1 indicates end-of-file */     *
*       Call RXVSAMIO 'MYFILE', 'READ';                               *
*       If rc == 0 Then Do                                            *
*         r = r + 1;                                                  *
*         Say "Record" r", rba =" myfile._rba",",                     *
*                         "key =" myfile._fld.FIELD_NAME_2,           *
*                                 myfile._fld.FIELD_NAME_3;           *
*       End                                                           *
*     End                                                             *
*   End                                                               *
*                            /* if no errors, close the dataset */    *
*   If rc == 0 | rc == -1 Then Do                                     *
*     Call RXVSAMIO 'MYFILE', 'CLOSE';                                *
*   End                                                               *
*                                                                     *
*                                                                     *
*      ...and,  finally (using the  original  key & field definitions *
* and the error reporting,  above),  the  following  is an example of *
* starting at the end of a key range and reading backwards to process *
* all records in the key range  in a generic manner.   If you have an *
* exact (full) key  (or  if  the  entire  key  is  high-values -- for *
* retrieving the last record in  the  file),  then you can just start *
* there with BWDPOS.   Otherwise, you need to use FWDPOS to set up to *
* get the next full key and then use BWDPOS to proceed backwards from *
* there -- as demonstrated below.                                     *
*                                                                     *
*   desired_order_no = "5712434-10"; /* process this order */         *
*                            /* open the dataset for input */         *
*   CALL RXVSAMIO 'MYFILE', 'OPEN', 'input', 'dynamic';               *
*                                                                     *
*   If rc == 0 Then Do       /* point to next key range */            *
*     myfile._key.FIELD_NAME_2 = desired_order_no;                    *
*     myfile._key.FIELD_NAME_3 = '*HIGH';      /* end of range */     *
*     CALL RXVSAMIO 'MYFILE', 'FWDPOS', 'key', 'gteq';                *
*   End                                                               *
*                                                                     *
*   If rc == 0 Then Do       /* get record from next key range */     *
*     Call RXVSAMIO 'MYFILE', 'READ', 'next';                         *
*     If rc == 0 Then Do     /* reverse file direction */             *
*       myfile._key.FIELD_NAME_2 = myfile._fld.FIELD_NAME_2           *
*       myfile._key.FIELD_NAME_3 = myfile._fld.FIELD_NAME_3           *
*       CALL RXVSAMIO 'MYFILE', 'BWDPOS', 'key';                      *
*     End                                                             *
*     If rc == 0 Then Do     /* get same record but in reverse */     *
*       CALL RXVSAMIO 'MYFILE', 'READ', 'prev';                       *
*     End                                                             *
*   End                                                               *
*                                                                     *
*   If rc == 0 Then Do       /* get the desired key range */          *
*     Say "List of file keys:";                                       *
*     Say " ";                                                        *
*     r = 0;                                                          *
*     Do Until rc <> 0       /* rc of -1 indicates end-of-file */     *
*       Call RXVSAMIO 'MYFILE', 'READ', 'prev'                        *
*       If rc == 0,                                                   *
*       &  myfile._fld.FIELD_NAME_2 <> desired_order_no,              *
*       Then Do                                                       *
*         rc == -1;          /* end of desired key range */           *
*       End                                                           *
*       If rc == 0 Then Do                                            *
*         r = r + 1;                                                  *
*         Say "Record" r", key =" myfile._fld.FIELD_NAME_2,           *
*                                 myfile._fld.FIELD_NAME_3;           *
*       End                                                           *
*     End                                                             *
*   End                                                               *
*                            /* if no errors, close the dataset */    *
*   If rc == 0 | rc == -1 Then Do                                     *
*     Call RXVSAMIO 'MYFILE', 'CLOSE';                                *
*   End                                                               *
*                                                                     *
*                                                                     *
* Lastly, note that the full key fields (layout) definition is always *
* required.   But, the full record fields (layout) definition is only *
* required when writing or updating a file record.  If just reading a *
* file,  then only the record  fields  that  will  be used need to be *
* defined.   However,  even for a partial definition, the _fld.n stem *
* element numbers *must* be in sequence with *no* gaps.               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
FILETBL  DSECT
* ------------------------------------------------------------------- *
FILETBLE DS    0CL32                   FILE TABLE ENTRY
FILEDD   DS    CL7
FILEOPT  DS    XL1                     OPTION FLAGS
FILECLUS EQU   B'10000000'             DEFINED AS CLUSTER
FILEAIX  EQU   B'01000000'             DEFINED AS AIX
FILEPATH EQU   B'00100000'             DEFINED AS PATH
FILESEQU EQU   B'00000100'             ACCESS IS SEQUENTIAL VS. DIRECT
FILEAPND EQU   B'00000010'             OPENED FOR APPEND
FILEUPDT EQU   B'00000001'             OPENED FOR UPDATE
FILECAT  DS    F                       MY CAT ACB ADDRESS
FILECATR DS    F                       MY CAT RPL ADDRESS
FILEACB  DS    F                       MY ACB BASE ADDRESS
FILERPL  DS    F                       MY RPL BASE ADDRESS
FILETYPE DS    XL1                     ATTRIBUTE FLAGS
ESDS     EQU   B'01111000'             ENTRY-SEQUENCED (REVERSED)
KSDS     EQU   B'01000000'             KEY-SEQUENCED
RRDS     EQU   B'00001000'             RELATIVE-RECORD
VRDS     EQU   B'01001000'             VARIABLE-RELATIVE
SAMD     EQU   B'00010000'             MSAM DATASET
SAME     EQU   B'00010000'             MSAM-EXPLICIT DEFINE
SAMI     EQU   B'10010000'             MSAM-IMPLICIT DEFINE
SAMN     EQU   B'00100000'             MSAM-NOCIFORMAT
SAMB     EQU   B'00000100'             MSAM-BLOCKED
SAMV     EQU   B'00000010'             MSAM-VARIABLE
SAMF     EQU   B'00000001'             MSAM-FIXED
FILEDCI  DS    XL3                     CATALOG DATA COMP. CI NUMBER
         DS    XL1
FILEICI  DS    XL3                     CATALOG INDEX COMP. CI NUMBER
* =================================================================== *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* ------------------------------------------------------------------- *
DYNASAVE DS    18D                     STANDARD OR F4SA REG. SVAEAREA
AWORK4   DS    3A                      SAVE ADDRESSES
SAVE_R0  DS    A                       SAVE REGISTER
SAVE_R5  DS    A                       SAVE REGISTER
SAVE_RE  DS    A                       SAVE REGISTER
SAVE_RF  DS    A                       SAVE REGISTER
EPVECTOR DS    A                       REXX ENTRY-POINT VECTOR ADDRESS
EVALBLKA DS    A                       REXX EVALUATION BLOCK ADDRESS
*
DYNATBL  DS    9XL(L'FILETBLE)         BEGINNING OF FILE TABLE
DYNATBLE DS    XL8                     END OF FILE TABLE
*
PWORK16  DS    0PL16                   DOUBLE PACKED WORK FIELD
PWORK8   DS    PL8                     SINGLE PACKED WORK FIELD
         DS    XL8                     RESERVED
EWORK    DS    CL34                    EDITED WORK FIELD
CWORK    DS    CL34                    CHARACTER WORK FIELD
*
SAYPARMB DS    3F                      ARXSAY PARAMETER BLOCK
SAYPARM1 DS    CL8                     'WRITE   '
SAYPARM2 DS    XL4                     ADDRESS OF SAY OUTPUT
SAYPARM3 DS    XL4                     LENGTH OF SAY OUTPUT
*
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
DDNLENG  DS    F                       STEM NAME LENGTH
DDNAME   DS    CL8                     STEM NAME
REQUEST  DS    CL8                     ACCESS REQUEST
RQARG2   DS    CL8                     REQUEST ARGUMENT 2
RQARG3   DS    CL8                     REQUEST ARGUMENT 3
*
MACRO    DS    CL8                     LAST FAILED MACRO (IF ANY)
OPTFLAG  DS    XL1                     OPTION FLAGS
ACBERRF  EQU   B'00001000'             GET ACB ERROR INFO
RPLERRF  EQU   B'00000100'             GET RPL ERROR INFO
CTLBLKF  EQU   B'00000010'             GET CTLBLK ERROR INFO
SET_KLZ  EQU   B'00000001'             KEEP LEADING ZEROES
CINUM    DS    XL3                     CATALOG CI NUMBER
VARDATA  DS    CL256                   VARIABLE VALUE
VARNAME  DS    CL256                   VARIABLE NAME
SHOWAREA DS    XL512                   SHOWCAT/SHOWCB RESULT AREA
PARMLIST DS    XL256                   GENCB/MODCB PARMLIST AREA
*
DTEPLIST DS    A                       DTEMAN PLIST
DTEPARMS DS    0CL32                   DTEMAN PARMS
DTEREQU  DS    CL1                     REQUEST CODE
DTERETN  DS    CL1                     RETURN CODE
DTEGREG  DS    0CL10                   GREGORIAN DATE
DTEG_FIL DS    CL2                       00
DTEG_MC  DS    CL2                       CC
DTEG_YY  DS    CL2                       YY
DTEG_MM  DS    CL2                       MM
DTEG_DD  DS    CL2                       DD
DTEJULN  DS    0CL8                    JULIAN DATE
DTEJ_FIL DS    CL1                       0
DTEJ_MC  DS    CL2                       CC
DTEJ_YY  DS    CL2                       YY
DTEJ_DDD DS    CL3                       DDD
DTETOTD  DS    PL4                     TOTAL DAYS
DTEMNTH  DS    PL2                     MONTH NUMBER (01-12, beg:JAN)
DTEDAYN  DS    PL1                     DAY NUMBER (1-7, beg:SUNDAY)
DTELEAP  DS    CL1                     LEAP YEAR INDICATOR (Y/N)
DTEWORK  DS    PL4                     DTEMAN WORK AREA
*
         DS    0D                      ALIGN TRANSLATE TABLE
TRTBL    DS    CL256                   TEMPORARY TRANSLATE TABLE
         DS    0D                      ALIGN RECORD KEY
KEYBUF   DS    CL255                   STORAGE FOR RECORD KEY AREA
         DS    0D                      ALIGN RECORD AREA
RECBUF   DS    CL65535                 STORAGE FOR RECORD I/O AREA
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
*        CONTROL BLOCKS
* ------------------------------------------------------------------- *
         COPY  AMDSBDS                 VSAM DATA STATISTICS BLOCK
         IKQACB DSECT=YES              VSAM ACB DSECT
         IKQRPL DSECT=YES              VSAM RPL DESCT
         IEANTASM                      NAME/TOKEN SERVICE
*                                                                     *
         ARXENVB                       REXX ENVIRONMENT BLOCK
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXEFPL                       EXTERNAL FUNCTION PARAMETER LIST
         ARXEVALB                      REXX EVALUATION BLOCK
         DS    CL127
         ARXARGTB                      REXX PARSED ARGUMENT TABLE
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK
R1       EQU   1                       ON ENTRY, REXX EFPL REG
R2       EQU   2                       WORK REGISTER
R3       EQU   3                       WORK REGISTER
R4       EQU   4                       WORK REGISTER
R5       EQU   5                       WORK REGISTER
R6       EQU   6                       TEMPORARY DSECT BASE REGISTER
R7       EQU   7                       FILETBL ENTRY DSECT BASE REG
R8       EQU   8                       ACB/RPL DSECT BASE REGISTER
R9       EQU   9                       REXX SHARED VARIABLES CTL BLOCK
R10      EQU   10                      RXVSAMIO CSECT BASE REG 3
R11      EQU   11                      RXVSAMIO CSECT BASE REG 2
R12      EQU   12                      RXVSAMIO CSECT BASE REG 1
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MY MACROS                                                    *
* ------------------------------------------------------------------- *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
RXVSAMIO HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)DEC21',            X
               BASE=(R12,R11,R10),SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         SAM31                         SWITCH TO 31-BIT ADDRESSING
*                                       BECAUSE OF DEBUGGER: TRAPPER
         STMH  R2,R14,SAVEAREA+72      SAVE HIGH HALVES OF REGISTERS
*                                       BECAUSE OF SOME 64-BIT REG USE
*
         LA    R1,RTVPLIST             SET PARM LIST
         L     R15,=V(IEANTRT)         SET ENTRY POINT
         BASR  R14,R15                 RETRIEVE TOKEN
*
         SELECT                        IF TOKEN NOT FOUND, CREATE ONE
         WHEN  RETCODE,(EQ,CLC),=A(IEANT_NOT_FOUND)
*         WTO  'RXVSAMIO: CREATING TOKEN...'
          L    R0,SALEN                 GET LENGTH OF DYNAMIC STORAGE   00012300
          GETVIS SVA=NO                 OBTAIN DYNAMIC STORAGE AREA     00012600
          IF   R15,(NZERO,LTR),R15      IF UNABLE TO OBTAIN STORAGE     00013100
           ST  R15,RETCODE               SAVE RETURN CODE
           WTO 'RXVSAMIO: GETVIS FAILURE, RC=R15'
           L   R15,RETCODE               PASS IT BACK
           DUMP RC=(R15)                 RETURN TO SYSTEM               00006500
          ENDIF                                                         00013300
          XC   TOKEN,TOKEN              CLEAR THE TOKEN VALUE           00013400
          ST   R1,TOKEN                 PUT STORAGE ADDR IN TOKEN       00013400
          LA   R1,CRTPLIST              SET PARM LIST
          L    R15,=V(IEANTCR)          SET ENTRY POINT
          BASR R14,R15                  CREATE TOKEN
          IF   RETCODE,(NE,CLC),=A(IEANT_OK)   CREATE FAILURE?
           WTO 'RXVSAMIO: UNABLE TO CREATE TOKEN, RC=R15'
           L   R15,RETCODE               PASS IT BACK
           DUMP RC=(R15)                 RETURN TO SYSTEM               00006500
          ENDIF                                                         00013300
          L    R15,TOKEN                GET ADDRESS OF STORAGE
          USING DYNASTOR,R15            TEMPORARY ADDRESSABILITY
          LA   R1,DTEPARMS              GET ADDR OF SUBR PARM BLOCK
          ST   R1,DTEPLIST              SET UP PARAMETER LIST
          OI   DTEPLIST,X'80'           FLAG AS LAST PARAMETER
          XR   R1,R1                    BEGIN INIT OF TR TABLE
          LA   R2,TRTBL
          WHILE R1,(LT,CH),=H'256'
           STC R1,0(,R2)
           AHI R1,1
           AHI R2,1
          WEND
          LA   R2,TRTBL+C'a'            Translate lower-case
          MVC  0(9,R2),=C'ABCDEFGHI'       to upper-case
          LA   R2,TRTBL+C'j'            Translate lower-case
          MVC  0(9,R2),=C'JKLMNOPQR'       to upper-case
          LA   R2,TRTBL+C's'            Translate lower-case
          MVC  0(8,R2),=C'STUVWXYZ'        to upper-case
          LA   R2,TRTBL+X'FA'           Translate unpacked data
          MVC  0(6,R2),=C'ABCDEFGHI'       to valid hex
          MVC  DYNATBLE,ENDMARK         MARK END OF FILE TABLE
          DROP R15                      REMOVE ADDRESSABILITY
*
         WHEN  RETCODE,(NE,CLC),=A(IEANT_OK)   OTHER FAILURE?
          WTO  'RXVSAMIO: UNABLE TO RETRIEVE TOKEN, RC=R15'
          L    R15,RETCODE              PASS IT BACK
          DUMP RC=(R15)                 RETURN TO SYSTEM                00006500
         ENDSL                         END OF TOKEN HANDLING
*
         L     R15,4(,R13)             GET ADDR OF CALLER'S SAVEAREA
         LM    R0,R1,20(R15)           RESTORE REGS 0 & 1 FROM CALLER   00013500
         L     R13,TOKEN               GET ADDR OF DYNAMIC SAVE AREA
         ST    R13,8(,R15)             SET NEXT SAVEAREA IN CALLER'S
         USING DYNASTOR,R13            PERMANENT DYNAMIC STORAGE
         MVC   DYNASAVE(128),SAVEAREA  COPY INTERNAL SAVEAREA THERE
         XR    R15,R15                 CLEAR RETURN CODE REGISTER
         XC    OPTFLAG,OPTFLAG         CLEAR OPTIONS FLAG
*
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5             TEMPORARY ADDRESSABILITY
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RXVSAMIO: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LHI  R15,12                  SET ABNORMAL RETURN CODE
          DUMP RC=(R15)                RETURN TO SYSTEM                 00006500
         ENDIF
         L     R6,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         ST    R6,EPVECTOR             SAVE IT
         DROP  R5                      REMOVE ADDRESSABILITY
*
         LR    R8,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R8                 TEMPORARY ADDRESSABILITY
         L     R6,EFPLEVAL             REXX EVALUATION BLOCK POINTER
         L     R6,0(,R6)               REXX EVALUATION BLOCK
         ST    R6,EVALBLKA             SAVE IT
         USING EVALBLOCK,R6            TEMPORARY ADDRESSABILITY
         MVI   EVALBLOCK_EVDATA,X'00'  INIT FIRST BYTE OF RESULTS BUF
         DROP  R6                      REMOVE ADDRESSABILITY
*
         LA    R9,SHVCTLB              STORAGE FOR
         USING SHVBLOCK,R9              SHARED VARIABLES CONTROL BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R1,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R3,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R4,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R1,R4,SHVPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
         OI    SHVPARMB+(4*4-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         XC    VARNAME,VARNAME         CLEAR BUFFER FOR VAR. NAME
         XC    VARDATA,VARDATA         CLEAR BUFFER FOR VAR. VALUE
         LA    R1,VARNAME              GET ADDRESS FOR SHVNAMA
         LHI   R2,L'VARNAME            GET LENGTH  FOR SHVNAML
         LA    R3,VARDATA              GET ADDRESS FOR SHVVALA
         LHI   R4,L'VARDATA            GET LENGTH  FOR SHVVALL
         STM   R1,R4,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R2,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R4,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
* OUTPUT TO SYSLST -- ARXSAY
*
         LA    R1,SAYPARM1             GET ADDRESS OF PARM 1
         LA    R2,SAYPARM2             GET ADDRESS OF PARM 2
         LA    R3,SAYPARM3             GET ADDRESS OF PARM 3
         STM   R1,R3,SAYPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
         OI    SAYPARMB+(4*3-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
         MVC   SAYPARM1,=CL8'WRITE'    SET PARM 1
         LA    R1,VARDATA              GET ADDRESS FOR PARM 2
         LHI   R2,L'VARDATA            GET LENGTH FOR PARM 3
         STM   R1,R2,SAYPARM2          PUT IN PARMS 2 AND 3

*        MVC   VARDATA+0(18),=C'RXVSAMIO Load at: '
*        ST    R12,AWORK4              STORE BASE ADDRESS
*        UNPK  EWORK(9),AWORK4(5)      UNPACK BASE ADDRESS
*        TR    EWORK(8),TRTBL           AND TRANSLATE TO PRINTABLE      00015800
*        MVC   VARDATA+18(8),EWORK     THEN PUT IN MSG AREA
*        LHI   R2,26                   SET LENGTH OF OUTPUT
*        BAS   R5,SAYIT                GO SAY IT
*
* POINT TO ARGUMENT(S)
*
         L     R6,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R6       TEMPORARY ADDRESSABILITY
         DROP  R8                      REMOVE ADDRESSABILITY
         LR    R7,R6                   REXX ARGUMENT STRING
         USING ARGSTRING,R7            TEMPORARY ADDRESSABILITY
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    DDNLENG,DDNLENG
         MVC   DDNAME,SPACES
         MVC   REQUEST,SPACES
         MVC   RQARG2,SPACES
         MVC   RQARG3,SPACES
         MVC   MACRO,SPACES
*
         WHILE ARGTABLE_END,(NE,CLC),ENDMARK
          L    R2,ARGTABLE_ARGSTRING_LENGTH
          IF   ARGTABLE_ARGSTRING_LENGTH,(NH,CLC),=F'8'
           LR  R3,R2                    GET ARG LENGTH
          ELSE
           LHI R3,8                     SET MAX LENGTH
          ENDIF
          SHI  R3,1                      AND DECR. FOR EXECUTE
          L    R4,ARGTABLE_ARGSTRING_PTR

          SELECT
          WHEN DDNAME,(NH,CLC),SPACES  WORKING ON 1ST ARGUMENT?
           ST  R2,DDNLENG               SAVE DDNAME LENGTH
           LA  R5,DDNAME                SET DEST ADDR
           EX  R3,MVPARM                MOVE ARG DATA
           TR  DDNAME,TRTBL             TRANSLATE TO UPPER-CASE
           IF  DDNLENG,(HT,CLC),=F'7',OR,                              X
               DDNAME,(NH,CLC),SPACES   DDNAME INVALID?
            LHI R5,4                     SET RETURN CODE
            B  R15ZERO                   GO RETURN ERROR
           ENDIF                        ENDIF
          WHEN REQUEST,(NH,CLC),SPACES WORKING ON 2ND ARGUMENT?
           LA  R5,REQUEST               SET DEST ADDR
           EX  R3,MVPARM                MOVE ARG DATA
           TR  REQUEST,TRTBL            TRANSLATE TO UPPER-CASE
           IF  REQUEST,(NH,CLC),SPACES  REQUEST MISSING?
            LHI R5,1                     SET RETURN CODE
            B  R15ZERO                   GO RETURN ERROR
           ENDIF                        ENDIF
          WHEN RQARG2,(NH,CLC),SPACES  WORKING ON 3RD ARGUMENT?
           LA  R5,RQARG2                SET DEST ADDR
           EX  R3,MVPARM                MOVE ARG DATA
           TR  RQARG2,TRTBL             TRANSLATE TO UPPER-CASE
           IF  RQARG2,(NH,CLC),SPACES   REQU ARG2 MISSING?
            LHI R5,2                     SET RETURN CODE
            B  R15ZERO                   GO RETURN ERROR
           ENDIF                        ENDIF
          WHEN RQARG3,(NH,CLC),SPACES  WORKING ON 4TH ARGUMENT?
           LA  R5,RQARG3                SET DEST ADDR
           EX  R3,MVPARM                MOVE ARG DATA
           TR  RQARG3,TRTBL             TRANSLATE TO UPPER-CASE
           IF  RQARG3,(NH,CLC),SPACES   REQU ARG3 MISSING?
            LHI R5,3                     SET RETURN CODE
            B  R15ZERO                   GO RETURN ERROR
           ENDIF                        ENDIF
          WHEN OTHER                   TOO MANY ARGUMENTS
           LHI R5,4                     SET RETURN CODE
           B   R15ZERO                  GO RETURN ERROR
MVPARM   MVC   0(0,R5),0(R4)           (SEE "EX" INSTRUCTION ABOVE)
          ENDSL

          LA   R6,L'ARGTABLE_END(,R6)  ADVANCE TO NEXT ARGUMENT
          LR   R7,R6
         WEND
*
         DROP  R6                      REMOVE ADDRESSABILITY
         DROP  R7                      REMOVE ADDRESSABILITY
*
* READY TO PROCEED?
*
         IF    DDNAME,(NH,CLC),SPACES  DIDN'T GET ANY ARGUMENTS?
          LHI  R5,4                     SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF

         SELECT                        CHECK REQUEST ARGUMENTS

         WHEN  REQUEST,EQ,C'A'         ATTRS
          MVC  REQUEST,=CL8'ATTRS'
          IF   RQARG2,(HT,CLC),SPACES   2ND ARG NOT ALLOWED
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG3,(HT,CLC),SPACES   3RD ARG NOT ALLOWED
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  REQUEST,EQ,C'O'         OPEN
          MVC  REQUEST,=CL8'OPEN'
          IF   RQARG2,(NH,CLC),SPACES   NO 2ND REQ ARG?
           MVI RQARG2,C'I'               DEFAULT=INPUT
          ENDIF
          IF   RQARG2,NE,C'I'           INPUT
          AND  RQARG2,NE,C'U'           UPDATE
          AND  RQARG2,NE,C'O'           OUTPUT
           MVI RQARG3,C'S'               FORCE TO SEQUENCE
           IF  RQARG2,NE,C'A'            APPEND
           AND RQARG2(2),(NE,CLC),=C'RE' RESET
            LHI R5,2                      SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF
          ENDIF
          IF   RQARG3,(NH,CLC),SPACES   NO 3RD REQ ARG?
           MVI RQARG3,C'S'               DEFAULT=SEQUENCE
          ENDIF
          IF   RQARG3,NE,C'S'           SEQUENCE
          AND  RQARG3(2),(NE,CLC),=C'RA' RANDOM
          AND  RQARG3,NE,C'D'           DYNAMIC
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  REQUEST,EQ,C'F',OR,REQUEST,EQ,C'B'
          IF   REQUEST,EQ,C'B'         BWDPOS
           MVC REQUEST,=CL8'BWDPOS'
          ELSE                         FWDPOS
           MVC REQUEST,=CL8'FWDPOS'
          ENDIF
          IF   RQARG2,(NH,CLC),SPACES   NO 2ND REQ ARG?
           MVI RQARG2,C'K'               DEFAULT=KEY
          ENDIF
          IF   RQARG2,NE,C'K'           KEY
          AND  RQARG2,NE,C'R'           RBA
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG2,EQ,C'K'           KEY
           IF  RQARG3,(NH,CLC),SPACES    NO 3RD REQ ARG?
            MVI RQARG3,C'G'               DEFAULT=GTEQ
           ENDIF
           IF  RQARG3,NE,C'G'            GTEQ
           AND RQARG3,NE,C'E'            EQUAL
            LHI R5,3                      SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF
          ELSE
           IF  RQARG3,(HT,CLC),SPACES    3RD ARG NOT ALLOWED
            LHI R5,3                      SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF
          ENDIF

         WHEN  REQUEST,EQ,C'R'         READ
          MVC  REQUEST,=CL8'READ'
          IF   RQARG2,(NH,CLC),SPACES   NO 2ND REQ ARG?
           MVI RQARG2,C'N'               DEFAULT=NEXT
           MVC RQARG3,SPACES
          ENDIF
          IF   RQARG2,NE,C'N'           NEXT
          AND  RQARG2,NE,C'P'           PREV
          AND  RQARG2,NE,C'R'           RBA
          AND  RQARG2,NE,C'K'           KEY
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG2,EQ,C'K'           KEY
           IF  RQARG3,(NH,CLC),SPACES    NO 3RD REQ ARG?
            MVI RQARG3,C'E'               DEFAULT=EQUAL
           ENDIF
           IF  RQARG3,NE,C'E'            EQUAL
           AND RQARG3,NE,C'G'            GTEQ
            LHI R5,3                      SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF
          ELSE
           IF  RQARG3,(HT,CLC),SPACES    3RD ARG NOT ALLOWED
            LHI R5,3                      SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF
          ENDIF

         WHEN  REQUEST,EQ,C'W'         WRITE
          MVC  REQUEST,=CL8'WRITE'
          IF   RQARG2,(HT,CLC),SPACES   2ND ARG NOT ALLOWED
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG3,(HT,CLC),SPACES   3RD ARG NOT ALLOWED
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  REQUEST,EQ,C'U'         UPDATE
          MVC  REQUEST,=CL8'UPDATE'
          IF   RQARG2,(HT,CLC),SPACES   2ND ARG NOT ALLOWED
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG3,(HT,CLC),SPACES   3RD ARG NOT ALLOWED
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  REQUEST,EQ,C'D'         DELETE
          MVC  REQUEST,=CL8'DELETE'
          IF   RQARG2,(HT,CLC),SPACES   2ND ARG NOT ALLOWED
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG3,(HT,CLC),SPACES   3RD ARG NOT ALLOWED
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  REQUEST,EQ,C'C'         CLOSE
          MVC  REQUEST,=CL8'CLOSE'
          IF   RQARG2,(HT,CLC),SPACES   2ND ARG NOT ALLOWED
           LHI R5,2                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF
          IF   RQARG3,(HT,CLC),SPACES   3RD ARG NOT ALLOWED
           LHI R5,3                      SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF

         WHEN  OTHER                   ANYTHING ELSE
          LHI  R5,1                     SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDSL
*
* BEGIN PROCESSING
*
*        MVC   VARDATA+0(18),=C'RXVSAMIO Request: '
*        MVC   VARDATA+18(8),DDNAME
*        MVC   VARDATA+26(8),REQUEST
*        MVC   VARDATA+34(8),RQARG2
*        MVC   VARDATA+42(8),RQARG3
*        LHI   R2,50                   SET LENGTH OF OUTPUT
*        BAS   R5,SAYIT                GO SAY IT
*
         IF    REQUEST,NE,C'A'         ATTRS
         AND   REQUEST,NE,C'C'         CLOSE
*                            make sure the record key array exists
          LA   R1,RECKEY                POINT TO STEM ELEMENT NAME
          LHI  R2,L'RECKEY              SET LENGTH OF ELEMENT NAME
          LA   R3,=C'0'                 POINT TO STEM ELEMENT VALUE
          LHI  R4,1                     SET LENGTH OF ELEMENT VALUE
          BAS  R5,GET_ELEM              RETRIEVE THE STEM ELEMENT
          IF   SHVVALL,(NH,CLC),=F'0'   IF VALUE LENGTH NOT > ZERO
           MVC MACRO,=CL8'GETVAR'        LAST FAILED MACRO
           LHI R5,-6                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF
*                            make sure the record field array exists
          LA   R1,RECFLD                POINT TO STEM ELEMENT NAME
          LHI  R2,L'RECFLD              SET LENGTH OF ELEMENT NAME
          LA   R3,=C'0'                 POINT TO STEM ELEMENT VALUE
          LHI  R4,1                     SET LENGTH OF ELEMENT VALUE
          BAS  R5,GET_ELEM              RETRIEVE THE STEM ELEMENT
          IF   SHVVALL,(NH,CLC),=F'0'   IF VALUE LENGTH NOT > ZERO
           MVC MACRO,=CL8'GETVAR'        LAST FAILED MACRO
           LHI R5,-7                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         LA    R7,DYNATBL              POINT TO THE FILE TABLE
         USING FILETBL,R7              ESTABLISH ADDRESSABILITY
         BAS   R8,FILEINIT             FIND ENTRY ON FILE TABLE
*
         LA    R1,FILETBLE             GET ADDR. OF FILE TABLE ENTRY
         LA    R2,AWORK4               GET ADDR. OF MOST OF DYNASTOR
         STM   R1,R2,SUBRPLST          CREATE SUBROUTINE PARM LIST
*
         LA    R1,=C'0'                SET VSAM RTN CODE TO ZERO
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,VSAMRC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMRC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         LA    R1,=C'0'                SET VSAM ERR CODE TO ZERO
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,VSAMEC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMEC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         LA    R1,=C'0'                SET VSAM FUNC CODE TO ZERO
         LHI   R2,1                    SET LENGTH OF VAR DATA
         LA    R3,VSAMFC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMFC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         MVI   DTEREQU,C'C'            REQUEST CURRENT DATE
         LA    R1,DTEPLIST             GET ADDRESS OF PARM BLOCK
         L     R15,=V(DTEMAN)          GET ADDRESS OF SUBROUTINE
         BALR  R14,R15                 PERFORM CONVERSION/VALIDATION
         IF    DTERETN,NE,C' '         IF ERROR RETURNED
          MVC  MACRO,=CL8'DTEMAN'       INDICATE OPERATION PERFORMED
          XR   R15,R15                  CLEAR WORK REGISTER
          IC   R15,DTERETN              INSERT RETURN CODE CHARACTER
          LHI  R5,8                     SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
* EXECUTE REQUEST
*
         SELECT
* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'A'         "ATTRS"
          L    R8,FILEACB               GET FILE ACB ADDRESS
          USING IKQACB,R8               TEMPORARY ADDRESSABILITY
          IF   ACBOFLGS,(ON,TM),ACBOPEN IF THE ACB IS OPEN
           BAS R6,CLOSEACB               CLOSE IT
          ENDIF                         ENDIF
          DROP R8                       REMOVE ADDRESSABILITY

          MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(IN,SEQ,NRS)
          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
          MODCB MF=(E,PARMLIST)          MODIFY ACB
          POP  ACONTROL
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'MODCB/A'       INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          L    R8,FILEACB               GET FILE ACB ADDRESS
          BAS  R6,OPENACB               OPEN THE ACB

          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMXA)         GET SUBR. ENTRY POINT
          BASR R14,R15                  RETRIEVE VOLUME LIST
          IF   R15,(NZ,LTR),R15         IF FAILED
           IF  OPTFLAG,(ON,TM),RPLERRF   IF RPL ERROR OCCURRED...
            L  R8,FILECATR                GET CATALOG RPL ADDRESS
            BAS R5,VSAMERRC               PASS BACK RTN AND ERR CODES
            LHI R5,16                     SET RETURN CODE
            B  RETERR                     GO RETURN ERROR
           ELSE                          ELSE OTHER ERROR OCCURRED...
            LR R5,R15                     SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          L    R8,FILEACB               GET FILE ACB ADDRESS
          BAS  R6,CLOSEACB              CLOSE THE ACB

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'O'         "OPEN"
          IF   RQARG2,EQ,C'I'           IF INPUT
           IF  RQARG3,EQ,C'S'            SEQUENCE
            MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(IN,SEQ)
            OI FILEOPT,FILESEQU           INDICATE ACCESS SEQUENTIAL
           ELSE
            NI FILEOPT,X'FF'-FILESEQU     INDICATE ACCESS DIRECT
            IF RQARG3,EQ,C'D'             DYNAMIC
             MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(IN,SKP)
            ELSE                          RANDOM
             MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(IN,DIR)
            ENDIF
           ENDIF
          ELSE                          ELSE
           IF  RQARG3,EQ,C'S'            SEQUENCE
            MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(OUT,SEQ)
            OI FILEOPT,FILESEQU           INDICATE ACCESS SEQUENTIAL
           ELSE
            NI FILEOPT,X'FF'-FILESEQU     INDICATE ACCESS DIRECT
            IF RQARG3,EQ,C'D'             DYNAMIC
             MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(OUT,SKP)
            ELSE                          RANDOM
             MODCB MF=(L,PARMLIST),ACB=(*,FILEACB),MACRF=(OUT,DIR)
            ENDIF
           ENDIF
           IF  RQARG2,EQ,C'U'            IF UPDATE
            OI FILEOPT,FILEUPDT           INDICATE OPENED FOR UPDATE
            NI FILEOPT,X'FF'-FILEAPND
           ELSE                          ELSE
            NI FILEOPT,X'FF'-FILEUPDT     NOT OPENED FOR UPDATE
            IF RQARG2,EQ,C'A'             IF APPEND
             OI FILEOPT,FILEAPND           INDICATE OPENED FOR APPEND
            ELSE                          ELSE
             NI FILEOPT,X'FF'-FILEAPND     NOT OPENED FOR APPEND
            ENDIF                         ENDIF
           ENDIF                         ENDIF
          ENDIF                         ENDIF
          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
          MODCB MF=(E,PARMLIST),AM=VSAM
          POP  ACONTROL
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'MODCB/A'       INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          L    R8,FILEACB               GET ACB ADDRESS
          BAS  R6,OPENACB               OPEN THE ACB

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'F',OR,REQUEST,EQ,C'B'  "FWDPOS" / "BWDPOS"
          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMBK)         GET SUBR. ENTRY POINT
          BASR R14,R15                  BUILD KEY BUFFER
          IF   R15,(NZ,LTR),R15         IF FAILED
           LR  R5,R15                    SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

          BAS  R5,GET_MAXV              GET KEY AND REC MAX LENGTHS

          L    R8,FILERPL               GET RPL ADDRESS
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY

          IF   RQARG2,EQ,C'R'           BY RBA
           NI  RPLOPT1,X'FF'-RPLKEY      NOT KEY PROCESSING
           OI  RPLOPT1,RPLADR            SET ADR PROCESSING
           NI  RPLOPT2,X'FF'-RPLKGE      NOT KGE PROCESSING
           NI  RPLOPT2,X'FF'-RPLGEN      NOT GENERIC PROCESSING
          ELSE                          BY KEY
           OI  RPLOPT1,RPLKEY            SET KEY PROCESSING
           NI  RPLOPT1,X'FF'-RPLADR      NOT ADR PROCESSING
           IF  RQARG3,EQ,C'E'            IF EQUAL
            NI RPLOPT2,X'FF'-RPLKGE       NOT KGE PROCESSING
           ELSE                          ELSE GTEQ
            OI RPLOPT2,RPLKGE             SET KGE PROCESSING
           ENDIF                         ENDIF
           IF  FILETYPE,(O,TM),KSDS      IF KEY-SEQUENCED
           AND FILETYPE,(NO,TM),RRDS     AND NOT RELATIVE-RECORD
            LA R1,KEYLEN                  POINT TO STEM ELEMENT NAME
            LHI R2,L'KEYLEN               SET LENGTH OF ELEMENT NAME
            BAS R5,GET_STEM               GET GENERIC KEY LENGTH
            IF SHVVALL,(NH,CLC),=F'0'     IF NO DEF KEY LEN SUPPLIED
             L R2,KMAX                     USE MAX KEY LEN
            ELSE                          ELSE
             LA R1,VARDATA                 POINT TO ZONED KEY LENGTH
             L R2,SHVVALL                  GET LENGTH OF VAR DATA
             BAS R5,ZONE2REG               CONVERT TO BINARY IN REG2
            ENDIF                         ENDIF
            IF R2,(LT,C),=F'1',OR,        IF VALUE < 1                 +
               R2,(HT,C),KMAX             OR VALUE > MAX KEY LEN
             MVC MACRO,=CL8'POINT_X'       LAST FAILED OPERATION
             LHI R5,-8                     SET RETURN CODE
             B R15ZERO                     GO RETURN ERROR
            ENDIF                         ENDIF
            IF R2,(LT,C),KMAX             IF REQ KEY LEN < MAX KEY LEN
             STH R2,RPLKEYL                SET KEY LENGTH FOR POSITION
             OI RPLOPT2,RPLGEN             SET GENERIC PROCESSING
            ELSE                          ELSE
             NI RPLOPT2,X'FF'-RPLGEN       NOT GENERIC PROCESSING
             L R2,KMAX                     GET MAX KEY LENGTH
             STH R2,RPLKEYL                SET KEY LENGTH FOR POSITION
            ENDIF                         ENDIF
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          IF   FILEOPT,(ON,TM),FILESEQU IF ACCESS IS SEQUENTIAL
           OI  RPLOPT1,RPLSEQ            SET SEQ PROCESSING
           NI  RPLOPT1,X'FF'-RPLDIR      NOT DIR PROCESSING
          ELSE                          ACCESS IS DIRECT
           NI  RPLOPT1,X'FF'-RPLSEQ      NOT SEQ PROCESSING
           OI  RPLOPT1,RPLDIR            SET DIR PROCESSING
          ENDIF                         ENDIF

          IF   FILEOPT,(ON,TM),FILEUPDT IF OPENED FOR UPDATE
           OI  RPLOPT1,RPLUPD            SET UPD PROCESSING
           NI  RPLOPT2,X'FF'-RPLNSP      NOT NSP PROCESSING
           NI  RPLOPT2,X'FF'-RPLNUP      NOT NUP PROCESSING
          ELSE                          ELSE NOT UPDATE
           NI  RPLOPT1,X'FF'-RPLUPD      NOT UPD PROCESSING
           IF  FILEOPT,(ON,TM),FILESEQU  IF ACCESS IS SEQUENTIAL
            NI RPLOPT2,X'FF'-RPLNSP       NOT NSP PROCESSING
            OI RPLOPT2,RPLNUP             SET NUP PROCESSING
           ELSE                          ELSE ACCESS IS DIRECT
            OI RPLOPT2,RPLNSP             SET NSP PROCESSING
            NI RPLOPT2,X'FF'-RPLNUP       NOT NUP PROCESSING
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          IF   REQUEST,EQ,C'B'          BWDPOS
           OI  RPLOPT2,RPLBWD            SET BACKWARD PROCESSING
           NI  RPLOPT2,X'FF'-RPLLRD      SET ANY RECORD PROCESSING
           LA  R1,KEYBUF                 POINT TO KEY BUFFER
           L   R2,KMAX                   SET LENGTH OF KEY BUFFER
           SHI R2,1                      DECREMENT BEFORE EXECUTE
           LA  R3,HIVAL                  CHECK FOR HIGH VALUES
           IF  R2,(EQ,EX),CHECK_IT       IF HIGH-VALUES
            OI RPLOPT2,RPLLRD             SET LAST RECORD PROCESSING
           ENDIF                         ENDIF
          ELSE                          FWDPOS
           NI  RPLOPT2,X'FF'-RPLBWD      SET FORWARD PROCESSING
           NI  RPLOPT2,X'FF'-RPLLRD      SET ANY RECORD PROCESSING
          ENDIF                         ENDIF

          DROP R8                       REMOVE ADDRESSABILITY

          POINT RPL=(R8)                POINT TO A RECORD
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'POINT'         INDICATE OPERATION PERFORMED
           OI  OPTFLAG,RPLERRF           SIGNAL ERROR IN RPL
           BAS R5,VSAMERRC               PASS BACK RTN AND ERR CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
CHECK_IT CLC   0(0,R1),0(R3)           (SEE "EX" INSTRUCTION ABOVE)
          ENDIF                         ENDIF

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'R'         "READ"
          IF   RQARG2,EQ,C'K',OR,RQARG2,EQ,C'R'
           LA  R1,SUBRPLST               POINT TO PARAMETER LIST
           L   R15,=V(RXVSAMBK)          GET SUBR. ENTRY POINT
           BASR R14,R15                  BUILD KEY BUFFER
           IF  R15,(NZ,LTR),R15          IF FAILED
            LR R5,R15                     SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          BAS  R5,GET_MAXV              GET KEY AND REC MAX LENGTHS

          L    R8,FILERPL               GET RPL ADDRESS
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY

          IF   FILETYPE,(O,TM),KSDS     IF KEY-SEQUENCED
          AND  FILETYPE,(NO,TM),RRDS    AND NOT RELATIVE-RECORD
           LA  R1,KEYLEN                 POINT TO STEM ELEMENT NAME
           LHI R2,L'KEYLEN               SET LENGTH OF ELEMENT NAME
           BAS R5,GET_STEM               GET GENERIC KEY LENGTH
           IF  SHVVALL,(NH,CLC),=F'0'    IF NO DEF KEY LEN SUPPLIED
            L  R2,KMAX                    USE MAX KEY LEN
           ELSE                          ELSE
            LA R1,VARDATA                 POINT TO ZONED KEY LENGTH
            L  R2,SHVVALL                 GET LENGTH OF VAR DATA
            BAS R5,ZONE2REG               CONVERT TO BINARY IN REG2
           ENDIF                         ENDIF
           IF  R2,(LT,C),=F'1',OR,       IF VALUE < 1                  +
               R2,(HT,C),KMAX            OR VALUE > MAX KEY LEN
            MVC MACRO,=CL8'READ_X'        LAST FAILED OPERATION
            LHI R5,-8                     SET RETURN CODE
            B  R15ZERO                    GO RETURN ERROR
           ENDIF                         ENDIF
           IF  R2,(LT,C),KMAX            IF REQ KEY LEN < MAX KEY LEN
            STH R2,RPLKEYL                SET KEY LENGTH TO READ
            OI RPLOPT2,RPLGEN             SET GENERIC PROCESSING
           ELSE                          ELSE
            NI RPLOPT2,X'FF'-RPLGEN       NOT GENERIC PROCESSING
            L  R2,KMAX                    GET MAX KEY LENGTH
            STH R2,RPLKEYL                SET KEY LENGTH TO READ
           ENDIF                         ENDIF
          ENDIF                         ENDIF
          L    R2,RMAX                  GET MAX RECORD LENGTH
          ST   R2,RPLRLEN               SET RECORD LENGTH TO READ

          IF   RQARG2,EQ,C'N',OR,RQARG2,EQ,C'P'  IF NEXT/PREV
           OI  RPLOPT1,RPLSEQ            SET SEQUENTIAL PROCESSING
           NI  RPLOPT1,X'FF'-RPLDIR      NOT DIRECT PROCESSING
           IF  RQARG2,EQ,C'P'            IF PREV
            OI RPLOPT2,RPLBWD             SET BACKWARD PROCESSING
           ELSE                          ELSE NEXT
            NI RPLOPT2,X'FF'-RPLBWD       SET FORWARD PROCESSING
           ENDIF                         ENDIF
           NI  RPLOPT2,X'FF'-RPLLRD      SET ANY RECORD PROCESSING
          ELSE                          BY KEY/RBA
           NI  RPLOPT2,X'FF'-RPLBWD      SET FORWARD PROCESSING
           NI  RPLOPT2,X'FF'-RPLLRD      SET ANY RECORD PROCESSING
           IF  RQARG2,EQ,C'R'            BY RBA
            NI RPLOPT1,X'FF'-RPLKEY       NOT KEY PROCESSING
            OI RPLOPT1,RPLADR             SET ADR PROCESSING
           ELSE                          BY KEY
            OI RPLOPT1,RPLKEY             SET KEY PROCESSING
            NI RPLOPT1,X'FF'-RPLADR       NOT ADR PROCESSING
            IF RQARG3,EQ,C'G'             IF GTEQ
             OI RPLOPT2,RPLKGE             SET KGE PROCESSING
            ELSE                          ELSE EQUAL
             NI RPLOPT2,X'FF'-RPLKGE       NOT KGE PROCESSING
            ENDIF                         ENDIF
           ENDIF                         ENDIF
           IF  FILEOPT,(ON,TM),FILESEQU  IF ACCESS IS SEQUENTIAL
            OI RPLOPT1,RPLSEQ             SET SEQ PROCESSING
            NI RPLOPT1,X'FF'-RPLDIR       NOT DIR PROCESSING
           ELSE                          ACCESS IS DIRECT
            NI RPLOPT1,X'FF'-RPLSEQ       NOT SEQ PROCESSING
            OI RPLOPT1,RPLDIR             SET DIR PROCESSING
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          IF   FILEOPT,(ON,TM),FILEUPDT IF OPENED FOR UPDATE
           OI  RPLOPT1,RPLUPD            SET UPD PROCESSING
           NI  RPLOPT2,X'FF'-RPLNSP      NOT NSP PROCESSING
           NI  RPLOPT2,X'FF'-RPLNUP      NOT NUP PROCESSING
          ELSE                          ELSE NOT UPDATE
           NI  RPLOPT1,X'FF'-RPLUPD      NOT UPD PROCESSING
           IF  FILEOPT,(ON,TM),FILESEQU  IF ACCESS IS SEQUENTIAL
            NI RPLOPT2,X'FF'-RPLNSP       NOT NSP PROCESSING
            OI RPLOPT2,RPLNUP             SET NUP PROCESSING
           ELSE                          ELSE ACCESS IS DIRECT
            OI RPLOPT2,RPLNSP             SET NSP PROCESSING
            NI RPLOPT2,X'FF'-RPLNUP       NOT NUP PROCESSING
           ENDIF                         ENDIF
          ENDIF                         ENDIF

          DROP R8                       REMOVE ADDRESSABILITY

          BAS  R6,GETREC                READ THE FILE

          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
          SHOWCB RPL=(8),AREA=(S,SHOWAREA),LENGTH=L'SHOWAREA,          +
               FIELDS=(RECLEN,RBA)
          POP  ACONTROL
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'SHOWCB/R'      INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LGF  R2,SHOWAREA+0            GET REC LEN INTO 64-BIT REG
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,RECLEN                POINT BACK TO VAR NAME
          LHI  R4,L'RECLEN              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          LA   R1,CWORK                 POINT TO "RESULT" AREA
          LLGF R2,SHOWAREA+4            GET LAST RBA INTO 64-BIT REG
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,RECRBA                POINT BACK TO VAR NAME
          LHI  R4,L'RECRBA              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT

          IF   FILETYPE,(ON,TM),RRDS    IF RELATIVE-RECORD DATASET
           LA  R1,CWORK                  POINT TO "RESULT" AREA
           LLGF R2,KEYBUF                GET LAST RRN INTO 64-BIT REG
           BAS R5,REG2ZONE               CONVERT THAT TO ZONED-DECIMAL
           LA  R3,RECRRN                 POINT BACK TO VAR NAME
           LHI R4,L'RECRRN               SET LENGTH OF VAR NAME
           BAS R5,SET_STEM               CREATE THE STEM ELEMENT
          ENDIF                         ENDIF

          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMXR)         GET SUBR. ENTRY POINT
          BASR R14,R15                  EXTRACT RECORD FIELDS
          IF   R15,(NZ,LTR),R15         IF FAILED
           LR  R5,R15                    SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'W'         "WRITE"
          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMBK)         GET SUBR. ENTRY POINT
          BASR R14,R15                  BUILD KEY BUFFER
          IF   R15,(NZ,LTR),R15         IF FAILED
           LR  R5,R15                    SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMBR)         GET SUBR. ENTRY POINT
          BASR R14,R15                  BUILD REC BUFFER
          IF   R15,(NZ,LTR),R15         IF FAILED
           LR  R5,R15                    SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

          BAS  R5,GET_MAXV              GET KEY AND REC MAX LENGTHS

          L    R8,FILERPL               GET RPL ADDRESS
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY

          LA   R1,RECLEN                POINT TO STEM ELEMENT NAME
          LHI  R2,L'RECLEN              SET LENGTH OF ELEMENT NAME
          BAS  R5,GET_STEM              GET RECORD LENGTH
          IF   SHVVALL,(NH,CLC),=F'0'   IF NO DEF REC LEN SUPPLIED
           L   R2,RMAX                   USE MAX REC LEN
          ELSE                          ELSE
           LA  R1,VARDATA                POINT TO ZONED RECORD LENGTH
           L   R2,SHVVALL                GET LENGTH OF VAR DATA
           BAS R5,ZONE2REG               CONVERT TO BINARY IN REG2
          ENDIF                         ENDIF
          IF   R2,(LT,C),KMAX,OR,       IF VALUE < MAX KEY LEN         +
               R2,(HT,C),RMAX           OR VALUE > MAX REC LEN
           MVC MACRO,=CL8'WRITE_X'       LAST FAILED MACRO
           LHI R5,-9                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R2,RPLRLEN               SET RECORD LENGTH TO WRITE
          L    R2,KMAX                  GET MAX KEY LEN
          STH  R2,RPLKEYL               SET KEY LENGTH TO WRITE

          OI   RPLOPT1,RPLSEQ           SET SEQUENTIAL PROCESSING
          NI   RPLOPT1,X'FF'-RPLDIR     NOT DIRECT PROCESSING
          NI   RPLOPT1,X'FF'-RPLUPD     NOT UPD PROCESSING
          IF   FILEOPT,(ON,TM),FILESEQU IF ACCESS IS SEQUENTIAL
           NI  RPLOPT2,X'FF'-RPLNSP      NOT NSP PROCESSING
           OI  RPLOPT2,RPLNUP            SET NUP PROCESSING
          ELSE                          ELSE ACCESS IS DIRECT
           OI  RPLOPT2,RPLNSP            SET NSP PROCESSING
           NI  RPLOPT2,X'FF'-RPLNUP      NOT NUP PROCESSING
          ENDIF                         ENDIF
          DROP R8                       REMOVE ADDRESSABILITY

          BAS  R6,PUTREC                WRITE TO THE FILE

* ------------------------------------------------------------------- *
         WHEN  REQUEST,(EQ,CLC),=CL8'UPDATE'
          LA   R1,SUBRPLST              POINT TO PARAMETER LIST
          L    R15,=V(RXVSAMBR)         GET SUBR. ENTRY POINT
          BASR R14,R15                  BUILD REC BUFFER
          IF   R15,(NZ,LTR),R15         IF FAILED
           LR  R5,R15                    SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

          BAS  R5,GET_MAXV              GET KEY AND REC MAX LENGTHS

          L    R8,FILERPL               GET RPL ADDRESS
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY

          LA   R1,RECLEN                POINT TO STEM ELEMENT NAME
          LHI  R2,L'RECLEN              SET LENGTH OF ELEMENT NAME
          BAS  R5,GET_STEM              GET RECORD LENGTH
          IF   SHVVALL,(NH,CLC),=F'0'   IF NO DEF REC LEN SUPPLIED
           L   R2,RMAX                   USE MAX REC LEN
          ELSE                          ELSE
           LA  R1,VARDATA                POINT TO ZONED RECORD LENGTH
           L   R2,SHVVALL                GET LENGTH OF VAR DATA
           BAS R5,ZONE2REG               CONVERT TO BINARY IN REG2
          ENDIF                         ENDIF
          IF   R2,(LT,C),KMAX,OR,       IF VALUE < MAX KEY LEN         +
               R2,(HT,C),RMAX           OR VALUE > MAX REC LEN
           MVC MACRO,=CL8'UPDATE_X'      LAST FAILED MACRO
           LHI R5,-9                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R2,RPLRLEN               SET RECORD LENGTH TO UPDATE
          L    R2,KMAX                  GET MAX KEY LEN
          STH  R2,RPLKEYL               SET KEY LENGTH TO UPDATE

          OI   RPLOPT1,RPLSEQ           SET SEQUENTIAL PROCESSING
          NI   RPLOPT1,X'FF'-RPLDIR     NOT DIRECT PROCESSING
          OI   RPLOPT1,RPLUPD           SET UPD PROCESSING
          NI   RPLOPT2,X'FF'-RPLNSP     NOT NSP PROCESSING
          NI   RPLOPT2,X'FF'-RPLNUP     NOT NUP PROCESSING
          DROP R8                       REMOVE ADDRESSABILITY

          BAS  R6,PUTREC                UPDATE THE FILE

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'D'         "DELETE"
          L    R8,FILERPL               GET RPL ADDRESS
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          OI   RPLOPT1,RPLSEQ           SET SEQUENTIAL PROCESSING
          NI   RPLOPT1,X'FF'-RPLDIR     NOT DIRECT PROCESSING
          OI   RPLOPT1,RPLUPD           SET UPD PROCESSING
          NI   RPLOPT2,X'FF'-RPLNSP     NOT NSP PROCESSING
          NI   RPLOPT2,X'FF'-RPLNUP     NOT NUP PROCESSING
          DROP R8                       REMOVE ADDRESSABILITY

          ERASE RPL=(R8)                DELETE A RECORD
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'ERASE'         INDICATE OPERATION PERFORMED
           OI  OPTFLAG,RPLERRF           SIGNAL ERROR IN RPL
           BAS R5,VSAMERRC               PASS BACK RTN AND ERR CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

* ------------------------------------------------------------------- *
         WHEN  REQUEST,EQ,C'C'         "CLOSE"
          L    R8,FILEACB               GET ACB ADDRESS
          BAS  R6,CLOSEACB              CLOSE THE ACB

* ------------------------------------------------------------------- *
         WHEN  OTHER
          LHI  R5,1                     SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR

* ------------------------------------------------------------------- *
         ENDSL
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
R15ZERO  XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   DS    0H
         ST    R5,SAVE_R5              SAVE REGISTER 5
         ST    R15,SAVE_RF             SAVE REGISTER 15
*
         L     R8,EVALBLKA             GET ADDRESS OF EVALUATION BLOCK
         USING EVALBLOCK,R8            TEMPORARY ADDRESSABILITY
         LA    R1,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         LGF   R2,SAVE_RF              COPY R15 TO R2
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         ST    R2,EVALBLOCK_EVLEN      LENGTH OF CONVERTED RESULT
         DROP  R8                      REMOVE ADDRESSABILITY
*
         LA    R1,VARDATA              POINT TO VARIABLE DATA AREA
         LGF   R2,SAVE_R5              COPY R5 TO R2 AS 64 BITS
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
         MVC   VARNAME+0(3),=C'RC '    MOVE IN THE VARIABLE NAME
         LHI   R4,2                    SET LENGTH OF VARIABLE NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         IF    DDNAME,(HT,CLC),SPACES  STEM NAME AVAILABLE?
*
          LA   R1,DDNAME                POINT BACK TO VAR DATA
          L    R2,DDNLENG               SET LENGTH OF VAR DATA
          LA   R3,DDN                   POINT BACK TO VAR NAME
          LHI  R4,L'DDN                 SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
*
          LA   R1,REQUEST               POINT BACK TO VAR DATA
          LA   R2,REQUEST+L'REQUEST-1   POINT TO LAST BYTE OF DATA
          BAS  R5,SET_LENG              SET LENGTH IN R2
          LA   R3,LASTFN                POINT BACK TO VAR NAME
          LHI  R4,L'LASTFN              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
*
          LA   R1,MACRO                 POINT BACK TO VAR DATA
          LA   R2,MACRO+L'MACRO-1       POINT TO LAST BYTE OF DATA
          BAS  R5,SET_LENG              SET LENGTH IN R2
          LA   R3,LASTFM                POINT BACK TO VAR NAME
          LHI  R4,L'LASTFM              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
*
         ENDIF
* ------------------------------------------------------------------- *
         XR    R15,R15                 CLEAR REG15 FOR RETURN CODE
RETR15   DS    0H
         LMH   R2,R14,DYNASAVE+72      RESTORE HI HALVES OF REGISTERS
*                                       BECAUSE OF SOME 64-BIT REG USE
         TRAILER RETCODE=R15,SVAREA=DYNASAVE
* =================================================================== *
         EJECT
* =================================================================== *
OPENACB  DS    0H            OPEN ACB POINTED TO BY R2
* ------------------------------------------------------------------- *
         ST    R6,AWORK4+8             SAVE RETURN REGISTER
*
         OPEN  (R8)                    OPEN THE ACB
         IF    R15,(NZ,LTR),R15        IF ERROR
          MVC  MACRO,=CL8'OPEN'         INDICATE OPERATION PERFORMED
          OI   OPTFLAG,ACBERRF          SIGNAL ERROR IN ACB
          BAS  R5,VSAMERRC              PASS BACK RTN AND ERR CODES
          LHI  R5,16                    SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         L     R6,FILEACB              GET FILE ACB ADDRESS
         USING IKQACB,R6               TEMPORARY ADDRESSABILITY
         L     R5,ACBAMBL              POINT TO DATASET AMBL
         LA    R1,152(R5)              POINT TO DATASET NAME
         DROP  R6                      REMOVE ADDRESSABILITY
         LA    R2,44-1(,R1)            POINT TO LAST BYTE OF DSN
         BAS   R5,SET_LENG             SET LENGTH IN R2
         LA    R3,DSN                  POINT BACK TO VAR NAME
         LHI   R4,L'DSN                SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         SELECT                        IDENTIFY FILE TYPE
         WHEN  FILEOPT,(ON,TM),FILEAIX
          MVC  CWORK(4),=C'AIX '
         WHEN  FILEOPT,(ON,TM),FILEPATH
          MVC  CWORK(4),=C'PATH'
         WHEN  FILETYPE,(Z,TM),ESDS    ENTRY-SEQUENCED DATASET?
          MVC  CWORK(4),=C'ESDS'
         WHEN  FILETYPE,(O,TM),VRDS    VARIABLE-RELATIVE DATASET?
          MVC  CWORK(4),=C'VRDS'
         WHEN  FILETYPE,(O,TM),RRDS    RELATIVE-RECORD DATASET?
          MVC  CWORK(4),=C'RRDS'
         WHEN  FILETYPE,(O,TM),KSDS    KEY-SEQUENCED DATASET?
          L    R6,4(,R8)                GET AMBL ADDRESS FROM ACB
          L    R6,4(,R6)                GET DATA AMDSB ADDRESS
          USING AMDSBDS,R6              TEMPORARY ADDRESSABILITY
          IF   AMDATTR3,(O,TM),B'00100000'  EXTRALARGE DATASET?
           MVC CWORK(4),=C'XLDS'         INDICATE XXL DATASET
          ELSE
           MVC CWORK(4),=C'KSDS'         INDICATE NOT XXL DATASET
          ENDIF
          DROP R6                       REMOVE ADDRESSABILITY
         WHEN  FILETYPE,(O,TM),SAMN    MSAM-NOCIFORMAT DATASET?
          MVC  CWORK(4),=C'SAMN'
         WHEN  FILETYPE,(O,TM),SAMI    MSAM-IMPLICIT DEFINE?
          MVC  CWORK(4),=C'SAMI'
         WHEN  FILETYPE,(O,TM),SAME    MSAM-EXPLICIT DEFINE?
          MVC  CWORK(4),=C'SAME'
         WHEN  OTHER
          MVC  CWORK(3),=C'UNK'
          MVC  CWORK+3(1),FILETYPE
         ENDSL
         LA    R1,CWORK               POINT TO VAR DATA
         LHI   R2,4                    SET LENGTH OF VAR DATA
         LA    R3,TYP                  POINT BACK TO VAR NAME
         LHI   R4,L'TYP                SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         PUSH  ACONTROL
         ACONTROL FLAG(NOPAGE0)
         SHOWCB ACB=(R8),AM=VSAM,OBJECT=DATA,                          +
               AREA=(S,SHOWAREA),LENGTH=L'SHOWAREA,                    +
               FIELDS=(LRECL,KEYLEN,RKP)
         POP   ACONTROL
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  MACRO,=CL8'SHOWCB/A'     INDICATE OPERATION PERFORMED
          ST   R0,SAVE_R0               SAVE REGISTER 0
          OI   OPTFLAG,CTLBLKF          SIGNAL ERROR IN CTLBLK
          BAS  R5,SHOWRESN              PASS BACK RTN AND RSN CODES
          LHI  R5,16                    SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         IF    FILERPL,(EQ,CLC),=F'0'  IF NO RPL, YET
          IF   FILETYPE,(NZ,TM),ESDS    IF NOT ESDS DATASET
          AND  FILETYPE,(NO,TM),SAMD    AND NOT MSAM DATASET
           GENCB BLK=RPL,AM=VSAM,ACB=(R8),MF=(L,PARMLIST),             +
               AREA=(S,RECBUF),AREALEN=(*,SHOWAREA+0),                 +
               ARG=(S,KEYBUF),KEYLEN=(*,SHOWAREA+4),                   +
               OPTCD=(KEY,SEQ,NUP),RECLEN=(*,SHOWAREA+0)
          ELSE
           GENCB BLK=RPL,AM=VSAM,ACB=(R8),MF=(L,PARMLIST),             +
               AREA=(S,RECBUF),AREALEN=(*,SHOWAREA+0),                 +
               ARG=(S,KEYBUF),KEYLEN=(*,SHOWAREA+4),                   +
               OPTCD=(ADR,SEQ,NUP),RECLEN=(*,SHOWAREA+0)
          ENDIF
          GENCB MF=(E,PARMLIST)
          IF  R15,(NZ,LTR),R15          IF FAILED
           MVC MACRO,=CL8'GENCB/R'       INDICATE OPERATION PERFORMED
           ST R0,SAVE_R0                 SAVE REGISTER 0
           OI OPTFLAG,CTLBLKF            SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B  RETERR                     GO RETURN ERROR
          ENDIF                         ENDIF
          ST  R1,FILERPL                SAVE RPL ADDRESS
         ENDIF                         ENDIF
*
         LA    R1,CWORK               POINT TO "RESULT" AREA
         LGF   R2,SHOWAREA+0           GET LRECL VALUE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,RECMAX               POINT BACK TO VAR NAME
         LHI   R4,L'RECMAX             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         LA    R1,CWORK               POINT TO "RESULT" AREA
         LGF   R2,SHOWAREA+4           GET KEYLEN VALUE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,KEYMAX               POINT BACK TO VAR NAME
         LHI   R4,L'KEYMAX             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         LA    R1,CWORK               POINT TO "RESULT" AREA
         LGF   R2,SHOWAREA+8           GET RKP VALUE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,KEYPOS               POINT BACK TO VAR NAME
         LHI   R4,L'KEYPOS             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R6,AWORK4+8             RESTORE RETURN REGISTER
         BR    R6                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
GETREC   DS    0H            READ A RECORD BASED UPON RPL=(R8)
* ------------------------------------------------------------------- *
         ST    R6,AWORK4+8             SAVE RETURN REGISTER
*
         GET   RPL=(R8)                READ A RECORD
         IF    R15,(NZ,LTR),R15        IF FAILED
          ST   R15,SAVE_RF              SAVE REGISTER 15
          MVC  MACRO,=CL8'GET'          INDICATE OPERATION PERFORMED
          OI   OPTFLAG,RPLERRF          SIGNAL ERROR IN RPL
          BAS  R5,VSAMERRC              PASS BACK RTN AND ERR CODES
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          L    R5,SAVE_RF               COPY R15 TO R5
          IF   R5,(EQ,CHI),RPLLOGER     IF LOGICAL ERROR (RC=8)
          AND  RPLFDBKC,EQ,RPLEOFDS     AND FEEDBACK = EOF (4)
           LHI R5,-1                     SET END-OF-FILE CONDITION
          ELSE                          ELSE
           LHI R5,16                     SET GENERAL VSAM ERROR
          ENDIF                         ENDIF
          DROP R8                       REMOVE ADDRESSABILITY
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
         LLC   R2,RPLFDBKC             GET RPL FEEDBACK CODE
         DROP  R8                      REMOVE ADDRESSABILITY
         IF    R2,(NZ,LTR),R2          IF NON-ZERO FEEDBACK CODE
          LA   R1,CWORK                 POINT TO "RESULT" AREA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,VSAMEC                POINT BACK TO VAR NAME
          LHI  R4,L'VSAMEC              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          LLC  R2,RPLFTNCD              GET RPL FUNCTION CODE
          DROP R8                       REMOVE ADDRESSABILITY
          LA   R1,CWORK                 POINT TO "RESULT" AREA
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,VSAMFC                POINT BACK TO VAR NAME
          LHI  R4,L'VSAMFC              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF                         ENDIF
*
         L     R6,AWORK4+8             RESTORE RETURN REGISTER
         BR    R6                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
PUTREC   DS    0H            WRITE A RECORD BASED UPON RPL=(R8)
* ------------------------------------------------------------------- *
         ST    R6,AWORK4+8             SAVE RETURN REGISTER
*
         PUT   RPL=(R8)                WRITE A RECORD
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  MACRO,=CL8'PUT'          INDICATE OPERATION PERFORMED
          OI   OPTFLAG,RPLERRF          SIGNAL ERROR IN RPL
          BAS  R5,VSAMERRC              PASS BACK RTN AND ERR CODES
          LHI  R5,16                    SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         L     R6,AWORK4+8             RESTORE RETURN REGISTER
         BR    R6                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
CLOSEACB DS    0H            CLOSE ACB POINTED TO BY R2
* ------------------------------------------------------------------- *
         ST    R6,AWORK4+8             SAVE RETURN REGISTER
*
         CLOSE (R8)                    CLOSE THE ACB
         IF    R15,(NZ,LTR),R15        IF FAILED
          MVC  MACRO,=CL8'CLOSE'        INDICATE OPERATION PERFORMED
          OI   OPTFLAG,ACBERRF          SIGNAL ERROR IN ACB
          BAS  R5,VSAMERRC              PASS BACK RTN AND ERR CODES
          LHI  R5,16                    SET RETURN CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         L     R6,AWORK4+8             RESTORE RETURN REGISTER
         BR    R6                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
FILEINIT DS    0H            FIND THE FILE ENTRY OR INITIALIZE ONE
* ------------------------------------------------------------------- *
         ST    R8,AWORK4+8             SAVE RETURN REGISTER

         WHILE FILEDD,LT,X'FF',AND,    SEARCH FOR REQ. DATA LABEL      X
               FILEDD,(NE,CLC),DDNAME
          AHI  R7,L'FILETBLE            INCR. BY LENGTH OF AN ENTRY
         WEND
         IF    FILEDD,EQ,X'FF'         IF REQ. DATA LABEL NOT FOUND
          LA   R7,DYNATBL
          WHILE FILEDD,LT,X'FF',AND,    FIND AN EMPTY TABLE ENTRY      X
               FILEDD,(HT,CLC),SPACES
           AHI R7,L'FILETBLE             INCR. BY LENGTH OF AN ENTRY
          WEND
         ENDIF
         IF    FILEDD,EQ,X'FF'         IF FILE TABLE IS FULL
          LHI  R5,-2                    SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF

         IF    FILEDD,(NE,CLC),DDNAME  IF NOT FOUND...
          MVC  FILEDD,DDNAME            INITIALIZE NEW FILE ENTRY
          XC   FILEOPT,FILEOPT

          XC   SHOWAREA(256),SHOWAREA   CLEAR SHOWCAT DISPLAY AREA
          XC   SHOWAREA+256(256),SHOWAREA+256
          LHI  R2,L'SHOWAREA            GET LENGTH OF SHOWCAT AREA
          STH  R2,SHOWAREA              PUT IN FIRST TWO BYTES OF AREA

          LA   R2,FILEDD                POINT TO CLUSTER DLBL NAME
          LA   R3,SHOWAREA              POINT TO SHOWCAT DISPLAY AREA
          SHOWCAT DDNAME=(R2),AREA=(R3)
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'SHOWCAT'       INDICATE OPERATION PERFORMED
           BAS  R5,SHOWRESN              PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          SELECT
          WHEN SHOWAREA+8,EQ,C'C'       FOR CLUSTER RECORD
           OI  FILEOPT,FILECLUS          INDICATE DEFINE CLUSTER
          WHEN SHOWAREA+8,EQ,C'G'       FOR AIX RECORD
           OI  FILEOPT,FILEAIX           INDICATE DEFINE AIX
          WHEN SHOWAREA+8,EQ,C'R'       FOR PATH RECORD
           OI  FILEOPT,FILEPATH          INDICATE DEFINE PATH
          WHEN OTHER                    OTHERWISE
           LHI R5,-3                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDSL
          MVC  FILECAT,SHOWAREA+4       SAVE CATALOG ACB ADDRESS

          SELECT                        LOCATE DATA COMPONENT
          WHEN SHOWAREA+12,EQ,C'D'      FIRST PAIR IS FOR DATA CMP?
           MVC FILEDCI,SHOWAREA+13       POINT TO FIRST CI NUMBER
          WHEN SHOWAREA+16,EQ,C'D'      SECOND PAIR IS FOR DATA CMP?
           MVC FILEDCI,SHOWAREA+17       POINT TO SECOND CI NUMBER
          WHEN SHOWAREA+20,EQ,C'D'      THIRD PAIR IS FOR DATA CMP?
           MVC FILEDCI,SHOWAREA+21       POINT TO THIRD CI NUMBER
          WHEN OTHER                    OTHERWISE
           MVC FILEDCI,SHOWAREA+25       POINT TO FOURTH CI NUMBER
          ENDSL
          SELECT                        LOCATE INDEX COMPONENT
          WHEN SHOWAREA+12,EQ,C'I'      FIRST PAIR IS FOR INDEX CMP?
           MVC FILEICI,SHOWAREA+13       POINT TO FIRST CI NUMBER
          WHEN SHOWAREA+16,EQ,C'I'      SECOND PAIR IS FOR INDEX CMP?
           MVC FILEICI,SHOWAREA+17       POINT TO SECOND CI NUMBER
          WHEN SHOWAREA+20,EQ,C'I'      THIRD PAIR IS FOR INDEX CMP?
           MVC FILEICI,SHOWAREA+21       POINT TO THIRD CI NUMBER
          WHEN OTHER                    OTHERWISE
           MVC FILEICI,SHOWAREA+25       POINT TO FOURTH CI NUMBER
          ENDSL

          LA   R2,FILEDCI               POINT TO CI NUMBER OF CAT REC
          L    R3,FILECAT               GET ADDRESS OF THE CATALOG ACB
          LA   R4,SHOWAREA              POINT TO SHOWCAT DISPLAY AREA
          SHOWCAT CI=(R2),ACB=(R3),AREA=(R4)
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'SHOWCAT2'      INDICATE OPERATION PERFORMED
           BAS  R5,SHOWRESN              PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          IF   SHOWAREA+8,NE,C'D'       IF NOT A DATA COMP. RECORD
           LHI R5,-4                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF
          MVC  FILETYPE,SHOWAREA+9      SAVE FILE TYPE ATTRIBUTE
          IF   FILETYPE,(ON,TM),SAMN    IF MSAM-NOCIFORMAT
           LHI R5,-5                     SET RETURN CODE
           B   R15ZERO                   GO RETURN ERROR
          ENDIF                         ENDIF

          L    R8,FILECAT               GET ADDRESS OF CATALOG ACB
          GENCB BLK=RPL,AM=VSAM,ACB=(R8),MF=(L,PARMLIST),              +
               AREA=(S,RECBUF),AREALEN=505,                            +
               ARG=(S,KEYBUF),KEYLEN=44,                               +
               OPTCD=(KEY,DIR,KGE,NUP),RECLEN=505
          GENCB MF=(E,PARMLIST)
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'GENCB/R'       INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R1,FILECATR              SAVE CATALOG RPL ADDRESS

          IF   FILETYPE,(NZ,TM),ESDS    IF NOT ESDS DATASET
          AND  FILETYPE,(NO,TM),SAMD    AND NOT MSAM DATASET
           IF  REQUEST,EQ,C'O'           OPEN
           AND RQARG2,NE,C'I'            INPUT
           AND RQARG2,NE,C'U'            UPDATE
            IF RQARG2,EQ,C'R'             RESET
             GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),  +
               MACRF=(KEY,SEQ,OUT,RST)
            ELSE
             GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),  +
               MACRF=(KEY,SEQ,OUT,NRS)
            ENDIF
           ELSE
            GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),   +
               MACRF=(KEY,SEQ,IN,NRS)
           ENDIF
          ELSE
           IF  REQUEST,EQ,C'O'           OPEN
           AND RQARG2,NE,C'I'            INPUT
           AND RQARG2,NE,C'U'            UPDATE
            IF RQARG2,EQ,C'R'             RESET
             GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),  +
               MACRF=(ADR,SEQ,OUT,RST)
            ELSE
             GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),  +
               MACRF=(ADR,SEQ,OUT,NRS)
            ENDIF
           ELSE
            GENCB BLK=ACB,AM=VSAM,DDNAME=(*,FILEDD),MF=(L,PARMLIST),   +
               MACRF=(ADR,SEQ,IN,NRS)
           ENDIF
          ENDIF

          PUSH ACONTROL
          ACONTROL FLAG(NOPAGE0)
          GENCB MF=(E,PARMLIST)
          POP  ACONTROL
          IF   R15,(NZ,LTR),R15         IF FAILED
           MVC MACRO,=CL8'GENCB/A'       INDICATE OPERATION PERFORMED
           ST  R0,SAVE_R0                SAVE REGISTER 0
           OI  OPTFLAG,CTLBLKF           SIGNAL ERROR IN CTLBLK
           BAS R5,SHOWRESN               PASS BACK RTN AND RSN CODES
           LHI R5,16                     SET RETURN CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          ST   R1,FILEACB               SAVE ACB ADDRESS
          XC   FILERPL,FILERPL          CLEAR RPL ADDRESS

         ENDIF                         ENDIF
*
         L     R8,AWORK4+8             RESTORE RETURN REGISTER
         BR    R8                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
GET_MAXV DS    0H    RETRIEVE KEY AND RECORD MAXIMUM LENGTH VARIABLES
* ------------------------------------------------------------------- *
         ST    R5,AWORK4+8             SAVE RETURN REGISTER
*
         LA    R1,KEYMAX               POINT TO STEM ELEMENT NAME
         LHI   R2,L'KEYMAX             SET LENGTH OF ELEMENT NAME
         BAS   R5,GET_STEM             GET MAX KEY LENGTH
         IF    SHVVALL,(NH,CLC),=F'0'  MAX KEY LENGTH IS REQUIRED
          MVC  MACRO,=CL8'GET_MAXV'     LAST FAILED MACRO
          LHI  R5,-8                    SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF
         LA    R1,VARDATA              POINT TO ZONED MAX KEY LEN
         L     R2,SHVVALL              GET LENGTH OF VAR DATA
         BAS   R5,ZONE2REG             CONVERT TO BINARY IN REG2
         IF    R2,(LT,C),=F'1',OR,     IF VALUE < ZERO                 +
               R2,(HT,C),=F'255'       OR VALUE > MAX KEY LEN
          MVC  MACRO,=CL8'GET_MAXV'     LAST FAILED MACRO
          LHI  R5,-8                    SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF
         ST    R2,KMAX                 SAVE AS FULLWORD VALUE

         LA    R1,RECMAX               POINT TO STEM ELEMENT NAME
         LHI   R2,L'RECMAX             SET LENGTH OF ELEMENT NAME
         BAS   R5,GET_STEM             GET MAX RECORD LENGTH
         IF    SHVVALL,(NH,CLC),=F'0'  MAX RECORD LENGTH IS REQUIRED
          MVC  MACRO,=CL8'GET_MAXV'     LAST FAILED MACRO
          LHI  R5,-9                    SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF
         LA    R1,VARDATA              POINT TO ZONED MAX REC LEN
         L     R2,SHVVALL              GET LENGTH OF VAR DATA
         BAS   R5,ZONE2REG             CONVERT TO BINARY IN REG2
         IF    R2,(LT,C),KMAX,OR,      IF VALUE < MAX KEY LEN          +
               R2,(HT,C),=F'65535'     OR VALUE > MAX REC LEN
          MVC  MACRO,=CL8'GET_MAXV'     LAST FAILED MACRO
          LHI  R5,-9                    SET RETURN CODE
          B    R15ZERO                  GO RETURN ERROR
         ENDIF                         ENDIF
         ST    R2,RMAX                 SAVE AS FULLWORD VALUE
*
         L     R5,AWORK4+8             RESTORE RETURN REGISTER
         BR    R5                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZONE DS    0H    CONVERT REG2 TO LEFT-SIGN, ZONED-DECIMAL AT REG1
*                                               RETURN LENGTH IN REG2
* ------------------------------------------------------------------- *
         LR    R3,R1                   SAVE R1 IN R3
         XR    R1,R1                   CLEAR R1 FOR EDIT AND MARK
         CVDG  R2,PWORK16              CONVERT REG2 TO PACKED-DECIMAL
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          MVC  EWORK,=X'40212020202020202020202020202020202020206040'
          LA   R1,EWORK+2              POINT TO SIGNIFICANCE BYTE
         ELSE
          MVC  EWORK,=X'40202020202020202020202020202020202021206040'
          LA   R1,EWORK+19             POINT TO SIGNIFICANCE BYTE
         ENDIF
         LA    R2,EWORK+20             POINT TO SIGN BYTE (& IGNORE IT)
         IF    EWORK(22),(M,EDMK),PWORK16+6  IF NEGATIVE NUMBER
          SHI  R1,1                     DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'               FLOAT A LEFT MINUS SIGN
         ENDIF
         IF    OPTFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          IF   0(R1),EQ,C'-'
           LA  R1,EWORK                  POINT TO SIGN BYTE
           LHI R2,20                     SET LENGTH OF DATA
          ELSE
           LA  R1,EWORK+1                POINT TO SIGNIFICANT BYTE
           LHI R2,19                     SET LENGTH OF DATA
          ENDIF
         ELSE
          SR   R2,R1                    CALC NO. OF CHARS TO MOVE
         ENDIF
         SHI   R2,1                    DECR. LEN FOR ZERO REF.
         EX    R2,MOVE_RESULT          MOVE VALUE TO RESULT AREA
         AHI   R2,1                    INCR. LEN FOR NORMAL REF.
         LR    R1,R3                   RESTORE R1 FROM R3
*
         BR    R5
*
MOVE_RESULT MVC 0(0,R3),0(R1)          SEE EXECUTE INSTRUCTION ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
ZONE2REG DS    0H    CONVERT ZONED-DECIMAL AT REG1 (L'REG2) INTO REG2
* ------------------------------------------------------------------- *
         SHI   R2,1                    DECREMENT BEFORE EXECUTE
         EX    R2,PACK_RESULT          PACK 0(?,R1) FOR LENGTH OF R2
         CVB   R2,PWORK8               CONVERT PACKED-DECIMAL INTO REG2
* ------------------------------------------------------------------- *
         BR    R5
PACK_RESULT PACK PWORK8,0(0,R1)        SEE EXECUTE INSTRUCTION ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
SET_LENG DS    0H    STRIP END OF R1 DATA AREA && RETURN R2 LENGTH
* ------------------------------------------------------------------- *
         IF    0(R1),NH,X'40'          IF FIRST CHAR IS BLANK
          LHI  R2,0                     SET ZERO LENGTH
         ELSE                          ELSE
          WHILE 0(R2),NH,X'40'          STRIP OFF NON-DISPLAY
           SHI R2,1                     ONE AT A TIME
          WEND
          SR   R2,R1                    COMPUTE LENGTH OF DATA
          AHI  R2,1                     ADJUST FOR ZERO OFFSET
         ENDIF                         ENDIF
*
         BR    R5                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
GET_ELEM DS    0H            RETRIEVE A STEM ELEMENT
* ------------------------------------------------------------------- *
         ST    R5,AWORK4               SAVE RETURN ADDRESS
*
         MVC   VARNAME(L'DDNAME),DDNAME   MOVE IN STEM ROOT
         LA    R5,VARNAME              POINT TO START OF VAR NAME
         A     R5,DDNLENG              INCR. VAR NAME ADDRESS
         MVI   0(R5),C'.'              ADD A PERIOD
         AHI   R5,1                    INCR. VAR NAME ADDRESS
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,ELEMNAME_MVC         MOVE IN ELEMENT NAME
         AHI   R2,1                    INCREMENT BACK
         AR    R5,R2                   ADD TO VAR NAME ADDRESS
         MVI   0(R5),C'.'              ADD A PERIOD
         AHI   R5,1                    INCR. VAR NAME ADDRESS
*
         LR    R1,R3                   TRANSFER ELEM VALUE ADDRESS
         LR    R2,R4                   TRANSFER ELEM VALUE LENGTH
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,ELEMNAME_MVC         MOVE IN ELEMENT VALUE
         AHI   R2,1                    INCREMENT BACK
         AR    R5,R2                   ADD TO VAR NAME ADDRESS
*
         LR    R4,R5                   COPY ADDRESS OF END OF NAME
         LA    R3,VARNAME              POINT TO START OF VAR NAME
         SR    R4,R3                   COMPUTE LENGTH OF STEM VAR NAME
         BAS   R5,GETVAR               RETRIEVE THE VARIABLE VALUE
*
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
*
ELEMNAME_MVC MVC 0(0,R5),0(R1)         (SEE "EX" INSTR. ABOVE && BELOW)
* =================================================================== *
         EJECT
* =================================================================== *
GET_STEM DS    0H            RETRIEVE A STEM VARIABLE
* ------------------------------------------------------------------- *
         ST    R5,AWORK4               SAVE RETURN ADDRESS
*
         MVC   VARNAME(L'DDNAME),DDNAME   MOVE IN STEM ROOT
         LA    R5,VARNAME              POINT TO START OF VAR NAME
         A     R5,DDNLENG              INCR. VAR NAME ADDRESS
         MVI   0(R5),C'.'              ADD A PERIOD
         AHI   R5,1                    INCR. VAR NAME ADDRESS
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,ELEMNAME_MVC         MOVE IN ELEMENT NAME
         AHI   R2,1                    INCREMENT BACK
         AR    R5,R2                   ADD TO VAR NAME ADDRESS
*
         LR    R4,R5                   COPY ADDRESS OF END OF NAME
         LA    R3,VARNAME              POINT TO START OF VAR NAME
         SR    R4,R3                   COMPUTE LENGTH OF STEM VAR NAME
         BAS   R5,GETVAR               RETRIEVE THE VARIABLE VALUE
*
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
* =================================================================== *
         EJECT
* =================================================================== *
SET_STEM DS    0H            CREATE A STEM ELEMENT
* ------------------------------------------------------------------- *
         ST    R5,AWORK4               SAVE RETURN ADDRESS
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,STEMDATA_MVC         MOVE DATA TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT BACK
         ST    R2,SHVVALL              PUT IN SHV CONTROL BLOCK
*
         LR    R1,R3                   TRANSFER NAME ADDRESS
         LR    R2,R4                   TRANSFER NAME LENGTH
         LA    R3,VARNAME              POINT TO VAR NAME BUFFER
         XR    R4,R4                   STARTING WITH ZERO LENGTH
         MVC   0(L'DDNAME,R3),DDNAME   MOVE IN STEM NAME
         A     R3,DDNLENG              INCR. VAR NAME ADDR.
         A     R4,DDNLENG              INCR. VAR NAME LENGTH
         MVI   0(R3),C'.'              ADD A PERIOD
         AHI   R3,1                    INCR. VAR NAME ADDR.
         AHI   R4,1                    INCR. VAR NAME LENGTH
*
         SHI   R2,1                    DECREMENT FOR EXECUTE
         EX    R2,STEMNAME_MVC         MOVE NAME TO CONTROL BLOCK
         AHI   R2,1                    INCREMENT BACK
         AR    R4,R2                   INCR. VAR NAME LENGTH
         BAS   R5,SETVAR               CREATE THE VARIABLE
*
         L     R5,AWORK4               RESTORE RETURN ADDRESS
         BR    R5                      RETURN TO CALLER
*
STEMDATA_MVC MVC VARDATA(0),0(R1)      SEE FIRST EXECUTE UP ABOVE
STEMNAME_MVC MVC 0(0,R3),0(R1)         SEE SECOND EXECUTE JUST ABOVE
* =================================================================== *
         EJECT
* =================================================================== *
VSAMERRC DS    0H    PASS BACK VSAM RTN AND ERR CODES
* ------------------------------------------------------------------- *
         ST    R5,AWORK4+4             SAVE RETURN ADDRESS
*
         LA    R1,CWORK                POINT TO "RESULT" AREA
         LGR   R2,R15                  GET VSAM RTN CODE
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,VSAMRC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMRC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         MVC   CWORK(8),SPACES         CLEAR WORK AREA
         LA    R1,CWORK                POINT TO "RESULT" AREA
         IF    OPTFLAG,(ON,TM),RPLERRF IF RPL ERROR OCCURRED...
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          LLC  R2,RPLFDBKC              GET RPL FEEDBACK CODE
          DROP R8                       REMOVE ADDRESSABILITY
         ELSE                          ELSE ACB ERROR OCCURRED...
          USING IKQACB,R8               TEMPORARY ADDRESSABILITY
          LLC  R2,ACBERFLG              GET ACB ERROR FLAG
          DROP R8                       REMOVE ADDRESSABILITY
         ENDIF                         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,VSAMEC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMEC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         IF    OPTFLAG,(ON,TM),RPLERRF IF RPL ERROR OCCURRED...
          LA   R1,CWORK                 POINT TO "RESULT" AREA
          USING IKQRPL,R8               TEMPORARY ADDRESSABILITY
          LLC  R2,RPLFTNCD              GET RPL FUNCTION CODE
          DROP R8                       REMOVE ADDRESSABILITY
          BAS  R5,REG2ZONE              CONVERT THAT TO ZONED-DECIMAL
          LA   R3,VSAMFC                POINT BACK TO VAR NAME
          LHI  R4,L'VSAMFC              SET LENGTH OF VAR NAME
          BAS  R5,SET_STEM              CREATE THE STEM ELEMENT
         ENDIF                         ENDIF
*
         L     R5,AWORK4+4             RESTORE RETURN ADDRESS
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
SHOWRESN DS    0H    PASS BACK SHOWCAT AND XXXXCB RTN AND RSN CODES
* ------------------------------------------------------------------- *
         ST    R5,AWORK4+4             SAVE RETURN ADDRESS
*
         LA    R1,CWORK                POINT TO "RESULT" AREA
         IF    R15,(LT,C),=F'12',OR,                                   X
               OPTFLAG,(ON,TM),CTLBLKF
          LGR  R2,R15
         ELSE
          LGH  R2,SHOWAREA+2           GET SHOWCAT RTN CODE
         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         LA    R3,VSAMRC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMRC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         MVC   CWORK(8),SPACES         CLEAR WORK AREA
         LA    R1,CWORK                POINT TO "RESULT" AREA
         IF    OPTFLAG,(ON,TM),CTLBLKF
          LGF  R2,SAVE_R0              RESTORE REGISTER 0
         ELSE
          LGH  R2,SHOWAREA+4           GET SHOWCAT RTN CODE
         ENDIF
         BAS   R5,REG2ZONE             CONVERT THAT TO ZONED-DECIMAL
         IF    SHOWAREA+6,NE,X'00',AND, MODULE CODE PRESENT?           X
               OPTFLAG,(Z,TM),CTLBLKF    AND NOT CTLBLK ERROR?
          MVC  CWORK+4(2),SHOWAREA+6    ADD THE MODULE CODE             00015800
          LHI  R2,6                      SET LENGTH OF VAR DATA
         ENDIF                         ENDIF
         LA    R3,VSAMEC               POINT BACK TO VAR NAME
         LHI   R4,L'VSAMEC             SET LENGTH OF VAR NAME
         BAS   R5,SET_STEM             CREATE THE STEM ELEMENT
*
         L     R5,AWORK4+4             RESTORE RETURN ADDRESS
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H    GET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         ST    R4,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
         ST    R5,AWORK4+4             SAVE RETURN ADDRESS
*
*        MVC   VARDATA+0(18),=C'RXVSAMIO Get var: '
*        MVC   VARDATA+18(40),VARNAME
*        LA    R2,18(,R4)              ADD HEADER LEN TO VAR NAME LEN
*        BAS   R5,SAYIT                GO SAY IT
*
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          MVC  MACRO,=CL8'ARXEXCOM'     LAST FAILED MACRO
          LHI  R5,8                     SET RESULT CODE
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           IC  R15,SHVRET                SAVE ARXEXCOM CC
          ENDIF                         ENDIF
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         L     R5,AWORK4+4             RESTORE RETURN ADDRESS
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
SAYIT    DS    0H    USE REXX "SAY" TO DISPLAY MESSAGE
* ------------------------------------------------------------------- *
         ST    R2,SAYPARM3             PUT BUFFER LENGTH IN SAY PARM 3
*
         LA    R1,SAYPARMB             GET ADDRESS OF SAY PARM BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXSAY              POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXSAY
         IF    R15,(NZ,LTR),R15        IF NON-ZERO RETURN CODE
          MVC  MACRO,=CL8'ARXSAY'       LAST FAILED MACRO
          LHI  R5,8                     SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H            SET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R4,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R6,EPVECTOR             GET REXX VECTOR TABLE ADDRESS
         USING ARXEXTE,R6              TEMPORARY ADRESSABILITY
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         DROP  R6                      REMOVE ADDRESSABILITY
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   VARNAME(3),(NE,CLC),=C'RC '  IF NOT 'RC', REPORT ERRORS
          AND  SHVRET,NE,SHVNEWV        AND NOT NEW VARIABLE
           MVC MACRO,=CL8'ARXEXCOM'      LAST FAILED MACRO
           LHI R5,8                      SET RESULT CODE
           IF  R15,(Z,LTR),R15           IF ZERO RETURN CODE
            IC R15,SHVRET                 SAVE ARXEXCOM CC
           ENDIF                         ENDIF
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    18D                     STANDARD OR F4SA REG. SVAEAREA
SALEN    DC    A(DYNALENG)
BUFLEN   DC    A(L'RECBUF)
SPACES   DC    CL8' '
ENDMARK  DC    XL8'FFFFFFFFFFFFFFFF'
KMAX     DS    F                       MAXIMUM KEY LENGTH
RMAX     DS    F                       MAXIMUM REC LENGTH
*
LEVEL    DC    A(IEANT_TASK_LEVEL)     Task level IEANTASM
NAME     DC    CL16'REXX_VSAM_IO'      Name for NAME/TOKEN Pair
TOKEN    DS    XL16                    Token for NAME/TOKEN Pair
PERSOPT  DC    A(IEANT_NOPERSIST)      Persist option
RETCODE  DS    F                       Return code
*
RTVPLIST DC    A(LEVEL,NAME,TOKEN,RETCODE+X'80000000')
CRTPLIST DC    A(LEVEL,NAME,TOKEN,PERSOPT,RETCODE+X'80000000')
SUBRPLST DC    A(0,0)
*
HIVAL    DC    256X'FF'                CHECK KEY BUFFER
*
         LTORG
         SPACE 3                                                        00053400
* ******************************************************************* * 00053500
*        VARIABLE NAMES TO RETRIEVE/CREATE                            * 00053600
* ******************************************************************* * 00053700
LASTFN   DC    C'_LFN'                 last function requested
LASTFM   DC    C'_LFM'                 last failed macro name
VSAMRC   DC    C'_RC'                  VSAM return code
VSAMEC   DC    C'_EC'                  VSAM error code
VSAMFC   DC    C'_FC'                  VSAM function code
DDN      DC    C'_DDN'                 dataset label
DSN      DC    C'_DSN'                 dataset name
TYP      DC    C'_TYP'                 dataset type
RECRBA   DC    C'_RBA'                 relative byte address
RECRRN   DC    C'_RRN'                 relative record number
RECKEY   DC    C'_KEY'                 record key definitions
RECFLD   DC    C'_FLD'                 record field definitions
KEYMAX   DC    C'_KMX'                 key maximum length
KEYLEN   DC    C'_KLN'                 key generic length
KEYPOS   DC    C'_RKP'                 relative key position
RECMAX   DC    C'_RMX'                 record maximum length
RECLEN   DC    C'_RLN'                 record variable length
RECAVG   DC    C'_RAV'                 record average length
* =================================================================== *
         END   RXVSAMIO
