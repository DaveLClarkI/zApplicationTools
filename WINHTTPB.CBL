 PROCESS DATA(24)
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    WINHTTPB.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  SEP 2024.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       WIN HTTP BATCH CLIENT.

      * CHANGE HISTORY ------------------------------------------------
      * 09/11/2024 DLC Original design.  For initial implimentation,
      *                arbitrarily supports up to 96K in both output
      *                and input buffers with console notice if(when)
      *                actual usage exceeds 64K in either direction.
      *
      *                The template generation and parsing code used
      *                here is also used in the WINHTTPC online program.
      *
      * 12/24/2025 DLC Add support for HTML ACCEPT header.
      *                Capture JSON Array Level for parsing purposes.
      * 01/05/2026 DLC Allow generated data to be returned to caller.
/DLC1/* 05/12/2026 DLC Add support for HTTP persistent connections.
      * END OF HISTORY ------------------------------------------------

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *           W I N   H T T P   B A T C H   C L I E N T           *
      *                                                               *
      *                                                               *
      * Winsupply Group Services                      Written by:     *
      * 3110 Kettering Boulevard                   Dave Leonard Clark *
      * Dayton, Ohio  45439                          (937)294-5331    *
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
      *      This program represents  a  batch  client for performing *
      * simple HTTP call-outs to an XML or JSON web service as speci- *
      * fied by the caller  to  this program.   (Examples of "simple" *
      * are that no cookies or multi-part attachments are supported.) *
      *                                                               *
      *      Note that  the  caller,  in do-it-yourself fashion,  may *
      * either build their  own  request  buffer  or  parse their own *
      * response  buffer (or both) ... or ... supply a template id to *
      * automatically drive the  built-in  process to either generate *
      * the request body or  parse  the response body (or both).   If *
      * defined,  the supplied  template  should contain ascendingly- *
      * numbered variables  and  these  are  used as references,  for *
      * value replacement,  in the  buffer  strings  then supplied by *
      * (and returned to) the caller of this program.                 *
      *                                                               *
      * (See  the  CICS/OLPD  "HTPL"  transaction  for  the  provided *
      * template editor associated with this process.)                *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                        IMPLEMENTATION                         *
      *                                                               *
      *      The invoking program only needs  to pass a 90-byte para- *
      * meter block,  with the following parameter elements, in order *
      * to call a web service:                                        *
      *                                                               *
      * --1----+----2----+----3----+----4----+----5----+----6----+--- *
      * 01  WINHTTPC-PARMS.                                           *
      *   03  HTTP-TEMPLATE-ID          PIC  X(12).                   *
      *     88  HTTP-NO-TEMPLATE                     VALUE '**NONE**'.*
      *   03  HTTP-REQUEST              PIC  X(10).                   *
      *     88  HTTP-REQ-GET                       VALUE 'GET       '.*
      *     88  HTTP-REQ-GET-TEXT                  VALUE 'GET-TEXT  '.*
      *     88  HTTP-REQ-GET-BINARY                VALUE 'GET-BIN   '.*
      *     88  HTTP-REQ-POST                      VALUE 'POST      '.*
      *     88  HTTP-REQ-POST-TEXT                 VALUE 'POST-TEXT '.*
      *     88  HTTP-REQ-POST-BINARY               VALUE 'POST-BIN  '.*
      *     88  HTTP-REQ-PUT                       VALUE 'PUT       '.*
      *     88  HTTP-REQ-PUT-TEXT                  VALUE 'PUT-TEXT  '.*
      *     88  HTTP-REQ-PUT-BINARY                VALUE 'PUT-BIN   '.*
      *     88  HTTP-REQ-PATCH                     VALUE 'PATCH     '.*
      *     88  HTTP-REQ-PATCH-TEXT                VALUE 'PATCH-TEXT'.*
      *     88  HTTP-REQ-PATCH-BINARY              VALUE 'PATCH-BIN '.*
      *     88  HTTP-REQ-DELETE                    VALUE 'DELETE    '.*
      *     88  HTTP-REQ-CLOSE                     VALUE 'CLOSE     '.*
      *   03  HTTP-RTNCODE              PIC S9(9)    BINARY.          *
      *   03  HTTP-URL-ADR                           POINTER.         *
      *   03  HTTP-URL-LEN              PIC S9(9)    BINARY.          *
      *   03  HTTPS-CERT-MEMBER         PIC  X(8).                    *
      *   03  HTTP-HEADERS-ADR                       POINTER.         *
      *   03  HTTP-HEADERS-LEN          PIC S9(9)    BINARY.          *
      *   03  HTTP-SEND-DATA-ADR                     POINTER.         *
      *   03  HTTP-SEND-DATA-LEN        PIC S9(9)    BINARY.          *
      *   03  HTTP-SEND-CONTENT-ADR                  POINTER.         *
      *   03  HTTP-SEND-CONTENT-LEN     PIC S9(9)    BINARY.          *
      *   03  HTTP-RECV-DATA-ADR                     POINTER.         *
      *   03  HTTP-RECV-DATA-LEN        PIC S9(9)    BINARY.          *
      *   03  HTTP-RECV-CONTENT-ADR                  POINTER.         *
      *   03  HTTP-RECV-CONTENT-LEN     PIC S9(9)    BINARY.          *
      *   03  HTTP-RESPONSE-ADR                      POINTER.         *
      *   03  HTTP-RESPONSE-LEN         PIC S9(9)    BINARY.          *
      *                                                               *
      *                                                               *
      *      In RPG-II, that might look like this:                    *
      *                                                               *
      * ----+----1----+----2----+----3----+----4----+----5----+----6- *
      *      IWINHTP      DS                             90           *
      *      I                                        1  12 HTPLID    *
      *      I                                       13  22 HTPREQ    *
      *      I                                    B  23  260HTPRTC    *
      *      I                                       27  30 HTPURA    *
      *      I                                    B  31  340HTPURL    *
      *      I                                       35  42 HTPCTM    *
      *      I                                       43  46 HTPHDA    *
      *      I                                    B  47  500HTPHDL    *
      *      I                                       51  54 HTPOUA    *
      *      I                                    B  55  580HTPOUL    *
      *      I                                       59  62 HTPOCA    *
      *      I                                    B  63  660HTPOCL    *
      *      I                                       67  70 HTPINA    *
      *      I                                    B  71  740HTPINL    *
      *      I                                       75  78 HTPICA    *
      *      I                                    B  79  820HTPICL    *
      *      I                                       83  86 HTPRSA    *
      *      I                                    B  87  900HTPRSL    *
      *                                                               *
      *                                                               *
      *      However,  note that  for  both  COBOL  and RPG-II,  some *
      * parameter elements are  passing  addresses  instead of actual *
      * values.   This is required  so  that  (1)  you don't run into *
      * limitations on the length of  the parameter block that can be *
      * passed  back  and  forth   between  the  called  and  calling *
      * programs,  and (2) a single  subroutine  can be used for many *
      * different web service calls.  (See "INPUT and OUTPUT" for how *
      * to obtain the addresses needed.)                              *
      *                                                               *
      *                                                               *
      *                      PARAMETER ELEMENTS                       *
      *                                                               *
      * HTTP-TEMPLATE-ID                                              *
      *      This is mostly  self-explanatory,  but  it  is  an up to *
      *      12-character primary  key  name  that  you've given your *
      *      template(s) for a particular  web service you would like *
      *      to consume.   If  the  web  service  has specific buffer *
      *      requirements that don't  fit the JSON/XML mold,  specify *
      *      "**NONE**" and you can build  your own output buffer and *
      *      receive back the raw buffer returned by the web service. *
      *      This is known as "do-it-yourself" buffering.             *
      *                                                               *
      * HTTP-REQUEST                                                  *
      *      This is the type  of  HTTP  request you're making to the *
      *      web service.   Except for "CLOSE", these are pre-defined *
      *      by the web world with the following meanings:            *
      *        GET    - Retrieves data from the resource specified in *
      *                 the URL without altering it;                  *
      *        POST   - Creates a new resource, such as adding a file *
      *                 to a directory or a row to a database table,  *
      *                 using the data included in the request body;  *
      *        PUT    - Updates an existing resource in its entirety  *
      *                 using the data included in the request body;  *
      *        PATCH  - Updates part of an existing resource using    *
      *                 the data included in the request body;        *
      *        DELETE - Completely removes a resource, as specified   *
      *                 in the URL; and,                              *
      *        CLOSE  - This is an internal type, as a final request  *
      *                 by the caller, to indicate there will be no   *
      *                 further HTTP requests coming, and the temp-   *
      *                 late VSAM files may be closed (batch, only).  *
      *                                                               *
      *      NOTE: The first four request  types also have a TEXT     *
      *            or BINARY mode.  The base mode leaves it up to     *
      *            the VSE HTTP  (IESHTTPB)  client as to whether     *
      *            EBCDIC-to-ASCII  and  ASCII-to-EBCDIC transla-     *
      *            tion occurs.  Using an explicit mode overrides     *
      *            (or, forces) IESHTTPB translation.                 *
      *                                                               *
      * HTTP-RTNCODE                                                  *
      *      This is the client return code, which is not the same as *
      *      the HTTP response  code  (see  below for that).   Either *
      *      this WIN template client or the VSE web client will pass *
      *      back its return code in this element.  The caller should *
      *      check both this  return  code  and,  if zero,  then also *
      *      check the  HTTP  response  code  before  concluding that *
      *      everything was successful.                               *
      *                                                               *
      * HTTP-URL-ADR                                                  *
      * HTTP-URL-LEN                                                  *
      *      This is the address of,  and the length of, the HTTP URL *
      *      for the target web service.  See below for how to obtain *
      *      the address and obtaining the length is up to you.       *
      *                                                               *
      * HTTPS-CERT-MEMBER                                             *
      *      This may be left  blank,  unless  the target web service *
      *      requires encryption (HTTPS)  --  in which case,  this is *
      *      the VSE member name,  in  the VSE key ring library,  for *
      *      the certificate associated with the target web service.  *
      *                                                               *
      * HTTP-HEADERS-ADR                                              *
      * HTTP-HEADERS-LEN                                              *
      *      This is the address of,  and the length of, the optional *
      *      HTTP headers (multiple headers to be delimited by EBCDIC *
      *      CR/NL x'0D15') to  be  sent  to  the target web service. *
      *      (See below for how  to  obtain the address and obtaining *
      *      the length is up  to  you.)  If  an address is provided, *
      *      then the length must  be  greater than zero.   If a non- *
      *      zero  length  is  provided,  then  a  valid  address  is *
      *      required.   If no HTTP  headers are required,  then this *
      *      address must be null ("low values" or binary zeroes) and *
      *      the length must be zero.                                 *
      *                                                               *
/DLC1/*      NOTE: Batch persistent connections are now supported     *
/DLC1/*            by IESHTTPB.   So,  to close a connection, the     *
/DLC1/*            caller should pass "connection: close"  as the     *
/DLC1/*            first optional header.  This program will then     *
/DLC1/*            set the IESHTTPB flag  to close the connection     *
/DLC1/*            at response time.                                  *
      *                                                               *
      * HTTP-SEND-DATA-ADR                                            *
      * HTTP-SEND-DATA-LEN                                            *
      *      This is the address of,  and the length of,  the data to *
      *      be sent to the target  web service.   (See below for how *
      *      to obtain the address and  obtaining the length is up to *
      *      you.) If an address is provided, then the length must be *
      *      greater than zero.   If  a  non-zero length is provided, *
      *      then a valid address is required.  If no request body is *
      *      required,  then this address must  be null ("low values" *
      *      or binary zeroes) and the length must be zero.           *
      *                                                               *
      * HTTP-SEND-CONTENT-ADR                                         *
      * HTTP-SEND-CONTENT-LEN                                         *
      *      This is the  address  of,  and  the length of,  the HTTP *
      *      Content Type for the request body,  if any.   (See below *
      *      for how to obtain  the  address and obtaining the length *
      *      is up to you.)  Note: Only required for "do-it-yourself" *
      *      buffering and,  then,  only if there  is a request body. *
      *      Minimum length is  8.   Otherwise,  this does not apply, *
      *      but serves as an override if that suits your purposes.   *
      *                                                               *
      *      NOTE: In particular, if 'return-to-caller' is speci-     *
      *            fied as an  override,  then the generated data     *
      *            is not sent at all but is immediately returned     *
      *            to the caller as 'received' data--for example,     *
      *            to use in a multipart scenario.                    *
      *                                                               *
      * HTTP-RECV-DATA-ADR                                            *
      * HTTP-RECV-DATA-LEN                                            *
      *      This is the address  of,  and the length of,  the target *
      *      area in the  caller's  storage  where  you want the data *
      *      received from the  target  web  service  to be returned. *
      *      (See below for how  to  obtain the address and obtaining *
      *      the length is up to you.)  This is always required and a *
      *      minimum length of  64  is  expected.   Any received data *
      *      that does not fit in the length provided is truncated.   *
      *                                                               *
      * HTTP-RECV-CONTENT-ADR                                         *
      * HTTP-RECV-CONTENT-LEN                                         *
      *      This is the address  of,  and the length of,  the target *
      *      area in the  caller's  storage  where  you want the HTTP *
      *      Content Type for the response body to be returned.  (See *
      *      below for how to  obtain  the  address and obtaining the *
      *      length  is  up  to  you.)  This  is  only  required  for *
      *      "do-it-yourself" buffering.  The minimum length is 8 and *
      *      what does not fit  in  the length provided is truncated. *
      *      Otherwise, it is optional.                               *
      *                                                               *
      *      NOTE: This may optionally point  to a non-blank area     *
      *            and,  if so,  the address  and length are also     *
      *            used for the HTTP  "Accept" header -- else,  a     *
      *            default (generic) "Accept" header is sent.         *
      *                                                               *
      * HTTP-RESPONSE-ADR                                             *
      * HTTP-RESPONSE-LEN                                             *
      *      This is the address  of,  and the length of,  the target *
      *      area in the  caller's  storage  where  you want the HTTP *
      *      response to be returned.  This response begins with a 3- *
      *      digit code, followed by a space and then the textual in- *
      *      terpretation of that code.  Hence, the minimum length is *
      *      3 and what does not fit  in the length provided is trun- *
      *      cated.  Common responses are as follows:                 *
      *        200 OK/Success                                         *
      *        301 Permanent Redirect                                 *
      *        302 Temporary Redirect                                 *
      *        304 Not Modified                                       *
      *        400 Bad Request                                        *
      *        401 Unauthorized Error                                 *
      *        403 Forbidden                                          *
      *        404 Not Found                                          *
      *        ...etc...                                              *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                       INPUT and OUTPUT                        *
      *                                                               *
      *      Besides  the  (optional)  template  id,  the  main input *
      * parameter to this  program  is  primarily  intended  to be at *
      * least the address of a URL, with or without an attached query *
      * string and secondarily,  if the web service calls for it, the *
      * address of  a  buffer  of  variables  and  their values (plus *
      * delimiters,  as needed)  represented  as  a  single character *
      * string.   This HTTP client  will  turn  that character string *
      * into  a  generated  JSON  or   XML  request  body  using  the *
      * associated template.                                          *
      *                                                               *
      *      Such a variable  string,  e.g.,  might  be  a request to *
      * setup an employee at  a  company with an employee id,  first, *
      * middle,  and last name,  and up  to  three different types of *
      * addresses (home,  work, and mail addresses) made up of street *
      * address, city, state, and numeric zip+4.  For example:        *
      *                                                               *
      * @1@"12345"@2@"67890"@3@"DAVID"@4@"L"@5@"CLARK"...             *
      * @6@"HM"@7@"1234 MYHOME ST"@8@"MY CITY"@9@"OH"@10@123456789... *
      * @6@"WK"@7@"1234 MYWORK ST"@8@"MY CITY"@9@"OH"@10@123456789... *
      * @6@"ML"@7@"P.O. BOX 12345"@8@"MY CITY"@9@"OH"@10@123456789    *
      *                                                               *
      *      The associated JSON template might look like this:       *
      * {                                                             *
      *   "setup"        :                                            *
      *   { "company"    : @1@,                                       *
      *     "employee"   : @2@,                                       *
      *     "name"       :                                            *
      *     { "first"    : @3@,                                       *
      *       "middle"   : @4@,                                       *
      *       "last"     : @5@                                        *
      *     },                                                        *
      *     "address"    :                                            *
      *     [                                                         *
      *       { "type"   : @6@,                                       *
      *         "street" : @7@,                                       *
      *         "city"   : @8@,                                       *
      *         "state"  : @9@,                                       *
      *         "zip"    : @10@                                       *
      *       }                                                       *
      *     ]                                                         *
      *   }                                                           *
      * }                                                             *
      *      Notice that  the  template  definition  for  the address *
      * doesn't repeat, but because the data does, then this template *
      * engine will repeat that section each time the variable values *
      * are repeated.   This is why the variable numbering must be in *
      * ascending order (but not necessarily  in sequence) -- so if a *
      * lower number is detected, it signals a repeating section.     *
      *                                                               *
      *      The response buffer returned to the caller will have the *
      * same variable list format as  the request buffer format shown *
      * above.   But in the likely case the web service response is a *
      * different format than the request (as is normal),  a separate *
      * response template is available to be defined and is used when *
      * parsing the web service response.   Thus, variable numbers in *
      * the response buffer to  the  caller  will correspond to those *
      * used in the response template -- not any request template.    *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                          INVOCATION                           *
      *                                                               *
      * COBOL:                                                        *
      *      First,  set up the  following  data  areas in your COBOL *
      * Working-Storage Section.                                      *
      *                                                               *
      * +----1----+----2----+----3----+----4----+----5----+----6----+ *
      *    77  WINHTTPB                    PIC X(8)  VALUE 'WINHTTPB'.*
      *                                                               *
      *    COPY WINHTTPC.                                             *
      *                                                               *
      * Then, you can dynamically invoke this subroutine as follows:  *
      *                                                               *
      *        CALL WINHTTPB USING WINHTTPC-PARMS                     *
      *                                                               *
      *      However,  as was mentioned,  some  of the copybook para- *
      * meters require the caller  to  pass  an  address of a storage *
      * area and,  if that storage  area  is in the caller's working- *
      * storage (which is  likely),  then  another subroutine call is *
      * required in order  to  obtain  that address (because "ADDRESS *
      * OF" currently only works for linkage section items).          *
      *                                                               *
      *      So,  the following subroutine call accepts any number of *
      * pairs where the first half of the pair is the working-storage *
      * item itself and the second half  of the pair is the parameter *
      * item to receive  the  4-byte  address  of the working-storage *
      * item listed as the first half.  For example:                  *
      *                                                               *
      *        CALL 'GETADDR' USING WS-ITEM1 PARM1-ADR                *
      *                             WS-ITEM2 PARM2-ADR                *
      *                             WS-ITEM3 PARM3-ADR                *
      *                                                               *
      * RPG-II:                                                       *
      *      In RPG-II,  you  statically  invoke  the HTTP subroutine *
      * this way (and  note  that  because  you  are  calling a COBOL *
      * subroutine,  you must call the ILBDSET0 subroutine first, and *
      * one time,  only,  so that  COBOL  knows  it  is  running in a *
      * subroutine environment and not as a mainline program):        *
      *                                                               *
      * ----+----1----+----2----+----3----+----4----+----5----+----6- *
      * /INCLUDE COPYBOOKS.RWINHTTPB                                  *
      *                                                               *
      *      C                     CALL 'ILBDSET0'                    *
      * ----+----1----+----2----+----3----+----4----+----5----+----6- *
      *      C                     CALL 'WINHTTPB'                    *
      *      C                     PARM           WINHTP              *
      *                                                               *
      * If you prefer PLIST, then you create it just once and reuse:  *
      *                                                               *
      * ----+----1----+----2----+----3----+----4----+----5----+----6- *
      * /INCLUDE COPYBOOKS.RWINHTTPB                                  *
      *                                                               *
      *      C           WINHPL    PLIST                              *
      *      C                     PARM           WINHTP              *
      *                                                               *
      *      C                     CALL 'ILBDSET0'                    *
      * ----+----1----+----2----+----3----+----4----+----5----+----6- *
      *      C                     CALL 'WINHTTPB'WINHPL              *
      *                                                               *
      * The address retrieval subroutine  is invoked this way (again, *
      * any number of pairs are accepted):                            *
      *                                                               *
      *      C                     CALL 'GETADDR'                     *
      *      C                     PARM           ITEM1               *
      *      C                     PARM           PARM1               *
      *      C                     PARM           ITEM2               *
      *      C                     PARM           PARM2               *
      *      C                     PARM           ITEM3               *
      *      C                     PARM           PARM3               *
      *                                                               *
      *                                                               *
      * NOTE: No matter the  calling  language,  this HTTP subroutine *
      *       leaves files open in anticipation of another invocation *
      *       from the same  caller.  (For performance  reasons,  you *
      *       don't want to be closing  and re-opening the same files *
      *       repeatedly  if  it  is  not  necessary--especially when *
      *       those files are being  used  for read-only,  as in this *
      *       case.)  For this reason, the caller should make sure to *
      *       perform a final  "CLOSE"  call  to this subroutine just *
      *       before the caller goes to end-of-job.                   *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                         RETURN CODES                          *
      *                                                               *
      *      Rather than issue return  codes in the usual way,  which *
      * might have a more severe impact than desired, this subroutine *
      * passes back those return  codes  in  the parameter block that *
      * was passed by the  calling  program.   This is in addition to *
      * the HTTP response,  which is also  passed back to the caller. *
      * It is the caller's responsibility  to  check BOTH of these to *
      * determine any remedial action that might be required.         *
      *                                                               *
      *      The return codes from this subroutine are as follows:    *
      *                                                               *
      *      16  =  HTTP-REQUEST is unknown.                          *
      *      15  =  HTTP-TEMPLATE-ID was not supplied                 *
      *               or no id record found in HTTPLID file           *
      *               or no RESP record found in HTTPLID file         *
      *               or no matching template found in HTTPLTE file.  *
      *      14  =  HTTP-URL-ADR is null                              *
      *               or HTTP-URL-LEN is less than 16.                *
      *      13  =  HTTP-HEADERS-ADR is null                          *
      *              and HTTP-HEADERS-LEN is greater than zero        *
      *              or  HTTP-HEADERS-ADR is not null                 *
      *              and HTTP-HEADERS-LEN is less than 8.             *
      *      12  =  HTTP-RECV-DATA-ADR is null                        *
      *               or HTTP-RECV-DATA-LEN is less than 64           *
      *               or actual HTTP-RECV-LEN is greater than 96K     *
      *               or REQU template id is present                  *
      *               and HTTP-SEND-DATA-ADR is null                  *
      *                or HTTP-SEND-DATA-LEN is less than 1           *
      *                or HTTP-SEND with template is greater than 96K *
      *      11  =  No template requested (do-it-yourself buffers)    *
      *               and HTTP-RECV-CONTENT-ADR is null               *
      *                or HTTP-RECV-CONTENT-LEN is less than 8        *
      *               or HTTP-SEND-DATA-ADR is not null               *
      *               and HTTP-SEND-CONTENT-ADR is null               *
      *                or HTTP-SEND-CONTENT-LEN is less than 8.       *
      *      10  =  HTTP-RESPONSE-ADR is null                         *
      *               or HTTP-RESPONSE-LEN is less than 3.            *
      *                                                               *
      *      Internally,  this HTTP client calls  the VSE HTTP client *
      * to perform the actual call  out  to the web service.   If the *
      * VSE HTTP client issues a  return code,  then that return code *
      * is passed back in the absence of a return code from this HTTP *
      * client.  This HTTP client provides the template-driven engine *
      * while the VSE HTTP client provides the web services engine.   *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      /****************************************************************
      *                                                               *
      * ENVIRONMENT DIVISION                                          *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           SYSLST                      IS PRINTER,
           CLASS  DIGITS               IS '0' THRU '9'.

      ******************************************************************
      *    INPUT-OUTPUT SECTION                                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT HTTPLID
               ASSIGN TO HTTPLID
               ACCESS IS DYNAMIC INDEXED
               RECORD KEY IS TPID-KEY
               FILE STATUS IS FILE1-STAT
                              FILE1-FDBK.

           SELECT HTTPLTE
               ASSIGN TO HTTPLTE
               ACCESS IS DYNAMIC INDEXED
               RECORD KEY IS TPLT-KEY
               FILE STATUS IS FILE2-STAT
                              FILE2-FDBK.

      /****************************************************************
      *                                                               *
      * DATA DIVISION                                                 *
      *                                                               *
      *****************************************************************
       DATA DIVISION.

      ******************************************************************
      *    FILE SECTION                                                *
      ******************************************************************
       FILE SECTION.

       FD  HTTPLID.
       COPY HTTPLID.

       FD  HTTPLTE
           RECORD IS VARYING IN SIZE
             FROM 16 TO 141
             DEPENDING ON TPLT-RECL.
       COPY HTTPLTE.

      *****************************************************************
      *    WORKING-STORAGE SECTION                                    *
      *****************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(1)    VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'WINHTTPB'.
         03  IESHTTPB                  PIC  X(8)    VALUE 'IESHTTPB'.
         03  GETADDR                   PIC  X(8)    VALUE 'GETADDR '.

         03  TPID                      PIC S9(4)    BINARY VALUE 1.
         03  TPLT                      PIC S9(4)    BINARY VALUE 2.

         03  IDX                       PIC S9(4)    BINARY.
         03  POS                       PIC S9(4)    BINARY.
         03  LEN                       PIC S9(4)    BINARY.
         03  LEN2                      PIC S9(4)    BINARY.
         03  TOT1                      PIC S9(4)    BINARY.
         03  TOT2                      PIC S9(4)    BINARY.
         03  TOT3                      PIC S9(4)    BINARY.
         03  TPL-MAX                   PIC S9(4)    BINARY VALUE +1023.
         03  B                         PIC  9       VALUE 1.
         03  E                         PIC  9       VALUE 2.
         03  DLM                       PIC  X.
         03  DLM2                      PIC  X.
         03  BEGG                      PIC  X.
         03  ENDG                      PIC  X.
      *        the following is square brackets for code page 37
         03  JSON-A37                  PIC  X(2)    VALUE X'BABB'.
      *        the following is square brackets for code page 1047
         03  JSON-A47                  PIC  X(2)    VALUE X'ADBD'.
      *
         03  JSON-BEG                  PIC  X       VALUE '{'.
         03  JSON-END                  PIC  X       VALUE '}'.
         03  XML-BEG                   PIC  X       VALUE '<'.
         03  XML-END                   PIC  X       VALUE '/'.
         03  VADR                      PIC  X(4).
         03  VPTR            REDEFINES VADR         POINTER.
         03  PROTOCOL                  PIC  X(5)    VALUE SPACES.

         03  NON-DISPLAY               PIC  X(65)   VALUE
                                      X'000102030405060708090A0B0C0D0E0F
      -                                '101112131415161718191A1B1C1D1E1F
      -                                '202122232425262728292A2B2C2D2E2F
      -                            '303132333435363738393A3B3C3D3E3FFF'.
         03  PERIODS-65                PIC  X(65)   VALUE
                                       '................................
      -                            '.................................'.

         03  VAR-DATA                  PIC  X(832)  VALUE SPACES.
         03  DATA-IN                   PIC  X(832)  VALUE SPACES.
         03  TPLT-IN                   PIC  X(832)  VALUE SPACES.
         03  DATA-OUT                  PIC  X(832)  VALUE SPACES.
         03  TPLT-OUT                  PIC  X(832)  VALUE SPACES.
         03  TEMP-DATA                 PIC  X(832).

         03  ARYLVL                    PIC  9(4)    VALUE ZEROES.
         03  SECNO                     PIC  9(4)    VALUE ZEROES.
         03  VARNO                     PIC  9(4)    VALUE ZEROES.
         03  LSTVAR                    PIC  9(4)    VALUE ZEROES.
         03  EDIT-CODE                 PIC  9(4)    VALUE ZEROES.
         03  EDIT-VARX                 PIC  X(4).
         03  EDIT-VAR        REDEFINES EDIT-VARX    PIC   ZZZ9.
         03  EDIT-LEN                  PIC  9(5)    VALUE ZEROES.
         03  TPID-RECL                 PIC  9(5)    VALUE ZEROES.
         03  TPLT-RECL                 PIC  9(5)    VALUE ZEROES.
         03  TPLT-DATL                 PIC  9(5)    VALUE ZEROES.
         03  VLEN                      PIC  9(7)    VALUE ZEROES.
         03  PDX                       PIC S9(9)    BINARY.
         03  PTR                       PIC S9(9)    BINARY.
         03  OUT                       PIC S9(9)    BINARY.
         03  CALLER-BUFLEN             PIC S9(9)    BINARY VALUE ZEROES.
         03  SEND-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.
         03  RECV-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.
         03  DATA-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.

         03  DEBUG-SWITCH              PIC  X       VALUE 'N'.
      * no debugging ...
           88  NO-DEBUG                             VALUE 'N'.
      * all debugging ...
           88  DEBUG-ON                             VALUE 'Y'.
      * just table debugging ...
           88  TABLE-ON                             VALUE 'T'.
         03  PROXY-SWITCH              PIC  X       VALUE 'N'.
           88  NO-PROXY                             VALUE 'N'.
           88  PROXY-ON                             VALUE 'Y'.
         03  FILES-SWITCH              PIC  X       VALUE 'N'.
           88  FILES-CLOSED                         VALUE 'N'.
           88  FILES-OPEN                           VALUE 'Y'.
         03  KEYWORD-FOUND             PIC  X       VALUE 'N'.
           88  NOT-FOUND                            VALUE 'N'.
           88  WAS-FOUND                            VALUE 'Y'.

         03  USER-AGENT                PIC  X(64)   VALUE
             'Winsupply LIBRA Client'.

         03  PROXY-HTTP-URL            PIC  X(64)   VALUE '10.10.0.32'.
         03  PROXY-HTTP-PORT           PIC  9(9)    BINARY VALUE 8080.

         03  JSON-CONTENT              PIC  X(64)   VALUE
             'application/json;charset=utf-8'.
         03  XML-CONTENT               PIC  X(64)   VALUE
             'application/xml;charset=utf-8'.
         03  RECV-CONTENT              PIC  X(64)   VALUE LOW-VALUES.

         03  KEYRING-LIB               PIC  X(16)   VALUE
             'PRD2.KEYRING'.
         03  SSL-TYPE                  PIC  X(8)    VALUE
             'TLSV1.2'.

      ******************************************************************
      *       A S C I I   t o   E B C D I C   T a b l e s              *
      ******************************************************************
 
       01  ASCII-DATA.
           05 CRLF  PIC X(02) VALUE X'0D0A'.
           05       PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.
           05       PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.
           05       PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.
           05       PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.
           05       PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.
           05       PIC X(15) VALUE X'707172737475767778797A7B7C7D7E'.
       01  ASCII    REDEFINES ASCII-DATA  PIC X(97).

       01  EBCDIC-DATA.
           05 CRNL  PIC X(02) VALUE X'0D15'.
           05       PIC X(16) VALUE ' !"#$%&''()*+,-./'.
           05       PIC X(16) VALUE '0123456789:;<=>?'.
           05       PIC X(16) VALUE '@ABCDEFGHIJKLMNO'.
           05       PIC X(16) VALUE 'PQRSTUVWXYZ[\]^_'.
           05       PIC X(16) VALUE '`abcdefghijklmno'.
           05       PIC X(15) VALUE 'pqrstuvwxyz{|}~'.
       01  EBCDIC   REDEFINES EBCDIC-DATA PIC X(97).

      ******************************************************************

       01  REQU-TEMPLATE.
         03  REQU-NAME                 PIC  X(12).
         03  REQU-TYPE                 PIC  X(4).
         03  REQU-ITEM                 PIC S9(4)    BINARY.
         03  REQU-COUNT                PIC S9(4)    BINARY.
         03  REQU-ENTRY                             OCCURS 1023.
           05  REQU-DELIM              PIC  X(1).
           05  REQU-ARYLVL             PIC  9(4).
           05  REQU-SECNO              PIC  9(4).
           05  REQU-VARNO              PIC  9(4).
           05  REQU-LEN                PIC S9(4)    BINARY.
           05  REQU-DATA               PIC  X(125).

       01  RESP-TEMPLATE.
         03  RESP-NAME                 PIC  X(12).
         03  RESP-TYPE                 PIC  X(4).
         03  RESP-ITEM                 PIC S9(4)    BINARY.
         03  RESP-COUNT                PIC S9(4)    BINARY.
         03  RESP-ENTRY                             OCCURS 1023.
           05  RESP-DELIM              PIC  X(1).
           05  RESP-ARYLVL             PIC  9(4).
           05  RESP-SECNO              PIC  9(4).
           05  RESP-VARNO              PIC  9(4).
           05  RESP-LEN                PIC S9(4)    BINARY.
           05  RESP-DATA               PIC  X(125).

       COPY BITMAN.

       COPY GETPARM.

       COPY HEXMAN.

       COPY IESHTTPR.

       COPY RTCMAN.

       COPY TXTMAN.

       COPY VSMSTATW.

       01  SEND-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).
         03                            PIC  X(32768).

       01  RECV-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).
         03                            PIC  X(32768).

       01  DATA-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).
         03                            PIC  X(32768).

      /****************************************************************
      *    LINKAGE SECTION                                            *
      *****************************************************************
       LINKAGE SECTION.

       COPY WINHTTPC.

       01  VDATA.
         03                            PIC  X(256).

       01  CALLER-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).
         03                            PIC  X(32768).

      /****************************************************************
      *                                                               *
      * PROCEDURE DIVISION                                            *
      *                                                               *
      *****************************************************************
       PROCEDURE DIVISION           USING WINHTTPC-PARMS.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-MAINLINE-ROUTINE.

           PERFORM B10-INITIALIZATION THRU B15-EXIT.

           IF  RTC-CODE = ZERO
           AND NOT HTTP-REQ-CLOSE
               PERFORM C00-HTTP-REQUEST THRU C90-EXIT
           END-IF.

           PERFORM B20-TERMINATION THRU B25-EXIT.

           MOVE RTC-CODE               TO HTTP-RTNCODE.
           MOVE ZEROES                 TO RETURN-CODE.
           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           MOVE ZEROES                 TO RTC-CODE.

           IF  NOT HTTP-REQ-GET
           AND NOT HTTP-REQ-GET-TEXT
           AND NOT HTTP-REQ-GET-BINARY
           AND NOT HTTP-REQ-POST
           AND NOT HTTP-REQ-POST-TEXT
           AND NOT HTTP-REQ-POST-BINARY
           AND NOT HTTP-REQ-PUT
           AND NOT HTTP-REQ-PUT-TEXT
           AND NOT HTTP-REQ-PUT-BINARY
           AND NOT HTTP-REQ-PATCH
           AND NOT HTTP-REQ-PATCH-TEXT
           AND NOT HTTP-REQ-PATCH-BINARY
           AND NOT HTTP-REQ-DELETE
           AND NOT HTTP-REQ-CLOSE
               MOVE +16                TO RTC-CODE
           END-IF.

           IF  RTC-CODE = ZERO
           AND FILES-CLOSED
           AND NOT HTTP-NO-TEMPLATE
               COMPUTE STAT-TOTL        = LENGTH OF STAT-DATA
                                        / LENGTH OF STAT-ENTRY

               MOVE 'HTTPLID'          TO VSAM-FILE(TPID)
               MOVE 'HTTPLTE'          TO VSAM-FILE(TPLT)

               MOVE LENGTH OF TPID-KEY TO VSAM-KEYL(TPID)
               MOVE LENGTH OF TPLT-KEY TO VSAM-KEYL(TPLT)

               PERFORM WITH TEST BEFORE
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                   SET FUNC-OPEN(VSUB) TO TRUE
               END-PERFORM

               OPEN INPUT  HTTPLID
                           HTTPLTE

               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL

               IF  RTC-CODE = ZERO
                   SET  FILES-OPEN     TO TRUE
                   PERFORM WITH TEST BEFORE
                     VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                       SET FUNC-READ(VSUB) TO TRUE
                   END-PERFORM
               END-IF
           END-IF.

           IF  RTC-CODE = ZERO
           AND NOT HTTP-REQ-CLOSE
           AND NOT HTTP-NO-TEMPLATE
               IF  HTTP-TEMPLATE-ID <= SPACES
                   MOVE +15            TO RTC-CODE
               ELSE
                   MOVE HTTP-TEMPLATE-ID
                                       TO TPID-KEY
                   SET FUNC-START(TPID) TO TRUE
                   START HTTPLID KEY > TPID-KEY END-START
                   IF  NOT STAT-NORMAL(TPID)
                     MOVE TPID         TO VSUB
                     MOVE TPID-KEY     TO VSAM-KEYD(VSUB)
                     PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                     GO TO B15-EXIT
                   END-IF
                   SET FUNC-READNEXT(TPID) TO TRUE
                   READ HTTPLID NEXT RECORD END-READ
                   IF  NOT STAT-NORMAL(TPID)
                     MOVE TPID         TO VSUB
                     MOVE TPID-KEY     TO VSAM-KEYD(VSUB)
                     PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                     GO TO B15-EXIT
                   END-IF
                   IF  TPID-NAME NOT = HTTP-TEMPLATE-ID
                     MOVE +15          TO RTC-CODE
                     GO TO B15-EXIT
                   END-IF
               END-IF
           END-IF.

           IF  RTC-CODE = ZERO
               IF  HTTP-URL-ADR OF WINHTTPC-PARMS = NULL
               OR  HTTP-URL-LEN OF WINHTTPC-PARMS < 16
                 MOVE +14              TO RTC-CODE
                 GO TO B15-EXIT
               END-IF

               IF (HTTP-HEADERS-ADR = NULL
               AND HTTP-HEADERS-LEN > ZERO)
               OR (HTTP-HEADERS-ADR NOT = NULL
               AND HTTP-HEADERS-LEN < 8)
                 MOVE +13              TO RTC-CODE
                 GO TO B15-EXIT
               END-IF

               EVALUATE TRUE
               WHEN HTTP-NO-TEMPLATE
                 IF (HTTP-RECV-DATA-ADR = NULL
                 OR  HTTP-RECV-DATA-LEN < 64)
                 OR (HTTP-SEND-DATA-ADR = NULL
                 AND HTTP-SEND-DATA-LEN > ZERO)
                 OR (HTTP-SEND-DATA-ADR NOT = NULL
                 AND HTTP-SEND-DATA-LEN < 1)
                   MOVE +12            TO RTC-CODE
                   GO TO B15-EXIT
                 END-IF
                 IF  HTTP-RECV-CONTENT-ADR = NULL
                 OR  HTTP-RECV-CONTENT-LEN < 8
                 OR  HTTP-SEND-DATA-ADR NOT = NULL
                 AND (HTTP-SEND-CONTENT-ADR = NULL
                  OR  HTTP-SEND-CONTENT-LEN < 8)
                   MOVE +11            TO RTC-CODE
                   GO TO B15-EXIT
                 END-IF
               WHEN OTHER
                 IF (HTTP-RECV-DATA-ADR = NULL
                 OR  HTTP-RECV-DATA-LEN < 64)
                 OR  TPID-REQU
                 AND (HTTP-SEND-DATA-ADR = NULL
                  OR  HTTP-SEND-DATA-LEN < 1)
                   MOVE +12            TO RTC-CODE
                   GO TO B15-EXIT
                 END-IF
               END-EVALUATE

               IF  HTTP-RESPONSE-ADR OF WINHTTPC-PARMS = NULL
               OR  HTTP-RESPONSE-LEN OF WINHTTPC-PARMS < 3
                 MOVE +10              TO RTC-CODE
                 GO TO B15-EXIT
               END-IF
           END-IF.

           MOVE THIS-PGM(1:7)          TO GET-PNAME.
           CALL GETPARM             USING GETPARM-PARMS.
           EVALUATE TRUE
           WHEN GET-VALUE(1:5) = 'DEBUG'
               SET  DEBUG-ON           TO TRUE
           WHEN GET-VALUE(1:5) = 'TABLE'
               SET  TABLE-ON           TO TRUE
           END-EVALUATE.
       B15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM TERMINATION ROUTINE                                 *
      ******************************************************************
       B20-TERMINATION.

           IF  RTC-CODE = ZERO
           AND HTTP-REQ-CLOSE
           AND FILES-OPEN
               PERFORM WITH TEST BEFORE
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                   SET FUNC-CLOSE(VSUB) TO TRUE
               END-PERFORM

               CLOSE HTTPLID
                     HTTPLTE

               SET  FILES-CLOSED       TO TRUE

               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
           END-IF.

       B25-EXIT.
           EXIT.

      /*****************************************************************
      *    CHECK A VSAM OR WORK FILE'S STATUS                          *
      ******************************************************************
       COPY VSMSTATP.

      /*****************************************************************
      *    PROCESS HTTP REQUEST                                        *
      ******************************************************************
       C00-HTTP-REQUEST.

           MOVE LOW-VALUES             TO HTTP-REQ.
           MOVE LENGTH OF HTTP-REQ     TO HTTP-REQ-LEN.
           SET  HTTP-URL-ADR OF HTTP-REQ
                                       TO HTTP-URL-ADR OF WINHTTPC-PARMS
           MOVE HTTP-URL-LEN OF WINHTTPC-PARMS
                                       TO HTTP-URL-LEN OF HTTP-REQ.
           IF  DEBUG-ON
           OR  TABLE-ON
             SET  ADDRESS OF VDATA     TO HTTP-URL-ADR OF HTTP-REQ
             MOVE HTTP-URL-LEN OF HTTP-REQ TO VLEN EDIT-LEN
             DISPLAY THIS-PGM ': HTTP URL LINK : ' EDIT-LEN
                                     UPON PRINTER
             DISPLAY VDATA(1:VLEN)   UPON PRINTER
             DISPLAY ' '             UPON PRINTER
           END-IF.

           EVALUATE TRUE
           WHEN HTTP-REQ-DELETE       SET HTTP-DELETE        TO TRUE
           WHEN HTTP-REQ-PATCH-BINARY SET HTTP-PATCH-BINARY  TO TRUE
           WHEN HTTP-REQ-PATCH-TEXT   SET HTTP-PATCH-TEXT    TO TRUE
           WHEN HTTP-REQ-PATCH        SET HTTP-PATCH         TO TRUE
           WHEN HTTP-REQ-PUT-BINARY   SET HTTP-PUT-BINARY    TO TRUE
           WHEN HTTP-REQ-PUT-TEXT     SET HTTP-PUT-TEXT      TO TRUE
           WHEN HTTP-REQ-PUT          SET HTTP-PUT           TO TRUE
           WHEN HTTP-REQ-POST-BINARY  SET HTTP-POST-BINARY   TO TRUE
           WHEN HTTP-REQ-POST-TEXT    SET HTTP-POST-TEXT     TO TRUE
           WHEN HTTP-REQ-POST         SET HTTP-POST          TO TRUE
           WHEN HTTP-REQ-GET-BINARY   SET HTTP-GET-BINARY    TO TRUE
           WHEN HTTP-REQ-GET-TEXT     SET HTTP-GET-TEXT      TO TRUE
           WHEN OTHER                 SET HTTP-GET           TO TRUE
           END-EVALUATE.
      * optionally turn client trace on ...
           IF  DEBUG-ON
             MOVE HTTP-TRACE-TO-SYSLST(1:1)
                                       TO HTTP-REQUEST OF HTTP-REQ(1:1)
           END-IF.
/DLC1/* optionally close persistent connection ...
/DLC1/     IF  HTTP-HEADERS-ADR NOT = NULL
/DLC1/       SET ADDRESS OF VDATA      TO HTTP-HEADERS-ADR
/DLC1/       IF  VDATA(1:17) = 'connection: close'
/DLC1/         SET BIT-LOGICAL-OR-BYTE TO TRUE
/DLC1/         MOVE HTTP-CLOSE-CONNECTION(1:1)
/DLC1/                                 TO BIT-LOGICAL
/DLC1/         MOVE HTTP-REQUEST OF HTTP-REQ(1:1)
/DLC1/                                 TO BIT-BYTE
/DLC1/         CALL 'BITMAN'        USING BITMAN-PARMS
/DLC1/         MOVE BIT-BYTE           TO HTTP-REQUEST OF HTTP-REQ(1:1)
/DLC1/       END-IF
/DLC1/     END-IF.
      *
           CALL GETADDR             USING USER-AGENT HTTP-USER-AGENT-ADR
           MOVE USER-AGENT             TO VAR-DATA.
           PERFORM U00-SET-LENGTH.
           MOVE LEN                    TO HTTP-USER-AGENT-LEN.
      *
      * if receive content type specified, use as accept content type
      *
           IF  HTTP-RECV-CONTENT-ADR NOT = NULL
             SET ADDRESS OF VDATA      TO HTTP-RECV-CONTENT-ADR
             IF  VDATA(1:HTTP-RECV-CONTENT-LEN) > SPACES
               SET  HTTP-ACCEPT-ADR    TO HTTP-RECV-CONTENT-ADR
               MOVE VDATA(1:HTTP-RECV-CONTENT-LEN)
                                       TO VAR-DATA
               PERFORM U00-SET-LENGTH
               MOVE LEN                TO HTTP-ACCEPT-LEN
             END-IF
           END-IF.
      *
      * Set up for request buffer -- either DIY or generated ...
      *
           EVALUATE TRUE
           WHEN HTTP-NO-TEMPLATE
               IF  HTTP-SEND-DATA-ADR = NULL
               OR  HTTP-SEND-DATA-LEN = ZERO
                 SET  HTTP-POST-NO-HANDLER
                                       TO TRUE
               ELSE
                 SET  HTTP-POST-BUFFER TO TRUE
                 SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-SEND-DATA-ADR
                 MOVE HTTP-SEND-DATA-LEN
                                       TO SEND-BUFLEN
                 SET  HTTP-POST-DATA-ADR
                                       TO ADDRESS OF CALLER-BUFFER
                 MOVE SEND-BUFLEN      TO HTTP-POST-DATA-LEN
                 SET  HTTP-POST-CONTENT-ADR
                                       TO HTTP-SEND-CONTENT-ADR
                 MOVE HTTP-SEND-CONTENT-LEN
                                       TO HTTP-POST-CONTENT-LEN
               END-IF
      *
      * There are two possible records in the HTTPLID file -- a request
      * template id and a response template id.  The request template id
      * occurs first, in the collating sequence, so if it was not found
      * by the initialization routine (above) then the following code
      * assumes there is no request body for this call out.
      *
           WHEN NOT TPID-REQU
               SET  HTTP-POST-NO-HANDLER
                                       TO TRUE
           WHEN OTHER
               IF  TABLE-ON
      * for debugging, let's see the incoming request string
                 MOVE HTTP-SEND-DATA-LEN
                                       TO EDIT-LEN
                 DISPLAY THIS-PGM ': REQUEST STRING: ' EDIT-LEN
                                     UPON PRINTER
                 IF  EDIT-LEN > ZERO
                   SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-SEND-DATA-ADR
                   DISPLAY CALLER-BUFFER(1:EDIT-LEN)
                                     UPON PRINTER
                 END-IF
                 DISPLAY ' '         UPON PRINTER
               END-IF
      *
               SET  HTTP-POST-BUFFER   TO TRUE
               IF  TPID-XML
                 IF  REQU-NAME NOT = TPID-NAME
                   PERFORM D00-LOAD-XML-REQU-TEMPLATE THRU D90-EXIT
                 END-IF
      * for debugging, this also displays the generated data lines
                 PERFORM G00-GENERATE-XML-REQUEST THRU G90-EXIT
                 CALL GETADDR   USING SEND-BUFFER  HTTP-POST-DATA-ADR
                                      XML-CONTENT  HTTP-POST-CONTENT-ADR
                 MOVE SEND-BUFLEN      TO HTTP-POST-DATA-LEN
                 MOVE XML-CONTENT      TO VAR-DATA
                 PERFORM U00-SET-LENGTH
                 MOVE LEN              TO HTTP-POST-CONTENT-LEN
               ELSE
                 IF  REQU-NAME NOT = TPID-NAME
                   PERFORM E00-LOAD-JSON-REQU-TEMPLATE THRU E90-EXIT
                 END-IF
      * for debugging, this also displays the generated data lines
                 PERFORM J00-GENERATE-JSON-REQUEST THRU J90-EXIT
                 CALL GETADDR   USING SEND-BUFFER  HTTP-POST-DATA-ADR
                                      JSON-CONTENT HTTP-POST-CONTENT-ADR
                 MOVE SEND-BUFLEN      TO HTTP-POST-DATA-LEN
                 MOVE JSON-CONTENT     TO VAR-DATA
                 PERFORM U00-SET-LENGTH
                 MOVE LEN              TO HTTP-POST-CONTENT-LEN
               END-IF
      *
               IF  SEND-BUFLEN >= (96 * 1024)
                   DISPLAY THIS-PGM ': SEND BUFFER OVERFLOW: '
                       SEND-BUFLEN   UPON CONSOLE
                   MOVE +12            TO RTC-CODE
                   GO TO C90-EXIT
               END-IF
           END-EVALUATE.
      *
           IF  TABLE-ON
      * for debugging, let's see the generated request to the server
               MOVE HTTP-POST-CONTENT-LEN
                                       TO EDIT-LEN
               DISPLAY THIS-PGM ': CONTENT-TYPE  : ' EDIT-LEN
                                     UPON PRINTER
               IF  EDIT-LEN > ZERO
                 SET  ADDRESS OF VDATA TO HTTP-POST-CONTENT-ADR
                 DISPLAY VDATA(1:EDIT-LEN)
                                     UPON PRINTER
               END-IF
               DISPLAY ' '           UPON PRINTER
               MOVE HTTP-POST-DATA-LEN TO EDIT-LEN
               DISPLAY THIS-PGM ': GENERATED DATA: ' EDIT-LEN
                                     UPON PRINTER
               IF  EDIT-LEN > ZERO
                 SET  ADDRESS OF VDATA TO HTTP-POST-DATA-ADR
                 DISPLAY VDATA(1:EDIT-LEN)
                                     UPON PRINTER
               END-IF
               DISPLAY ' '           UPON PRINTER
           END-IF.
      *
      * if a return-to-caller override, then return generated data
      *
           IF  HTTP-SEND-CONTENT-ADR NOT = NULL
           AND HTTP-SEND-CONTENT-LEN > ZERO
               SET  ADDRESS OF VDATA   TO HTTP-SEND-CONTENT-ADR
               MOVE FUNCTION UPPER-CASE(VDATA(1:16))
                                       TO VAR-DATA
               IF  VAR-DATA = 'RETURN-TO-CALLER'
                 MOVE HTTP-POST-CONTENT-LEN
                                       TO VLEN
                 SET  ADDRESS OF VDATA TO HTTP-POST-CONTENT-ADR
                 MOVE VDATA(1:VLEN)    TO VAR-DATA
                 SET  ADDRESS OF VDATA TO HTTP-RECV-CONTENT-ADR
                 MOVE VAR-DATA         TO VDATA(1:VLEN)
                 MOVE VLEN             TO HTTP-RECV-CONTENT-LEN
                 SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-RECV-DATA-ADR
                 MOVE SEND-BUFFER(1:SEND-BUFLEN)
                       TO CALLER-BUFFER(1:HTTP-RECV-DATA-LEN)
                 MOVE SEND-BUFLEN      TO HTTP-RECV-DATA-LEN
                 GO TO C90-EXIT
               END-IF
           END-IF.
      *
      * override content type, if requested
      *
           IF  HTTP-SEND-CONTENT-ADR NOT = NULL
           AND HTTP-SEND-CONTENT-LEN > ZERO
               SET  HTTP-POST-CONTENT-ADR
                                       TO HTTP-SEND-CONTENT-ADR
               MOVE HTTP-SEND-CONTENT-LEN
                                       TO HTTP-POST-CONTENT-LEN
           END-IF.
      *
      * This simply sets up to receive a response body from the call.
      *
           SET  HTTP-GET-BUFFER        TO TRUE.
           CALL GETADDR          USING RECV-BUFFER  HTTP-GET-DATA-ADR
                                       RECV-CONTENT HTTP-GET-CONTENT-ADR
           MOVE LOW-VALUES             TO RECV-BUFFER(1:80).
           MOVE LENGTH OF RECV-BUFFER  TO HTTP-GET-DATA-LEN.
           MOVE LOW-VALUES             TO RECV-CONTENT.
           MOVE LENGTH OF RECV-CONTENT TO HTTP-GET-CONTENT-LEN

           SET  HTTP-RESPONSE-ADR OF HTTP-REQ
                                       TO HTTP-RESPONSE-ADR
                                                      OF WINHTTPC-PARMS.
           MOVE HTTP-RESPONSE-LEN OF WINHTTPC-PARMS
                                       TO HTTP-RESPONSE-LEN OF HTTP-REQ.
      *
      * Set up mainframe proxy to get past Bluehill firewall
      *
           SET  ADDRESS OF VDATA       TO HTTP-URL-ADR OF HTTP-REQ.
           MOVE FUNCTION UPPER-CASE(VDATA(1:5))
                                       TO PROTOCOL.
           IF  PROTOCOL NOT = 'HTTPS'
               SET  HTTP-TYPE-PROXY    TO TRUE
      * ... use proxy port 8080 for http port 80
               CALL GETADDR         USING PROXY-HTTP-URL
                                          HTTP-PROXY-URL-ADR
               MOVE PROXY-HTTP-URL     TO VAR-DATA
               PERFORM U00-SET-LENGTH
               MOVE LEN                TO HTTP-PROXY-URL-LEN
               MOVE PROXY-HTTP-PORT    TO HTTP-PROXY-PORT
           END-IF.
      *
      * If requested, pass HTTP headers to the target web service.
      *
           IF  HTTP-HEADERS-ADR NOT = NULL
           AND HTTP-HEADERS-LEN > ZERO
               SET  HTTP-HDR-LINE-ADR  TO HTTP-HEADERS-ADR
               MOVE HTTP-HEADERS-LEN   TO HTTP-HDR-LINE-LEN
           END-IF.
      *
      * If needed, set up for SSL/TLS encryption.
      *
           IF  HTTPS-CERT-MEMBER > SPACES
               CALL GETADDR      USING KEYRING-LIB HTTP-KEY-RING-LIB-ADR
                                     HTTPS-CERT-MEMBER HTTP-KEY-NAME-ADR
                                       SSL-TYPE        HTTP-SSL-TYPE-ADR
               MOVE KEYRING-LIB        TO VAR-DATA
               PERFORM U00-SET-LENGTH
               MOVE LEN                TO HTTP-KEY-RING-LIB-LEN
               MOVE HTTPS-CERT-MEMBER  TO VAR-DATA
               PERFORM U00-SET-LENGTH
               MOVE LEN                TO HTTP-KEY-NAME-LEN
               MOVE SSL-TYPE           TO VAR-DATA
               PERFORM U00-SET-LENGTH
               MOVE LEN                TO HTTP-SSL-TYPE-LEN
           END-IF.

      *    MOVE 120                    TO HTTP-SESS-TIMEOUT.
      *
      * Dump parameters for debugging purposes
      *
           IF  NOT DEBUG-ON
               GO TO C50-CALL-WEB-CLIENT
           END-IF.
      *
           MOVE HTTP-REQ-LEN           TO VLEN.
           DISPLAY 'REQ-LEN                 V=' VLEN
                                     UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-URL-ADR OF HTTP-REQ.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-URL-LEN OF HTTP-REQ
                                       TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           SET  ADDRESS OF VDATA       TO HTTP-URL-ADR OF HTTP-REQ.
           DISPLAY 'HTTP-URL     A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           MOVE HTTP-REQUEST OF HTTP-REQ
                                       TO HEX-CHRT.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'REQUEST      V=' HEX-HEXT
                                     UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-USER-AGENT-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-USER-AGENT-LEN    TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'USER-AGENT   A=' HEX-HEXT ' L=' VLEN ' V='
                USER-AGENT(1:VLEN)   UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-ACCEPT-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-ACCEPT-LEN        TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'ACCEPT       A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-ACCEPT-ADR
             DISPLAY 'ACCEPT       A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER
           END-IF.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-USER-DATA-ADR.
           MOVE VADR                   TO HEX-CHRT.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'USER-DATA    A=' HEX-HEXT
                                     UPON PRINTER.

           DISPLAY ' '               UPON PRINTER.
           MOVE HTTP-POST-HANDLER      TO VLEN.
           DISPLAY 'POST-HANDLER            V=' VLEN
                                     UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-POST-DATA-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-POST-DATA-LEN     TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'POST-DATA    A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-POST-DATA-ADR
             MOVE VDATA(1:80)          TO VAR-DATA(1:80)
             INSPECT VAR-DATA(1:80)
                CONVERTING NON-DISPLAY TO PERIODS-65
             DISPLAY 'POST-DATA    A=' HEX-HEXT ' L=' VLEN ' V='
                VAR-DATA(1:80)       UPON PRINTER
           END-IF.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-POST-CONTENT-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-POST-CONTENT-LEN  TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'POST-CONTENT A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-POST-CONTENT-ADR
             DISPLAY 'POST-CONTENT A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER
           END-IF.

           DISPLAY ' '               UPON PRINTER.
           MOVE HTTP-GET-HANDLER       TO VLEN.
           DISPLAY 'GET-HANDLER             V=' VLEN
                                     UPON PRINTER.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-GET-DATA-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-GET-DATA-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'GET-DATA     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-GET-DATA-ADR
             MOVE VDATA(1:80)          TO VAR-DATA(1:80)
             INSPECT VAR-DATA(1:80)
                CONVERTING NON-DISPLAY TO PERIODS-65
             DISPLAY 'GET-DATA     A=' HEX-HEXT ' L=' VLEN ' V='
                VAR-DATA(1:80)       UPON PRINTER
           END-IF.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-GET-CONTENT-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-GET-CONTENT-LEN   TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'GET-CONTENT  A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-GET-CONTENT-ADR
             DISPLAY 'GET-CONTENT  A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER
           END-IF.

           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-RESPONSE-ADR OF HTTP-REQ.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-RESPONSE-LEN OF HTTP-REQ
                                       TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'RESPONSE     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-RESPONSE-ADR OF HTTP-REQ
             DISPLAY 'RESPONSE     A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER
           END-IF.

           DISPLAY ' '               UPON PRINTER.
           MOVE HTTP-PROXY-TYPE        TO VLEN.
           DISPLAY 'PROXY-TYPE              V=' VLEN
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-PROXY-URL-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-PROXY-URL-LEN     TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'PROXY-URL    A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-PROXY-URL-ADR
             DISPLAY 'PROXY-URL    A=' HEX-HEXT ' L=' VLEN ' V='
                VDATA(1:VLEN)        UPON PRINTER
           END-IF.
           MOVE HTTP-PROXY-PORT        TO VLEN.
           DISPLAY 'PROXY-PORT              V=' VLEN
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-USER-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-USER-LEN          TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'USER         A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-PASSWORD-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-PASSWORD-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'PASSWORD     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.

           DISPLAY ' '               UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-ASCII-CPID-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-ASCII-CPID-LEN    TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'ASCII-CPID   A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-EBCDIC-CPID-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-EBCDIC-CPID-LEN   TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'EBCDIC-CPID  A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-HDR-LINE-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-HDR-LINE-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'HDR-LINE     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             SET  ADDRESS OF VDATA     TO HTTP-HDR-LINE-ADR
             MOVE VDATA(1:80)          TO VAR-DATA(1:80)
             INSPECT VAR-DATA(1:80)
                CONVERTING NON-DISPLAY TO PERIODS-65
             DISPLAY 'HDR-LINE     A=' HEX-HEXT ' L=' VLEN ' V='
                VAR-DATA(1:80)       UPON PRINTER
           END-IF.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-NEW-LOCATION-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-NEW-LOCATION-LEN  TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'NEW-LOCA     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.

           DISPLAY ' '               UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-KEY-RING-LIB-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-KEY-RING-LIB-LEN  TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'KEY-RING     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             DISPLAY 'KEY-RING     A=' HEX-HEXT ' L=' VLEN ' V='
                KEYRING-LIB(1:VLEN)  UPON PRINTER
           END-IF.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-KEY-NAME-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-KEY-NAME-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'KEY-NAME     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             DISPLAY 'KEY-NAME     A=' HEX-HEXT ' L=' VLEN ' V='
                HTTPS-CERT-MEMBER    UPON PRINTER
           END-IF.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-CIPER-SPECS-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-CIPER-SPECS-LEN   TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'CIPER-SPECS  A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           MOVE HTTP-SESS-TIMEOUT      TO VLEN.
           DISPLAY 'SESS-TIMEOUT            V=' VLEN
                                     UPON PRINTER.

           DISPLAY ' '               UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-AUTH-USER-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-AUTH-USER-LEN     TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'AUTH-USER    A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-AUTH-PWD-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-AUTH-PWD-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'AUTH-PWD     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-SSL-TYPE-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-SSL-TYPE-LEN      TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           IF  VLEN <= ZERO
             DISPLAY 'SSL-TYPE     A=' HEX-HEXT ' L=' VLEN ' V='
                                     UPON PRINTER
           ELSE
             DISPLAY 'SSL-TYPE     A=' HEX-HEXT ' L=' VLEN ' V='
                SSL-TYPE(1:VLEN)     UPON PRINTER
           END-IF.
           SET  HEX-DECODE-FROM-CHAR   TO TRUE.
           MOVE +04                    TO HEX-LENG.
           SET  VPTR                   TO HTTP-HDR-HANDLER-ADR.
           MOVE VADR                   TO HEX-CHRT.
           MOVE HTTP-HDR-HANDLER       TO VLEN.
           CALL HEXMAN              USING HEXMAN-PARMS.
           DISPLAY 'HDR-HANDLER  A=' HEX-HEXT ' V=' VLEN
                                     UPON PRINTER.
           DISPLAY ' '               UPON PRINTER.

       C50-CALL-WEB-CLIENT.

      * check if we need to convert sending content to ASCII ourselves
      *    IF  HTTP-SEND-DATA-ADR    NOT = NULL
      *    AND HTTP-SEND-CONTENT-ADR NOT = NULL
      *        SET  ADDRESS OF VDATA   TO HTTP-SEND-CONTENT-ADR
      *        PERFORM WITH TEST BEFORE
      *          VARYING IDX FROM 1 BY 1
      *            UNTIL IDX > HTTP-SEND-CONTENT-LEN
      *               OR VDATA(IDX:11) = 'application'
      *               OR VDATA(IDX:9)  = 'multipart'
      *        END-PERFORM
      *        IF  VDATA(IDX:09) = 'multipart'
      *        OR  VDATA(IDX:15) = 'application/xml'
      *        OR  VDATA(IDX:16) = 'application/json'
      *          SET  ADDRESS OF CALLER-BUFFER
      *                                TO HTTP-SEND-DATA-ADR
      * (note that the VSEn version normally does this for us.)
      *          INSPECT CALLER-BUFFER(1:HTTP-SEND-DATA-LEN)
      *                        CONVERTING EBCDIC TO ASCII
      *        END-IF
      *    END-IF.

      * this is just a heads-up notice of things to come
           IF  SEND-BUFLEN > (64 * 1024)
               DISPLAY THIS-PGM ': SENDING  MORE THAN 64K: '
                   SEND-BUFLEN       UPON CONSOLE
           END-IF.

      * call out to the requested web service
           CALL IESHTTPB            USING HTTP-REQ.
           MOVE RETURN-CODE            TO RTC-CODE
                                          HTTP-RTNCODE.
           IF  RTC-CODE > ZERO
               MOVE RTC-CODE           TO EDIT-CODE
               DISPLAY THIS-PGM ': ERROR IN IESHTTPB, RC='
                       EDIT-CODE     UPON CONSOLE
               GO TO C90-EXIT
           END-IF.

      *    MOVE 'GSKUNINIT'            TO EZA-FUNCTION.
      *    MOVE ZEROES                 TO EZA-ERRNO
      *                                   EZA-RETCODE.
      *    CALL 'EZASOKET'          USING EZA-FUNCTION
      *                                   EZA-ERRNO  EZA-RETCODE.

      * this is a heads-up failure of truncation on receipt
           IF  HTTP-GET-DATA-LEN >= (96 * 1024)
               DISPLAY THIS-PGM ': RECEIVE BUFFER IS FULL: '
                   HTTP-GET-DATA-LEN UPON CONSOLE
               MOVE +12                TO RTC-CODE
           ELSE
      * this is just a heads-up notice of things to come
           IF  HTTP-GET-DATA-LEN > (64 * 1024)
               DISPLAY THIS-PGM ': RECEIVED MORE THAN 64K: '
                   HTTP-GET-DATA-LEN UPON CONSOLE
           END-IF.

       C60-WEB-CLIENT-OUTPUT.

      * this is how much data we got back from the request
           MOVE HTTP-GET-DATA-LEN      TO RECV-BUFLEN.

      * check if we need to convert received content to EBCDIC ourselves
           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > HTTP-GET-CONTENT-LEN
                  OR RECV-CONTENT(IDX:11) = 'application'
           END-PERFORM.
      *    IF  RECV-CONTENT(IDX:15) = 'application/xml'
      *    OR  RECV-CONTENT(IDX:16) = 'application/json'
           IF  RECV-CONTENT(IDX:16) = 'application/json'
           AND RECV-BUFFER(1:1) = '#'
      * (note that the VSEn version normally does this for us.)
               INSPECT RECV-BUFFER(1:RECV-BUFLEN)
                               CONVERTING ASCII TO EBCDIC
           END-IF.

           IF  DEBUG-ON
           OR  TABLE-ON
      * let's see the raw response from the server
             MOVE RECV-BUFLEN          TO EDIT-LEN
             DISPLAY THIS-PGM ': RECEIVED DATA : ' EDIT-LEN
                                     UPON PRINTER
             IF  RECV-BUFLEN > ZERO
               DISPLAY RECV-BUFFER(1:RECV-BUFLEN)
                                     UPON PRINTER
             END-IF
             DISPLAY ' '             UPON PRINTER
           END-IF.

      * optionally pass back the received content type
           IF  HTTP-RECV-CONTENT-ADR NOT = NULL
             MOVE HTTP-GET-CONTENT-LEN TO VLEN
             MOVE RECV-CONTENT(1:VLEN) TO VAR-DATA
             PERFORM U00-SET-LENGTH
             SET  ADDRESS OF VDATA     TO HTTP-RECV-CONTENT-ADR
             MOVE RECV-CONTENT(1:LEN)  TO VDATA(1:HTTP-RECV-CONTENT-LEN)
             MOVE LEN                  TO HTTP-RECV-CONTENT-LEN
           END-IF.

           IF  DEBUG-ON
             MOVE HTTP-GET-CONTENT-LEN TO VLEN
             MOVE RECV-CONTENT(1:VLEN) TO VAR-DATA
             PERFORM U00-SET-LENGTH
             IF  LEN > ZERO
               DISPLAY THIS-PGM ': CONTENT TYPE : ' RECV-CONTENT(1:LEN)
                                     UPON PRINTER
               DISPLAY ' '           UPON PRINTER
             END-IF
           END-IF.

      * along with value, pass back the length of actual http response
           MOVE HTTP-RESPONSE-LEN OF HTTP-REQ TO VLEN.
           SET  ADDRESS OF VDATA       TO HTTP-RESPONSE-ADR
                                                      OF WINHTTPC-PARMS.
           MOVE VDATA(1:VLEN)          TO VAR-DATA.
           PERFORM U00-SET-LENGTH.
           MOVE LEN                    TO HTTP-RESPONSE-LEN
                                                      OF WINHTTPC-PARMS.
           IF  DEBUG-ON AND LEN > ZERO
           OR  VDATA(1:3) NOT = '200'
             DISPLAY THIS-PGM ': HTTP RESPONSE: ' VDATA(1:LEN)
                                     UPON PRINTER
             DISPLAY ' '             UPON PRINTER
           END-IF.

      * if unsuccessful http response, abandon further processing ...
           IF  VDATA(1:3) NOT = '200'
           OR  HTTP-NO-TEMPLATE
           OR  RTC-CODE > ZERO
      * ... but first pass back whatever was received for response body
               SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-RECV-DATA-ADR
               MOVE RECV-BUFFER(1:RECV-BUFLEN)
                       TO CALLER-BUFFER(1:HTTP-RECV-DATA-LEN)
               MOVE RECV-BUFLEN        TO HTTP-RECV-DATA-LEN
               GO TO C90-EXIT
           END-IF.

       C80-PROCESS-OUTPUT.
      *
      * Make sure we are positioned to the response template id
      *
           IF  NOT TPID-RESP
               SET FUNC-READNEXT(TPID) TO TRUE
               READ HTTPLID NEXT RECORD END-READ
               IF  NOT STAT-NORMAL(TPID)
                 MOVE TPID             TO VSUB
                 MOVE TPID-KEY         TO VSAM-KEYD(VSUB)
                 PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 GO TO C90-EXIT
               END-IF
               IF  TPID-NAME NOT = HTTP-TEMPLATE-ID
               OR  NOT TPID-RESP
                 MOVE +15              TO RTC-CODE
                 GO TO C90-EXIT
               END-IF
           END-IF.
      *
      * Parse the web service response into our own internal buffer...
      *
           IF  TPID-JSON
               IF  RESP-NAME NOT = TPID-NAME
                 PERFORM L00-LOAD-JSON-RESP-TEMPLATE THRU L90-EXIT
               END-IF
               PERFORM P00-PARSE-JSON-RESPONSE THRU P90-EXIT
           ELSE
               IF  RESP-NAME NOT = TPID-NAME
                 PERFORM M00-LOAD-XML-RESP-TEMPLATE THRU M90-EXIT
               END-IF
               PERFORM R00-PARSE-XML-RESPONSE THRU R90-EXIT
           END-IF.
      *
      * ...then, pass it back to the caller.
      *
           SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-RECV-DATA-ADR.
           MOVE DATA-BUFFER(1:DATA-BUFLEN)
                       TO CALLER-BUFFER(1:HTTP-RECV-DATA-LEN).
           MOVE DATA-BUFLEN            TO HTTP-RECV-DATA-LEN.
      *
           IF  TABLE-ON
      * for debugging, let's see the returned response string
               MOVE HTTP-RECV-DATA-LEN TO EDIT-LEN
               DISPLAY THIS-PGM ': RESULT STRING : ' EDIT-LEN
                                     UPON PRINTER
               IF  EDIT-LEN > ZERO
                 DISPLAY CALLER-BUFFER(1:HTTP-RECV-DATA-LEN)
                                     UPON PRINTER
               END-IF
               DISPLAY ' '           UPON PRINTER
           END-IF.
       C90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD XML REQUEST TEMPLATE                                   *
      ******************************************************************
       D00-LOAD-XML-REQU-TEMPLATE.
           MOVE LOW-VALUES             TO REQU-TEMPLATE.
           MOVE TPID-NAME              TO REQU-NAME.
           MOVE 'XML'                  TO REQU-TYPE.
           MOVE ZEROES                 TO REQU-COUNT
                                          ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.
           PERFORM T00-START-HTTPLTE THRU T05-EXIT.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL RTC-CODE > ZERO
                OR STAT-EOFILE(TPLT)
                OR REQU-COUNT > TPL-MAX
             PERFORM T10-NEXT-HTTPLTE THRU T15-EXIT
             IF  RTC-CODE = ZERO
             AND NOT STAT-EOFILE(TPLT)
               ADD  1                  TO REQU-COUNT
               MOVE TPLT-DATA(1:TPLT-DATL)
                                       TO TEMP-DATA
               PERFORM U10-LEFT-JUSTIFY
               MOVE LEN                TO REQU-LEN(REQU-COUNT)
               MOVE TEMP-DATA(1:LEN)   TO REQU-DATA(REQU-COUNT)
               MOVE 1                  TO PTR
               MOVE ZERO               TO VARNO
               PERFORM WITH TEST BEFORE
                 UNTIL PTR > LENGTH OF TEMP-DATA
                   PERFORM U50-XML-TEMPLATE
                   IF  PTR <= LENGTH OF TEMP-DATA
                     IF  DLM2 = '<' AND TEMP-DATA(PTR:1) NOT = '/'
                       MOVE DLM2       TO BEGG
                       ADD  1          TO SECNO
                     END-IF
                     IF  DLM2 = '@'
                     AND TPLT-IN(1:LEN2) IS DIGITS
                       MOVE TPLT-IN(1:LEN2)
                                       TO VARNO
                     END-IF
                     IF  DLM2 = '/'
                       MOVE DLM2       TO ENDG
                     END-IF
                   END-IF
               END-PERFORM
               MOVE BEGG               TO REQU-DELIM(REQU-COUNT)
               MOVE ARYLVL             TO REQU-ARYLVL(REQU-COUNT)
               MOVE SECNO              TO REQU-SECNO(REQU-COUNT)
               MOVE VARNO              TO REQU-VARNO(REQU-COUNT)
               IF  ENDG = '/'
                 MOVE ENDG             TO REQU-DELIM(REQU-COUNT)
                 MOVE SPACE            TO ENDG
                 SUBTRACT 1          FROM SECNO
               END-IF
               MOVE SPACE              TO BEGG
             END-IF
           END-PERFORM.
           IF  REQU-COUNT > TPL-MAX
               DISPLAY THIS-PGM ': REQU TEMPLATE OVERFLOW: '
                       REQU-NAME     UPON CONSOLE
               MOVE +15                TO RTC-CODE
           END-IF.
           IF  DEBUG-ON
           OR  TABLE-ON
               DISPLAY 'TYPE  COUNT   ARY# SEC# VAR#  LENG DATA'
                                     UPON PRINTER
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > REQU-COUNT
                 DISPLAY 'REQU: ' IDX ' ' REQU-DELIM(IDX)
                                      ' ' REQU-ARYLVL(IDX)
                                      ' ' REQU-SECNO(IDX)
                                      ' ' REQU-VARNO(IDX)
                                      ' ' REQU-LEN(IDX)
                                      ' ' REQU-DATA(IDX)(1:60)
                                     UPON PRINTER
               END-PERFORM
               DISPLAY ' '           UPON PRINTER
           END-IF.
       D90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD JSON REQUEST TEMPLATE                                  *
      ******************************************************************
       E00-LOAD-JSON-REQU-TEMPLATE.
           MOVE LOW-VALUES             TO REQU-TEMPLATE.
           MOVE TPID-NAME              TO REQU-NAME.
           MOVE 'JSON'                 TO REQU-TYPE.
           MOVE ZEROES                 TO REQU-COUNT
                                          ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.
           PERFORM T00-START-HTTPLTE THRU T05-EXIT.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL RTC-CODE > ZERO
                OR STAT-EOFILE(TPLT)
                OR REQU-COUNT > TPL-MAX
             PERFORM T10-NEXT-HTTPLTE THRU T15-EXIT
             IF  RTC-CODE = ZERO
             AND NOT STAT-EOFILE(TPLT)
               ADD  1                  TO REQU-COUNT
               MOVE TPLT-DATA(1:TPLT-DATL)
                                       TO TEMP-DATA
               PERFORM U10-LEFT-JUSTIFY
               MOVE LEN                TO REQU-LEN(REQU-COUNT)
               MOVE TEMP-DATA(1:LEN)   TO REQU-DATA(REQU-COUNT)
               MOVE 1                  TO PTR
               MOVE ZERO               TO VARNO
               PERFORM WITH TEST BEFORE
                 UNTIL PTR > LENGTH OF TEMP-DATA
                   PERFORM U50-JSON-TEMPLATE
                   IF  PTR <= LENGTH OF TEMP-DATA
                     IF  DLM2 = '{' OR JSON-A37(B:1) OR JSON-A47(B:1)
                       MOVE DLM2       TO BEGG
                       ADD  1          TO SECNO
                       IF  DLM2 = JSON-A37(B:1) OR JSON-A47(B:1)
                         ADD  1        TO ARYLVL
                       END-IF
                     END-IF
                     IF  DLM2 = '@'
                     AND TPLT-IN(1:LEN2) IS DIGITS
                       MOVE TPLT-IN(1:LEN2)
                                       TO VARNO
                     END-IF
                     IF  DLM2 = '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
                       MOVE DLM2       TO ENDG
                     END-IF
                   END-IF
               END-PERFORM
               MOVE BEGG               TO REQU-DELIM(REQU-COUNT)
               MOVE ARYLVL             TO REQU-ARYLVL(REQU-COUNT)
               MOVE SECNO              TO REQU-SECNO(REQU-COUNT)
               MOVE VARNO              TO REQU-VARNO(REQU-COUNT)
               IF  ENDG = '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
                 MOVE ENDG             TO REQU-DELIM(REQU-COUNT)
                 IF  ENDG = JSON-A37(E:1) OR JSON-A47(E:1)
                   SUBTRACT 1        FROM ARYLVL
                 END-IF
                 MOVE SPACE            TO ENDG
                 SUBTRACT 1          FROM SECNO
               END-IF
               MOVE SPACE              TO BEGG
             END-IF
           END-PERFORM.
           IF  REQU-COUNT > TPL-MAX
               DISPLAY THIS-PGM ': REQU TEMPLATE OVERFLOW: '
                       REQU-NAME     UPON CONSOLE
               MOVE +15                TO RTC-CODE
           END-IF.
           IF  DEBUG-ON
           OR  TABLE-ON
               DISPLAY 'TYPE  COUNT   ARY# SEC# VAR#  LENG DATA'
                                     UPON PRINTER
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > REQU-COUNT
                 DISPLAY 'REQU: ' IDX ' ' REQU-DELIM(IDX)
                                      ' ' REQU-ARYLVL(IDX)
                                      ' ' REQU-SECNO(IDX)
                                      ' ' REQU-VARNO(IDX)
                                      ' ' REQU-LEN(IDX)
                                      ' ' REQU-DATA(IDX)(1:60)
                                     UPON PRINTER
               END-PERFORM
               DISPLAY ' '           UPON PRINTER
           END-IF.
       E90-EXIT.
           EXIT.

      /*****************************************************************
      *    GENERATE XML REQUEST OUTPUT                                 *
      ******************************************************************
       G00-GENERATE-XML-REQUEST.
           SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-SEND-DATA-ADR.
           MOVE HTTP-SEND-DATA-LEN     TO CALLER-BUFLEN.
           MOVE ZERO                   TO LSTVAR.
           MOVE 1                      TO SECNO
                                          PTR OUT
                                          REQU-ITEM.

           IF  DEBUG-ON
               DISPLAY 'GENERATED DATA LINES:'
                                     UPON PRINTER
           END-IF.

           PERFORM WITH TEST AFTER
             UNTIL PTR > CALLER-BUFLEN
               PERFORM U20-VAR-INPUT
               IF  PTR <= CALLER-BUFLEN
                 IF  DLM = '@'
                 AND DATA-IN(1:LEN) IS DIGITS
                   MOVE DATA-IN(1:LEN)
                                       TO VARNO
                   IF  VARNO < LSTVAR
                     PERFORM U20-VAR-RESTART
                   END-IF
                   PERFORM U20-VAR-OUTPUT
                     VARYING REQU-ITEM FROM REQU-ITEM BY 1
                       UNTIL REQU-ITEM > REQU-COUNT
                          OR REQU-VARNO(REQU-ITEM) = VARNO
                          OR OUT > LENGTH OF SEND-BUFFER
                   IF  REQU-ITEM <= REQU-COUNT
                     MOVE REQU-SECNO(REQU-ITEM)
                                       TO SECNO
                     PERFORM U20-VAR-INPUT
                     PERFORM U20-VAR-REPLACE
                     STRING TXT-MSTR-BUFFER(1:TXT-MSTR-BUFLEN)
                            CRNL          DELIMITED BY SIZE
                       INTO SEND-BUFFER WITH POINTER OUT
                     IF  DEBUG-ON
                       DISPLAY TXT-MSTR-BUFFER(1:TXT-MSTR-BUFLEN)
                                     UPON PRINTER
                     END-IF
                     MOVE VARNO        TO LSTVAR
                     ADD  1            TO REQU-ITEM
                   END-IF
                 END-IF
               END-IF
           END-PERFORM.

           PERFORM U20-VAR-OUTPUT
             VARYING REQU-ITEM FROM REQU-ITEM BY 1
               UNTIL REQU-ITEM > REQU-COUNT
                  OR OUT > LENGTH OF SEND-BUFFER.

           IF  DEBUG-ON
               DISPLAY ' '           UPON PRINTER
           END-IF.
           SUBTRACT 1                FROM OUT
                                   GIVING SEND-BUFLEN.
       G90-EXIT.
           EXIT.

      /*****************************************************************
      *    GENERATE JSON REQUEST OUTPUT                                *
      ******************************************************************
       J00-GENERATE-JSON-REQUEST.
           SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-SEND-DATA-ADR.
           MOVE HTTP-SEND-DATA-LEN     TO CALLER-BUFLEN.
           MOVE ZERO                   TO LSTVAR.
           MOVE 1                      TO SECNO
                                          PTR OUT
                                          REQU-ITEM.

           IF  DEBUG-ON
               DISPLAY 'GENERATED DATA LINES:'
                                     UPON PRINTER
           END-IF.

           PERFORM WITH TEST AFTER
             UNTIL PTR > CALLER-BUFLEN
               PERFORM U20-VAR-INPUT
               IF  PTR <= CALLER-BUFLEN
                 IF  DLM = '@'
                 AND DATA-IN(1:LEN) IS DIGITS
                   MOVE DATA-IN(1:LEN)
                                       TO VARNO
                   IF  VARNO < LSTVAR
                     PERFORM U20-VAR-RESTART
                   END-IF
                   PERFORM U20-VAR-OUTPUT
                     VARYING REQU-ITEM FROM REQU-ITEM BY 1
                       UNTIL REQU-ITEM > REQU-COUNT
                          OR REQU-VARNO(REQU-ITEM) = VARNO
                          OR OUT > LENGTH OF SEND-BUFFER
                   IF  REQU-ITEM <= REQU-COUNT
                     MOVE REQU-SECNO(REQU-ITEM)
                                       TO SECNO
                     PERFORM U20-VAR-INPUT
                     PERFORM U20-VAR-REPLACE
                     STRING TXT-MSTR-BUFFER(1:TXT-MSTR-BUFLEN)
                                          DELIMITED BY SIZE
                       INTO SEND-BUFFER WITH POINTER OUT
                     IF  DEBUG-ON
                       DISPLAY TXT-MSTR-BUFFER(1:TXT-MSTR-BUFLEN)
                                     UPON PRINTER
                     END-IF
                     MOVE VARNO        TO LSTVAR
                     ADD  1            TO REQU-ITEM
                   END-IF
                 END-IF
               END-IF
           END-PERFORM.

           PERFORM U20-VAR-OUTPUT
             VARYING REQU-ITEM FROM REQU-ITEM BY 1
               UNTIL REQU-ITEM > REQU-COUNT
                  OR OUT > LENGTH OF SEND-BUFFER.

           IF  DEBUG-ON
               DISPLAY ' '           UPON PRINTER
           END-IF.
           SUBTRACT 1                FROM OUT
                                   GIVING SEND-BUFLEN.
       J90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD JSON RESPONSE TEMPLATE                                 *
      ******************************************************************
       L00-LOAD-JSON-RESP-TEMPLATE.
           MOVE LOW-VALUES             TO RESP-TEMPLATE.
           MOVE TPID-NAME              TO RESP-NAME.
           MOVE 'JSON'                 TO RESP-TYPE.
           MOVE ZEROES                 TO RESP-COUNT
                                          ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.
           PERFORM T00-START-HTTPLTE THRU T05-EXIT.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL RTC-CODE > ZERO
                OR STAT-EOFILE(TPLT)
                OR RESP-COUNT > TPL-MAX
             PERFORM T10-NEXT-HTTPLTE THRU T15-EXIT
             IF  RTC-CODE = ZERO
             AND NOT STAT-EOFILE(TPLT)
               ADD  1                  TO RESP-COUNT
               MOVE TPLT-DATA(1:TPLT-DATL)
                                       TO TEMP-DATA
               PERFORM U10-LEFT-JUSTIFY
               MOVE LEN                TO RESP-LEN(RESP-COUNT)
               MOVE TEMP-DATA(1:LEN)   TO RESP-DATA(RESP-COUNT)
               MOVE 1                  TO PTR
               MOVE ZERO               TO VARNO
               PERFORM WITH TEST BEFORE
                 UNTIL PTR > LENGTH OF TEMP-DATA
                   PERFORM U50-JSON-TEMPLATE
                   IF  PTR <= LENGTH OF TEMP-DATA
                     IF  DLM2 = '{' OR JSON-A37(B:1) OR JSON-A47(B:1)
                       MOVE DLM2       TO BEGG
                       ADD  1          TO SECNO
                       IF  DLM2 = JSON-A37(B:1) OR JSON-A47(B:1)
                         ADD  1        TO ARYLVL
                       END-IF
                     END-IF
                     IF  DLM2 = '@'
                     AND TPLT-IN(1:LEN2) IS DIGITS
                       MOVE TPLT-IN(1:LEN2)
                                       TO VARNO
                     END-IF
                     IF  DLM2 = '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
                       MOVE DLM2       TO ENDG
                     END-IF
                   END-IF
               END-PERFORM
               MOVE BEGG               TO RESP-DELIM(RESP-COUNT)
               MOVE ARYLVL             TO RESP-ARYLVL(RESP-COUNT)
               MOVE SECNO              TO RESP-SECNO(RESP-COUNT)
               MOVE VARNO              TO RESP-VARNO(RESP-COUNT)
               IF  ENDG = '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
                 MOVE ENDG             TO RESP-DELIM(RESP-COUNT)
                 IF  ENDG = JSON-A37(E:1) OR JSON-A47(E:1)
                   SUBTRACT 1        FROM ARYLVL
                 END-IF
                 MOVE SPACE            TO ENDG
                 SUBTRACT 1          FROM SECNO
               END-IF
               MOVE SPACE              TO BEGG
             END-IF
           END-PERFORM.
           IF  RESP-COUNT > TPL-MAX
               DISPLAY THIS-PGM ': RESP TEMPLATE OVERFLOW: '
                       RESP-NAME     UPON CONSOLE
               MOVE +15                TO RTC-CODE
           END-IF.
           IF  DEBUG-ON
           OR  TABLE-ON
               DISPLAY 'TYPE  COUNT   ARY# SEC# VAR#  LENG DATA'
                                     UPON PRINTER
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > RESP-COUNT
                 DISPLAY 'RESP: ' IDX ' ' RESP-DELIM(IDX)
                                      ' ' RESP-ARYLVL(IDX)
                                      ' ' RESP-SECNO(IDX)
                                      ' ' RESP-VARNO(IDX)
                                      ' ' RESP-LEN(IDX)
                                      ' ' RESP-DATA(IDX)(1:60)
                                     UPON PRINTER
               END-PERFORM
               DISPLAY ' '           UPON PRINTER
           END-IF.
       L90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD XML RESPONSE TEMPLATE                                  *
      ******************************************************************
       M00-LOAD-XML-RESP-TEMPLATE.
           MOVE LOW-VALUES             TO RESP-TEMPLATE.
           MOVE TPID-NAME              TO RESP-NAME.
           MOVE 'XML'                  TO RESP-TYPE.
           MOVE ZEROES                 TO RESP-COUNT
                                          ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.
           PERFORM T00-START-HTTPLTE THRU T05-EXIT.
      *
      * As template is being loaded, parse it
      * to set array keys by section level
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL RTC-CODE > ZERO
                OR STAT-EOFILE(TPLT)
                OR RESP-COUNT > TPL-MAX
             PERFORM T10-NEXT-HTTPLTE THRU T15-EXIT
             IF  RTC-CODE = ZERO
             AND NOT STAT-EOFILE(TPLT)
               ADD  1                  TO RESP-COUNT
               MOVE TPLT-DATA(1:TPLT-DATL)
                                       TO TEMP-DATA
               PERFORM U10-LEFT-JUSTIFY
               MOVE LEN                TO RESP-LEN(RESP-COUNT)
               MOVE TEMP-DATA(1:LEN)   TO RESP-DATA(RESP-COUNT)
               MOVE 1                  TO PTR
               MOVE ZERO               TO VARNO
               PERFORM WITH TEST BEFORE
                 UNTIL PTR > LENGTH OF TEMP-DATA
                   PERFORM U50-XML-TEMPLATE
                   IF  PTR <= LENGTH OF TEMP-DATA
                     IF  DLM2 = '<' AND TEMP-DATA(PTR:1) NOT = '/'
                       MOVE DLM2       TO BEGG
                       ADD  1          TO SECNO
                     END-IF
                     IF  DLM2 = '@'
                     AND TPLT-IN(1:LEN2) IS DIGITS
                       MOVE TPLT-IN(1:LEN2)
                                       TO VARNO
                     END-IF
                     IF  DLM2 = '/'
                       MOVE DLM2       TO ENDG
                     END-IF
                   END-IF
               END-PERFORM
               MOVE BEGG               TO RESP-DELIM(RESP-COUNT)
               MOVE ARYLVL             TO RESP-ARYLVL(RESP-COUNT)
               MOVE SECNO              TO RESP-SECNO(RESP-COUNT)
               MOVE VARNO              TO RESP-VARNO(RESP-COUNT)
               IF  ENDG = '/'
                 MOVE ENDG             TO RESP-DELIM(RESP-COUNT)
                 MOVE SPACE            TO ENDG
                 SUBTRACT 1          FROM SECNO
               END-IF
               MOVE SPACE              TO BEGG
             END-IF
           END-PERFORM.
           IF  RESP-COUNT > TPL-MAX
               DISPLAY THIS-PGM ': RESP TEMPLATE OVERFLOW: '
                       RESP-NAME     UPON CONSOLE
               MOVE +15                TO RTC-CODE
           END-IF.
           IF  DEBUG-ON
           OR  TABLE-ON
               DISPLAY 'TYPE  COUNT   ARY# SEC# VAR#  LENG DATA'
                                     UPON PRINTER
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > RESP-COUNT
                 DISPLAY 'RESP: ' IDX ' ' RESP-DELIM(IDX)
                                      ' ' RESP-ARYLVL(IDX)
                                      ' ' RESP-SECNO(IDX)
                                      ' ' RESP-VARNO(IDX)
                                      ' ' RESP-LEN(IDX)
                                      ' ' RESP-DATA(IDX)(1:60)
                                     UPON PRINTER
               END-PERFORM
               DISPLAY ' '           UPON PRINTER
           END-IF.
       M90-EXIT.
           EXIT.

      /*****************************************************************
      *    PARSE JSON RESPONSE INPUT                                   *
      ******************************************************************
       P00-PARSE-JSON-RESPONSE.
           MOVE ZERO                   TO SECNO.
           MOVE 1                      TO RESP-ITEM
                                          PTR OUT.

           PERFORM WITH TEST AFTER
             UNTIL PTR > RECV-BUFLEN
               PERFORM U30-JSON-INPUT
               IF  PTR <= RECV-BUFLEN
                 IF  DLM = '{' OR JSON-A37(B:1) OR JSON-A47(B:1)
                   ADD  1              TO SECNO
                 END-IF
                 IF  DLM = ':'
                   PERFORM U60-FIND-KEY-VAR THRU U65-EXIT
                   IF  WAS-FOUND
                   AND RESP-VARNO(RESP-ITEM) > ZERO
                     IF  RECV-BUFFER(PTR + 0:1) = '"'
                     OR  RECV-BUFFER(PTR + 1:1) = '"'
                       PERFORM U40-DATA-STRING
                       MOVE DATA-IN(1:LEN) TO VAR-DATA(1:LEN)
                       STRING '"' VAR-DATA(1:LEN) '"'
                         DELIMITED BY SIZE INTO DATA-IN
                     ELSE
                       PERFORM U40-DATA-VALUE
                     END-IF
                     MOVE RESP-VARNO(RESP-ITEM)
                                       TO EDIT-VAR
                     MOVE EDIT-VAR     TO TEMP-DATA
                     PERFORM U10-LEFT-JUSTIFY
                     MOVE TEMP-DATA(1:LEN)
                                       TO EDIT-VARX
                     MOVE DATA-IN      TO TEMP-DATA
                     PERFORM U10-LEFT-JUSTIFY
                     STRING '@'           DELIMITED BY SIZE
                            EDIT-VARX     DELIMITED BY SPACE
                            '@'           DELIMITED BY SIZE
                            TEMP-DATA(1:LEN) DELIMITED BY SIZE
                       INTO DATA-BUFFER WITH POINTER OUT
                   END-IF
                 END-IF
                 IF  DLM = '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
                   SUBTRACT 1        FROM SECNO
                 END-IF
               END-IF
           END-PERFORM.

           SUBTRACT 1                FROM OUT
                                   GIVING DATA-BUFLEN.
       P90-EXIT.
           EXIT.

      /*****************************************************************
      *    PARSE XML RESPONSE INPUT                                    *
      ******************************************************************
       R00-PARSE-XML-RESPONSE.
           MOVE ZERO                   TO SECNO.
           MOVE 1                      TO RESP-ITEM
                                          PTR OUT.

           PERFORM WITH TEST AFTER
             UNTIL PTR > RECV-BUFLEN
               PERFORM U30-XML-INPUT
               IF  PTR <= RECV-BUFLEN
                 IF  DLM = '<' AND RECV-BUFFER(PTR:1) NOT = '/'
                   ADD  1              TO SECNO
                 END-IF
                 IF  DLM = '='
                 OR  DLM = '<' AND RECV-BUFFER(PTR:1) = '/'
                   PERFORM U60-FIND-KEY-VAR THRU U65-EXIT
                   IF  WAS-FOUND
                   AND RESP-VARNO(RESP-ITEM) > ZERO
                     IF  RECV-BUFFER(PTR + 0:1) = '"'
                     OR  RECV-BUFFER(PTR + 1:1) = '"'
                     OR  RECV-BUFFER(PTR + 0:1) = ''''
                     OR  RECV-BUFFER(PTR + 1:1) = ''''
                     OR  DLM = '<' AND RECV-BUFFER(PTR:1) = '/'
                       PERFORM U40-DATA-STRING
                       MOVE DATA-IN(1:LEN) TO VAR-DATA(1:LEN)
                       STRING '"' VAR-DATA(1:LEN) '"'
                         DELIMITED BY SIZE INTO DATA-IN
                     ELSE
                       PERFORM U40-DATA-VALUE
                     END-IF
                     MOVE RESP-VARNO(RESP-ITEM)
                                       TO EDIT-VAR
                     MOVE EDIT-VAR     TO TEMP-DATA
                     PERFORM U10-LEFT-JUSTIFY
                     MOVE TEMP-DATA(1:LEN)
                                       TO EDIT-VARX
                     MOVE DATA-IN      TO TEMP-DATA
                     PERFORM U10-LEFT-JUSTIFY
                     STRING '@'           DELIMITED BY SIZE
                            EDIT-VARX     DELIMITED BY SPACE
                            '@'           DELIMITED BY SIZE
                            TEMP-DATA(1:LEN) DELIMITED BY SIZE
                       INTO DATA-BUFFER WITH POINTER OUT
                   END-IF
                 END-IF
                 IF  DLM = '/'
                   SUBTRACT 1        FROM SECNO
                 END-IF
               END-IF
           END-PERFORM.

           SUBTRACT 1                FROM OUT
                                   GIVING DATA-BUFLEN.
       R90-EXIT.
           EXIT.

      /*****************************************************************
      *    OTHER PERFORMED ROUTINES                                    *
      ******************************************************************

       T00-START-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.
           MOVE ZEROES                 TO TPLT-SEQ.
           SET  FUNC-START(TPLT)       TO TRUE.
           START HTTPLTE KEY > TPLT-KEY END-START.
           IF  NOT STAT-NORMAL(TPLT)
               MOVE TPLT               TO VSUB
               MOVE TPLT-KEY           TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               GO TO T05-EXIT
           END-IF.
       T05-EXIT.
           EXIT.

       T10-NEXT-HTTPLTE.
           MOVE LENGTH OF HTTPLTE-RECORD
                                       TO TPLT-RECL.
           SET  FUNC-READNEXT(TPLT)    TO TRUE.
           READ HTTPLTE NEXT RECORD END-READ
           IF  NOT STAT-NORMAL(TPLT)
           OR  TPLT-ID NOT = TPID-KEY
               IF  STAT-EOFILE(TPLT)
               OR  TPLT-ID NOT = TPID-KEY
                 SET STAT-EOFILE(TPLT) TO TRUE
               ELSE
                 MOVE TPLT             TO VSUB
                 MOVE TPLT-KEY         TO VSAM-KEYD(VSUB)
                 PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               GO TO T15-EXIT
           END-IF.
           SUBTRACT LENGTH OF TPLT-KEY
                                     FROM TPLT-RECL
                                   GIVING TPLT-DATL.
       T15-EXIT.
           EXIT.

       U00-SET-LENGTH.
           IF  VAR-DATA > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF VAR-DATA BY -1
                   UNTIL LEN < 1
                      OR VAR-DATA(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
               MOVE SPACES             TO VAR-DATA
           END-IF.

       U10-LEFT-JUSTIFY.
           IF  TEMP-DATA > SPACES
               MOVE ZEROES             TO TALLY
               INSPECT TEMP-DATA TALLYING TALLY FOR LEADING SPACES
               MOVE TEMP-DATA(TALLY + 1:)  TO TEMP-DATA(1:)
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF TEMP-DATA BY -1
                   UNTIL LEN < 1
                      OR TEMP-DATA(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
               MOVE SPACES             TO TEMP-DATA
           END-IF.

       U20-VAR-INPUT.
           UNSTRING CALLER-BUFFER(1:CALLER-BUFLEN)
             DELIMITED BY '@'
             INTO DATA-IN
                    DELIMITER IN DLM
                    COUNT     IN LEN
             WITH POINTER PTR
           END-UNSTRING.

       U20-VAR-REPLACE.
           MOVE REQU-LEN(REQU-ITEM)    TO TXT-MSTR-BUFLEN.
           MOVE REQU-DATA(REQU-ITEM)   TO TXT-MSTR-BUFFER.

           MOVE REQU-VARNO(REQU-ITEM)  TO EDIT-VAR.
           MOVE EDIT-VAR               TO TEMP-DATA.
           PERFORM U10-LEFT-JUSTIFY.
           MOVE TEMP-DATA(1:LEN)       TO EDIT-VARX.
           MOVE DATA-IN                TO TEMP-DATA
           PERFORM U10-LEFT-JUSTIFY

           MOVE 1                      TO POS.
           STRING '@'                     DELIMITED BY SIZE
                  EDIT-VARX               DELIMITED BY SPACE
                  '@'                     DELIMITED BY SIZE
               INTO VAR-DATA         WITH POINTER POS.

           SET  TXT-REQUEST-FIND       TO TRUE.
           MOVE ZEROES                 TO TXT-PNTR.
           SUBTRACT 1     FROM POS GIVING TXT-PARM-STRLEN.
           MOVE VAR-DATA(1:POS)        TO TXT-PARM-STRING.
           CALL TXTMAN              USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           SET  TXT-REQUEST-DELETE     TO TRUE.
           MOVE SPACES                 TO TXT-PARM-STRING.
           CALL TXTMAN              USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           SUBTRACT TXT-PARM-STRLEN  FROM TXT-MSTR-BUFLEN.
           SET  TXT-REQUEST-INSERT     TO TRUE.
           MOVE LEN                    TO TXT-PARM-STRLEN.
           MOVE TEMP-DATA(1:LEN)       TO TXT-PARM-STRING.
           ADD  TXT-PARM-STRLEN        TO TXT-MSTR-BUFLEN.
           CALL TXTMAN              USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.

       U20-VAR-RESTART.
           PERFORM U20-VAR-OUTPUT
             VARYING REQU-ITEM FROM REQU-ITEM BY 1
               UNTIL REQU-ITEM > REQU-COUNT
                  OR REQU-SECNO(REQU-ITEM) NOT = SECNO
                  OR OUT > LENGTH OF SEND-BUFFER.
           SUBTRACT 1                FROM REQU-ITEM.
           PERFORM WITH TEST BEFORE
             VARYING REQU-ITEM FROM REQU-ITEM BY -1
               UNTIL REQU-ITEM < 1
                  OR REQU-SECNO(REQU-ITEM) NOT = SECNO
           END-PERFORM.
           ADD  1                      TO REQU-ITEM.
           IF  REQU-TYPE = 'JSON'
               STRING ','                 DELIMITED BY SIZE
                 INTO SEND-BUFFER    WITH POINTER OUT
               IF  DEBUG-ON
                 DISPLAY ','         UPON PRINTER
               END-IF
           END-IF.

       U20-VAR-OUTPUT.
           IF  REQU-VARNO(REQU-ITEM) = ZERO
             MOVE REQU-LEN(REQU-ITEM)  TO LEN
             STRING REQU-DATA(REQU-ITEM)(1:LEN)
                                          DELIMITED BY SIZE
               INTO SEND-BUFFER      WITH POINTER OUT
             IF  REQU-TYPE = 'XML'
               STRING CRNL                DELIMITED BY SIZE
                 INTO SEND-BUFFER    WITH POINTER OUT
             END-IF
             IF  DEBUG-ON
               DISPLAY REQU-DATA(REQU-ITEM)(1:LEN)
                                     UPON PRINTER
             END-IF
           END-IF.

       U30-JSON-INPUT.
           UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
             DELIMITED BY '{' OR ':' OR '}' OR ','
                       OR JSON-A37(B:1) OR JSON-A47(B:1)
                       OR JSON-A37(E:1) OR JSON-A47(E:1)
             INTO DATA-IN
                    DELIMITER IN DLM
                    COUNT     IN LEN
             WITH POINTER PTR
           END-UNSTRING.

       U30-XML-INPUT.
           PERFORM WITH TEST BEFORE
             VARYING PDX FROM PTR BY 1
               UNTIL PDX > (HTTP-GET-DATA-LEN - 1)
                  OR RECV-BUFFER(PDX:2) = '</'
                  OR RECV-BUFFER(PDX:1) = '=' OR '<' OR '>'
           END-PERFORM.
           IF  RECV-BUFFER(PDX:2) = '</'
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY '<'
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
           ELSE
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY '<' OR ' ' OR '=' OR '/' OR '>'
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
           END-IF.

       U40-DATA-STRING.
           EVALUATE TRUE
           WHEN RECV-BUFFER(PTR + 0:1) = ''''
           OR   RECV-BUFFER(PTR + 1:1) = ''''
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY ''''
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY ''''
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
           WHEN RECV-BUFFER(PTR + 0:1) = '"'
           OR   RECV-BUFFER(PTR + 1:1) = '"'
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY '"'
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
               UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
                 DELIMITED BY '"'
                 INTO DATA-IN
                        DELIMITER IN DLM
                        COUNT     IN LEN
                 WITH POINTER PTR
               END-UNSTRING
           END-EVALUATE.

       U40-DATA-VALUE.
           UNSTRING RECV-BUFFER(1:HTTP-GET-DATA-LEN)
             DELIMITED BY ALL ' ' OR ',' OR '}' OR '<' OR '>'
                           OR JSON-A37(E:1) OR JSON-A47(E:1)
             INTO DATA-IN
                    DELIMITER IN DLM
                    COUNT     IN LEN
             WITH POINTER PTR
           END-UNSTRING.
           IF DLM = '<' OR '>' OR '}' OR JSON-A37(E:1) OR JSON-A47(E:1)
             SUBTRACT 1     FROM PTR
             MOVE SPACE       TO DLM
           END-IF.

       U50-JSON-TEMPLATE.
           UNSTRING TEMP-DATA
             DELIMITED BY '{' OR '@' OR '}'
                       OR JSON-A37(B:1) OR JSON-A47(B:1)
                       OR JSON-A37(E:1) OR JSON-A47(E:1)
             INTO TPLT-IN
                    DELIMITER IN DLM2
                    COUNT     IN LEN2
             WITH POINTER PTR
           END-UNSTRING.

       U50-XML-TEMPLATE.
           UNSTRING TEMP-DATA
             DELIMITED BY '<' OR '@' OR '/' OR '>'
             INTO TPLT-IN
                    DELIMITER IN DLM2
                    COUNT     IN LEN2
             WITH POINTER PTR
           END-UNSTRING.

       U60-FIND-KEY-VAR.
           SET  NOT-FOUND              TO TRUE.
           MOVE DATA-IN                TO TEMP-DATA.
           PERFORM U10-LEFT-JUSTIFY.
           IF  LEN = ZERO
               GO TO U65-EXIT
           END-IF.

           EVALUATE TRUE
           WHEN RESP-TYPE = 'XML'
           AND DLM = '<' AND RECV-BUFFER(PTR:1) = '/'
             PERFORM WITH TEST BEFORE
               VARYING RESP-ITEM FROM RESP-ITEM BY 1
                 UNTIL RESP-ITEM > RESP-COUNT
                    OR RESP-DATA(RESP-ITEM)(1:2) = '</'
             END-PERFORM
             IF  RESP-ITEM <= RESP-COUNT
             AND RESP-DATA(RESP-ITEM)(1:2) = '</'
               IF  RESP-VARNO(RESP-ITEM) = ZERO
                 SUBTRACT 1          FROM RESP-ITEM
               END-IF
               MOVE RESP-SECNO(RESP-ITEM)
                                       TO SECNO
               SET  WAS-FOUND          TO TRUE
             END-IF
           WHEN RESP-TYPE = 'XML'
             PERFORM U70-CURR-SECTION THRU U75-EXIT
             PERFORM WITH TEST AFTER
               VARYING RESP-ITEM FROM RESP-ITEM BY 1
                 UNTIL RESP-ITEM > RESP-COUNT
                    OR WAS-FOUND
               IF  LEN <= RESP-LEN(RESP-ITEM)
                 PERFORM WITH TEST AFTER
                   VARYING POS FROM 1 BY 1
                     UNTIL POS > RESP-LEN(RESP-ITEM) - LEN + 1
                        OR WAS-FOUND
                   IF  RESP-DATA(RESP-ITEM)(POS:LEN) = TEMP-DATA(1:LEN)
                     MOVE RESP-SECNO(RESP-ITEM)
                                       TO SECNO
                     SET  WAS-FOUND    TO TRUE
                   END-IF
                 END-PERFORM
               END-IF
             END-PERFORM
           WHEN RESP-TYPE = 'JSON'
             PERFORM U70-CURR-SECTION THRU U75-EXIT
             PERFORM WITH TEST AFTER
               VARYING RESP-ITEM FROM RESP-ITEM BY 1
                 UNTIL WAS-FOUND
                    OR RESP-ITEM > RESP-COUNT
                    OR RESP-DELIM(RESP-ITEM) = JSON-A37(E:1)
                                            OR JSON-A47(E:1)
               IF  RESP-ITEM <= RESP-COUNT
               AND LEN <= RESP-LEN(RESP-ITEM)
               AND (RESP-DELIM(RESP-ITEM) NOT = JSON-A37(E:1)
                                             OR JSON-A47(E:1))
                 PERFORM WITH TEST AFTER
                   VARYING POS FROM 1 BY 1
                     UNTIL POS > RESP-LEN(RESP-ITEM) - LEN + 1
                        OR WAS-FOUND
                   IF  RESP-DATA(RESP-ITEM)(POS:LEN) = TEMP-DATA(1:LEN)
                     MOVE RESP-SECNO(RESP-ITEM)
                                       TO SECNO
                     SET  WAS-FOUND    TO TRUE
                   END-IF
                 END-PERFORM
               END-IF
             END-PERFORM
             IF  NOT-FOUND
             AND RESP-ITEM <= RESP-COUNT
             AND (RESP-DELIM(RESP-ITEM) NOT = JSON-A37(E:1)
                                           OR JSON-A47(E:1))
               PERFORM U80-CURR-ARRAY THRU U85-EXIT
               PERFORM WITH TEST AFTER
                 VARYING RESP-ITEM FROM RESP-ITEM BY 1
                   UNTIL WAS-FOUND
                      OR RESP-ITEM > RESP-COUNT
                      OR RESP-DELIM(RESP-ITEM) = JSON-A37(E:1)
                                              OR JSON-A47(E:1)
                 IF  RESP-ITEM <= RESP-COUNT
                 AND LEN <= RESP-LEN(RESP-ITEM)
                 AND (RESP-DELIM(RESP-ITEM) NOT = JSON-A37(E:1)
                                               OR JSON-A47(E:1))
                   PERFORM WITH TEST AFTER
                     VARYING POS FROM 1 BY 1
                       UNTIL POS > RESP-LEN(RESP-ITEM) - LEN + 1
                          OR WAS-FOUND
                     IF RESP-DATA(RESP-ITEM)(POS:LEN) = TEMP-DATA(1:LEN)
                       MOVE RESP-SECNO(RESP-ITEM)
                                       TO SECNO
                       SET  WAS-FOUND  TO TRUE
                     END-IF
                   END-PERFORM
                 END-IF
               END-PERFORM
             END-IF
             IF  NOT-FOUND
             AND RESP-ITEM <= RESP-COUNT
             AND (RESP-DELIM(RESP-ITEM) NOT = JSON-A37(E:1)
                                           OR JSON-A47(E:1))
               IF  RESP-ARYLVL(RESP-ITEM) > 1
                 PERFORM U82-PREV-ARRAY THRU U85-EXIT
               END-IF
               PERFORM WITH TEST AFTER
                 VARYING RESP-ITEM FROM RESP-ITEM BY 1
                   UNTIL WAS-FOUND
                      OR RESP-ITEM > RESP-COUNT
                 IF  RESP-ITEM <= RESP-COUNT
                 AND LEN <= RESP-LEN(RESP-ITEM)
                   PERFORM WITH TEST AFTER
                     VARYING POS FROM 1 BY 1
                       UNTIL POS > RESP-LEN(RESP-ITEM) - LEN + 1
                          OR WAS-FOUND
                     IF RESP-DATA(RESP-ITEM)(POS:LEN) = TEMP-DATA(1:LEN)
                       MOVE RESP-SECNO(RESP-ITEM)
                                       TO SECNO
                       SET  WAS-FOUND  TO TRUE
                     END-IF
                   END-PERFORM
                 END-IF
               END-PERFORM
             END-IF
           END-EVALUATE.
       U65-EXIT.
           EXIT.

       U70-CURR-SECTION.
           MOVE RESP-SECNO(RESP-ITEM)  TO SECNO.
           PERFORM WITH TEST AFTER
             VARYING RESP-ITEM FROM RESP-ITEM BY -1
               UNTIL RESP-ITEM < 1
                  OR RESP-SECNO(RESP-ITEM) NOT = SECNO
           END-PERFORM.
           ADD  1                      TO RESP-ITEM.
       U75-EXIT.
           EXIT.

       U80-CURR-ARRAY.
           MOVE RESP-ARYLVL(RESP-ITEM) TO ARYLVL.
           GO TO U84-FIND-BEGIN.
       U82-PREV-ARRAY.
           MOVE 1                      TO ARYLVL.
       U84-FIND-BEGIN.
           PERFORM WITH TEST AFTER
             VARYING RESP-ITEM FROM RESP-ITEM BY -1
               UNTIL RESP-ITEM < 1
                  OR RESP-ARYLVL(RESP-ITEM) = ARYLVL
                 AND (RESP-DELIM(RESP-ITEM) = JSON-A37(B:1)
                                           OR JSON-A47(B:1))
           END-PERFORM.
           ADD  1                      TO RESP-ITEM.
       U85-EXIT.
           EXIT.

