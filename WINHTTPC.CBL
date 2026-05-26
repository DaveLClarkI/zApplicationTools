       IDENTIFICATION DIVISION.

       PROGRAM-ID.    WINHTTPC.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  OCT 2024.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       WIN HTTP ONLINE CLIENT.

      * CHANGE HISTORY ------------------------------------------------
      * 10/28/2024 DLC Original design.  For initial implimentation,
      *                arbitrarily supports up to 64K in both output
      *                and input buffers with DTLG log notice if(when)
      *                actual usage exceeds 32K in either direction.
      *
      *                The template generation and parsing code used
      *                here is also used in the WINHTTPB batch program.
      *
      * 12/24/2025 DLC Add support for HTML ACCEPT header.
      *                Capture JSON Array Level for parsing purposes.
      * 01/05/2026 DLC Allow generated data to be returned to caller.
      * END OF HISTORY ------------------------------------------------

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *          W I N   H T T P   O N L I N E   C L I E N T          *
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
      *      This program represents an  online client for performing *
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
      *      The invoking program only  needs  to pass a 90-byte com- *
      * munication area,  with the  following parameter elements,  in *
      * order to call a web service:                                  *
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
      *      However,  note that some  parameter elements are passing *
      * addresses instead of actual values.  This is required so that *
      * (1) you don't  run  into  limitations  on  the  length of the *
      * parameter block that can be passed back and forth between the *
      * called and calling programs,  and (2) a single subprogram can *
      * be used for many  different  web service calls.   (See "INPUT *
      * and OUTPUT" for how to obtain the addresses needed.)          *
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
      *      web service.   These are  pre-defined  by  the web world *
      *      with the following meanings:                             *
      *        GET    - Retrieves data from the resource specified in *
      *                 the URL without altering it;                  *
      *        POST   - Creates a new resource, such as adding a file *
      *                 to a directory or a row to a database table,  *
      *                 using the data included in the request body;  *
      *        PUT    - Updates an existing resource in its entirety  *
      *                 using the data included in the request body;  *
      *        PATCH  - Updates part of an existing resource using    *
      *                 the data included in the request body; and,   *
      *        DELETE - Completely removes a resource, as specified   *
      *                 in the URL.                                   *
      *                                                               *
      *      NOTE: The first four request  types also have a TEXT     *
      *            or BINARY mode.  The base mode leaves it up to     *
      *            the VSE HTTP  (IESHTTPB)  client as to whether     *
      *            EBCDIC-to-ASCII  and  ASCII-to-EBCDIC transla-     *
      *            tion occurs.  Using an explicit mode overrides     *
      *            (or, forces) IESHTTPB translation.                 *
      *                                                               *
      *      NOTE: The CLOSE request is for batch, only.              *
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
      *      First,  set up the  following  data  areas in your COBOL *
      * Working-Storage Section.                                      *
      *                                                               *
      * +----1----+----2----+----3----+----4----+----5----+----6----+ *
      *    77  WINHTTPC                    PIC X(8)  VALUE 'WINHTTPC'.*
      *                                                               *
      *    COPY WINHTTPC.                                             *
      *                                                               *
      * Then, you can invoke this subprogram as follows:              *
      *                                                               *
      *        EXEC CICS LINK                                         *
      *                  PROGRAM(WINHTTPC)                            *
      *                  COMMAREA(WINHTTPC-PARMS)                     *
      *                  NOHANDLE                                     *
      *        END-EXEC                                               *
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
      * ============================================================= *
      *                                                               *
      *                         RETURN CODES                          *
      *                                                               *
      *      Rather than issue return  codes in the usual way,  which *
      * might have a more severe impact than desired, this subprogram *
      * passes back those return  codes  in  the parameter block that *
      * was passed by the  calling  program.   This is in addition to *
      * the HTTP response,  which is also  passed back to the caller. *
      * It is the caller's responsibility  to  check BOTH of these to *
      * determine any remedial action that might be required.         *
      *                                                               *
      *      The return codes from this subprogram are as follows:    *
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
           CLASS  DIGITS               IS '0' THRU '9'.

      /****************************************************************
      *                                                               *
      * DATA DIVISION                                                 *
      *                                                               *
      *****************************************************************
       DATA DIVISION.

      *****************************************************************
      *    WORKING-STORAGE SECTION                                    *
      *****************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(1)    VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'WINHTTPC'.
         03  IESHTTPC                  PIC  X(8)    VALUE 'IESHTTPC'.

         03  HTTPLID                   PIC  X(8)    VALUE 'HTTPLID'.
         03  HTTPLTE                   PIC  X(8)    VALUE 'HTTPLTE'.

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
         03  TPID-RECL                 PIC S9(4)    BINARY VALUE ZEROES.
         03  TPLT-RECL                 PIC S9(4)    BINARY VALUE ZEROES.
         03  TPLT-DATL                 PIC S9(4)    BINARY VALUE ZEROES.
         03  VLEN                      PIC  9(7)    VALUE ZEROES.
         03  PDX                       PIC S9(9)    BINARY.
         03  PTR                       PIC S9(9)    BINARY.
         03  OUT                       PIC S9(9)    BINARY.
         03  CALLER-BUFLEN             PIC S9(9)    BINARY VALUE ZEROES.
         03  SEND-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.
         03  RECV-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.
         03  DATA-BUFLEN               PIC S9(9)    BINARY VALUE ZEROES.

         03  DEBUG-SWITCH              PIC  X       VALUE 'N'.
           88  NO-DEBUG                             VALUE 'N'.
           88  DEBUG-ON                             VALUE 'Y'.
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

       COPY BITMAN.

       COPY HEXMAN.

       COPY IESHTTPR.

       COPY LOGGING.

       COPY NUMEDIT.

       COPY TXTMAN.

       COPY UNEXERRW.

      /****************************************************************
      *    LINKAGE SECTION                                            *
      *****************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
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

       COPY HTTPLID.

       COPY HTTPLTE.

       01  VDATA.
         03                            PIC  X(256).

       01  CALLER-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).

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

       01  SEND-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).

       01  RECV-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).

       01  DATA-BUFFER.
         03                            PIC  X(32768).
         03                            PIC  X(32768).

      /****************************************************************
      *                                                               *
      * PROCEDURE DIVISION                                            *
      *                                                               *
      *****************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-MAINLINE-ROUTINE.

           SET  DTLG-LOGGING           TO TRUE.
           MOVE ZEROES                 TO HTTP-RTNCODE.

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
               MOVE +16                TO HTTP-RTNCODE
               GO TO Z90-HTTP-TERMINATION
           END-IF.

           IF  NOT HTTP-NO-TEMPLATE
               IF  HTTP-TEMPLATE-ID <= SPACES
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
               PERFORM T10-INITKEY-HTTPLID
               MOVE HTTP-TEMPLATE-ID   TO TPID-KEY
               PERFORM T11-STARTBR-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                 STRING 'STARTBWS ON TEMPLATE ID = '
                        TPID-KEY          DELIMITED BY SIZE
                                     INTO LOGF-MESG
                 PERFORM Q200-UNEX-LOG
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
               PERFORM T13-READNEXT-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                 STRING 'READNEXT ON TEMPLATE ID = '
                        TPID-KEY          DELIMITED BY SIZE
                                     INTO LOGF-MESG
                 PERFORM Q200-UNEX-LOG
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
               IF  TPID-NAME NOT = HTTP-TEMPLATE-ID
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
           END-IF.

           IF  HTTP-URL-ADR OF DFHCOMMAREA = NULL
           OR  HTTP-URL-LEN OF DFHCOMMAREA < 16
               MOVE +14                TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
           END-IF.

           EVALUATE TRUE
           WHEN HTTP-NO-TEMPLATE
               IF (HTTP-RECV-DATA-ADR = NULL
               OR  HTTP-RECV-DATA-LEN < 64)
               OR (HTTP-SEND-DATA-ADR = NULL
               AND HTTP-SEND-DATA-LEN > ZERO)
               OR (HTTP-SEND-DATA-ADR NOT = NULL
               AND HTTP-SEND-DATA-LEN < 1)
                 MOVE +13              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
               IF  HTTP-RECV-CONTENT-ADR = NULL
               OR  HTTP-RECV-CONTENT-LEN < 8
               OR  HTTP-SEND-DATA-ADR NOT = NULL
               AND (HTTP-SEND-CONTENT-ADR = NULL
                OR  HTTP-SEND-CONTENT-LEN < 8)
                 MOVE +12              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
           WHEN OTHER
               IF (HTTP-RECV-DATA-ADR = NULL
               OR  HTTP-RECV-DATA-LEN < 64)
               OR  TPID-REQU
               AND (HTTP-SEND-DATA-ADR = NULL
                OR  HTTP-SEND-DATA-LEN < 1)
                 MOVE +13              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
           END-EVALUATE.

           IF  HTTP-RESPONSE-ADR OF DFHCOMMAREA = NULL
           OR  HTTP-RESPONSE-LEN OF DFHCOMMAREA < 3
               MOVE +11                TO HTTP-RTNCODE
               GO TO Z90-HTTP-TERMINATION
           END-IF.

      /*****************************************************************
      *    PROCESS HTTP REQUEST                                        *
      ******************************************************************
       C00-HTTP-REQUEST.

           MOVE LOW-VALUES             TO HTTP-REQ.
           MOVE LENGTH OF HTTP-REQ     TO HTTP-REQ-LEN.
           SET  HTTP-URL-ADR OF HTTP-REQ
                                       TO HTTP-URL-ADR OF DFHCOMMAREA
           MOVE HTTP-URL-LEN OF DFHCOMMAREA
                                       TO HTTP-URL-LEN OF HTTP-REQ.

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
      *
           CALL 'GETADDR'           USING USER-AGENT HTTP-USER-AGENT-ADR
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
               EXEC CICS GETMAIN
                         SET     (ADDRESS OF SEND-BUFFER)
                         FLENGTH (LENGTH OF SEND-BUFFER)
                         INITIMG (LOWVALUE)
               END-EXEC
      *
               SET  HTTP-POST-BUFFER   TO TRUE
               IF  TPID-XML
                 IF  REQU-NAME NOT = TPID-NAME
                   PERFORM D00-LOAD-XML-REQU-TEMPLATE THRU D90-EXIT
                 END-IF
      * for debugging, this also displays the generated data lines
                 PERFORM G00-GENERATE-XML-REQUEST THRU G90-EXIT
                 CALL 'GETADDR' USING SEND-BUFFER  HTTP-POST-DATA-ADR
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
                 CALL 'GETADDR' USING SEND-BUFFER  HTTP-POST-DATA-ADR
                                      JSON-CONTENT HTTP-POST-CONTENT-ADR
                 MOVE SEND-BUFLEN      TO HTTP-POST-DATA-LEN
                 MOVE JSON-CONTENT     TO VAR-DATA
                 PERFORM U00-SET-LENGTH
                 MOVE LEN              TO HTTP-POST-CONTENT-LEN
               END-IF
      *
               IF  SEND-BUFLEN >= (64 * 1024)
                 MOVE +10              TO EDT-INTG
                 MOVE +00              TO EDT-DECM
                 MOVE SEND-BUFLEN      TO EDT-REQ-D0
                 SET  EDT-COMMAS-YES   TO TRUE
                 CALL 'NUMEDIT'     USING NUMEDIT-PARMS
                 STRING 'SEND BUFFER OVERFLOW: '
                        EDT-OUTP(1:EDT-OUTL) DELIMITED BY SIZE
                                     INTO LOGF-MESG
                 PERFORM Q100-LOGIT  THRU Q199-EXIT
                 MOVE +12              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
           END-EVALUATE.
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
                 GO TO Z90-HTTP-TERMINATION
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
           EXEC CICS GETMAIN
                     SET     (ADDRESS OF RECV-BUFFER)
                     FLENGTH (LENGTH OF RECV-BUFFER)
                     INITIMG (LOWVALUE)
           END-EXEC.
      *
           SET  HTTP-GET-BUFFER        TO TRUE.
           CALL 'GETADDR'        USING RECV-BUFFER  HTTP-GET-DATA-ADR
                                       RECV-CONTENT HTTP-GET-CONTENT-ADR
           MOVE LENGTH OF RECV-BUFFER  TO HTTP-GET-DATA-LEN.
           MOVE LENGTH OF RECV-CONTENT TO HTTP-GET-CONTENT-LEN

           SET  HTTP-RESPONSE-ADR OF HTTP-REQ
                                       TO HTTP-RESPONSE-ADR
                                                      OF DFHCOMMAREA.
           MOVE HTTP-RESPONSE-LEN OF DFHCOMMAREA
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
               CALL 'GETADDR'       USING PROXY-HTTP-URL
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
               CALL 'GETADDR'    USING KEYRING-LIB HTTP-KEY-RING-LIB-ADR
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

           MOVE 120                    TO HTTP-SESS-TIMEOUT.

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
           IF  SEND-BUFLEN > (32 * 1024)
               MOVE +10                TO EDT-INTG
               MOVE +00                TO EDT-DECM
               MOVE SEND-BUFLEN        TO EDT-REQ-D0
               SET  EDT-COMMAS-YES     TO TRUE
               CALL 'NUMEDIT'       USING NUMEDIT-PARMS
               STRING 'SENDING  MORE THAN 32K: '
                      EDT-OUTP(1:EDT-OUTL) DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
           END-IF.

      * call out to the requested web service
           EXEC CICS LINK
                     PROGRAM(IESHTTPC)
                     COMMAREA(HTTP-REQ)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE EIBRESP2           TO HTTP-RTNCODE
                                          EDIT-CODE
               STRING 'ERROR IN IESHTTPC, RC=' EDIT-CODE
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               GO TO Z90-HTTP-TERMINATION
           END-IF.

      * this is a heads-up failure of truncation on receipt
           IF  HTTP-GET-DATA-LEN >= (64 * 1024)
               MOVE +10                TO EDT-INTG
               MOVE +00                TO EDT-DECM
               MOVE HTTP-GET-DATA-LEN  TO EDT-REQ-D0
               SET  EDT-COMMAS-YES     TO TRUE
               CALL 'NUMEDIT'       USING NUMEDIT-PARMS
               STRING 'RECEIVE BUFFER IS FULL: '
                      EDT-OUTP(1:EDT-OUTL) DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               MOVE +12                TO HTTP-RTNCODE
           ELSE
      * this is just a heads-up notice of things to come
           IF  HTTP-GET-DATA-LEN > (32 * 1024)
               MOVE +10                TO EDT-INTG
               MOVE +00                TO EDT-DECM
               MOVE HTTP-GET-DATA-LEN  TO EDT-REQ-D0
               SET  EDT-COMMAS-YES     TO TRUE
               CALL 'NUMEDIT'       USING NUMEDIT-PARMS
               STRING 'RECEIVED MORE THAN 32K: '
                      EDT-OUTP(1:EDT-OUTL) DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
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
           AND HTTP-GET-DATA-LEN < (64 * 1024)
      * let's see the raw response from the server
             MOVE 20                   TO TXT-MSTR-BUFLEN
             MOVE 'RECEIVED DATA LINES:' TO TXT-MSTR-BUFFER
             EXEC CICS WRITEQ TD QUEUE('IESN')
                       FROM(TXT-MSTR-BUFFER)
                       LENGTH(TXT-MSTR-BUFLEN)
                       NOHANDLE END-EXEC
             PERFORM WITH TEST BEFORE
               VARYING IDX FROM 1 BY 120
                 UNTIL IDX > RECV-BUFLEN
               IF  IDX > RECV-BUFLEN
                 COMPUTE LEN = RECV-BUFLEN - (IDX - 120)
               ELSE
                 MOVE 120              TO LEN
               END-IF
               EXEC CICS WRITEQ TD QUEUE('IESN')
                         FROM(RECV-BUFFER(IDX:LEN))
                         LENGTH(LEN) NOHANDLE END-EXEC
             END-PERFORM
             MOVE SPACE                TO ENDG
             EXEC CICS WRITEQ TD QUEUE('IESN')
                       FROM(ENDG) LENGTH(1)
                       NOHANDLE END-EXEC
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

      * along with value, pass back the length of actual http response
           MOVE HTTP-RESPONSE-LEN OF HTTP-REQ TO VLEN.
           SET  ADDRESS OF VDATA       TO HTTP-RESPONSE-ADR
                                                      OF DFHCOMMAREA.
           MOVE VDATA(1:VLEN)          TO VAR-DATA.
           PERFORM U00-SET-LENGTH.
           MOVE LEN                    TO HTTP-RESPONSE-LEN
                                                      OF DFHCOMMAREA.

      * if unsuccessful http response, abandon further processing ...
           IF  VDATA(1:3) NOT = '200'
           OR  HTTP-NO-TEMPLATE
           OR  HTTP-RTNCODE > ZERO
      * ... but first pass back whatever was received for response body
               SET  ADDRESS OF CALLER-BUFFER
                                       TO HTTP-RECV-DATA-ADR
               MOVE RECV-BUFFER(1:RECV-BUFLEN)
                       TO CALLER-BUFFER(1:HTTP-RECV-DATA-LEN)
               MOVE RECV-BUFLEN        TO HTTP-RECV-DATA-LEN
               GO TO Z90-HTTP-TERMINATION
           END-IF.

       C80-PROCESS-OUTPUT.
      *
      * Make sure we are positioned to the response template id
      *
           IF  NOT TPID-RESP
               PERFORM T13-READNEXT-HTTPLID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                 STRING 'READNEXT ON TEMPLATE ID = '
                        TPID-KEY          DELIMITED BY SIZE
                                     INTO LOGF-MESG
                 PERFORM Q200-UNEX-LOG
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
               IF  TPID-NAME NOT = HTTP-TEMPLATE-ID
               OR  NOT TPID-RESP
                 MOVE +15              TO HTTP-RTNCODE
                 GO TO Z90-HTTP-TERMINATION
               END-IF
           END-IF.
           PERFORM T15-ENDBR-HTTPLID.
      *
      * Parse the web service response into our own internal buffer...
      *
           EXEC CICS GETMAIN
                     SET     (ADDRESS OF DATA-BUFFER)
                     FLENGTH (LENGTH OF DATA-BUFFER)
                     INITIMG (LOWVALUE)
           END-EXEC.
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

           GO TO Z90-HTTP-TERMINATION.

      /*****************************************************************
      *    LOAD XML REQUEST TEMPLATE                                   *
      ******************************************************************
       D00-LOAD-XML-REQU-TEMPLATE.
           PERFORM T00-INIT-REQU-TEMPLATE THRU T05-EXIT.
           IF  REQU-TYPE = 'XML'
               GO TO D90-EXIT
           END-IF.
           MOVE 'XML'                  TO REQU-TYPE.
           MOVE ZEROES                 TO ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.

           PERFORM T20-INITKEY-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.
           PERFORM T21-STARTBR-HTTPLTE.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL HTTP-RTNCODE > ZERO
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR REQU-COUNT > TPL-MAX
             PERFORM T23-READNEXT-HTTPLTE
             IF  HTTP-RTNCODE = ZERO
             AND EIBRESP = DFHRESP(NORMAL)
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
           PERFORM T25-ENDBR-HTTPLTE.
           IF  REQU-COUNT > TPL-MAX
               STRING 'REQU TEMPLATE OVERFLOW: '
                      REQU-NAME ' ' REQU-TYPE
                 DELIMITED BY SIZE   INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               MOVE +15                TO HTTP-RTNCODE
           END-IF.
       D90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD JSON REQUEST TEMPLATE                                  *
      ******************************************************************
       E00-LOAD-JSON-REQU-TEMPLATE.
           PERFORM T00-INIT-REQU-TEMPLATE THRU T05-EXIT.
           IF  REQU-TYPE = 'JSON'
               GO TO E90-EXIT
           END-IF.
           MOVE 'JSON'                 TO REQU-TYPE.
           MOVE ZEROES                 TO ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.

           PERFORM T20-INITKEY-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.
           PERFORM T21-STARTBR-HTTPLTE.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL HTTP-RTNCODE > ZERO
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR REQU-COUNT > TPL-MAX
             PERFORM T23-READNEXT-HTTPLTE
             IF  HTTP-RTNCODE = ZERO
             AND EIBRESP = DFHRESP(NORMAL)
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
           PERFORM T25-ENDBR-HTTPLTE.
           IF  REQU-COUNT > TPL-MAX
               STRING 'REQU TEMPLATE OVERFLOW: '
                      REQU-NAME ' ' REQU-TYPE
                 DELIMITED BY SIZE   INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               MOVE +15                TO HTTP-RTNCODE
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
      * let's see the raw request to the server
             MOVE 21                   TO TXT-MSTR-BUFLEN
             MOVE 'GENERATED DATA LINES:'
                                       TO TXT-MSTR-BUFFER
             EXEC CICS WRITEQ TD QUEUE('IESN')
                       FROM(TXT-MSTR-BUFFER)
                       LENGTH(TXT-MSTR-BUFLEN)
                       NOHANDLE END-EXEC
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
      * let's see the raw request to the server
                       EXEC CICS WRITEQ TD QUEUE('IESN')
                                 FROM(TXT-MSTR-BUFFER)
                                 LENGTH(TXT-MSTR-BUFLEN)
                                 NOHANDLE END-EXEC
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
      * let's see the raw request to the server
             MOVE SPACE                TO ENDG
             EXEC CICS WRITEQ TD QUEUE('IESN')
                       FROM(ENDG) LENGTH(1)
                       NOHANDLE END-EXEC
           END-IF.

           SUBTRACT 1                FROM OUT
                                   GIVING SEND-BUFLEN.
       J90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD JSON RESPONSE TEMPLATE                                 *
      ******************************************************************
       L00-LOAD-JSON-RESP-TEMPLATE.
           PERFORM T00-INIT-RESP-TEMPLATE THRU T06-EXIT.
           IF  RESP-TYPE = 'JSON'
               GO TO L90-EXIT
           END-IF.
           MOVE 'JSON'                 TO RESP-TYPE.
           MOVE ZEROES                 TO ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.

           PERFORM T20-INITKEY-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.
           PERFORM T21-STARTBR-HTTPLTE.
      *
      * As template is being loaded, parse it
      * to set array keys by section number
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL HTTP-RTNCODE > ZERO
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR RESP-COUNT > TPL-MAX
             PERFORM T23-READNEXT-HTTPLTE
             IF  HTTP-RTNCODE = ZERO
             AND EIBRESP = DFHRESP(NORMAL)
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
           PERFORM T25-ENDBR-HTTPLTE.
           IF  RESP-COUNT > TPL-MAX
               STRING 'RESP TEMPLATE OVERFLOW: '
                      RESP-NAME ' ' RESP-TYPE
                 DELIMITED BY SIZE   INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               MOVE +15                TO HTTP-RTNCODE
           END-IF.
       L90-EXIT.
           EXIT.

      /*****************************************************************
      *    LOAD XML RESPONSE TEMPLATE                                  *
      ******************************************************************
       M00-LOAD-XML-RESP-TEMPLATE.
           PERFORM T00-INIT-RESP-TEMPLATE THRU T06-EXIT.
           IF  RESP-TYPE = 'XML'
               GO TO M90-EXIT
           END-IF.
           MOVE 'XML'                  TO RESP-TYPE.
           MOVE ZEROES                 TO ARYLVL
                                          SECNO.
           MOVE SPACE                  TO BEGG.

           PERFORM T20-INITKEY-HTTPLTE.
           MOVE TPID-KEY               TO TPLT-ID.
           PERFORM T21-STARTBR-HTTPLTE.
      *
      * As template is being loaded, parse it
      * to set array keys by section level
      * and by variable number, if present.
      *
           PERFORM WITH TEST BEFORE
             UNTIL HTTP-RTNCODE > ZERO
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR RESP-COUNT > TPL-MAX
             PERFORM T23-READNEXT-HTTPLTE
             IF  HTTP-RTNCODE = ZERO
             AND EIBRESP = DFHRESP(NORMAL)
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
           PERFORM T25-ENDBR-HTTPLTE.
           IF  RESP-COUNT > TPL-MAX
               STRING 'RESP TEMPLATE OVERFLOW: '
                      RESP-NAME ' ' RESP-TYPE
                 DELIMITED BY SIZE   INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               MOVE +15                TO HTTP-RTNCODE
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
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    OTHER PERFORMED ROUTINES                                    *
      ******************************************************************

       T00-INIT-REQU-TEMPLATE.
           IF  ADDRESS OF REQU-TEMPLATE = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF REQU-TEMPLATE)
                         FLENGTH  (LENGTH OF REQU-TEMPLATE)
               END-EXEC
           END-IF.
           IF  REQU-NAME NOT = TPID-NAME
               MOVE LOW-VALUES         TO REQU-TEMPLATE
               MOVE TPID-NAME          TO REQU-NAME
               MOVE SPACES             TO REQU-TYPE
               MOVE ZEROES             TO REQU-ITEM
                                          REQU-COUNT
           END-IF.
       T05-EXIT.
           EXIT.

       T00-INIT-RESP-TEMPLATE.
           IF  ADDRESS OF RESP-TEMPLATE = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF RESP-TEMPLATE)
                         FLENGTH  (LENGTH OF RESP-TEMPLATE)
               END-EXEC
           END-IF.
           IF  RESP-NAME NOT = TPID-NAME
               MOVE LOW-VALUES         TO RESP-TEMPLATE
               MOVE TPID-NAME          TO RESP-NAME
               MOVE SPACES             TO RESP-TYPE
               MOVE ZEROES             TO RESP-ITEM
                                          RESP-COUNT
           END-IF.
       T06-EXIT.
           EXIT.

       T10-INITKEY-HTTPLID.
           IF  ADDRESS OF HTTPLID-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HTTPLID-RECORD)
                         LENGTH   (LENGTH OF HTTPLID-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO TPID-KEY.
       T11-STARTBR-HTTPLID.
           EXEC CICS STARTBR
                     DATASET  (HTTPLID)
                     RIDFLD   (TPID-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       T13-READNEXT-HTTPLID.
           MOVE LENGTH OF HTTPLID-RECORD TO TPID-RECL.
           EXEC CICS READNEXT
                     DATASET  (HTTPLID)
                     INTO     (HTTPLID-RECORD)
                     LENGTH   (TPID-RECL)
                     RIDFLD   (TPID-KEY)
                     NOHANDLE
           END-EXEC.
       T15-ENDBR-HTTPLID.
           EXEC CICS ENDBR
                     DATASET  (HTTPLID)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       T19-EXIT.
           EXIT.

       T20-INITKEY-HTTPLTE.
           IF  ADDRESS OF HTTPLTE-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF HTTPLTE-RECORD)
                         LENGTH   (LENGTH OF HTTPLTE-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO TPLT-ID
           MOVE ZEROES                 TO TPLT-SEQ.
       T21-STARTBR-HTTPLTE.
           EXEC CICS STARTBR
                     DATASET  (HTTPLTE)
                     RIDFLD   (TPLT-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               STRING 'STARTBWS ON TEMPLATE DATA = '
                      TPLT-KEY            DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               MOVE +15                TO HTTP-RTNCODE
           END-IF.
       T23-READNEXT-HTTPLTE.
           MOVE LENGTH OF HTTPLTE-RECORD TO TPLT-RECL.
           EXEC CICS READNEXT
                     DATASET  (HTTPLTE)
                     INTO     (HTTPLTE-RECORD)
                     LENGTH   (TPLT-RECL)
                     RIDFLD   (TPLT-KEY)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  TPLT-ID NOT = TPID-KEY
               IF  EIBRESP = DFHRESP(ENDFILE)
               OR  TPLT-ID NOT = TPID-KEY
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               ELSE
                 STRING 'READNEXT ON TEMPLATE DATA = '
                        TPLT-KEY          DELIMITED BY SIZE
                                     INTO LOGF-MESG
                 PERFORM Q200-UNEX-LOG
                 MOVE +15              TO HTTP-RTNCODE
               END-IF
           ELSE
               SUBTRACT LENGTH OF TPLT-KEY
                                     FROM TPLT-RECL
                                   GIVING TPLT-DATL
           END-IF.
       T25-ENDBR-HTTPLTE.
           EXEC CICS ENDBR
                     DATASET  (HTTPLTE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       T29-EXIT.
           EXIT.

      /*****************************************************************
      *    OTHER UTILITY ROUTINES                                      *
      ******************************************************************

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
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           SET  TXT-REQUEST-DELETE     TO TRUE.
           MOVE SPACES                 TO TXT-PARM-STRING.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           SUBTRACT TXT-PARM-STRLEN  FROM TXT-MSTR-BUFLEN.
           SET  TXT-REQUEST-INSERT     TO TRUE.
           MOVE LEN                    TO TXT-PARM-STRLEN.
           MOVE TEMP-DATA(1:LEN)       TO TXT-PARM-STRING.
           ADD  TXT-PARM-STRLEN        TO TXT-MSTR-BUFLEN.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
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
             EXEC CICS WRITEQ TD QUEUE('IESN')
                       FROM(REQU-DATA(REQU-ITEM))
                       LENGTH(LEN)
                       NOHANDLE END-EXEC
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

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *
      ******************************************************************

       Z90-HTTP-TERMINATION.

           EXEC CICS RETURN END-EXEC.

