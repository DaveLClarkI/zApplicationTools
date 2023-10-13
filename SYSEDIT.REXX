/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* REXX: SYSEDIT    Proc Created 03/02/2004   By Dave Clark          */
/*                  Last Revised 10/12/2023                          */
/*                                                                   */
/*      This procedure  provides  for  quick-n-dirty creation and/or */
/* editing of VSE sublibrary  members  from  the  BG partition -- in */
/* either line mode (on the  VSE console,  only) or full-screen mode */
/* (using a native 3270 terminal session).   The target situation is */
/* primarily one in which all that  is active is the BG partition in */
/* which to work.                                                    */
/*                                                                   */
/*      However,  this could certainly be  used  in small shops as a */
/* programmer's only editor --  as  long  as  each programmer can be */
/* allocated their own long-running partition for this job and their */
/* own sublibrary  from  which  to  work.   This  procedure could be */
/* executed from a PROC,  since it  requires a minimal amount of JCL */
/* to execute in full-screen mode (but only the EXEC to execute from */
/* the VSE console in line mode).  In online mode it will prompt for */
/* the device address from the console.                              */
/*                                                                   */
/* // SETPFIX LIMIT=8K         /* for online use, only */            */
/* // EXEC REXX=SYSEDIT,SIZE=256K<,PARM='{ONLINE|BATCH}'>            */
/*                                        ¯¯¯¯¯¯                     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* No warranty is expressed or implied.      Written by Dave L Clark */
/* Neither the author nor the company is     DAPSCO Information Sys. */
/* responsible for any loss or damage        3110 Kettering Blvd.    */
/* resulting from the use of this code.      Dayton, OH  45439       */
/* Source is provided on an as-is basis.     Phone (937)294-5331     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*      Note that the following block of  source lines are used as a */
/* HELP display on the console  when in batch mode.   Online has its */
/* own help screens.  Some edit commands are different for batch vs. */
/* online and other commands are batch-only vs. online-only.         */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Edit commands are:  (only capital letters of Cmd required)    */
/*                                                               */
/* Help          Results in this display.                        */
/* Save          Saves any changes and ends member edit.         */
/* Quit          Ends member edit without saving any changes.    */
/* Top           Positions to top line of member for editing.    */
/* Bottom        Positions to bottom line of member for editing. */
/* Up (nn)       Moves up 1 or nn number of lines for editing.   */
/* Next (nn)     Moves down 1 or nn number of lines for editing. */
/* Overlay (str) Erases the entire content of the current line   */
/*                 and, if specified, fills with entered string. */
/* Find str      Searches forward in the current member until    */
/* Locate str      specified string is found, then displays that */
/*                 line with the located string repeated below.  */
/* Insert str    Inserts the specified string before the current */
/*                 location pointer position.  If not shown, it  */
/*                 is at the beginning of the current line.      */
/* Replace str   Changes the previously located string to the    */
/*                 specified string, then redisplays the changed */
/*                 line and an asterisk location pointer below.  */
/* Delete        Erases the previously located string, then re-  */
/*                 displays the changed line & location pointer. */
/* LAdd (str)    Creates a new member line after the current one */
/*                 and, if specified, fills with entered string. */
/* LDrop         Eliminates the current line from the member.    */
/* COpy (n)      Add current line (plus n-1 more) to the FIFO    */
/*                 stack.  An '*' goes to end-of-member.         */
/* Paste (n)     Get one or more new lines, off of stack ('*' to */
/*                 end-of-stack), inserting after current line.  */
/* CLear         Empty the data stack.  Note that if the stack   */
/*                 is not empty when the editor ends, then Job   */
/*                 Control will process (as JCL) what remains    */
/*                 up to the first "/&" card encountered.        */
/* SUBmit        Resets member edit back to the first line and,  */
/*                 if VSE/POWER is active, places the current    */
/*                 member content into the RDR queue.            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*                                                                   */
/*         MODULE, FUNCTION, and SUBROUTINE Cross-Reference          */
/*         ------------------------------------------------          */
/*                                                                   */
/* MAINLINE MODULES                                                  */
/* ----------------                                                  */
/* Map0_Processing           (Index: 988)                            */
/*             prompts for a library specification.                  */
/*                                                                   */
/* Map1_Processing           (Index: 1028)                           */
/*             prompts for a sublibrary selection.                   */
/*             Optionally, this process also supports sublibrary     */
/*             create and delete.                                    */
/*                                                                   */
/* Map2_Processing           (Index: 1154)                           */
/*             prompts for a member selection.                       */
/*             Optionally, this process also supports member         */
/*             create, delete, copy, move, and rename.               */
/*                                                                   */
/* Map3_Processing           (Index: 1291)                           */
/*             prompts for member editing.                           */
/*             Optionally, this process also supports member         */
/*             submit to POWER.                                      */
/*                                                                   */
/* GENERAL ROUTINES                                                  */
/* ----------------                                                  */
/* Dump                      (Index: 1393)                           */
/*             invokes RXDMPVAR to dump all REXX variables for       */
/*             sorted (SHAKESRT) output to SYSLST.                   */
/*                                                                   */
/* IFF                       (Index: 1410)                           */
/*             is an in-line IF function.                            */
/*             Arguments:  (all are optional)                        */
/*                 Boolean condition                                 */
/*                 value to return if boolean condition is true      */
/*                 value to return if boolean condition is false     */
/*                                                                   */
/* MakeStem                  (Index: 1421)                           */
/*             is a subroutine to automatically create stem variable */
/*             Arguments:  (at least the first one is required)      */
/*                 stem name to create (must include the period)     */
/*                 one or more comma-separated values to assign to   */
/*                   sequentially numbered stem variables.           */
/*                                                                   */
/* ParseCommandString        (Index: 1434)                           */
/*             is a subroutine to handle quoted command operands.    */
/*             Arguments:  (required)                                */
/*                 string  (format: command oper1 oper2 oper3 ...)   */
/*                                                                   */
/* INPUT-OUTPUT ROUTINES                                             */
/* ---------------------                                             */
/* Screen_IO                 (Index: 1463)                           */
/*             erases the screen, writes the specified map to the    */
/*             screen, and performs a read-modified operation.       */
/*             Arguments:  (required)                                */
/*                 stem name of the current map.                     */
/*                                                                   */
/* Converse                  (Index: 1477)                           */
/*             writes supplied argument string to row 22 column 1    */
/*             and optionally accepts unparsed input data.           */
/*             Arguments:  (first one is required)                   */
/*                 output string to write                            */
/*                 length of optional input                          */
/*             Note that, except for AID byte and cursor position,   */
/*                 the optional input data is not parsed and must be */
/*                 retrieved from the RAW stem variable for the map. */
/*                                                                   */
/* Help                      (Index: 1504)                           */
/*             presents as many pages of the specified help map as   */
/*             requested by the user.                                */
/*             Arguments:  (required)                                */
/*                 group number for the help map(s) to display.      */
/*                                                                   */
/* VSE LIBRARY INTERFACE ROUTINES                                    */
/* ------------------------------                                    */
/* ListDir_Successful        (Index: 1577)                           */
/*             function returns a Boolean value as to the success or */
/*             failure of listing the contents of the specified      */
/*             library or sublibrary name.                           */
/*             Arguments:  (required)                                */
/*                 library or sublibrary name to list.               */
/*                 Note that this argument must be prefixed by       */
/*                 "L=" or "S=" to indicate a library or sublibrary. */
/*                                                                   */
/* ListMbr_Successful        (Index: 1600)                           */
/*             function returns a Boolean value as to the success or */
/*             failure of listing the contents of the specified      */
/*             sublibrary name.                                      */
/*             Arguments:  (required)                                */
/*                 member name selection,                            */
/*                 sublibrary name to access.                        */
/*                                                                   */
/* NewMember                 (Index: 1633)                           */
/*             prompts for the creation of a new member and returns  */
/*             a Boolean value on its creation success or failure.   */
/*             Arguments:  (required)                                */
/*                 member name to create,                            */
/*                 sublibrary name to access.                        */
/*                                                                   */
/* NewSublibrary             (Index: 1656)                           */
/*             prompts for the creation of a new sublibrary and      */
/*             returns a Boolean value on its success or failure.    */
/*             Arguments:  (required)                                */
/*                 sublibrary name to create,                        */
/*                                                                   */
/* OperationFailed           (Index: 1668)                           */
/*             invokes LIBR to perform the requested operation and   */
/*             returns a Boolean value on its success or failure.    */
/*             Arguments:  (first two are required)                  */
/*                 operation name,                                   */
/*                 object on which to perform the operation,         */
/*                 object in which the operation is performed.       */
/*                                                                   */
/* EDIT COMMAND ROUTINES                                             */
/* ---------------------                                             */
/* SetTopOfMember            (Index: 1709)                           */
/*             sets the internal record pointer to the first record  */
/*             in the internal virtual member.                       */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* SetRecordPointer          (Index: 1714)                           */
/*             sets the internal record pointer to the argument      */
/*             record number in the internal virtual member.         */
/*             Arguments:  (required)                                */
/*                 line number to position at top of screen.         */
/*                                                                   */
/* ResetLocationPointer      (Index: 1722)                           */
/*             sets the internal location pointer to the beginning   */
/*             of the current line in the virtual member.            */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* ResetSourceString         (Index: 1727)                           */
/*             clears the internal source string buffer.             */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* ReadMember                (Index: 1731)                           */
/*             loads the content of the current VSE Library member   */
/*             into an internal virtual member.                      */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* WriteMember               (Index: 1745)                           */
/*             saves the content of the internal virtual member into */
/*             the current VSE Library member.                       */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* LineAdd                   (Index: 1758)                           */
/*             inserts a new line into the internal virtual member   */
/*             and optionally initializes it with a supplied string. */
/*             Arguments:  (optional)                                */
/*                 string to initialize new line of virtual member.  */
/*                                                                   */
/* LineDrop                  (Index: 1777)                           */
/*             removes a line from the internal virtual member.      */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* ProcessLineCommands       (Index: 1796)                           */
/*             processes line commands captured in SaveBodyData.     */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* SubmitMember              (Index: 2160)                           */
/*             submits the content of the internal virtual member    */
/*             to VSE/POWER for subsequent processing.               */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* HELPER ROUTINES FOR SCROLLABLE MAPS                               */
/* -----------------------------------                               */
/* CursorPosition            (Index: 2181)                           */
/*             function accepts a zero-offset buffer position for    */
/*             the cursor and returns the row and column numbers.    */
/*             Arguments:  (first two are required)                  */
/*                 buffer-offset-type cursor position                */
/*                 maximum columns per screen size,                  */
/*                 optional number of map header rows to subtract    */
/*                                                                   */
/* Map1HasSelections         (Index: 2188)                           */
/*             finds and processes any detail line selections for    */
/*             Map1.  Returns a Boolean response if any found.       */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* Map2HasSelections         (Index: 2235)                           */
/*             finds and processes any detail line selections for    */
/*             Map2.  Returns a Boolean response if any found.       */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* RebuildMap1Detail         (Index: 2329)                           */
/*             sorts the detail lines for Map1, removing any lines   */
/*             flagged as deleted, and optionally searches the list  */
/*             for the specified sublibrary argument named.          */
/*             Arguments:  (optional)                                */
/*                 sublibrary name to position to top of detail area */
/*                                                                   */
/* RebuildMap2Detail         (Index: 2347)                           */
/*             sorts the detail lines for Map2, removing any lines   */
/*             flagged as deleted, and optionally searches the list  */
/*             for the specified member argument named.              */
/*             Arguments:  (optional)                                */
/*                 member name to position to top of detail area     */
/*                                                                   */
/* SaveBodyData              (Index: 2366)                           */
/*             transfers screen data from the body of Map3 into the  */
/*             internal buffer for the member currently under edit.  */
/*             Arguments:  (none)                                    */
/*                                                                   */
/* HELPER ROUTINES FOR BUILDING MAPS                                 */
/* ---------------------------------                                 */
/* BuildAttribute            (Index: 2397)                           */
/*             function returns 4-byte field attribute value --      */
/*             for use when calling the BuildField subroutine.       */
/*             Arguments: (all optional -- defaults provided)        */
/*                 field attribute selection,                        */
/*                 cursor option selection,                          */
/*                 extended color selection,                         */
/*                 extended hilight selection.                       */
/*                                                                   */
/* BuildField                (Index: 2409)                           */
/*             subroutine creates stem variables for a map field --  */
/*             called by the BuildHeader & BuildTrailer subroutines. */
/*             Arguments: (all required -- except for the last one)  */
/*                 stem name of the current map,                     */
/*                 maximum rows per screen size,                     */
/*                 maximum columns per screen size,                  */
/*                 row position for field attribute,                 */
/*                 column position for field attribute,              */
/*                 4-byte field attribute value,                     */
/*                 optional output (display) value for field.        */
/*                                                                   */
/* BuildHeader               (Index: 2422)                           */
/*             subroutine creates all header fields for a map --     */
/*             automatically calls the BuildTrailer subroutine.      */
/*             Arguments: (only the stem name is required)           */
/*                 stem name of the current map,                     */
/*                 optional title for 2nd-row-center of map,         */
/*                 optional value for first PFkey display line,      */
/*                 optional value for second PFkey display line.     */
/*                                                                   */
/* BuildPFkeys               (Index: 2485)                           */
/*             function returns a formatted PF-key line for a map -- */
/*             for use when calling the BuildHeader subroutine.      */
/*             Arguments: (all optional)                             */
/*                 accepts one or more arguments for concatenation   */
/*                 into a single string value with imbedded color    */
/*                 attributes around the PFkey number for emphasis.  */
/*                                                                   */
/* BuildTrailer              (Index: 2512)                           */
/*             subroutine creates all trailer fields for a map --    */
/*             called by the BuildHeader subroutine.                 */
/*             Arguments: (only the stem name is required)           */
/*                 stem name of the current map,                     */
/*                 optional value for first PFkey display line,      */
/*                 optional value for second PFkey display line.     */
/*                                                                   */
/* MAP BUILDING ROUTINES  (the following have no arguments)          */
/* ---------------------                                             */
/* BuildMap0                 (Index: 2533)                           */
/*             builds the field definitions  for  Map0 -- which is a */
/*             single,  full-screen map that  is displayed only once */
/*             for the sole purpose of obtaining the name of the VSE */
/*             library to use throughout the edit session.           */
/*                                                                   */
/* BuildMap1Detail           (Index: 2739)                           */
/* BuildMap1Body             (Index: 2690)                           */
/* BuildMap1                 (Index: 2607)                           */
/*             builds the field definitions  for  Map1 -- which is a */
/*             scrollable map  that  displays  sublibraries,  in the */
/*             current   library,    for   selection   and   further */
/*             processing.                                           */
/*                                                                   */
/* BuildMap2Detail           (Index: 2905)                           */
/* BuildMap2Body             (Index: 2856)                           */
/* BuildMap2                 (Index: 2773)                           */
/*             builds the field definitions  for  Map2 -- which is a */
/*             scrollable map that displays members,  in the current */
/*             sublibrary, for selection and further processing.     */
/*                                                                   */
/* BuildMap3Body             (Index: 3072)                           */
/* BuildMap3                 (Index: 2943)                           */
/*             builds the field definitions  for  Map3 -- which is a */
/*             scrollable map  that  displays  the  content,  of the */
/*             current member, for editing purposes.                 */
/*                                                                   */
/* BuildHelp1a               (Index: 3130)                           */
/* BuildHelp1b               (Index: 3248)                           */
/* BuildHelp2a               (Index: 3359)                           */
/* BuildHelp2b               (Index: 3498)                           */
/* BuildHelp3a               (Index: 3609)                           */
/* BuildHelp3b               (Index: 3826)                           */
/* BuildHelp3c               (Index: 3969)                           */
/* BuildHelp3d               (Index: 4088)                           */
/*             build the  field  definitions  for  the  help screens */
/*             associated with a given Map.   These help screens are */
/*             single, full-screen maps (without input fields) which */
/*             provide function  keys  for  navigation  from  one to */
/*             another.                                              */
/*                                                                   */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

Parse Source sys env pgm tokens;

False  = (1=0);
True   = (0=0);
luno   = '000';                        /* logical unit number */
cuu    = '';                           /* control unit address */
rc     = 0;

Arg msg;

z = ASSGN('STDIN','SYSLOG');
z = ASSGN('STDOUT','SYSLOG');

If msg = 'BATCH' Then
  online = False;
Else Do
  online = True;
  Do While cuu = ''
    Say 'Enter "cuu" for terminal session, else EOB to exit:';
    Parse Upper External cuu .;
    If cuu = '' Then Exit 0;
    If Length(cuu) <> 3 | \Datatype(cuu,'W') Then
      cuu = '';
    Else Do
      Address JCL "// ASSGN SYS"luno","cuu;   /* assign device */
      If rc = 0 Then Do
        Say pgm": Attached to "cuu" for 3270 I/O via SYS"luno;
        z = ASSGN('STDOUT','SYSLST');
        Say pgm": Attached to "cuu" for 3270 I/O via SYS"luno;
      End
      Else Do
        Say pgm": JCL ASSGN error: rc = "rc;
        cuu = '';
      End
    End
  End
  z = ASSGN('STDIN','SYSIPT');
  z = ASSGN('STDOUT','SYSLST');
End

mbuild. = online;                      /* reflect online status */
exclude = '|DTSAFE|DUMP|LIST|OBJ|PHASE|SYSDEBUG|';

/* ----------------------------------------------------------------- */
/* Get library name                                                  */
/* ----------------------------------------------------------------- */

If online Then Do
  dlbl = '';
  Call Map0_Processing;
End

/* ----------------------------------------------------------------- */
/* Get library.sublibrary name                                       */
/* ----------------------------------------------------------------- */

Do While rc = 0

  If online Then Do
    lib = '';
    map1_save_ptr = '';
    Call Map1_Processing;
    If map1._aid = RX3270_AID.LIT_PF3_KEY,
    |  map1._aid = RX3270_AID.LIT_PF15_KEY,
    Then Do
      Leave;
    End
  End
  Else Do
    Say 'Enter "lib.sublib" name, else EOB to exit:';
    Parse Upper External lib .;
    If lib = '' Then Leave;
    If \ListDir_Successful('S='lib) Then Do
      Say ' ';
      Iterate;
    End
  End

/* ----------------------------------------------------------------- */
/* Get member.type name                                              */
/* ----------------------------------------------------------------- */

Do While rc = 0

  new = False;
  opt = 'NODATA';
  map3_view = 1;

  If online Then Do
    mbr = '';
    map2_save_ptr = '';
    Call Map2_Processing;
    If map2._aid = RX3270_AID.LIT_PF3_KEY,
    |  map2._aid = RX3270_AID.LIT_PF15_KEY,
    Then Do
      Leave;
    End
  End
  Else Do
    Say 'Enter "member.type" name, else EOB to return:';
    Parse Upper External mbr .;
    If mbr = '' Then Leave;
    If \ListMbr_Successful(mbr,lib) Then Do
      If result = 'INVTYP' Then Say ' Cannot edit member type of "'t'"';
      If result = 'NOTFND' Then Do
        new = True;
        Say 'Member "'lib'.'mbr'" not found.  Create it?';
        Parse Upper External resp .;
        If Left(resp,1) <> 'Y' Then
          new = False;
        Else Do
          Parse Upper Var mbr mem '.' typ .;
          If typ = 'PROC' Then Do
            Say 'Should the "DATA" flag be ON for this member?';
            Parse Upper External resp .;
            If Left(resp,1) = 'Y' Then opt = 'DATA';
          End
        End
      End
      If \new Then Do
        Say ' ';
        Iterate;
      End
    End
  End

/* ----------------------------------------------------------------- */
/* Initialize variables and extract member data for editing          */
/* ----------------------------------------------------------------- */

  lca.  = '';
  lca.0 = 0;
  lca_pending = False;
  lca_cmds = 'ABCDILMU"<>';
  lca_blks = '|DD|CC|MM|LL|UU|<<|>>|';

  rec.  = '';
  rec.0 = 0;
  ruler = '....+....1....+....2....+....3....+....4....';
  str   = '';
  mixed = True;
  upd_flag = False;
  lower_case = 'abcdefghijklmnopqrstuvwxyz';
  upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  Call ReadMember;
  If rc <> 0 Then Leave;

  Call SetTopOfMember;

/* ----------------------------------------------------------------- */
/* Get edit command                                                  */
/* ----------------------------------------------------------------- */

Do While rc = 0

  If online Then Do
    cmd_string = '';
    map3_save_ptr = '';
    Call Map3_Processing;
  End
  Else Do
    Say pgm': Enter edit command (else EOB to see NEXT line):';
    Say '("str" may contain quotes and embedded & trailing spaces)';
    Say 'LOCATE str, REPLACE str, SAVE, or HELP for more commands.';
    If rec.0 = 0 Then Do
      Say ' ';
      Say ' Cannot display empty member.';
    End
    Else Do
      Say ruler 'Line' r 'of' rec.0;
      If rec.r > ' ' Then
        Say Strip(rec.r,'Trailing');
      Else Do
        Say '...(empty line)...';
      End
      If p > 0 Then Do
        If src = '' Then Say Right('*',p);
                    Else Say Right(' ',p-1)||src;
      End
    End
    Parse External cmd_string;
  End

  Call ParseCommandString cmd_string;

/* ----------------------------------------------------------------- */
/* Process edit commands                                             */
/* ----------------------------------------------------------------- */

  Select

    When Left(cmd,3) == 'SUB'  Then      /* SUBMIT */
      Do
        Call SetTopOfMember;
        Call SubmitMember;
        If rc = 0 Then Do
          If online Then
            map3.map3_Bruler.output = ' Member submitted.';
          Else Do
            Say ' Member submitted.';
            Say ' ';
          End
        End
        rc    = 0;
      End

    When Left(cmd,1) == 'Q',             /* QUIT */
       | Left(cmd,3) == 'CAN'  Then      /* CANCEL */
      Do
        If \upd_flag | Left(cmd,3) == 'CAN' Then Leave;
        If online Then
          map3.map3_Bruler.output = ' Member changed.  Use SAVE or CANCEL.';
        Else Do
          Say pgm': Member changed, are you sure you wish to quit?';
          Parse Upper External resp .;
          If Left(resp,1) = 'Y' Then Leave;
          Say ' Edit continues...';
          Say ' ';
        End
        Call SetTopOfMember;
      End

    When Left(cmd,1) == 'S'    Then      /* SAVE */
      Do
        If upd_flag Then Call WriteMember;
        map2.map2_member.input = '';
        Leave;
      End

    When Left(cmd,1) == 'H'    Then      /* HELP */
      Do
        If online Then
          Call Help '3';
        Else Do
          Do n  = 1 Until Left(Sourceline(n),2) = '  '
          End               /* skip prolog */
          n     = n + 1;
          Do n  = n Until Left(Sourceline(n),2) = '  '
          End               /* skip disclaimer */
          n     = n + 1;
          Do n  = n While Left(Sourceline(n),2) = '/*'
            Say Strip(Sourceline(n));
          End
          Call ResetLocationPointer;
        End
      End

    When Left(cmd,2) == 'RE'   Then      /* RECALL */
      Do
        map3.map3_command.output = cmd_input;
      End

    When online,
       & Left(cmd,4) == 'CASE' Then      /* CASE   */
      Do
        If Translate(Left(oper.1,1)) = 'M' Then mixed = True;
        If Translate(Left(oper.1,1)) = 'U' Then mixed = False;
      End

    When online,
       & Left(cmd,2) == 'LE'   Then      /* LEFT   */
      Do
        map3_view = 1;
        map3.map3_Bruler.output = SubStr(source_ruler,map3_view,72);
      End

    When online,
       & Left(cmd,2) == 'RI'   Then      /* RIGHT  */
      Do
        map3_view = 9;
        map3.map3_Bruler.output = SubStr(source_ruler,map3_view,72);
      End

    When Left(cmd,1) == 'T'    Then      /* TOP  */
      Do
        If r > 1 Then
          r = 1;
        Else Do
          If online Then
            map3.map3_Bruler.output = ' Top of member.';
          Else Do
            Say ' End of member.';
            Say ' ';
          End
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When Left(cmd,2) == 'BO'   Then      /* BOTTOM */
      Do
        If online Then
          r = rec.0 - (map3_cnt - 2);
        Else Do
          r = rec.0;
        End
        If r < 1 Then Do
          If online Then
            map3.map3_Bruler.output = ' End of member.';
          Else Do
            Say ' End of member.';
            Say ' ';
          End
          r = 1;
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When online,
       & Left(cmd,2) == 'BA'   Then      /* BACKWARD */
      Do
        row = map3._csr % map3._cols + 1;
        col = map3._csr // map3._cols + 1;
        If row <= map3_hdc | row > (map3_hdc + map3_cnt) Then
          r = r - map3_cnt;
        Else Do
          row = row - map3_hdc;
          r = r - (map3_cnt - row);
        End
        If r < 1 Then Do
          map3.map3_Bruler.output = ' Top of member.';
          r  = 0;
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When online,
       & Left(cmd,2) == 'FO'   Then      /* FORWARD  */
      Do
        row = map3._csr % map3._cols + 1;
        col = map3._csr // map3._cols + 1;
        If row <= map3_hdc | row > (map3_hdc + map3_cnt) Then
          r = r + map3_cnt;
        Else Do
          row = row - map3_hdc;
          r = r + (row - 1);
        End
        If r > rec.0 Then Do
          map3.map3_Bruler.output = ' End of member.';
          r  = rec.0;
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When Left(cmd,1) == 'U'    Then      /* UP   */
      Do
        If Datatype(oper.1,'W') Then r = r - oper.1;
                               Else r = r - 1;
        If r  < 1 Then Do
          If online Then
            map3.map3_Bruler.output = ' Top of member.';
          Else Do
            Say ' Top of member.';
            Say ' ';
          End
          r   = 1;
          If r > rec.0 Then r = 0;
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When Left(cmd,1) == 'N',             /* NEXT */
       | Left(cmd,2) == 'DO',            /* DOWN */
       | (\online & cmd = '')  Then
      Do
        If Datatype(oper.1,'W') Then r = r + oper.1;
        Else r = r + 1;
        If r  > rec.0 Then Do
          If online Then
            map3.map3_Bruler.output = ' End of member.';
          Else Do
            Say ' End of member.';
            Say ' ';
          End
          r   = rec.0;
        End
        Call SetRecordPointer r;
        Call ResetLocationPointer;
      End

    When Left(cmd,2) == 'LA'   Then      /* LADD  */
      Do
        If oper.0 > 1 Then Do
          If online Then
            map3.map3_Bruler.output = ' Too many operands.';
          Else Do
            Say ' Too many operands.';
            Say ' ';
          End
        End
        Else Do
          Call LineAdd Strip(Left(oper.1,80),'Trailing');
          Call ResetLocationPointer;
        End
      End

    When Left(cmd,2) == 'LD'   Then      /* LDROP */
      Do
        Call LineDrop;
        Call ResetLocationPointer;
      End

    When Left(cmd,2) == 'CL',            /* CLEAR */
       | Left(cmd,2) == 'RE'   Then      /* RESET */
      Do
        DropBuf;
        Drop lca.;
        lca.  = '';
        lca.0 = 0;
        Call ResetLocationPointer;
      End

    When Left(cmd,2) == 'CO'   Then      /* COPY  */
      Do
        If oper.1 == '*' Then oper.1 = rec.0 - r + 1;
        If \DataType(oper.1,'W') Then oper.1 = 1;
        If oper.1 > rec.0 Then oper.1 = rec.0 - r + 1;
        Do r  = r to rec.0 While oper.1 > 0
          Queue rec.r;
          oper.1 = oper.1 - 1;
        End
        Call ResetLocationPointer;
      End

    When Left(cmd,1) == 'P'    Then      /* PASTE */
      Do
        If oper.1 = '*' | oper.1 = '' Then oper.1 = Queued();
        If \DataType(oper.1,'W') Then oper.1 = 1;
        If oper.1 > Queued() Then oper.1 = Queued();
        Do n  = oper.1 to 1 by -1
          Parse Pull str;
          Call LineAdd Strip(Left(str,80),'Trailing');
        End
        Call ResetLocationPointer;
      End

    When Left(cmd,1) == 'L',             /* LOCATE */
       | Left(cmd,1) == 'F'    Then      /* FIND   */
      Do
        If oper.0 > 1 Then Do
          If online Then
            map3.map3_Bruler.output = ' Too many operands.';
          Else Do
            Say ' Too many operands.';
            Say ' ';
          End
        End
        Else Do
          s = r;                 /* temporary record pointer */
          p = p + 1;
          If Pos(Translate(oper.1),Translate(rec.r),p) = 0 Then Do
            s = s + 1;
            Do s = s to rec.0
              p = Pos(Translate(oper.1),Translate(rec.s));
              If p > 0 Then Leave;
            End
          End
          If s > rec.0 Then Do
            If online Then
              map3.map3_Bruler.output = ' String not found.';
            Else Do
              Say ' String not found.';
              Say ' ';
            End
          End
          Else Do
            Call ResetSourceString;
            p = Pos(Translate(oper.1),Translate(rec.s),p);
            If p > 0 Then src = oper.1;
            r = s;
          End
          Call SetRecordPointer r;
        End
      End

    When online,
       & Left(cmd,1) == 'C'    Then      /* CHANGE */
      Do
        If oper.0 <> 2 Then Do
          If online Then
            map3.map3_Bruler.output = ' Wrong number of operands.';
          Else Do
            Say ' Wrong number of operands.';
            Say ' ';
          End
        End
        Else Do
          If p < 1 Then
            q = Pos(Translate(oper.1),Translate(rec.map3_ptr));
          Else Do
            q = Pos(Translate(oper.1),Translate(rec.map3_ptr),p);
          End
          If q > 0 Then Do
            upd_flag = True;
            src = oper.1;
            tgt = oper.2;
            rec.map3_ptr = Insert(tgt,DelStr(rec.map3_ptr,q,Length(src)),q-1);
          End
        End
      End

    When Left(cmd,1) == 'O'    Then      /* OVERLAY */
      Do
        If oper.0 > 1 Then Do
          If online Then
            map3.map3_Bruler.output = ' Too many operands.';
          Else Do
            Say ' Too many operands.';
            Say ' ';
          End
        End
        Else Do
          upd_flag = True;
          rec.r = Strip(Left(oper.1,80),'Trailing');
          Call ResetLocationPointer;
        End
      End

    When Left(cmd,1) == 'I',             /* INSERT  */
      |  Left(cmd,1) == 'R',             /* REPLACE */
      |  Left(cmd,1) == 'D'    Then      /* DELETE  */
      Select
        When cmd <> 'INSERT' & p = 0 Then
          Do
            If online Then
              map3.map3_Bruler.output = ' Perform LOCATE first.';
            Else Do
              Say ' Perform LOCATE first.';
              Say ' ';
            End
          End
        When cmd <> 'INSERT' & src = '' Then
          Do
            If online Then
              map3.map3_Bruler.output,
                         = ' New LOCATE required after REPLACE/DELETE.';
            Else Do
              Say ' New LOCATE required after REPLACE/DELETE.';
              Say ' ';
            End
          End
        Otherwise
          upd_flag = True;
          If cmd <> 'INSERT' Then Do
            rec.r = DelStr(rec.r,p,Length(src));
          End
          If cmd <> 'DELETE' Then Do
            rec.r = Insert(oper.1,rec.r,IFF(p=0,p,p-1));
          End
          rec.r = Strip(Left(rec.r,80),'Trailing');
          Call ResetSourceString;
      End

    Otherwise
      If cmd > ' ' Then Do
        If online Then
          map3.map3_Bruler.output = ' Unknown command =' cmd;
        Else Do
          Say ' Unknown command =' cmd;
          Say ' ';
        End
      End

  End

  Call ProcessLineCommands;
  If lca_pending,
  &  Pos('--+--',map3.map3_Bruler.output) > 0,
  Then Do
    map3.map3_Bruler.output = ' LCA block command pending.';
  End

/* ----------------------------------------------------------------- */
/* End of editing                                                    */
/* ----------------------------------------------------------------- */

End

/* ----------------------------------------------------------------- */
/* End of member selection                                           */
/* ----------------------------------------------------------------- */

End

/* ----------------------------------------------------------------- */
/* End of sublibrary selection                                       */
/* ----------------------------------------------------------------- */

End

If online Then Do
  Call RX3270IO 'ERASE','map0.';       /* clear screen */
  If rc <> 0 Then Do
    Say " RX3270IO Erase error: rc = "rc", result = "result;
    Signal Dump;
  End
End

If Queued() > 0 & rc = 0 Then Do
  z = ASSGN('STDIN','SYSLOG');
  z = ASSGN('STDOUT','SYSLOG');
  Say pgm': The Data stack is not empty.  Clear it?';
  Parse Upper External resp .;
  z = ASSGN('STDOUT','SYSLST');
  If Left(resp,1) == 'Y' Then DropBuf;
End

Say pgm': Terminated.';
Exit rc;

/* ----------------------------------------------------------------- */
/* MAINLINE MODULES                                                  */
/* ----------------------------------------------------------------- */

Map0_Processing:
  If mbuild.m0 Then Do
    Call RX3270IN; /* initialize REXX program with 3270 literals */
    If rc <> 0 | result <> 0 Then Do
      Say " RX3270IN Call error: rc = "rc", result = "result;
      Signal Dump;
    End
    Call BuildMap0;
  End
  Do While rc = 0 & dlbl = ''
    map0.hdr_date.output = Left(Date('Usa'),6)||Right(Date(),4);
    t = Right(Time('Civilian'),7,'0');
    map0.hdr_time.output = Left(t,5)' 'Right(t,2);
    If map0.err_mesg.output <> '' Then Do
      map0.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED','REVERSE');
    End
    Call Screen_IO 'map0.';            /* send and receive map */
    map0.map0_library.input = Translate(map0.map0_library.input);
    map0.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED',);
    map0.err_mesg.output = '';
    Select
      When map0._aid == RX3270_AID.LIT_CLEAR_KEY Then Iterate;
      When map0._aid == RX3270_AID.LIT_PF3_KEY,
         | map0._aid == RX3270_AID.LIT_PF15_KEY  Then Exit 0;
      When map0.map0_library.input = '' Then
        map0.err_mesg.output = 'An entry is required.';
      Otherwise
        If ListDir_Successful('L='map0.map0_library.input) Then
          Call BuildMap1;
        Else Do
          If result <> 'NOTFND' Then
            map0.err_mesg.output = 'Invalid library specification.';
          Else Do
            map0.err_mesg.output = 'Library not found.';
          End
        End
    End
  End
Return;

Map1_Processing:
  If map1_dtl.0 = 0 Then Do
    map1.err_mesg.output = 'Enter a sublibrary "name" to create.';
  End
  Do While rc = 0 & lib = ''
    If map1.err_mesg.output <> '' Then Do
      map1.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED','REVERSE');
    End
    If map1_ptr <> map1_save_ptr Then Call BuildMap1Body;
    map1.hdr_date.output = Left(Date('Usa'),6)||Right(Date(),4);
    t = Right(Time('Civilian'),7,'0');
    map1.hdr_time.output = Left(t,5)' 'Right(t,2);
    map1.map1_sublib.input = '';
    Call Screen_IO 'map1.';          /* send and receive map */
    map1.map1_sublib.input = Translate(map1.map1_sublib.input);
    map1.map1_sublib.output = '';
    map1.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED',);
    map1.err_mesg.output = '';
    Select
      When map1._aid == RX3270_AID.LIT_CLEAR_KEY Then Iterate;
      When map1._aid == RX3270_AID.LIT_PF1_KEY,
         | map1._aid == RX3270_AID.LIT_PF13_KEY  Then
        Call Help '1';
      When map1._aid == RX3270_AID.LIT_PF3_KEY,
         | map1._aid == RX3270_AID.LIT_PF15_KEY  Then
        Leave;
      When map1._aid == RX3270_AID.LIT_PF5_KEY,
         | map1._aid == RX3270_AID.LIT_PF17_KEY  Then
        Do
          str = Strip(map1.map1_sublib.input);
          If Length(str) = 0 Then
            map1.err_mesg.output = 'No find string entered.';
          Else Do
            i = map1_ptr + 1;
            Do i = i to map1_dtl.0,
              While Pos(str,Left(map1_dtl.i,8)) = 0
            End
            If i > map1_dtl.0 Then Do
              map1.map1_sublib.output = str;
              map1.err_mesg.output = 'String not found.';
            End
            Else Do
              map1.err_mesg.output = 'String found.';
              map1_ptr = i;
            End
          End
        End
      When map1._aid == RX3270_AID.LIT_PF6_KEY,
         | map1._aid == RX3270_AID.LIT_PF18_KEY  Then
        map1_ptr = 1;
      When map1._aid == RX3270_AID.LIT_PF7_KEY,
         | map1._aid == RX3270_AID.LIT_PF19_KEY  Then
        Do
          map1_ptr = map1_ptr - map1_cnt;
          If map1_ptr < 1 Then map1_ptr = 0;
        End
      When map1._aid == RX3270_AID.LIT_PF8_KEY,
         | map1._aid == RX3270_AID.LIT_PF20_KEY  Then
        Do
          map1_ptr = map1_ptr + map1_cnt;
          If map1_ptr > map1_dtl.0 Then Do
            map1_ptr = map1_dtl.0 - map1_cnt + 1;
            If map1_ptr < 1 Then map1_ptr = 1;
          End
        End
      When map1._aid == RX3270_AID.LIT_PF9_KEY,
         | map1._aid == RX3270_AID.LIT_PF21_KEY  Then
        Do
          map1_ptr = map1_dtl.0 - map1_cnt + 2;
          If map1_ptr < 1 Then map1_ptr = 1;
        End
      When map1._aid == RX3270_AID.LIT_PF12_KEY,
         | map1._aid == RX3270_AID.LIT_PF24_KEY  Then
        Do
          Call ListDir_Successful 'L='dlbl;
          Call BuildMap1;
          map1_save_ptr = '';
        End
      Otherwise
        If map1.map1_sublib.input = '' Then Do
          If Map1HasSelections() Then Do
            If map1.map1_sublib.input = '' Then Iterate;
          End
          Else Do
            Parse Value CursorPosition(map1._csr,map1._cols,map1_hdc),
                  With row col;
            If row > 0 & row <= map1_fnm.0 Then Do
              i = map1_ptr + (row - 1);
              If i > 0 & i <= map1_dtl.0 Then Do
                Parse Var map1_dtl.i map1.map1_sublib.input .;
              End
            End
            If map1.map1_sublib.input = '' Then Do
              map1.err_mesg.output = 'An entry is required.';
              Iterate;
            End
          End
        End
        If ListDir_Successful('S='dlbl'.'map1.map1_sublib.input) Then
          Call BuildMap2;
        Else Do
          If result <> 'NOTFND' Then
            map1.err_mesg.output = 'Invalid Sublibrary specification.';
          Else Do
            If NewSublibrary('S='dlbl'.'map1.map1_sublib.input) Then Do
              If map1_dtl.0 > 1 Then Do
                Call RebuildMap1Detail map1.map1_sublib.input;
                If map1_ptr = map1_save_ptr Then Call BuildMap1Body;
              End
              Else Do
                Call ListDir_Successful 'L='dlbl;
                Call BuildMap1;
                map1_save_ptr = '';
              End
            End
            Else Do
              If map0._aid <> RX3270_AID.LIT_PF2_KEY Then
                   map1.err_mesg.output = 'Sublibrary not found.';
              Else map1.err_mesg.output = 'Sublibrary define failed.';
            End
          End
        End
    End
  End
Return;

Map2_Processing:
  If map2_dtl.0 = 0 Then Do
    map2.err_mesg.output = 'Enter a member "name.type" to create.';
  End
  Do While rc = 0 & mbr = ''
    If map2.err_mesg.output <> '' Then Do
      map2.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED','REVERSE');
    End
    If map2_ptr <> map2_save_ptr Then Call BuildMap2Body;
    map2.hdr_date.output = Left(Date('Usa'),6)||Right(Date(),4);
    t = Right(Time('Civilian'),7,'0');
    map2.hdr_time.output = Left(t,5)' 'Right(t,2);
    map2.map2_member.input = '';
    Call Screen_IO 'map2.';          /* send and receive map */
    map2.map2_member.input = Translate(map2.map2_member.input);
    map2.map2_member.output = '';
    map2.err_mesg.attrib = BuildAttribute('ASKIP_BRT',,'RED',);
    map2.err_mesg.output = '';
    Select
      When map2._aid == RX3270_AID.LIT_CLEAR_KEY Then Iterate;
      When map2._aid == RX3270_AID.LIT_PF1_KEY,
         | map2._aid == RX3270_AID.LIT_PF13_KEY  Then
        Call Help '2';
      When map2._aid == RX3270_AID.LIT_PF3_KEY,
         | map2._aid == RX3270_AID.LIT_PF15_KEY  Then
        Do
          map1.map1_sublib.input = '';
          Leave;
        End
      When map2._aid == RX3270_AID.LIT_PF5_KEY,
         | map2._aid == RX3270_AID.LIT_PF17_KEY  Then
        Do
          str = Strip(map2.map2_member.input);
          If Length(str) = 0 Then
            map2.err_mesg.output = 'No find string entered.';
          Else Do
            i = map2_ptr + 1;
            Do i = i to map2_dtl.0,
              While Pos(str,Left(map2_dtl.i,17)) = 0
            End
            If i > map2_dtl.0 Then Do
              map2.map2_member.output = str;
              map2.err_mesg.output = 'String not found.';
            End
            Else Do
              map2.err_mesg.output = 'String found.';
              map2_ptr = i;
            End
          End
        End
      When map2._aid == RX3270_AID.LIT_PF6_KEY,
         | map2._aid == RX3270_AID.LIT_PF18_KEY  Then
        map2_ptr = 1;
      When map2._aid == RX3270_AID.LIT_PF7_KEY,
         | map2._aid == RX3270_AID.LIT_PF19_KEY  Then
        Do
          map2_ptr = map2_ptr - map2_cnt;
          If map2_ptr < 1 Then map2_ptr = 0;
        End
      When map2._aid == RX3270_AID.LIT_PF8_KEY,
         | map2._aid == RX3270_AID.LIT_PF20_KEY  Then
        Do
          map2_ptr = map2_ptr + map2_cnt;
          If map2_ptr > map2_dtl.0 Then Do
            map2_ptr = map2_dtl.0 - map2_cnt + 1;
            If map2_ptr < 1 Then map2_ptr = 1;
          End
        End
      When map2._aid == RX3270_AID.LIT_PF9_KEY,
         | map2._aid == RX3270_AID.LIT_PF21_KEY  Then
        Do
          map2_ptr = map2_dtl.0 - map2_cnt + 2;
          If map2_ptr < 1 Then map2_ptr = 1;
        End
      When map2._aid == RX3270_AID.LIT_PF12_KEY,
         | map2._aid == RX3270_AID.LIT_PF24_KEY  Then
        Do
          Call ListDir_Successful 'S='lib;
          Call BuildMap2;
          map2_save_ptr = '';
        End
      Otherwise
        If map2.map2_member.input = '' Then Do
          If Map2HasSelections() Then Do
            If map2.map2_member.input = '' Then Iterate;
          End
          Else Do
            Parse Value CursorPosition(map2._csr,map2._cols,map2_hdc),
                  With row col;
            If row > 0 & row <= map2_fnm.0 Then Do
              i = map2_ptr + (row - 1);
              If i > 0 & i <= map2_dtl.0 Then Do
                Parse Var map2_dtl.i n1 n2 .;
                map2.map2_member.input = n1'.'n2;
              End
            End
            If map2.map2_member.input = '' Then Do
              map2.err_mesg.output = 'An entry is required.';
              Iterate;
            End
          End
        End
        If ListMbr_Successful(map2.map2_member.input,lib) Then
          Call BuildMap3;
        Else Do
          Select
          When result = 'MEMTYP' Then
            map2.err_mesg.output = 'Member type not specified.';
          When result = 'INVTYP' Then
            map2.err_mesg.output = 'Cannot edit that member type.';
          When result = 'NOTFND' Then
            Do
              If NewMember(map2.map2_member.input,lib) Then Do
                If map2_dtl.0 > 1 Then Do
                  Call RebuildMap2Detail map2.map2_member.input;
                  If map2_ptr = map2_save_ptr Then Call BuildMap2Body;
                End
                Else Do
                  Call ListDir_Successful 'S='lib;
                  Call BuildMap2;
                  map2_save_ptr = '';
                End
              End
              Else Do
                If map0._aid <> RX3270_AID.LIT_PF2_KEY Then
                     map2.err_mesg.output = 'Member not found.';
                Else map2.err_mesg.output = 'Member define failed.';
              End
            End
          Otherwise
            map2.err_mesg.output = 'Invalid Member specification.';
          End
        End
    End
  End
Return;

Map3_Processing:
  Do While rc = 0 & cmd_string = ''
    If Pos('--+--',map3.map3_Bruler.output) = 0 Then Do
      map3.map3_Bruler.attrib = BuildAttribute('ASKIP_BRT',,'RED','REVERSE');
    End
    Call BuildMap3Body;
    map3.map3_curl.output = map3_ptr;
    map3.map3_maxl.output = rec.0;
    map3.map3_date.output = Left(Date('Usa'),6)||Right(Date(),4);
    t = Right(Time('Civilian'),7,'0');
    map3.map3_time.output = Left(t,5)' 'Right(t,2);
    map3.map3_command.input = '';
    map3.map3_Truler.output = SubStr(source_ruler,map3_view,72);
    If p > 0 Then Do
      map3.map3_Truler.output = Overlay('|',map3.map3_Truler.output,p,1);
    End
    If mixed Then map3.map3_case.output = 'MIXED';
             Else map3.map3_case.output = 'UPPER';
    Call Screen_IO 'map3.';          /* send and receive map */
    map3.map3_command.output = '';
    map3.map3_Bruler.attrib = BuildAttribute('ASKIP_BRT',,'WHITE',);
    map3.map3_Bruler.output = SubStr(source_ruler,map3_view,72);
    If map3._aid <> RX3270_AID.LIT_CLEAR_KEY,
     & map3._aid <> RX3270_AID.LIT_PA1_KEY,
     & map3._aid <> RX3270_AID.LIT_PA2_KEY,
     & map3._aid <> RX3270_AID.LIT_PA3_KEY,
     & map3._aid <> RX3270_AID.LIT_PF12_KEY,
     & map3._aid <> RX3270_AID.LIT_PF24_KEY,
    Then Do
      Call SaveBodyData;
    End
    If Symbol('map3.map3_command.input') <> 'VAR' Then
      map3.map3_command.input = '';
    Else Do
      map3.map3_command.input = Strip(map3.map3_command.input);
    End
    Select
      When map3._aid == RX3270_AID.LIT_CLEAR_KEY Then Iterate;
      When map3._aid == RX3270_AID.LIT_ENTER_KEY,
         & Length(map3.map3_command.input) > 0   Then
        Do
          cmd_input = map3.map3_command.input;
          cmd_string = cmd_input;
        End
      When map3._aid == RX3270_AID.LIT_PF1_KEY,
         | map3._aid == RX3270_AID.LIT_PF13_KEY  Then
        cmd_string = 'HELP' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF2_KEY,
         | map3._aid == RX3270_AID.LIT_PF14_KEY  Then
        cmd_string = 'RECALL' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF3_KEY,
         | map3._aid == RX3270_AID.LIT_PF15_KEY  Then
        cmd_string = 'SAVE' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF4_KEY,
         | map3._aid == RX3270_AID.LIT_PF16_KEY  Then
        Do
          If map3.map3_command.input > ' ' Then
            cmd_string = 'CHANGE' map3.map3_command.input;
          Else Do
            cmd_string = 'CHANGE' src tgt;
          End
        End
      When map3._aid == RX3270_AID.LIT_PF5_KEY,
         | map3._aid == RX3270_AID.LIT_PF17_KEY  Then
        Do
          If map3.map3_command.input > ' ' Then
            cmd_string = 'FIND' map3.map3_command.input;
          Else Do
            cmd_string = 'FIND' src;
          End
        End
      When map3._aid == RX3270_AID.LIT_PF6_KEY,
         | map3._aid == RX3270_AID.LIT_PF18_KEY  Then
        cmd_string = 'TOP' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF7_KEY,
         | map3._aid == RX3270_AID.LIT_PF19_KEY  Then
        cmd_string = 'BACKWARD' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF8_KEY,
         | map3._aid == RX3270_AID.LIT_PF20_KEY  Then
        cmd_string = 'FORWARD' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF9_KEY,
         | map3._aid == RX3270_AID.LIT_PF21_KEY  Then
        cmd_string = 'BOTTOM' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF10_KEY,
         | map3._aid == RX3270_AID.LIT_PF22_KEY  Then
        cmd_string = 'LEFT' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF11_KEY,
         | map3._aid == RX3270_AID.LIT_PF23_KEY  Then
        cmd_string = 'RIGHT' map3.map3_command.input;
      When map3._aid == RX3270_AID.LIT_PF12_KEY,
         | map3._aid == RX3270_AID.LIT_PF24_KEY  Then
        cmd_string = 'CANCEL' map3.map3_command.input;
      Otherwise
        Leave;
    End
  End
Return;

/* ----------------------------------------------------------------- */
/* Dump on non-zero return codes                                     */
/* ----------------------------------------------------------------- */

Dump:
  str = RXDMPVAR('List');    /* list all variable names */
  Parse Var str resp list
  If resp <> 'OK' Then Do
    Say ' Response = 'str;
    Exit 4;
  End
  list = SHAKESRT(list);     /* do a Shaker-Sort of list */
  Do n = 1 To Words(list)
    Interpret "Say ' "Word(list,n)" =' "Word(list,n);
  End
Exit 4;

/* ----------------------------------------------------------------- */
/* IF Function whose first argument should be a Boolean value that   */
/*             determines which of the other two arguments to return */
/* ----------------------------------------------------------------- */
IFF: Procedure Expose True False;
  Parse Arg ifCond, isTrue, isFalse;
  If ifCond = True Then Return isTrue;
Return isFalse;

/* ----------------------------------------------------------------- */
/* MakeStem subroutine creates and populates a REXX stem variable    */
/*                     from the supplied arguments; where, the first */
/*                     argument is the stem name and the remainder   */
/*                     of the arguments are the "array" elements.    */
/* ----------------------------------------------------------------- */
MakeStem:
  Interpret Arg(1)" =''";
  Interpret Arg(1)"0="||(Arg()-1);
  Do MakeStem_n = 1 to Value(Arg(1)||0)
    Interpret Arg(1)||MakeStem_n"="Arg(MakeStem_n+1);
  End
Return;

/* -------------------------------------------------------------- */
/* ParseCommandString procedure handles command string operands   */
/*                              that may optionally be delimited  */
/*                              with double or single quotes.     */
/* -------------------------------------------------------------- */
ParseCommandString: Procedure Expose cmd oper.;
  Parse Arg cmd str;
  cmd = Translate(cmd);
  str = Strip(str);
  Drop oper.;
  n = 0;
  oper. = ''
  oper.0 = n;
  Do While str <> ''
    n = n + 1;
    delim = Left(str,1);
    If delim <> '"' & delim <> "'" Then
      Parse Var str oper.n str;
    Else Do
      Parse Var str (delim) oper.n (delim) str;
      If Left(str,1) = delim Then Do
        Parse Var str 2 xtra (delim) str;
        oper.n = oper.n||delim||xtra;
      End
    End
    str = Strip(str);
  End
  oper.0 = n;
Return;

/* ----------------------------------------------------------------- */
/* INPUT-OUTPUT ROUTINES                                             */
/* ----------------------------------------------------------------- */

Screen_IO:
  Arg stem;
  Call RX3270IO 'ERWRT',stem;          /* send the map */
  If rc <> 0 Then Do
    Say " RX3270IO Write error: rc = "rc", result = "result;
    Signal Dump;
  End
  Call RX3270IO 'READM',stem;          /* receive the map */
  If rc <> 0 Then Do
    Say " RX3270IO ReadM error: rc = "rc", result = "result;
    Signal Dump;
  End
Return;

Converse:
  map0._ccc = RX3270_CCC.LIT_LCL_WRITE         ||,
              RX3270_WCC.LIT_FREEKB_FRSET_ALARM||,
              RX3270_DSO.LIT_SET_BUFFER_ADDRESS||,
              d2c(map0._cols*(22-1),2)         ||,
              RX3270_DSO.LIT_START_FIELD       ||,
              RX3270_ATR.LIT_ASKIP_BRT         ||,
              Arg(1);
  If Arg() > 1,
  &  DataType(Arg(2),'W'),
  Then Do
    map0._ccc = map0._ccc':'                ||,
                RX3270_DSO.LIT_START_FIELD  ||,
                RX3270_ATR.LIT_UNPROT_BRT   ||,
                RX3270_DSO.LIT_INSERT_CURSOR||,
                Copies('00'x,Arg(2));
  End
  map0._ccc = map0._ccc                 ||,
              RX3270_DSO.LIT_START_FIELD||,
              RX3270_ATR.LIT_ASKIP_NORM;
  Call RX3270IO 'WRTDS','map0.';      /* send/receive prompt */
  If rc <> 0 Then Do
    Say " RX3270IO WrtDS error: rc = "rc", result = "result;
    Signal Dump;
  End
Return;

Help:
  Arg m;
  cname = 'h'm'a';
  mname = 'help'm'a';
  Do While rc = 0
    If mbuild.cname Then Interpret "Call Build"mname;
    d = Left(Date('Usa'),6)||Right(Date(),4);
    t = Right(Time('Civilian'),7,'0');
    t = Left(t,5)' 'Right(t,2);
    Interpret mname".hdr_date.output = d";
    Interpret mname".hdr_time.output = t";
    Call Screen_IO mname'.';           /* send and receive map */
    Interpret mname".err_mesg.output = ''";
    Interpret "aid = "mname"._aid";
    Select
    When aid == RX3270_AID.LIT_CLEAR_KEY Then Iterate;
    When aid == RX3270_AID.LIT_PF3_KEY,
       | aid == RX3270_AID.LIT_PF15_KEY  Then Return;
    When aid == RX3270_AID.LIT_PF7_KEY,
       | aid == RX3270_AID.LIT_PF19_KEY  Then
      Do
        Select
        When Right(mname,1) == 'd' Then
          Do
            cname = Left(cname,2)'c';
            mname = Left(mname,5)'c';
          End
        When Right(mname,1) == 'c' Then
          Do
            cname = Left(cname,2)'b';
            mname = Left(mname,5)'b';
          End
        When Right(mname,1) == 'b' Then
          Do
            cname = Left(cname,2)'a';
            mname = Left(mname,5)'a';
          End
        Otherwise
          Interpret mname".err_mesg.output = 'First help page.';";
        End
      End
    When aid == RX3270_AID.LIT_PF8_KEY,
       | aid == RX3270_AID.LIT_PF20_KEY  Then
      Do
        Select
        When Right(mname,1) == 'a' Then
          Do
            cname = Left(cname,2)'b';
            mname = Left(mname,5)'b';
          End
        When Right(mname,1) == 'b' & m = '3' Then
          Do
            cname = Left(cname,2)'c';
            mname = Left(mname,5)'c';
          End
        When Right(mname,1) == 'c' & m = '3' Then
          Do
            cname = Left(cname,2)'d';
            mname = Left(mname,5)'d';
          End
        Otherwise
          Interpret mname".err_mesg.output = 'Last help page.';";
        End
      End
    Otherwise
    End
  End
Return;

/* ----------------------------------------------------------------- */
/* VSE LIBRARY INTERFACE ROUTINES                                    */
/* ----------------------------------------------------------------- */

ListDir_Successful: Procedure Expose syslst. result False True;

  sysipt.  = '';
  sysipt.0 = 1;
  sysipt.1 = ' LISTDIR' Arg(1);
  If Left(Arg(1),2) == 'L=' Then sysipt.1 = sysipt.1 'OUTPUT=STATUS';
  y = REXXIPT('sysipt.');
  y = OUTTRAP('syslst.','*','NOCONCAT');

  Address LINK 'LIBR';
  If rc <> 0 Then Do
    Do i = 1 to syslst.0
      If Left(syslst.i,5) == 'L042I',
      |  Left(syslst.i,5) == 'L252I',
      Then Do
        result = 'NOTFND';
        Leave;
      End
    End i
    Return False;
  End
Return True;

ListMbr_Successful: Procedure Expose syslst. result exclude False True;
  Parse Value Translate(Arg(1),' ','.') With m t .;
  Select
    When t = '' Then Do
      result = 'MEMTYP';
      Return False;
    End
    When Pos('|'t'|',exclude) > 0 Then Do
      result = 'INVTYP';
      Return False;
    End
    Otherwise
  End

  sysipt.  = '';
  sysipt.0 = 2;
  sysipt.1 = ' ACCESS S='Arg(2);
  sysipt.2 = ' LIST   'm'.'t;
  y = REXXIPT('sysipt.');
  y = OUTTRAP('syslst.','*','NOCONCAT');

  Address LINK 'LIBR';
  If rc <> 0 Then Do
    Do i = 1 to syslst.0
      If Left(syslst.i,5) == 'L082I' Then Do
        result = 'NOTFND';
        Leave;
      End
    End i
    Return False;
  End
Return True;

NewMember:
  Parse Upper Arg n1,n2;
  Call Converse 'Not found.  Press PF2 to create the "'||,
                n1'" member.';
  If map0._aid <> RX3270_AID.LIT_PF2_KEY Then Return False;
  opt = 'NODATA';
  Parse Upper Var n1 mem '.' typ .;
  If typ = 'PROC' Then Do
    Call Converse 'Press PF2 again if the "DATA" flag should be',
                  '"ON" for this member.';
    If map0._aid == RX3270_AID.LIT_PF2_KEY Then opt = 'DATA';
  End

  rec.0 = 1;
  rec.1 = ' ';
  "EXECIO * DISKW" n2'.'n1 "( OPEN FINIS" opt "STEM rec. )";
  If rc <> 0 Then Return False;
  n = map2_dtl.0 + 1;
  Parse Var n1 n1 '.' n2 .;
  map2_dtl.n = Left(n1,8) Left(n2,8) '*** New Member ***';
  map2_dtl.0 = n;
Return True;

NewSublibrary:
  Parse Upper Arg . '=' n1 '.' n2 .;
  Call Converse 'Not found.  Press PF2 to create the "'||,
                n1'.'n2'" sublibrary.';
  If map0._aid <> RX3270_AID.LIT_PF2_KEY Then Return False;

  If OperationFailed('define',Arg(1)) Then Return False;
  n = map1_dtl.0 + 1;
  map1_dtl.n = Left(n2,10) '*** New Sublibrary ***';
  map1_dtl.0 = n;
Return True;

OperationFailed: Procedure Expose False True;
  Arg oper2,obj2,obj1;
  If oper2 = 'MOVE',
  |  oper2 = 'COPY'
  Then
    oper1 = 'CONNECT';
  Else Do
    oper1 = 'ACCESS';
  End

  sysipt.  = '';
  sysipt.0 = 0;
  If obj1 <> '' Then Do
    n = sysipt.0 + 1
    sysipt.n = ' 'oper1' S='obj1;
    sysipt.0 = n;
  End
  n = sysipt.0 + 1
  sysipt.n = ' 'oper2  obj2;
  sysipt.0 = n;

  y = REXXIPT('sysipt.');
  y = OUTTRAP('syslst.','*','NOCONCAT');
  Address LINK 'LIBR';

  If rc <> 0 Then Do
    Do i = 1 to sysipt.0
      Say sysipt.i;
    End
    Do i = 1 to syslst.0
      Say syslst.i;
    End
    Say ' ';
    Return True;
  End
Return False;

/* ----------------------------------------------------------------- */
/* EDIT COMMAND ROUTINES                                             */
/* ----------------------------------------------------------------- */

SetTopOfMember:
  Call SetRecordPointer 1;
  Call ResetLocationPointer;
Return;

SetRecordPointer:
  r   = Arg(1);
  If r > rec.0 Then r = rec.0;
  If r < 1 Then r = 0;
  map3_ptr = r;
  If r < 0 Then r = 1;
Return;

ResetLocationPointer:
  p   = 0;
  Call ResetSourceString;
Return;

ResetSourceString:
  src = '';
Return;

ReadMember:
  If new Then Do
    new   = False;
    upd_flag = True;
    rec.0 = 1;
    rec.1 = ' ';
    Return;
  End
  "EXECIO * DISKR" lib'.'mbr "( OPEN FINIS STEM rec. )";
  If rc <> 0 Then Do
    Say ' Error: Reading member "'lib'.'mbr'"; rc = 'rc;
  End
Return;

WriteMember:
  upd_flag = False;
  If rec.0 = 0 Then Do
    Say ' Cannot save empty member.';
    Return;
  End
  "EXECIO * DISKW" lib'.'mbr "( OPEN FINIS" opt "STEM rec. )";
  If rc <> 0 Then Do
    Say ' Error: Writing member "'lib'.'mbr'"; rc = 'rc;
    rc = 0;
  End
Return;

LineAdd: Procedure Expose True False upd_flag lca. rec. r;
  Parse Arg str;
  n     = r;
  upd_flag = True;
  rec.0 = rec.0 + 1;
  Do r  = rec.0 to n by -1   /* loop from end backwards to current */
    s   = r - 1;
    If s > n Then Do
      lca.r = lca.s;
      rec.r = rec.s;
    End
    Else Do
      lca.r = '';
      rec.r = str||' ';
      Leave;
    End
  End
Return;

LineDrop: Procedure Expose True False upd_flag lca. rec. r;
  upd_flag = True;
  Do s  = r to rec.0         /* loop from current forwards to end */
    n   = s + 1;
    If s < rec.0 Then Do
      lca.s = lca.n;
      rec.s = rec.n;
    End
    Else Do
      lca.s = '';
      rec.s = '';
      Leave;
    End
  End
  If rec.0 > 0 Then Do
    rec.0 = rec.0 - 1;
  End
Return;

ProcessLineCommands: Procedure Expose True False upd_flag map3. rec.,
                                      map3_ptr map3_Bruler lca.,
                                      lca_pending lca_cmds lca_blks,
                                      lower_case upper_case;
  lca_errors = False;
  lca_pending = False;
  single_pending = False;
  delt_begin = 0; delt_ending = 0;
  copy_begin = 0; copy_ending = 0;
  move_begin = 0; move_ending = 0;
  lowr_begin = 0; lowr_ending = 0;
  uppr_begin = 0; uppr_ending = 0;
  left_begin = 0; left_ending = 0; left_cnt = 0;
  rght_begin = 0; rght_ending = 0; rght_cnt = 0;

  Do r = 1 Until r > rec.0   /* loop through member records */
    If Length(lca.r) = 0 Then Iterate;

    Parse Value Translate(lca.r,' ','0123456789') With str .;

    If Pos(Left(str,1),lca_cmds) = 0,
    |  Length(str) < 1 | Length(str) > 2 Then Do
      map3.map3_Bruler.output = ' Invalid LCA command format.';
      lca_errors = True;
      Iterate;
    End
    If Length(str) = 1,
    &  Pos(str,lca_cmds) > 0,
    |  Length(str) = 2,
    &  Pos('|'str'|',lca_blks) > 0,
    Then Do
      If Length(str) = 2 Then
        Parse Var lca.r 3 nbr .;  /* a block  line command */
      Else Do
        Parse Var lca.r 2 nbr .;  /* a single line command */
      End
      If Length(nbr) > 0,
      & (Pos(str,'AB') > 0 & nbr <> '*'),
      & \Datatype(nbr,'W'),
      Then Do
        map3.map3_Bruler.output = ' Invalid LCA count value.';
        lca_errors = True;
        Iterate;
      End
    End
    Else Do
      map3.map3_Bruler.output = ' Undefined LCA command.';
      lca_errors = True;
      Iterate;
    End
    If Pos(str,'AB') = 0 Then Do
      If Length(nbr) = 0 Then nbr = 1;      /* default of  1  */
    End
    Else Do
      single_pending = True;      /* do in second pass */
      If Length(nbr) = 0 Then nbr = '*';    /* default of ALL */
    End

    Select                   /* set beginning of block commmands */
      When delt_begin = 0 & str = 'DD' Then Do
        lca_pending = True; delt_begin = r; lca.r = 'DD';
        Iterate;
      End
      When copy_begin = 0 & str = 'CC' Then Do
        lca_pending = True; copy_begin = r; lca.r = 'CC';
        Iterate;
      End
      When move_begin = 0 & str = 'MM' Then Do
        lca_pending = True; move_begin = r; lca.r = 'MM';
        Iterate;
      End
      When lowr_begin = 0 & str = 'LL' Then Do
        lca_pending = True; lowr_begin = r; lca.r = 'LL';
        Iterate;
      End
      When uppr_begin = 0 & str = 'UU' Then Do
        lca_pending = True; uppr_begin = r; lca.r = 'UU';
        Iterate;
      End
      When left_begin = 0 & str = '<<' Then Do
        left_cnt = nbr;
        lca_pending = True; left_begin = r; lca.r = '<<'||nbr;
        Iterate;
      End
      When rght_begin = 0 & str = '>>' Then Do
        rght_cnt = nbr;
        lca_pending = True; rght_begin = r; lca.r = '>>'||nbr;
        Iterate;
      End
      Otherwise
    End

    If Length(str) = 1,      /* mix in no single line LCA cmds */
    & (copy_begin > 0 & copy_ending = 0,
    |  move_begin > 0 & move_ending = 0,
    |  lowr_begin > 0 & lowr_ending = 0,
    |  uppr_begin > 0 & uppr_ending = 0,
    |  left_begin > 0 & left_ending = 0,
    |  rght_begin > 0 & rght_ending = 0),
    Then Do
      If \lca_errors Then Do
        map3.map3_Bruler.output = ' Cannot mix single line commands',
                                ||' inside block line commands.';
        lca_errors = True;
      End
      Iterate;
    End

    If Length(str) = 2,      /* mix in no other block commands */
    & (copy_begin > 0 & Pos('|'str'|','|MM|LL|UU|<<|>>|') > 0,
    |  move_begin > 0 & Pos('|'str'|','|CC|LL|UU|<<|>>|') > 0,
    |  lowr_begin > 0 & Pos('|'str'|','|MM|CC|UU|<<|>>|') > 0,
    |  uppr_begin > 0 & Pos('|'str'|','|MM|CC|LL|<<|>>|') > 0,
    |  left_begin > 0 & Pos('|'str'|','|MM|CC|LL|UU|>>|') > 0,
    |  rght_begin > 0 & Pos('|'str'|','|MM|CC|LL|UU|<<|') > 0),
    Then Do
      If \lca_errors Then Do
        map3.map3_Bruler.output = ' Cannot mix LCA block commands',
                                ||' inside other block commands.';
        lca_errors = True;
      End
      Iterate;
    End

    Select                   /* set ending of block commmands */
      When delt_begin > 0 & delt_ending = 0 & str = 'DD' Then Do
        delt_ending  = r;
        If \lca_errors Then Do
          lca.delt_begin = ''; lca.delt_ending = '';
          r = delt_begin;
          s = r;                  /* save current record number */
          n = delt_ending - delt_begin + 1;
          Do i = 1 to n
            Call LineDrop;
          End
          r = s - 1;              /* re-test current record */
          delt_begin = 0; delt_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When copy_begin > 0 & copy_ending = 0 & str = 'CC' Then Do
        copy_ending  = r;
        If \lca_errors Then Do
          lca.copy_begin = ''; lca.copy_ending = '';
          s = r;                  /* save current record number */
          Do r = copy_begin to copy_ending
            Queue rec.r;
          End
          r = s - 1;              /* re-test current record */
          copy_begin = 0; copy_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When move_begin > 0 & move_ending = 0 & str = 'MM' Then Do
        move_ending  = r;
        If \lca_errors Then Do
          lca.move_begin = ''; lca.move_ending = '';
          Do r = move_begin to move_ending
            Queue rec.r;
          End
          r = move_begin;              /* restore to beginning */
          s = r;                  /* save current record number */
          n = move_ending - move_begin + 1;
          Do i = 1 to n
            Call LineDrop;
          End
          r = s - 1;              /* re-test current record */
          move_begin = 0; move_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When lowr_begin > 0 & lowr_ending = 0 & str = 'LL' Then Do
        lowr_ending  = r;
        If \lca_errors Then Do
          upd_flag = True;
          lca.lowr_begin = ''; lca.lowr_ending = '';
          Do s = lowr_begin to lowr_ending
            rec.s = Translate(rec.s,lower_case,upper_case);
          End
          lowr_begin = 0; lowr_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When uppr_begin > 0 & uppr_ending = 0 & str = 'UU' Then Do
        uppr_ending  = r;
        If \lca_errors Then Do
          upd_flag = True;
          lca.uppr_begin = ''; lca.uppr_ending = '';
          Do s = uppr_begin to uppr_ending
            rec.s = Translate(rec.s);
          End
          uppr_begin = 0; uppr_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When left_begin > 0 & left_ending = 0 & str = '<<' Then Do
        left_ending  = r;
        If \lca_errors Then Do
          upd_flag = True;
          lca.left_begin = ''; lca.left_ending = '';
          n = left_cnt;
          Do s = left_begin to left_ending
            rec.s = Substr(rec.s,n+1);
          End
          left_begin = 0; left_ending = 0;
          Call ResetLocationPointer;
        End
      End
      When rght_begin > 0 & rght_ending = 0 & str = '>>' Then Do
        rght_ending  = r;
        If \lca_errors Then Do
          upd_flag = True;
          lca.rght_begin = ''; lca.rght_ending = '';
          n = rght_cnt;
          Do s = rght_begin to rght_ending
            rec.s = Strip(Left(Copies(' ',n)||rec.s,80),'Trailing');
          End
          rght_begin = 0; rght_ending = 0;
          Call ResetLocationPointer;
        End
      End
                             /* single (non-block) LCA commands */
      When str = 'I' Then Do      /* do interspersed INSERT cmd */
        If \lca_errors Then Do
          lca.r = '';
          s = r;                  /* save current record number */
          Call LineAdd '13'x;
          Do r = (r+1) to (r+nbr-2)
            Call LineAdd;         /* add blank records */
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
      End
      When str = 'D' Then Do      /* do interspersed DELETE cmd */
        If \lca_errors Then Do
          If (r + nbr - 1) > rec.0 Then nbr = rec.0 - r + 1;
          lca.r = '';
          s = r;                  /* save current record number */
          Do i = 1 to nbr
            Call LineDrop;        /* delete records */
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
      End
      When str = 'C' Then Do      /* do interspersed COPY cmd */
        If \lca_errors Then Do
          If (r + nbr - 1) > rec.0 Then nbr = rec.0 - r + 1;
          lca.r = '';
          Do s = r to (r+nbr-1)
            Queue rec.s;
          End
          Call ResetLocationPointer;
        End
      End
      When str = 'M' Then Do      /* do interspersed MOVE cmd */
          If (r + nbr - 1) > rec.0 Then nbr = rec.0 - r + 1;
        If \lca_errors Then Do
          lca.r = '';
          Do s = r to (r+nbr-1)
            Queue rec.s;
          End
          s = r;                  /* save current record number */
          Do n = 1 to nbr
            Call LineDrop;
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
      End
      When str = 'L' Then Do      /* do interspersed LOWER cmd */
        If \lca_errors Then Do
          If (r + nbr - 1) > rec.0 Then nbr = rec.0 - r + 1;
          lca.r = '';
          upd_flag = True;
          Do s = r to (r+nbr-1)
            rec.s = Translate(rec.s,lower_case,upper_case);
          End
          Call ResetLocationPointer;
        End
      End
      When str = 'U' Then Do      /* do interspersed UPPER cmd */
        If \lca_errors Then Do
          If (r + nbr - 1) > rec.0 Then nbr = rec.0 - r + 1;
          lca.r = '';
          upd_flag = True;
          Do s = r to (r+nbr-1)
            rec.s = Translate(rec.s);
          End
          Call ResetLocationPointer;
        End
      End
      When str = '<' Then Do      /* do interspersed SHIFT cmd */
        If \lca_errors Then Do
          lca.r = '';
          upd_flag = True;
          rec.r = Substr(rec.r,nbr+1);
          Call ResetLocationPointer;
        End
      End
      When str = '>' Then Do
        If \lca_errors Then Do
          lca.r = '';
          upd_flag = True;
          rec.r = Strip(Left(Copies(' ',nbr)||rec.r,80),'Trailing');
          Call ResetLocationPointer;
        End
      End
      When str = '"' Then Do      /* do interspersed DITTO cmd */
        If \lca_errors Then Do
          lca.r = '';
          s = r;                  /* save current record number */
          Do i = 1 to nbr
            Call LineAdd rec.s;
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
      End
      Otherwise
    End
  End                             /* end first pass */
  If lca_errors Then Return;

  If lca_pending,
  & (copy_begin > 0 & copy_ending = 0,
  |  move_begin > 0 & move_ending = 0,
  |  lowr_begin > 0 & lowr_ending = 0,
  |  uppr_begin > 0 & uppr_ending = 0,
  |  left_begin > 0 & left_ending = 0,
  |  rght_begin > 0 & rght_ending = 0),
  Then Return;                    /* not all blocks processed */
  Else lca_pending = False;       /* all blocks processed */

  If single_pending Then Do       /* second pass for single cmds  */
    Do r = 1 Until r > rec.0      /* that remove lines from stack */
      If Length(lca.r) = 0 Then Iterate;
      Parse Var lca.r str 2 nbr .;
      If Length(nbr) = 0 | nbr = '*' | nbr > Queued() Then
        nbr = Queued();
      Select
        When str = 'A' Then Do    /* do interspersed AFTER cmd */
          lca.r = '';
          s = r;                  /* save current record number */
          Do n = nbr to 1 by -1
            Parse Pull str;
            Call LineAdd Strip(Left(str,80),'Trailing');
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
        When str = 'B' Then Do    /* do interspersed BEFORE cmd */
          lca.r = '';
          s = r;                  /* save current record number */
          r = r - 1;              /* point to previous record */
          Do n = nbr to 1 by -1
            Parse Pull str;
            Call LineAdd Strip(Left(str,80),'Trailing');
          End
          r = s - 1;              /* re-test current record */
          Call ResetLocationPointer;
        End
        Otherwise
      End
    End
    single_pending = False;
  End
Return;

SubmitMember: Procedure Expose rec. rc;
  If rec.0 = 0 Then Do
    rc = 1;
    Say ' Cannot submit empty member.';
    Return;
  End
  'SETUID' UserID();
  fc = OUTTRAP('err.','*','NOCONCAT');
  Address POWER "PUTQE RDR NOGENCM STEM rec.";
  If rc <> 0 Then Do
    Say ' Error: Submitting POWER RDR queue entry; rc =' rc;
    Do i = 1 to err.0
      Say err.i;
    End
  End
Return;

/* ----------------------------------------------------------------- */
/* HELPER ROUTINES FOR SCROLLABLE MAPS                               */
/* ----------------------------------------------------------------- */

CursorPosition: Procedure
  Arg cpos,cmax,rhdc;
  row = cpos  % cmax + 1;
  col = cpos // cmax + 1;
  If rhdc <> '' Then row = row - rhdc;
Return row col;

Map1HasSelections:
  answer = False;
  delete = False;
  If map1_fnm.0 > 0 Then Do
    Do row = 1 to map1_fnm.0
      n = map1_fnm.row;
      If map1.n.output == '*** Top of List ***',
      |  map1.n.output == '*** End of List ***',
      Then Iterate;
      map1.n.input = Translate(map1.n.input);
      Select
      When map1.n.input = 'S' Then
        Do
          answer = True;
          i = map1_ptr + (row - 1);
          If i > 0 & i <= map1_dtl.0 Then Do
            Parse Var map1_dtl.i map1.map1_sublib.input .;
          End
        End
      When map1.n.input = 'D' Then
        Do
          answer = True;
          i = map1_ptr + (row - 1);
          If i > 0 & i <= map1_dtl.0 Then Do
            Parse Var map1_dtl.i slib .;
            Call Converse 'Press PF2 to confirm deletion of the "'||,
                          slib'" sublibrary.';
            If map0._aid == RX3270_AID.LIT_PF2_KEY Then Do
              If OperationFailed('delete','S='dlbl'.'slib) Then
                map1.err_mesg.output = 'Delete failed.';
              Else Do
                delete = True;
                map1_dtl.i = 'FF'x;
              End
            End
          End
        End
      Otherwise
      End
    End
    If delete = True Then Do
      Call RebuildMap1Detail;
      If map1_ptr = map1_save_ptr Then Call BuildMap1Body;
    End
  End
Return answer;

Map2HasSelections:
  answer = False;
  delete = False;
  If map2_fnm.0 > 0 Then Do
    Do row = 1 to map2_fnm.0
      n = map2_fnm.row;
      If map2.n.output == '*** Top of List ***',
      |  map2.n.output == '*** End of List ***',
      Then Iterate;
      map2.n.input = Translate(map2.n.input);
      Select
      When map2.n.input = 'S' Then
        Do
          answer = True;
          i = map2_ptr + (row - 1);
          If i > 0 & i <= map2_dtl.0 Then Do
            Parse Var map2_dtl.i n1 n2 .;
            map2.map2_member.input = n1'.'n2;
          End
        End
      When map2.n.input = 'D' Then
        Do
          answer = True;
          i = map2_ptr + (row - 1);
          If i > 0 & i <= map2_dtl.0 Then Do
            Parse Var map2_dtl.i n1 n2 .;
            Call Converse 'Press PF2 to confirm deletion of the "'||,
                          n1'.'n2'" member.';
            If map0._aid == RX3270_AID.LIT_PF2_KEY Then Do
              If OperationFailed('delete',n1'.'n2,lib) Then
                map2.err_mesg.output = 'Delete failed.';
              Else Do
                delete = True;
                map2_dtl.i = 'FF'x;
              End
            End
          End
        End
      When map2.n.input = 'M',
        |  map2.n.input = 'C' Then
        Do
          answer = True;
          oper = IFF(map2.n.input = 'M','move','copy');
          i = map2_ptr + (row - 1);
          If i > 0 & i <= map2_dtl.0 Then Do
            Parse Var map2_dtl.i n1 n2 .;
            Call Converse 'Enter the sublibrary name to 'oper' "'||,
                          n1'.'n2'" to...',8;
            If Length(map0._raw) > 6 Then Do
              lib2 = dlbl'.'Translate(Strip(SubStr(map0._raw,7,8)));
              If OperationFailed(oper,n1'.'n2,lib':'lib2) Then
                map2.err_mesg.output = 'The 'oper' failed.';
              Else Do
                If oper = 'copy' Then
                  map2_dtl.i = Left(n1,8) Left(n2,8) '*** Copied ***';
                Else Do
                  map2_dtl.i = 'FF'x;
                  delete = True;
                End
              End
            End
          End
        End
      When map2.n.input = 'R' Then
        Do
          answer = True;
          i = map2_ptr + (row - 1);
          If i > 0 & i <= map2_dtl.0 Then Do
            Parse Var map2_dtl.i n1 n2 .;
            rf = n1'.'n2;
            Call Converse 'Enter the member.type name to rename "'||,
                          rf'" to...',17;
            If Length(map0._raw) > 6 Then Do
              rt = Translate(Strip(SubStr(map0._raw,7,17)));
              If OperationFailed('rename',rf':'rt,lib) Then
                map2.err_mesg.output = 'Rename failed.';
              Else Do
                Parse Var rt n1 '.' n2 .;
                map2_dtl.i = Left(n1,8) Left(n2,8) '*** Renamed ***';
                delete = True;
              End
            End
          End
        End
      Otherwise
      End
    End
    If delete = True Then Do
      Call RebuildMap2Detail;
      If map2_ptr = map2_save_ptr Then Call BuildMap2Body;
    End
  End
Return answer;

RebuildMap1Detail: Procedure Expose map1_ptr map1_dtl.;
  n = SORTSTEM('map1_dtl.','ZONE 1 8');
  If n <> 0 Then Do
    Say ' Error: Sorting sublibrary list; rc =' n;
    Exit 8
  End
  Do i = map1_dtl.0 to 1 by -1 While map1_dtl.i = 'FF'x
  End
  map1_dtl.0 = i;
  If Arg() > 0 Then Do
    str = Left(Arg(1),8);
    Do i = 1 to map1_dtl.0 Until Left(map1_dtl.i,8) == str
    End
  End
  If map1_ptr > map1_dtl.0 Then map1_ptr = map1_dtl.0;
  If map1_ptr < 1 Then map1_ptr = 1;
Return;

RebuildMap2Detail: Procedure Expose map2_ptr map2_dtl.;
  n = SORTSTEM('map2_dtl.','ZONE 1 17');
  If n <> 0 Then Do
    Say ' Error: Sorting member list; rc =' n;
    Exit 8
  End
  Do i = map2_dtl.0 to 1 by -1 While map2_dtl.i = 'FF'x
  End
  map2_dtl.0 = i;
  If Arg() > 0 Then Do
    Parse Upper Arg nam '.' typ .;
    str = Left(nam,8)' 'Left(typ,8);
    Do i = 1 to map2_dtl.0 Until Left(map2_dtl.i,17) == str
    End
  End
  If map2_ptr > map2_dtl.0 Then map2_ptr = map2_dtl.0;
  If map2_ptr < 1 Then map2_ptr = 1;
Return;

SaveBodyData: Procedure Expose True False upd_flag map3. map3_fnm.,
                               mixed lca. rec. map3_ptr map3_view;
  Do row = 1 to map3_fnm.0
    r = map3_ptr + (row - 1);
    Parse Var map3_fnm.row lca lda .;
    If lca <> 'x',
    &  Symbol('map3.lca.input') == 'VAR',
    Then Do
      lca.r = Translate(Strip(Translate(map3.lca.input,' ','=*')));
      If r > lca.0 Then lca.0 = r;
    End
    If lca <> 'x',
    &  Symbol('map3.lda.input') == 'VAR',
    Then Do
      upd_flag = True;
      a     = Left(rec.r,map3_view-1);
      If mixed Then
        b   = Left(map3.lda.input,72,' ');
      Else Do
        b   = Left(Translate(map3.lda.input),72,' ');
      End
      c     = SubStr(rec.r,map3_view+72);
      rec.r = Strip(Left(a||b||c,80),'Trailing');
    End
  End
Return;

/* ----------------------------------------------------------------- */
/* HELPER ROUTINES FOR BUILDING MAPS                                 */
/* ----------------------------------------------------------------- */

BuildAttribute:
  Arg atr,csr,exc,exh;
  If atr = '' Then atr = 'ASKIP_NORM';
  If csr = '' Then csr = 'NO';
  If exc = '' Then exc = 'DEFAULT';
  If exh = '' Then exh = 'OFF';
  Interpret "atrb = RX3270_ATR.LIT_"atr,
                 "||RX3270_CSR.LIT_"csr,
                 "||RX3270_EXC.LIT_"exc,
                 "||RX3270_EXH.LIT_"exh;
Return atrb;

BuildField:
  Parse Arg stem,rmax,cmax,row,col,atr,out;
  n = RX3270FN(rmax, cmax, row, col);
  If n = -1 | n = -2 Then Do
    Say " RX3270FN error: rc = "n;
    Signal Dump;
  End
  Interpret stem||"n.attrib = atr";
  If out <> '' Then Do
    Interpret stem||"n.output = out";
  End
Return;

BuildHeader:
/* layout of standard header
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU          |-----varies-and-will-be-centered------|           hh:mm xx
...
 &MSG
*/
  Parse Arg stem,title,pf1,pf2;
  Interpret stem" =''";
  Interpret stem"_lun = D2C(luno)";
  Interpret stem"_wcc = RX3270_WCC.LIT_FREEKB_FRSET";
  If stem == 'map0.' Then Do
    Call RX3270IO 'QUERY',stem;
    If rc <> 0 Then Do
      Say " RX3270IO Query error: rc = "rc", result = "result;
      Exit 6;
    End
  End
  Else Do
    Interpret stem"_qry   = map0._qry";
    Interpret stem"_dev   = map0._dev";
    Interpret stem"_bufsz = map0._bufsz";
    Interpret stem"_bufsa = map0._bufsa";
    Interpret stem"_rows  = map0._rows";
    Interpret stem"_cols  = map0._cols";
    Interpret stem"_rowa  = map0._rowa";
    Interpret stem"_cola  = map0._cola";
    Interpret stem"_exc   = map0._exc";
    Interpret stem"_exh   = map0._exh";
  End

  Call BuildField stem,map0._rows,map0._cols,1,1,,
                  BuildAttribute('ASKIP_NORM',,'BLUE',),,
                  'SYSEDIT';
  Call BuildField stem,map0._rows,map0._cols,1,27,,
                  BuildAttribute('ASKIP_NORM',,'TURQUOISE',),,
                  'Stand-alone System Editor';
  Call BuildField stem,map0._rows,map0._cols,1,69,,
                  BuildAttribute('ASKIP_NORM',,'BLUE',),,
                  'mm/dd/yyyy';
  hdr_date = n;
  Call BuildField stem,map0._rows,map0._cols,2,1,,
                  BuildAttribute('ASKIP_NORM',,'BLUE',),,
                  'CUU:';
  Call BuildField stem,map0._rows,map0._cols,2,6,,
                  BuildAttribute('ASKIP_NORM',,'GREEN',),,
                  cuu;
  Call BuildField stem,map0._rows,map0._cols,2,20,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  Center(title,40);
  Call BuildField stem,map0._rows,map0._cols,2,71,,
                  BuildAttribute('ASKIP_NORM',,'BLUE',),,
                  'hh:mm xx';
  hdr_time = n;
  Call BuildField stem,map0._rows,map0._cols,22,1,,
                  BuildAttribute('ASKIP_BRT',,'RED',),,
                  '';
  err_mesg = n;
  Call BuildField stem,map0._rows,map0._cols,22,80,,
                  BuildAttribute();
  Call BuildTrailer stem,pf1,pf2;
Return;

BuildPFkeys:
/* layout of standard pfkey lines
1=Help   3=Save   4=Change   6=Top      7=Backward   10=Left    12=Cancel
2=Recall          5=Find     9=Bottom   8=Forward    11=Right
*/
  str  = '';
  Do n = 1 to Arg()
    c = pfc.n;
    If n > 1 Then str = str' ';
    If Arg(n) = '' Then Do
      str = str||Copies(' ',c);
      Iterate;
    End
    s = Left(Arg(n),c,' ');
    If n < 5 Then w = 1;
             Else w = 2;
    str = str||RX3270_DSO.LIT_SET_ATTRIBUTE ||,
               RX3270_TYP.LIT_EXTENDED_COLOR||,
               RX3270_EXC.LIT_WHITE         ||,
               Left(s,w)                    ||,
               RX3270_DSO.LIT_SET_ATTRIBUTE ||,
               RX3270_TYP.LIT_EXTENDED_COLOR||,
               RX3270_EXC.LIT_DEFAULT       ||,
               SubStr(s,w+1);
  End
Return str;

BuildTrailer:
/* layout of standard trailer
 PF:  |-----varies...
      |-----varies...
*/
  Parse Arg stem,pf1,pf2;
  Call BuildField stem,map0._rows,map0._cols,23,1,,
                  BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                  'PF:';
  Call BuildField stem,map0._rows,map0._cols,23,6,,
                  BuildAttribute(,,'PINK',),,
                  pf1;
  Call BuildField stem,map0._rows,map0._cols,24,6,,
                  BuildAttribute(,,'PINK',),,
                  pf2;
Return;

/* ----------------------------------------------------------------- */
/* MAP BUILDING ROUTINES                                             */
/* ----------------------------------------------------------------- */

BuildMap0:
/* layout of map0
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                    Library Entry Screen                     hh:mm xx


      Welcome to Dave Clark's Stand-alone Emergency System Editor.  As was just
 hinted, this tool is actually intended for those times when no other editor is
 available.   However, smaller VSE shops could certainly use this tool as their
 only editor (where each user would  require  their own dynamic partition and a
 local non-SNA 3270 device address from which to run this tool).   Please note,
 however, that there is no protection from two users editing the same member.

      This screen is only presented once  per session.   Its sole purpose is to
 obtain the VSE library name which will drive the remainder of the session.


      VSE Library name: _______





 &MSG
 PF:           3=Exit

*/
  mbuild.m0 = False;
  Call MakeStem 'pfc.',8,8,10,10,12,10,10;
  Call BuildHeader 'map0.','Library Entry Screen',,
                  BuildPFkeys('','3=Exit');

  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute(,,'GREEN',),,
                  "     Welcome to Dave Clark's Stand-alon"||,
                  'e Emergency System Editor.  As was just';
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute(,,'GREEN',),,
                  'hinted, this tool is actually intended '||,
                  'for those times when no other editor is';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  'available.   However, smaller VSE shops'||,
                  ' could certainly use this tool as their';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute(,,'GREEN',),,
                  'only editor (where each user would  req'||,
                  'uire  their own dynamic partition and a';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute(,,'GREEN',),,
                  'local non-SNA 3270 device address from '||,
                  'which to run this tool).   Please note,';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute(,,'GREEN',),,
                  'however, that there is no protection fr'||,
                  'om two users editing the same member.  ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  '     This screen is only presented once'||,
                  '  per session.   Its sole purpose is to';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute(,,'GREEN',),,
                  'obtain the VSE library name which will '||,
                  'drive the remainder of the session.    ';
  Call BuildField stem,map0._rows,map0._cols,16,6,,
                  BuildAttribute(,,'TURQUOISE',),,
                  'VSE Library name:';
  Call BuildField stem,map0._rows,map0._cols,16,24,,
                  BuildAttribute('UNPROT_NORM','YES','YELLOW','UNDERLINE');
  map0_library = n;
  Call BuildField stem,map0._rows,map0._cols,16,32,,
                  BuildAttribute();
Return;

BuildMap1:
/* layout of map1
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                    SubLibrary Selection                     hh:mm xx

 Sublibrary: ________   else, select by cursor from the list below.

 ListDir Library=&LIB

   SUBLIBRARY CREATION   SPACE     NO. OF      USED  DELAYED    % LIBR.
              DATE       REUSAGE   MEMBERS     LB'S  LB'S       SPACE
   --------------------------------------------------------------------
 _ BSITEMP    2003-09-30   IMMED         0        1        0       0 %
 _ CICS       1997-08-23   AUTO          1        2        0       0 %
 _ JOBS       1997-08-23   AUTO         51      314        0       0 %
 _ PROD       1997-08-23   AUTO       8094   139478      281      84 %
 _ SAVE       1997-08-23   AUTO          7       66        0       0 %
 _ TCPIP      1999-11-19   AUTO         11       93        0       0 %
 _ TEST       1997-08-23   AUTO          6      116        0       0 %
 _ *** End of List ***
 _
 _
 _
 &MSG
 PF:  1=Help   3=Exit              6=Top      7=Backward              12=Refresh
                        5=Find     9=Bottom   8=Forward
*/
  Drop map1. map1_fnm.;
  map1_fnm.  = '';
  map1_fnm.0 = 0;
  map1_hdc = 10;
  map1_cnt = 11;
  Call BuildHeader 'map1.','SubLibrary Selection',,
                  BuildPFkeys('1=Help',,
                              '3=Exit',,
                              '',,
                              '6=Top',,
                              '7=Backward',,
                              '',,
                              '12=Refresh'),,
                  BuildPFkeys('',,
                              '',,
                              '5=Find',,
                              '9=Bottom',,
                              '8=Forward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute(,,'TURQUOISE',),,
                  'Sublibrary:';
  Call BuildField stem,map0._rows,map0._cols,4,13,,
                  BuildAttribute('UNPROT_NORM','YES','YELLOW','UNDERLINE');
  map1_sublib = n;
  Call BuildField stem,map0._rows,map0._cols,4,22,,
                  BuildAttribute();
  Call BuildField stem,map0._rows,map0._cols,4,24,,
                  BuildAttribute(,,'BLUE',),,
                  'else, select by cursor from the list below.';

  dlbl = map0.map0_library.input;
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'ListDir Library='           ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_RED           ||,
                  dlbl                      ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT         ;

  Call BuildMap1Detail;

  Call BuildField stem,map0._rows,map0._cols,8,3,,
                  BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                  map1_hdr.1;
  Call BuildField stem,map0._rows,map0._cols,9,3,,
                  BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                  map1_hdr.2;
  Call BuildField stem,map0._rows,map0._cols,10,3,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  map1_hdr.3;
Return;

BuildMap1Body:
  If map1_fnm.0 > 0 Then Do
    Do i = map1_fnm.0 to 1 by -1
      stem = 'map1.'map1_fnm.i'.attrib',
             'map1.'map1_fnm.i'.output',
             'map1.'map1_fnm.i'.input';
      Drop (stem);
    End
  End
  Drop map1_fnm.;

  map1_fnm.  = '';
  map1_fnm.0 = 0;
  s = 1;
  stem = 'map1.';
  map1_save_ptr = map1_ptr;

  Do i = map1_ptr to map1_dtl.0
    If s = 1 & i = 0 Then
      Call BuildField stem,map0._rows,map0._cols,map1_hdc+s,3,,
                      BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                      '*** Top of List ***';
    Else
    Call BuildField stem,map0._rows,map0._cols,map1_hdc+s,1,,
                    BuildAttribute('UNPROT_NORM',,'TURQUOISE','UNDERLINE'),,
                    '00'x                              ||,
                    RX3270_DSO.LIT_START_FIELD_EXTENDED||,
                    '03'x                              ||,
                    RX3270_TYP.LIT_FIELD_ATTRIBUTE     ||,
                    RX3270_ATR.LIT_ASKIP_NORM          ||,
                    RX3270_TYP.LIT_EXTENDED_COLOR      ||,
                    RX3270_EXC.LIT_GREEN               ||,
                    RX3270_TYP.LIT_EXTENDED_HILIGHT    ||,
                    RX3270_EXH.LIT_OFF                 ||,
                    map1_dtl.i                           ;
    map1_fnm.0 = s;
    map1_fnm.s = n;
    s = s + 1;
    If s > map1_cnt Then Leave;
  End
  If s <= map1_cnt Then Do
    Call BuildField stem,map0._rows,map0._cols,map1_hdc+s,3,,
                    BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                    '*** End of List ***';
    map1_fnm.0 = s;
    map1_fnm.s = n;
  End
Return;

BuildMap1Detail: Procedure Expose syslst. map1_hdr. map1_dtl. map1_ptr;
  map1_hdr.  = '';
  map1_hdr.0 = 0;
  map1_dtl.  = '';
  map1_dtl.0 = 0;
  map1_ptr   = 1;

  i = 1;
  Do 2             /* find beginning of report headers */
    Do i = i to syslst.0 While Left(syslst.i,15) <> '---------------'
    End
    i = i + 1;
  End
                   /* extract report headers */
  Do i = i to syslst.0 Until Left(syslst.i,15) == '---------------'
    n = map1_hdr.0 + 1;
    map1_hdr.n = Strip(syslst.i,'Trailing');
    map1_hdr.0 = n;
  End
  i = i + 1;       /* extract sublibrary list */
  Do i = i to syslst.0 While Left(syslst.i,12) <> 'L113I RETURN'
    str = Left(syslst.i,15);
    If str <> '               ',
    &  str <> 'SUBLIBRARY CREA',
    &  str <> '           DATE',
    &  str <> '---------------',
    Then Do
      n = map1_dtl.0 + 1;
      map1_dtl.n = Strip(syslst.i,'Trailing');
      map1_dtl.0 = n;
    End
  End
Return;

BuildMap2:
/* layout of map2
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                      Member Selection                       hh:mm xx

 Member.type: _________________   else, select by cursor from the list below.

 ListDir SubLibrary=&SUBLIB

    M E M B E R      CREATION   LAST     BYTES    LIBR CONT SVA  A- R-
   NAME     TYPE     DATE      UPDATE   RECORDS   BLKS STOR ELIG MODE
   --------------------------------------------------------------------
 _ INSERTS  A        02-06-07 02-06-07    194 R       7 YES   -   -   -
 _ FIXSOCK  JCL      02-06-10   -  -      863 R      52 YES   -   -   -
 _ APLASER  PHASE    04-01-06 04-04-09  21184 B      22  NO  NO  31 ANY
 _ BITBANG  PHASE    01-07-25 01-08-15    152 B       1 YES  NO  24  24
 _ DAPFXFER PHASE    00-04-17 00-04-18  19440 B      20 YES  NO  31 ANY
 _ PRPCDRDB PHASE    00-04-14   -  -    10176 B      11  NO  NO  24  24
 _ *** End of List ***
 _
 _
 _
 _
 &MSG
 PF:  1=Help   3=End               6=Top      7=Backward              12=Refresh
                        5=Find     9=Bottom   8=Forward
*/
  Drop map2. map2_fnm.;
  map2_fnm.  = '';
  map2_fnm.0 = 0;
  map2_hdc = 10;
  map2_cnt = 11;
  Call BuildHeader 'map2.','Member Selection',,
                  BuildPFkeys('1=Help',,
                              '3=End',,
                              '',,
                              '6=Top',,
                              '7=Backward',,
                              '',,
                              '12=Refresh'),,
                  BuildPFkeys('',,
                              '',,
                              '5=Find',,
                              '9=Bottom',,
                              '8=Forward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute(,,'TURQUOISE',),,
                  'Member.type:';
  Call BuildField stem,map0._rows,map0._cols,4,14,,
                  BuildAttribute('UNPROT_NORM','YES','YELLOW','UNDERLINE');
  map2_member = n;
  Call BuildField stem,map0._rows,map0._cols,4,32,,
                  BuildAttribute();
  Call BuildField stem,map0._rows,map0._cols,4,34,,
                  BuildAttribute(,,'BLUE',),,
                  'else, select by cursor from the list below.';

  lib = dlbl'.'map1.map1_sublib.input;
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'ListDir SubLibrary='        ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_RED           ||,
                  lib                      ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT         ;

  Call BuildMap2Detail;

  Call BuildField stem,map0._rows,map0._cols,8,3,,
                  BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                  map2_hdr.1;
  Call BuildField stem,map0._rows,map0._cols,9,3,,
                  BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                  map2_hdr.2;
  Call BuildField stem,map0._rows,map0._cols,10,3,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  map2_hdr.3;
Return;

BuildMap2Body:
  If map2_fnm.0 > 0 Then Do
    Do i = map2_fnm.0 to 1 by -1
      stem = 'map2.'map2_fnm.i'.attrib',
             'map2.'map2_fnm.i'.output',
             'map2.'map2_fnm.i'.input';
      Drop (stem);
    End
  End
  Drop map2_fnm.;

  map2_fnm.  = '';
  map2_fnm.0 = 0;
  s = 1;
  stem = 'map2.';
  map2_save_ptr = map2_ptr;

  Do i = map2_ptr to map2_dtl.0
    If s = 1 & i = 0 Then
      Call BuildField stem,map0._rows,map0._cols,map2_hdc+s,3,,
                      BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                      '*** Top of List ***';
    Else
    Call BuildField stem,map0._rows,map0._cols,map2_hdc+s,1,,
                    BuildAttribute('UNPROT_NORM',,'TURQUOISE','UNDERLINE'),,
                    '00'x                              ||,
                    RX3270_DSO.LIT_START_FIELD_EXTENDED||,
                    '03'x                              ||,
                    RX3270_TYP.LIT_FIELD_ATTRIBUTE     ||,
                    RX3270_ATR.LIT_ASKIP_NORM          ||,
                    RX3270_TYP.LIT_EXTENDED_COLOR      ||,
                    RX3270_EXC.LIT_GREEN               ||,
                    RX3270_TYP.LIT_EXTENDED_HILIGHT    ||,
                    RX3270_EXH.LIT_OFF                 ||,
                    map2_dtl.i                           ;
    map2_fnm.0 = s;
    map2_fnm.s = n;
    s = s + 1;
    If s > map2_cnt Then Leave;
  End
  If s <= map2_cnt Then Do
    Call BuildField stem,map0._rows,map0._cols,map2_hdc+s,3,,
                    BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                    '*** End of List ***';
    map2_fnm.0 = s;
    map2_fnm.s = n;
  End
Return;

BuildMap2Detail: Procedure Expose syslst. map2_hdr. map2_dtl. map2_ptr;
  map2_hdr.  = '';
  map2_hdr.0 = 0;
  map2_dtl.  = '';
  map2_dtl.0 = 0;
  map2_ptr   = 1;

  i = 1;
  Do 4             /* find beginning of report headers */
    Do i = i to syslst.0 While Left(syslst.i,15) <> '---------------'
    End
    i = i + 1;
  End
                   /* extract report headers */
  Do i = i to syslst.0 Until Left(syslst.i,15) == '---------------'
    n = map2_hdr.0 + 1;
    map2_hdr.n = Strip(syslst.i,'Trailing');
    map2_hdr.0 = n;
  End
  i = i + 1;       /* extract sublibrary list */
  Do i = i to syslst.0 While Left(syslst.i,12) <> 'L113I RETURN'
    str = Left(syslst.i,15);
    If str <> 'DIRECTORY DISPL',
    &  str <> '               ',
    &  str <> ' M E M B E R   ',
    &  str <> 'NAME     TYPE  ',
    &  str <> '---------------',
    Then Do
      Parse Var syslst.i . t . 44 m 45 .;
      If t <> 'OBJ' &  m == 'R' Then Do
        n = map2_dtl.0 + 1;
        map2_dtl.n = Strip(syslst.i,'Trailing');
        map2_dtl.0 = n;
      End
    End
  End
Return;

BuildMap3:
/* layout of map3
 SYSEDIT: &MBR                               Line nnnnnnn of nnnnnnn mm/dd/yyyy
 ====> ____________________________________________________________    hh:mm xx
 U=&CU ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       *** Top of Member ***
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
 *===*
       *** End of Member ***
 MIXED ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 PF:  1=Help   3=Save   4=Change   6=Top      7=Backward   10=Left    12=Cancel
      2=Recall          5=Find     9=Bottom   8=Forward    11=Right
*/
  Drop map3. map3_fnm.;
  map3_fnm.  = '';
  map3_fnm.0 = 0;
  map3_hdc = 3;
  map3_cnt = 18;
  map3_ptr = 1;

  stem = 'map3.';
/*Interpret stem" =''";*/
  Interpret stem"_lun = D2C(luno)";
  Interpret stem"_wcc = RX3270_WCC.LIT_FREEKB_FRSET";
  Interpret stem"_qry   = map0._qry";
  Interpret stem"_dev   = map0._dev";
  Interpret stem"_bufsz = map0._bufsz";
  Interpret stem"_bufsa = map0._bufsa";
  Interpret stem"_rows  = map0._rows";
  Interpret stem"_cols  = map0._cols";
  Interpret stem"_rowa  = map0._rowa";
  Interpret stem"_cola  = map0._cola";
  Interpret stem"_exc   = map0._exc";
  Interpret stem"_exh   = map0._exh";

  source_ruler = '----+----1----+----2----+----3----+----4'||,
                 '----+----5----+----6----+----7----+----8';

  Call BuildField 'map3.',map0._rows,map0._cols,1,1,,
                  BuildAttribute(,,'BLUE',),,
                  'SYSEDIT:';
  mbr = map2.map2_member.input;
  Call BuildField 'map3.',map0._rows,map0._cols,1,10,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  lib'.'mbr;
  Call BuildField 'map3.',map0._rows,map0._cols,1,45,,
                  BuildAttribute(,,'TURQUOISE',),,
                  'Line';
  Call BuildField 'map3.',map0._rows,map0._cols,1,50,,
                  BuildAttribute(,,'GREEN',),,
                  '0';
  map3_curl = n;
  Call BuildField 'map3.',map0._rows,map0._cols,1,58,,
                  BuildAttribute(,,'TURQUOISE',),,
                  'of';
  Call BuildField 'map3.',map0._rows,map0._cols,1,61,,
                  BuildAttribute(,,'GREEN',),,
                  '0';
  map3_maxl = n;
  Call BuildField stem,map0._rows,map0._cols,1,69,,
                  BuildAttribute(,,'BLUE',),,
                  'mm/dd/yyyy';
  map3_date = n;
  Call BuildField stem,map0._rows,map0._cols,2,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '====>';
  Call BuildField stem,map0._rows,map0._cols,2,7,,
                  BuildAttribute('UNPROT_NORM','YES','YELLOW','UNDERLINE');
  map3_command = n;
  Call BuildField stem,map0._rows,map0._cols,2,68,,
                  BuildAttribute();
  Call BuildField stem,map0._rows,map0._cols,2,71,,
                  BuildAttribute(,,'BLUE',),,
                  'hh:mm xx';
  map3_time = n;
  Call BuildField stem,map0._rows,map0._cols,3,1,,
                  BuildAttribute(,,'BLUE',),,
                  'U='||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  cuu                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT         ;
  Call BuildField stem,map0._rows,map0._cols,3,7,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  SubStr(source_ruler,map3_view,72);
  map3_Truler = n;
  Call BuildField stem,map0._rows,map0._cols,22,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'MIXED';
  map3_case = n;
  Call BuildField stem,map0._rows,map0._cols,22,7,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  SubStr(source_ruler,map3_view,72);
  map3_Bruler = n;
  Call BuildField stem,map0._rows,map0._cols,22,80,,
                  BuildAttribute();
  Call BuildTrailer stem,,
                  BuildPFkeys('1=Help',,
                              '3=Save',,
                              '4=Change',,
                              '6=Top',,
                              '7=Backward',,
                              '10=Left',,
                              '12=Cancel'),,
                  BuildPFkeys('2=Recall',,
                              '',,
                              '5=Find',,
                              '9=Bottom',,
                              '8=Forward',,
                              '11=Right');
Return;

BuildMap3Body:
  If map3_fnm.0 > 0 Then Do
    Do i = map3_fnm.0 to 1 by -1
      Parse Var map3_fnm.i n1 n2 .;
      If n1 == 'x' Then
        stem = '';
      Else Do
        stem = 'map3.'n1'.attrib',
               'map3.'n1'.output',
               'map3.'n1'.input';
      End
      stem = stem 'map3.'n2'.attrib',
                  'map3.'n2'.output',
                  'map3.'n2'.input';
      Drop (stem);
    End
  End
  Drop map3_fnm.;

  map3_fnm.  = '';
  map3_fnm.0 = 0;
  s = 1;
  stem = 'map3.';
  map3_save_ptr = map3_ptr;

  Do i = map3_ptr to rec.0
    If s = 1 & i = 0 Then Do
      map3_fnm.s = 'x';
      Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,7,,
                      BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                      '*** Top of Member ***';
    End
    Else Do
      Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,1,,
                      BuildAttribute('UNPROT_NORM',,'YELLOW',),,
                      IFF(lca.i>'',lca.i,'*===*');
      map3_fnm.s = n;
      Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,7,,
                      BuildAttribute('UNPROT_NORM',,'GREEN',),,
                      SubStr(rec.i,map3_view,72);
    End
    map3_fnm.0 = s;
    map3_fnm.s = map3_fnm.s n;
    s = s + 1;
    If s > map3_cnt Then Leave;
  End
  If s <= map3_cnt Then Do
    map3_fnm.s = 'x';
    Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,7,,
                    BuildAttribute('ASKIP_BRT',,'TURQUOISE',),,
                    '*** End of Member ***';
    map3_fnm.0 = s;
    map3_fnm.s = map3_fnm.s n;
  End
  Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,80,,
                  BuildAttribute();
Return;

BuildHelp1a:
/* layout of help1a
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                    Sublibrary Selection                     hh:mm xx

 To obtain a member listing, enter a     The following operations are available
 valid sublibrary name (in the input     from the Sublibrary Selection screen:
 field provided for that purpose) and
 press ENTER.  Alternatively, you may    S   select the indicated sublibrary
 use the cursor to select from the           and display its member contents.
 sublibrary listing shown.                   Cursor selection performs this
                                             operation by default.
 For the purposes of creating a new
 sublibrary, enter a valid (but non-     D   delete the indicated sublibrary
 existent) name in the Sublibrary            (a verification prompt is issued).
 input field and press ENTER.  This          This also deletes any members
 application then issues a verifica-         currently in that sublibrary.
 tion prompt to allow you the
 opportunity to agree or disagree to
 the definition of a new sublibrary.




 PF:           3=End
                                              8=Forward
*/
  mbuild.h1a = False;
  Call BuildHeader 'help1a.','Sublibrary Selection',,
                  BuildPFkeys('1=Help',,
                              '3=End'),,
                  BuildPFkeys('',,
                              '',,
                              '',,
                              '',,
                              '8=Forward');
  /* column 1 */
  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute(,,'GREEN',),,
                  'To obtain a member listing, enter a    ';
  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute(,,'GREEN',),,
                  'valid sublibrary name (in the input    ';
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute(,,'GREEN',),,
                  'field provided for that purpose) and   ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  'press ENTER.  Alternatively, you may   ';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute(,,'GREEN',),,
                  'use the cursor to select from the      ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute(,,'GREEN',),,
                  'sublibrary listing shown.              ';
  Call BuildField stem,map0._rows,map0._cols,11,1,,
                  BuildAttribute(,,'GREEN',),,
                  'For the purposes of creating a new     ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  'sublibrary, enter a valid (but non-    ';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute(,,'GREEN',),,
                  'existent) name in the Sublibrary       ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute(,,'GREEN',),,
                  'input field and press ENTER.  This     ';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute(,,'GREEN',),,
                  'application then issues a verifica-    ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  'tion prompt to allow you the           ';
  Call BuildField stem,map0._rows,map0._cols,17,1,,
                  BuildAttribute(,,'GREEN',),,
                  'opportunity to agree or disagree to    ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute(,,'GREEN',),,
                  'the definition of a new sublibrary.    ';
  /* column 2 */
  Call BuildField stem,map0._rows,map0._cols,4,41,,
                  BuildAttribute(,,'GREEN',),,
                  'The following operations are available ';
  Call BuildField stem,map0._rows,map0._cols,5,41,,
                  BuildAttribute(,,'GREEN',),,
                  'from the Sublibrary Selection screen:  ';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'S';
  Call BuildField stem,map0._rows,map0._cols,7,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  select the indicated sublibrary    ';
  Call BuildField stem,map0._rows,map0._cols,8,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    and display its member contents.   ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    Cursor selection performs this     ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    operation by default.              ';
  Call BuildField stem,map0._rows,map0._cols,12,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'D';
  Call BuildField stem,map0._rows,map0._cols,12,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  delete the indicated sublibrary    ';
  Call BuildField stem,map0._rows,map0._cols,13,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    (a verification prompt is issued). ';
  Call BuildField stem,map0._rows,map0._cols,14,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    This also deletes any members      ';
  Call BuildField stem,map0._rows,map0._cols,15,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    currently in that sublibrary.      ';
Return;

BuildHelp1b:
/* layout of help1b
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                    Sublibrary Selection                     hh:mm xx

 Program Function Keys:
 1=Help displays these Help panels.      12=Refresh the sublibrary listing.
                                            In other words, though the presence
 3=Exit completely from this                of new and deleted sublibraries
    application.                            (created or deleted from that
                                            screen) are immediately reflected
 5=Find a sublibrary based on the full      in the listing, sublibrary
    or partial information entered in       statistics are not re-accessed for
    the Sublibrary input field.             display until this key is pressed.

 6=Top
 9=Bottom displays the first or last
    page of the sublibrary listing.

 7=Forward
 8=Backward scrolls forward or backward
    through the pages of the sublibrary
    listing.

 PF:           3=End                          7=Backward

*/
  mbuild.h1b = False;
  Call BuildHeader 'help1b.','Sublibrary Selection',,
                  BuildPFkeys('',,
                              '3=End',,
                              '',,
                              '',,
                              '7=Backward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Program Function Keys:';
  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'1=Help';
  Call BuildField stem,map0._rows,map0._cols,5,8,,
                  BuildAttribute(,,'GREEN',),,
                  'displays these Help panels.     ';
  Call BuildField stem,map0._rows,map0._cols,5,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '12=Refresh';
  Call BuildField stem,map0._rows,map0._cols,5,52,,
                  BuildAttribute(,,'GREEN',),,
                  'the sublibrary listing.     ';
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   In other words, though the presence ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '3=Exit';
  Call BuildField stem,map0._rows,map0._cols,7,8,,
                  BuildAttribute(,,'GREEN',),,
                  'completely from this            ',
                  '   of new and deleted sublibraries     ';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   application.                        ',
                  '   (created or deleted from that       ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   screen) are immediately reflected   ';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '5=Find';
  Call BuildField stem,map0._rows,map0._cols,10,8,,
                  BuildAttribute(,,'GREEN',),,
                  'a sublibrary based on the full  ',
                  '   in the listing, sublibrary          ';
  Call BuildField stem,map0._rows,map0._cols,11,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   or partial information entered in   ',
                  '   statistics are not re-accessed for  ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   the Sublibrary input field.         ',
                  '   display until this key is pressed.  ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '6=Top';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '9=Bottom';
  Call BuildField stem,map0._rows,map0._cols,15,10,,
                  BuildAttribute(,,'GREEN',),,
                  'displays the first or last    ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   page of the sublibrary listing.     ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '7=Forward';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '8=Backward';
  Call BuildField stem,map0._rows,map0._cols,19,12,,
                  BuildAttribute(,,'GREEN',),,
                  'scrolls forward or backward ';
  Call BuildField stem,map0._rows,map0._cols,20,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   through the pages of the sublibrary ';
  Call BuildField stem,map0._rows,map0._cols,21,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   listing.                            ';
Return;

BuildHelp2a:
/* layout of help2a
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                      Member Selection                       hh:mm xx

 To alter a member's source, enter a     The following operations are available
 valid member.type name (in the input    from the Member Selection screen:
 field provided for that purpose) and
 press ENTER.  Alternatively, you may    S   select the indicated member and
 use the cursor to select from the           display its contents for update.
 member listing shown.                       Cursor selection performs this
                                             operation by default.
 For the purposes of creating a new
 member, enter a valid (but non-         D   delete the indicated member
 existent) name and type into the            (a verification prompt is issued).
 Member.type input field and press
 ENTER.  This application then issues    C   copy or
 a verification prompt to allow you      M   move the indicated member to a
 the opportunity to agree or disagree        different sublibrary within the
 to the definition of a new member.          same library.

                                         R   rename the indicated member
                                             within the same sublibrary.

 PF:           3=End
                                              8=Forward
*/
  mbuild.h2a = False;
  Call BuildHeader 'help2a.','Member Selection',,
                  BuildPFkeys('1=Help',,
                              '3=End'),,
                  BuildPFkeys('',,
                              '',,
                              '',,
                              '',,
                              '8=Forward');
  /* column 1 */
  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute(,,'GREEN',),,
                  "To alter a member's source, enter a    ",
  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute(,,'GREEN',),,
                  'valid member.type name (in the input   ';
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute(,,'GREEN',),,
                  'field provided for that purpose) and   ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  'press ENTER.  Alternatively, you may   ';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute(,,'GREEN',),,
                  'use the cursor to select from the      ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute(,,'GREEN',),,
                  'member listing shown.                  ';
  Call BuildField stem,map0._rows,map0._cols,11,1,,
                  BuildAttribute(,,'GREEN',),,
                  'For the purposes of creating a new     ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  'member, enter a valid (but non-        ';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute(,,'GREEN',),,
                  'existent) name and type into the       ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute(,,'GREEN',),,
                  'Member.type input field and press      ';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute(,,'GREEN',),,
                  'ENTER.  This application then issues   ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  'a verification prompt to allow you     ';
  Call BuildField stem,map0._rows,map0._cols,17,1,,
                  BuildAttribute(,,'GREEN',),,
                  'the opportunity to agree or disagree   ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute(,,'GREEN',),,
                  'to the definition of a new member.     ';
  /* column 2 */
  Call BuildField stem,map0._rows,map0._cols,4,41,,
                  BuildAttribute(,,'GREEN',),,
                  'The following operations are available ';
  Call BuildField stem,map0._rows,map0._cols,5,41,,
                  BuildAttribute(,,'GREEN',),,
                  'from the Member Selection screen:      ';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'S';
  Call BuildField stem,map0._rows,map0._cols,7,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  select the indicated member and    ';
  Call BuildField stem,map0._rows,map0._cols,8,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    display its contents for update.   ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    Cursor selection performs this     ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    operation by default.              ';
  Call BuildField stem,map0._rows,map0._cols,12,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'D';
  Call BuildField stem,map0._rows,map0._cols,12,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  delete the indicated member        ';
  Call BuildField stem,map0._rows,map0._cols,13,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    (a verification prompt is issued). ';
  Call BuildField stem,map0._rows,map0._cols,15,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'C';
  Call BuildField stem,map0._rows,map0._cols,15,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  copy or                            ';
  Call BuildField stem,map0._rows,map0._cols,16,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'M';
  Call BuildField stem,map0._rows,map0._cols,16,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  move the indicated member to a     ';
  Call BuildField stem,map0._rows,map0._cols,17,1,,
                  BuildAttribute(,,'GREEN',),,
                  '    different sublibrary within the    ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute(,,'GREEN',),,
                  '    same library.                      ';
  Call BuildField stem,map0._rows,map0._cols,20,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'R';
  Call BuildField stem,map0._rows,map0._cols,20,43,,
                  BuildAttribute(,,'GREEN',),,
                  '  rename the indicated member        ';
  Call BuildField stem,map0._rows,map0._cols,21,41,,
                  BuildAttribute(,,'GREEN',),,
                  '    within the same sublibrary.        ';
Return;

BuildHelp2b:
/* layout of help2b
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                      Member Selection                       hh:mm xx

 Program Function Keys:
 1=Help displays these Help panels.      12=Refresh the member listing.
                                            In other words, though the presence
 3=End the member listing and go back       of new and deleted members
    to the sublibrary listing.              (created or deleted from that
                                            screen) are immediately reflected
 5=Find a member or type based upon the     in the listing, member statistics
    full or partial information entered     are not re-accessed for display
    in the Member.type input field.         until this key is pressed.

 6=Top
 9=Bottom displays the first or last
    page of the member listing.

 7=Forward
 8=Backward scrolls forward or backward
    through the pages of the member
    listing.

 PF:           3=End                          7=Backward

*/
  mbuild.h2b = False;
  Call BuildHeader 'help2b.','Member Selection',,
                  BuildPFkeys('',,
                              '3=End',,
                              '',,
                              '',,
                              '7=Backward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Program Function Keys:';
  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'1=Help';
  Call BuildField stem,map0._rows,map0._cols,5,8,,
                  BuildAttribute(,,'GREEN',),,
                  'displays these Help panels.     ';
  Call BuildField stem,map0._rows,map0._cols,5,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '12=Refresh';
  Call BuildField stem,map0._rows,map0._cols,5,52,,
                  BuildAttribute(,,'GREEN',),,
                  'the member listing.         ';
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   In other words, though the presence ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '3=End';
  Call BuildField stem,map0._rows,map0._cols,7,7,,
                  BuildAttribute(,,'GREEN',),,
                  'the member listing and go back   ',
                  '   of new and deleted members          ';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   to the sublibrary listing.          ',
                  '   (created or deleted from that       ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   screen) are immediately reflected   ';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '5=Find';
  Call BuildField stem,map0._rows,map0._cols,10,8,,
                  BuildAttribute(,,'GREEN',),,
                  'a member or type based upon the ',
                  '   in the listing, member statistics   ';
  Call BuildField stem,map0._rows,map0._cols,11,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   full or partial information entered ',
                  '   are not re-accessed for display     ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   in the Member.type input field.     ',
                  '   until this key is pressed.          ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '6=Top';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '9=Bottom';
  Call BuildField stem,map0._rows,map0._cols,15,10,,
                  BuildAttribute(,,'GREEN',),,
                  'displays the first or last    ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   page of the member listing.         ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '7=Forward';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '8=Backward';
  Call BuildField stem,map0._rows,map0._cols,19,12,,
                  BuildAttribute(,,'GREEN',),,
                  'scrolls forward or backward ';
  Call BuildField stem,map0._rows,map0._cols,20,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   through the pages of the member     ';
  Call BuildField stem,map0._rows,map0._cols,21,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   listing.                            ';
Return;

BuildHelp3a:
/* layout of help3a
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                       Member Update                         hh:mm xx

 Shortcuts: (entered in the command area at the left of the source lines)

 I(n) adds (inserts) one or more blank   L(n),LL
    lines after this line.               U(n),UU translates one or more lines
 D(n),DD removes this line (plus n-1        to lowercase or to uppercase.
    more) from the member.
                                         "(n) duplicates the indicated line one
 C(n),CC places this line (plus n-1         or more times.
    more) onto the FIFO stack.
 M(n),MM combines the C and D shortcuts  >(n),>>(n) shifts the content of the
    to place this line (plus n-1 more)      indicated line(s) by "n" number of
    onto the FIFO stack and remove          columns to the right.
    them at the same time.               <(n),<<(n) shifts the content of the
                                            indicated line(s) by "n" number of
 A(n) inserts all, or "n" number of,        columns to the left.
    stack lines after this line.
 B(n) inserts all, or "n" number of,     Note that the above two commands may
    stack lines before this line.        cause truncation of line content.

 PF:           3=End
                                              8=Forward
*/
  mbuild.h3a = False;
  Call BuildHeader 'help3a.','Member Update',,
                  BuildPFkeys('',,
                              '3=End'),,
                  BuildPFkeys('',,
                              '',,
                              '',,
                              '',,
                              '8=Forward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Shortcuts:';
  Call BuildField stem,map0._rows,map0._cols,4,12,,
                  BuildAttribute(,,'TURQUOISE',),,
        '(entered in the command area at the left of the source lines)';
  /* column 1 */
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'I(n)';
  Call BuildField stem,map0._rows,map0._cols,6,6,,
                  BuildAttribute(,,'GREEN',),,
                  'adds (inserts) one or more blank  ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   lines after this line.              ';
  Call BuildField stem,map0._rows,map0._cols,8,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'D(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  'DD';
  Call BuildField stem,map0._rows,map0._cols,8,9,,
                  BuildAttribute(,,'GREEN',),,
                  'removes this line (plus n-1    ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   more) from the member.              ';
  Call BuildField stem,map0._rows,map0._cols,11,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'C(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  'CC';
  Call BuildField stem,map0._rows,map0._cols,11,9,,
                  BuildAttribute(,,'GREEN',),,
                  'places this line (plus n-1     ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   more) onto the FIFO stack.          ';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'M(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  'MM';
  Call BuildField stem,map0._rows,map0._cols,13,9,,
                  BuildAttribute(,,'GREEN',),,
                  'combines the C and D shortcuts ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   to place this line (plus n-1 more)  ';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   onto the FIFO stack and remove      ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   them at the same time.              ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'A(n)';
  Call BuildField stem,map0._rows,map0._cols,18,6,,
                  BuildAttribute(,,'GREEN',),,
                  'inserts all, or "n" number of,    ';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   stack lines after this line.        ';
  Call BuildField stem,map0._rows,map0._cols,20,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'B(n)';
  Call BuildField stem,map0._rows,map0._cols,20,6,,
                  BuildAttribute(,,'GREEN',),,
                  'inserts all, or "n" number of,    ';
  Call BuildField stem,map0._rows,map0._cols,21,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   stack lines before this line.       ';
  /* column 2 */
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'L(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  'LL';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'U(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  'UU';
  Call BuildField stem,map0._rows,map0._cols,7,49,,
                  BuildAttribute(,,'GREEN',),,
                  'translates one or more lines';
  Call BuildField stem,map0._rows,map0._cols,8,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   to lowercase or to uppercase.       ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '"(n)';
  Call BuildField stem,map0._rows,map0._cols,10,46,,
                  BuildAttribute(,,'GREEN',),,
                  'duplicates the indicated line     ';
  Call BuildField stem,map0._rows,map0._cols,11,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   one or more times.                  ';
  Call BuildField stem,map0._rows,map0._cols,13,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '>(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  '>>(n)';
  Call BuildField stem,map0._rows,map0._cols,13,52,,
                  BuildAttribute(,,'GREEN',),,
                  'shifts the content of the   ';
  Call BuildField stem,map0._rows,map0._cols,14,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   indicated line(s) by "n" number of  ';
  Call BuildField stem,map0._rows,map0._cols,15,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   columns to the right.               ';
  Call BuildField stem,map0._rows,map0._cols,16,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '<(n)'                       ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_GREEN         ||,
                  ','                          ||,
                  RX3270_DSO.LIT_SET_ATTRIBUTE ||,
                  RX3270_TYP.LIT_EXTENDED_COLOR||,
                  RX3270_EXC.LIT_DEFAULT       ||,
                  '<<(n)';
  Call BuildField stem,map0._rows,map0._cols,16,52,,
                  BuildAttribute(,,'GREEN',),,
                  'shifts the content of the   ';
  Call BuildField stem,map0._rows,map0._cols,17,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   indicated line(s) by "n" number of  ';
  Call BuildField stem,map0._rows,map0._cols,18,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   columns to the left.                ';
  Call BuildField stem,map0._rows,map0._cols,20,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Note';
  Call BuildField stem,map0._rows,map0._cols,20,46,,
                  BuildAttribute(,,'GREEN',),,
                  'that the above two commands may';
  Call BuildField stem,map0._rows,map0._cols,21,41,,
                  BuildAttribute(,,'GREEN',),,
                  'cause truncation of line content.      ';
Return;

BuildHelp3b:
/* layout of help3b
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                       Member Update                         hh:mm xx

 Commands: (entered into the command field above the source lines)

 Function key text may be entered as     Change src tgt changes the matching
    commands (see last page of help).       string on the current line to the
                                            second string specified.  Both
 CASE {Mixed|Upper} toggles translation     strings are saved for use with the
    of changed member lines.                PF4 and PF5 keys.

 Up (n) moves current line pointer up    Insert str before current line content
    by one or more lines.                   or before position of last Find.

 DOwn (n) moves current line pointer     Delete erases the last Find string
    down by one or more lines.              moving other content to the left.

 Find src searches for the string from   Overlay str clears current line and
    the current line forward.  String       fills with the specified string.
    is saved for use with the PF5 key.


 PF:           3=End                          7=Backward
                                              8=Forward
*/
  mbuild.h3b = False;
  Call BuildHeader 'help3b.','Member Update',,
                  BuildPFkeys('',,
                              '3=End',,
                              '',,
                              '',,
                              '7=Backward'),,
                  BuildPFkeys('',,
                              '',,
                              '',,
                              '',,
                              '8=Forward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Commands:';
  Call BuildField stem,map0._rows,map0._cols,4,11,,
                  BuildAttribute(,,'TURQUOISE',),,
              '(entered into the command field above the source lines)';
  /* column 1 */
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'Function key text';
  Call BuildField stem,map0._rows,map0._cols,6,19,,
                  BuildAttribute(,,'GREEN',),,
                  'may be entered as';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   commands (see last page of help).   ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'CASE {Mixed|Upper}';
  Call BuildField stem,map0._rows,map0._cols,9,20,,
                  BuildAttribute(,,'GREEN',),,
                  'toggles translation ';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   of changed member lines.            ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Up (n)';
  Call BuildField stem,map0._rows,map0._cols,12,8,,
                  BuildAttribute(,,'GREEN',),,
                  'moves current line pointer up';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   by one or more lines.               ';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'DOwn (n)';
  Call BuildField stem,map0._rows,map0._cols,15,10,,
                  BuildAttribute(,,'GREEN',),,
                  'moves current line pointer';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   down by one or more lines.          ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Find src';
  Call BuildField stem,map0._rows,map0._cols,18,10,,
                  BuildAttribute(,,'GREEN',),,
                  'searches for the string from';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   the current line forward.  String   ';
  Call BuildField stem,map0._rows,map0._cols,20,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   is saved for use with the PF5 key.  ';
  /* column 2 */
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'CHange src tgt';
  Call BuildField stem,map0._rows,map0._cols,6,56,,
                  BuildAttribute(,,'GREEN',),,
                  'changes the matching';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   string on the current line to the   ';
  Call BuildField stem,map0._rows,map0._cols,8,41,,
                  BuildAttribute('ASKIP_BRT',,'GREEN',),,
                  '   second string specified.  Both      ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   strings are saved for use with the  ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   PF4 and PF5 keys.                   ';
  Call BuildField stem,map0._rows,map0._cols,12,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Insert str';
  Call BuildField stem,map0._rows,map0._cols,12,52,,
                  BuildAttribute(,,'GREEN',),,
                  'before current line content';
  Call BuildField stem,map0._rows,map0._cols,13,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   or before position of last Find.    ';
  Call BuildField stem,map0._rows,map0._cols,15,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Delete';
  Call BuildField stem,map0._rows,map0._cols,15,48,,
                  BuildAttribute(,,'GREEN',),,
                  'erases the last Find string';
  Call BuildField stem,map0._rows,map0._cols,16,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   moving other content to the left.   ';
  Call BuildField stem,map0._rows,map0._cols,18,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Overlay str';
  Call BuildField stem,map0._rows,map0._cols,18,53,,
                  BuildAttribute(,,'GREEN',),,
                  'clears current line and';
  Call BuildField stem,map0._rows,map0._cols,19,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   the current line forward.  String   ';
Return;

BuildHelp3c:
/* layout of help3c
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                       Member Update                         hh:mm xx

 Commands: (continued)

 LAdd (str) inserts the string as a      CLear empties the stack.  Note that
    new line after the current line.        Job Control processes (as JCL) what
                                            remains in the stack on exit.
 LDrop removes the current line from
    the member.                          SUBmit places the current content into
                                            the POWER RDR queue (if POWER is
 COpy (n) places the current line (plus     active, of course).
    n-1 more) onto the FIFO stack.  An
    '*' goes to end of member.

 Paste (n) inserts one or more lines
    off the stack ('*' to end of stack)
    after the current line.  You must
    copy again in order to paste again.



 PF:           3=End                          7=Backward
                                              8=Forward
*/
  mbuild.h3c = False;
  Call BuildHeader 'help3c.','Member Update',,
                  BuildPFkeys('',,
                              '3=End',,
                              '',,
                              '',,
                              '7=Backward'),,
                  BuildPFkeys('',,
                              '',,
                              '',,
                              '',,
                              '8=Forward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Commands:';
  Call BuildField stem,map0._rows,map0._cols,4,11,,
                  BuildAttribute(,,'TURQUOISE',),,
                  '(continued)';
  /* column 1 */
  Call BuildField stem,map0._rows,map0._cols,6,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'LAdd (str)';
  Call BuildField stem,map0._rows,map0._cols,6,12,,
                  BuildAttribute(,,'GREEN',),,
                  'inserts the string as a';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   new line after the current line.    ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'LDrop';
  Call BuildField stem,map0._rows,map0._cols,9,7,,
                  BuildAttribute(,,'GREEN',),,
                  'removes the current line from';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   the member.                         ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'COpy (n)';
  Call BuildField stem,map0._rows,map0._cols,12,10,,
                  BuildAttribute(,,'GREEN',),,
                  'places the current line (plus';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   n-1 more) onto the FIFO stack.  An  ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute(,,'GREEN',),,
                  "   '*' goes to end of member.          ";
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Paste (n)';
  Call BuildField stem,map0._rows,map0._cols,16,11,,
                  BuildAttribute(,,'GREEN',),,
                  'inserts one or more lines';
  Call BuildField stem,map0._rows,map0._cols,17,1,,
                  BuildAttribute(,,'GREEN',),,
                  "   off the stack ('*' to end of stack) ";
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   after the current line.  You must   ';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   copy again in order to paste again. ';
  /* column 2 */
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'CLear';
  Call BuildField stem,map0._rows,map0._cols,6,47,,
                  BuildAttribute(,,'GREEN',),,
                  'empties the stack.  Note that';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   Job Control processes (as JCL) what ';
  Call BuildField stem,map0._rows,map0._cols,8,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   remains in the stack on exit.       ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'SUBmit';
  Call BuildField stem,map0._rows,map0._cols,10,48,,
                  BuildAttribute(,,'GREEN',),,
                  'places the current content into ';
  Call BuildField stem,map0._rows,map0._cols,11,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   the POWER RDR queue (if POWER is    ';
  Call BuildField stem,map0._rows,map0._cols,12,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   active, of course).                 ';
Return;

BuildHelp3d:
/* layout of help3d
 SYSEDIT                   Stand-alone System Editor                 mm/dd/yyyy
 CUU: &CUU                       Member Update                         hh:mm xx

 Program Function Keys:
 1=Help displays these Help panels.      6=Top
                                         9=Bottom displays the first or last
 2=Recall the last command entered.         page of the member listing.

 3=Save the updated member and go back   7=Forward
    to the member listing.               8=Backward scrolls forward or backward
                                            through the pages of the member
 4=Change                                   listing.  (cursor-sensitive)
 5=Find a string from the current line
    to the end of the member source      10=Left
    for optional replacement.  These     11=Right shift of the source lines by
    keys will substitute strings from       8 columns in either direction.
    the command line as find and            Provides update access to source
    replace strings.  If the command        lines up to 80 characters wide.
    line is blank, the last find or
    replacement string is reused to      12=Cancel any member updates and go
    repeat the find/change.                 back to the member listing.

 PF:           3=End                          7=Backward

*/
  mbuild.h3d = False;
  Call BuildHeader 'help3d.','Member Update',,
                  BuildPFkeys('',,
                              '3=End',,
                              '',,
                              '',,
                              '7=Backward');

  Call BuildField stem,map0._rows,map0._cols,4,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  'Program Function Keys:';
  Call BuildField stem,map0._rows,map0._cols,5,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  RX3270_DSO.LIT_INSERT_CURSOR'1=Help';
  Call BuildField stem,map0._rows,map0._cols,5,8,,
                  BuildAttribute(,,'GREEN',),,
                  'displays these Help panels.     ';
  Call BuildField stem,map0._rows,map0._cols,5,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '6=Top';
  Call BuildField stem,map0._rows,map0._cols,6,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '9=Bottom';
  Call BuildField stem,map0._rows,map0._cols,6,51,,
                  BuildAttribute(,,'GREEN',),,
                  'displays the first or last    ';
  Call BuildField stem,map0._rows,map0._cols,7,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '2=Recall';
  Call BuildField stem,map0._rows,map0._cols,7,10,,
                  BuildAttribute(,,'GREEN',),,
                  'the last command entered.     ';
  Call BuildField stem,map0._rows,map0._cols,7,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   page of the member listing.         ';
  Call BuildField stem,map0._rows,map0._cols,9,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '3=Save';
  Call BuildField stem,map0._rows,map0._cols,9,8,,
                  BuildAttribute(,,'GREEN',),,
                  'the updated member and go back  ';
  Call BuildField stem,map0._rows,map0._cols,9,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '7=Forward';
  Call BuildField stem,map0._rows,map0._cols,10,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   to the member listing.              ';
  Call BuildField stem,map0._rows,map0._cols,10,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '8=Backward';
  Call BuildField stem,map0._rows,map0._cols,10,52,,
                  BuildAttribute(,,'GREEN',),,
                  'scrolls forward or backward ';
  Call BuildField stem,map0._rows,map0._cols,11,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   through the pages of the member     ';
  Call BuildField stem,map0._rows,map0._cols,12,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '4=Change';
  Call BuildField stem,map0._rows,map0._cols,12,41,,
                  BuildAttribute(,,'GREEN',),,
                  '   listing.  (cursor-sensitive)        ';
  Call BuildField stem,map0._rows,map0._cols,13,1,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '5=Find';
  Call BuildField stem,map0._rows,map0._cols,13,8,,
                  BuildAttribute(,,'GREEN',),,
                  'a string from the current line  ';
  Call BuildField stem,map0._rows,map0._cols,14,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   to the end of the member source     ';
  Call BuildField stem,map0._rows,map0._cols,14,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '10=Left';
  Call BuildField stem,map0._rows,map0._cols,15,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   for optional replacement.  These    ';
  Call BuildField stem,map0._rows,map0._cols,15,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '11=Right';
  Call BuildField stem,map0._rows,map0._cols,15,51,,
                  BuildAttribute(,,'GREEN',),,
                  'shift of the source lines by  ';
  Call BuildField stem,map0._rows,map0._cols,16,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   keys will substitute strings from   ',
                  '   8 columns in either direction.      ';
  Call BuildField stem,map0._rows,map0._cols,17,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   the command line as find and        ',
                  '   Provides update access to source    ';
  Call BuildField stem,map0._rows,map0._cols,18,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   replace strings.  If the command    ',
                  '   lines up to 80 characters wide.     ';
  Call BuildField stem,map0._rows,map0._cols,19,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   line is blank, the last find or     ';
  Call BuildField stem,map0._rows,map0._cols,20,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   replacement string is reused to     ';
  Call BuildField stem,map0._rows,map0._cols,20,41,,
                  BuildAttribute('ASKIP_BRT',,'WHITE',),,
                  '12=Cancel';
  Call BuildField stem,map0._rows,map0._cols,20,51,,
                  BuildAttribute(,,'GREEN',),,
                  'any member updates and go    ';
  Call BuildField stem,map0._rows,map0._cols,21,1,,
                  BuildAttribute(,,'GREEN',),,
                  '   repeat the find/change.             ',
                  '   back to the member listing.         ';
Return;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
