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
/*      See separate member SYSEDIT.DOC for additional information.  */
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
/*                 queue.  An '*' goes to end-of-member.         */
/* Paste (n)     Get one or more new lines, off of queue ('*' to */
/*                 end-of-queue), inserting after current line.  */
/* CLear         Empty the data queue.  Note that if the queue   */
/*                 is not empty when the editor ends, then Job   */
/*                 Control will process (as JCL) what remains    */
/*                 up to the first "/&" card encountered.        */
/* SUBmit        Resets member edit back to the first line and,  */
/*                 if VSE/POWER is active, places the current    */
/*                 member content into the RDR queue.            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

Parse Source sys env pgm tokens;

False  = (1=0);
True   = (0=0);

luno   = '000';                        /* logical unit number */
cuu    = '';                           /* controller/unit address */
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
maplib  = 'DAP.DLC';                   /* library for map members */

compile = '|ASM|CAS|CBL|CCB|FOR|PLI|REX|RPG|';
nonedit = '|DTSAFE|DUMP|LIST|OBJ|PHASE|SYSDEBUG|';

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

  src = '';

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
  src   = '';
  tgt   = '';
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

  cmd = ParseCommandString(cmd_string);

/* ----------------------------------------------------------------- */
/* Process edit commands                                             */
/* ----------------------------------------------------------------- */

  Select

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
        rc = 0;
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

    When online,
       & Left(cmd,2) == 'RE'   Then      /* RECALL */
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
       & Left(cmd,4) == 'COMP' Then      /* COMPILE */
      Do
        Parse Var map2.map2_member.input m '.' t .;
        If Pos('|'Left(t,3)'|',compile) = 0 Then
          map3.map3_Bruler.output = ' Use SAVE.  Member type not compilable.';
        Else Do
          If upd_flag Then Call WriteMember;
          Drop rec.;                /* clear residual data */
          rec. = '';                /* start fresh */
          rec.0 = 0;
          save_type = t;
          map2.map2_member.input = m'.COMPJOB';
          If \ListMbr_Successful(map2.map2_member.input,lib) Then Do
            If \NewCompile(map2.map2_member.input,lib) Then Do
              map2.err_mesg.output = 'Unable to create compile member.';
              Leave;
            End
            Call RebuildMap2Detail map2.map2_member.input;
            If map2_ptr = map2_save_ptr Then Call BuildMap2Body;
          End
          Call BuildMap3;
          Call ReadMember;
          If rc <> 0 Then Leave;
          Call SetTopOfMember;
        End
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
            p = Pos(Translate(oper.1),Translate(rec.s),p);
            If p > 0 Then
              src = oper.1;
            Else Do
              Call ResetSourceString;
            End
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
  Say pgm': The Data queue is not empty.  Clear it?';
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
          If map1.map1_sublib.input > ' ' Then Do
            src = Strip(map1.map1_sublib.input);
          End
          If Length(src) = 0 Then
            map1.err_mesg.output = 'No find string entered.';
          Else Do
            i = map1_ptr + 1;
            Do i = i to map1_dtl.0,
              While Pos(src,Left(map1_dtl.i,8)) = 0
            End
            If i > map1_dtl.0 Then Do
              map1.map1_sublib.output = src;
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
          If map2.map2_member.input > ' ' Then Do
            src = Strip(map2.map2_member.input);
          End
          If Length(src) = 0 Then
            map2.err_mesg.output = 'No find string entered.';
          Else Do
            i = map2_ptr + 1;
            Do i = i to map2_dtl.0,
              While Pos(src,Left(map2_dtl.i,17)) = 0
            End
            If i > map2_dtl.0 Then Do
              map2.map2_member.output = src;
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
                Parse Var map2_dtl.i n2 n1 .;
                map2.map2_member.input = n1'.'n2;
              End
            End
            If map2.map2_member.input = '' Then Do
              map2.err_mesg.output = 'An entry is required.';
              Iterate;
            End
          End
        End
        Parse Value Translate(map2.map2_member.input,' ','.') With m t .;
        map2.map2_member.input = m'.'t;
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
            cmd_string = 'CHANGE' '"'src'"' '"'tgt'"';
          End
        End
      When map3._aid == RX3270_AID.LIT_PF5_KEY,
         | map3._aid == RX3270_AID.LIT_PF17_KEY  Then
        Do
          If map3.map3_command.input > ' ' Then
            cmd_string = 'FIND' map3.map3_command.input;
          Else Do
            cmd_string = 'FIND' '"'src'"';
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
  str = RXDMPVAR('Dump');    /* dump all variable names */
  Parse Var str resp .;
  If resp <> 'OK' Then Do
    Say ' RXDMPVAR response = 'str;
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
/* ParseCommandString function handles command string operands    */
/*                             that may optionally be delimited   */
/*                             with double or single quotes.      */
/* -------------------------------------------------------------- */
ParseCommandString: Procedure Expose oper.;
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
Return cmd;

/* ----------------------------------------------------------------- */
/* INPUT-OUTPUT ROUTINES                                             */
/* ----------------------------------------------------------------- */

Screen_IO:
  Parse Arg stem;
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
  Parse Arg mp;
  pg = 'a';
  cname = 'h'mp||pg;
  mname = 'help'mp||pg;
  Do While rc = 0
    If mbuild.cname Then Call BuildHelp mp,pg;
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
            When pg = 'd' Then pg = 'c';
            When pg = 'c' Then pg = 'b';
            When pg = 'b' Then pg = 'a';
            Otherwise
              Interpret mname".err_mesg.output = 'First help page.';";
          End
          cname = 'h'mp||pg;
          mname = 'help'mp||pg;
        End
      When aid == RX3270_AID.LIT_PF8_KEY,
         | aid == RX3270_AID.LIT_PF20_KEY  Then
        Do
          Select
            When pg = 'a'            Then pg = 'b';
            When pg = 'b' & mp = '3' Then pg = 'c';
            When pg = 'c' & mp = '3' Then pg = 'd';
            Otherwise
              Interpret mname".err_mesg.output = 'Last help page.';";
          End
          cname = 'h'mp||pg;
          mname = 'help'mp||pg;
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

ListMbr_Successful: Procedure Expose syslst. result nonedit False True;
  Parse Value Translate(Arg(1),' ','.') With m t .;
  Select
    When t = '' Then Do
      result = 'MEMTYP';
      Return False;
    End
    When Pos('|'t'|',nonedit) > 0 Then Do
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
  Parse Upper Var n1 mem '.' typ .;
  Drop rec.;                 /* clear residual data */
  rec. = '';                 /* start fresh */
  rec.0 = 1;
  rec.1 = ' ';
  opt = 'NODATA';
  If typ = 'PROC' Then Do
    Call Converse 'Press PF2 again if the "DATA" flag should be',
                  '"ON" for this member.';
    If map0._aid == RX3270_AID.LIT_PF2_KEY Then opt = 'DATA';
  End
  Signal NewMemberContinue;
NewCompile:
  Parse Arg n1,n2;
  Parse Var n1 mem '.' typ .;
  rec.0  = 15;
  rec.1  = "* $$ JOB JNM="mem",CLASS=C,DISP=D,USER='"lib"'";
  rec.2  = "* $$ LST LST=SYSLST,CLASS=Z,DISP=H";
  rec.3  = "// JOB    "mem"   COMPILE" mem;
  rec.4  = "// LIBDEF *,SEARCH=("lib",DAP.PROD),TEMP";
  rec.5  = "// LIBDEF PHASE,CATALOG=DAP.PROD,TEMP";
  rec.6  = "// OPTION CATAL,SXREF";
  rec.7  = "   PHASE  "mem",*";
  Select
    When save_type = 'ASM' Then
      Do
  rec.8  = "// EXEC   PGM=ASMA90,SIZE=(ASMA90,64K),        ";
  rec.9  = "               PARM='EX(LBX(EDECKXIT)),SZ(MAX-200K,ABOVE)'";
      End
    When save_type = 'CBL' Then
      Do
  rec.8  = "// EXEC   PGM=IGYCRCTL,SIZE=IGYCRCTL,          ";
  rec.9  = "               PARM='SD("lib"."sublib"("mem"))'";
      End
    When save_type = 'RPGII' Then
      Do
  rec.8  = "// EXEC   PGM=RPGII,SIZE=256K,                 ";
  rec.9  = "               PARM=' '                        ";
      End
    Otherwise
  rec.8  = "// EXEC   PGM=compiler,SIZE=*,                 ";
  rec.9  = "               PARM=' '                        ";
  End
  rec.8  = Left(rec.8,71)||"X";
  rec.10 = "* $$ SLI MEM="mem"."save_type",S="lib;
  rec.11 = "/* EOD";
  rec.12 = "// EXEC   PGM=LNKEDT,SIZE=256K";
  rec.13 = "/* EOD";
  rec.14 = "/& EOJ";
  rec.15 = "* $$ EOJ";
  opt    = 'NODATA';
NewMemberContinue:
  "EXECIO * DISKW" n2'.'n1 "( OPEN FINIS" opt "STEM rec. )";
  If rc <> 0 Then Return False;
  n = map2_dtl.0 + 1;
  Parse Var n1 n1 '.' n2 .;
  map2_dtl.n = Left(n2,8) Left(n1,8) '*** New Member ***';
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
  tgt = '';
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

  Do r = 1 While r <= rec.0  /* loop through member records */
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
    & (delt_begin > 0 & delt_ending = 0,
    |  copy_begin > 0 & copy_ending = 0,
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
    & (delt_begin > 0 & Pos('|'str'|','|CC|MM|LL|UU|<<|>>|') > 0,
    |  copy_begin > 0 & Pos('|'str'|','|DD|MM|LL|UU|<<|>>|') > 0,
    |  move_begin > 0 & Pos('|'str'|','|DD|CC|LL|UU|<<|>>|') > 0,
    |  lowr_begin > 0 & Pos('|'str'|','|DD|MM|CC|UU|<<|>>|') > 0,
    |  uppr_begin > 0 & Pos('|'str'|','|DD|MM|CC|LL|<<|>>|') > 0,
    |  left_begin > 0 & Pos('|'str'|','|DD|MM|CC|LL|UU|>>|') > 0,
    |  rght_begin > 0 & Pos('|'str'|','|DD|MM|CC|LL|UU|<<|') > 0),
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
          Do n = (s+1) to (s+nbr-1)
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
    Do r = 1 While r <= rec.0     /* that remove lines from queue */
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

CursorPosition: Procedure;
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
            Parse Var map2_dtl.i n2 n1 .;
            map2.map2_member.input = n1'.'n2;
          End
        End
      When map2.n.input = 'D' Then
        Do
          answer = True;
          i = map2_ptr + (row - 1);
          If i > 0 & i <= map2_dtl.0 Then Do
            Parse Var map2_dtl.i n2 n1 .;
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
            Parse Var map2_dtl.i n2 n1 .;
            Call Converse 'Enter the sublibrary name to 'oper' "'||,
                          n1'.'n2'" to...',8;
            If Length(map0._raw) > 6 Then Do
              lib2 = dlbl'.'Translate(Strip(SubStr(map0._raw,7,8)));
              If OperationFailed(oper,n1'.'n2,lib':'lib2) Then
                map2.err_mesg.output = 'The 'oper' failed.';
              Else Do
                If oper = 'copy' Then
                  map2_dtl.i = Left(n2,8) Left(n1,8) '*** Copied ***';
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
            Parse Var map2_dtl.i n2 n1 .;
            rf = n1'.'n2;
            Call Converse 'Enter the member.type name to rename "'||,
                          rf'" to...',17;
            If Length(map0._raw) > 6 Then Do
              rt = Translate(Strip(SubStr(map0._raw,7,17)));
              If OperationFailed('rename',rf':'rt,lib) Then
                map2.err_mesg.output = 'Rename failed.';
              Else Do
                Parse Var rt n1 '.' n2 .;
                map2_dtl.i = Left(n2,8) Left(n1,8) '*** Renamed ***';
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
    str = Left(typ,8)' 'Left(nam,8);
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
  fn = RX3270FN(rmax, cmax, row, col);
  If fn = -1 | fn = -2 Then Do
    Say " RX3270FN error: rc = "fn;
    Signal Dump;
  End
  Interpret stem||"fn.attrib = atr";
  If out <> '' Then Do
    q = Pos('%%%',out);
    If q > 0 Then out = Overlay('280000'x,out,q,3);
    Interpret stem||"fn.output = out";
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
  hdr_date = fn;
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
  hdr_time = fn;
  Call BuildField stem,map0._rows,map0._cols,22,1,,
                  BuildAttribute('ASKIP_BRT',,'RED',),,
                  '';
  err_mesg = fn;
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
  mbuild.m0 = False;
  mname = 'map0';
  Call MakeStem 'pfc.',8,8,10,10,12,10,10;
  Call BuildHeader mname'.','Library Entry Screen',,
                   BuildPFkeys('','3=Exit');
  Call GetMapMember mname;
Return;

BuildMap1:
  Drop map1. map1_fnm.;
  map1_fnm.  = '';
  map1_fnm.0 = 0;
  map1_hdc = 10;
  map1_cnt = 11;
  mname = 'map1';
  Call BuildHeader mname'.','SubLibrary Selection',,
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
  Call GetMapMember mname;
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
                      BuildAttribute('ASKIP_BRT',,'BLUE',),,
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
    map1_fnm.s = fn;
    s = s + 1;
    If s > map1_cnt Then Leave;
  End
  If s <= map1_cnt Then Do
    Call BuildField stem,map0._rows,map0._cols,map1_hdc+s,3,,
                    BuildAttribute('ASKIP_BRT',,'BLUE',),,
                    '*** End of List ***';
    map1_fnm.0 = s;
    map1_fnm.s = fn;
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
  Drop map2. map2_fnm.;
  map2_fnm.  = '';
  map2_fnm.0 = 0;
  map2_hdc = 10;
  map2_cnt = 11;
  mname = 'map2';
  Call BuildHeader mname'.','Member Selection',,
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
  Call GetMapMember mname;
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
                      BuildAttribute('ASKIP_BRT',,'BLUE',),,
                      '*** Top of List ***';
    Else Do
      Parse Var map2_dtl.i n2 n1 . 19 str;
      str = Left(n1,8) Left(n2,8) str;
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
                      str;
    End
    map2_fnm.0 = s;
    map2_fnm.s = fn;
    s = s + 1;
    If s > map2_cnt Then Leave;
  End
  If s <= map2_cnt Then Do
    Call BuildField stem,map0._rows,map0._cols,map2_hdc+s,3,,
                    BuildAttribute('ASKIP_BRT',,'BLUE',),,
                    '*** End of List ***';
    map2_fnm.0 = s;
    map2_fnm.s = fn;
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
      If m = 'R' & Pos('|'t'|',nonedit) = 0 Then Do
        n = map2_dtl.0 + 1;
        Parse Var syslst.i n1 n2 . 19 str;
        map2_dtl.n = Left(n2,8) Left(n1,8) str;
        map2_dtl.0 = n;
      End
    End
  End
Return;

BuildMap3:
  Drop map3. map3_fnm.;
  map3_fnm.  = '';
  map3_fnm.0 = 0;
  map3_hdc = 3;
  map3_cnt = 18;
  map3_ptr = 1;
  stem = 'map3.';
  Interpret stem"_lun   = D2C(luno)";
  Interpret stem"_wcc   = RX3270_WCC.LIT_FREEKB_FRSET";
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

  mname = 'map3';
  Call GetMapMember mname;
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
                      BuildAttribute('ASKIP_BRT',,'BLUE',),,
                      '*** Top of Member ***';
    End
    Else Do
      Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,1,,
                      BuildAttribute('UNPROT_NORM',,'YELLOW',),,
                      IFF(lca.i>'',lca.i,'*===*');
      map3_fnm.s = fn;
      Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,7,,
                      BuildAttribute('UNPROT_NORM',,'GREEN',),,
                      SubStr(rec.i,map3_view,72);
    End
    map3_fnm.0 = s;
    map3_fnm.s = map3_fnm.s fn;
    s = s + 1;
    If s > map3_cnt Then Leave;
  End
  If s <= map3_cnt Then Do
    map3_fnm.s = 'x';
    Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,7,,
                    BuildAttribute('ASKIP_BRT',,'BLUE',),,
                    '*** End of Member ***';
    map3_fnm.0 = s;
    map3_fnm.s = map3_fnm.s fn;
  End
  Call BuildField stem,map0._rows,map0._cols,map3_hdc+s,80,,
                  BuildAttribute();
Return;

BuildHelp:
  Parse Arg mp,pg;           /* get map number and page letter */
  mbuild.cname = False;      /* build help page just one time  */
  Select
    When mp = 3 Then title = 'Member Update';
    When mp = 2 Then title = 'Member Selection';
    Otherwise        title = 'Sublibrary Selection';
  End
  Select
    When pg = 'd' Then Do
      pf1 = BuildPFkeys(,'3=End',,,'7=Backward');
      pf2 = '';
    End
    When pg = 'c' Then Do
      pf1 = BuildPFkeys(,'3=End',,,'7=Backward');
      pf2 = BuildPFkeys(,,,,'8=Forward');
    End
    When pg = 'b' & mp = 3 Then Do
      pf1 = BuildPFkeys(,'3=End',,,'7=Backward');
      pf2 = BuildPFkeys(,,,,'8=Forward');
    End
    When pg = 'b' Then Do
      pf1 = BuildPFkeys(,'3=End',,,'7=Backward');
      pf2 = '';
    End
    Otherwise
      pf1 = BuildPFkeys(,'3=End');
      pf2 = BuildPFkeys(,,,,'8=Forward');
  End
  Call BuildHeader mname'.',title,pf1,pf2;
  Call GetMapMember mname;
Return;

GetMapMember:
  Parse Arg _map;
  map.  = '';
  map.0 = 0;
  "EXECIO * DISKR" maplib'.'pgm'.'_map "( OPEN FINIS STEM map. )";
  If rc <> 0 Then Do
    Say ' Error: Reading map "'maplib'.'pgm'.'_map'"; rc = 'rc;
    Exit rc;
  End
  _map = _map'.';
  Do n = 1 to map.0
    If Left(map.n,1) = '*' Then Iterate;
    If Left(map.n,5) <> 'STMT:' Then Do
      Parse Var map.n row col atr csr exc exh init;
      If atr = '.' Then atr = '';
      If csr = '.' Then csr = '';
      If exc = '.' Then exc = '';
      If exh = '.' Then exh = '';
      init = Strip(init,'B');
      Call BuildField _map,map0._rows,map0._cols,row,col,,
                      BuildAttribute(atr,csr,exc,exh),init;
    End
    Else Do
      Parse Var map.n . stmt;
      Interpret stmt;
    End
  End
  Drop _map map. stmt init;
Return;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
