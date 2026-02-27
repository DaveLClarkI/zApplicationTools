TXT      TITLE 'TXTMAN   - TEXT MANIPULATION FOR COBOL PROGRAMS'                
         PRINT ON,GEN                                                           
TXTMAN   AMODE 31                                                               
TXTMAN   RMODE ANY                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*       T E X T   M A N I P U L A T I O N   S U B R O U T I N E       *         
*                 F O R   C O B O L   P R O G R A M S                 *         
*                                                                     *         
*                                                                     *         
* Winsupply Group Svcs.                         Written by:           *         
* 3110 Kettering Blvd.                          Dave L Clark I        *         
* Dayton, OH  45439                             davelclark1@gmail.com *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* TXTMAN   - Text Manipulaton subroutine for COBOL programs           *         
* Original copyright (C) 1986 Dave L Clark I (v1.0)                   *         
*                    (C) 2004 Dave L Clark I (v1.1)                   *         
*                    (C) 2011 Dave L Clark I (v1.2)                   *         
*                    (C) 2026 Dave L Clark I (v2.0)                   *         
*                                                                     *         
* This program is free software; you can redistribute it and/or       *         
* modify it under the terms of the GNU General Public License         *         
* as published by the Free Software Foundation; either                *         
* version 3 of the License, or any later version.                     *         
*                                                                     *         
* This program is distributed in the hope that it will be             *         
* useful, but WITHOUT ANY WARRANTY; without even the implied          *         
* warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR             *         
* PURPOSE.                                                            *         
*                                                                     *         
* See the GNU General Public License for more details.                *         
* You should have received a copy of the                              *         
* GNU General Public License along with this program; if not,         *         
* see <http://www.gnu.org/licenses/>.                                 *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
*                               PURPOSE                               *         
*                                                                     *         
*      This routine provides the  COBOL  programmer with an efficient *         
* means of performing various text string operations.   As originally *         
* implemented,  this meant performing the same operation (request) on *         
* every parm string  presented  in  a  single  CALL  to this routine. *         
* However,  as a later  enhancement,  it  means  this  routine can be *         
* called as a mini-scripting  engine  to perform different operations *         
* on a parm-string by parm-string basis--up to and including the full *         
* number of parm strings presented in a single CALL to this routine.  *         
*                                                                     *         
*      As an example,  of  the  difference,  it  should be noted that *         
* there is no single find/replace function.   In the legacy operation *         
* mode,  this  would  require  two  (find/overlay,  for  same  length *         
* replacement) or  three  (find/delete/insert,  for  different length *         
* replacement) calls to this routine.  Conversely, in scripting mode, *         
* this would be a  single  call  presenting  a series of parm strings *         
* that outlined the successive operations to perform (find/overlay or *         
* find/delete/insert) in sequence.                                    *         
*                                                                     *         
* NOTE: The legacy operation mode is still supported.                 *         
* =================================================================== *         
*                                                                     *         
*                           IMPLEMENTATION                            *         
*                                                                     *         
*      This implementation will  handle  a  master  "buffer" of up to *         
* 9,998 characters in length (except  "P"ARSE  only handles up to 256 *         
* characters at a time) and one  or more parameter "string"s of up to *         
* 256 characters in length.   The  invoking  COBOL program should use *         
* the following data areas (or similar) for passing parameters:       *         
*                                                                     *         
*      01  TXTMAN-PARMS.                                              *         
*          05  TXT-REQU                PIC  X(01)   VALUE SPACES.     *         
*              88  TXT-REQUEST-SCRIPTING            VALUE 'S'.        *         
*     * legacy requests dependent on a master buffer to manipulate    *         
*              88  TXT-REQUEST-COPY                 VALUE 'C'.        *         
*              88  TXT-REQUEST-DELETE               VALUE 'D'.        *         
*              88  TXT-REQUEST-FIND                 VALUE 'F'.        *         
*              88  TXT-REQUEST-INSERT               VALUE 'I'.        *         
*              88  TXT-REQUEST-OVERLAY              VALUE 'O'.        *         
*              88  TXT-REQUEST-PARSE                VALUE 'P'.        *         
*     * legacy requests that transform only the string parameter(s)   *         
*              88  TXT-REQUEST-JUSTIFY-L            VALUE 'J'.        *         
*              88  TXT-REQUEST-JUSTIFY-M            VALUE 'M'.        *         
*              88  TXT-REQUEST-JUSTIFY-R            VALUE 'R'.        *         
*              88  TXT-REQUEST-TRIM-R               VALUE 'T'.        *         
*     * legacy requests that translate only the string parameter(s)   *         
*              88  TXT-REQUEST-TOASCII              VALUE 'A'.        *         
*              88  TXT-REQUEST-TOEBCDIC             VALUE 'E'.        *         
*              88  TXT-REQUEST-PRINTMASK            VALUE 'K'.        *         
*              88  TXT-REQUEST-LOWERCASE            VALUE 'L'.        *         
*              88  TXT-REQUEST-UPPERCASE            VALUE 'U'.        *         
*              88  TXT-REQUEST-WORDCAPS             VALUE 'W'.        *         
*     * new requests to convert string parm(s) into the output buffer *         
*              88  TXT-REQUEST-BINTOB64             VALUE 'B'.        *         
*              88  TXT-REQUEST-B64TOBIN             VALUE 'X'.        *         
*     * note: B64-TO-BIN requires string lengths evenly divided by 4  *         
*     *        (the last quadruplet may be padded with '=' signs).    *         
*          05  TXT-RETN                PIC  X(01)   VALUE SPACES.     *         
*              88  TXT-REQUEST-COMPLETED            VALUE ' '.        *         
*              88  TXT-STRING-NOT-FOUND             VALUE 'N'.        *         
*              88  TXT-STRING-WAS-FOUND             VALUE 'Y'.        *         
*              88  TXT-BUFFER-IS-FULL               VALUE 'F'.        *         
*              88  TXT-PARSED-AT-BUFEND             VALUE LOW-VALUE.  *         
*              88  TXT-INVALID-REQU                 VALUE 'E'.        *         
*              88  TXT-INVALID-PARMLIST             VALUE 'L'.        *         
*              88  TXT-INVALID-PNTR                 VALUE 'P'.        *         
*              88  TXT-INVALID-BUFLEN               VALUE 'B'.        *         
*              88  TXT-INVALID-STRLEN               VALUE 'S'.        *         
*              88  TXT-SCRIPT-TERMINATED            VALUE 'T'.        *         
*          05  TXT-PNTR                PIC  9(04)   COMP VALUE ZEROES.*         
*     * these delimiters may be changed for the text "PARSE" request  *         
*          05  TXT-DLMS.                                              *         
*              10  TXT-DLM1            PIC  X(01)   VALUE '('.        *         
*              10  TXT-DLM2            PIC  X(01)   VALUE ')'.        *         
*              10  TXT-DLM3            PIC  X(01)   VALUE ','.        *         
*              10  TXT-DLM4            PIC  X(01)   VALUE QUOTE.      *         
*              10  TXT-DLM5            PIC  X(01)   VALUE '='.        *         
*              10  TXT-DLM6            PIC  X(01)   VALUE SPACE.      *         
*              10  TXT-DLM7            PIC  X(01)   VALUE SPACE.      *         
*              10  TXT-DLM8            PIC  X(01)   VALUE SPACE.      *         
*          05  TXT-INTERNAL-USE-ONLY   PIC  X(260)  VALUE LOW-VALUES. *         
*                                                                     *         
*      01  TXTMAN-BUFFER.                                             *         
*          05  TXT-MSTR-BUFLEN         PIC  9(04)   COMP VALUE 9998.  *         
*          05  TXT-MSTR-BUFFER.                                       *         
*              10  TXT-MSTR-BYTE       PIC  X(01)                     *         
*                          OCCURS 9998 TIMES                          *         
*                          DEPENDING   TXT-MSTR-BUFLEN.               *         
*                                                                     *         
* The following is the legacy layout for the string parameter(s).     *         
*                                                                     *         
*      01  TXTMAN-STRING.                                             *         
*          05  TXT-PARM-STRLEN         PIC  9(04)   COMP VALUE 256.   *         
*          05  TXT-PARM-STRING.                                       *         
*              10  TXT-PARM-BYTE       PIC  X(01)                     *         
*                          OCCURS 0256 TIMES                          *         
*                          DEPENDING   TXT-PARM-STRLEN.               *         
*                                                                     *         
* The following is  the  scripting  layout  for the string parameter. *         
* Then the CALL statement will  list  each of the array elements that *         
* you are actually using in  your  "script".   Note that the array is *         
* not limited to 99,  as there  is no technical limit-- only whatever *         
* COBOL allows for the total size of an 01-level.                     *         
*                                                                     *         
*      01  TXTMAN-SCRIPT.                                             *         
*        03  TXTMAN-S-ENTRY                         OCCURS 99.        *         
*          05  TXT-S-PARM-REQU         PIC  X(01).                    *         
*     * scripting requests dependent on a master buffer to manipulate *         
*              88  TXT-SCRIPT-COPY                  VALUE 'C'.        *         
*              88  TXT-SCRIPT-DELETE                VALUE 'D'.        *         
*              88  TXT-SCRIPT-FIND                  VALUE 'F'.        *         
*              88  TXT-SCRIPT-INSERT                VALUE 'I'.        *         
*              88  TXT-SCRIPT-OVERLAY               VALUE 'O'.        *         
*              88  TXT-SCRIPT-PARSE                 VALUE 'P'.        *         
*     * scripting requests that transform only the string parameter   *         
*              88  TXT-SCRIPT-JUSTIFY-L             VALUE 'J'.        *         
*              88  TXT-SCRIPT-JUSTIFY-M             VALUE 'M'.        *         
*              88  TXT-SCRIPT-JUSTIFY-R             VALUE 'R'.        *         
*              88  TXT-SCRIPT-TRIM-R                VALUE 'T'.        *         
*     * scripting requests that translate only the string parameter   *         
*              88  TXT-SCRIPT-TOASCII               VALUE 'A'.        *         
*              88  TXT-SCRIPT-TOEBCDIC              VALUE 'E'.        *         
*              88  TXT-SCRIPT-PRINTMASK             VALUE 'K'.        *         
*              88  TXT-SCRIPT-LOWERCASE             VALUE 'L'.        *         
*              88  TXT-SCRIPT-UPPERCASE             VALUE 'U'.        *         
*              88  TXT-SCRIPT-WORDCAPS              VALUE 'W'.        *         
*          05  TXT-S-PARM-RETN         PIC  X(01).                    *         
*              88  TXT-S-STRING-COMPLETED           VALUE ' '.        *         
*              88  TXT-S-REQUEST-INHERIT            VALUE 'I'.        *         
*              88  TXT-S-STRING-NOT-FOUND           VALUE 'N'.        *         
*              88  TXT-S-STRING-WAS-FOUND           VALUE 'Y'.        *         
*              88  TXT-S-PARSED-AT-BUFEND           VALUE LOW-VALUE.  *         
*              88  TXT-S-INVALID-REQU               VALUE 'E'.        *         
*              88  TXT-S-INVALID-INHERIT            VALUE 'F'.        *         
*              88  TXT-S-INVALID-STRLEN             VALUE 'S'.        *         
*          05  TXT-S-PARM-STRLEN       PIC  9(04)   BINARY.           *         
*          05  TXT-S-PARM-STRING.                                     *         
*              10  TXT-S-PARM-BYTE     PIC  X(01)   OCCURS 256.       *         
*                                                                     *         
*                                                                     *         
*                  SPECIAL CASES FOR SCRIPTING, ONLY                  *         
*                                                                     *         
* Also note that the scripting string parameters cannot use the COBOL *         
* "DEPENDING ON" phrase because this routine may have to increase the *         
* length of the string on-the-fly.  Why would it do that?  For script *         
* purposes,  you may want  one  or  more  of your operations,  in the *         
* script, to inherit the manipulated value of the previous string.    *         
*                                                                     *         
* To do this,  preset the return code  of that string parameter to an *         
* "I" to signal that this string is to inherit its value (and, hence, *         
* its length) from the previous string in the script.                 *         
*                                                                     *         
* In addition,  note that there is a  way to reset the buffer pointer *         
* in the middle of a  script  (if needed).   Simply preset the string *         
* parameter's return code to "R" and the buffer pointer will be reset *         
* before that string operation is performed.                          *         
*                                                                     *         
* Of course,  this means that you cannot perform a string inherit and *         
* a buffer pointer reset in the same string operation.   However, you *         
* shouldn't need to as pointer reset  is mostly for the FIND function *         
* and you generally have a specific string in mind for that.          *         
* =================================================================== *         
*                                                                     *         
*                             INVOCATION                              *         
*                                                                     *         
*      This subroutine can be invoked in several ways (depending upon *         
* the function desired).   Supplied functions  provide the ability to *         
* copy/parse, insert/overlay,  delete,  find,  justify, and translate *         
* text strings.   These functions generally operate based on a master *         
* "buffer" and one or more parameter "strings" -- as follows:         *         
*                                                                     *         
*                   ----- MANIPULATIONS -----                         *         
*     "C"opy      - copy from master buffer into parm string(s)       *         
*     "P"arse     - parse master buffer into parm string(s)           *         
*     "I"nsert    - insert parm string(s) into master buffer          *         
*     "O"verlay   - overlay parm string(s) onto master buffer         *         
*     "D"elete    - delete single parm string from master buffer      *         
*                   (multiple strings allowed in scripting mode)      *         
*     "F"ind      - find single parm string in master buffer          *         
*                   (multiple strings allowed in scripting mode)      *         
*                                                                     *         
*                   ----- TRANSFORMATIONS -----                       *         
*     "J"ustify   - skip leading chars  <=  spaces in parm string(s), *         
*                   left-justify  the  remainder,  compress delimiter *         
*                   chars out (if any, else hi-values for no compres- *         
*                   sion), dropping trailing chars <= spaces (setting *         
*                   the string length accordingly)                              
*     "M"justify  - skip leading/trailing  chars  <=  spaces  in parm *         
*                   string(s) and middle-justify the remainder        *         
*     "R"justify  - skip trailing chars  <=  spaces in parm string(s) *         
*                   and right-justify the remainder                   *         
*     "T"rimR     - get length of parm  string(s) w/o trailing spaces *         
*                   (setting the string length parameter accordingly) *         
*                                                                     *         
*                   ----- TRANSLATIONS -----                          *         
*     to"A"SCII   - translate EBCDIC parm string(s) to ASCII          *         
*     to"E"BCDIC  - translate ASCII parm string(s) to EBCDIC          *         
*     PrintMas"K" - mask non-printable chars in parm string(s)        *         
*     "L"owerCase - translate parm string(s) to lower-case            *         
*     "U"pperCase - translate parm string(s) to upper-case            *         
*     "W"ordCaps  - first translate parm string(s) to lower-case and  *         
*                   then capitalize the first letter of each word     *         
*                                                                     *         
*                   ----- SCRIPTING -----                             *         
*     "S"cripting - operate in mini-scripting mode--this can perform  *         
*                   different functions, by string, in a single CALL. *         
*                   But this excludes the following two functions, as *         
*                   they completely rebuild the output buffer.        *         
*                                                                     *         
*                   ----- CONVERSIONS -----                           *         
*     to"B"ase64  - encode binary input  string(s) into Base64 output *         
*                   (use a maximum of 7,494 binary characters,  or up *         
*                   to 29 full input strings + 70 bytes in 30th input *         
*                   string for up to 9,992 bytes into output buffer-- *         
*                   i.e., 29 x 256 + 70 =  7,494 / 3 x 4 = 9,992)     *         
*                   Also, after encoding, translate to ASCII before   *         
*                   storing for web services or email attachments.    *         
*     "X"fromB64  - decode Base64 input  string(s) into binary output *         
*                   (use a maximum of 13,324 Base64 characters, or up *         
*                   to 52 full input strings + 12 bytes in 53rd input *         
*                   string for up to 9,993 bytes into output buffer-- *         
*                   i.e., 52 x 256 + 12 = 13,324 / 4 x 3 = 9,993)     *         
*                   Then, before decoding, translate from ASCII to    *         
*                   EBCDIC if from web services or email attachments. *         
*                                                                     *         
*      The  example  below  demonstrates  how  to  "F"ind  a  string, *         
* starting at a given position, in the buffer.   Note that "TXT-PNTR" *         
* applies to the current master buffer position and,  therefore,  all *         
* operations based on the content of  the master buffer will start at *         
* the buffer position pointed  to  by  "TXT-PNTR" and proceed for the *         
* length of the parameter string(s);  except "P"arse where the length *         
* is based on delimiter location(s).   Also  note that the same group *         
* of functions (except "D"elete)  automatically advance "TXT-PNTR" an *         
* amount equal  to  the  length  of  the parameter string(s);  except *         
* "F"ind which advances "TXT-PNTR"  to  the  location of the matching *         
* string--or, to the end of the master buffer.                        *         
*                                                                     *         
*          SET TXT-REQUEST-FIND TO TRUE                               *         
*          MOVE starting-point  TO TXT-PNTR                           *         
*          MOVE your-buflen     TO TXT-BUFLEN                         *         
*          MOVE your-buffer     TO TXT-BUFFER                         *         
*          MOVE your-strlen     TO TXT-STRLEN                         *         
*          MOVE your-string     TO TXT-STRING                         *         
*          CALL 'TXTMAN'     USING TXTMAN-PARMS                       *         
*                                  TXTMAN-BUFFER                      *         
*                                  TXTMAN-STRING                      *         
*          EVALUATE TRUE                                              *         
*          WHEN TXT-STRING-WAS-FOUND                                  *         
*     *        your-string-was-found-code                             *         
*          WHEN TXT-STRING-NOT-FOUND                                  *         
*     *        your-string-not-found-code                             *         
*          WHEN OTHER                                                 *         
*     *        your-error-handling-code                               *         
*          END-EVALUATE                                               *         
*                                                                     *         
* Note you do not have to use either of the structures represented by *         
* "TXTMAN-BUFFER" or "TXTMAN-STRING"  (or  those  names)  in the CALL *         
* statement shown above.  You may code your own names, and associated *         
* structures,  as long as  those  structures  begin  with a half-word *         
* length field,  as shown,  and  are  not  longer  than the structure *         
* lengths shown.   In fact,  functions  supporting multiple parameter *         
* strings ("C"opy/"P"arse and "I"nsert/"O"verlay) will *require* that *         
* you code your own  parameter  string  structures  where you wish to *         
* specify multiple parameter string arguments in the CALL statement.  *         
*                                                                     *         
*      The example  below  demonstrates  how  to  perform  a find and *         
* replace operation in a single CALL,  using the new scripting method *         
* (which,  by the legacy method,  would  require two or three CALLs-- *         
* depending on same-length vs. different-length replacements).        *         
*                                                                     *         
*          SET TXT-REQUEST-SCRIPTING TO TRUE                          *         
*          MOVE starting-point       TO TXT-PNTR                      *         
*          MOVE your-buflen          TO TXT-BUFLEN                    *         
*          MOVE your-buffer          TO TXT-BUFFER                    *         
*          SET TXT-SCRIPT-FIND(1)    TO TRUE                          *         
*          MOVE your-strlen          TO TXT-S-STRLEN(1)               *         
*          MOVE your-string          TO TXT-S-STRING(1)               *         
*     * this part optional but required for different-length replace  *         
*          SET TXT-SCRIPT-DELETE(2)  TO TRUE                          *         
*          SET TXT-S-REQUEST-INHERIT(2) TO TRUE                       *         
*     * if no "DELETE", above, then use "OVERLAY" below               *         
*          SET TXT-SCRIPT-INSERT(3)  TO TRUE                          *         
*          MOVE your-strlen2         TO TXT-S-STRLEN(3)               *         
*          MOVE your-string2         TO TXT-S-STRING(3)               *         
*          CALL 'TXTMAN'          USING TXTMAN-PARMS                  *         
*                                       TXTMAN-BUFFER                 *         
*                                       TXTMAN-S-ENTRY(1)             *         
*     * this part optional but required for different-length replace  *         
*                                       TXTMAN-S-ENTRY(2)             *         
*     * this part still required for either insert or overlay         *         
*                                       TXTMAN-S-ENTRY(3)             *         
*          IF  TXT-SCRIPT-TERMINATED                                  *         
*     *      loop-thru-string-return-codes-to-identify-failure-point  *         
*          END-IF                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
* =================================================================== *         
*        PROGRAM DUMMY SECTIONS                                                 
* ------------------------------------------------------------------- *         
         COPY  TXTMAN                                                           
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
*        MACROS                                                       *         
* ------------------------------------------------------------------- *         
         PRINT OFF                                                              
         COPY  HEADER        PROGRAM ENTRY CODE                                 
         COPY  TRAILER       PROGRAM RETURN CODE                                
         COPY  DATAAREA      STATIC STORAGE ALIGNMENT                           
         COPY  DATAEND       STATIC STORAGE ALIGNMENT                           
         COPY  SHI           SUBTRACT HALFWORD IMMEDIATE                        
         COPY  CF            CLEAR FLAG                                         
         COPY  SF            SET FLAG                                           
         COPY  TF            TEST FLAG                                          
         COPY  IF            IF/AND/ELSE/ENDIF CONSTRUCT                        
         COPY  SELECT        SELECT/WHEN/WHEN OTHER/ENDSL CONSTRUCT             
         COPY  WHILE         WHILE/WEXIT/WLOOP/WEND CONSTRUCT                   
         PRINT ON                                                               
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
*        REGISTER USAGES                                                        
* ------------------------------------------------------------------- *         
R0       EQU   0             WORK REG                                           
R1       EQU   1             PARM REG (INPUT & TRT INSTRUCTION)                 
R2       EQU   2             PARM REG (TRT INSTRUCTION)                         
R3       EQU   3             WORK REG (PNTR/COUNTER/COMPARATOR)                 
R4       EQU   4             WORK REG (FROM ADDRESS)                            
R5       EQU   5             WORK REG (FROM LENGTH)                             
R6       EQU   6             WORK REG ( TO    ADDRESS)                          
R7       EQU   7             WORK REG ( TO    LENGTH)                           
R8       EQU   8             TXTPARM DSECT BASE REG                             
R9       EQU   9             TXTMSTR DSECT BASE REG                             
R10      EQU   10            TXTINPT DSECT BASE REG                             
R11      EQU   11            TXTMAN CSECT BASE REG                              
R12      EQU   12            NOT USED                                           
R13      EQU   13            REGISTER SAVEAREA                                  
R14      EQU   14            RETURN REG                                         
R15      EQU   15            ENTRY REG                                          
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
TXTMAN   HEADER VERSION='2.0',AUTHOR='DLC',DATE='(C)DEC86',            X        
               BASE=R11                                                         
* ------------------------------------------------------------------- *         
         IF    R1,(EQ,CR),R11     IF PARM LIST IS MISSING                       
          WTO  'TXTMAN: PARAMETER LIST REQUIRED'                                
          J    RETURN              GO RETURN TO ORIG CALLER                     
         ENDIF                    ENDIF                                         
*                                                                               
         LM    R8,R9,0(R1)        GET ADDR. OF FIRST 2 PARAMETERS               
         USING TXTPARM,R8         ESTABLISH REG 8 AS BASE                       
         USING TXTMSTR,R9         ESTABLISH REG 9 AS BASE                       
*                                                                               
         IF    R8,(M,LTR),R8,OR,  PARM LIST CAN'T END WITH PARM 1      X        
               R9,(M,LTR),R9           OR PARM 2, A 3RD IS REQUIRED             
          MVI  TXTRETN,INV_PLST    SIGNAL PARMLIST LENGTH ERROR                 
          J    RETURN              GO RETURN TO ORIG CALLER                     
         ENDIF                    ENDIF                                         
*                                                                               
         LA    R1,4(,R1)          INCREMENT TO LAST PARM REFERENCED             
         ST    R1,PARMPTR         SAVE PARAMETER ADDRESSES POINTER              
         SF    STRGADDR,X'80'     SIGNAL NEXT PARM STRING AS THE FIRST          
* ------------------------------------------------------------------- *         
SETUP    EQU   *                  LOOP TIL ALL PARM STRINGS PROCESSED           
         JAS   R2,GETSTRG         GET (NEXT) STRING PARAMETER ADDRESS           
         USING TXTINPT,R10        ESTABLISH REG 10 AS BASE                      
*                                                                               
         LH    R3,TXTBUFL         GET MASTER BUFFER LENGTH                      
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE AND              
         AND   TXTSREQU,(EQ,CLIY),STR_PARSE     PARSE FUNCTION                  
          IF   R3,(NP,LTR),R3,OR,R3,(H,CH),=H'256'                              
           MVIY TXTSRETN,INV_BUFL   SIGNAL BUFFER LENGTH ERROR                  
           J   SCRIPTXT             TAKE THE SCRIPTING EXIT                     
          ENDIF                    ENDIF                                        
          LH   R3,TXTSTRL          GET PARAMETER STRING LENGTH                  
          IF   R3,(M,LTR),R3,OR,R3,(H,CH),=H'256'                               
           MVIY TXTSRETN,S_INV_STRL SIGNAL STRING LENGTH ERROR                  
           J   SCRIPTXT             GO RETURN TO ORIG CALLER                    
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
*                                                                               
         LH    R3,TXTBUFL         GET MASTER BUFFER LENGTH                      
         IF    TXTREQU,EQ,REQU_PARSE   IF NON-SCRIPTING PARSE FUNC              
          IF   R3,(NP,LTR),R3,OR,R3,(H,CH),=H'256'                              
           MVI TXTRETN,INV_BUFL     SIGNAL BUFFER LENGTH ERROR                  
           J   RETURN               GO RETURN TO ORIG CALLER                    
          ENDIF                    ENDIF                                        
          LH   R3,TXTSTRL          GET PARAMETER STRING LENGTH                  
          IF   R3,(M,LTR),R3,OR,R3,(H,CH),=H'256'                               
           MVI TXTRETN,INV_STRL     SIGNAL STRING LENGTH ERROR                  
           J   RETURN               GO RETURN TO ORIG CALLER                    
          ENDIF                    ENDIF                                        
         ELSE                     ELSE--NOT PARSING                             
          IF   R3,(NP,LTR),R3,OR,R3,(H,CH),=H'9998'                             
           IF  TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
            MVI TXTRETN,INV_BUFL     SIGNAL BUFFER LENGTH ERROR                 
            J  RETURN                GO RETURN TO ORIG CALLER                   
           ELSE                     ELSE                                        
            MVIY TXTSRETN,INV_BUFL   SIGNAL BUFFER LENGTH ERROR                 
            J  SCRIPTXT              TAKE THE SCRIPTING EXIT                    
           ENDIF                    ENDIF                                       
          ENDIF                    ENDIF                                        
          LH   R3,TXTSTRL          GET PARAMETER STRING LENGTH                  
          IF   R3,(NP,LTR),R3,OR,R3,(H,CH),=H'256'                              
           IF  TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
            MVI TXTRETN,INV_STRL     SIGNAL STRING LENGTH ERROR                 
            J  RETURN                GO RETURN TO ORIG CALLER                   
           ELSE                     ELSE                                        
            MVIY TXTSRETN,S_INV_STRL SIGNAL STRING LENGTH ERROR                 
            J  SCRIPTXT              TAKE THE SCRIPTING EXIT                    
           ENDIF                    ENDIF                                       
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
*                                                                               
         LH    R3,TXTPNTR         GET BUFFER POINTER VALUE                      
         IF    R3,(Z,LTR),R3      IF POINTER IS ZERO                            
          LHI  R3,1                SET DEFAULT POINTER OF 1                     
          STH  R3,TXTPNTR          SAVE DEFAULT BACK TO PARM FIELD              
         ENDIF                    ENDIF                                         
         IF    R3,(NP,LTR),R3,OR,R3,(HT,CH),TXTBUFL                             
          IF   TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
           MVI TXTRETN,INV_PNTR     SIGNAL POINTER VALUE ERROR                  
           J   RETURN               GO RETURN TO ORIG CALLER                    
          ELSE                     ELSE                                         
           MVIY TXTSRETN,INV_PNTR   SIGNAL POINTER VALUE ERROR                  
           J   SCRIPTXT             TAKE THE SCRIPTING EXIT                     
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
*                                                     MANIPULATIONS             
         IF    TXTREQU,EQ,REQU_COPY,BC,TXTCOPY                                  
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_DELETE,BC,TXTDLET                                
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_FIND,BC,TXTFIND                                  
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_INSERT,BC,TXTISRT                                
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_OVERLAY,BC,TXTCOPY                               
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_PARSE,BC,TXTPARS                                 
         ENDIF                                                                  
*                                                     TRANSFORMATIONS           
         IF    TXTREQU,EQ,REQU_LJUSTIFY,BC,TXTLEFT                              
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_MJUSTIFY,BC,TXTRGHT                              
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_RJUSTIFY,BC,TXTRGHT                              
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_RTRIM,BC,TXTTRIM                                 
         ENDIF                                                                  
*                                                     TRANSLATIONS              
         IF    TXTREQU,EQ,REQU_2ASCII,BC,TXT2ASC                                
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_2EBCDIC,BC,TXT2EBC                               
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_PMASK,BC,TXTMASK                                 
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_LCASE,BC,TXTCASE                                 
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_UCASE,BC,TXTCASE                                 
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_WCAPS,BC,TXTCASE                                 
         ENDIF                                                                  
*                                                     CONVERSIONS               
         IF    TXTREQU,EQ,REQU_BTOB64,BC,TXT2B64                                
         ENDIF                                                                  
         IF    TXTREQU,EQ,REQU_4TOBIN,BC,TXT4B64                                
         ENDIF                                                                  
*                                                     SCRIPTING MODE            
         IF    TXTREQU,EQ,REQU_SCRIPTED                                         
          IF   TXTSREQU,(EQ,CLIY),REQU_COPY,BC,TXTCOPY                          
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_DELETE,BC,TXTDLET                        
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_FIND,BC,TXTFIND                          
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_INSERT,BC,TXTISRT                        
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_OVERLAY,BC,TXTCOPY                       
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_PARSE,BC,TXTPARS                         
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_LJUSTIFY,BC,TXTLEFT                      
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_MJUSTIFY,BC,TXTRGHT                      
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_RJUSTIFY,BC,TXTRGHT                      
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_RTRIM,BC,TXTTRIM                         
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_2ASCII,BC,TXT2ASC                        
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_2EBCDIC,BC,TXT2EBC                       
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_PMASK,BC,TXTMASK                         
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_LCASE,BC,TXTCASE                         
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_UCASE,BC,TXTCASE                         
          ENDIF                                                                 
          IF   TXTSREQU,(EQ,CLIY),REQU_WCAPS,BC,TXTCASE                         
          ENDIF                                                                 
          MVIY TXTSRETN,INV_REQU   SIGNAL STRING REQUEST ERROR                  
          J    SCRIPTXT            TAKE THE SCRIPTING EXIT                      
         ENDIF                    ENDIF                                         
*                                                                               
         MVI   TXTRETN,INV_REQU   SIGNAL TEXT REQUEST ERROR                     
         J     RETURN             GO RETURN TO ORIG CALLER                      
         EJECT                                                                  
* ------------------------------------------------------------------- *         
SCRIPTXT EQU   *                                                                
         MVI   TXTRETN,SCR_TERM   SIGNAL SCRIPT TERMINATED                      
         J     RETURN             GO RETURN TO ORIG CALLER                      
TXTDONE  EQU   *                                                                
         MVI   TXTRETN,REQ_DONE   SIGNAL REQUEST COMPLETED                      
RETURN   EQU   *                                                                
         TRAILER RETCODE=0        RETURN TO ORIGINAL CALLER                     
* ------------------------------------------------------------------- *         
         EJECT                                                                  
* ------------------------------------------------------------------- *         
GETSTRG  EQU   *   GET THE NEXT STRING PARAMETER ADDRESS (IF ANY)               
* ------------------------------------------------------------------- *         
         L     R1,PARMPTR         GET PARAMETER ADDRESSES POINTER               
         L     R10,0(,R1)         GET ADDRESS OF PREV PARAMETER                 
         IF    R10,(M,LTR),R10,BC,TXTDONE   IF LAST PARM PROCESSED              
         ENDIF                               RETURN TO ORIG CALLER              
         LA    R1,4(,R1)          INCREMENT TO NEXT PARAMETER                   
         ST    R1,PARMPTR         SAVE PARAMETER ADDRESSES POINTER              
         L     R10,0(,R1)         GET ADDRESS OF THIS PARAMETER                 
         LA    R10,0(,R10)        CLEAR LAST PARM INDICATOR (IF ANY)            
*                                                                               
         IF    TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
          MVI  TXTRETN,REQ_DONE    CLEAR LEGACY RETURN CODE                     
         ELSE                     ELSE--SCRIPTING MODE                          
          AHI  R10,2               RE-BASE FOR LEGACY LEN AND VAL               
* Note that "re-basing" is required because I didn't want to duplicate          
*      the legacy functions to work with two different formats for the          
*      input string parameter.  With re-basing, the two bytes prior to          
*      the legacy input string parm are available in scripting mode by          
*      using CLIY or MVIY as they allow a negative displacement. Thus,          
*      TXTSTRL and TXTSTRG are the same for both execution modes.               
*                                                                               
          IF   TXTSRETN,(EQ,CLIY),S_REQ_RSTP   POINTER RESET REQUEST?           
           MVHHI TXTPNTR,1          RESET POINTER TO BUFFER                     
          ENDIF                    ENDIF                                        
          IF   TXTSRETN,(EQ,CLIY),S_REQ_INHRT   INHERITANCE REQUEST?            
           IF  STRGADDR,(ON,TF),X'80'   IF FIRST SCRIPTING STRING               
            MVIY TXTSRETN,S_INV_INHRT SIGNAL INVALID INHERITANCE                
            J  SCRIPTXT              TAKE THE SCRIPTING EXIT                    
           ENDIF                    ENDIF                                       
           CF  STRGADDR,X'80'       OK, FIRST PARM STRING PROCESSED             
           SHI R1,4                 POINT TO PREVIOUS PARAMETER                 
           L   R10,0(,R1)           GET PARAMETER ADDRESS                       
           AHI R10,2                RE-BASING (SEE ABOVE)                       
           LH  R5,TXTSTRL           GET STRING LENGTH                           
           LA  R4,TXTSTRG           GET STRING ADDRESS                          
           AHI R1,4                 RETURN TO CURRENT PARAMETER                 
           L   R10,0(,R1)           GET PARAMETER ADDRESS                       
           LA  R10,0(,R10)          CLEAR LAST PARM INDICATOR (IF ANY)          
           AHI R10,2                RE-BASING (SEE ABOVE)                       
           STH R5,TXTSTRL           PUT STRING LENGTH                           
           SHI R5,1                  MINUS 1 FOR EXECUTE                        
           EXRL R5,GETMVC           PUT STRING VALUE                            
          ENDIF                    ENDIF                                        
*                                                                               
          MVIY TXTSRETN,S_REQ_DONE CLEAR SCRIPTING RETURN CODE                  
         ENDIF                    ENDIF                                         
*                                                                               
         BR    R2                 RETURN TO INTERNAL CALLER                     
*                                                                               
GETMVC   MVC   TXTSTRG(0),0(R4)   (SEE "EXRL" INSTRUCTION ABOVE)                
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTCOPY  EQU   *   COPY THE REQUESTED STRING FROM/OVER THE BUFFER               
* ------------------------------------------------------------------- *         
         LH    R3,TXTPNTR         GET POINTER VALUE                             
         SHI   R3,1               LESS 1 FOR OFFSET                             
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                            
         AR    R4,R3              ADD POINTER OFFSET                            
         LH    R5,TXTSTRL         GET STRING LENGTH                             
         SHI   R5,1               LESS 1 FOR EXECUTE                            
*                                                                               
         IF    TXTREQU,EQ,REQU_COPY,OR,TXTREQU,EQ,REQU_PARSE                    
          EXRL R5,TXTMVCX         COPY(PARSE), DO IT                            
         ELSE                                                                   
          IF   TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE                  
           IF  TXTSREQU,(EQ,CLIY),STR_COPY,OR,                         X        
               TXTSREQU,(EQ,CLIY),STR_PARSE                                     
            EXRL R5,TXTMVCX       COPY(PARSE), DO IT                            
           ELSE                                                                 
            EXRL R5,TXTMVCO       OVERLAY(INSERT), DO IT                        
           ENDIF                                                                
          ELSE                                                                  
           EXRL R5,TXTMVCO        OVERLAY(INSERT), DO IT                        
          ENDIF                                                                 
         ENDIF                                                                  
*                                                                               
         LH    R3,TXTPNTR         GET POINTER VALUE                             
         AH    R3,TXTSTRL         ADD STRING LENGTH                             
         STH   R3,TXTPNTR         PUT POINTER VALUE                             
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
TXTMVCX  MVC   TXTSTRG(0),0(R4)   EXTRACT STRING FROM BUFFER                    
TXTMVCO  MVC   0(0,R4),TXTSTRG    OVERLAY BUFFER WITH STRING                    
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTDLET  EQU   *   DELETE THE REQUESTED AMOUNT FROM BUFFER                      
* ------------------------------------------------------------------- *         
         LH    R3,TXTPNTR         GET POINTER VALUE                             
         SHI   R3,1               LESS 1 FOR OFFSET                             
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                            
         AR    R4,R3              ADD POINTER OFFSET                            
         LH    R5,TXTBUFL         GET LENGTH OF BUFFER                          
         SR    R5,R3              SUBTRACT POINTER OFFSET                       
         LR    R6,R4              COPY BUFFER ADDRESS + POINTER                 
         AH    R6,TXTSTRL         ADD DELETE LENGTH                             
         LR    R7,R5              COPY LENGTH OF BUFFER - POINTER               
         SH    R7,TXTSTRL         SUBTRACT DELETE LENGTH                        
         ICM   R7,B'1000',=X'40'  GET PAD CHARACTER                             
         MVCL  R4,R6              PERFORM OPERATION                             
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE                  
          J    SETUP               GO GET THE NEXT PARAMETER                    
         ENDIF                    ENDIF                                         
         J     RETURN             GO RETURN TO ORIG CALLER                      
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTFIND  EQU   *   FIND THE REQUESTED STRING IN BUFFER                          
* ------------------------------------------------------------------- *         
         IF    TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
          MVI  TXTRETN,STR_NFND    INITIAL SETTING FOR "NOT FOUND"              
         ELSE                     ELSE--SCRIPTING MODE                          
          MVIY TXTSRETN,S_STR_NFND INITIAL SETTING FOR "NOT FOUND"              
         ENDIF                    ENDIF                                         
*                                                                               
         XC    TRTTABLE,TRTTABLE  CLEAR TRANSLATE TABLE                         
         XR    R1,R1              CLEAR REGISTER                                
         IC    R1,TXTSTRG         GET FIRST CHARACTER OF STRING                 
         LA    R2,TRTTABLE(R1)    POINT TO TABLE POSITION                       
         MVI   0(R2),X'FF'        SET FUNCTION BYTE VALUE                       
         LH    R3,TXTPNTR         GET POINTER VALUE                             
         SHI   R3,1               MINUS 1 FOR OFFSET                            
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                            
         LR    R6,R4              COPY BUFFER ADDRESS                           
         AR    R4,R3              ADD POINTER OFFSET                            
         LH    R5,TXTBUFL         GET BUFFER LENGTH                             
         AR    R6,R5              SET BUFFER STOP ADDRESS                       
         SR    R5,R3              SUBTRACT POINTER OFFSET                       
*                                                                               
         WHILE R4,(L,CR),R6       WHILE CURRENT ADDR < STOP ADDR                
          LR   R5,R6               GET STOP ADDRESS                             
          SR   R5,R4               SUBTRACT CURRENT TO GET REMAINING            
          LH   R7,=H'256'          GET MAXIMUM LENGTH FOR "TRT" INSTR.          
          IF   R7,(H,CR),R5        IF MAX LEN > BUF LEN                         
           LR  R7,R5                USE REMAINING BUFFER LENGTH                 
          ENDIF                    ENDIF                                        
          SHI  R7,1                MINUS 1 FOR ZERO REFERENCE                   
          XR   R1,R1               CLEAR ADDRESS REGISTER                       
          XR   R2,R2               CLEAR FUNCTION REGISTER                      
          IF   R7,(Z,EXRL),TXTTRT  IF CHAR NOT FOUND IN THIS BLOCK              
           AR  R4,R7                ADD SEARCH LEN TO POINTER                   
           AHI R4,1                 PLUS THE 1 WE SUBTRACTED FOR EXRL           
           WLOOP                    GO REPEAT LOOP                              
TXTTRT     TRT 0(0,R4),TRTTABLE     (SEE "EXRL" INSTRUCTION ABOVE)              
          ENDIF                    ENDIF                                        
          LR   R4,R1               SYNC POINTER TO THE FOUND CHAR               
          LR   R7,R6               COPY BUFFER STOP POINT                       
          SR   R7,R4               DISTANCE FROM FOUND CHAR TO END              
          IF   R7,(L,CR),R3        IF DISTANCE < STRING LENGTH                  
           WEXIT                    FOUND CHAR, BUT TOO CLOSE TO END            
          ENDIF                    ENDIF                                        
          LH   R5,TXTSTRL          GET STRING LENGTH                            
          SHI  R5,1                   MINUS 1 FOR ZERO REFERENCE                
          IF   R5,(EQ,EXRL),TXTCLC IF STRING MATCHES                            
           IF  TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
            MVI TXTRETN,STR_YFND     SIGNAL THE STRING WAS FOUND                
           ELSE                     ELSE--SCRIPTING MODE                        
            MVIY TXTSRETN,S_STR_YFND SIGNAL THE STRING WAS FOUND                
           ENDIF                    ENDIF                                       
           WEXIT                    GO EXIT LOOP                                
TXTCLC     CLC 0(0,R4),TXTSTRG      (SEE "EXRL" INSTRUCTION ABOVE)              
          ENDIF                    ENDIF                                        
          AHI  R4,1                INCREMENT PAST THIS CHAR OCCURANCE           
         WEND                     END WHILE R4,(L,CR),R6                        
*                                                                               
         LA    R1,TXTBUFR         GET BUFFER START ADDRESSES                    
         LR    R3,R4              COPY CURRENT SCAN POINTER                     
         SR    R3,R1              CALCULATE 0-BASED OFFSET                      
         AHI   R3,1               ADJUST TO 1-BASED POINTER                     
         STH   R3,TXTPNTR         PUT BACK IN PARAMETER                         
*                                                                               
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE                  
          J    SETUP               GO GET THE NEXT PARAMETER                    
         ENDIF                    ENDIF                                         
         J     RETURN             GO RETURN TO ORIG CALLER                      
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTISRT  EQU   *   INSERT THE REQUESTED STRING INTO BUFFER                      
* ------------------------------------------------------------------- *         
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                    00034700
         LH    R5,TXTBUFL         GET BUFFER LENGTH                     00034800
         AR    R4,R5              CALCULATE ADDRESS OF BUFFER END       00034900
         SHI   R4,1                  LESS 1 FOR ZERO REFERENCE          00035000
         LR    R6,R4              COPY ADDRESS OF BUFFER END            00035100
         SH    R6,TXTSTRL         SUBTRACT STRING LEN FROM BUFFER ADR   00035200
         SH    R5,TXTSTRL         SUBTRACT STRING LEN FROM BUFFER LEN   00035300
         SH    R5,TXTPNTR         SUBTRACT POINTER FROM BUFFER LEN      00035400
         AHI   R5,1                   PLUS 1 FOR ACTUAL LENGTH          00035500
         WHILE R5,(NZ,LTR),R5                                           00035600
          MVC  0(1,R4),0(R6)      MOVE CHARACTER TO THE RIGHT           00035800
          SHI  R6,1               DECREMENT FROM ADDRESS FOR MOVE       00035900
          SHI  R4,1               DECREMENT  TO  ADDRESS FOR MOVE       00036000
          SHI  R5,1               DECREMENT LOOP COUNT                  00035700
         WEND                                                           00036100
*                                                                               
         J     TXTCOPY            GO OVERLAY INSERT TEXT                        
*                                                                               
MVCRIGHT MVC   0(0,R6),0(R4)      (USED ONLY FOR REMAINDER)                     
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTPARS  EQU   *   PARSE THE BUFFER INTO THE STRING(S)                          
* ------------------------------------------------------------------- *         
         XC    TRTTABLE,TRTTABLE  CLEAR TRANSLATE TABLE                         
         XR    R2,R2              CLEAR REGISTER                                
         LA    R6,TXTDLMS         GET ADDRESS OF DELIMITERS                     
         LHI   R7,8               LOOP FOR EIGHT DELIMITERS                     
         WHILE R7,(NZ,LTR),R7                                                   
          SHI  R7,1               DECREMENT LOOP COUNT                          
          IC   R2,0(R6)           GET DELIMITER CHARACTER                       
          STC  R2,TRTTABLE(R2)    PUT IT IN TRANSLATE & TEST TABLE              
          AHI  R6,1               POINT TO NEXT DELIMITER                       
         WEND                                                                   
*                                                                               
         XR    R1,R1              CLEAR DELIMETER ADDR REGISTER                 
         XR    R2,R2              CLEAR DELIMETER CHAR REGISTER                 
         LH    R3,TXTPNTR         GET POINTER VALUE                             
         SHI   R3,1               LESS 1 FOR OFFSET                             
         LA    R4,TXTBUFR         GET BUFFER ADDRESS                            
         AR    R4,R3              ADD POINTER OFFSET                            
         LH    R5,TXTBUFL         GET BUFFER LENGTH                             
         SR    R5,R3              SUBTRACT POINTER OFFSET                       
*                                                                               
         IF    R5,(Z,EXRL),TXTTRT IF NO DELIMITERS FOUND                        
          LR   R1,R4               COPY BUFFER ADDRESS                          
          AR   R1,R5               ADD REMAINING LENGTH                         
          IF   TXTREQU,NE,REQU_SCRIPTED   IF NOT SCRIPTING MODE                 
           MVI TXTRETN,AT_BUFEND     SIGNAL END OF BUFFER                       
          ELSE                      ELSE--SCRIPTING MODE                        
           MVIY TXTSRETN,S_AT_BUFEND SIGNAL END OF BUFFER                       
          ENDIF                     ENDIF                                       
         ELSE                     ELSE--PARSE END OF VALUE                      
          IF   R2,(EQ,CLM,B'0001'),=X'40'   CHECK TRT FUNCTION BYTE             
           LA  R6,TXTBUFR           GET BUFFER ADDRESS                          
           AH  R6,TXTBUFL           CALCULATE ADDRESS OF BUFFER END             
           SHI R6,1                    LESS 1 FOR LAST BYTE                     
           WHILE R1,(LT,CR),R6,AND,1(R1),EQ,X'40'                               
            AHI R1,1                POINT TO NEXT BYTE                          
           WEND                                                                 
           IF  R1,(EQ,CR),R6        IF END OF BUFFER                            
            IF  TXTREQU,NE,REQU_SCRIPTED  IF NOT SCRIPTING MODE                 
             MVI TXTRETN,AT_BUFEND    SIGNAL END OF BUFFER                      
            ELSE                     ELSE--SCRIPTING MODE                       
             MVIY TXTSRETN,S_AT_BUFEND  SIGNAL END OF BUFFER                    
            ENDIF                    ENDIF                                      
           ENDIF                    ENDIF                                       
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
*                                                                               
         LR    R3,R1              COPY DELIMITER ADDRESS                        
         SR    R3,R4              SUBTRACT STARTING POINT                       
         AHI   R3,1               PLUS 1 FOR ACTUAL LENGTH                      
         STH   R3,TXTSTRL         PUT IN STRING LENGTH FIELD                    
*                                                                               
         J     TXTCOPY            GO COPY PARSE TEXT                            
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTLEFT  EQU   *   LEFT JUSTIFY AND TRUNCATE REQUESTED STRING CONTENT           
* ------------------------------------------------------------------- *         
         LA    R4,TXTSTRG         GET ORIG STRING START                         
         LH    R5,TXTSTRL         GET ORIG STRING LENGTH                        
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         LR    R6,R4              COPY AS NEW STRING START                      
         WHILE 0(R6),NH,X'40'     IF CHAR NOT GREATER THAN SPACE                
          SHI  R5,1                DECR. FOR NEW STRING LENGTH                  
          AHI  R6,1                INCR. FOR NEW STRING START                   
         WEND                     LOOP                                          
         IF    R4,(LT,CR),R6      IF ORIG START LESS THAN NEW START             
          EXRL R5,MVCLEFT          SHIFT CONTENT TO THE LEFT                    
         ENDIF                    ENDIF                                         
*                                                                               
         IF    TXTDLMS,NE,X'FF'   IF DELIMITERS PROVIDED, STRIP 'EM             
          XC   TRTTABLE,TRTTABLE   CLEAR TRANSLATE TABLE                        
          LHI  R2,1                SET OPERATION CODE                           
          XR   R3,R3               CLEAR REGISTER                               
          LA   R6,TXTDLMS          GET ADDRESS OF DELIMITERS                    
          LHI  R7,8                LOOP FOR EIGHT DELIMITERS                    
          WHILE R7,(NZ,LTR),R7                                                  
           IC  R3,0(R6)            GET DELIMITER CHARACTER                      
           STC R2,TRTTABLE(R3)     SET OPER IN TRANSLATE & TEST TABLE           
           AHI R6,1                POINT TO NEXT DELIMITER                      
           SHI R7,1                DECREMENT LOOP COUNT                         
          WEND                                                                  
          XR   R1,R1               CLEAR DELIMETER ADDR REGISTER                
          XR   R2,R2               CLEAR DELIMETER CHAR REGISTER                
          LR   R6,R4               RESET STRING START                           
          LR   R7,R5               RESET STRING LENGTH (R5-1 HERE)              
          WHILE R7,(NZ,EXRL),FDELIM  FIND DELIMITER CHAR AND REMOVE IT          
           IF  $NOOP,H,$NOOP        IF DELIM FOUND AT HIGHEST POSITION          
            MVI 0(R1),X'40'          BLANK IT OUT                               
            WEXIT                    EXIT FROM LOOP                             
           ELSE                     ELSE ADDRESS OF DELIMITER                   
            AR R7,R6                 POINT TO END OF STRING                     
            LR R3,R7                 SAVE END-OF-STRING ADDRESS                 
            SR R7,R1                 SUBT DELIM POS FOR REM. LENGTH             
            LR R6,R1                 GET DELIMITER POSITION                     
            EXRL R7,RMVDELIM         SHIFT LEFT TO REMOVE DELIMITER             
            MVI 0(R3),X'40'          PUT A BLANK AT END                         
            SHI R7,1                 DECR. FOR REMOVED CHARACTER                
            IF R7,(M,LTR),R7         IF REL.LENGTH LESS THAN ZERO               
             WEXIT                    EXIT FROM LOOPOR NEXT TRT                 
            ENDIF                    ENDIFF MORE BYTES REMAIN                   
           ENDIF                    ENDIF                                       
          WEND                     LOOP                                         
         ENDIF                    END OF DELIMITERS PROVIDED                    
*                                                                               
         LH    R5,TXTSTRL         GET ORIG. STRING LENGTH                       
         AR    R4,R5              POINT TO ADDRESS *AFTER* STRING               
         SHI   R4,1               POINT TO LAST BYTE OF STRING                  
         WHILE R5,(P,LTR),R5,AND,0(R4),NH,X'40'                                 
          SHI  R4,1                DECREMENT STRING POINTER                     
          SHI  R5,1                DECREMENT STRING LENGTH                      
         WEND                                                                   
         STH   R5,TXTSTRL         PUT BACK IN PARM FIELD                        
*                                                                               
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
MVCLEFT  MVC   0(0,R4),0(R6)      SHIFT CONTENT TO THE LEFT                     
FDELIM   TRT   0(0,R6),TRTTABLE   FIND DELIMITER CHARS                          
RMVDELIM MVC   0(0,R6),1(R6)      REMOVE DELIMITER CHARACTER                    
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTRGHT  EQU   *   RIGHT JUSTIFY REQUESTED STRING CONTENT                       
* ------------------------------------------------------------------- *         
         LA    R4,TXTSTRG         GET ORIG. STRING START                        
         LH    R5,TXTSTRL         GET ORIG. STRING LENGTH                       
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         LR    R7,R4              COPY ORIG. STRING START                       
         AR    R7,R5              POINT TO END OF ORIG. STRING                  
         LR    R6,R7              COPY AS NEW STRING END                        
         WHILE 0(R6),NH,X'40'     IF CHAR NOT GREATER THAN SPACE                
          SHI  R6,1                DECREMENT END OF NEW STRING                  
         WEND                     LOOP                                          
         IF    R6,(LT,CR),R7      IF NEW END LESS THAN ORIG END                 
          WHILE R6,(NL,CR),R4      IF NEW END NOT LESS THAN ORIG. START         
           MVC 0(1,R7),0(R6)        SHIFT CHAR TO END OF ORIG. STRING           
           SHI R7,1                 DECREMENT END OF ORIG. STRING               
           SHI R6,1                 DECREMENT END OF NEW STRING                 
          WEND                     LOOP                                         
          WHILE R7,(NL,CR),R4      IF NEW END NOT LESS THAN ORIG. START         
           MVI 0(R7),X'40'          LEFT PAD WITH SPACE CHAR                    
           SHI R7,1                 DECREMENT END OF ORIG. STRING               
          WEND                     LOOP                                         
         ENDIF                    ENDIF                                         
*                                                                               
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE AND              
          IF TXTSREQU,(EQ,CLIY),STR_RJUSTIFY,BC,SETUP IF RIGHT, GET NXT         
          ENDIF                                                                 
         ELSE                     ELSE--LEGACY MODE                             
          IF   TXTREQU,EQ,REQU_RJUSTIFY,BC,SETUP   IF RIGHT, GET NEXT           
          ENDIF                                                                 
         ENDIF                                                                  
*                                 ELSE, PERFORM MID-JUSTIFY                     
         AHI   R5,1               INCR. STRING LEN FOR ACTUAL LENGTH            
         LR    R6,R4              SET SOURCE ADDRESS AS ORIG. START             
         LR    R7,R5              SET SOURCE LENGTH AS ORIG. LENGTH             
         WHILE 0(R6),NH,X'40'     IF CHAR NOT GREATER THAN SPACE                
          AHI  R6,1                INCREMENT NEW STRING START                   
          SHI  R7,1                DECREMENT NEW STRING LENGTH                  
         WEND                     LOOP                                          
         IF    R4,(LT,CR),R6      IF ORIG. START LESS THAN NEW START            
          LR   R2,R5               LENGTH OF LEFT SHIFT IS ORIG. LEN            
          SR   R2,R7                MINUS LENGTH OF NEW STRING                  
          SRL  R2,1                  DIVIDED BY 2                               
          AR   R4,R2               SET TARGET ADDRESS                           
          ICM  R7,B'1000',=X'40'   SET RIGHT PAD CHAR                           
          MVCL R4,R6               CENTER REQUESTED STRING CONTENT              
         ENDIF                    ENDIF                                         
*                                                                               
         J     SETUP              GO GET THE NEXT PARAMETER                     
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTTRIM  EQU   *   TRIM THE RIGHT SIDE OF INPUT STRING FOR SPACES               
* ------------------------------------------------------------------- *         
         LA    R4,TXTSTRG         GET ORIG STRING START                         
         LH    R2,TXTSTRL         GET ORIG STRING LENGTH                        
         AR    R4,R2              POINT TO ADDRESS *AFTER* STRING               
         SHI   R4,1               POINT TO LAST BYTE OF STRING                  
*                                                                               
         WHILE R2,(P,LTR),R2,AND,0(R4),EQ,X'40'                                 
          SHI  R2,1                DECREMENT STRING LENGTH                      
          SHI  R4,1                DECREMENT STRING POINTER                     
         WEND                                                                   
         STH   R2,TXTSTRL         PUT BACK IN PARM FIELD                        
*                                                                               
         J     SETUP              GO GET THE NEXT PARAMETER                     
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXT2ASC  EQU   *   TRANSLATE PARM STRING TO ASCII                               
* ------------------------------------------------------------------- *         
         LH    R5,TXTSTRL         GET STRING LENGTH                             
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         EXRL  R5,TXTRANA         TRANSLATE STRING TO ASCII                     
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
TXTRANA  TR    TXTSTRG(0),TRASCII (SEE "EXRL" INSTRUCTION ABOVE)                
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXT2EBC  EQU   *   TRANSLATE PARM STRING TO EBCDIC                              
* ------------------------------------------------------------------- *         
         LH    R5,TXTSTRL         GET STRING LENGTH                             
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         EXRL  R5,TXTRANE         TRANSLATE STRING TO EBCDIC                    
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
TXTRANE  TR    TXTSTRG(0),TREBCDIC (SEE "EXRL" INSTRUCTION ABOVE)               
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTMASK  EQU   *   MASK NON-PRINTABLE CHARS WITH PERIODS                        
* ------------------------------------------------------------------- *         
         MVC   TRTTABLE,NOTRANS   GET DEFAULT TRANSLATE TABLE                   
         MVC   TRTTABLE+X'00'(L'PERIODS),PERIODS   SET NON-PRINT MASK           
         MVI   TRTTABLE+X'FF',C'.'   PLUS THE HIGH-VALUE POSITION               
*                                                                               
         LH    R5,TXTSTRL         GET STRING LENGTH                             
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         EXRL  R5,TXTRANSL        TRANSLATE STRING USING MASK                   
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
TXTRANSL TR    TXTSTRG(0),TRTTABLE (SEE "EXRL" INSTR. ABOVE & BELOW)            
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTCASE  EQU   *   TRANSLATE PARM STRING CASE                                   
* ------------------------------------------------------------------- *         
         MVC   TRTTABLE,NOTRANS   GET DEFAULT TRANSLATE TABLE                   
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE                  
          IF   TXTSREQU,(NE,CLIY),STR_UCASE,BC,TXT_DO_LOWER                     
          ENDIF                    ENDIF                                        
         ELSE                     ELSE--LEGACY MODE                             
          IF   TXTREQU,NE,REQU_UCASE,BC,TXT_DO_LOWER                            
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
TXT_DO_UPPER DS 0H                                                              
         MVC   TRTTABLE+C'a'(9),=C'ABCDEFGHI'   UPPER-CASE MASK                 
         MVC   TRTTABLE+C'j'(9),=C'JKLMNOPQR'   UPPER-CASE MASK                 
         MVC   TRTTABLE+C's'(8),=C'STUVWXYZ'    UPPER-CASE MASK                 
         J     TXT_DO_CASE                                                      
TXT_DO_LOWER DS 0H                                                              
         MVC   TRTTABLE+C'A'(9),=C'abcdefghi'   LOWER-CASE MASK                 
         MVC   TRTTABLE+C'J'(9),=C'jklmnopqr'   LOWER-CASE MASK                 
         MVC   TRTTABLE+C'S'(8),=C'stuvwxyz'    LOWER-CASE MASK                 
TXT_DO_CASE  DS 0H                                                              
         LH    R5,TXTSTRL         GET STRING LENGTH                             
         SHI   R5,1                LESS 1 FOR ZERO REFERENCE                    
         EXRL  R5,TXTRANSL        TRANSLATE STRING CASE                         
*                                                                               
         IF    TXTREQU,EQ,REQU_SCRIPTED   IF IN SCRIPTING MODE AND              
          IF   TXTSREQU,(NE,CLIY),STR_WCAPS,BC,SETUP  NOT WCAPS,GET NXT         
          ENDIF                    ENDIF                                        
         ELSE                     ELSE--LEGACY MODE                             
          IF   TXTREQU,NE,REQU_WCAPS,BC,SETUP   NOT WCAPS, GET NXT PARM         
          ENDIF                    ENDIF                                        
         ENDIF                    ENDIF                                         
*                                                                               
         XC    TRTTABLE,TRTTABLE  DO WORD CAPS                                  
         XR    R1,R1              CLEAR REGISTER                                
         LA    R6,TXTDLMS         GET ADDRESS OF DELIMITERS                     
         LHI   R7,8               LOOP FOR EIGHT DELIMITERS                     
         WHILE R7,(NZ,LTR),R7                                                   
          IC   R1,0(R6)           GET DELIMITER CHARACTER                       
          LA   R2,TRTTABLE(R1)    POINT TO TABLE POSITION                       
          MVI  0(R2),X'FF'        SET FUNCTION BYTE VALUE                       
          AHI  R6,1               POINT TO NEXT DELIMITER                       
          SHI  R7,1               DECREMENT LOOP COUNT                          
         WEND                                                                   
*                                                                               
         LA    R4,TXTSTRG         GET STRING START FOR POINTER                  
         LR    R6,R4              COPY STRING POINTER                           
         AR    R6,R5              THEN POINT TO STRING END                      
         AHI   R5,1               ADD BACK 1 SUBTRACTED ABOVE                   
         IF    R5,(P,LTR),R5      IF THERE IS STRING REMAINING                  
          JAS  R2,TXTCASEW         CHECK LOWER-CASE TO CAPITALIZE               
         ENDIF                    ENDIF                                         
         WHILE R4,(NH,CR),R6      IF CURR. POINTER <= END POINTER               
          IF   R5,(M,LTR),R5       IF NO STRING REMAINING                       
           WEXIT                    GET OUT                                     
          ELSE                     ELSE--SEARCH FOR DELIMITER                   
           IF  R5,(Z,EXRL),WORDTRT  IF NO DELIMITER FOUND                       
            WEXIT                    GET OUT                                    
           ELSE                     ELSE--DELIMITER FOUND                       
            LR R7,R1                 GET DELIMITER ADDRESS                      
            SR R7,R4                 ZERO-BASED LENGTH TRAVELED                 
            AHI R7,1                  PLUS 1 FOR REAL LENGTH                    
            SR R5,R7                 DECR. REMAINING STRING LEN                 
            LA R4,1(,R1)             GET POS. AFTER DELIMITER                   
            SHI R5,1                 DECR. REMAINING STRING LEN                 
            IF R5,(P,LTR),R5         IF THERE IS STRING REMAINING               
             JAS R2,TXTCASEW          CHECK LOWER-CASE TO CAPITALIZE            
            ENDIF                    ENDIF                                      
           ENDIF                    ENDIF R7,(NZ,EXRL),WORDTRT                  
          ENDIF                    ENDIF R5,(P,LTR),R5                          
         WEND                     END WHILE R4,(NH,CR),R6                       
*                                                                               
         J     SETUP              GO GET THE NEXT PARAMETER                     
*                                                                               
WORDTRT  TRT   0(0,R4),TRTTABLE   (SEE "EXRL" INSTRUCTION ABOVE)                
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXTCASEW EQU   *   CHECK FOR LOWER-CASE TO CAPITALIZE                           
* ------------------------------------------------------------------- *         
         SELECT                   CHECK FOR LOWER-CASE                          
         WHEN  0(R4),NL,X'81',AND,0(R4),NH,X'89'                                
          OI   0(R4),X'40'         CAPITALIZE CHARACTER                         
          AHI  R4,1                SKIP CAPITALIZED CHARACTER                   
          SHI  R5,1                DECR. REMAINING STRING LEN                   
         WHEN  0(R4),NL,X'91',AND,0(R4),NH,X'99'                                
          OI   0(R4),X'40'         CAPITALIZE CHARACTER                         
          AHI  R4,1                SKIP CAPITALIZED CHARACTER                   
          SHI  R5,1                DECR. REMAINING STRING LEN                   
         WHEN  0(R4),NL,X'A2',AND,0(R4),NH,X'A9'                                
          OI   0(R4),X'40'         CAPITALIZE CHARACTER                         
          AHI  R4,1                SKIP CAPITALIZED CHARACTER                   
          SHI  R5,1                DECR. REMAINING STRING LEN                   
         WHEN  OTHER               OTHER CASE?                                  
         ENDSL                    ENDSL                                         
*                                                                               
         BR    R2                 RETURN TO INTERNAL CALLER                     
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXT2B64  EQU   *   TRANSLATE BINARY PARM STRING(S) TO BASE64                    
* ------------------------------------------------------------------- *         
         LHI   R0,1               START AT 1ST POSITION                         
         STH   R0,TXTPNTR          OF OUTPUT BUFFER                             
         LA    R3,B64TBL          PRE-LOAD ADDRESS OF ENCODING TABLE            
         LA    R6,TXTBUFR         OUTPUT BUFFER ADDRESS                         
         LH    R7,TXTBUFL         OUTPUT BUFFER LENGTH                          
         IF    R7,(HT,CH),=H'9992'   IF GREATER THAN MAXIMUM ALLOWED            
          LHI  R7,9992             SET MAXIMUM OUTPUT BUFFER LENGTH             
          STH  R7,TXTBUFL          PUT BACK IN PARM FIELD                       
         ENDIF                    ENDIF                                         
*                                                                               
         WHILE R0,(LT,CR),R7      OUTER LOOP UNTIL BUFFER IS FULL               
          LA   R4,TXTSTRG          GET ORIG STRING START                        
          ST   R4,STRGADDR         SAVE CURRENT STRING POINTER                  
          XR   R5,R5               STARTING FROM ZERO                           
          STH  R5,STRGLEN          SET USED STRING LENGTH COUNTER               
          WHILE R0,(LT,CR),R7      INNER LOOP UNTIL BUFFER IS FULL              
           LH  R2,TXTSTRL           GET ORIGINAL STRING LENGTH                  
           SH  R2,STRGLEN           SUBTRACT USED STRING LENGTH                 
           IF  R2,(HT,CH),=H'3'     IF BYTES REMAINING > 3                      
            LH R2,=H'3'              SET MAXIMUM BINARY CHUNK SIZE              
           ENDIF                    ENDIF                                       
           IF  R2,(NP,LTR),R2       IF NOTHING REMAINING                        
            WEXIT                    EXIT FOR NEXT STRING                       
           ENDIF                                                                
           L   R4,STRGADDR          GET CURRENT STRING POINTER                  
           LH  R5,STRGLEN           GET USED STRING LENGTH                      
           XC  BINCHUNK,BINCHUNK    CLEAR 3-BYTE BINARY CHUNK                   
           STH R2,BINCNT            SET LENGTH OF BINARY CHUNK                  
           SHI R2,1                 LESS 1 FOR ZERO REFERENCE                   
           EXRL R2,BIN3MVC          GET UP TO 3-BYTE CHUNK                      
           AHI R2,1                 PLUS 1 FOR ORIGINAL LENGTH                  
           AR  R4,R2                INCREMENT INPUT STRING POINTER              
           AR  R5,R2                INCREMENT USED STRING LENGTH                
           ST  R4,STRGADDR          SAVE INPUT STRING POINTER                   
           STH R5,STRGLEN           SAVE USED STRING LENGTH                     
           JAS R2,ENCTO64           PERFORM ENCODING TO BASE64                  
           STH R0,TXTPNTR           PUT BACK IN PNTR FIELD                      
          WEND                     ENDWHILE R7,(LT,CH),TXTBUFL                  
          JAS  R2,GETSTRG          GET (NEXT) STRING PARAMETER                  
         WEND                     ENDWHILE R7,(LT,CH),TXTBUFL                   
*                                                                               
         MVI   TXTRETN,BUF_FULL   SIGNAL BUFFER IS FULL                         
         J     RETURN             GO RETURN TO ORIG CALLER                      
*                                                                               
BIN3MVC  MVC   BINCHUNK(0),0(R4)  (SEE "EXRL" INSTRUCTION ABOVE)                
         EJECT                                                                  
* ------------------------------------------------------------------- *         
TXT4B64  EQU   *   TRANSLATE BASE64 PARM STRING(S) TO BINARY                    
* ------------------------------------------------------------------- *         
         LHI   R0,1               START AT 1ST POSITION                         
         STH   R0,TXTPNTR          OF OUTPUT BUFFER                             
         LA    R3,BINTBL          PRE-LOAD ADDRESS OF DECODING TABLE            
         LA    R6,TXTBUFR         OUTPUT BUFFER ADDRESS                         
         LH    R7,TXTBUFL         OUTPUT BUFFER LENGTH                          
         IF    R7,(HT,CH),=H'9993'   IF GREATER THAN MAXIMUM ALLOWED            
          LHI  R7,9993             SET MAXIMUM OUTPUT BUFFER LENGTH             
          STH  R7,TXTBUFL          PUT BACK IN PARM FIELD                       
         ENDIF                    ENDIF                                         
*                                                                               
         WHILE R0,(LT,CR),R7      OUTER LOOP UNTIL BUFFER IS FULL               
          LA   R4,TXTSTRG          GET ORIG STRING START                        
          ST   R4,STRGADDR         SAVE CURRENT STRING POINTER                  
          XR   R5,R5               STARTING FROM ZERO                           
          STH  R5,STRGLEN          SET USED STRING LENGTH COUNTER               
          WHILE R0,(LT,CR),R7      INNER LOOP UNTIL BUFFER IS FULL              
           LH  R2,TXTSTRL           GET ORIGINAL STRING LENGTH                  
           IF  R2,(NZ,TMLL),3       IF NOT MULTIPLE OF 4                        
            MVI TXTRETN,INV_STRL     SIGNAL STRING LENGTH ERROR                 
            J  RETURN                GO RETURN TO ORIG CALLER                   
           ENDIF                    ENDIF                                       
           SH  R2,STRGLEN           SUBTRACT USED STRING LENGTH                 
           IF  R2,(HT,CH),=H'4'     IF BYTES REMAINING > 4                      
            LH R2,=H'4'              SET MAXIMUM BASE64 CHUNK SIZE              
           ENDIF                    ENDIF                                       
           IF  R2,(NP,LTR),R2       IF NOTHING REMAINING                        
            WEXIT                    EXIT FOR NEXT STRING                       
           ENDIF                                                                
           L   R4,STRGADDR          GET CURRENT STRING POINTER                  
           LH  R5,STRGLEN           GET USED STRING LENGTH                      
           XC  B64CHUNK,B64CHUNK    CLEAR 4-BYTE BASE64 CHUNK                   
           SHI R2,1                 LESS 1 FOR ZERO REFERENCE                   
           EXRL R2,B643MVC          GET UP TO 4-BYTE CHUNK                      
           AHI R2,1                 PLUS 1 FOR ORIGINAL LENGTH                  
           AR  R4,R2                INCREMENT INPUT STRING POINTER              
           AR  R5,R2                INCREMENT USED STRING LENGTH                
           ST  R4,STRGADDR          SAVE INPUT STRING POINTER                   
           STH R5,STRGLEN           SAVE USED STRING LENGTH                     
           JAS R2,DECFR64           PERFORM DECODING FROM BASE64                
           STH R0,TXTPNTR           PUT BACK IN PNTR FIELD                      
          WEND                     ENDWHILE R7,(LT,CH),TXTBUFL                  
          JAS  R2,GETSTRG          GET (NEXT) STRING PARAMETER                  
         WEND                     ENDWHILE R7,(LT,CH),TXTBUFL                   
*                                                                               
         MVI   TXTRETN,BUF_FULL   SIGNAL BUFFER IS FULL                         
         J     RETURN             GO RETURN TO ORIG CALLER                      
*                                                                               
B643MVC  MVC   B64CHUNK(0),0(R4)  (SEE "EXRL" INSTRUCTION ABOVE)                
         EJECT                                                                  
* ------------------------------------------------------------------- *         
ENCTO64  EQU   *   ENCODE 3-BYTE CHUNK INTO 4-BYTE BASE64 OUTPUT                
* ------------------------------------------------------------------- *         
* R0 = output buffer pointer                                                    
* R2 = return address                                                           
* R3 = address of 64-byte base 64 table                                         
* BINCHUNK is the 3 bytes to encode                                             
* BINCNT   is the actual number of bytes (1-3) in chunk                         
* R6 = address of current position in output buffer                             
*                                                                               
         XR    R4,R4                   CLEAR WORK REGISTER                      
         ICM   R4,B'0111',BINCHUNK     LOAD 3 BYTES (OR 1-2 + x'00's)           
         LR    R5,R4                   CHARACTER 1 (Always generated)           
         SRL   R5,18                   KEEP TOP 6 BITS                          
         IC    R5,0(R5,R3)             LOOKUP B64 REPLACEMENT                   
         STC   R5,0(,R6)               STORE FIRST BYTE                         
         LR    R5,R4                   CHARACTER 2 (Always generated)           
         SRL   R5,12                   SHIFT OUT LOW 12 BITS                    
         N     R5,=X'0000003F'         KEEP REMAINING 6 BITS                    
         IC    R5,0(R5,R3)             LOOKUP B64 REPLACEMENT                   
         STC   R5,1(,R6)               STORE SECOND BYTE                        
*                                                                               
         IF    =H'3',(HT,CLC),BINCNT   IF STARTED WITH < 3 BYTES                
          IF   =H'2',(EQ,CLC),BINCNT    IF STARTED WITH 2 BYTES                 
           LR  R5,R4                     CHARACTER 3                            
           SRL R5,6                      SHIFT OUT LOW 6 BITS                   
           N   R5,=X'0000003F'           KEEP REMAINING 6 BITS                  
           IC  R5,0(R5,R3)               LOOKUP B64 REPLACEMENT                 
           STC R5,2(,R6)                 STORE THIRD BYTE                       
           MVI 3(R6),C'='                PADDING FOR FOURTH BYTE                
          ELSE                          ELSE--ONLY 1 BYTE                       
           MVI 2(R6),C'='                PADDING FOR THIRD BYTE                 
           MVI 3(R6),C'='                PADDING FOR FOURTH BYTE                
          ENDIF                         ENDIF                                   
         ELSE                          ELSE--3 BYTES PRESENT                    
          LR   R5,R4                    CHARACTER 3                             
          SRL  R5,6                     SHIFT OUT LOW 6 BITS                    
          N    R5,=X'0000003F'          KEEP REMAINING 6 BITS                   
          IC   R5,0(R5,R3)              LOOKUP B64 REPLACEMENT                  
          STC  R5,2(,R6)                STORE THIRD BYTE                        
          LR   R5,R4                    CHARACTER 4                             
          N    R5,=X'0000003F'          KEEP REMAINING 6 BITS                   
          IC   R5,0(R5,R3)              LOOKUP B64 REPLACEMENT                  
          STC  R5,3(,R6)                STORE FOURTH BYTE                       
         ENDIF                         ENDIF                                    
*                                                                               
         AHI   R6,4                    INCREMENT OUTPUT BUFFER ADR              
         AHI   R0,4                    INCREMENT OUTPUT BUFFER PTR              
         BR    R2                      RETURN FROM ENCODING CHUNK               
         EJECT                                                                  
* ------------------------------------------------------------------- *         
DECFR64  EQU   *   DECODE 4-BYTE CHUNK INTO 3-BYTE BINARY OUTPUT                
* ------------------------------------------------------------------- *         
* R0 = output buffer pointer                                                    
* R2 = return address                                                           
* R3 = address of 256-byte binary table                                         
* B64CHUNK is the 4 bytes to encode                                             
* R6 = address of current position in output buffer                             
*                                                                               
         XR    R4,R4                   CLEAR FOR 24-BIT CONSTRUCTION            
         XR    R5,R5                   CLEAR BYTE 1                             
         IC    R5,B64CHUNK+0           GET CHAR 1                               
         IC    R5,0(R5,R3)             LOOKUP 0-63 REPLACEMENT                  
         SLL   R5,18                   SHIFT TO (3*6) 1ST POSITION              
         OR    R4,R5                   OVERLAY INTO R4                          
         XR    R5,R5                   CLEAR BYTE 2                             
         IC    R5,B64CHUNK+1           GET CHAR 2                               
         IC    R5,0(R5,R3)             LOOKUP 0-63 REPLACEMENT                  
         SLL   R5,12                   SHIFT TO (2*6) 2ND POSITION              
         OR    R4,R5                   OVERLAY INTO R4                          
*                                                                               
         IF    B64CHUNK+2,EQ,C'='      IF 3RD CHAR IS PADDING                   
          STCM R4,B'0100',0(R6)         STORE TOP 1 BYTE TO BIN BUFR            
          AHI  R6,1                     INCREMENT OUTPUT BUFFER ADR             
          AHI  R0,1                     INCREMENT OUTPUT BUFFER PTR             
         ELSE                          ELSE--USE 3RD BYTE                       
          XR   R5,R5                    CLEAR BYTE 3                            
          IC   R5,B64CHUNK+2            GET CHAR 3                              
          IC   R5,0(R5,R3)              LOOKUP 0-63 REPLACEMENT                 
          SLL  R5,6                     SHIFT TO (1*6) 3RD POSITION             
          OR   R4,R5                    OVERLAY INTO R4                         
          IF   B64CHUNK+3,EQ,C'='       IF 4TH CHAR IS PADDING                  
           STCM R4,B'0110',0(R6)         STORE TOP 2 BYTES TO BIN BUFR          
           AHI R6,2                      INCREMENT OUTPUT BUFFER ADR            
           AHI R0,2                      INCREMENT OUTPUT BUFFER PTR            
          ELSE                          ELSE--USE 4TH BYTE                      
           XR  R5,R5                     CLEAR BYTE 4                           
           IC  R5,B64CHUNK+3             GET CHAR 4                             
           IC  R5,0(R5,R3)               LOOKUP 0-63 REPLACEMENT                
           OR  R4,R5                     OVERLAY INTO R4 (4TH POSITION)         
           STCM R4,B'0111',0(R6)         STORE ALL 3 BYTES TO BIN BUFR          
           AHI R6,3                      INCREMENT OUTPUT BUFFER ADR            
           AHI R0,3                      INCREMENT OUTPUT BUFFER PTR            
          ENDIF                         ENDIF                                   
         ENDIF                         ENDIF                                    
*                                                                               
         BR    R2                      RETURN FROM DECODING CHUNK               
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
STORAGE  DATAAREA                                                               
* ------------------------------------------------------------------- *         
NOTRANS  DS    0D                      DEFAULT TRANSLATION TABLE                
         DC    256AL1(*-NOTRANS)       INITIALIZE FOR NO TRANSLATION            
         ORG                                                                    
TRASCII  DS    0D                      EBCDIC TO ASCII TRANSLATE TABLE          
*              X'000102030405060708090A0B0C0D0E0F'                              
*                                                                               
         DC    X'00010203EC09CA1CE2D2D30B0C0D0E0F'   X'00'                      
         DC    X'10111213EFC508CB1819DCD81A1D1E1F'   X'10'                      
         DC    X'B7B8B9BBC40A171BCCCDCFD0D1050607'   X'20'                      
         DC    X'D9DA16DDDEDFE004E3E5E9EBB0B19E7F'   X'30'                      
         DC    X'20FF838485A0F28687A49B2E3C282B7C'   X'40'                      
         DC    X'268288898AA18C8B8DE121242A293BAA'   X'50'                      
         DC    X'2D2FB28EB4B5B68F80A5B32C255F3E3F'   X'60'                      
         DC    X'BA90BCBDBEF3C0C1C2603A2340273D22'   X'70'                      
         DC    X'C3616263646566676869AEAFC6C7C8F1'   X'80'                      
         DC    X'F86A6B6C6D6E6F707172A6A791CE92A9'   X'90'                      
         DC    X'E67E737475767778797AADA8D4D5D6D7'   X'A0'                      
         DC    X'5E9C9DFA9F1514ACABFC5B5DE4FEBFE7'   X'B0'                      
         DC    X'7B414243444546474849E8939495A2ED'   X'C0'                      
         DC    X'7D4A4B4C4D4E4F505152EE968197A398'   X'D0'                      
         DC    X'5CF6535455565758595AFDF599F7F0F9'   X'E0'                      
         DC    X'30313233343536373839DBFB9AF4EAC9'   X'F0'                      
         ORG                                                                    
TREBCDIC DS    0D                      ASCII TO EBCDIC TRANSLATE TABLE          
*              X'000102030405060708090A0B0C0D0E0F'                              
*                                                                               
         DC    X'00010203372D2E2F1605250B0C0D0E0F'   X'00'                      
         DC    X'10111213B6B5322618191C27071D1E1F'   X'10'                      
         DC    X'405A7F7B5B6C507D4D5D5C4E6B604B61'   X'20'                      
         DC    X'F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F'   X'30'                      
         DC    X'7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'   X'40'                      
         DC    X'D7D8D9E2E3E4E5E6E7E8E9BAE0BBB06D'   X'50'                      
         DC    X'79818283848586878889919293949596'   X'60'                      
         DC    X'979899A2A3A4A5A6A7A8A9C04FD0A13F'   X'70'                      
         DC    X'68DC5142434447485253545756586367'   X'80'                      
         DC    X'719C9ECBCCCDDBDDDFECFC4AB1B23EB4'   X'90'                      
         DC    X'4555CEDE49699A9BAB9F5FB8B7AA8A8B'   X'A0'                      
         DC    X'3C3D626A6465662021227023727374BE'   X'B0'                      
         DC    X'7677788024158C8D8EFF061728299D2A'   X'C0'                      
         DC    X'2B2C090AACADAEAF1B3031FA1A333435'   X'D0'                      
         DC    X'36590838BC39A0BFCA3AFE3B04CFDA14'   X'E0'                      
         DC    X'EE8F4675FDEBE1ED90EFB3FBB9EABD41'   X'F0'                      
         ORG                                                                    
PERIODS  DS    0CL64                   NON-PRINTABLE MASK                       
         DC    64CL1'.'                                                         
*                                                                               
B64TBL   DS    0CL64                   BINARY TO BASE64 LOOKUP TABLE            
         DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         DC    C'abcdefghijklmnopqrstuvwxyz'                                    
         DC    C'0123456789+/'                                                  
BINTBL   DS    0D                      BASE64 TO BINARY LOOKUP TABLE            
         DC    256AL1(X'FF')           INIT ALL SLOTS 'INVALID' X'FF'           
         ORG   BINTBL+C'A'             Redefine position for A-I                
         DC    X'000102030405060708'                                            
         ORG   BINTBL+C'J'             Redefine position for J-R                
         DC    X'090A0B0C0D0E0F1011'                                            
         ORG   BINTBL+C'S'             Redefine position for S-Z                
         DC    X'1213141516171819'                                              
         ORG   BINTBL+C'a'             Redefine position for a-i                
         DC    X'1A1B1C1D1E1F202122'                                            
         ORG   BINTBL+C'j'             Redefine position for j-r                
         DC    X'232425262728292A2B'                                            
         ORG   BINTBL+C's'             Redefine position for s-z                
         DC    X'2C2D2E2F30313233'                                              
         ORG   BINTBL+C'0'             Redefine position for 0-9                
         DC    X'3435363738393A3B3C3D'                                          
         ORG   BINTBL+C'+'             Redefine position for +                  
         DC    X'3E'                                                            
         ORG   BINTBL+C'/'             Redefine position for /                  
         DC    X'3F'                                                            
         ORG   BINTBL+C'='             Redefine position for padding            
         DC    X'00'                   Treat padding as 0                       
         ORG                                                                    
*                                                                               
         LTORG                                                                  
* ------------------------------------------------------------------- *         
         DATAEND                                                                
* =================================================================== *         
         END                                                                    
