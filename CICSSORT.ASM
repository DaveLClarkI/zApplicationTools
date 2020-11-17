*ASM XOPTS(NOPROLOG NOEPILOG)                                                   
SORT     TITLE 'CICSSORT - A CICS UTILITY PROGRAM - CICS UPDATE, 11/90' 00002000
CICSSORT AMODE ANY                                                              
CICSSORT RMODE ANY                                                              
         PRINT ON,GEN                                                   00000200
* CHANGE HISTORY ------------------------------------------------------ 00000200
* 11/02/2002 DLC FIRST INSTALLED, PLUS MINOR CHANGES                    00000200
* 01/28/2009 DLC BETTER MAXIMUM STORAGE CHECKING                        00000200
* END OF HISTORY ------------------------------------------------------ 00000200
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00001400
*                               PURPOSE                               * 00001500
*                                                                     * 00001600
*      This  utility  program  provides  CICS  applications  with the * 00005000
* ability to perform a  SORT  operation,  a  function which cannot be * 00006000
* explicitly performed through CICS commands.                         * 00007000
*                                                                     * 00008000
* =================================================================== * 00002100
*                                                                     * 00002200
*                           IMPLEMENTATION                            * 00002300
*                                                                     * 00002000
*      A 'Quick-Sort' algorithm  is  used  to  carry  out the sorting * 00009000
* process.   The main  factor  in  measuring  sort  algorithms is the * 00010000
* number of comparisons  needed  to  perform  the sort.   This number * 00011000
* determines directly the number of  units  of time to accomplish the * 00012000
* sort.   The  traditional  'Bubble-Sort'  requires  about  n*(n-1)/2 * 00013000
* comparisons and thus should take a time roughly proportional to n2, * 00014000
* while the  algorithm  used  in  this  program  requires about log2n * 00015000
* comparisons, where n is the number of records to be sorted.         * 00016000
*                                                                     * 00018000
*      The input to this program is a temporary storage queue,  which * 00019000
* has to be  created  by  the  task  requesting the sort.   The input * 00020000
* records have to be written to this queue using EXEC CICS WRITEQ TS. * 00021000
* Each record will be an  item in the queue.   Normally,  queue names * 00022000
* would consist  of  the  current  terminal-id  concatenated with the * 00023000
* current transaction-id (i.e., EIBTRMID concatenated with EIBTRNID), * 00024000
* but any queue name may  be  used.   The  output is also a temporary * 00025000
* storage queue which will include  the sorted records.   They can be * 00026000
* retrieved using EXEC CICS READQ  TS.   The name of the output queue * 00027000
* is composed  from  the  input  queue  name  and  is  the first four * 00028000
* characters swapped with the last four characters of that name.   As * 00029000
* this program does  not  delete  either  temporary  storage queue at * 00032000
* program termination,  the invoking program must delete these queues * 00032000
* as soon as either of them are no longer needed.                     * 00032000
*                                                                     * 00032000
*      The invoking COBOL program should  use the following data area * 00005000
* for passing parameters:                                             * 00005100
*                                                                     * 00005200
*      01  CICSSORT-PARMS.                                            * 00001000
*        03  CICSSORT-QNAME            PIC  X(8).                     * 00091000
*        03  CICSSORT-RECCNT           PIC  9(4)    COMP.             * 00094000
*        03  CICSSORT-RECLEN           PIC  9(4)    COMP.             * 00095000
*        03  CICSSORT-KEYOFF           PIC  9(4)    COMP.             * 00096000
*        03  CICSSORT-KEYLEN           PIC  9(4)    COMP.             * 00097000
*        03  CICSSORT-SRTDIR           PIC  X(1)    VALUE 'A'.        * 00098000
*          88  CICSSORT-ASCENDING                   VALUE 'A'.        * 00102000
*          88  CICSSORT-DESCENDING                  VALUE 'D'.        * 00103000
*        03  FILLER                    PIC  X(1).                     * 00099000
*        03  CICSSORT-RC               PIC  9(4)    COMP.             * 00100000
*          88  CICSSORT-OK                          VALUE 0.          * 00101000
*          88  CICSSORT-BAD-NAME                    VALUE 1.          * 00102000
*          88  CICSSORT-BAD-QUEUE                   VALUE 2.          * 00103000
*          88  CICSSORT-BAD-NUMBER                  VALUE 3.          * 00104000
*          88  CICSSORT-BAD-SRTTYP                  VALUE 4.          * 00105000
*          88  CICSSORT-BAD-RECLEN                  VALUE 5.          * 00106000
*          88  CICSSORT-BAD-KEYLEN                  VALUE 6.          * 00107000
*          88  CICSSORT-BAD-GETMAIN                 VALUE 7.          * 00108000
*          88  CICSSORT-UNKNOWN-ERROR               VALUE 10.         * 00109000
*                                                                     * 00006000
*      A description of the fields in this data area follows:         * 00055000
*                                                                     * 00056000
* CICSSORT-QNAME is the name of  the temporary storage input queue as * 00057000
* described above.   An error in this field returns 1 in CICSSORT-RC. * 00058000
* In the case of the input queue not existing,  i.e.,  this queue has * 00059000
* not been created by the user, 2 is returned in CICSSORT-RC.         * 00060000
*                                                                     * 00062000
* CICSSORT-RECCNT is the number of records in the input queue.  There * 00063000
* must be at least 2 items.   The actual number of items in the input * 00064000
* queue is compared to this value.  Any error in this field returns 3 * 00065000
* in CICSSORT-RC.   (When the  software  was originally received,  it * 00066000
* enforced an upper  limit  of  1000  items.   This was suppressed in * 00067000
* 8/93.)                                                              * 00068000
*                                                                     * 00069000
* CICSSORT-RECLEN is the length of the input records.   As the length * 00070000
* can be variable,  this value is  the length of the smallest record. * 00071000
* An error in this field returns 5 in CICSSORT-RC.                    * 00072000
*                                                                     * 00074000
* CICSSORT-KEYOFF is the offset of the sorting key from the beginning * 00075000
* of an input record.   This value may vary from 0 to CICSSORT-RECLEN * 00076000
* minus 1.                                                            * 00077000
*                                                                     * 00078000
* CICSSORT-KEYLEN is the key length.   This  value may vary from 1 to * 00079000
* 255.  An error in this field returns 6 in CICSSORT-RC.              * 00080000
*                                                                     * 00081000
* CICSSORT-SRTDIR is the requested sort  type:  A  - Ascending or D - * 00082000
* Descending.  Errors in this field return 4 in CICSSORT-RC.          * 00083000
*                                                                     * 00084000
* Failure to  get  CICS  storage  will  return  7 in CICSSORT-RC,  an * 00085000
* unexpected error will  return  10 in CICSSORT-RC,  while successful * 00086000
* completion of the sort  operation  will cause the temporary storage * 00087000
* output queue to  be  built  and  a  value  of  0  to be returned in * 00088000
* CICSSORT-RC.                                                        * 00089000
*                                                                     * 00089000
* =================================================================== * 00011400
*                                                                     * 00011500
*                             INVOCATION                              * 00011600
*                                                                     * 00011700
*      At our installation,  the program is  named CICSSORT and it is * 00033000
* invoked in the following manner:                                    * 00034000
*                                                                     * 00035000
*          STRING EIBTRMID 'SORT'                                     * 00040000
*              DELIMITED BY SIZE  INTO CICSSORT-QNAME.                * 00040000
*          MOVE TS-RECORD-COUNT     TO CICSSORT-RECCNT.               * 00041000
*          MOVE LENGTH OF TS-RECORD TO CICSSORT-RECLEN.               * 00042000
*          MOVE         +0          TO CICSSORT-KEYOFF.               * 00043000
*          MOVE LEGNTH OF TS-KEYFLD TO CICSSORT-KEYLEN.               * 00044000
*          SET  CICSSORT-ASCENDING  TO TRUE.                          * 00045000
*                                                                     * 00046000
*          EXEC CICS LINK PROGRAM  ('CICSSORT')                       * 00047000
*                         COMMAREA (CICSSORT-PARMS)                   * 00048000
*                         END-EXEC.                                   * 00050000
*                                                                     * 00051000
*          IF  NOT CICSSORT-OK                                        * 00052000
*     *        <error logic here>                                     * 00053000
*          ELSE                                                       * 00053000
*     *        <first, delete old queue>                              * 00040000
*              STRING EIBTRMID 'SORT'                                 * 00040000
*                  DELIMITED BY SIZE  INTO CICSSORT-QNAME             * 00040000
*              EXEC CICS DELETEQ TS                                   * 00053000
*                        QUEUE(CICSSORT-QNAME)                        * 00053000
*                        NOHANDLE                                     * 00053000
*              END-EXEC                                               * 00053000
*     *        <then, rebuild new queue name>                         * 00040000
*              STRING 'SORT' EIBTRMID                                 * 00040000
*                  DELIMITED BY SIZE  INTO CICSSORT-QNAME             * 00040000
*     *        <output queue processing here>                         * 00053000
*          END-IF.                                                    * 00053000
*                                                                     * 00054000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
         EJECT                                                                  
* =================================================================== *         
COMSAVE  DSECT                         COMMUNICATION AREA                       
         USING COMSAVE,COMPTR                                                   
* ------------------------------------------------------------------- *         
COMTSNAM DS    CL8                                                      00045000
COMTSNUM DS    H                                                        00046000
COMRECLN DS    H                                                        00047000
COMKEYOF DS    H                                                        00048000
COMKEYLN DS    H                                                        00049000
COMSRTYP DS    C                                                        00050000
COMFIL   DS    C                                                        00051000
COMANSWR DS    H                                                        00052000
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
TSAREA   DSECT                         TEMP STORAGE I/O AREA            00055000
         USING TSAREA,TSPTR                                             00054000
* ------------------------------------------------------------------- *         
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
TABAREA  DSECT                         TABLE AREA (TO SORT KEYS)        00058000
         USING TABAREA,TABPTR                                           00057000
* ------------------------------------------------------------------- *         
* =================================================================== *         
         EJECT                                                          00012700
* =================================================================== * 00022600
STCKAREA DSECT                         STACK AREA (FOR QUICK SORT)      00061000
         USING STCKAREA,STCKPTR                                         00060000
* ------------------------------------------------------------------- *         
* =================================================================== * 00022600
         EJECT                                                                  
* =================================================================== *         
DFHEISTG DSECT                         EXECUTE INTERFACE + USER STORAGE         
* ------------------------------------------------------------------- *         
         DFHEISTG                                                               
*                                                                               
NUMCOMPR DS    F                                                        00028000
NUMITEMS DS    H                                                        00029000
RECLENIN DS    H                                                        00030000
KEYLENA2 DS    H                                                        00031000
LENMAIN  DS    H                                                        00032000
SAVETABA DS    F                                                        00033000
CURITEM  DS    H                                                        00034000
TEMP     DS    CL258                                                    00035000
NEWNAME  DS    CL8                                                      00036000
SAVESTCK DS    F                                                        00037000
LEFT     DS    H                                                        00038000
RIGHT    DS    H                                                        00039000
AIDLEFT  DS    H                                                        00040000
CURVALUE DS    CL256                                                    00041000
*                                                                               
         DFHEIEND                                                               
* =================================================================== *         
         EJECT                                                          00012700
* =================================================================== * 00020700
*        REGISTERS                                                      00020800
* =================================================================== * 00020900
R0       EQU   0                       PARM REG                         00008000
R1       EQU   1                       PARM REG                         00009000
R2       EQU   2                                                        00010000
R3       EQU   3                       PGM CSECT ADDR                   00011000
R4       EQU   4                                                        00012000
R5       EQU   5                                                        00013000
R6       EQU   6                       BASE REG FOR COMMAREA            00014000
R7       EQU   7                                                        00015000
R8       EQU   8                       BASE REG FOR TSAREA              00016000
R9       EQU   9                       BASE REG FOR TABAREA             00017000
R10      EQU   10                                                       00018000
R11      EQU   11                      EIB DSECT ADDR                   00019000
R12      EQU   12                      BASE REG FOR STACK-AREA          00020000
R13      EQU   13                      DATA/SAVEAREA REGISTER           00021000
R14      EQU   14                      BRANCH&LINK/RETURN REG           00022000
R15      EQU   15                      RETURN CODE/ENTRY REG            00023000
*                                                                               
COMPTR   EQU   6                       BASE REG FOR COMMAREA            00042000
TSPTR    EQU   8                       BASE REG FOR TSAREA              00053000
TABPTR   EQU   9                       BASE REG FOR TABAREA             00056000
STCKPTR  EQU   12                      BASE REG FOR STACK-AREA          00059000
* =================================================================== * 00022600
         EJECT                                                          00012700
* =================================================================== * 00022600
CICSSORT DFHEIENT CODEREG=(R3)                                                  
* ------------------------------------------------------------------- *         
         B     START                   BRANCH AROUND EYE CATCHER                
*                                                                               
PGMNAME  DC    CL8'CICSSORT'           PROGRAM                                  
         DC    CL4'1.2 '               VERSION                                  
         DC    CL4'DLC '               AUTHOR                                   
         DC    CL8'(C)NOV02'           DATE                                     
*                                                                               
START    DS    0H                                                               
         EXEC  CICS HANDLE CONDITION QIDERR(NOTS)                      X00063000
                                     ERROR(ERR)                         00064000
         CLC   EIBCALEN,=H'0'                                           00065000
         BE    OUT                     NO COMMAREA PASSED               00066000
         L     COMPTR,DFHEICAP         ADDRESSABILITY TO COMMAREA       00067000
* WE'RE JUST GOING TO ALLOW ANY QUEUE NAME                                      
*        IF    COMTSNAM(4),(NE,CLC),EIBTRMID     THE FIRST 4 CHARS              
*        AND   COMTSNAM+4(4),(NE,CLC),EIBTRMID   OR THE LAST 4 CHARS            
*         B    ERRNAME                           MUST BE TERM ID                
*        ENDIF                                                                  
*        IF    COMTSNAM+4(4),(NE,CLC),=CL4'SORT' THE LAST 4 CHARS               
*        AND   COMTSNAM(4),(NE,CLC),=CL4'SORT'   OR THE FIRST 4 CHARS           
*         B    ERRNAME                           MUST BE 'SORT'                 
*        ENDIF                                                                  
*                                                                               
*  IS THE REAL NUMBER OF ITEMS EQUAL TO THAT WHICH HAS BEEN PASSED ?    00074000
         EXEC  CICS READQ TS QUEUE(COMTSNAM) SET(TSPTR)                X00072000
               ITEM(1) NUMITEMS(NUMITEMS) LENGTH(RECLENIN)              00073000
         CLC   COMTSNUM,NUMITEMS                                        00075000
         BNE   ERRNUM                                                   00076000
*                                                                               
         CLI   COMSRTYP,C'A'           ASCENDING SORT  ?                00077000
         BE    GOONCHK                                                  00078000
         CLI   COMSRTYP,C'D'           DESCENDING SORT ?                00079000
         BNE   ERRTYPE                                                  00080000
GOONCHK  LH    R9,COMTSNUM                                              00081000
*        CH    R9,=H'1000'             MORE RECORDS THAN THE LIMIT ?    00082000
*        BH    ERRNUM                                                   00083000
         CH    R9,=H'1'                ONE RECORD TO SORT ?             00084000
         BE    ERRNUM                                                   00085000
         LH    R9,COMRECLN                                              00086000
         CH    R9,=H'0'                RECLEN > 0                       00087000
         BNH   ERRRECLN                                                 00088000
         LH    R9,COMKEYLN                                              00089000
         CH    R9,=H'0'                KEYLEN > 0                       00090000
         BNH   ERRKEY                                                   00091000
         CH    R9,=H'255'              KEYLEN < 256                     00092000
         BH    ERRKEY                                                   00093000
         LH    R9,COMKEYOF                                              00094000
         CH    R9,=H'0'                KEYOFFSET NOT < 0                00095000
         BL    ERRKEY                                                   00096000
         LH    R9,COMKEYOF                                              00097000
         AH    R9,COMKEYLN                                              00098000
         CH    R9,COMRECLN             KEYOFFSET + KEYLEN NOT > RECLEN  00099000
         BH    ERRKEY                                                   00100000
*     BUILDING THE POINTERS TABLE      *                                00101000
         SR    R9,R9                                                    00102000
         LH    R9,COMKEYLN                                              00103000
         AH    R9,=H'2'                                                 00104000
         STH   R9,KEYLENA2             KEYLENA2 = KEYLEN + 2            00105000
         MH    R9,COMTSNUM             AMT OF GETMAIN STG - TABAREA     00106000
         C     R9,=F'32767'            MORE THAN 32767 BYTES? /DLC1/    00108000
         BH    ERRMNSTG                                       /DLC1/    00109000
         STH   R9,LENMAIN                                               00107000
         EXEC  CICS HANDLE CONDITION NOSTG(ERRMNSTG)                    00110000
         EXEC  CICS GETMAIN SET(TABPTR) LENGTH(LENMAIN) NOSUSPEND       00111000
         ST    TABPTR,SAVETABA         SAVE ADDR OF TABAREA BEGINNING   00112000
         MVC   CURITEM,=H'1'                                            00113000
         SR    R4,R4                                                    00114000
         IC    R4,COMKEYLN+1           LOADING KEYLENGTH                00115000
         BCTR  R4,0                    AND SUBTRACT 1 - FOR EX COMMAND  00116000
LOOPTAB  EXEC  CICS READQ TS QUEUE(COMTSNAM) SET(TSPTR)                X00117000
               ITEM(CURITEM) LENGTH(RECLENIN)                           00118000
* IS THE SPECIFIED REC LENGTH LESS THAN THE CURRENT RECORD'S LENGTH ?   00119000
         CLC   RECLENIN,COMRECLN                                        00120000
         BL    ERRLNREC                                                 00121000
         MVC   0(2,TABPTR),CURITEM     MOVE THE CURRENT ITEM NUMBER     00122000
         AH    TSPTR,COMKEYOF          ADVANCE TO KEY LOC(IN TSAREA)    00123000
         AH    TABPTR,=H'2'            ADVANCE TO KEY LOC(IN TABAREA)   00124000
         EX    R4,EX1MVC                                                00125000
         AH    TABPTR,COMKEYLN         ADVANCE TO NEXT ENTRY            00126000
         SR    R5,R5                                                    00127000
         LH    R5,CURITEM                                               00128000
         AH    R5,=H'1'                                                 00129000
         STH   R5,CURITEM              INCREASE CURRENT ITEM BY 1       00130000
         CH    R5,COMTSNUM             THE END OF INPUT TS QUEUE ?      00131000
         BNH   LOOPTAB                                                  00132000
*     THE QUICK SORT ITSELF            *                                00133000
         SR    R9,R9                                                    00134000
         LH    R9,COMTSNUM                                              00135000
         MH    R9,=H'2'                AMT OF GETMAIN STG - STACK-AREA  00136000
         STH   R9,LENMAIN                                               00137000
         EXEC  CICS GETMAIN SET(STCKPTR) LENGTH(LENMAIN) NOSUSPEND      00138000
         MVC   LEFT,=H'1'              IN THE SORT BEGIN : LEFT = 1     00139000
         MVC   RIGHT,COMTSNUM          AND RIGHT = NUMBER OF ENTRIES    00140000
         ST    R12,SAVESTCK            SAVE ADDR OF STACK-AREA BEG      00141000
         L     TABPTR,SAVETABA         TABPTR TO START OF TABAREA       00142000
MAINLOOP CLC   RIGHT,LEFT              RIGHT > LEFT                     00143000
         BH    CHANGE                  YES - GO CHANGE                  00144000
         C     R12,SAVESTCK            R12 POINTS TO STACK BEG ?        00145000
         BE    ENDSORT                 YES - END OF SORT                00146000
         SH    R12,=H'2'                                                00147000
* POPPING THE CURRENT STACK ENTRY - TO RIGHT                            00148000
         MVC   RIGHT,0(R12)                                             00149000
         MVC   0(2,R12),=2X'00'        'CLEANING' THE ENTRY             00150000
         SH    R12,=H'2'                                                00151000
* AND POPPING THE NEXT ENTRY - TO LEFT                                  00152000
         MVC   LEFT,0(R12)                                              00153000
         MVC   0(2,R12),=2X'00'        'CLEANING' THE ENTRY             00154000
         B     MAINLOOP                                                 00155000
CHANGE   EQU   *                                                        00156000
         SR    R5,R5                                                    00157000
         LH    R5,RIGHT                                                 00158000
         SH    R5,=H'1'                                                 00159000
         MH    R5,KEYLENA2             R5 - OFFSET  OF RIGHT ENTRY      00160000
         AR    R5,TABPTR               R5 - ADDRESS OF RIGHT ENTRY      00161000
         EX    R4,EX2MVC                                                00162000
         SR    R1,R1                                                    00163000
         LH    R1,LEFT                 R1 - CURRENT-LEFT                00164000
         SR    R8,R8                                                    00165000
         LH    R8,RIGHT                                                 00166000
         SH    R8,=H'1'                R8 - CURRENT-RIGHT (RIGHT-1)     00167000
         LR    R10,R1                                                   00168000
         SH    R10,=H'1'                                                00169000
         MH    R10,KEYLENA2            R10 - OFFSET  OF R1 ENTRY        00170000
         AR    R10,TABPTR              R10 - ADDRESS OF R1 ENTRY        00171000
         LR    R7,R8                                                    00172000
         SH    R7,=H'1'                                                 00173000
         MH    R7,KEYLENA2             R7 - OFFSET  OF R8 ENTRY         00174000
         AR    R7,TABPTR               R7 - ADDRESS OF R8 ENTRY         00175000
BACKCHNG EQU   *                                                        00176000
         CR    R1,R8                   CURRENT-LEFT < CURRENT-RIGHT     00177000
         BL    CHNGLOOP                YES - GO CHANGE                  00178000
         CLC   COMSRTYP,=C'A'          ASCENDING SORT ?                 00179000
         BE    ASCEND3                                                  00180000
DESCND3  EX    R4,EX3CLC               DESCENDING SORT                  00181000
         BNH   GOON                                                     00182000
         B     CONT3                                                    00183000
ASCEND3  EX    R4,EX3CLC                                                00184000
         BNL   GOON                                                     00185000
CONT3    AH    R4,=H'2'                ADD ENTRY NO. LEN - 2 BYTES      00186000
         EX    R4,EX3MVC               EXCHANG THE                      00187000
         EX    R4,EX6MVC               RIGHT WITH                       00188000
         EX    R4,EX7MVC               THE CURRENT-LEFT ENTRIES         00189000
         SH    R4,=H'2'                SUBTRACT 2 FROM R4               00190000
GOON     MVC   AIDLEFT,LEFT            SAVE THE PREVIOUS LEFT           00191000
         SR    R2,R2                                                    00192000
         LR    R2,R1                                                    00193000
         AH    R2,=H'1'                                                 00194000
         STH   R2,LEFT                 LEFT = CURRENT-LEFT + 1          00195000
         SH    R2,=H'2'                R2 - PREVIOUS LEFT - 1           00196000
         CH    R2,AIDLEFT                                               00197000
         BNH   OUTCHNG                                                  00198000
         MVC   0(2,R12),AIDLEFT        PUSH SAVED LEFT TO STACK         00199000
         AH    R12,=H'2'               ADVANCE TO NEXT STACK ENTRY      00200000
         STH   R2,0(R12)               AND PUSH R2(RT.PART) TO STACK    00201000
         AH    R12,=H'2'               ADVANCE TO NEXT STACK ENTRY      00202000
OUTCHNG  B     MAINLOOP                                                 00203000
CHNGLOOP EQU   *                                                        00204000
LOOP1    CLC   COMSRTYP,=C'A'          ASCENDING SORT ?                 00205000
         BE    ASCEND1                                                  00206000
DESCND1  EX    R4,EX1CLC               DESCENDING SORT                  00207000
         BL    LOOP2                                                    00208000
         B     CONT1                                                    00209000
ASCEND1  EX    R4,EX1CLC                                                00210000
         BH    LOOP2                                                    00211000
CONT1    AH    R1,=H'1'                INCREASE R1  BY 1                00212000
         AH    R10,KEYLENA2            INCREASE R10 BY TABENTRY LEN     00213000
         CH    R1,RIGHT                R1 REACHED THE CURRENT RIGHT ?   00214000
         BL    LOOP1                   NO - BACK TO LOOP1               00215000
LOOP2    CLC   COMSRTYP,=C'A'          ASCENDING SORT ?                 00216000
         BE    ASCEND2                                                  00217000
DESCND2  EX    R4,EX2CLC               DESCENDING SORT                  00218000
         BH    OUTLOOP2                                                 00219000
         B     CONT2                                                    00220000
ASCEND2  EX    R4,EX2CLC                                                00221000
         BL    OUTLOOP2                                                 00222000
CONT2    SH    R8,=H'1'                DECREASE R8 BY 1                 00223000
         SH    R7,KEYLENA2             DECREASE R8 BY TABENTRY LEN      00224000
         CH    R8,LEFT                 R8 REACHED THE CURRENT LEFT ?    00225000
         BH    LOOP2                   NO - BACK TO LOOP2               00226000
OUTLOOP2 CR    R1,R8                   CURRENT-LEFT < CURRENT-RIGHT     00227000
         BNL   BACKCHNG                NO - DO NOT EXCHANGE ENTRIES     00228000
         AH    R4,=H'2'                ADD ENTRY NO. LEN - 2 BYTES      00229000
         EX    R4,EX3MVC               EXCHANGE THE                     00230000
         EX    R4,EX4MVC               CURRENT-LEFT                     00231000
         EX    R4,EX5MVC               WITH CURRENT-RIGHT ENTRIES       00232000
         SH    R4,=H'2'                SUBTRACT 2 FROM R4               00233000
         B     BACKCHNG                                                 00234000
*     BUILDING THE SORTED TS OUTPUT QUEUE     *                         00235000
ENDSORT  L     TABPTR,SAVETABA         TABPTR TO START OF TABAREA       00236000
         SR    R5,R5                                                    00237000
         LH    R5,COMTSNUM             IN R5-NUMBER OF ITEMS            00238000
         MVC   NEWNAME(4),COMTSNAM+4   SORTED OUTPUT TS NAME IS         00239000
         MVC   NEWNAME+4(4),COMTSNAM   REVERSE OF INPUT TS NAME         00240000
* DELETING THE OUTPUT TS QUEUE NAME - IN CASE IT EXISTS SOMEHOW         00241000
         EXEC  CICS DELETEQ TS QUEUE(NEWNAME) NOHANDLE                  00242000
LOOPBLD  MVC   CURITEM,0(TABPTR)       THE SORTED SEQ NUMBER TO CURITEM 00243000
         EXEC  CICS READQ TS QUEUE(COMTSNAM) SET(TSPTR)                X00244000
               ITEM(CURITEM) LENGTH(COMRECLN)                           00245000
         EXEC  CICS WRITEQ TS QUEUE(NEWNAME) FROM(TSAREA)              X00246000
               LENGTH(COMRECLN) NOSUSPEND                               00247000
         AH    TABPTR,KEYLENA2         ADVANCE TO NEXT TABAREA ENTRY    00248000
         BCT   R5,LOOPBLD                                               00249000
         L     TABPTR,SAVETABA         TABPTR TO START OF TABAREA       00250000
         EXEC  CICS FREEMAIN DATA(TABAREA)                              00251000
         L     STCKPTR,SAVESTCK        STCKPTR TO START OF STACK-AREA   00252000
         EXEC  CICS FREEMAIN DATA(STCKAREA)                             00253000
         MVC   COMTSNAM,NEWNAME        RETURN NEW QUEUE NAME                    
         MVC   COMANSWR,=H'0'          PASS BACK RETURN CODE            00254000
         B     OUT                                                      00255000
EX1CLC   CLC   2(0,R10),CURVALUE       COMPARE CURR-LEFT  WITH CURVALUE 00256000
EX2CLC   CLC   2(0,R7),CURVALUE        COMPARE CURR-RIGHT WITH CURVALUE 00257000
EX3CLC   CLC   2(0,R5),2(R10)          COMPARE RIGHT WITH CURRENT-LEFT  00258000
EX1MVC   MVC   0(0,TABPTR),0(TSPTR)    MOVING THE KEY INTO TABAREA      00259000
EX2MVC   MVC   CURVALUE(0),2(R5)       MOVING THE RIGHT KEY TO CURVALUE 00260000
EX3MVC   MVC   TEMP(0),0(R10)          MOVING CURRENT-LEFT  TO TEMP     00261000
EX4MVC   MVC   0(0,R10),0(R7)          MOVING CURR-RIGHT TO CURR-LEFT   00262000
EX5MVC   MVC   0(0,R7),TEMP            MOVING TEMP  TO CURRENT-RIGHT    00263000
EX6MVC   MVC   0(0,R10),0(R5)          MOVING RIGHT TO CURRENT-LEFT     00264000
EX7MVC   MVC   0(0,R5),TEMP            MOVING TEMP  TO RIGHT            00265000
ERRNAME  MVC   COMANSWR,=H'1'                                           00266000
         B     OUT                                                      00267000
NOTS     MVC   COMANSWR,=H'2'                                           00283000
         B     OUT                                                      00284000
ERRNUM   MVC   COMANSWR,=H'3'                                           00268000
         B     OUT                                                      00269000
ERRTYPE  MVC   COMANSWR,=H'4'                                           00270000
         B     OUT                                                      00271000
ERRRECLN MVC   COMANSWR,=H'5'                                           00272000
         B     OUT                                                      00273000
ERRLNREC L     TABPTR,SAVETABA         TABPTR TO START OF TABAREA       00274000
         EXEC  CICS FREEMAIN DATA(TABAREA)                              00275000
         MVC   COMANSWR,=H'5'                                           00276000
         B     OUT                                                      00277000
ERRKEY   MVC   COMANSWR,=H'6'                                           00278000
         B     OUT                                                      00279000
ERRMNSTG MVC   COMANSWR,=H'7'                                           00280000
         B     OUT                                                      00281000
ERR      MVC   COMANSWR,=H'10'         ANY UNEXPECTED ERROR             00287000
         B     OUT                                                      00288000
* ------------------------------------------------------------------- *         
OUT      XC    EIBRCODE,EIBRCODE       CLEAR EIB RETURN/RESPONSE CODES          
         XC    EIBRESP,EIBRESP         SO THAT THE LINKING PROGRAM              
         XC    EIBRESP2,EIBRESP2       WON'T THINK THEY'RE FROM "LINK"          
         DFHEIRET                      RETURN TO CICS                           
* =================================================================== *         
         EJECT                                                                  
* =================================================================== *         
STORAGE  DS    0D                                                               
* ------------------------------------------------------------------- *         
         LTORG                                                                  
* =================================================================== *         
         END   CICSSORT                                                 00291000
