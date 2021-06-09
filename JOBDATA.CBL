 PROCESS RMODE(ANY)
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    JOBDATA.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JUL 2004.
       DATE-COMPILED.
       INSTALLATION.  DAPSCO INFORMATION SYSTEMS.
       SECURITY.      NONE.
      *REMARKS.       PROVIDE JOB/PARTITION INFO TO CALLER.

      * CHANGE HISTORY ------------------------------------------------
      * END OF HISTORY ------------------------------------------------

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *             J O B   D A T A   S U B R O U T I N E             *
      *                                                               *
      *                                                               *
      * DAPSCO Information Systems                    Written by:     *
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
      *      This routine provides the  COBOL programmer with a means *
      * of dynamically obtaining  system  information  related to the *
      * currently executing job.                                      *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                        IMPLEMENTATION                         *
      *                                                               *
      *      The invoking  COBOL  program  should  pass the following *
      * data area to this routine  in  order to receive the available *
      * system information:                                           *
      *                                                               *
      *    01  JOBDATA-PARMS.                                         *
      *   * POWER JOB NAME                                            *
      *      03  JOBD-PWR-NAME             PIC  X(8).                 *
      *   * POWER JOB NUMBER                                          *
      *      03  JOBD-PWR-NUMB             PIC  X(6).                 *
      *   * SYSLOG PARTITION ID (DYNAMIC ONES, TOO)                   *
      *      03  JOBD-PART-ID              PIC  X(2).                 *
      *   * POWER JOB START TIME STAMP                                *
      *      03  JOBD-PWR-STIM             PIC  X(8).                 *
      *   * POWER FROM USER                                           *
      *      03  JOBD-PWR-FUSR             PIC  X(8).                 *
      *   * POWER USER INFORMATION                                    *
      *      03  JOBD-PWR-UINF             PIC  X(16).                *
      *   * VSE SECURITY USER ID                                      *
      *      03  JOBD-SEC-USER             PIC  X(8).                 *
      *   * VSE JOB DATE (PARTITION)                                  *
      *      03  JOBD-JOB-DATE             PIC  X(8).                 *
      *   * VSE JOB NAME                                              *
      *      03  JOBD-JOB-NAME             PIC  X(8).                 *
      *   * MAIN PROGRAM NAME                                         *
      *      03  JOBD-EXEC-PGM             PIC  X(8).                 *
      *   * PROGRAM START TIME STAMP                                  *
      *      03  JOBD-PGM-STIM             PIC  X(8).                 *
      *   * VSE SYSPARM VALUE                                         *
      *      03  JOBD-SYS-PARM             PIC  X(8).                 *
      *                                                               *
      * ============================================================= *
      *                                                               *
      *                          INVOCATION                           *
      *                                                               *
      *      First,  set up the following  data area at the beginning *
      * of your COBOL Working-Storage Section.                        *
      *                                                               *
      *    77  JOBDATA                 PIC  X(8)   VALUE 'JOBDATA'.   *
      *                                                               *
      * Then, you can invoke this routine as follows:                 *
      *                                                               *
      *   * RETRIEVE SYSTEM INFORMATION                               *
      *        CALL   JOBDATA    USING JOBDATA-PARMS.                 *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-7060-H30.
       OBJECT-COMPUTER. IBM-7060-H30.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  L                           PIC S9(4)    BINARY.
       77  P                           PIC S9(4)    BINARY.
       77  EDITTED-NUMBER              PIC  ZZZZZ9-.
       77  WORK-NUMBER REDEFINES EDITTED-NUMBER PIC X(7).

       01  DWORD                       PIC S9(18)   BINARY.
       01  EIGHT-BYTES       REDEFINES DWORD.
         03  FILLER                    PIC  X(4).
         03  MEMORY-ADDRESS                         POINTER.
         03  FWORD           REDEFINES MEMORY-ADDRESS
                                       PIC S9(9)    BINARY.
         03  FOUR-BYTES      REDEFINES FWORD.
           05  HI-MSB                  PIC  X.
           05  THREE-BYTES.
             07  HI-LSB                PIC  X.
             07  HWORD                 PIC S9(4)    BINARY.
             07  TWO-BYTES   REDEFINES HWORD.
               09  LO-MSB              PIC  X.
               09  ONE-BYTE            PIC  X.

       01  MDY-DATE.
           05  MM                      PIC  XX.
           05  DD                      PIC  XX.
           05  YY                      PIC  XX.

       01  CYMD-DATE.
         03  CC                        PIC  99.
         03  YMD-DATE.
           05  YY                      PIC  XX.
           05  MM                      PIC  XX.
           05  DD                      PIC  XX.

       LINKAGE SECTION.

       COPY JOBDATA.

       COPY COMREG.

       COPY PCEBLK.

       COPY PJBBLK.

       COPY SYSPARM.

       COPY TODCOM.

       PROCEDURE DIVISION           USING JOBDATA-PARMS.

           CALL 'SETCTLBK'          USING ADDRESS OF COMREG-BLOCK
                                          ADDRESS OF PCE-BLOCK
                                          ADDRESS OF PJB-BLOCK
                                          ADDRESS OF SYSPARM-BLOCK
                                          ADDRESS OF TODCOM-BLOCK.

           MOVE PJB-PJBPNAME           TO JOBD-PWR-NAME.
           MOVE             LOW-VALUES TO FOUR-BYTES.
           MOVE PJB-PJBPNUM            TO TWO-BYTES.
           MOVE                  FWORD TO EDITTED-NUMBER.
           MOVE  LENGTH OF WORK-NUMBER TO L.
           PERFORM WITH TEST BEFORE
             VARYING P FROM 1 BY 1 UNTIL P > L
                  OR WORK-NUMBER(P:1) > SPACE
           END-PERFORM.
           COMPUTE L = L - P + 1.
           IF  L < 1 THEN
               MOVE           '*ERROR' TO JOBD-PWR-NUMB
           ELSE
               MOVE   WORK-NUMBER(P:L) TO JOBD-PWR-NUMB
           END-IF.
           MOVE PCE-PCELID             TO JOBD-PART-ID.
           MOVE PJB-PJBPTIME           TO JOBD-PWR-STIM.
           MOVE PJB-PJBPUSER           TO JOBD-PWR-FUSR.
           MOVE PJB-PJBPUINF           TO JOBD-PWR-UINF.
           MOVE COMREG-IJBPOWUI        TO JOBD-SEC-USER.
           MOVE TODCOM-CURDATE         TO MDY-DATE.
           MOVE          CORR MDY-DATE TO YMD-DATE.
           MOVE TODCOM-CENTURY         TO CC.
           MOVE              CYMD-DATE TO JOBD-JOB-DATE.
           MOVE COMREG-COMNAME         TO JOBD-JOB-NAME.
           MOVE COMREG-IJBPHNAM        TO JOBD-EXEC-PGM.
           MOVE PJB-PJBJTIME           TO JOBD-PGM-STIM.
           MOVE             LOW-VALUES TO TWO-BYTES.
           MOVE SYSPARM-LENGTH         TO ONE-BYTE.
           MOVE SYSPARM-VALUE(1:HWORD) TO JOBD-SYS-PARM.

           MOVE ZEROES                 TO RETURN-CODE.
           GOBACK.
