/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* REXX: RX3270FN   Proc Created 03/28/2004   By Dave Clark          */
/*                  Last Revised 03/28/2004                          */
/*                                                                   */
/*      This external REXX function  generates  3270 map field names */
/* for use with the RX3270IN and RX3270IO enternal REXX subroutines. */
/* See those for more details.                                       */
/*                                                                   */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* No warranty is expressed or implied.      Written by Dave L Clark */
/* Neither the author nor the company is     DAPSCO Information Sys. */
/* responsible for any loss or damage        3110 Kettering Blvd.    */
/* resulting from the use of this code.      Dayton, OH  45439       */
/* Source is provided on an as-is basis.     Phone (937)294-5331     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  Arg maxRow, maxCol, fldRow, fldCol;

  If ¬DataType(maxRow,'W'),
  |  ¬DataType(maxCol,'W'),
  |  ¬DataType(fldRow,'W'),
  |  ¬DataType(fldCol,'W'),
  Then Do
    Return -1;
  End

  If maxRow < 1      |  maxRow > 43,
  |  maxCol < 1      |  maxCol > 132,
  |  maxRow > maxCol | (maxRow * maxCol) > 4096,
  |  fldRow < 1      |  fldRow > maxRow,
  |  fldCol < 1      |  fldCol > maxCol,
  Then Do
    Return -2;
  End

Return 'FN'||Right((fldRow - 1) * maxCol + (fldCol - 1), 4, '0');
