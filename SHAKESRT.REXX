/* ================================================================== */
/* ShakerSort: Procedure                                              */
/* ================================================================== */
  comp = 0; swap = 0;

  Parse Arg list,mode
  i = 1;
  k = Words(list);
  If mode == "" Then Do
    Say " "
    Say " ShakerSort of" k            "words in list",
                         Length(list) "bytes in length...";
  End

  Do While i < k
    min = i;
    max = k;
    Do j = i to k
      If j > i Then Do       /* find lowest word in the list */
        comp = comp + 1;
        If Word(list,j) < Word(list,min) Then min = j;
      End
      If j < k Then Do       /* find highest word in the list */
        comp = comp + 1;
        If Word(list,j) > Word(list,max) Then max = j;
      End
    End
    If min > i Then Do       /* swap to end of sorted area at top */
      p = WordIndex(list,i) - 1;
      list = DelWord(Insert(Word(list,min)" ",list,p),min+1,1);
      swap = swap + 1;
    End
    If max < min Then max = max + 1;
    If max < k Then Do       /* swap to top of sorted area at end */
      p = WordIndex(list,k) + Length(Word(list,k)) - 1;
      list = DelWord(Insert(" "Word(list,max),list,p),max,1);
      swap = swap + 1;
    End
    i = i + 1;
    k = k - 1;
  End

  If mode == "" Then Do
    Say " Words: "Words(list)", Comparisons: "comp", Swaps: "swap;
    Say " "
  End

Return(list)
/* ================================================================== */
