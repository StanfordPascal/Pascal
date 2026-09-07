program Queens(output);
const
 CMin = 1;
 CMax = 8;
 Dmin = -7;
 Dmax = 7;
type
 CRange  = Cmin..Cmax;
 Column  = set of CRange;
 Field   = array[CRange] of CRange;
 DRange  = Dmin..Dmax;
 Diag    = set of DRange;
var
 Queens   : Field;
 Solution : integer;
procedure PrintQueens;
 var
  Row, Col : CRange;
 begin
  Solution:=Solution+1;
  writeln('Solution ', Solution, ': ');
  for Row:=Cmin to Cmax do
   begin
    for Col:=Cmin to Cmax do
     if Field[Row]=Col
      then
       write('Q')
      else
       if Odd(Row+Col)
        then
         write(' ')
        else
         write('#');
    writeln
   end;
  writeln
 end;
procedure FindQueens(NewQueenRow: CRange;
                     Cols: Column;
                     UpDiag, DownDiag: Diag);
 var
  TrialCol  : CRange;
  TrialUp,
  TrialDown : DRange;
 begin
  for TrialCol := Cmin to Cmax do
   begin
    TrialUp:=NewQueenRow-TrialCol;
    TrialDown:=NewQueenRow-(Cmax+1-TrialCol);
    if not (TrialCol in Cols)
       and not (TrialUp in UpDiag)
       and not (TrialDown in DownDiag)
     then
      begin
       Field[NewQueenRow]:=TrialPos;
       if NewQueenRow<Cmax
        then
         FindQueens(NewQueenRow+1,
                    Cols+[TrialCol],
                    UpDiag+[TrialUp],
                    DownDiag+[TrialDown])
        else
         PrintQueens
      end
   end
 end;
begin
 Solution:=0;
 FindQueens(Cmin, [], [], []);
 writeln('Total solutions: ', Solution);
end.