PROGRAM PASC_P3;

//$A+

         VAR N, I1, I2 : INTEGER;
         VAR S1,EOF    : STRING(25);
         DESCRIPTION   : ARRAY[1..25] OF STRING(25);

        BEGIN

         WRITELN (' ');
         WRITELN ('STEP-0 POPULATE ARRAY');
         WRITELN ('STEP-0 POPULATE ARRAY');

         DESCRIPTION[01] := 'WIDGET-BLUE              ';
         DESCRIPTION[02] := 'WIDGET-YELLOW            ';
         DESCRIPTION[03] := 'WIDGET-GREEN             ';
         DESCRIPTION[04] := 'WIDGET-PURPLE-N-GOLD     ';
         DESCRIPTION[05] := 'WIDGET-BLUE              ';
         DESCRIPTION[06] := 'WIDGET-ORANGE            ';
         DESCRIPTION[07] := 'WIDGET-FANCY-RED         ';
         DESCRIPTION[08] := 'WIDGET-FANCY-GREEN       ';
         DESCRIPTION[09] := 'WIDGET-FANCY-YELLOW      ';
         DESCRIPTION[10] := 'WIDGET-FANCY-ORANGE      ';
         DESCRIPTION[11] := 'WIDGET-BLUE-WITH-SPARKLES';
         DESCRIPTION[12] := 'WIDGET-RED-WITH-SPARKLES ';
         DESCRIPTION[13] := 'WIDGET-BLACK-CHERRY      ';
         DESCRIPTION[14] := 'WIDGET-RED-N-GREEN       ';
         DESCRIPTION[15] := 'WIDGET-LIGHTED-CLEAR     ';
         DESCRIPTION[16] := 'ENDOFFILE                ';
         DESCRIPTION[17] := 'RECORD 17                ';
         DESCRIPTION[18] := 'RECORD 18                ';
         DESCRIPTION[19] := 'RECORD 19                ';

         WRITELN (' ');
         WRITELN('STEP-1 STRING TESTS');
         WRITELN('STEP-1 STRING TESTS');

 {01}    WRITELN (' ');
         I1      := LENGTH(DESCRIPTION[16]);
         WRITELN('STEP-1-01 LENGTH(DESCRIPTION[16])==>',I1);

 {02}    N  := 16;
         I1      := LENGTH(DESCRIPTION[N]);
         WRITELN('STEP-1-02 LENGTH(DESCRIPTION[N]) ==>',I1);

 {03}    WRITELN (' ');
         S1 := SUBSTR(DESCRIPTION[16],1,9);
         WRITELN('STEP-1-03 SUBSTR(DESCRIPTION[16],1,9) ==>',
                            S1,'<==');

 {04}    WRITELN (' ');
         N  := 16;
         S1 := SUBSTR(DESCRIPTION[N],1,9);
         WRITELN('STEP-1-04 SUBSTR(DESCRIPTION[N],1,9) ==>',
                            S1,'<==');

 {05}    WRITELN (' ');
         S1 := DESCRIPTION[16];
         WRITELN('STEP-1-05 DESCRIPTION[16]) ==>', S1,'<==');

 {06}    WRITELN (' ');
         N  := 16;
         S1 := DESCRIPTION[N];
         WRITELN('STEP-1-06 DESCRIPTION[N])  ==>', S1,'<==');

         END.
