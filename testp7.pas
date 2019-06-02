PROGRAM PASC_P30;
         {$A+ }
         VAR C1 : CHAR(25);
         VAR C2 : CHAR(44);
         VAR N  : INTEGER;
         VAR I1 : INTEGER;
         VAR I2 : INTEGER;
         VAR R1 : REAL;
         VAR S1 : STRING(09);
         VAR S2 : STRING(25);
         VAR EOF : STRING(09);
         VAR DESCRIPTION : ARRAY[1..25] OF STRING(25);
         VAR WIDGETI : TEXT;
         VAR BUFFER  : CHAR(80);

        BEGIN
         { }
         WRITELN (' ');
         WRITELN ('STEP-1 POPULATE ARRAY');
         WRITELN ('STEP-1 POPULATE ARRAY');
         { }
         DESCRIPTION[01] := 'WIDGET-BLUE';
         DESCRIPTION[02] := 'WIDGET-YELLOW';
         DESCRIPTION[03] := 'WIDGET-GREEN';
         DESCRIPTION[04] := 'WIDGET-PURPLE-N-GOLD';
         DESCRIPTION[05] := 'WIDGET-BLUE';
         DESCRIPTION[06] := 'WIDGET-ORANGE';
         DESCRIPTION[07] := 'WIDGET-FANCY-RED';
         DESCRIPTION[08] := 'WIDGET-FANCY-GREEN';
         DESCRIPTION[09] := 'WIDGET-FANCY-YELLOW';
         DESCRIPTION[10] := 'WIDGET-FANCY-ORANGE';
         DESCRIPTION[11] := 'WIDGET-BLUE-WITH-SPARKLES';
         DESCRIPTION[12] := 'WIDGET-RED-WITH-SPARKLES';
         DESCRIPTION[13] := 'WIDGET-BLACK-CHERRY';
         DESCRIPTION[14] := 'WIDGET-RED-N-GREEN';
         DESCRIPTION[15] := 'WIDGET-LIGHTED-CLEAR';
         DESCRIPTION[16] := 'ENDOFFILE';
         { }
         WRITELN(' ');
         WRITELN ('STEP-2 TEST STRINGS');
         WRITELN ('STEP-2 TEST STRINGS');
         WRITELN(' ');
         { }
         EOF     := 'ENDOFFILE';
         S1      := 'ENDOFFILE';
         I1      := LENGTH(EOF);
         I2      := LENGTH(S1);
         WRITELN ('STEP 2 -- EOF  ==>',EOF , '<==');
         WRITELN ('STEP 2 -- S1   ==>',S1  , '<==');
         WRITELN ('STEP 2 -- THE LENGTH OF EOF  IS ', I1);
         WRITELN ('STEP 2 -- THE LENGTH OF S1   IS ', I2);
         { }
         WRITELN(' ');
         IF   EOF  = S1 THEN WRITELN ('STEP2 -- EOF  IS EQUAL TO S1');
         { }
         WRITELN(' ');
         WRITELN ('STEP-3 TEST ARRAY');
         WRITELN ('STEP-3 TEST ARRAY');
         WRITELN(' ');
         FOR I1 := 1 TO 16 DO
             BEGIN
             R1 := I1;
             S2 := DESCRIPTION[I1];
             S1 := SUBSTR(S2,1,9);
             WRITELN ('STEP-3-01 DESCRIPTION[',R1:2:0,'] ==>',
                       DESCRIPTION[I1],'<==');
             WRITELN ('STEP-3-02 SUBSTRING       ==>',
                       S1,'<==');
             IF S1  = EOF THEN
                WRITELN ('STEP-3-03 S1 IS EQUAL TO EOF');
             END;

         WRITELN (' ');
         WRITELN ('STEP-4 READLN TEST');
         WRITELN ('STEP-4 READLN TEST');
         WRITELN (' ');
         RESET (WIDGETI);
         READLN (WIDGETI,BUFFER);
         S1  :=  SUBSTR(BUFFER,13,9);
         WRITELN ('STEP-4-01 COLS   ==>0000000001111111111222222<==');
         WRITELN ('STEP-4-01 COLS   ==>1234567890123456789012345<==');
         WRITELN ('STEP-4-01 BUFFER ==>', BUFFER,'<==');
         WRITELN ('STEP-4-02 SUBSTR(BUFFER,13,9)==>', S1,'<==');
         CLOSE (WIDGETI);

         WRITELN (' ');
         WRITELN ('STEP-5 READ   TEST');
         WRITELN ('STEP-5 READ   TEST');
         WRITELN (' ');
         RESET (WIDGETI);
         READ  (WIDGETI,I1,R1,C1,C2);
         WRITELN ('STEP-5-01  I1    ==>', I1,'<==');
         WRITELN ('STEP-5-02  R1    ==>', R1,'<==');
         WRITELN ('STEP-5-03  C1    ==>', C1,'<==');
         WRITELN ('STEP-5-04  C2    ==>', C2,'<==');
         WRITELN (' ');
         S1   := SUBSTR(C1,1,9);
         WRITELN ('STEP-5-05  COLS          ==>000000000111111<==');
         WRITELN ('STEP-5-05  COLS          ==>123456789012345<==');
         WRITELN ('STEP-5-05  CI            ==>', C1,'<==');
         WRITELN ('STEP-5-06 SUBSTR(C1,1,9) ==>', S1,'<==');

         CLOSE (WIDGETI);

         WRITELN (' ');
         WRITELN (' ');
         WRITELN ('EOJ');
         { }
         END.


