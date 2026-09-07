%MARGINS 1 80;
PROGRAM Lisp370;
(*********************************************************************)
(* The following material contains programming source code for your  *)
(* consideration.  No warranties or representations are made about   *)
(* the quality of this code.  The code is offered 'as is' and all    *)
(* warranties, expressed or implied, are specifically disclaimed.    *)
(*********************************************************************)
{
 
 
 This program is End-User-Support.  The functions Defun,
 Setq, Cond, and generic lambda expressions are all implemented in
 addition to many of the pure lisp functions.  In writing this program,
 I used a very functional style.  Many of the functions are implemented
 using the functions themselves as was This program is not support I/O
 handling yet.  Very little error-handling is done other than what
 already existed in the program.  When an error occurs, nil is always
 returned as the value of the S-expression.
 
 This program is almost bug-free.  I found one bug.  It appears that in
 recursive user defined functions, if there is an undefined function, an
 endless loop results.  i.e.  if null was inside a recursive function.
 
 The built-in functions implemented are listed in the reserved words
 array in the initialization section.  This program work with lower case
 only.  To Exit the program just type 'stop.
 
 Ex.
   Setq returns the value being stored.  Ex) > (Setq 'a 2) -> 2 Cond
   returns the eval of the cdr of the expression whose car is true.
   Defun returns the function name.
 
 Ex.
   You can (LOAD "FILE(QPASSRC) LIB(QPAS) MBR(LISPDEMO)") to get a
   small canned lisp demonstration.
 
}
   CONST
     maxwrdsize = 16;
     maxlitsize = 64;
     maxnumsize = 4;
     maxresword = 22;
     maxlinelenplus1 = 256; {max line length is 255}
 
   TYPE
     wordstr = string (maxwrdsize);
     litstr = string (maxlitsize);
     printnum = RECORD
                  num: integer;
                  len: 0..maxwrdsize
                END;
 
      stype = (i, w, sl, l);
      sexpr = @scell;
      scell = RECORD
                CASE celltype:stype OF
                  i: (number: printnum);
                  w: (word: wordstr);
                  sl: (literal: litstr);
                  l: (first, rest: sexpr)
              END;
      builtin = (atomi, caari, cadri, cari, cdari, cddri, cdri, consi,
                 eqi, greater, minusi, plusi, quotien, printi, loadi,
                 systemi, timesi,
                 condi, quotei, defuni, setqi, lambdai,
                 notfound);
      (* These are the lisp primitives. notfound is a dummy builtin
         used when search through the reserved word table fails. *)
 
      errtype = (nocarofatom, nocdrofatom, badcons, syntaxerror, undefatom,
                 nosetqdefun, undeffnc, badcar, badlambdaexp);
 
    VAR
      INPUT, OUTPUT : TEXT;
      stop,    {signal to exit interpreter when evaluated}
      s,       {dummy sexpr}
      t,       {the atom t}
      lispnil, {the atom nil}
      printnil, {the atom for print}
      quote,   {the atom 'quote'}
      lambda,  {the lambda atom}
      objList  {keep track of identifiers}
              : sexpr;
      allerrors: set of errtype; {set of all errs found in evaling an sexpr}
 
      reswordtab: array [1..maxresword] of wordstr;   {reserved word table}
      resenumtab: array [1..maxresword] of builtin;
                                              {parallel array to reswordtab}
 
      lnbuffer: string (maxlinelenplus1);
      lnbuffptr: 0..maxlinelenplus1;
      currlinelen: 0..maxlinelenplus1;
      currch, nextch: char;
 
      digits, delimiters, letters, alfanum: SET OF char;
      ord0: integer;
 
    TYPE
      IOSTATUS = (IOOKAY, IOFILEDEF, IOMEMBER, IOLRECL, IOSTRING);
    function  AMPXSTAT (Var P : TEXT) : IOSTATUS;
    EXTERNAL;
 
    (* UpCase -- convert char to upper case *)
    FUNCTION UpCase (c : char) : char;
    VAR
        j : integer;
    STATIC
        upper_case_letters : string(26);
    VALUE
        upper_case_letters := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    BEGIN
      j := index('abcdefghijklmnopqrstuvwxyz',str(c));
      IF (j <> 0) THEN
         UpCase := upper_case_letters[j]
      ELSE
         UpCase := c
    END;
 
 
    PROCEDURE initialize;
      BEGIN
        digits := ['0'..'9'];
        delimiters := ['(', ')', ' '];
        letters := ['a'..'z', 'A'..'Z'];
        alfanum := letters + digits;
        ord0 := ord('0');
        objList := nil;
 
        {stop atom}
        new (stop);
        WITH stop@ DO
          BEGIN
            celltype := w;
            word := 'STOP'
          END;
 
        {t atom (for "true")}
        new (t);
        WITH t@ DO
          BEGIN
            celltype := w;
            word := 'T'
          END;
 
        {nil atom}
        new (lispnil);
        WITH lispnil@ DO
          BEGIN
            celltype := w;
            word := 'NIL'
          END;
 
        {print atom}
        new (printnil);
        WITH printnil@ DO
          BEGIN
            celltype := w;
            word := ' '
          END;
 
        {quote atom}
        new (quote);
        WITH  quote@ DO
          BEGIN
            celltype := w;
            word := 'QUOTE'
          END;
 
        {lambda atom}
        new (lambda);
        WITH lambda@ DO
          BEGIN
            celltype := w;
            word := 'LAMBDA'
          END;
 
        reswordtab [1] := 'ATOM';      resenumtab [1] := atomi;
        reswordtab [2] := 'CAAR';      resenumtab [2] := caari;
        reswordtab [3] := 'CADR';      resenumtab [3] := cadri;
        reswordtab [4] := 'CAR';       resenumtab [4] := cari;
        reswordtab [5] := 'CDAR';      resenumtab [5] := cdari;
        reswordtab [6] := 'CDDR';      resenumtab [6] := cddri;
        reswordtab [7] := 'CDR';       resenumtab [7] := cdri;
        reswordtab [8] := 'COND';      resenumtab [8] := condi;
        reswordtab [9] := 'CONS';      resenumtab [9] := consi;
        reswordtab [10] := 'DEFUN';    resenumtab [10] := defuni;
        reswordtab [11] := 'EQ';       resenumtab [11] := eqi;
        reswordtab [12] := 'GREATER';  resenumtab [12] := greater;
        reswordtab [13] := 'LAMBDA';   resenumtab [13] := lambdai;
        reswordtab [14] := 'LOAD';     resenumtab [14] := loadi;
        reswordtab [15] := 'MINUS';    resenumtab [15] := minusi;
        reswordtab [16] := 'PLUS';     resenumtab [16] := plusi;
        reswordtab [17] := 'PRINT';    resenumtab [17] := printi;
        reswordtab [18] := 'QUOTE';    resenumtab [18] := quotei;
        reswordtab [19] := 'QUOTIENT'; resenumtab [19] := quotien;
        reswordtab [20] := 'SETQ';     resenumtab [20] := setqi;
        reswordtab [21] := 'SYSTEM';   resenumtab [21] := systemi;
        reswordtab [22] := 'TIMES';    resenumtab [22] := timesi;
       END; {initialize}
 
    PROCEDURE setupline;  {called from main one time, then from getch}
      BEGIN               {(within getsexpr) to start line reading   }
        nextch := ' ';
        IF eof(INPUT) THEN
          BEGIN
            close(INPUT);
            termin(INPUT);
          END;
        readln (INPUT,lnbuffer);
        lnbuffer := trim(lnbuffer) || ' ';
           {needed so lookahead at real eoln is ' '}
        currlinelen := length (lnbuffer);
        lnbuffptr := 1
      END;
 
 
    FUNCTION handleerror (err: errtype): sexpr;
      BEGIN
        allerrors := allerrors + [err];
        handleerror := nil
      END;
 
 
 
    PROCEDURE printerrors;
      BEGIN
        IF nocarofatom IN allerrors THEN
           writeln (output,'Can''t take car of atom');
        IF nocdrofatom IN allerrors THEN
           writeln (output,'Can''t take cdr of atom');
        IF badcons IN allerrors THEN
           writeln (output,'Only cons onto a list');
        IF syntaxerror IN allerrors THEN
           writeln (output,'Lisp syntax error');
        IF undefatom IN allerrors THEN
           writeln (output,'Undefined atom found');
        IF nosetqdefun IN allerrors THEN
           writeln (output,'Setq/defun only at top level');
        IF undeffnc IN allerrors THEN
           writeln (output,'Undefined function name');
        IF badcar IN allerrors THEN
           writeln (output,'Car of list must be atom');
        IF badlambdaexp IN allerrors THEN
           writeln (output,'Lambda expression required');
      END;
 
    FUNCTION atom (s: sexpr): sexpr;
      BEGIN
        IF      s = nil THEN atom := t
        ELSE IF s@.celltype = l THEN atom := nil
        ELSE atom := t;
      END;   {atom}
 
    FUNCTION car (s: sexpr): sexpr;
      BEGIN
        IF   atom (s) = nil THEN car := s@.first
        ELSE car := handleerror (nocarofatom)
      END;   {car}
 
    FUNCTION cdr (s: sexpr): sexpr;
      BEGIN
        IF   atom (s) = nil THEN cdr := s@.rest
        ELSE
         cdr := handleerror (nocdrofatom)
      END;   {cdr}
 
    FUNCTION caar (s: sexpr):sexpr;
      BEGIN
        caar := car (car (s))
      END;
 
    FUNCTION cadr (s: sexpr):sexpr;
      BEGIN
        cadr := car (cdr (s))
      END;
 
    FUNCTION cdar (s: sexpr):sexpr;
      BEGIN
        cdar := cdr (car (s))
      END;
 
    FUNCTION cddr (s: sexpr):sexpr;
      BEGIN
        cddr := cdr (cdr (s))
      END;
 
 
    FUNCTION cons (x,y: sexpr): sexpr;
      VAR s:sexpr;
      BEGIN
        new (s);
        WITH s@ DO
        BEGIN
          celltype := l;
          first := x;
          rest := y
        END;
        cons := s;
        IF   y <> NIL THEN
        IF   y@.celltype in [w,i]
        THEN BEGIN  {illegal cons.  dotted pairs not supported}
               dispose (s);
               cons := handleerror (badcons)
             END
    END;   {cons}
 
    FUNCTION append (s1, s2: sexpr):sexpr;
      BEGIN
        IF s1 = nil THEN append := s2
        ELSE append := cons (car (s1), append (cdr(s1), s2))
      END;
 
    FUNCTION eq (s1, s2: sexpr): sexpr;
      {returns t if s1 & s2 are equal atoms, else nil}
      BEGIN
        eq := nil;
        IF   s1 = nil THEN IF s2 = nil THEN eq := t ELSE {nothing here}
        ELSE {s1 <> nil}
        CASE s1@.celltype OF
          i: IF   s2@.celltype = i
             THEN IF s1@.number.num = s2@.number.num THEN eq := t;
          w: IF   s2@.celltype = w THEN
               BEGIN
                  IF s1@.word = s2@.word THEN eq := t;
               END
             ELSE IF   s2@.celltype = sl THEN
               BEGIN
                 IF s1@.word = s2@.literal THEN eq := t;
               END;
         sl: IF   s2@.celltype = sl THEN
               BEGIN
                 IF s1@.literal = s2@.literal THEN eq := t;
               END
             ELSE IF   s2@.celltype = w THEN
               BEGIN
                 IF s1@.literal = s2@.word THEN eq := t;
               END;
          l: {leave eq= nil}
        END
      END;  {eq}
 
    FUNCTION makesxint (int: integer): sexpr;
      {returns a new integer sexpr w/ value = int}
      VAR
        s: sexpr;
      BEGIN
        new(s);
        WITH s@ DO
        BEGIN
          celltype := i;
          number.num := int
          {;number.len := 1 + trunc(log(0.5 + abs(int)))}
          {log is a UCSD extension for log base 10}
        END;
        makesxint := s
      END;
 
    FUNCTION arith (s1, s2: sexpr; op:builtin): sexpr;
      {arith operation on 2 integer atoms; assumes op is an arith. op}
      VAR
        ans: integer;
      BEGIN
        arith := nil;
        IF      ((s1 <> nil) AND (s2 <> nil))
        THEN IF ((s1@.celltype = i) AND (s2@.celltype = i))
        THEN BEGIN CASE op OF
                     minusi: ans := s1@.number.num - s2@.number.num;
                     plusi: ans :=  s1@.number.num + s2@.number.num;
                     quotien: ans := s1@.number.num DIV s2@.number.num;
                     timesi: ans := s1@.number.num * s2@.number.num
                   END;
                   arith := makesxint (ans)
             END
      END;
 
    FUNCTION greaterp (s1, s2: sexpr):sexpr;
      BEGIN
        greaterp := nil;
        IF      ((s1 <> nil) AND (s2 <> nil))
        THEN IF ((s1@.celltype = i) AND (s2@.celltype = i))
        THEN IF s1@.number.num > s2@.number.num THEN greaterp := t
      END;
 
 
{**************************** I/O routines ***********************}
 
    PROCEDURE getsexpr (VAR s: sexpr);
      {reads a sexpr from input stream, puts it in s}
 
      TYPE
         ttype = (int, wrd,  quottok, lit, lparen, rparen);
         token = RECORD
                   CASE tokentype: ttype OF
                     int: (num: printnum);
                     wrd: (word: wordstr);
                     lit: (literal: litstr);
                     lparen, rparen, quottok: ()
                   END;
       VAR
         currtok: token;
 
       PROCEDURE gettoken (VAR t: token);
         {reads the next token from the input stream}
         VAR
           moretoread: boolean;  {used only in getnumtok & getwrdtok}
 
         PROCEDURE getch;
           {sets the new values of currch and nextch, readlning if needed}
           BEGIN
             currch := nextch;
             IF   lnbuffptr > currlinelen
             THEN setupline  {global procedure defined after initialize}
             ELSE BEGIN
                    nextch := lnbuffer [lnbuffptr];
                    lnbuffptr := lnbuffptr + 1
                  END
           END;
 
         PROCEDURE flushout (t:token);
           {prints message that t has been truncated;
            flushes input stream to next delimiter}
           VAR
             moretoflush: boolean;
           BEGIN  {flushout}
             moretoflush := true;
             WHILE moretoflush DO
               IF   not (nextch IN delimiters)
               THEN getch
               ELSE moretoflush := false;
             writeln(output);
             WITH t DO
               IF   tokentype = int
               THEN writeln (output,num.num,' has been truncated')
               ELSE writeln (output,word, ' has been truncated')
           END;   {flushout}
 
         PROCEDURE getnumtok (VAR t: token);
           {returns a numeric token, assuming 1st char
            has already been read into ch }
           BEGIN  {getnumtok}
             moretoread := true;
             t.tokentype := int;
             WITH t.num DO
             BEGIN
               num := ord(currch) - ord0;
               len := 1;
               WHILE moretoread AND (len < maxnumsize)
               DO IF   nextch in digits
                  THEN BEGIN
                         getch;
                         len := len + 1;
                         num := 10 * num + ord(currch) - ord0
                       END
                  ELSE {next char not a digit, this token is done}
                       moretoread := false
             END;
             IF NOT (nextch in delimiters) THEN flushout(t)
           END;   {getnumtok}
 
         PROCEDURE getwrdtok (VAR t: token);
           {returns an alphabetic token, assuming 1st char
            has already been read into currch }
           VAR i: 1..maxwrdsize;
           BEGIN  {getwrdtok}
             moretoread := true;
             t.tokentype := wrd;
             WITH t DO
             BEGIN
               word := str(upcase(currch)) ;
               WHILE moretoread AND (length(word) < maxwrdsize)
                  DO IF   nextch in alfanum
                     THEN BEGIN
                            getch;
                            word := word || str(upcase(currch));
                          END
                     ELSE moretoread := false
             END;
             IF NOT (nextch in delimiters) THEN flushout(t)
           END;   {getwrdtok}
 
         PROCEDURE getlittok (VAR t: token);
           {returns an literal token, assuming 1st char
            has already been read into currch }
           VAR i: 1..maxwrdsize;
           BEGIN  {getlittok}
             moretoread := true;
             t.tokentype := lit;
             WITH t DO
             BEGIN
               word := '';
               WHILE moretoread AND (length(literal) < maxlitsize)
                  DO BEGIN
                   IF   nextch <> '"'
                     THEN BEGIN
                            getch;
                            literal := literal || str(currch);
                          END
                     ELSE BEGIN
                            getch; { flush double quote }
                            moretoread := false
                          END;
                  END;
             END;
             IF NOT (nextch in delimiters) THEN flushout(t)
           END;   {getlittok}
 
         BEGIN {gettoken}
                  REPEAT getch UNTIL currch <> ' ';
                  IF      currch = '('      THEN t.tokentype := lparen
                  ELSE IF currch = ')'      THEN t.tokentype := rparen
                  ELSE IF currch = ''''     THEN t.tokentype := quottok
                  ELSE IF currch IN digits  THEN getnumtok(t)
                  ELSE IF currch IN letters THEN getwrdtok(t)
                  ELSE IF currch =  '"'     THEN getlittok(t)
                  ELSE BEGIN
                         writeln (output,' error in token; ',
                                  currch, ' ignored');
                         gettoken (t)
                       END
         END;  {gettoken}
 
       FUNCTION sread:sexpr;
         {reads a sexpr, given that the first token
          has alreay been read and put into currtok}
         VAR
           s: sexpr;
 
         FUNCTION lread: sexpr;
           {reads a list of 0 or more sexprs, assuming 1st '(' is read}
           BEGIN  {lread}
             gettoken (currtok);
             IF   currtok.tokentype = rparen
             THEN lread := nil
             ELSE lread := cons (sread, lread)
           END;   {lread}
 
         BEGIN {sread}
           CASE currtok.tokentype OF
           quottok:BEGIN
                     gettoken (currtok);
                     sread := cons (quote, cons (sread, nil))
                   END;
           int:    BEGIN
                     new (s);
                     s@.celltype := i;
                     s@.number := currtok.num;
                     sread := s
                   END;
           wrd:    BEGIN
                     new (s);
                     s@.celltype := w;
                     s@.word := currtok.word;
                     sread := s
                   END;
           lit:    BEGIN
                     new (s);
                     s@.celltype := sl;
                     s@.literal := currtok.literal;
                     sread := s
                   END;
           lparen: sread := lread;
           rparen: sread := handleerror (syntaxerror)
           END  {case}
         END;  {sread}
 
       BEGIN {getsexpr}
         nextch := ' ';
         gettoken (currtok);
         s := sread
       END;  {getsexpr}
 
       PROCEDURE swrite (s: sexpr);
       {writes out the given s expression}
 
       PROCEDURE writeatom (s: sexpr);
         VAR j: 1..maxwrdsize;
         BEGIN
           WITH s@ DO
             CASE celltype OF
              i: write (output,number.num, ' ');
              w: write (output,word, ' ');
              sl: write(output,'"',literal,'" ');
             END;
         END;   {writeatom}
 
       PROCEDURE lwrite (s: sexpr);
         {writes out the list s, assuming the leftmost '('
          has already been written    }
 
          BEGIN  {lwrite}
            IF   s = nil
            THEN write (output,')')
            ELSE BEGIN
                   swrite (car (s));
                   lwrite (cdr (s))
                 END
          END;   {lwrite}
 
        BEGIN  {swrite}
          IF   s = nil
          THEN writeatom (lispnil)
          ELSE CASE s@.celltype OF
                 w,i,sl: writeatom (s);
                 l:   BEGIN
                        write (output,'('); lwrite (s)
                      END
                 END
        END;   {swrite}
 
    FUNCTION print (s: sexpr):sexpr;
      BEGIN
        writeln(output);
        swrite(s);
        print := printnil;
      END {print};
 
    FUNCTION load (s: sexpr):sexpr;
    VAR
      filename : litstr;
      BEGIN
        load := NIL;
        if s <> nil THEN WITH s@ DO
          BEGIN
            IF celltype = sl THEN
              BEGIN
                close(INPUT);
                filename := s@.literal;
                reset(INPUT,filename||' STATUS ');
                if AMPXSTAT(INPUT) <> IOOKAY then
                  begin
                    writeln(output,'File: ',filename,' not found.');
                    termin(INPUT);
                  end
                else
                  load := s;
              END;
          END {with};
      END {print};
 
    FUNCTION docommand (s: sexpr):sexpr;
      var rc : integer;
      BEGIN
        docommand := NIL;
        if s <> nil THEN WITH s@ DO
          BEGIN
            IF celltype = sl THEN
              BEGIN
             {  system(s@.literal);  }
                docommand := s;
              END;
          END {with};
      END {docommand};
 
 
 
{******************** eval and its subroutines ********************}
 
    FUNCTION eval (s,CurrEnv: sexpr): sexpr;
      VAR
        primop: builtin;
 
        FUNCTION search (opstr: wordstr): builtin;
           VAR
             hi, lo, mid: integer;
           BEGIN
             lo := 1;
             hi := maxresword;
             REPEAT
               mid := (hi + lo) DIV 2;
               IF opstr <= reswordtab [mid] THEN hi := mid - 1;
               IF opstr >= reswordtab [mid] THEN lo := mid + 1
             UNTIL lo > hi;
             IF   lo-1 > hi THEN search := resenumtab [mid]
                            ELSE search := notfound
           END; {search}
 
         FUNCTION listeval (s: sexpr):sexpr;
           {returns a list of values of the sexprs in the list s}
           {the environment env is inherited from enveval}
           BEGIN
             IF   s = nil THEN listeval := nil
             ELSE listeval := cons (eval (car (s),CurrEnv),
                                    listeval (cdr (s)))
           END;
 
         FUNCTION LambdaExpr(s:Sexpr):boolean;
 
         { Determines whether or not s is a lambda expression. }
 
         BEGIN
          LambdaExpr := False;
          IF s<>nil THEN
           IF s@.celltype =L THEN
            BEGIN
             s:= car(s);
             IF s@.celltype = w THEN
              IF s@.word = 'LAMBDA' THEN
                 LambdaExpr := True
            END;
         END;
 
         FUNCTION apply (s1, s2:sexpr):sexpr;
           {applies s1 to list of args in s2.  neither has been eval'ed}
           VAR
             fncname: builtin;
 
           FUNCTION applyprim (anop: builtin; s: sexpr): sexpr;
             {returns value of applying anop to the list of values in s}
 
             VAR s1, s2: sexpr; {1st (and 2nd if needed) elements of s}
 
             BEGIN  {applyprim}
               s1 := car (s);
               IF anop IN [consi, eqi, greater, minusi, plusi, quotien,
                           timesi]
               THEN {2 arguments} s2 := cadr (s);
               CASE anop OF
                 atomi: applyprim := atom (s1);
                 cari:  applyprim := car (s1);
                 cdri:  applyprim := cdr (s1);
                 caari: applyprim := caar (s1);
                 cadri: applyprim := cadr (s1);
                 cdari: applyprim := cdar (s1);
                 cddri: applyprim := cddr (s1);
                 consi: applyprim := cons (s1, s2);
                 eqi:   applyprim := eq (s1, s2);
                 greater: applyprim := greaterp (s1, s2);
                 minusi, plusi, quotien, timesi:
                        applyprim := arith (s1, s2, anop);
                 printi: applyprim := print(s1);
                 loadi: applyprim := load(s1);
                 systemi: applyprim := docommand(s1);
               END
             END;  {applyprim}
 
 
             FUNCTION HandleCond(s1: sexpr):sexpr;
 
             { Recursively evaluates entrys in s1 until a true value is}
             { found, cdr is returned. }
             BEGIN
              IF s1=nil THEN HandleCond := nil
              ELSE
                  IF Eval(caar(s1),CurrEnv) = t THEN
                    HandleCond := eval(car(cdar(s1)),CurrEnv)
                  ELSE HandleCond := HandleCond(cdr(s1));
             END;
 
             FUNCTION AssocList(s1,s2,List:sexpr ):Sexpr;
 
             { Associates s1 with s2 after both have been evaluated and }
             { places this association in the object list. }
 
             BEGIN
              AssocList := nil;
              AssocList := Cons(Cons(s1,Cons(s2,nil)),List);
             END;
 
             FUNCTION HandleSetQ(s1,s2: sexpr):sexpr;
 
             BEGIN
              HandleSetQ := nil;
              s1 := Eval(s1,CurrEnv);
              IF s1<>nil THEN
               IF s1@.CellType = w THEN
                IF s2<>nil THEN
                  BEGIN
                   s2 := Eval(s2,CurrEnv);
                   ObjList := AssocList(s1,s2,objList);
                   HandleSetQ := s2;
                  END;
             END;
 
             FUNCTION HandleDefun(s:sexpr):Sexpr;
 
             { Stores function definition in lambda format in the object }
             { list. }
             VAR Name,Body:SExpr;
             BEGIN
              HandleDefun := nil;
              Name := Car(s);
              Body := Cdr(s);
              Writeln;
              IF Name<>nil THEN
               IF Name@.CellType = w THEN
                IF Body<>nil THEN
                  BEGIN
                   objList := AssocList(Name,Cons(lambda,Body),objList);
                   HandleDefun := Name;
                  END;
             END;
 
        FUNCTION BindFormals(Form,Act:sexpr):sexpr;
 
        { Associates formals of the function in question with the actuals. }
        { This function assumes Act have been evaluated. }
         PROCEDURE GetNewEnv(F,A:sexpr);
          BEGIN
           IF F<>nil THEN
            IF A<>nil THEN
             BEGIN
              CurrEnv := AssocList(car(F),car(A),CurrEnv);
              GetNewEnv(Cdr(F),Cdr(A));
             END;
          END;
         BEGIN
          GetNewEnv(Form,Act);
          BindFormals := CurrEnv;
         END;
 
 
         FUNCTION HandleLambda(LExpr,ArgList:sexpr):sexpr;
 
         { Interprets generic functions. }
             BEGIN
              HandleLambda := nil;
              IF lambdaExpr(LExpr) THEN
               BEGIN
                HandleLambda := Eval(Car(Cddr(LExpr)),
                         BindFormals(Cadr(LExpr),listeval(ArgList )));
               END;
             END;
 
         FUNCTION LookUpFun(s,List:SExpr):sexpr;
 
         { Attempts to find s in the list 'List' }
             VAR
              Entry,Identifier:SExpr;
 
              FUNCTION FindFunRecursive(L:Sexpr):sexpr;
              VAR temp:sexpr;
                  ch:char;
              BEGIN
               FindFunRecursive := nil;
               IF L<>nil THEN
                BEGIN
                 Entry := Car(L);
                 Identifier := Car(Entry);
                  IF (Identifier@.CellType = s@.CellType) and
                     (Identifier@.CellType = w ) and
                     (Identifier@.word = S@.word)
                    and (LambdaExpr(Cadr(Entry))) THEN
                        FindFunRecursive := Cadr(Entry)
                  ELSE
                       FindFunRecursive := FindFunRecursive(Cdr(L))
                END
             END;
 
        BEGIN
         LookUpFun := FindFunRecursive(List);
        END;
 
 
        FUNCTION HandleUserDef(UserDef,ArgList:sexpr):sexpr;
        VAR Definition:sexpr;
        BEGIN
         HandleUserDef := nil;
         Definition := LookUpFun(UserDef,CurrEnv);
         HandleUserDef := Eval(Car(Cddr(Definition)),
                          BindFormals(Cadr(Definition),listEval(ArgList)));
        END;
 
 
        BEGIN {apply}
          IF   s1 = nil THEN apply := handleerror (undeffnc)
          ELSE IF s1@.celltype = w
          THEN BEGIN
            fncname := search (s1@.word);
            IF fncname IN [atomi..timesi] THEN
               apply := applyprim (fncname, listeval (s2))
            ELSE
              CASE fncname OF
               quotei: apply := car (s2);
               condi: apply := HandleCond(s2);
               defuni: apply := HandleDefun(s2);
               lambdai: Writeln('lambda encountered');
               setqi: apply := HandleSetQ(car(s2),cadr(s2));
               notfound: apply := HandleUserDef(s1,s2);
              END {case}
           END  {begin}
          ELSE IF s1@.celltype = L THEN { if car of an s-expression is a }
           apply := HandleLambda(s1,s2) { list, check to see if is lambda }
          ELSE apply := handleerror (undeffnc)
        END; {apply}
 
        FUNCTION LookUpDef(s,List:SExpr):sexpr;
 
        {same as lookupfun, except now we are looking for variable references. }
         VAR
          Entry,Identifier:SExpr;
 
         FUNCTION FindDefRecursive(L:Sexpr):sexpr;
         VAR temp:sexpr;
              ch:char;
         BEGIN
          FindDefRecursive := nil;
          IF L<>nil THEN
           BEGIN
            Entry := Car(L);
            Identifier := Car(Entry);
            IF Identifier <> nil THEN
             IF Identifier@.CellType = s@.CellType THEN
              IF Identifier@.CellType = w THEN
               IF Identifier@.word = S@.word THEN
                BEGIN
                 IF LambdaExpr(Cdr(Car(L))) THEN
                  FindDefRecursive := FindDefRecursive(Cdr(L))
                 ELSE
                   FindDefRecursive := Cadr(Entry);
                END
               ELSE
                FindDefRecursive := FindDefRecursive(Cdr(L));
             END
         END;
 
        BEGIN
         LookUpDef := nil;
         LookUpDef := FindDefRecursive(List);
         IF (s@.celltype = w) THEN
          IF (s@.word = 'T') THEN
           LookUpDef := t;
        END;
 
   BEGIN {eval}
        IF s = nil THEN eval := nil
        ELSE CASE s@.celltype OF
               i: eval := s;
               w: eval := LookUpDef(s,CurrEnv);
               sl: eval := s;
               l: eval := apply (car(s), cdr(s))
             END {case s@.celltyp of}
   END; {eval}
 
 
   BEGIN {main}
     termout(output);
     termin(input);
     writeln(output);writeln(output,'Lisp/38 Intepreter':50);
     writeln(output);writeln(output,'Type ''stop to end':50);
     initialize;
     setupline;
     repeat
       allerrors := [];
       write(output,'>');
       getsexpr (s);
       s := eval (s,ObjList);
       swrite (s);
       writeln(output);
       IF allerrors <> [] THEN printerrors
     UNTIL eq (s, stop) = t;
     writeln(output,' Bye....');
   END.
