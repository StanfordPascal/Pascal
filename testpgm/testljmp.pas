program TESTLJMP ( OUTPUT ) ;



procedure JUMP1 ;

   label 999 ;


   procedure JUMPTEST ;

      begin (* JUMPTEST *)
        goto 999 ;
      end (* JUMPTEST *) ;


   begin (* JUMP1 *)
     JUMPTEST ;
     if FALSE then
       begin
         999 :
         WRITELN ( 'longjump succeeded !!' ) ;
       end (* then *)
   end (* JUMP1 *) ;



begin (* HAUPTPROGRAMM *)
  JUMP1
end (* HAUPTPROGRAMM *) .
