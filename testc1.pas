program testc1 (input, output);

type
        TMyRecord = record
                        a: integer;
                        b: string (5);
        end;

const  default: TMyRecord = (100, 'foo');

var
        r: TMyRecord;

begin
   r := default;
   writeln ('test const with records');
   writeln ('r.a = ', r.a);
   writeln ('r.b = ', r.b);
end.


