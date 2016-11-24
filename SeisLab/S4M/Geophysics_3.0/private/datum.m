function string=datum()
%  Function outputs date and time in the form yyyy-mo-day(hh:mm:ss)
%  string=datum()

dt=fix(clock);
string=[int2str(dt(1)),'-',int2str(dt(2)),'-',int2str(dt(3)),' (',...
   int2str(dt(4)),':',int2str(dt(5)),':',int2str(dt(6)),')'];

