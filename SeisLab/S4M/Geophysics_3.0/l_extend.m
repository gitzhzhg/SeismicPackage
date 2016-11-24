function wlog=l_extend(wlog,from,to)
% Function extends the depth range of a log structure by 
% appending/prepending null values
% Written by: E. Rietsch: October 4, 2002
% Last updated:
%
%      	 wlog=l_extend(wlog,from,to)
% INPUT
% wlog   log structure
% from   starting depth
% to     final depth
% OUTPUT
% wlog   extended log

wlog=l_select(wlog,{'depths',from,to});
step=wlog.step;

if wlog.first > from && wlog.last < to
   wlog.first=from;
   wlog.last=to;
   wlog.curves=[wlog.null*ones(1,size(wlog.curves,2));wlog.curves; ...
               wlog.null*ones(1,size(wlog.curves,2))];

else
   if wlog.first > from
      wlog.first=from;
      wlog.step=0;
      wlog.curves=[wlog.null*ones(1,size(wlog.curves,2));wlog.curves];
   end

   if wlog.last < to
      wlog.last=to;
      wlog.step=0;
      wlog.curves=[wlog.curves;wlog.null*ones(1,size(wlog.curves,2))];
   end
end

if step > 0
   wlog=l_resample(wlog,step);
end
