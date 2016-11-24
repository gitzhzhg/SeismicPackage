function info=l_info(wlog,mnem)
% Get the row of "column_info" that pertains to a particular column of a well log.
% If global variable S4M.case_sensitive is set to false, the case of the
% column mnemonic is disregarded.
%
% Written by: E. Rietsch, September 1, 2003
% Last updated: September 3, 2003: bug fix
%
%         info=l_info(wlog,mnem);
% INPUT
% wlog    well log
% mnem    column mnemonic
% OUTPUT
% info    cell array (cell vector with three components) with info about 
%         the curve with mnemonic "mnem"
%         {mnemonic,units of measurement,description}

global S4M

mnems=wlog.curve_info(:,1);
if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else
   idx=find(ismember(lower(mnems),lower(mnem)));
end

if length(idx) == 1
   info=wlog.curve_info(idx,:);
   return
end

% Handle error condition
if isempty(idx)
   disp([' Curve "',mnem,'" not found. Available curves are:'])
   disp(mnems')
else
   disp([' More than one curve found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination')
