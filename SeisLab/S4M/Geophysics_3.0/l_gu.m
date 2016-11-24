function units=l_gu(wlog,mnem)
% Get units of measurement of curve with mnemonic "mnem" from log data set "wlog"
% If S4M.case_sensitive is set to false, the case of the curve mnemonic is disregarded.
%
% See also: l_gc, l_gd
%
% Written by: E. Rietsch: December 30, 2000
% Last updated: December 12, 2005: use S4M.case_sensitive
%
%             units=l_gu(wlog,mnem);
% INPUT
% wlog    log data set
% mnem    curve mnemonic
% OUTPUT
% units   units of measurement of curve with mnemonic "mnem"

global S4M

mnems=wlog.curve_info(:,1);

if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else
   idx=find(ismember(lower(mnems),lower(mnem)));
end
if length(idx) == 1
   units=wlog.curve_info{idx,2};
   return
end

%       Handle error condition
if isempty(idx)
   disp([' Curve "',mnem,'" not found. Available curves are:'])
   disp(mnems')
else
   disp([' More than one curve found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination.')
