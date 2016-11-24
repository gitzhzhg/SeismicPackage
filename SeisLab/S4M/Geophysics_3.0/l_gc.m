function [curve,info]=l_gc(wlog,mnem,depths)
% Function extracts curve with mnemonic "mnem" from log data set "wlog"
% If global variable S4M.case_sensitive is set to false, the case of the curve
% mnemonic is disregarded.
%
% See also: l_gu, l_gd
%
% Written by: E. Rietsch, December 28, 2000
% Last updated: May 12, 2008: Improved display of curve mnemonics using "disps"
%
%           [curve,info]=l_gc(wlog,mnem,depths)
% INPUT
% wlog      log structure
% mnem      curve mnemonic
% depths    optional vector of depth values for which the curve values are 
%           required; these values need not coincide with depth values in "wlog".
%           if not given, all samples of curve are output
% OUTPUT
% curve     column vector with curve
% info      relevant row of "wlog.curve_info" with mnemonic name, units of 
%           measurement, and description

% UPDATE HISTORY
%           July 27, 2003; second output argument


global S4M

mnems=wlog.curve_info(:,1);

if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else
   idx=find(ismember(lower(mnems),lower(mnem)));
end
if ~isempty(idx) && length(idx) == 1
   if nargin == 2
      curve=wlog.curves(:,idx);
   else
      if wlog.step > 0
         curve=interp1(wlog.curves(:,1),wlog.curves(:,idx),depths,'*linear');
      else
         curve=interp1q(wlog.curves(:,1),wlog.curves(:,idx),depths);
      end
   end
   if nargout == 2
      info=wlog.curve_info(idx,:);
   end
   return
end

% Handle error condition
if isempty(idx)
  disp([' Curve "',mnem,'" not found. Available curves are:'])
  disps(cell2str(mnems,', '))
else
  disps([' More than one curve found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination')
