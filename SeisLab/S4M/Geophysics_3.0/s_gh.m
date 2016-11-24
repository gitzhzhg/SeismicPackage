function [header,info]=s_gh(seismic,mnem)
% Function gets header with mnemonic "mnem" from seismic data set "seismic"
% If global variable "S4M.case_sensitive" is set to false, the case of the 
% header mnemonic is disregarded.
% DEPRECATED: use "ds_gh" instead.
%
% Written by: E. Rietsch, December 27, 2000
% Last updated: July 28, 2006; streamlining
%
%            [header,info]=s_gh(seismic,mnem)
% INPUT
% seismic    seismic data set
% mnem       header mnemonic (string or first entry of a cell array)
% OUTPUT
% header     row vector with header values
% info       relevant row of "seismic.header_info" with mnemonic name, units of 
%            measurement, and description

global S4M

if iscell(mnem)
   mnem=mnem{1};
end

if strcmpi(mnem,'trace_no')       % Implied header "trace_no"
   header=1:size(seismic.traces,2);
   if nargout > 1
      info={'trace_no','n/a','Trace number'};
   end
   return
end

mnems=seismic.header_info(:,1);

if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else 
   idx=find(ismember(lower(mnems),lower(mnem)));
end

if length(idx) == 1
   header=seismic.headers(idx,:);
   if nargout == 2
      info=seismic.header_info(idx,:);
   end
   return
end

%	Handle error condition
if isempty(idx)
   disp([' Requested header "',mnem,'" has not been found. Available headers are:'])
   disp(cell2str(mnems,', '))
else
   disp([' More than one header found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination')
