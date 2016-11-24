function [header,info]=ds_gh(dataset,mnem)
% Function gets values of header with mnemonic "mnem" from dataset "dataset".
% Works for seismic and pseudo-well datasets.
% If global variable "S4M.case_sensitive" is set to "false", the case of the 
% header mnemonic is disregarded.
%
% Written by: E. Rietsch, January 27, 2007
% Last updated: December 11, 2007: Make it usable for seismic and pseudo-well datasets.
%
%            [header,info]=ds_gh(dataset,mnem)
% INPUT
% dataset     pseudo-well data set
% mnem       header mnemonic (string or first entry of a cell array)
% OUTPUT
% header     row vector with header values
% info       relevant row of "dataset.header_info" with mnemonic name, units of 
%            measurement, and description

global S4M

switch dataset.type
case 'seismic'
   if strcmpi(mnem,'trace_no')       % Implied trace number "trace_no"
      header=1:size(dataset.traces,2);
      if nargout > 1
         info={'trace_no','n/a','Trace number'};
      end
      return
   end
case 'pseudo-wells'
   if strcmpi(mnem,'well_no')         % Implied header "well_no"
      header=1:panelsize(dataset,2);
      if nargout > 1
         info={'well_no','n/a','Well number'};
      end
      return
   end
otherwise
   error(' First input argument must be a seismic or a pseudo-well dataset.')
end

if iscell(mnem)
   mnem=mnem{1};
end


mnems=dataset.header_info(:,1);

if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else 
   idx=find(ismember(lower(mnems),lower(mnem)));
end

if length(idx) == 1
   header=dataset.headers(idx,:);
   if nargout == 2
      info=dataset.header_info(idx,:);
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

error('Abnormal termination')
