function description=s_gd(seismic,mnem)
% Get the description of the header with mnemonic "mnem" from seismic data set "seismic"
% If the global variable S4M.case_sensitive is set to false, the case of the header mnemonic
% is disregarded
%
% Written by: E. Rietsch: December 30, 2000
% Last updated: December 30, 2000: compact header representation
%
%             description=s_gd(seismic,mnem)
% INPUT
% seismic     seismic data set
% mnem        header mnemonic
% OUTPUT
% description string with description of header with mnemonic "mnem"

global S4M

if strcmpi(mnem,'trace_no')       % Implied header "trace_no"
   description='Trace number';
   return
end

mnems=seismic.header_info(:,1);

if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else 
   idx=find(ismember(lower(mnems),lower(mnem)));
end

if length(idx) == 1
   description=seismic.header_info{idx,3};
   return
end

%       Handle error condition
if isempty(idx)
   disp([' Header "',mnem,'" not found. Available headers (in addition to the pseudo-header) are:'])
   disp(mnems')
else
   disp([' More than one header found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination.')
