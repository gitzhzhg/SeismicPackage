function info=param_info(structure,mnem)
% Get the row of "structure.parameter_info" that pertains to a particular 
% parameter. If global variable S4M.case_sensitive is set to false, the case
% of the parameter mnemonic is disregarded
%
% Written by: E. Rietsch, September 12, 2003
% Last updated:
%
%             info=param_info(structure,mnem);
% INPUT
% structure   structure with field "parameter_info"
% mnem        parameter mnemonic
% OUTPUT
% info        cell array (cell vector with three components) with info about 
%             column with mnemonic "mnem"
%             {mnemonic,units of measurement,description}

global S4M

mnems=structure.parameter_info(:,1);
if S4M.case_sensitive
   idx=find(ismember(mnems,mnem));
else
   idx=find(ismember(lower(mnems),lower(mnem)));
end

if ~isempty(idx) && length(idx) == 1
   info=structure.parameter_info(idx,:);
   return
end

% Handle error condition
if isempty(idx)
   disp([' Parameter "',mnem,'" not found. Available parameters are:'])
   disp(mnems')
else
   disp([' More than one parameter found: ',cell2str(mnems(idx),', ')])
end

error(' Abnormal termination')
