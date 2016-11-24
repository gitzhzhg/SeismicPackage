function [param,info]=get_parameter(dataset,mnem,abortOnError)
% Get units of a parameter from a  dataset (seismic, log, etc.)
% The dataset must have a field 'parameter_info'.
%
% Written by: E. Rietsch: May 2, 2006
% Last updated:
%
%          [param,info]=get_parameter(dataset,mnem,abortOnError)
% INPUT
% dataset  dataset represented by a structure
% mnem     parameter mnemonic
% abortOnError   optional logical variable indication what to do in case the
%          requested  parameter is not found.
%          if abortOnError == true the function aborts with an error message
%          if no parameter with the requested mnemonic is found
%          Default: abortOnError=true
% OUTPUT
% param    parameter value
% info     three-column cell array with parameter mnemonic, units of 
%          measurement, and description

param=[];
info=[];

if nargin < 3
   abortOnError=true;
end

if ~isfield(dataset,'parameter_info')
   if abortOnError
      error('Data set has no field "parameter_info".')
   else
      return
   end
end

idx=find(ismember(dataset.parameter_info(:,1),mnem));

if length(idx) == 1
   param=dataset.(mnem);
   info=dataset.parameter_info(idx,:);
else
   if abortOnError
      if isempty(idx)
         disp(['Mnemonic "',mnem,'" not found.'])
         disp('Existing mnemonics are:')
         disp(cell2str(dataset.parameter_info(:,1),', '))
         error('Abnormal termination')
   
      else
         disp(['More than one nemonic "',mnem,'" found.'])
         disp('Existing mnemonics are:')
         disp(cell2str(dataset.parameter_info(:,1),', '))
         error('Abnormal termination')
      end
   else
      return
   end
end
