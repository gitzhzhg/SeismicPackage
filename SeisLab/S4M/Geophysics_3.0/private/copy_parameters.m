function structout=copy_parameters(structin,structout)
% Function copies all parameters of a log structure, seismic structure, or table  
% structure to another (parameter in a structure can be identified by an entry 
% in the 'parameter_info' field (cell array) of the structure); if a parameter 
% exists already in the output structure it will be overwritten.
%
% Written by: E. Rietsch: October 6, 2005
% Last updated: July 22, 2007: "mlint" compliant
%
%            structout=copy_parameters(structin,structout)
% INPUT
% structin   structure from which the parameters are to be copied
% structout  structure to which the parameters are to be copied
% OUTPUT
% structout  structure to which the parameters are to be copied

if ~isstruct(structin)
   error(' First input data set must be a structure.')
end

if ~isfield(structin,'parameter_info')
   return
end

params=structin.parameter_info(:,1);

for ii=1:length(params)
%   structout=ds_add_parameter(structout,getfield(structin,params{ii}), ...
%                           structin.parameter_info(ii,:));
   structout=ds_add_parameter(structout,structin.(params{ii}), ...
                           structin.parameter_info(ii,:));
end
