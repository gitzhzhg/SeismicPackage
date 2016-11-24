function structure=ds_add_parameter(structure,value,info)
% Add a parameter to a structure; includes creating (or appending to) the 
% field 'parameter_info', which is a three-column cell array dit info about 
% each parameter
% 
% Written by: E. Rietsch: September 12, 2003
% Last updated: July 8, 2006: Use dynamic field names
%
%            structure=ds_add_parameter(structure,value,info)
% INPUT
% structure  structure 
% value      parameter value
% info       three-element cell vector; the elements are strings: 
%            {parameter mnemonic, units of measurement, description}
% OUTPUT
% structure  input structure with parameter added
%
% EXAMPLE
%            wlog=l_data;
%            wlog=ds_add_parameter(wlog,100,{'wd','ft','Water depth'})
%            show(wlog.parameter_info)


if isfield(structure,'parameter_info')
   [index,ier]=param_index1(structure,info{1});  %#ok second parameter needed 
                     % to simulate outside error handling
   if isempty(index)
      structure.parameter_info=[structure.parameter_info;info];
   else
      structure.parameter_info(index,:)=info;
   end
else
   structure.parameter_info=info;
end

structure.(info{1})=value;
