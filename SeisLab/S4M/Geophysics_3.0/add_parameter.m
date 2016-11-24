function structure=add_parameter(structure,value,info)
% Add a parameter to a structure; includes creating (or appending to) the 
% field 'parameter_info', which is a three-column cell array dit info about 
% each parameter.
%
% OBSOLETE: Replace by "ds_add_parameter".
% 
% Written by: E. Rietsch: September 12, 2003
% Last updated: July 8, 2006: Use dynamic field names
%
%            structure=add_parameter(structure,value,info)
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
%            wlog=add_parameter(wlog,100,{'wd','ft','Water depth'})
%            show(wlog.parameter_info)

alert('DEPRECATED: use "ds_add_parameter" instead.')

structure=ds_add_parameter(structure,value,info);
