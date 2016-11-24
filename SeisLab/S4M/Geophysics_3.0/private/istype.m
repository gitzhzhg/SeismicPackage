function bool=istype(structure,typ)
% Check if structure has field "type" and if it matches the string "typ",
% (or one of the strings in cell array "typ").
%
% Written by: E. Rietsch: September 1, 2003
% Last updated: January 27, 2008: Update check of "type"
%
%            bool=istype(structure,typ)
% INPUT
% structure  Matlab structure
% typ        string or cell array of strings; possible strings are: 'seismic',
%            'well_log','table','pdf','pseudo-wells'
% OUTPUT
% bool       logical variable; set to true if "structure has field "type"
%            and if it is equal to string "typ";
%            otherwise it is set to false
% EXAMPLES
%            seis=s_data;
%            istype(seis,'seismic')
%            istype(seis,{'well_log','seismic'})

% UPDATE HISTORY
%            April 28, 2006: input argument "typ" can be a cell array

if ~ischar(typ) && ~iscell(typ)
   error(['Input argument "',typ,'" must be a string or a cell array of strings.'])
end

bool=false;
if ~isempty(structure)  &&  isstruct(structure(1))
   if isfield(structure(1),'type')
      bool=any(ismember({structure(1).type},typ));
   end
end
