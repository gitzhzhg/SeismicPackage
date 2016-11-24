function wlog=add_curve_type(wlog,curvetype)
% Function adds curve type info to a log structure if a field "curve_type" exists
% If the the curve type has already been defined for the mnemonic, it is overwritten
%
% Written by: E. R.: June 4, 2004
% Last updated: February 7, 2007: Handle empty field 'curve_types'
%
%	           wlog=add_curve_type(wlog,curvetype)
% INPUT
% wlog        well log structure
% curve type  three-element cell array with 
%             curve mnemonic, generic curve type, description
%             e.g. {'aImp','Imp','impedance'}
% OUTPUT
% wlog        well log with updated field curve type (if it existed on input)

global S4M

if ~isfield(wlog,'curve_types') || isempty(wlog.curve_types)
   wlog.curve_types=curvetype;
   return
end

mnems=wlog.curve_types(:,1);
if S4M.case_sensitive
   index=find(ismember(mnems,curvetype{1}));
else
   index=find(ismember(lower(mnems),lower(curvetype{1})));
end

if isempty(index)
   wlog.curve_types=[wlog.curve_types;curvetype];
else
   wlog.curve_types(index,:)=curvetype;
end
