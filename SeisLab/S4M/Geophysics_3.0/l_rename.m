function wlog=l_rename(wlog,varargin)
% Function replaces one or more curve mnemonics by new ones
% The function also works with structure arrays.
% See also: l_curve with option "rename"


% Written by: E. Rietsch: December 24, 2000
% Last updated: November 8, 2004: update also field "curve_types (if it exists).                
%
%          wlog=l_rename(wlog,varargin)
% INPUT
% wlog     log structure or structure array
% varargin  two-element cell arrays. The first element is a string representing an existing
%          curve mnemonic, the second is the desired curve mnemonic. If S4M.case_sensitive == 0, the
%          existing curve mnemonic is not case sensitive.
% OUTPUT
% wlog   log structure or structure array with the new header mnemonics
%
% EXAMPLE
%       wlog=l_data;
%       wlog=l_rename(wlog,{'DTP','DT'},{'RHO','rhob'})  % Change 'DT' to 'DTP' and 'RHOB' to 'rho'
%       l_curve(wlog)
%       presets              % Resore original default values


global S4M

for kk=1:length(wlog)
   for ii=1:length(varargin)
      mnems=varargin{ii};
      if size(mnems) ~= 2
         disp(mnems)
         error(' Input arguments: old and new header mnemonic must be represented as a two-element cell')
      end

      idx=curve_index1(wlog(kk),mnems{1});

      %		Check if new mnemonic already exists
      wlog(kk).curve_info(idx,1)=mnems(2);
      wlog(kk).curve_info=description_substitution(wlog(kk).curve_info);
      if isfield(wlog(kk),'curve_types')
         if S4M.case_sensitive
            idx=find(ismember(wlog(kk).curve_types(:,1),mnems{1}));
         else
            idx=find(ismember(lower(wlog(kk).curve_types(:,1)),lower(mnems{1})));
         end
         if ~isempty(idx)
            wlog(kk).curve_types(idx,1)=mnems(2);
         end
      end
      ier=l_check(wlog(kk));
      if ier
         alert('Possible inconsistency in the well log')
      end   
   end
   
end

