function l_redefine(varargin)
% Function redefines one or more of the standard curve mnemonics (e.g. to make them the same as
% esisting curve mnemonics).
% Written by: E. Rietsch: July 15, 2001
% Last updated: September 25, 2004: global variable CURV_MNEMONICS chande to CURVES
%
%               l_redefine(varargin)
% INPUT
% varargin  two-elemnet cell arrays. The first element is a string representing an existing
%       standard curve mnemonic, the second is the desired curve mnemonic. 
%       If S4M.case_sensitive == 0, the
%       existing curve mnemonic is not case sensitive.
%
% EXAMPLE
%       l_redefine({'DTp','DT'},{'rho','RHOB'})         % Change standard log mnemonic 'DTp'
%                                          to 'DT' and standard log mnemonic 'rho' to 'RHOB'
%
%       See also functions "l_rename" and "l_curve" with "option" 'rename'

global CURVES

if isempty(CURVES)
   error(' Global variable "CURVES" must be defined (run function "presets")')
end
  
for ii=1:length(varargin)
   mnems=varargin{ii};
   if size(mnems) ~= 2
      disp(mnems)
      error(' Input arguments: old and new header mnemonic must be represented as a two-element cell')
   end
   ier=0;
   mnem1=lower(mnems{1});
   if ~isfield(CURVES,mnem1)
      disp(['"',mnems{1},'" is not a standard curve mnemonic'])
      ier=1;
   else
      CURVES.(mnem1)=mnems{2};
   end
end

if ier
   disp(' Standard curve mnemonics are:')
   disp(cell2str(fieldnames(CURVES),', '))
end


