function mnemo=mnem2tex(mnemi)
% Function substitutes TeX-style mnemonics for input mnemonics (used for
% axis annotation plots, legends, titles, etc); e.g. DT_S ==> DT\_S
%
% Written by: E. Rietsch: May, 6, 2000
% Last updated: September 20, 2001: Handle case when the substitution 
%                                               has already been made
%          mnemo=mnem2tex(mnemi)
% INPUT
% mnemi    string or cell with input text
% OUTPUT
% mnemo    string with text where backslashes, "\", preceed any underscores "_".

mnemo=strrep(mnemi,'\_','_');   % If the substitution has already been made: reverse it
mnemo=strrep(mnemo,'_','\_');
