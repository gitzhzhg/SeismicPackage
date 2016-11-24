function bool=iscurve(wlog,mnem)
% Returns true if mnem is a curve mnemonic in log structure "wlog"
% and false otherwise
%
% Written by: E. Rietsch: December 17, 2003
% Last updated:
%
%         bool=iscurve(wlog,mnem)
% INPUT
% wlog    log structure
% mnem    curve mnemonic
% OUTPUT
% bool    boolean variable; set to true if mnem is a curve mnemonic in 
%         log structure "wlog" and false otherwise

[dummy,ierr]=curve_index1(wlog,mnem);  %#ok First output argument is not required

bool=~ierr;
