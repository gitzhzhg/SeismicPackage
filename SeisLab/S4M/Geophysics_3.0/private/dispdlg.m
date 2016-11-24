function dispdlg(txt)
% Generalization of disp function; text is written to screen if 
% S4M.interactive  == 0 and written to a dialog box if S4M.interactive  == 1
%
% Written by: E. Rietsch: April 20, 2004
% Last updated:
%
%          dispdlg(txt)
% INPUT
% txt      Text to display
%
global S4M

try
  if S4M.deployed || S4M.interactive
     handle=msgdlg(txt);
  else
     disp(txt)
  end

catch
  disp(txt)
end
