function v = getMnemonicUnit(obj,sn,mn)
%
%function v = getMnemonicUnit(obj,sn,mn)
% sn = (partial) section name (char)
% mn = LAS mnemomic (char)
% v  = Mnemonic description (char)
%
% mnemonics are in the 1st row
% units are in the 2nd row
% values are in the 3rd row
% descriptions are in the 4th row
% formats are in the 5th row
% associations are in the 6th row
%
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

% Get indices for mnemonic of interest in the section data
midx = obj.getMnemonicIndex(sn,mn);

switch sum(midx)
    case 0 %mnemonic not found in first row of cellarray
        v='';
    otherwise %units are in the 2nd row
        ca = obj.getSectionData(sn);
        v=ca{2,midx};
end

end