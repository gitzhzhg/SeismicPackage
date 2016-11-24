function PI_limlnoptions()
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


h=get(gcf,'userdata');
hlimbox=h{14};
limdat=get(hlimbox,'userdata');

stdat=get(gcbo,'userdata');
st={'linestyle' 'marker' 'color'};
ls={'-' '--' '-.' ':'};
mk={'*' 'x' 'o' '+' '.'};
col={'r' 'g' 'b' 'k' 'm'};

ch={ls mk col};
ch=ch{stdat(1)};

set(limdat{1},st{stdat(1)},ch{stdat(2)});
set(limdat{2},st{stdat(1)},ch{stdat(2)});

if(stdat(1)==3) %only change center point if asking for different color
    set(limdat{4},st{stdat(1)},ch{stdat(2)});
end