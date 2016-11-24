function dclose = closeline(xxx,yyy,xw,yw) 
% function dclose = closeline(xxx,yyy,xw,yw) 
%
% function dclose = closeline(xxx,yyy,xw,yw) 
% function linewelltie(action) 
%  finds closest possible point in line area
%    (line defined by xxx(i),yyy(i))
%     to given well location (xw,yw). 
%    Line area is defined by
%     circumscribed rectangle which surrounds line.
%  returns dclose, the distance of closest 
%     possible point in the line area to the well.
%
%  T. N. BISHOP,  DECEMBER 1993,  CPTC CANADA
%   see also seis2well, linewelltie
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
    xxxmin = min(xxx);
    yyymin = min(yyy);
    xxxmax = max(xxx);
    yyymax = max(yyy);
    if xw >= xxxmax
      xclose=xxxmax;
    elseif xw <= xxxmin
      xclose=xxxmin;
    else
      xclose=xw;
    end
    if yw >= yyymax
      yclose=yyymax;
    elseif yw <= yyymin
      yclose=yyymin;
    else
      yclose=yw;
    end
    dclose=sqrt( (xclose-xw).^2 + (yclose-yw).^2 );