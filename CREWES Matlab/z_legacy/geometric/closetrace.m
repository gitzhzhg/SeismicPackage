function ind = closetrace(xxx,yyy,xw,yw,rd) 
% function ind = closetrace(xxx,yyy,xw,yw,rd) 
%
% function linewelltie(action) 
%  finds closest trace (point) in line to given well location (xw,yw)
%  (as long as the trace is within a radius rd)
%  line defined by points xxx(i),yyy(i)
%  returns index of closest trace 
%  returns null if no trace is close enough
%
%  T.N.Bishop,  December 1993, CPTC Canada
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
%  FIND WELL TO SEISMIC MATCHES THAT ARE WITHIN RADIUS
    ind = [];
    dclose = closeline(xxx,yyy,xw,yw);
    if dclose <= rd
      d=sqrt( (xxx-xw).^2 + (yyy-yw).^2 );
      dmin = min(d);
      if dmin <= rd
        ind = find(d == dmin);
      end
    end