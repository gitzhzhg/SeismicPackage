function yi=pwlint(x,y,xi)
% PWLINT: piecewise linear interpolation (much faster than interp1)
%
% yi=pwlint(x,y,xi)
%
% PWLINT performs linear interpolation when it is known that that
% input function (x,y) is piecewise linear. If length(x) is much less than
% the length(xi), this is MUCH faster than the built in INTERP1. Points
% in xi which are outside the bounds of x will return nans.
%
% G.F. Margrave
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

if(length(x)<=length(xi))
    nsegs=length(x)-1;
    yi=nan*zeros(size(xi));

    for k=1:nsegs

        %find the points in this line segment
        ii=between(x(k),x(k+1),xi,2);

        if( ii )
            % interpolate
            yi(ii)=y(k)+(y(k+1)-y(k))*(xi(ii)-x(k))/(x(k+1)-x(k));
        end

    end
else
    yi=nan*zeros(size(xi));
    for k=1:length(xi)
        ii=surround(x,xi(k));
        if(~isempty(ii))
            yi(k) = y(ii)*(x(ii+1)-xi(k))/(x(ii+1)-x(ii))+y(ii+1)*(xi(k)-x(ii))/(x(ii+1)-x(ii));
        else
            yi(k)=nan;
        end
    end
end