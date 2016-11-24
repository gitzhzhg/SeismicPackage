function sigout=removenull(sigin,z,nul,inter,cons)
% sigout=removenull(sigin,z,nul,inter,cons)
%
% REMOVENULL will remove any null values from a log and replace it either
% with a nan or interpolate the missing data.  If the log has large areas of
% null values the interpolation will be inaccurate.
%
% sigin = signal or log input
%  z    = the depth or time vector coresponding to sigin
% nul   = the null value **DEFAULT =-999.25**
% inter = the type of interpolation
%            0= no interpolation the null value will be replaced with nan
%            1= linear interploation will be applied
%            2= pchip interplotation will be applied **DEFAULT**
%            3= spline interpolation will be applied
%            4= a constant will replace the null values
% cons  = a constant to fill the null values with when inter=4
%
% sigout = signal with nulls removed and interpolated depending on settings
%
% H.J.E. Lloyd November 2013
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
if nargin <4
    inter=2;
end
if nargin <3
nul=-999.25;
end
if inter==4
    if nargin<5
        error('Constant must be specified if interpolation method is set to 4');
    end
end
method=['linear';'pchip ';'spline'];
indnull=find(sigin==nul);
indgood=find(sigin~=nul);

sigout=sigin;
if ~isempty(indnull)
if inter==0
    sigout(indnull)=nan;
elseif inter==4
    sigout(indnull)=cons;
else
    sigout(indnull)=interp1(z(indgood),sigin(indgood),z(indnull),method(inter,:));
end
end