function logout=cliplogs(login,z,vallim,inter,zlim,cons)
% logout=cliplogs(login,z,vallim,inter,zlim,cons)
%
% CLIPLOGS will remove values in the log that are outside an acceptable
% range given by the user.  The holes are then filled by nan,a constant or
% interpolated using linear, spline or pchip interpolation methods.
%
% login  = signal or log input
% z      = the depth or time vector coresponding to sigin
% vallim = range of log acceptable values [min,max]
% inter  = the type of interpolation
%            0= no interpolation the null value will be replaced with nan
%            1= linear interploation will be applied
%            2= pchip interplotation will be applied **DEFAULT**
%            3= spline interpolation will be applied
%            4= a constant will replace the null values
% zlim   = range of depth values clipping is to be done [min,max]
%                  **DEFAULT** [min(z),max(z)]
% cons   = a constant to fill the null values with when inter=4
%
% logout = log that has been cliped and interpolated
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
if nargin <5
zlim=[max(z),min(z)];
end
if inter==4
    if nargin<6
        error('Constant must be specified if interpolation method is set to 4');
    end
else
    cons=0;
end
ind=min(near(z,min(zlim))):max(near(z,max(zlim)));
nul=-999999999999999;
log=login;
log(login<min(vallim))=nul;
log(login>max(vallim))=nul;
log=removenull(log,z,nul,inter,cons);

logout=login;logout(ind)=log(ind);