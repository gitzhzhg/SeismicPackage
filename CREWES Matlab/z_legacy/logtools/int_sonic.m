function tson=int_sonic(slog,zlog)
% tson=int_sonic(slog,zlog)
%
% INT_SONIC directly integrates a sonic log and computes TWO WAY
% travel time. The sonic log is assumed to be in slowness in
% units of msec/1000length_units
%
% slog = vector of sonic log samples
% zlog = vector of depths of the same size as slog
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
nz=length(slog);
%generate a vector of differences
dz=diff(zlog);
%a vector of the average of adjacent points on the sonic
s2=(slog(1:nz-1)+slog(2:nz))/2;
%integrate by summing the array product of dz and s2
tson=zeros(size(zlog));
tson(2:nz)=2*(1.e-06)*cumsum(dz.*s2);