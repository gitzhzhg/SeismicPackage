function vave=vrms2vave(vrms,t)
% VRMS2VAVE: convert rms velocity to average
%
% vave=vrms2vave(vrms,t)
%
% VRMS2VAVE computes average velocity as a function of time
% given rms velocity as a function of time. The method is simply to call
% vrms2vint followed by vint2vave. Non-physical vint values resulting from
% the vrms function are thrown out and new values are interpolated from
% neighbors.
%
% vrms = input rms velocity vector
% t = input time vector to go with vrms
% tout = vector of output times at which vave estimates are
%	desired. Requirement tout >= t(1)
%*********** default tout=t *********
%
%
% G.F. Margrave June 1995, CREWES Project
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

%test input arguments
if(length(vrms)~=length(t))
	error('vrms and t must have same lengths')
end

%force column vectors
vrms=vrms(:);
t=t(:);

vint=vrms2vint(vrms,t,1);
vave=vint2vave(vint,t);