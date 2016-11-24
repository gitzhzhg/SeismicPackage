function vave=vint2vave(vint,t,tout)
% VINT2VAVE: convert interval velocity to average
%
% vave=vint2vave(vint,t,tout)
% vave=vint2vave(vint,t)
%
% VINS2VAVE computes average velocity as a function of time
% given interval velocity as a function of time.
%
% vint = input interval velocity vector
% t = input time vecot to go with vint
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
if(length(vint)~=length(t))
	error('vint and t must have same lengths')
end

%force column vectors
vint=vint(:);
t=t(:);

if(nargin < 3)
	%integrate vint
	dt=diff(t);
	vave=zeros(size(vint));
	nt=length(t);
	i1=1:nt-1;
	vave(i1)=cumsum(dt.*vint(i1));

	vave(i1)=vave(i1)./(t(i1+1)-t(1));
	vave(end)=vave(end-1);
   vave=vave(:); %return a column vector
else
   tout=tout(:);
   ind=find(tout<t(1));
   if(~isempty(ind))
		error('tout must be greater than t(1)');
	end
   dt=diff(t);
	vave2=zeros(size(vint));
	nt=length(t);
	i1=1:nt-1;
	vave2(i1)=cumsum(dt.*vint(i1));

	vave2(i1)=vave2(i1)./(t(i1+1)-t(1));
	vave2(end)=vave2(end-1);
	vave=pwlint(t,vave2,tout);
   vave=vave(:); %return a column vector;
end