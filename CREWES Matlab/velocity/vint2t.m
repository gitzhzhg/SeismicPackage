function [t1,t2]=vint2t(vint,z,zout,tnot)
% VINT2T: compute time given interval velocity versus depth
%
% [t1,t2]=vint2t(vint,z,zout,tnot)
% [t1,t2]=vint2t(vint,z,zout);
% t1=vint2t(vint,z);
%
% Given interval velocity versus depth, compute 1 way vertical
% time as a function of depth.
%
% vint ... vector of interval velocities
% z ... vector of depths for each interval velocity.
%   Note: vint(k) is the velocity between z(k) and z(k+1). The
%   last v is used indefinitly as a half space
% zout ... vector of depths for which vertical travel times
%	are desired.
% ********* default = z ***************
% tnot ... constant time shift (seconds). (The time to z(1) )
%    Setting tnot=0 makes z(1) the reference datum
% ********** default = vint(1)/z(1) *********
%
% t1 ... vector of 1-way vertical times (seconds) corresponding to zout
% t2 ... vector of 1-way vertical times (seconds) corresponding to z
% Note: if zout was not the same as z, then you may wish to 
% compute a set of velocities to match. Use vint2vrms or vint2vave
% depending on which type of interval velocities you want.
%
% G.F. Margrave, June 1995
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

if(nargin<4)
	tnot=z(1)/vint(1);
end

if(length(vint)~=length(z))
    error('vint and z must be vectors of the same length')
end

if(nargin<3)
	%force column vectors
	vint=vint(:);z=z(:);
	
	% compute a dz vector
	nz=length(z);
	dz=diff(z);

	% integrate
	t1=zeros(size(z));
	t1(2:nz)=cumsum(dz./vint(1:nz-1));

	t1=t1+tnot;
	t2=t1;
else
	%force column vectors
	vint=vint(:);z=z(:);zout=zout(:);
	
	vintout=pcint(z,vint,zout);
	nz=length(z);
	znew=[z(:);zout(:)];
	vnew=[vint(:);vintout(:)];
	[znew,iz]=sort(znew);
	vnew=vnew(iz);

	nz2=length(znew);
	dz=diff(znew);
	tnew=zeros(size(znew));
	tnew(2:nz2)=cumsum(dz./vnew(1:nz2-1));
	tnew=tnew+tnot;

   t=zeros(size(vint));
 	t(iz)=tnew; %unsort
	t1=t(nz+1:length(t));
	t2=t(1:nz);
end