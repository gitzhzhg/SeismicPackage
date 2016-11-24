function [arymig,tmig,xmig]=psmig(aryin,aryvel,t,x)

% [arymig,tmig,xmig]=psmig(aryin,aryvel,t,x)
%
% PSMIG is a phase shift time migration routine.
%
% aryin ... matrix of zero offset data. One trace per column.
% aryvel ... velocity information. The are 2 possibilities:
%		1) if a scalar, then a constant velocity migration with
%		velocity=aryvel is performed.
%		2) if a vector, then it must be the same length as the number
%		of rows in aryin. In this case it is assumed to be an rms 
%		velocity function (of time) which is applied at all positions
%		along the section.
% t ... if a scalar, this is the time sample rate in SECONDS.
%		If a vector, it gives the time coordinates for the rows of 
%		aryin.
% x ... if a scalar, this is the spatial sample rate (in units 
%		consistent with the velocity information. If a vector, then
%		it gives the x coordinates of the columns of aryin
%
% OUTPUT arguments
%
% arymig ... the output migrated time section
% tmig ... t coordinates of migrated data
% xmig ... x coordinates of migrated data
%
% By Xinxiang Li,  CREWES Project, U of Calgary, 1996
% Reference : GEOPHYSICS, V.44, pp.1661-1666
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

[nsamp,ntr]=size(aryin);

% check the validity input arguments

%  ---- check t  ----
if(length(t)>1)
	if(length(t)~=nsamp)
		error('Incorrect time specification')
	end
	[nrow,nvol] = size(t) ;
	if nrow < nvol
		t = t' ;
	end
	dt=t(2)-t(1);
else
	dt=t;
	t=((0:nsamp-1)*dt)';
end

%  ---- checck x ----
if(length(x)>1)
	if(length(x)~=ntr)
		error('Incorrect x specification')
	end
	[nrow,nvol] = size(x) ;
	if nrow > nvol
		x = x' ;
	end
	dx=x(2)-x(1);
else
	dx = x;
	x=(0:ntr-1)*dx;
end

tmig = t;
xmig = x;

%  ---- test velocity info ----
[nvsamp,nvtr]=size(aryvel);

if min([nvsamp,nvtr]) > 1
	error('The input velocity should be a column vector');
elseif(length(aryvel) > 1)
	if nvtr > 1
		%might be transposed vector
		if(nvtr==nsamp)
			aryvel=aryvel';
		else
			error('Velocity vector is wrong size');
		end
	else
		if nvsamp ~= nsamp
			error('Velocity vector is wrong size');
		end
	end

else 
	aryvel=aryvel*ones(nsamp,1);
end
%
%ok, we now have a velocity vector the same length of the input trace
%

arymig = zeros(size(tmig*xmig));

nsamp2 = 2;
while nsamp2 <= nsamp
	nsamp2 = nsamp2*2;
end
ntr2 = 2;
while ntr2 < ntr
	ntr2 = 2*ntr2;
end

npadtr = ntr2-ntr;
npadsamp = nsamp2-nsamp;
if npadtr > 0
	aryin = [aryin'; zeros(npadtr,nsamp)]';
end
if npadsamp > 0
	aryin = [aryin; zeros(npadsamp,ntr2)];
end

tzeta(1:ntr2/2) = 2*pi*(0:ntr2/2-1)/(ntr2*dx);
tzeta(ntr2:-1:ntr2/2+1) = -tzeta(1:ntr2/2);
tzeta(ntr2)=2*pi/dx;
omega(1:nsamp2/2) = 2*pi*(0:nsamp2/2-1)/(nsamp2*dt);
omega(nsamp2:-1:nsamp2/2+1) = -omega(1:nsamp2/2);
omega(nsamp2)=2*pi/dt;
omega1=omega;
ind=find(abs(omega) < 0.00001);
omega1(ind)=0.00001;
tpad=((0:nsamp2-1)*dt)';
xpad=(0:ntr2-1)*dx;

% step 1
for j = 1:nsamp2
	arytk(j,:)=fft(aryin(j,:));
end
clear aryin;

%step 2
for s = 1:ntr2
if (rem(s,20)==0)
disp([' Migrated wavenumber #' int2str(s) ' out of ' int2str(ntr2) ]);
end
	%step 3
	%aryfk(:,s)=fft(arytk(:,s));
	%nsamp22 = nsamp2/2;
	%aryfk(nsamp22+1:nsamp2,s)=aryfk(nsamp22:-1:1,s)-2*real(aryfk(nsamp22:-1:1,s));
	
	for f=1:nsamp2
	aryfk(f,s)=sum(arytk(:,s).*exp(-i*tpad*omega(f)));
	end
		%step 5
		phi = zeros(nsamp2,1);
	% step 4
	for j = 1: nsamp
		%step 6
		omegalimit=0.5*abs(tzeta(s)*aryvel(j));
		ind = find(abs(omega) > omegalimit);
		temp = omega(ind).*sqrt(1-omegalimit^2./omega(ind).^2)*dt;
		%step 7
		phi(ind) = phi(ind)+temp';
		
		% step 8
		[aa,bb]=size(aryfk(ind,s));
		[cc,dd]=size(exp( phi(ind)*i));
		if( aa==0 | bb==0 | cc==0 | dd==0 )
			abcd = 0;
		else 
			abcd = aryfk(ind,s) .* exp( phi(ind) * i);
		end
		migfk(j,s)=sum( abcd );
		
		%step 9
	end
%step 10
end
%step 11
for j = 1:nsamp
	mig = ifft(migfk(j,:));
	%for ix = 1: ntr2
	%mig(ix)=sum(migfk(j,:).*exp(i*xpad(ix)*tzeta));
	%end
	arymig(j,:) = real(mig(1:ntr));
end






 =======
function [arymig,tmig,xmig]=psmig(aryin,aryvel,t,x)

% [arymig,tmig,xmig]=psmig(aryin,aryvel,t,x)
%
% PSMIG is a phase shift time migration routine.
%
% aryin ... matrix of zero offset data. One trace per column.
% aryvel ... velocity information. The are 2 possibilities:
%		1) if a scalar, then a constant velocity migration with
%		velocity=aryvel is performed.
%		2) if a vector, then it must be the same length as the number
%		of rows in aryin. In this case it is assumed to be an rms 
%		velocity function (of time) which is applied at all positions
%		along the section.
% t ... if a scalar, this is the time sample rate in SECONDS.
%		If a vector, it gives the time coordinates for the rows of 
%		aryin.
% x ... if a scalar, this is the spatial sample rate (in units 
%		consistent with the velocity information. If a vector, then
%		it gives the x coordinates of the columns of aryin
%
% OUTPUT arguments
%
% arymig ... the output migrated time section
% tmig ... t coordinates of migrated data
% xmig ... x coordinates of migrated data
%
% By Xinxiang Li,  CREWES Project, U of Calgary, 1996
% Reference : GEOPHYSICS, V.44, pp.1661-1666
%
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.
 
% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by 
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE

%flops(0);
[nsamp,ntr]=size(aryin);

% check the validity input arguments

%  ---- check t  ----
if(length(t)>1)
	if(length(t)~=nsamp)
		error('Incorrect time specification')
	end
	[nrow,nvol] = size(t) ;
	if nrow < nvol
		t = t' ;
	end
	dt=t(2)-t(1);
else
	dt=t;
	t=((0:nsamp-1)*dt)';
end

%  ---- checck x ----
if(length(x)>1)
	if(length(x)~=ntr)
		error('Incorrect x specification')
	end
	[nrow,nvol] = size(x) ;
	if nrow > nvol
		x = x' ;
	end
	dx=x(2)-x(1);
else
	dx = x;
	x=(0:ntr-1)*dx;
end

tmig = t;
xmig = x;

%  ---- test velocity info ----
[nvsamp,nvtr]=size(aryvel);

if min([nvsamp,nvtr]) > 1
	error('The input velocity should be a column vector');
elseif(length(aryvel) > 1)
	if nvtr > 1
		%might be transposed vector
		if(nvtr==nsamp)
			aryvel=aryvel';
		else
			error('Velocity vector is wrong size');
		end
	else
		if nvsamp ~= nsamp
			error('Velocity vector is wrong size');
		end
	end

else 
	aryvel=aryvel*ones(nsamp,1);
end
%
%ok, we now have a velocity vector the same length of the input trace
%

arymig = zeros(size(tmig*xmig));

nsamp2 = 2;
while nsamp2 <= nsamp
	nsamp2 = nsamp2*2;
end
ntr2 = 2;
while ntr2 < ntr
	ntr2 = 2*ntr2;
end

npadtr = ntr2-ntr;
npadsamp = nsamp2-nsamp;
if npadtr > 0
	aryin = [aryin'; zeros(npadtr,nsamp)]';
end
if npadsamp > 0
	aryin = [aryin; zeros(npadsamp,ntr2)];
end

tzeta(1:ntr2/2) = 2*pi*(0:ntr2/2-1)/(ntr2*dx);
tzeta(ntr2:-1:ntr2/2+1) = -tzeta(1:ntr2/2);
tzeta(ntr2)=2*pi/dx;
omega(1:nsamp2/2) = 2*pi*(0:nsamp2/2-1)/(nsamp2*dt);
omega(nsamp2:-1:nsamp2/2+1) = -omega(1:nsamp2/2);
omega(nsamp2)=2*pi/dt;
omega1=omega;
ind=find(abs(omega) < 0.00001);
omega1(ind)=0.00001;
tpad=((0:nsamp2-1)*dt)';
xpad=(0:ntr2-1)*dx;

% step 1
for j = 1:nsamp2
	arytk(j,:)=fft(aryin(j,:));
end
clear aryin;

%step 2
for s = 1:ntr2
if (s/5 == round(s/5) )
disp([' --- Wavenubmer #' int2str(s) ' ---']);
end
	%step 3
	%aryfk(:,s)=fft(arytk(:,s));
	%nsamp22 = nsamp2/2;
	%aryfk(nsamp22+1:nsamp2,s)=aryfk(nsamp22:-1:1,s)-2*real(aryfk(nsamp22:-1:1,s));
	
	for f=1:nsamp2
	aryfk(f,s)=sum(arytk(:,s).*exp(-i*tpad*omega(f)));
	end
		%step 5
		phi = zeros(nsamp2,1);
	% step 4
	for j = 1: nsamp
		%step 6
		omegalimit=0.5*abs(tzeta(s)*aryvel(j));
		ind = find(abs(omega) > omegalimit);
		temp = omega(ind).*sqrt(1-omegalimit^2./omega(ind).^2)*dt;
		%step 7
		phi(ind) = phi(ind)+temp';
		
		% step 8
		[aa,bb]=size(aryfk(ind,s));
		[cc,dd]=size(exp( phi(ind)*i));
		if( aa==0 | bb==0 | cc==0 | dd==0 )
			abcd = 0;
		else 
			abcd = aryfk(ind,s) .* exp( phi(ind) * i);
		end
		migfk(j,s)=sum( abcd );
		
		%step 9
	end
%step 10
end
%step 11
for j = 1:nsamp
	mig = ifft(migfk(j,:));
	%for ix = 1: ntr2
	%mig(ix)=sum(migfk(j,:).*exp(i*xpad(ix)*tzeta));
	%end
	arymig(j,:) = real(mig(1:ntr));
end
%disp(['Total floating operation --' int2str(flops)]);