function [seismig,zmig,xmig]=psnsps_mig(aryin,t,x,velmod,dz,zmax,params)
% PSNSPS_MIG: Exploding reflector depth migration by NSPS
%
% [seismig,zmig,xmig]=psnsps_mig(aryin,t,x,velmod,dz,zmax,params)
%
% psnsps performs a zero offset wavefield extrapolation using
% 	nonstationary filters. One depth step is taken through an 
%	arbitrarily laterally variable velocity field. The algorithm is
%   convolutional nonstationary phase shift implemented in the mixed domain.
%	The direction of extrapolation can be either up or down. Ecanescent
%	energy is correctly handled as a decaying real exponential. The depth
%	step can be x variant.
% 
% aryin ... matrix of zero offset data. One trace per column.
% velmod ... velocity model, same number of columns as aryin
% t ... the time coordinates for the rows of aryin.
% x ... the x coordinates of the columns of aryin
% dz ... extrapolation step size, either a scalar or a vector the same size
%	as x.
% zmax ... maximum depth to extrapolate to
% params ... vector of extrapolation parameters
%	params(1) ... maximum frequency (Hz) to extrapolate
%       ***** default = .6*fnyquist *********
%	params(2) ... width of cosine taper (Hz) to apply above params(1)
%       ***** default = .2*(fnyquist-params(1)) ********
%	params(3) ... maximum dip (degrees) to extrapolate
%       ***** default = 80 degrees *****
%	params(4) ... butterworth order of dip filter
%       ***** default = 12 *****
%	params(5) ... size of zero pad in time (seconds)
%       ***** default = min([.5*tmax, tmax/cos(params(3))]) ******
%   params(6) ... size of zero pad in space (length units)
%       ***** default = min([.5*xmax, xmax*sin(params(3))]) ******
%   params(7) ... if 1, zero pads are removed, if 0 they are retained
%       ***** default = 1 *****
%   params(8) ...  percentage of imaginary velocity to use
%       ***** default = 1.0 (percent) ********
%	params(9) ... Not used
%	params(10) ... Not used
%	params(11) ... Not used
%	params(12) ... if 1 use faster fk transforms
%	               if 0, use slower, memory conserving, transforms
%	    ******* default = 0 ******
%	params(13) ... =n means print a message as every n'th frequency 
%			is extrapolated.
%	    ******* default = 50 ******
% aryex ... the output extrapolated time section
% tex ... t coordinates of extrapolated data
% xex ... x coordinates of extrapolated data
%
% G.F. Margrave, CREWES Project, U of Calgary, 1996
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
 

tstart=clock; % save start time
%totflops=flops;

[nsamp,ntr]=size(aryin);

if(length(t)>1)
	if(length(t)~=nsamp)
		error('Incorrect time specification')
	end
	dt=t(2)-t(1);
else
	dt=t;
	t=((0:nsamp-1)*dt)';
end

if(length(x)>1)
	if(length(x)~=ntr)
		error('Incorrect x specification')
	end
	dx=x(2)-x(1);
else
	dx=x;
	x=(0:ntr-1)*dx;
end
if(length(dz)>1)
	if(length(dz)~=ntr)
		error('Incorrect dz specification')
	end
end

fnyq=1/(2*dt);
knyq=1/(2*dx);
tmax=t(nsamp);
xmax=abs(x(ntr)-x(1));

%examine parameters
nparams=13;% number of defined parameters
if(nargin<7) params= nan*ones(1,nparams); end
if(length(params)<nparams) 
		params = [params nan*ones(1,nparams-length(params))];
end

%assign parameter defaults
if( isnan(params(1)) ) fmax= .6*fnyq; 
else fmax = params(1); if(fmax>fnyq) fmax=fnyq; end
end
if( isnan(params(2)) ) fwid = .2*(fnyq-fmax);
else fwid = params(2);
		if( (fmax+fwid)>fnyq ) fwid = fnyq-fmax; end
end
if( isnan(params(3)) ) dipmax = 85;
else dipmax = params(3); if(dipmax>90) dipmax=90; end
end
if( isnan(params(4)) ) order = 12;
else order = params(4);
end
if( isnan(params(5)) ) tpad= min([.5*tmax abs(tmax/cos(pi*dipmax/180))]);
else tpad = params(5);
end
if( isnan(params(6)) ) xpad= min([.5*xmax xmax*sin(pi*dipmax/180)]);
else xpad = params(6);
end
if( isnan(params(7)) ) padflag= 1;
else padflag = params(7);
end
if( isnan(params(8)) ) ivel= 1;
else ivel = params(8);
end
if( isnan(params(9)) ) cosflag= 1;
else cosflag = params(9);
end
if( isnan(params(10)) ) lsinc= 8;
else lsinc = params(10);
end
if( isnan(params(11)) ) ntable= 25;
else ntable = params(11);
end
if( isnan(params(12)) ) mcflag= 1;
else mcflag = params(12);
end
if( isnan(params(13)) ) fpflag= 10;
else fpflag = params(13);
end

%apply pads

%tpad
nsampnew = round((tmax+tpad)/dt+1);
nsampnew = 2^nextpow2(nsampnew);
tmaxnew = (nsampnew-1)*dt;
tnew = t(1):dt:tmaxnew;
ntpad = nsampnew-nsamp;
aryin = [aryin;zeros(ntpad,ntr)];

%xpad
ntrnew = round((xmax+xpad)/dx+1);
ntrnew = 2^nextpow2(ntrnew);
xmaxnew = (ntrnew-1)*dx+x(1);
xnew = x(1):dx:xmaxnew;
nxpad = ntrnew-ntr;
aryin = [aryin zeros(nsampnew,nxpad)];
velmod = [velmod velmod(:,length(x))*ones(1,nxpad)]; %pad velocities

disp([' tpad = ' int2str(ntpad) ' samples']);
disp([' xpad = ' int2str(nxpad) ' traces']);

%forward f transform
disp('forward f transform')

[spec,f]= fftrl(aryin, tnew);

clear aryin; %free space
df=f(2)-f(1);
nf=length(f);

%compute frequency mask
ifmaxex = round((fmax+fwid)/df+1);%determine maximum frequency to extrapolate
pct = 100*(fwid/(fmax+fwid));
fmask = [mwhalf(ifmaxex,pct); zeros(nf-ifmaxex,1)];
fmaxex = (ifmaxex-1)*df; %i.e. fmax+fwid to nearest sample

%compute wavenumber vector
kxnyq = 1./(2.*(xnew(2)-xnew(1)));
dkx = 2.*kxnyq/ntrnew;
kx=2*pi*[-kxnyq:dkx:-dkx 0:dkx:kxnyq-dkx ]';
dkx=kx(2)-kx(1);
kxnyq=-kx(1);

eikxx = exp(i*kx*xnew);
ik0=length(kx)/2 + 1;
kxh=kx(1:ik0); %first half of kx
kxh2= kxh.*kxh;
kxh2mat=kxh2(:,ones(size(xnew))); % expand to matrix



dzmat=[];
if(length(dz)>1)
	error('topographic stuff not allowed');
	dz=dz(:)';
	dzmat=dz(ones(size(kx)),:);
end

%loop over depth steps
nz=floor(zmax/dz+1);

%allocate seismig
seismig=zeros(nz,length(kx));
zmig=zeros(nz,1);

seismig(1,:)=sum(2*real(spec(1:ifmaxex,:)));

disp([int2str(nz) ' depth steps to take'])
disp([int2str(ifmaxex) ' frequencies to extrapolate']);

time0=clock;

for iz=2:nz
	%extrapolate to the next depth
	zmig(iz)=zmig(iz-1)+dz;
	
	%determine v
	v=.5*velmod(iz,:);

	%include a small imaginary component
	%v=(1+i*ivel/100)*v;
	iv2= v.^(-2);
	iv2mat= iv2(ones(size(kxh)),:);% expand to matrix
	


%
% the extrapolation matrix for nsps operates of a frequency slice of the
% (x,f) spectrum and both extrapolates and forward transforms (x to kx)
% simultaneously. If the frequency slice is considered as a column vector
% then the extrapolator must have x as the column coordinate and kx as 
% the row coordinate. This operator is nearly the transpose of that 
% required for pspi. The "nearly" stems from the requirement that the
% sign on i*kx*x has to flip to allow pspi to do an inverse transform 
% while nsps does a forward transform. 
%

	

	%jj=near(f,25);

	%now loop over frequencies
	for j=1:ifmaxex
		tmp= spec(j,:).';
		%extrapolate
		w=2*pi*f(j);
		w2=w*w;
		%build phase shift operator

		psop=dz*sqrt( w2*iv2mat - kxh2mat );
		%determine wholly evanescent wavenumbers
		kxlim=w/min(v);
		ik1=max([floor((kxnyq-kxlim)/dkx)+1,1]);
		ik2=min([length(kx)-ik1+2 length(kx)]);
		%ik2=min([floor((kxnyq+kxlim)/dkx)+1,length(kx)]);

		eop=exp(i*real(psop(ik1:ik0,:))-abs(imag(psop(ik1:ik0,:))));

		%dip filter prep
		%ind=find(v~=0.0);
		%big=1.e12;
		%px = big*ones(size(v));
		%px(ind) = sin(pi*dipmax/180)./v(ind);
		%px=px(:)';
		%build dip filter
		%if(j==1|dipmax==90)
			%dipfilt=fmask(j)*ones(size(psop));
		%else
			%ikxnot = 1 ./(w*px);
			%dipfilt = fmask(j)*(1+(kx*ikxnot).^order).^(-1);
		%end
		%extrapolate and forward kx transform
		%tmp2 = (dipfilt(ik1:ik2,:).*exp(i*real(psop(ik1:ik2,:)) ...
		tmp2 = ([eop;flipud(eop(2:ik2-ik0+1,:))].*eikxx(ik1:ik2,:))*tmp;
		%tmp2 = ([eop;eop(ik0-1:-1:ik1,:)].*eikxx(ik1:ik2,:))*tmp;

		tmp = [zeros(1,ik1-1) tmp2.' zeros(1,length(kx)-ik2)]/length(kx);

		% inverse transform over kx
		spec(j,:) = fft(fftshift(tmp));
	
		if( floor(j/fpflag)*fpflag == j)
			disp(['finished frequency ' int2str(j)]);
		end
	end

	%normalize
	%spec=spec/length(kx);

	%inverse transform over kx
	%spec=fft(fftshift(spec.')).';

	%image the current depth
	seismig(iz,:)=sum(2*real(spec(1:ifmaxex,:)));

	%rezero the x pad
	spec(:,ntrnew-nxpad+1:ntrnew)=0;

	disp(['completed depth step ' int2str(iz) ' of ' int2str(nz)])
	timenow=clock;
	timeused=etime(timenow,time0);
	timeleft=timeused*nz/iz;
	disp(['elapsed time ' num2str(timeused) ' sec. Time left ' ...
		num2str(timeleft) ' sec.']);
	
end


%remove pad
seismig=seismig(:,1:ntr);
	
xmig=x;
	
tend=etime(clock,tstart);
disp(['Total elapsed time ' num2str(tend)])
%totflops= flops-totflops;
%disp(['Total floating point operations ' num2str(totflops)])