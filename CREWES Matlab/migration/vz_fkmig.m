function [arymig,tmig,xmig]=vz_fkmig(aryin,t,x,vel,params)
% VZ_FKMIG: fk migration for v(z)
% [arymig,tmig,xmig]=vz_fkmig(aryin,t,x,vel,params)
%
% VZ_FKMIG performs post stack migration in the frequency wavenumber domain
% using the time migration algorithm of Margrave, G. F., 2001, Direct
% Fourier migration for vertical velocity variations, Geophysics, 66,
% 1504-1514.
% 
% aryin ... matrix of zero offset data. One trace per column.
% t ... if a scalar, this is the time sample rate in SECONDS.
%		If a vector, it gives the time coordinates for the rows of 
%		aryin.
% x ... if a scalar, this is the spatial sample rate (in units 
%		consistent with the velocity information. If a vector, then
%		it gives the x coordinates of the columns of aryin
% vel ... a vector giving the interval velocities for each time. Length of
%		vel must equal row dimension of aryin. See params(9)
%   params ... vector of migration parameters
%	params(1) ... maximum frequency (Hz) to migrate
%       ***** default = .6*fnyquist *********
%	params(2) ... width of cosine taper (Hz) to apply above params(1)
%       ***** default = .2*(fnyquist-params(1)) ********
%	params(3) ... maximum dip (degrees) to migrate
%       ***** default = 80 degrees *****
%	params(4) ... with of cosine dip taper (degrees)
%       ***** default = 90-params(3) degrees *****
%	params(5) ... size of zero pad in time (seconds)
%       ***** default = min([.5*tmax, tmax/cos(params(3))]) ******
%   params(6) ... size of zero pad in space (length units)
%       ***** default = min([.5*xmax, xmax*sin(params(3))]) ******
%   params(7) ... if 1, zero pads are removed, if 0 they are retained
%       ***** default = 1 *****
%   params(8) ... if 1, use full Fourier algorithm. If 0, use mixed domain
%                 NOT IMPLEMENTED - NO EFFECT
%       ******* default = 1 *******
%	params(9) ... if 1, velocities are rms velocities, use rms algorithm
%			  	  if 2, velocities are interval velocities, use WKBJ algorithm
%       ******* default = 2 *******
%	params(10) ... ray parameter sampling flag for WKBJ algorithm
%				   The phase shift table is built for a number of ray parameters equal
%				   to params(10)*length(t_with_pad)
%		******* default = .5 *********
%	params(11) ... not used
%	params(12) ... if 1 use faster fk transforms
%	               if 0, use slower, memory conserving, transforms
%	    ******* default = 0 ******
%	params(13) ... =n means print a message as every n'th wavenumber 
%			is migrated.
%	    ******* default = 50 ******
% arymig ... the output migrated time section
% tmig ... t coordinates of migrated data
% xmig ... x coordinates of migrated data
%
% G.F. Margrave CREWES Project, U of Calgary, 1998
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

disp('vz_fkmig')
tstart=clock; % save start time

[nsamp,ntr]=size(aryin);

if(length(vel)~=nsamp)
	error('number of velocities must equal number of time samples')
end

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

fnyq=1/(2*dt);
knyq=1/(2*dx);
tmax=t(nsamp);
xmax=abs(x(ntr)-x(1));

%examine parameters
nparams=13;% number of defined parameters
if(nargin<5) params= nan*ones(1,nparams); end
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
if( isnan(params(4)) ) dipwid = 90-dipmax;
else dipwid = params(4);
		if((dipwid+dipmax)>90) dipwid= 90-dipmax; end
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
if( isnan(params(8)) ) algflag= 1;
else algflag = params(8);
end
if( isnan(params(9)) ) velflag= 2;
else velflag = params(9);
end
if( isnan(params(10)) ) pnum= .5;
else pnum = params(10);
end
if( isnan(params(11)) ) ntable= 25;
else ntable = params(11);
end
if( isnan(params(12)) ) mcflag= 0;
else mcflag = params(12);
end
if( isnan(params(13)) ) kpflag= 50;
else kpflag = params(13);
end

if(velflag ==1)
	disp('rms velocity algorithm used')
elseif(velflag ==2)
	disp('interval velocity algorithm (WKBJ) used')
else
	error('invalid velocity flag (params(9))')
end

if(algflag~=1)
	error(' MUST use full Fourier algorithm, others not implemented')
end

%apply pads

%tpad
nsampnew = round((tmax+tpad)/dt+1);
nsampnew = 2^nextpow2(nsampnew);
tmaxnew = (nsampnew-1)*dt;
tnew = (t(1):dt:tmaxnew)';%must be a column vector
ntnew=length(tnew);
ntpad = nsampnew-nsamp;
aryin = [aryin;zeros(ntpad,ntr)];


%xpad
ntrnew = round((xmax+xpad)/dx+1);
ntrnew = 2^nextpow2(ntrnew);
xmaxnew = (ntrnew-1)*dx+x(1);
xnew = x(1):dx:xmaxnew;
nxpad = ntrnew-ntr;
aryin = [aryin zeros(nsampnew,nxpad)];

disp([' tpad = ' int2str(ntpad) ' samples']);
disp([' xpad = ' int2str(nxpad) ' traces']);

%forward f-k transform
disp('forward f-k transform')
if(mcflag)
	[fkspec,f,kx] = fktran(aryin,tnew,xnew,nsampnew,ntrnew,0,0);
else
	[fkspec,f,kx] = fktran_mc(aryin,tnew,xnew,nsampnew,ntrnew,0,0);
end
f=f(:)'; %make sure f is a row vector
clear aryin; %free space
df=f(2)-f(1);
nf=length(f);

%compute frequency mask
nfmax = round((fmax+fwid)/df+1);
pct = 100*(fwid/(fmax+fwid));
fmask = mwhalf(nfmax,pct);
fmaxmig = (nfmax-1)*df; %i.e. fmax+fwid to nearest sample
disp(['frequencies greater than ' num2str(fmaxmig) ' not migrated'])

%precompute things before main loop
vel = vel(:); % force a column vector
%pad velocities with last value
if(ntpad~=0)
	vel=[vel;vel(length(vel))*ones(ntpad,1)];
end

ve = vel/2; %exploding reflector velocity
ve2 = ve.^2; %exploding reflector velocity squared

ind=find(f==0.0);
f(ind)=df/100;% guard against zero divide;
if2= f.^(-2);% inverse frequency squared
if(velflag==2)
	iw=i*2*pi*(f'); %angular frequency times i
	if1= (1 ./f)'; %inverse frequency
else
	iw=i*2*pi*f; %angular frequency times i
end

%tf= tnew*f;
%vf2= ve2*if2;

th1 = dipmax*pi/180;
th2 = (dipmax+dipwid)*pi/180;
zip = zeros(size(f));

%build phase shift table
np=pnum*length(tnew); %maybe set this equal to number of time samples?
pmin=0; pmax=1/min(ve);
dp=(pmax-pmin)/np;
idp=1/dp;
p=pmin:dp:pmax+dp;
phstable = dt*sqrt(1- (ve.^2)*(p.^2));%note that we just let it go imaginary
%integrate the table
% plabels the columns and t labels the rows of table
% integrate over time
phstable=cumsum(phstable).';


%now loop over wavenumbers
disp([int2str(length(kx)) ' wavenumbers to migrate']);

%operator is symmetric in kx so we only loop over half of kx
avetime=0.0;
tbuild=0;
tfft=0.;
tapply=0.;
ttotal=0.;
%mf=flops;

nj=length(kx)/2+1;
timeskx=zeros(1,nj);
for j=1:nj
%for j=nj-10:nj
%for j=round(nj/4)
	t1loop=clock;
	%evanescent cutoff
	%fmin = abs(kx(j))*ve;
	%ifmin = ceil(fmin/df+1);
	
	%compute dip mask
%	if(th1~=th2)
%		ifbeg=max([ifmin 2]);%first physical frequency excluding dc
%		ifuse = ifbeg:ifmaxmig; %frequencies to migrate
%		theta=asin(fmin./f(ifuse)); % physical dips for each frequency
%		if1 = round(fmin/(sin(th1)*df))+1; % sample number to begin ramp
%		if1=max([if1 ifbeg]); 
%		if2 = round(fmin/(sin(th2)*df))+1; % sample number to end ramp
%		if2=max([if2 ifbeg]);
%		dipmask=zip; % initialize mask to zeros
%		dipmask(if1:nf)=ones(size(if1:nf)); % pass these dips
%		dipmask(if2:if1) = .5+.5*...
%			cos((theta((if2:if1)-ifbeg+1)-th1)*pi/(th2-th1));
%	else
%		dipmask=ones(size(f));
%	end
	
	% apply frequency mask
	%tmp=fkspec(:,j).*fmask;

	%compute f's which map to kz
	%fmap = ve*sqrt(kx(j)^2 + kz2);
	% fmap contains one value for each kz giving the frequency
	% which must map there to migrate the data. Of course many
	% of these frequencies will be far too high.
	%ind=find(fmap<=fmaxmig);
	%
	% ind is a vector of indicies into fmap which gives will
	% always start at 1 and end at the highest f which is to be 
	% migrated
	
		
	% build the migration filter. t is the row coordinate (i.e. a
	% column vector) while w is the column coordinate.
	% 
	% mfilt = exp(i*2*pi*tf.*sqrt(1-(kx(j)^2)*vf2));
	% the above line computes the migration filter in one step but is
	% memory intensive and only marginally faster
	
	if(algflag==1)
	
		%ttemp=clock;
		% we form the mfilt  with f running down the columns because it
		% is more efficient to load a matrix by columns. Later it gets transposed
		% for the fft.
		mfilt=zeros(nf,ntnew);
		if(velflag==1)
			for kt=1:ntnew
				fev = abs(kx(j)*ve(kt)); % evanescent boundary
				iev = min([ceil(fev/df+1) nf+1]);
				mfilt(:,kt) = [zeros(1,iev-1) ...
					exp(tnew(kt)*iw(iev:nf).*sqrt(1-if2(iev:nf)*ve2(kt)*(kx(j)^2)))].';
			end
		elseif(velflag==2)
			for kt=1:ntnew
				%determine indexing into the phase table
				ifuse=(ceil(kx(j)/(pmax*df)+1):nf)';%find nonevanescent frequency indicies
				ipf= kx(j)*if1(ifuse)*idp+1;%sample numbers of pvalues for the f's at this kx
				ipf1=floor(ipf);
				irem=ipf-ipf1;
				%Interpolate the phase table
				phs= phstable(ipf1,kt).*(1-irem) + (irem).*phstable(ipf1+1,kt);
				%compute the migration filter
				mfilt(ifuse,kt) = exp(iw(ifuse).*phs);
			end
		end
		%tbuild=tbuild+etime(clock,ttemp);
		
		% move the migration filter to the Fourier domain
		%ttemp=clock;
		mfilt= fft(mfilt.');
		%tfft=tfft+etime(clock,ttemp);
		
		%apply it
		%ttemp=clock;
		%apply to positive wavenumbers
		fkspec(:,j)= [(mfilt(1:nfmax,1:nfmax)*fkspec(1:nfmax,j)).*fmask; zeros(nf-nfmax,1)];
		if((j>1) & (j<nj))
			%apply to negative wavenumbers
			fkspec(:,2*nj-j)= [(mfilt(1:nfmax,1:nfmax)*fkspec(1:nfmax,2*nj-j)).*fmask; zeros(nf-nfmax,1)];
		end
		%tapply=tapply+etime(clock,ttemp);
	
	elseif(algflag ==0 )
		tmp=fkspec(:,j);
		for kt=1:ntnew
			fkspec(:,j) = exp(i*2*pi*tnew(kt)*f.*sqrt(1-if2*ve2(kt)*(kx(j)^2)))*tmp;
		end
	else
		error(' invalid algorithm flag (params(8))')
	end
	
	t2loop=clock;
	etimeloop=etime(t2loop,t1loop);
	timeskx(j)=etimeloop;
	ttotal=ttotal+etimeloop;
	avetime= ((j-1)*avetime+etimeloop)/j;
	if( floor(j/kpflag)*kpflag == j)
		disp(['finished wavenumber ' int2str(2*j-1) ' kx = ' num2str(kx(j))]);
		disp(['loop time ' num2str(etimeloop) ' time remaining '... 
			num2str((nj-j)*avetime)]);
	end
end

%time summary
disp([' total compute time ' num2str(ttotal)])
%mf=flops-mf;
%disp(['total flops ' num2str(mf)])
%disp([' total build time ' num2str(tbuild)])
%disp([' total fft time ' num2str(tfft)])
%disp([' total apply time ' num2str(tapply)])

%inverse transform
if(algflag==1)
	disp('inverse f-k transform')
	clear mfilt
	if(mcflag)
		[arymig,tmig,xmig]=ifktran(fkspec,f,kx);
	else
		[arymig,tmig,xmig]=ifktran_mc(fkspec,f,kx);
	end
elseif(algflag==0)
	%only inverse kx transform required
end

%remove pad if desired
if(padflag)
	arymig=arymig(1:nsamp,1:ntr);
	tmig=tmig(1:nsamp);
	xmig=xmig(1:ntr);
end
	
tend=etime(clock,tstart);
disp(['Total elapsed time ' num2str(tend)])

% figure;
% plot(kx(1:nj),timeskx);xlabel('wavenumber');ylabel('compute time')