function [arymig,tmig,xmig]=fkmig(aryin,v,t,x,params)
%
% [arymig,tmig,xmig]=fkmig(aryin,v,t,x,params)
%
% FKMIG performs post stack migration in the frequency wavenumber domain
% using the constant velocity method of Stolt (Geophysics 1978)
% 
% aryin ... matrix of zero offset data. One trace per column.
% v ... a scalar giving the constant velocity to migrate with
% t ... if a scalar, this is the time sample rate in SECONDS.
%		If a vector, it gives the time coordinates for the rows of 
%		aryin.
% x ... if a scalar, this is the spatial sample rate (in units 
%		consistent with the velocity information. If a vector, then
%		it gives the x coordinates of the columns of aryin
% params ... vector of migration parameters
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
%   params(8) ... if 0, use nearest neighbor interpolation
%                 if 1, use sinc function
%                 if 2, use spline function
%                 if 3, use linear interpolation
%       ***** default = 1 *****
%	params(9) ... if 1, apply cos(theta) wt factor.
%                 if 0, don't apply it
%       ******* default = 1 *******
%	params(10) ... number of points in sinc function interpolator. Must be
%	               an even number. 
%       ******* default = 8 ******
%	params(11) ... number of points in sinc function interpolator. Must be
%	               an even number. 
%       ******* default = 8 ******
% arymig ... the output migrated time section
% tmig ... t coordinates of migrated data
% xmig ... x coordinates of migrated data
%
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

% G.F. Margrave and J. Bancroft, CREWES Project, U of Calgary, 1996
tstart=clock; % save start time
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
fnyq=1/(2*dt);
knyq=1/(2*dx);
tmax=t(nsamp);
xmax=abs(x(ntr)-x(1));
%examine parameters
nparams=11;% number of defined parameters
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
if( isnan(params(8)) ) intflag= 1;
else intflag = params(8);
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
xmaxnew = (ntrnew-1)*dx;
xnew = x(1):dx:xmaxnew;
nxpad = ntrnew-ntr;
aryin = [aryin zeros(nsampnew,nxpad)];
disp([' tpad = ' int2str(ntpad) ' samples']);
disp([' xpad = ' int2str(nxpad) ' traces']);
%forward f-k transform
disp('forward f-k transform')
[fkspec,f,kx] = fktran(aryin,tnew,xnew,nsampnew,ntrnew,0,0);
clear aryin; %free space
df=f(2)-f(1);
nf=length(f);
%compute frequency mask
ifmaxmig = round((fmax+fwid)/df+1);
pct = 100*(fwid/(fmax+fwid));
fmask = [mwhalf(ifmaxmig,pct)'; zeros(nf-ifmaxmig,1)];
fmaxmig = (ifmaxmig-1)*df; %i.e. fmax+fwid to nearest sample
%now loop over wavenumbers
ve = v/2; %exploding reflector velocity
dkz= df/ve;
kz = ((0:length(f)-1)*dkz)';
kz2=kz.^2;
th1= dipmax*pi/180;
th2= (dipmax+dipwid)*pi/180;
zip= zeros(size(f));
disp([int2str(length(kx)) ' wavenumbers to migrate']);
for j=1:length(kx)
	%evanescent cutoff
	fmin = abs(kx(j))*ve;
	ifmin = ceil(fmin/df+1);
	
	%compute dip mask
	if(th1~=th2)
		ifbeg=max([ifmin 2]);%first physical frequency excluding dc
		ifuse = ifbeg:ifmaxmig; %frequencies to migrate
		theta=asin(fmin./f(ifuse)); % physical dips for each frequency
		if1 = round(fmin/(sin(th1)*df))+1; % sample number to begin ramp
		if1=max([if1 ifbeg]);
		if2 = round(fmin/(sin(th2)*df))+1; % sample number to end ramp
		if2=max([if2 ifbeg]);
		dipmask=zip; % initialize mask to zeros
		dipmask(if1:nf)=ones(size(if1:nf)); % pass these dips
		dipmask(if2:if1) = .5+.5*...
			cos((theta((if2:if1)-ifbeg+1)-th1)*pi/(th2-th1));
	else
		dipmask=ones(size(f));
	end
	
	% apply masks
	tmp=fkspec(:,j).*fmask.*dipmask;
	%compute f's which map to kz
	fmap = ve*sqrt(kx(j)^2 + kz2);
	% fmap contains one value for each kz giving the frequency
	% which must map there to migrate the data. Of course many
	% of these frequencies will be far too high.
	ind=find(fmap<=fmaxmig);
	%
	% ind is a vector of indicies into fmap which gives will
	% always start at 1 and end at the highest f which is to be 
	% migrated
	
	%now map samples by interpolation
	fkspec(:,j) = zip; %initialize output spectrum to zero
	if( ~isempty(ind) )
		%compute cosine scale factor
		if(cosflag)
			if( fmap(ind(1))==0)
				scl=ones(size(ind));
				li=length(ind);
				scl(2:li)=ve*kz(ind(2:li))./fmap(ind(2:li));
			else
				scl=ve*kz(ind)./fmap(ind);
			end
		else
			scl=ones(size(ind));
		end
		if(intflag==1)
			%complex sinc interpolation
			fkspec(ind,j) = scl.*csinci(tmp,f,fmap(ind),[lsinc,ntable]);
		elseif(intflag==0)
			% nearest neighbor interpolation
			ifmap = (fmap(ind)/df+1);
			fkspec(ind,j) = scl.*tmp(ifmap);
		elseif(intflag==2)
			% spline interpolation
			ifmap = (fmap(ind)/df+1);
			fkspec(ind,j) = scl.*interp1(f,tmp,fmap(ind),'spline');
		elseif(intflag==3)
			% complex linear interpolation
			fkspec(ind,j)=scl.*clinint(f,tmp,fmap(ind));
		end
	end
	if( floor(j/50)*50 == j)
		disp(['finished wavenumber ' int2str(j)]);
	end
end
%inverse transform
disp('inverse f-k transform')
[arymig,tmig,xmig]=ifktran(fkspec,f,kx);
%remove pad if desired
if(padflag)
	arymig=arymig(1:nsamp,1:ntr);
	tmig=tmig(1:nsamp);
	xmig=xmig(1:ntr);
end
	
tend=etime(clock,tstart);
disp(['Total elapsed time ' num2str(tend)])