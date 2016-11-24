function [phs,fphs,amp,famp,rwind] = fxtran(seis,x,t,tmin,tmax, pctaper,pctrand,fmax,ialign,xmin,xmax)
%
% [phs,fphs,amp,famp,rwind] = fxtran(seis,x,t,tmin,tmax, pctaper,pctrand,fmax,ialign,xmin,xmax)
%
% FXTRAN computes the f-x spectrum of an input seismic dataset and
% returns the "complex phase" and amplitude spectra.
%
% seis ... input matrix containing the seismic section
% x ... x (column) coordinate for seis
% t ... time (row) coordinate for seis
% tmin ... tmin(1) is beginning of time zone at x=xmin and tmin(2) is
%         beginning of time zone at x=xmax. If only one entry then tmin(2)=tmin(1)
%         is assumed
% tmax ... tmax(1) is end of time zone at x=xmin and tmax(2) is
%         end of time zone at x=xmax. If only one entry then tmax(2)=tmax(1)
%         is assumed
% pctaper ... percentage taper on each end of the dataset
% pctrand ... percentage size of a random fluctuation in widow length. It is
% 		taken as a percentage of the non-zero samples in the window
% fmax ... maximum frequency of interest
%    default = Nyquist
% ialign ... 0 do not time shift traces prior to transform.
%            1 time shift traces shuch that the center of each analysis window is 
%            aligned prior to transform.
%         default = 0 *******NOT IMPLEMENTED YET******* only option 0 works
% NOTE: for sloping windows this will cause better phase alignments is events also slope
% xmin .. minimum x coordinate to analyze. default is min(x)
% xmax .. maximum x coordinate to analyze. default is max(x)
% phs ... complex phase matrix
% fphs ... frequency (row) coordinate for phs
% amp ... amplitude spectrum matirx
% famp ... frequency (row) coordinate for amp
% rwind ... matrix of random windows used to window seis.
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

%

dt=t(2)-t(1);

if(nargin < 8)
	fmax=1/(2*dt);
end

if(nargin<9)
	ialign=0;
end

if(nargin<10)
	xmin=min(x);
end
if(nargin<11)
	xmax=max(x);
end

if(length(tmin)==1)
	tmin=[tmin tmin];
end
if(length(tmax)==1)
	tmax=[tmax tmax];
end

tminmin=min(tmin);
tmaxmax=max(tmax);

%window the seismic
iz=near(t,tminmin,tmaxmax);
ix=near(x,xmin,xmax);
swin=seis(iz,ix);
t=t(iz);

seis=[];
[nsamp,ntr]=size(swin);

%determine window center and bounds

tmn = tmin(1) + (tmin(2)-tmin(1))*(x(ix)-xmin)/(xmax-xmin);
tmx = tmax(1) + (tmax(2)-tmax(1))*(x(ix)-xmin)/(xmax-xmin);
tc= (tmn+tmx)/2;%center time
ic= round((tc-tminmin)/dt) + 1;%center index

if(ialign)
% ********* $$$$$$$$$ not implemented yet $$$$$$$$$ *********
% use slicemat.m
end

%determine the nominal zero pad
n2=2^nextpow2(nsamp);
zpad = zeros(n2-nsamp,1);
tz = xcoord(t(1),dt,n2);

%generate random window fluctuations
fracfluct = pctrand*rand(1,ntr)/100;

%loop over traces
for k=1:ntr

		tfluct=fracfluct(k)*(tmx(k)-tmn(k));
		%generate window
		mw=mwindow( round((tmx(k)-tmn(k)-tfluct)/dt+1), pctaper)';

		nlive=length(mw);
		ilive1 = ic(k)-round(nlive/2)+1;
		ilive1=max([ilive1 1]);
		ilive2 = ilive1 + nlive -1;
		ilive2=min([ilive2 length(iz)]);
		if(ilive2-ilive1+1~=length(mw))
			mw=mwindow(ilive2-ilive1+1,pctaper)';
			nlive=length(mw);
		end
		
		ss=zeros(nsamp,1);
		ss(ilive1:ilive2)=swin(ilive1:ilive2,k).*mw;

		s = [ss; zpad];

		[S,f]=fftrl(s,tz);

		%skip zero traces
		if(sum(abs(ss))>10000*eps)
			if(nlive>0)
				% normalize for number of live samples
				a=abs(S)*nsamp/nlive;
				p=S./a;
			else
				a=zeros(size(real(S)));
				p=a+i*a;
			end
		else
			a=zeros(size(real(S)));
			p=a+i*a;
		end
		


		if(k==1)
			
			if(nargout>1)
				indf=near(f,0,fmax);
				famp=f(indf);
				amp=zeros(length(famp),ntr);
				if(nargout==5)
					rwind=zeros(nsamp,ntr);
				end
			end
			np=2*length(famp);
			phs=zeros(np,ntr);
			fphs=xcoord(f(1),(f(2)-f(1))/2,np);
 			indp=near(fphs,0,fmax);
		end

		if(nargout > 1)
			amp(:,k) = a(indf);
			if(nargout==5)
					rwind(ilive1:ilive2,k) = mw;
			end
		end
		
		phs(1:2:np,k) = real(p(indf));
		phs(2:2:np,k) = imag(p(indf));
		
		if(rem(k,50)==0)
			disp([' finished trace ' int2str(k) ' of ' int2str(ntr)])
		end

	end

 swin=[];
 %f2=xcoord(f(1),(f(2)-f(1))/2,np);
 %ind=near(f2,0,fmax);
 %fphs=f2(ind);
 %phs=phs(ind,:);
 
 %ind=near(f,0,fmax);
 %famp=f(ind);
 %amp = amp(ind,:);