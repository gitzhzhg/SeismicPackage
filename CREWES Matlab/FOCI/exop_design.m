function Wx=exop_design(dx,dz,nfor,ninv,v,f,ipct,dipnom,pow,nwin,iphase,nsteps)
%
% Wx=exop_design(dx,dz,nfor,ninv,v,f,ipct,dipnom,pow,nwin,iphase,nsteps)
%
% Design a 2D stable, explicit space-frequency domain extrapolator
% for depth migration. The algorithm is the FOCI
% (forward-operator-conjugate-inverse) method of Margrave (2004).
% 
% 
% dx ... lateral grid spacing
% dz ... depth step size
% nfor ... number of points in forward operator
%      ****** must be an odd integer ******
% ninv ... number of points in inverse operator
%      ****** must be an odd integer ******
% v ... velocity (scalar)
% f ... frequency in Hertz (scalar) 
% ipct ... max percentage of imaginary velocity
%    Should be between 0 and 100 
% dipnom ... nominal design dip. This has no effect if ipct=0
% pow ... raise the design spectrum to this power before designing the
%         inverse operator. Try something bewtween 0 and 1. .01 is good
% nwin ... window the final operator (with a hanning window) to this length
%         0 means no windowing, ninv+nfor-1 would give the nominal length
%         operator with hanning tapers.
% ********* default 0 *************
% iphase ... if 1, incorporate residual phase corrections
%         ************ default 0 ********
% nsteps ... if >0, then make analysis plots
%            The amplitude spectrum is raised to this power to test
%            stability.
%         ****** default 0 ******
%
%
% G.F. Margrave, CREWES/POTSI 2004
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

% Wiener filter approximation to extrapolator.
% idea: suppose we want a stable extrapolator that takes a step dz
%       First design an extrapolator, nfor pts long, using a least squares
%       approximation that takes a step dz/2. Call this W. Then design an 
%       ninv-pt least square inverse (Wiener filter) to W, called WI. 
%       The final extrapolator will be conv(conj(WI),W).
%       It will be nfor+ninv-1 pts long. This is the basic idea and there
%       are a number of wrinkles. Most important among these is that a 
%       very special, band-limited inverse is developed.
%

if(nargin<12)
    nsteps=0;
end
if(nargin<11)
    iphase=0;
end
if(nargin<10)
    nwin=0;
end
if(floor(nfor/2)*2==nfor)|(floor(ninv/2)*2==ninv)
     error('nfor and ninv must be odd integers')
end
if(floor(nwin/2)*2==nwin & nwin~=0)
     error('nwin must be an odd integer')
end

if(nwin>(nfor+ninv-1))
    disp('Warning: exop_design, nwin reset to nfor+ninv-1')
    nwin=nfor+ninv-1;
end

ipct=ipct/100;

mx=nfor+ninv-1;%number of x points in final operator. should be odd

nx=1024;%arbitrary, just to generate a greatly padded response
x=(0:nx-1)*dx;

%get a kx vector
kx=fftshift(freqfft(x));
kk=kx*v/f;%sin(theta)
kkm=max(abs(kk));

%design complex velocity. The velocity is entirly real for angles less than
%dipnom. Then, from dipnom to the nyquist wavenumber, it ramps up linearly
% (in wavenumber) to a maximum of ipct*v at nyquist.

thnom=pi*dipnom/180;
kknom=sin(thnom);%sin of nominal design dip
vr=v*ones(size(kx));
ind=find(abs(kk)>=kknom);
vi=zeros(size(kx));
vi(ind)=ipct*v*(abs(kk(ind))-kknom)/(kkm-kknom);
vc=vr+i*vi;
% vc=v*(1+ipct*i);

%build the ideal extrapolator
eta=2*pi*f*(dz/2)./vc;
kz=eta.*sqrt(1-(kx.*vc/f).^2);
truephase=zeros(size(kx));
ind=find(abs(kx)<f/v);
truephase(ind)=2*pi*dz*sqrt((f./v).^2-kx(ind).^2);
Wk=exp(i*real(kz)-abs(imag(kz)));
%ind=find(abs(kx)>=f/v);
%Wk(ind)=1;
Wk=[Wk(1:nx/2+1) fliplr(Wk(2:nx/2))];%force symmetry
Wfull=ifft(Wk);

%form W by unwrapping and truncation
W=[Wfull(end-ceil((nfor-1)/2)+1:end) Wfull(1:floor((nfor+1)/2)) ];
%lw=lamwin(floor(nfor-1)/2,1,4);
lw=ones(1,nfor);
%lw=hanning(nfor)';
W=W.*lw;
%need a zero padded version
mask=zeros(size(Wfull));
mask(end-floor((nfor-1)/2)+1:end)=lw(1:floor(nfor-1)/2);
mask(1:floor((nfor+1)/2))=lw(floor(nfor-1)/2+1:end);
Wpad=fftshift(mask.*Wfull);

%least squares approximate inverse
%we do this by matching W to an appropriate sinc
%in the centered position

%Generate the signal to be matched
Aopt=abs(Wk).^(pow);
if(iphase)
    residualphase=truephase-2*real(kz);
    Sopt=Aopt.*exp(-i*residualphase);
else
    Sopt=Aopt;
end
%matching signal
sigmatch=fftshift(ifft(Sopt)).';%a sinc-like function
sigmatch=pad_trace(sigmatch,1:ninv+nx-1,1);
%build convolution matrix
Wmtx=convmtx(Wpad(:),ninv);

%solve for WI by matrix division
WI=(pinv(Wmtx)*sigmatch).'; %force row vector

%form final operator
%Wx=conv(fliplr(conj(WI)),W);
%Wx=conv(conj(WI),W).*hanning(ninv+nfor-1)';
Wx=conv(conj(WI),W);
if(nwin)
    ndump=mx-nwin;
	%Wx=Wx(floor(ndump/2)+1:floor(ndump/2)+nwin).*(hamming(nwin).^(1))';
    Wx=Wx(floor(ndump/2)+1:floor(ndump/2)+nwin).*lamwin(floor(nwin/2),nwin-2*floor(nwin/2),4);
	mx=length(Wx);
end

if(nsteps)
    
    tit=['Wx=exop\_design(' int2str(dx) ',' int2str(dz) ',' int2str(nfor) ',' ...
            int2str(ninv) ',' int2str(v) ',' int2str(f) ',' int2str(100*ipct) ...
            ',' int2str(dipnom) ',' num2str(pow,2) ',' int2str(nwin) ','...
             int2str(iphase) ',' int2str(nsteps) ')' ];
    
	Wxk=fftshift(fft(fftshift(Wx)));
	kxx=freqfft(dx*(1:mx));
	
	%pad to length nx and transform
	Wxpad=pad_trace(Wx,x,1);
	Wxpadk=fft(fftshift(Wxpad));
	
	%plot
	figure;
	subplot(4,1,1)
    x2=dx*(0:mx-1)-dx*ceil((mx-1)/2);
	plot(x2,real(Wx),x2,imag(Wx))
    title(tit)
    legend('Wx real','Wx imag')
	subplot(4,1,2)
	plot(fftshift(kx),fftshift(abs(Wxpadk)),fftshift(kx),fftshift(abs(Wxpadk).^nsteps));
    legend('Amp spec (padded)',['Raised to power ',int2str(nsteps)])
	grid
    yl=get(gca,'ylim');
    yl(1)=0; if (yl(2)<1.5) yl(2)=1.5; end
    set(gca,'ylim',yl)
	subplot(4,1,3)
    opphase=fftshift(angle(Wxpadk));
	plot(fftshift(kx),fftshift(truephase),fftshift(kx),opphase,'r.');
    legend('True phase','Wx phase')
    ind=find(abs(kx)<=f/v);
    err=norm(truephase(ind)-opphase(ind))/length(ind);
    text(0,-.5,['rms phase err ' num2str(err,2)])
    yl=get(gca,'ylim');
    if(yl(1)>-1)
        set(gca,'ylim',[-1 yl(2)])
    end
	
	subplot(4,1,4)
	WIk=fftshift(fft(fftshift(WI)));
    if(ninv>1)
        kxi=freqfft(dx*(0:ninv-1));
    else
        kxi=0;
    end
	Wkk=fftshift(fft(fftshift(W)));
    kxf=freqfft(dx*(0:nfor-1));
	plot(kxf,abs(Wkk),kxi,abs(WIk));
    legend('Forward op','Inverse op')
end