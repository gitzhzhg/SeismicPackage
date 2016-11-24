function trout=tvsw(trin,t,aec_length,flow,fhigh,nfilt,picture)
% TVSW ... time-variant spectral whitening
%
% trout=tvsw(trin,t,aec_length,flow,fhigh,nfilt,picture)
% 
% TVSW does time variant spectral whitening. A set of gaussian filters are
% designed to slice a frequency range in such a way that the linear sum of
% slices returns the original spectrum. Each slice is then ifft'd into the
% time domain and agc'd. Finally, the slices are summed in the time domain.
%
% trin ... input trace 
% t    ... time coordinate vector for trin
% aec_length ... length (seconds) of the aec operator (see aec.m, this is a
%       form of agc)
% flow ... lowest frequency to be whitened (Hz)
% fhigh ... highest frequency to be whitened (Hz)
% nfilt ... number of Gaussian filter slices
% NOTE: The Gaussians will be centered at the frequencies
%   f0=flow+(k-1)*fwidth where fwidth=(fhigh-flow)/(nfilt-1) and have
%   standard deviation of fwidth. This means that there will be some
%   whitening outside of flow->fhigh.
% picture ... if 1, the a picture showing how the method works will be
%   created in a new figure window using the input trace. If 0, then no
%   picture.
% *********** default = 0 ***********
% trout= output trace
%
% by G.F. Margrave, May 1991
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

%check for zero trace
if(sum(abs(trin))==0)
    trout=zeros(size(trin));
    return;
end
if(nargin<7)
    picture=0;
end

%forward fft
m=length(trin);
trouttmp=zeros(size(trin));
trin=padpow2(trin);
t2=(0:length(trin)-1)*(t(2)-t(1))+t(1);
[spec,f]=fftrl(trin,t2);
% loop over slices
fwidth=(fhigh-flow)/(nfilt-1);
slice=zeros(length(trin),nfilt+1);
slice(:,1)=trin(:);
slicea=slice;
names=cell(1,nfilt+1);
for n=1:nfilt
    g=gauss(f,flow+(n-1)*fwidth,fwidth); % make gaussian
    Slice=spec.*g; % make slice
    slice(:,n+1)=ifftrl(Slice,f); % inverse transform
    slicea(:,n+1)=aec(slice(:,n+1),t2,aec_length); % aec
    trouttmp=trouttmp+slicea(1:m,n+1); % accumulate slices
    if(picture==1)
        names{n+1}=['Filter slice at ' num2str(sigfig(flow+(n-1)*fwidth,2)) 'Hz'];
    end
end
a=norm(trin,inf)/norm(trouttmp,inf);
trout=a*trouttmp;
slicea(:,1)=pad_trace(trout,slicea(:,2));

if(picture==1)
    slicea=slicea*a;
    ind=near(t2,t(1),t(end));
    figure
    subplot(1,2,1)
    names{1}='input trace';
    fs=10;
    trplot(t,slice(ind,:),'names',names,'fontsize',fs,'yaxis','y','order','d')
    title({'before TVSW','filter slices sum to recreate the input'})
    yl=get(gca,'ylim');
    xlim([t(1) t(end)+.1*(t(end)-t(1))]);
    subplot(1,2,2)
    names{1}='output trace';
    slicea(:,1)=pad_trace(trout(:),slicea(:,2));
    trplot(t,slicea(ind,:),'names',names,'fontsize',fs,'yaxis','y','order','d')
    title({'after TVSW','filter slices are AGC''d and summed to create the output'});
    ylim(yl);
    xlim([t(1) t(end)+.1*(t(end)-t(1))]);
    prepfiga
end
    

     
 
  
  