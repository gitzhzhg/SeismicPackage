function hh=phspec(t,ss,flag,unwrapflag)
%
% hh=phspec(t,s,flag)
% hh=phspec(t,s)
%
% PHSPEC plots a simple Fourier phase spectrum.
%
% Note: PHSPEC plots in the current axes. Use the figure command if you
% want a new window first.
%
% s= input trace or traces. Should be a column vector
% t= input time vector
% flag= 0... apply an n length mwindow to s before transforming
%       1... apply an n length half-mwindow to s before transforming
%       2... transform directly without windowing
% ************* default=2 **************
% unwrapflag= 0 do not unwrap phase
%             1 unwrap phase
% ************* default = 0 ************
%
% hh= handles of phase curves
% 
% by G.F. Margrave
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

[m,n]=size(ss);
%check for multiple traces
if((m-1)*(n-1)>0)
    ntrcs=n;
else
    ntrcs=1;
    ss=ss(:);%ensure column vector
end

if nargin<3
   flag=2; %window flag
end
if nargin<4
   unwrapflag=0; %unwrap flag
end

hh=zeros(1,ntrcs);

for k=1:ntrcs
    
    if(k==2)
        hold on;
    end
    
    s=ss(:,k);
    
    if flag <2
        mw=ones(size(s));
        if flag==0, mw=mwindow(s(1,:));end
        if flag==1, mw=mwhalf(s(1,:));end
        for it=1:m, s(it,:)=s(it,:).*mw;end
    end
    %adjust for time zero
    izero=near(t,0);
    %make sure its close
    if(abs(t(izero))<t(2)-t(1))
        s2=[s(izero:length(s)); s(1:izero-1)];
    else
        disp('***WARNING*** unable to find time zero, phase may be inaccurate')
    end
    
    %spectrum
    [spec,f]=fftrl(s2,t);
    
    %to db & phs
    spec=todb(spec);
    %s=fft(s',n);
    
    phr=imag(spec);
    
    if(unwrapflag)
        phr=unwrap(phr);
    end
    
    ph=phr*180/pi;
    
    % now plot it
    hh(k)=plot(f,ph);
    
end

grid;
set(gca,'xlabel',text(0,0,' Frequency (Hz)'));
set(gca,'ylabel',text(0,0,' phase in degrees'));
if(~unwrapflag)
    ylim([-180 180])
end