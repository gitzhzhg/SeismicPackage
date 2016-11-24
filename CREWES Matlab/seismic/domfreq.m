function fdom=domfreq(w,t,p,t1,t2,taperopt)
%DOMFREQ ... Estimate the dominant frequency of a signal
%
% fdom=domfreq(w,t,p,t1,t2,taperopt);
%
% If W is the Fourier transform of the input signal, then the dominant
% frequency is estimated by the centroid method
% fdom=Int(f*abs(W).^p)/Int(abs(W).^p)
% where f is frequency, Int is an integral over frequency, and p is
% typically 2.
%
% w ... input signal (maybe a wavelet or can be a gather. If a gather, then
%       there should be one trace per column and the returned fdom will
%       have one value per trace.)
% t ... time coordinate for w
% p ... small integer
% **********default p=2 ********
% t1 ... start time of estimation window
% t2 ... end time of estimation window
% If w is a gather, then t1 and t2 should either be a single scalar each or
% one value per trace.
% ********** default for t1 and t2 is the entire trace *******
% taperopt ... 0  means use a boxcar window
%              1  means use a half mwindow that tapers only at the end
%              2  meanse use an mwindow with tapers at both ends
% ********** default = 0 ***********
%
% fdom ... dominant frequency of w
%
% by G.F. Margrave, 2013
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

if(nargin<3)
    p=2;
end
if(nargin<4)
    t1=t(1);
    t2=t(end);
end
if(nargin<6)
    taperopt=0;
end

if(sum(abs(w))==0)
    error('Input signal is all zero')
end
[nt,ntraces]=size(w);
if(nt==1)
    ntraces=1;%takes care of the row vector input case
    nt=length(w);
    w=w';
end
if(ntraces>1)
    if(length(t1)==1)
        t1=t1*ones(1,ntraces);
        t2=t2*ones(1,ntraces);
    else
        if(length(t1)~=ntraces)
            error('length(t1) must equal the number of traces');
        end
        if(length(t2)~=ntraces)
            error('length(t2) must equal the number of traces');
        end
    end
end
fdom=zeros(1,ntraces);
for k=1:ntraces
    tmp=w(:,k);
    ind=near(t,t1(k),t2(k));
    if(taperopt==0)
        win=ones(length(ind),1);
    elseif(taperopt==1)
        win=mwhalf(length(ind),10);
    elseif(taperopt==2)
        win=mwindow(length(ind),10);
    else
        error('invalid taperopt');
    end
    [W,f]=fftrl(tmp(ind).*win,t(ind));
    fdom(k)=sum(f.*abs(W).^p)/sum(abs(W).^p);
end

% [W,f]=fftrl(w,t);
% if(ntraces==1)
%     fdom=sum(f.*abs(W).^p)/sum(abs(W).^p);
% else
%     ff=f*ones(1,ntraces);
%     fdom=sum(ff.*abs(W).^p)./sum(abs(W).^p);
% end