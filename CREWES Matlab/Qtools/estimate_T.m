function [T,Q,f1,f2]=estimate_T(A1,A2,f,dt)
%
% 
% [T,Q,f1,f2]=estimate_T(A1,A2,f,dt)
%
% In Q estimation, it can be difficult to separate reflectivity effects
% (transmission loss) from attenuation.  While the latter is strongly
% frequency dependent and the former is not, for typical seismic wavelets
% which are narrow band, these effects are easily confused. Q estimation
% usually involves the comparison of two signals, one with less attenuation
% having amplitude spectrum A1 and the other with more attenuation due to
% increased traveltime and having amplitude spectrum A2. A simple model
% connecting A1 with A2 is A2=A1*T*exp(-pi*f*dt/Q) where T is a frequency
% independent scalar due to transmission loss or reflection, f is freqency,
% dt is the traveltime difference, and Q is the attenuation constant. The
% goal here is to estimate T and this in done is a way which also produces
% an estimate of Q. If we form the log-spectral-ratio, or lsr, given by
% lsr=log(A2/A1) = log(T) -pi*f*dt/Q, we see that our model predicts that
% the lsr is a linear function of frequency whose intercept determines T and
% whose slope determines Q. The essential problem is that this model only
% ever fits the data over a narrow frequency band and will definitely be
% erroneous if fitted over all frequencies. Because attenuation predicts a
% propagating wavelet that is progressively losing high-frequency signal,
% the signal band of A2 will always be less than that of A1, and this
% effect will worsen as dt increases. If frequecies higher than the maximum
% signal frequency of A2 are permitted into the solution, then A2 will
% appear to have too much strength and this can lead to unreasonable
% estimates of T and Q. If frequencies higher than the signal maximum of A1
% are allowed, then the situation becomes worse yet.
%
% Method: We fit a straight line to the lsr but only allow frequencies
% within a limited band f1<=f<=f2 into the fit. Initially f1 is set to f(1)
% and f2 is f(k) where k=2,3,...length(f). Thus we fix f1 and allow f2 to
% vary over the range of f. At each f2, both T and Q are estimated. An
% estimate for T is considered valid only if it lies in the range 0<T<=1.
% Those values of f2 giving valid T's are flagged and examined for a
% continuous range up to some maximum. Outliers beyond this maximum are
% rejected. The final estimate for T is returned as the median of those
% obtained from the valid set, and the same for Q.
%
% A1 ... amplitude spectrum correspondng to the earlier time with less
%       attenuation.
% A2 ... amplitude spectrum corresponding to the later time with more
%       attenuation.
% f ... frequency coordinate vector for A1 and A2 (in Hertz).
% dt ... time difference t2-t1 where t2 is the time of measurement for A2
%       and t1 for A1. Must be a positive number (in seconds).
% NOTE: A1,A2, and F must all be vectors of the same length. dt is a
%       scaler.
%
% T ... estimate of the transmission scalar
% Q ... estimate of Q
% f1 ... lower frequency bound used in the estimmates
% F2 ... upper frequency bound used in the estimates
%
% 
% G.F. Margrave, 2014, CREWES
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

if(any(size(A1)~=size(A2)))
    error('A1 and A2 must be the same size')
end
if(any(size(f)~=size(A1)))
    error('size of f must be the same as A1 and A2')
end
if(dt<=0 || dt>10)
    error('dt must be positve and expressed in seconds')
end
method='specrat';
if(strcmp(method,'specrat'))

    f1=f(1);
    f2k=f(2:end);
    nk=length(f)-1;
    Tk=zeros(1,nk);
    Qk=Tk;
    vark=Tk;
    lsr=log(A2./A1);% the log spectral ratio

    for k=2:nk
        infit=1:k;
        p=polyfit(f(infit),lsr(infit),1);
        lsrfit=polyval(p,f(infit));
        vark(k-1)=sum((lsr(infit)-lsrfit).^2)/k;
        Tk(k-1)=exp(p(2));
        Qk(k-1)=-pi*dt/p(1);
    end

    %ok find the valid T values

    %estimate Tmax
    % p=polyfit(f,lsr,3);
    % Tmax=exp(p(4));
    % Tmax=min([Tmax 1]);
    Tmax=1.0;
    indr1=find(Tk>0);
    indr2= Tk(indr1)<=Tmax;
    indr=indr1(indr2);%these are valid.
    %search for valid Q's
    indQ1=find(Qk(indr)>0);
    indQ2= Qk(indr(indQ1))<200;
    ivalid=indr(indQ1(indQ2));
    %check the variance
    n0=max([3 round(nk/10)]);
    v0=mean(vark(1:n0));
    indv=vark(ivalid)<10*v0;
    ivalid=ivalid(indv);

    if(isempty(ivalid))
       T=1;
       Q=nan;
       f2=nan;
       return;
    end

    %search for a continuous range of f2 and reject outliers
    di=diff(ivalid);
    ind=find(di>1);
    if(~isempty(ind))
        ivalid=ivalid(1:ind(1));
    end

    if(isempty(ivalid))
       T=1;
       Q=nan;
       f2=nan;
       return;
    end

    T=median(Tk(ivalid));
    Q=median(Qk(ivalid));
    f2=f2k(ivalid(end));
elseif(strcmp(method,'domfreq'))
    
else
    error('invalid method')
end