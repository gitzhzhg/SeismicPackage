function [Q,T,lsr,lsr_fit,f1,f2]=q_specrat(A1,A2,f,t1,t2,f1,f2,fsmo)
% Q_specrat: estimate Q from spectral ratios
%
% [Q,T,lsr,lsr_fit,f1,f2]=Q_specrat(A1,A2,f,t1,t2,f1,f2,fsmo)
%
% It is assumed that A2 is related to A1 through the model
% A2=A1*T*exp(-pi*f*(t2-t1)/Q), where T and Q are real, positive, scalars
% to be determined. T represents frequency independent loss while Q
% parameterizes the frequency-dependent attenuation. Forming the
% log-spectral-ratio gives lsr=log(A2./A2)=log(T)-pi*f*(t2-t1)/Q .  Thus a
% linear fit to the lsr determines both T and Q. A major problem is that
% this model will only apply to a very limited frequency band in which signal
% stands out well above noise. The lower bound of this band is required on
% input while the upper bound is estimated automatically by calling
% estimate_T. The estimated value of T is critical to obtaining a realistic
% Q value. As with any linear fit, the slope and intercept are correlated
% and an error in one affects the other. Also of prime importance is the
% accurate determination of the frequency range of the fit. A linear fit
% over the entire frequency range is almost always wrong.
%
% A1 ... amplitude spectrum at time t1 (A1 must never vanish)
% A2 ... amplitude spectrum at time t2 (Note t2 should be greater than t1)
% f  ... frequency coordinate vector for A1 and A2. 
%NOTE: A1, A2, and f must be real-valued vectors of identical size
% t1 ... time at which the spectrum A1 was measured
% t2 ... time at which the spectrum t2 was measured
% f1 ... lowest frequency to trust in the spectral ratio estimate
% f2 ... highest frequency to trust in the spectral ratio estimate
% fsmo ... length of spectral smoother (in Hz) applied to A1 and A2 befor
%        ratioing.
% ******** default = 0 **********
%
% Q       ... estimated Q value
% T       ... estimated transmission coefficient
% lsr     ... calculated log spectral ratio
% lsr_fit ... linear fit to SR. Calculated by least-squares between f1 and f2.
%NOTE: lsr and lsr_fit are vectors the same length as f. 
% f1,f2   ... upper and lower frequencies 
%
% by G.F. Margrave, 2014
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


if(nargin<8)
    fsmo=0;
end
if(any(size(A1)~=size(A2)))
    error('A1 and A2 must be the same size')
end
if(any(size(f)~=size(A1)))
    error('size of f must be the same as A1 and A2')
end

delt=t2-t1;
i1=near(f,f1);
infit=i1(1):length(f);%frequency range of estimate
% infit=near(f,f1,f2);
ef=f(infit);
if(fsmo>0)
    nsmo=max([1 round(fsmo/(f(2)))]);
    op=ones(1,nsmo);
    A2=convz(A2,op)/length(op);
    A1=convz(A1,op)/length(op);
end
%If A2=A1*T*exp(-pi*f*t/Q) where T is a frequency independent scalar
%due to transmission loss or reflection, then we need to estimmate T.
[T,Qe,f1e,f2e]=estimate_T(A1(infit),A2(infit),ef,delt);
if(~isnan(T))
    infit=near(f,f1,f2);
    A2=A2/T;
    lsr=log(A2./A1);
    % SR=abs(S2)./abs(S1);

    %estimate Q

    p=polyfit(f(infit),lsr(infit),1);
    lsr_fit=polyval(p,f);

    % determine interval Q
    Q=-pi*delt/p(1);
else
    Q=nan;
    lsr=nan*ones(size(f));
    lsr_fit=nan*ones(size(f));
end