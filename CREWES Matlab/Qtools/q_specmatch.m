function [Q,T,obj]=q_specmatch(A1,A2,f,t1,t2,f1,f2,Qmax,Qmin)
% Q_specmatch: estimate Q from spectral matching
%
% [Q,T,obj]=Q_specmatch(A1,A2,f,t1,t2,f1,f2,Qmax,Qmin)
%
% It is assumed that A2 is related to A1 through the model
% A2=A1*T*exp(-pi*f*(t2-t1)/Q), where T and Q are real, positive, scalars
% to be determined. T represents frequency independent loss while Q
% parameterizes the frequency-dependent attenuation. Spectral matching is
% achived by searching the possible Q values for the minimum of the
% norm(A2-T*A1*T*exp(-pi*f*(t2-t1)/Q)).  By letting Q range over all
% integer valies from Qmin to Qmax, we can find the minimimum value of this
% norm. The Value of Q at the minimum is returned as the estimated Q. It is
% required to also estimate T and this is done by estimate_T.m . This
% method is, in theory, similar to spectra ratios but it is more stable
% because the estimate is found without division. The estimated value of T
% is critical to obtaining a realistic
% Q value. 
%
% A1 ... amplitude spectrum at time t1 (A1 must never vanish)
% A2 ... amplitude spectrum at time t2 (Note t2 should be greater than t1)
% f  ... frequency coordinate vector for A1 and A2. 
%NOTE: A1, A2, and f must be real-valued vectors of identical size
% t1 ... time at which the spectrum A1 was measured
% t2 ... time at which the spectrum t2 was measured
% f1 ... lowest frequency to trust in the spectral matching estimate
% f2 ... highest frequency to trust in the spectral matching estimate
% Qmax ... maximum value of Q in the search
% *********** default Qmax=250 ************
% Qmin ... minimum value of Q in the search
% *********** default Qmin=5 ***********
%
% Q       ... estimated Q value
% T       ... estimated transmission coefficient
% obj     ... objective function
%NOTE: obj has length Qmin:Qmax  
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

if(nargin<9)
    Qmin=5;
end
if(nargin<8)
    Qmax=250;
end

if(any(size(A1)~=size(A2)))
    error('A1 and A2 must be the same size')
end
if(any(size(f)~=size(A1)))
    error('size of f must be the same as A1 and A2')
end

infit=near(f,f1,f2);%frequency range of estimate
ef=f(infit);
Qtest=Qmin:Qmax;
delt=t2-t1;
%If A2=A1*T*exp(-pi*f*t/Q) where T is a frequency independent scalar
%due to transmission loss or reflection, then we need to estimmate T.
[T,Qe,f1e,f2]=estimate_T(A1(infit),A2(infit),ef,delt);
if(~isnan(T))
%     R=1;
    A2=A2/T;
    obj=zeros(size(Qtest));
    for k=1:length(Qtest)
        obj(k)=norm((A1(infit).*exp(-pi*ef*delt/Qtest(k))-A2(infit)));
    end
    [~,iq]=min(obj);
    Q=Qtest(iq);
else
    Q=Qe;
    obj=nan;
end