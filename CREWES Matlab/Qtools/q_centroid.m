function [Q,obj,fd1,fd2]=q_centroid(A1,A2,f,t1,t2,f1,f2,Qmax,Qmin,p)
% Q_centroid: estimate Q from matching centroid frequecies
%
% [Q,obj,fd1,fd2]=Q_centroid(A1,A2,f,t1,t2,f1,f2,Qmax,Qmin,p)
%
% It is assumed that A2 is related to A1 through the model
% A2=A1*T*exp(-pi*f*(t2-t1)/Q), where T and Q are real, positive, scalars
% to be determined. T represents frequency independent loss while Q
% parameterizes the frequency-dependent attenuation. The centroid
% frequency, also known as the dominant frequency is defined by
% fc=sum(f.*A.^p)/sum(A.^p) for spectrum A where p defaults to 2. Let fc2
% be the centroid frequency for spectrum A2 and consider the spectrum
% Aq=A1*exp(-pi*f*(t2-t1)/Q) which has centroid frequency fc. By letting Q
% range over all integer valies from Qmin to Qmax, we can find the
% minimimum value of Qtest.*(fc-fc2).^2 where Qtest=Qmin:Qmax. The value of
% Q at the minimum is returned as the estimated Q. A virtue of this method
% is that is does not requite estimation of T. The method fails if fc1 is
% less than fc2.
%
% A1 ... amplitude spectrum at time t1 (A1 must never vanish)
% A2 ... amplitude spectrum at time t2 (Note t2 should be greater than t1)
% f  ... frequency coordinate vector for A1 and A2. 
%NOTE: A1, A2, and f must be real-valued vectors of identical size
% t1 ... time at which the spectrum A1 was measured
% t2 ... time at which the spectrum t2 was measured
% Qmax ... maximum value of Q in the search
% *********** default Qmax=250 ************
% Qmin ... minimum value of Q in the search
% *********** default Qmin=5 ***********
% p ... exponent for dominant frequency calclation
% ************* default p=2 *********
%
% Q       ... estimated Q value
% obj     ... objective function
% fc1     ... dominant frequency of spectrum A1
% fc2     ... dominant frequency of spectrum A2
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
if(nargin<10)
    p=2;
end
if(nargin<9)
    Qmin=5;
end
if(nargin<8)
    Qmax=250;
end
if(nargin<7)
    f2=f(end);
end
if(nargin<6)
    f1=f(1);
end

if(any(size(A1)~=size(A2)))
    error('A1 and A2 must be the same size')
end
if(any(size(f)~=size(A1)))
    error('size of f must be the same as A1 and A2')
end

Qtest=Qmin:Qmax;
delt=t2-t1;
ind=near(f,f1,f2);
fd1=sum(f(ind).*A1(ind).^p)/sum(A1(ind).^p);
fd2=sum(f(ind).*A2(ind).^p)/sum(A2(ind).^p);
sig1=sum((f(ind)-fd1).^2.*A1(ind).^p)/sum(A1(ind).^p);
sig2=sum((f(ind)-fd2).^2.*A2(ind).^p)/sum(A2(ind).^p);
if(fd1>fd2)
    fdtest=zeros(size(Qtest));
    sigtest=fdtest;
    for k=1:length(Qtest)
        Atest=A1(ind).*exp(-pi*f(ind)*delt/Qtest(k));
        fdtest(k)=sum(f(ind).*Atest.^p)/sum(Atest.^p);
        sigtest(k)=sum((f(ind)-fdtest(k)).^2.*Atest.^p)/sum(Atest.^p);
    end
    %find where fdtest is closest to fd2;
    obj1=Qtest.*(fdtest-fd2).^2;%objective function for fdom
    obj2=Qtest.*(sigtest-sig2).^2;%objective function for sigma
    obj=obj1/max(obj1)+1*obj2/max(obj2);
    [omin,imin]=min(obj);
    Q=Qtest(imin);
else
    Q=inf;
    obj=[];
end