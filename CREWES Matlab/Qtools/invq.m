function qmatinv=invq(Q,t,tol)
% INVQ ... design an inverse Q matrix
% 
% qmatinv=invq(Q,t,tol)
%
% Method: use qmatrix to build a forward Q operator and then send this into
% pinv to build a pseudo inverse. The tolerance parameter is passed into
% pinv and serves to threshold the singular values. This can be used to
% control the inversion of low-amplitude noisy data components.
%
% Q ... value of Q, may be a single scalar or a vector of length(t)
%        of average Q values
% t ... time coordinate vector
% tol ... tolerance parameter in pinv
%  ************* default .000001 ****************
%
% qmatinv... matrix of size length(t) by length(t) which applies an inverse
%   Q filter to a column vector.
%
% % Example (Copy and paste these lines to the Matlab command window)
% dt=.002;%time sample rate
% tmax=2;%maximum trace time
% Q=50;%Q value
% fdom=30;%dominant frequency of minimum phase source waveform
% [r,t]=reflec(tmax,dt);%synthetic reflectivity
% [w,tw]=wavemin(dt,fdom,.2);
% qmat=qmatrix(Q,t,w,tw);%build the qmatrix
% sn=qmat*r;%apply the Q matrix to the reflectivity
% s=convm(r,w);%stationary synthetic for comparison
% qmatinv=invq(Q,t);%inverse Q matrix
% s2=qmatinv*sn;%undo the effects of Q
% figure
% plot(t,s,t,sn,'k',t,s2,'r.')
% xlabel('time (s)')
% legend('stationary',['nonstationary, Q=' num2str(Q)],'After inverse Q filter')
%
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

% design an inverse Q matrix 

if(nargin<3)
    tol=.000001;
end

%Forward Q matrix
qmat=qmatrix(Q,t,[1 0],[0 t(2)-t(1)]);
%invert
qmatinv=pinv(qmat,tol);