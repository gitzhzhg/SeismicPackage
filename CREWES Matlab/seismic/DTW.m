function [e,d,u]=DTW(s1,s2,L,b)
% DTW: estimates time-variant time shifts betweem two 1-D traces
% 
% [e,d,u]=DTW(s1,s2,L,b)
%
% DTW is an acronym for dynamic time warping. The algorithm is essentially 
% that of Hale (2013) with adaptations described in the 2014 CREWES report
% by Cui and Margrave, and the 2015 M.Sc. thesis by Cui. 
%
% s1 ... input trace to be warped
% s2 ... reference trace. Time shifts are w.r.t this trace. s1 and s2 
%        must have the same length
% L ... 2*L+1 lags will be searched 
% b ... the lag sequence is constrained to change by at most 1 sample in 
%       blocks of b samples
%
% e ... alignment error array
% d ... distance array
% u ... estimated lag sequence. estimated time shifts are u*dt (dt is the 
%       time sample interval of the two traces)
%
% by T. Cui, 2014
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
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE

% calculate the alignment error array e
N=length(s1);
e=zeros(2*L+1,N);
for n=1:N %column number
    for m=-L:L %row number
        npm=max(1,min(n+m,N));
        e(m+L+1,n)=norm(s1(n)-s2(npm),1); 
    end
end

% dynamic programming
% accumulation to calculate the distance array d
d=zeros(size(e));
for n=1:N
    n1=max(1,min(N,n-1));% 1<=(n1=n-1)<=N
    nb=max(1,min(N,n-b));% 1<=(nb=n-b)<=N
    for m=-L:L
        mm=max(m-1+L+1,1);% 1<=(mm=m-1)
        mp=min(m+1+L+1,L);% (mp=m+1)<=L
        dm=d(mm,nb);
        dn=d(m+L+1,n1);
        dp=d(mp,nb);
        for kb1=nb+1:n1
            dm=dm+e(mm,kb1);
            dp=dp+e(mp,kb1);
        end
        d(m+L+1,n)=e(m+L+1,n)+min([dm,dn,dp]);
    end 
end

% backtracking to choose the optimal lag sequence u
u=zeros(size(s1));
[~,u(end)]=min(d(:,end));
m=u(end);
n=N;
while n~=1  
n1 = max(1,min(N,n-1)); %1<=(n1=n-1)<=N
nb = max(1,min(N,n-b)); %1<=(nb=n-b)<=N
mm = max(m-1,1); 
mp = min(m+1,2*L+1);
dm = d(mm,nb);
di = d(m,n1);
dp = d(mp,nb);
   for kb=nb+1:n1 
       dm = dm+e(mm,kb);
       dp = dp+e(mp,kb);
   end
[~,ind]=min([dm,di,dp]);
   if ind==1
      m=mm;
   elseif ind==3
      m=mp;
   end
n=n-1;
u(n)=m;
   if m==mm||m==mp
      for kb=nb:n1-1
          n=n-1;
          u(n)=m;
      end
   end
end
u=u-L-1;
