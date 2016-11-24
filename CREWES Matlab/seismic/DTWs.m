function [e,dint,M,uint] = DTWs(s1,s2,L,j)
% DTWs: estimates smooth time-variant time shifts betweem two 1-D traces
% 
% [e,dint,M,uint]=DTWs(s1,s2,L,j)
%
% DTWs is an acronym for smooth dynamic time warping. The algorithm is 
% essentially that of Compton and Hale (2014) with adaptations described in
% the 2015 CREWES report by Cui and Margrave, and the 2015 M.Sc. thesis by Cui. 
%
% s1 ... input trace to be warped
% s2 ... reference trace. Time shifts are w.r.t this trace. s1 and s2 
%        must have the same length
% L ... 2*L+1 lags will be searched 
% j ... the coarse sample numbers where the locally optimal subpaths are 
%       searched and chosen, e.g. j=1:h:N (h is the coarse sample interval, 
%       N is the number of the samples in a trace). DTWs(h=1) is equivalent to 
%       DTW(b=1).
%       
% e ... alignment error array
% dint ... distance array d after interpolation so that size(dint,2)=length(t)
% M ... the array storing the locally optimal subpaths while accumulation. 
%       The globally optimal path is chosen from M while backtracking
% uint ... estimated smooth lag sequence u after interpolation so that 
%          length(uint)=length(t). The estimated time shifts are uint*dt 
%          (dt is the time sample interval of the two traces)
%
% by T. Cui, 2015
%
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.

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
       if n+m<1||n+m>N
          e(m+L+1,n)=norm(s1(n),1);
       else
          e(m+L+1,n)=norm(s1(n)-s2(n+m),1); 
       end
    end
end

% dynamic programming
% accumulation to calculate the distance array d
n=1:N;
Nj=length(j);
d=zeros(2*L+1,Nj);
M=zeros(2*L+1,Nj-1);
d(:,1)=e(:,1);
for a=2:Nj
    h=n(j(a))-n(j(a-1));
    for m=-L:L
      dj=inf;
      ql=max([-h m-L]);
      qh=min([h m+L]);
      for q=ql:qh
          dq=d(m-q+L+1,a-1);
          for p=0:h-1
              k=p*q/h;
              kr=rem(k,1);
              if kr==0
              dq=dq+e(m-k+L+1,n(j(a))-p);
              else
              kl=floor(k);
              kh=ceil(k);
              el=e(m-kl+L+1,n(j(a))-p);
              eh=e(m-kh+L+1,n(j(a))-p);
                   if q>0
                   dq=dq+el*(1-kr)+eh*kr;%linear interpolation
                   else
                   dq=dq+el*(-kr)+eh*(1+kr);%linear interpolation   
                   end
              end
          end
          if dq<dj
             dj=dq;
             d(m+L+1,a)=dj;
             M(m+L+1,a-1)=q;
          end    
      end
    end
end
% interpolate d
dint=zeros(size(e));
for a=1:Nj-1
    temp=d(:,a)*ones(1,j(a+1)-j(a));
    dint(:,j(a):j(a+1)-1)=temp;
end
if j(end)~=N
    dint(:,j(end):N)=d(:,end)*ones(1,N-j(end)+1);
end

% backtracking to choose the optimal lag sequence u
u=zeros(1,Nj);
[~,u(end)]=min(d(:,end));
for a=Nj-1:-1:1
    u(a)=u(a+1)-M(u(a+1),a);
end
u=u-L-1;
% interpolate u
uint=interp1(j,u,1:N,'linear','extrap');
