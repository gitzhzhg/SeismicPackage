function L=sphdiv(v,z,p)
% SPHDIV: for a v(z) medium, compute the geometrical spreading factor
%
% L=sphdiv(v,z,p)
%
% SPHDIV shoots a single ray or fan of rays through a stratified 
% velocity model and calculates their geometrical spreading factors.
% It is assumed that v is a vector of interval velocities and that
% the ray goes from z(1) to z(length(z)). This routine does no error
% checking for efficiency. Be sure that v and z are both the same
% length. It returns inf if a critical refration occurs. 
%
% v... COLUMN vector of interval velocities
% z... COLUMN vector of depths to the tops of the intervals
% NOTE: v and z must be at least length 2 or an abort will occur
% p... scalar or ROW vector ray parameters
%
% L ... scalar or ROW vector spreading factors
%
% G.F. Margrave and P. F. Daley, CREWES Project, Nov. 2000
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

%check for critical angle
iprop=1:length(z)-1;
sn = v(iprop)*p;
 %
 % sn is an n by m matrix where n is the length of iprop (the number of
 %    layers propagated through) and m is the length of p (the number of
 %    unique ray parameters to use). Each column of sn corresponds to a
 %    single ray parameter and contains the sin of the vertical angle;
 %
 
 [ichk,pchk]=find(sn>1);

 %compute x and t
 cs=sqrt(1-sn.*sn);
 vprop=v(iprop)*ones(1,length(p));
 thk=abs(diff(z))*ones(1,length(p));
 if(size(sn,1)>1)
   s1=sum((vprop.*thk)./cs);
   s2=sum((vprop.*thk)./(cs.^3));
   L=(cs(1,:)./vprop(1,:)).*sqrt(s1.*s2);
 else
   s1=(vprop.*thk)./cs;
   s2=(vprop.*thk)./(cs.^3);
   L=(cs(1,:)./vprop(1,:)).*sqrt(s1.*s2);
 end
 %assign zeros to nonphysical terms
 if(~isempty(ichk))
 	L(pchk)=zeros(size(pchk));
 end