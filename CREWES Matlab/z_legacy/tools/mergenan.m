function vout=mergenan(v1,v2,w1,w2)
% vout=mergenan(v1,v2,w1,w2)
%
% MERGENAN forms the weighted sum of 2 matricies with special attention 
% paid to nan's. If both inputs are nan at any position, then the output
% will also be; however, if only one is nan then the output will equal
% the live matrix at that location. If both are live then the output is
% the direct weighted sum. If v1 and v2 contain no nans, then the result 
% will be exactly ((w1.*v1)+(w2.*v2))./(w1+w2); 
% The weights can be simple scalars or they can be matricies of exactly
% the same size as v1 and v2.
%
% G.F. Margrave March 1994
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
i1nan=isnan(v1);
i2nan=isnan(v2);
[m,n]=size(w1);
if( m==1 & n==1)
	w1=w1*ones(size(v1));
end
[m,n]=size(w2);
if( m==1 & n==1)
	w2=w2*ones(size(v2));
end
bothlive= (~i1nan).*(~i2nan);
vout=nan*ones(size(v1));
aa=find(bothlive==1);
vout(aa)= ((w1(aa).*v1(aa))+(w2(aa).*v2(aa)))./...
(w1(aa)+w2(aa));
	
i1dead=find(i1nan);
i2live=find(~isnan(v2(i1dead)));
vout(i1dead(i2live))=v2(i1dead(i2live));
i2dead=find(i2nan);
i1live=find(~isnan(v1(i2dead)));
vout(i2dead(i1live))=v1(i2dead(i1live));