function [Qave,tqa]=qint2qave(Qint,tq)
%
% [Qave,tqa]=qint2qave(Qint,tq)
% 
% Qint ... vector of interval Q's
% tq    ... vector of times whose length is 1 greater than the length of Qint
% NOTE: Qint(k) is the Q for the interval tq(k)->tq(k+1). This is why tq
%   must be one sample longer than Qint.
% 
% Qave ... vector of average Q's the same length at Qint. Qave(k) is the
%       average Q from t(1) to t(k+1). Qave(1) is identical to Qint(1).
%       This meanst the Qave correspond to times tq(2:end)
% tqa ... provided for convenience. This is just tq(2:end)
%
% Average Q from t1 to t3 is defined as the Q value needed in a single
% layer of time thickness t3-t1 to get the same attenuation as n interval
% Q's arrayed between t1 and t3 at times between t1 and t3. This if
% Qint=[Q1 Q2] and tq=[t1 t2 t3], then Qave=[Qa1 Qa2 Qa3] where Qa1=Qa2=Q1,
% and (t3-t0)/Qa3=(t2-t1)/Q1+(t3-t2)/Q2.
%
% G.F. Margrave, Devon Energy, 2016
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

% average Q from t(1) to t(k). Qave(1) is identical to Qint(1).


%columnize
Qint=Qint(:);
tq=tq(:);
%
dtq=diff(tq);
%Qave=zeros(size(Qint));
% dtm=mean(dt);
% dtq=[dt;dtm];%tack on the last interval
tq2=cumsum(dtq);
iQave=cumsum(dtq./Qint)./tq2;
Qave=1./iQave;
tqa=tq(2:end);