function prfilt=predict(trin,nop,nlag,stab)

% prfilt= predict(trin,nop,nlag,stab)
% prfilt= predict(trin,nop,nlag)
% prfilt= predict(trin,nop)
%
% PREDICT returns the nop long Wiener prediction filter for a  
% lag of nlag time samples. The filter is designed on the input
% trace, trin, and a stab factor is included by multiplying the 
% zero lag of the normalized autocorrelation by 1+stab.
%
% trin= input trace used to design the prediction operator
% nop= number of points in the prediction operator
% nlag= prediction lag distance in samples
% *********** default= 1 **************
% stab= stabilazation factor expressed as a fraction of the zero
%       lag of the autocorrelation.
%  ************ default= .0001 ***********
% prfilt= minimum phase Wiener prediction filter. Designed
%  such that:  prediction error= p.e.=> 
%      p.e.=trin(nlag:length(trin))-trinhat(1:length(trin)-nlag+1)
%  has minimum squared length and where:
%      trinhat= conv(trin,prfilt) is the predictable part of trin
%
% Re: Peacock and Treitel, Geophysics vol 34, 1968
% 
% by: G.F. Margrave, July 1991
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
  
% set defaults
  if nargin<4
    stab=.0001;
  end
  if nargin<3
     nlag=1;
  end
% generate the autocorrelation
  a=auto(trin,nlag+nop,0);
% stabilize the auto
  a(1)=a(1)*(1.0 +stab);
  a=a/a(1);
% generate the right hand side of the normal equations
  b=a(nlag+1:nlag+nop);
% do the levinson recursion
  prfilt=levrec(a(1:nop),b);
  