function [trout,x]= deconw(trin,trdsign,n,stab)
% [trout,x]=deconw(trin,trdsign,n,stab)
% [trout,x]=deconw(trin,trdsign,n)
%
% routine performs a Weiner style deconvolution of the
% input trace
%
% trin= input trace to be deconvolved
% trdsign= input trace to be used for operator design
% n= number of autocorrelogram lags to use (and length of
%    inverse operator
% stab= stabilization factor expressed as a fraction of the
%       zero lag of the autocorrelation.
%      ********* default= .0001 **********
%
% trout= output trace which is the deconvolution of trin
% x= output inverse operator used to deconvolve trin
%
% by: G.F. Margrave, May 1991
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
% generate the autocorrelation
  a=auto(trdsign,n,0);
% stabilize the auto
  a(1)=a(1)*(1.0 +stab);
  a=a/a(1);
% generate the right hand side of the normal equations
  b=[1.0 zeros(1,length(a)-1)];
% do the levinson recursion
  x=levrec(a,b);
% normalize the inverse operator
  x=x/sqrt(x'*x);
% deconvolve trin
  trout=convm(trin,x);
  trout=balans(trout,trin);
  