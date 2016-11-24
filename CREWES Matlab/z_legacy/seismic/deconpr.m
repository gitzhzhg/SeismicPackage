function [trout,x]= deconpr(trin,trdsign,nop,nlag,stab)
% [trout,x]= deconpr(trin,trdsign,nop,nlag,stab)
% [trout,x]= deconpr(trin,trdsign,nop,nlag)
%
% DECONPR performs Wiener predictive deconvolution by calling
% PREDICT to design a prediction filter, nop long with lag nlag
% and stab factor, using trdsign. The predicted part of trin, trinhat,   
% then formed by convolving the prediction operator with trin,
% and trout is computed by delaying trinhat by nlag samples and 
% subtracting it from trin. The prediction operator is returned
% in x.
%
% trin= input trace to be deconvolved
% trdsign= input trace used to design the prediction operator
% nop= number of points in the prediction operator
% nlag= prediction lag distance in samples
% stab= stabilazation factor expressed as a fraction of the zero
%       lag of the autocorrelation.
%  ************ default= .0001 ***********
%
% trout= deconvolved output trace
% x= prediction operator
%
% See also: Peacock and Treitel, Geophysics vol 34, 1968
%  and the description of PREDICT
%
% by G.F. Margrave, July 1991
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
 if nargin<5
   stab=.0001;
 end
% design prediction operator
 x= predict(trdsign,nop,nlag,stab);
% form the predicted part of trin
 trinhat= conv(trin,x);
% delay and subtract
 trout= trin(nlag+1:length(trin))-trinhat(1:length(trin)-nlag);
 trout= [trin(1:nlag) trout];
% balance the output
 trout=balans(trout,trin);