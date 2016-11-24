function [mfilt,tm]=match_pinv(trin,trdsign,t,mlength,flag,tol)
% [mfilt,tm]=match(trin,trdsign,t,mlength,flag,tol)
%
% MATCH designs a match filter of temporal length 'mlength'
% which matches trin to trdsign in the least squares sense.
% That is sum_of_sqs(conv(mfilt,trin)-trdsign)==minimum
%
% trin= input trace to be matched to trdsign
% trdsign= input trace which is to be matched
% t= time coordinate vector for trin
% ***** note: trin and trdsign must be the same length
% mlength= length of the match filter in seconds
% flag=0 ... a noncausal operator is desired
%     =1 ... a causal operator is desired
% tol ... tolerance for pinv command. See pinv for more.
% NOTE: Suppose that 'a' is a known time series and 'w' is also
%  a known wavelet. Then let bm=convm(a,w) and bz=convz(a,w).
%  Then [westm,tw]=match(a,bm,ta,length(w),1) or
%       [westz,tw]=match(a,bz,ta,length(w),0) 
%  will both produce good estimates of w. 
%  However, 
%       [westx,tw]=match(a,bz,ta,length(w),1) 
%  should not be used
%  but
%       [westy,tw]=match(a,bm,ta,2*length(w),0)
%  will produce a valid estimate of w in the second half of westy.         
%
% mfilt= output mlength match filter
% tm= time coordinate for the match filter
%
% by G.F. Margrave, June 1991
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
% preliminaries
 n=round(mlength/(t(2)-t(1)))+1;
 trin2=trin(:);
 trdsign=trdsign(:);
% generate the Toeplitz matrix for the convolution equations
 TRIN= convmtx(trin2,n);
% solve the equations with left division
tol_default=max(size(TRIN))*norm(TRIN)*eps(TRIN(1));
 if flag==1
  mfilt=pinv(TRIN,tol*tol_default)*[trdsign;zeros(n-1,1)];
  tm=xcoord(0.,t(2)-t(1),mfilt);
 else
  nh=fix(n/2);
  top=[zeros(nh,1);trdsign;zeros(n-nh-1,1)];
  mfilt=pinv(TRIN,tol*tol_default)*top;
  tm=xcoord(-(nh)*(t(2)-t(1)),t(2)-t(1),mfilt);
 end
 [j,k]=size(trin);
 if j==1, mfilt=mfilt.'; tm=tm'; end
   
 
 