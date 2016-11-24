function [mfilt,tm]=matchs(trin,trdsign,t,mlength,flag,mu)
% MATCHS: A least-squares, smoothness constrained, matching filter, good for wavelet estimation. 
%
% [mfilt,tm]=matchs(trin,trdsign,t,mlength,flag,mu)
%
% MATCHS designs a match filter of specified temporal length
% which matches trin to trdsign in the least squares sense and is
% constrained to be smooth. The smoothness constraint is imposed by
% requiring the second derivative of the filter (the model) to be small at
% the same time as we minimize the error of the data fit. The filter can be
% causal or non causal. See Constable et al, 1987, Geophysics, "Occams's
% inversion: a practical algorithm for generating smooth models from
% electromagnetic sounding data".
%
% trin= input trace to be matched to trdsign
% trdsign= input trace which is to be matched
% t= time coordinate vector for trin
% ***** note: trin and trdsign must be the same length
% mlength= length of the match filter in seconds
% flag=0 ... a fully noncausal operator is desired. (t=0 is in the middle)
%     =1 ... a causal operator is desired (t=0 is the first sample).
%     =N ... where N>1 means that the operator will have N-1 samples in negative time. This
%     allows a continuum of variation between symmetric and causal. (t=0 will be sample N).
% NOTE: flag=0 is the same as flag=nsamp/2+1 where nsamp is the closest odd number of samples
%   to the requested filter length. This control is about how many samples before a
%   given time and after that time are needed to predict trdesign. Fully causal operators, even
%   if the wavelet is known to be causal, are generally not as good as those where flag is
%   slightly greater than 1 (say 2 through 10).
% mu ... tradeoff parameter controlling the tradeoff between filter (model)
%       smoothness and data fitting. Should be a nonnegative number. Larger
%       values give smoother filters while 0 gives the best fit to the data
%       with no smoothness. 
% ************* default mu=.1 **********
%
% mfilt= output mlength match filter
% tm= time coordinate for the match filter
%
% by G.F. Margrave, 2016
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

%Figure out varargin
if(nargin<6)
    mu=.1;
end


% preliminaries
 n=round(mlength/(t(2)-t(1)))+1;
 nn=floor(n/2);
 n=2*nn+1;%force n odd
 trin2=trin(:);
 trdsign=trdsign(:);
%  if(flag==1)
%      trdsign=trdsign.*mwhalf(length(trdsign));
%  else
%      trdsign=trdsign.*mwindow(length(trdsign));
%  end
 
% generate the Toeplitz matrix for the convolution equations
 TRIN= convmtx(trin2,n);
 
% generate a 2nd derivative operator
D=zeros(n,n);
for k=2:n-1
    D(k,k-1:k+1)=[1 -2 1];
end
D(1,1:2)=[1 -1];
D(n,n-1:n)=[1 -1];
% if(flag==0)
%     for k=2:n-1
%         D(k,k-1:k+1)=[1 -2 1];
%     end
%     D(1,1:2)=[1 -1];
%     D(n,n-1:n)=[1 -1];
% else
%     for k=1:n-2
%         D(k,k:k+2)=[1 -2 1];
%     end
%     D(n-1,n-1:n)=[1 -1];
%     D(n,n)=1;
% end
D2=D'*D;

% solve the equations with left division
if flag>=1
    nneg=flag-1;%number of negative time samples
    TRTR=TRIN'*TRIN;
    %A=max(abs(TRTR(:)));
    B=mu*D2+TRTR;
    %mfilt=pinv(B)*TRIN'*[trdsign;zeros(n-1,1)];
    mfilt=pinv(B)*TRIN'*[zeros(nneg,1);trdsign;zeros(n-1-nneg,1)];
    
    %tm=xcoord(0.,t(2)-t(1),mfilt)';
    tm=(t(2)-t(1))*(-nneg:length(mfilt)-nneg-1)';
else
    nh=fix(n/2);
    top=[zeros(nh,1);trdsign;zeros(n-nh-1,1)];
    
    TRTR=TRIN'*TRIN;
    %A=max(abs(TRTR(:)));
    B=mu*D2+TRTR;
    mfilt=pinv(B)*TRIN'*top; %#ok<*MINV>
    
    %tm=xcoord(-(nh)*(t(2)-t(1)),t(2)-t(1),mfilt)';
    tm=(t(2)-t(1))*(-nh:nh)';
end

j=size(trin,1);
if j==1, mfilt=mfilt.'; tm=tm'; end
   
 
 