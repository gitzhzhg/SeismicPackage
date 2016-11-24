function [mfilt,tm]=match(trin,trdsign,t,mlength,flag,varargin)
% MATCH: A least-squares matching filter, good for wavelet estimation. 
%
% [mfilt,tm]=match(trin,trdsign,t,mlength,flag,varargin)
%
% MATCH designs a match filter of specified temporal length
% which matches trin to trdsign in the least squares sense.
% That is sum_of_sqs(conv(mfilt,trin)-trdsign)==minimum. Options include
% causal or noncausal, and the ability to specify the solver to better
% handle noise.
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
%
% EXTRA ARGUMENTS must be name-value pairs
% 'solver' ... either 'leftdiv', or 'pseudoinv', or 'normaleqns'
%       Default is 'pseudoinv'
% 'tol' ... tolerance for solver 'pseudoinv'. This is expressed as a
%       fraction of the default tolerance. So a value of 2 is twice the
%       default and .5 is half the default. Must be nonnegative. Default is 1.
% 'stab' ... stability constant for solver 'normaleqns'. Must be
%       nonnegative and less than 1. The default tolerance is 0.0001.
%
%
% mfilt= output mlength match filter
% tm= time coordinate for the match filter
%
% by G.F. Margrave, 1991-2016
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
    varargin='';
end
nargs=length(varargin);
if(2*floor(nargs/2)~=nargs)
    error('extra arguments must be name-value pairs');
end
solver='pseudoinv';
tol=1;
stab=.0001;
for k=1:2:nargs
    name=varargin{k};
    switch name
        case 'solver'
            solver=varargin{k+1};
            if(~strcmp(solver,'leftdiv') && ~strcmp(solver,'pseudoinv') && ~strcmp(solver,'normaleqns'))
                error('solver must be one of ''leftdiv'', ''pseudoinv'', or ''normaleqns''')
            end
        case 'tol'
            tol=varargin{k+1};
            if(~isnumeric(tol) || tol<0)
                error('invalid value for tol')
            end
            
        case 'stab'
            stab=varargin{k+1};
            if(~isnumeric(stab) || stab<0 || stab>1)
                error('invalid value for stab')
            end
        otherwise
            error(['unrecognized name''' name ''' in name-value pair'])
    end
end



% preliminaries
 n=round(mlength/(t(2)-t(1)))+1;
 nn=floor(n/2);
 n=2*nn+1;%force n odd
 trin2=trin(:);
 trdsign=trdsign(:);
% generate the Toeplitz matrix for the convolution equations
 TRIN= convmtx(trin2,n);
% solve the equations with left division
if flag>=1
    nneg=flag-1;
    RHS=[zeros(nneg,1);trdsign;zeros(n-1-nneg,1)];
    switch solver
        case 'leftdiv'
            mfilt=TRIN\RHS;
        case 'pseudoinv'
            dtol=max(size(TRIN))*norm(TRIN)*eps(class(TRIN));
            mfilt=pinv(TRIN,tol*dtol)*RHS;
        case 'normaleqns'
            TRTR=TRIN'*TRIN;
            A=max(abs(TRTR(:)));
            mfilt=inv(TRTR+stab*A*eye(n))*TRIN'*RHS;
    end
    
    tm=(t(2)-t(1))*(-nneg:length(mfilt)-nneg-1)';
else
    nh=fix(n/2);
    top=[zeros(nh,1);trdsign;zeros(n-nh-1,1)];
    switch solver
        case 'leftdiv'
            mfilt=TRIN\top;
        case 'pseudoinv'
            dtol=max(size(TRIN))*norm(TRIN)*eps(class(TRIN));
            mfilt=pinv(TRIN,tol*dtol)*top;
        case 'normaleqns'
            TRTR=TRIN'*TRIN;
            A=max(abs(TRTR(:)));
            mfilt=inv(TRTR+stab*A*eye(n))*TRIN'*top; %#ok<*MINV>
    end
    %tm=xcoord(-(nh)*(t(2)-t(1)),t(2)-t(1),mfilt)';
    tm=(t(2)-t(1))*(-nh:nh)';
end

 j=size(trin,1);
 if j==1, mfilt=mfilt.'; tm=tm'; end
   
 
 