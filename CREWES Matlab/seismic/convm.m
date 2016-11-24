function s = convm(r,w,pct)
% CONVM: convolution followed by truncation for min phase filters
%
% s= convm(r,w,pct)
%
% CONVM is a modification of the 'conv' routine from the MATLAB
% toolbox. The changes make it more convenient for seismic purposes
% in that the output vector, s, has a length equal to the first
% input vector,  r. Thus, 'r' might correspond to a reflectivity
% estimate to be convolved with a wavelet contained in 'w' to
% produce a synthetic seismic response 's'. It is assumed that
% the wavelet in w is causal and that the first sample occurs at time zero.
% For non-causal wavelets, use 'convz'. An warning will occur if
% w is longer than r. If the first argument is a matrix, then convm outputs
% a matrix of the same size where the second argument has been convolved
% with each column. By default convm does a raised cosine taper at the end
% of the trace to reduce truncation artefacts.
%
% r ... reflectivity
% w ... wavelet
% pct ... percent taper at the end of the trace to reduce truncation
%       effects. See mwhalf.
%  ********** default = 10 ********
%
% by G.F. Margrave, May 1991
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

if(nargin<3)
    pct=10;
end

% 
%convert to column vectors
[a,b]=size(r);
if(a==1) r=r.'; end
w=w(:);
[nsamps,ntr]=size(r);
% if(length(w)>nsamps) 
%     warning('second argument longer than the first, output truncated to length of first argument.'); 
% end
s=zeros(size(r));
if(pct>0)
    mw=mwhalf(nsamps,pct);
else
    mw=ones(nsamps,1);
end

for k=1:ntr
    temp=conv(r(:,k),w);
    s(:,k)=temp(1:nsamps).*mw;
end

if(a==1)
	s=s.';
end