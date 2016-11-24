function k=kurtosis(X,flag)
% k=kurtosis(X), k=kurtosis(X,flag)
%
% Kurtosis is a measure of the peakedness of the probablility distribution
% of a real random variable.  It is used in proabablitiy and statistics.
%
% This function will return the kurtosis of set of data.  If the data is a
% 2-D matrix it will return a value for each column.
%
% if flag=0 a scaling is applied. flag=1; is the default.
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

%
if nargin <2
    flag=1;
end


sz=size(X);
if sz(1)==1
    X=X';
sz=size(X);
end
n=sz(1);

xbar=mean(X);
k=ones(1,sz(2));

for m=1:sz(2)
    x=X(:,m);
xi4=(1/n)*sum((x-xbar(m)).^4);
xi2=(1/n)*sum((x-xbar(m)).^2);
k(m)=(xi4/(xi2.^2));
end

if ~flag
    k=(((n-1)/((n-2)*(n-3)))*(((n+1)*k)-(3*(n-1))))+3;
end

end