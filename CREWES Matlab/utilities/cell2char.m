function a=cell2char(c)
%
% a=cell2char(c)
% 
%Given a cell array of strings, cell2char converts these into a single
%string vector with each cell string separated by a logical '|'. For example,
%let c{1}='fred' and c{2}='billygoat'. Then 
% a=cell2char(c) results in a='fred|billygoat'. The final form is usedful
% in user interface dialogs like askthings.
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

n=length(c);

nmax=n*120;%assume 120 charaters in each cell

a=char(32*ones(1,nmax));
n1=1;
for k=1:n
    sk=c{k};%the kth string
    if(k<n)
        n2=n1+length(sk);
        a(n1:n2)=[sk '|'];
    else
        n2=n1+length(sk)-1;
        a(n1:n2)=sk;
    end
    n1=n2+1;
end

if(n1<length(a))
    a(n1:end)='';
end