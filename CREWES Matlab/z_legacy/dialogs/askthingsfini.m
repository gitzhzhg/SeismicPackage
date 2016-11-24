function a=askthingsfini
% Please use "askthingsle" if developing new programs
%
% a=askthingsfini
%
% ASKTHINGSFINI terminates the askthings dialog and returns a matrix of
% strings containing the answers to the asked questions. One per row. If the
% user canceled then a is equal to -1.
%
% G.F. Margrave January 1994
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
as=get(gca,'userdata');
% hfig=abs(as(length(as))); %fix
% as(length(as))=[]; %fix
% test for cancel
if(as==-1)
	a=as;
	return;
end
ind=find(double(as)==255);
% determine the maximum answer length
test=[0 ind];
len=diff(test)-1;
maxlen=max(len);
% get the answers
na=length(ind);
a=setstr(32*ones(na,maxlen));
kbegin=1;
for k=1:na
	a(k,1:len(k))=as(kbegin:ind(k)-1);
	kbegin=ind(k)+1;
end
set(gca,'userdata',[]);
% close(hfig); %fix