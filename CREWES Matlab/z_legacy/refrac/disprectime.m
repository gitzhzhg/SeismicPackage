function disprectime(rtrange,rt1,mint);
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

% Display of the reciprocal time for shot pair(s) 
f=gcf;
diffmat=refdata('get','diffmat');
nshots=refdata('get','nshots');
absdiffmat=abs(diffmat);
% All the possible shot pairs
if (rtrange==0)
	figure('menubar','none')
	image(absdiffmat);
	set(gca,'ydir','normal');
	colormap(hsv);
        colorbar;
	xlabel('shot number');
	ylabel('shot number');
	zlabel('Reciprocal traveltime difference (ms)');
	title('Reciprocal times ')
	figure('menubar','none');
	[s1,s2]=find(absdiffmat>mint);
	plot(s2,s1,'*');
	xlabel('shot number');
	ylabel('shot number');
	titlestr = sprintf('Shot pairs with over %d ms of reciprocal time difference ', mint);
	title(titlestr);
% Only two shots with all the other possible shots 
else
        figure('menubar','none')
	hold on;
	plot(1:nshots,absdiffmat(rt1,:),'gx')
	plot(1:nshots,absdiffmat(:,rt1),'gx')
	a1=find(absdiffmat(rt1,:)>mint);
	a2=find(absdiffmat(:,rt1)>mint);
	plot(a1,absdiffmat(rt1,a1),'go');
	plot(a2,absdiffmat(a2,rt1),'go');
	xlabel('shot number');
	ylabel('Reciprocal traveltime difference (ms)');
	titlestr = sprintf('Reciprocal time difference for shot %d (circles show a time difference over %d) ',rt1, mint);
	title(titlestr);
        set(gcf,'units','pixels','position',[0 0 864 576])
end