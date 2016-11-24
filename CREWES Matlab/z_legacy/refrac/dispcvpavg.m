function dispcvpavg
% Display of all the Cross Over point averages (left and rigth) for each shot
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

% with the arrival time curves 
f=gcf;
fbtime=refdata('get','fbtime');
fbcoord=refdata('get','fbcoord');
nshots = refdata('get','nshots');
cvpi = refdata('get', 'cvpi');
cvpj = refdata('get', 'cvpj');
shotcoord=refdata('get','shotcoord');
cvpavg=refdata('get','cvpavg');
% Call the Cross Over point averaging function
if( length(cvpavg) == 0 )
[cvpavg, cvpstd, cvpfold] = avgcvp(cvpi, cvpj, nshots);
figure('menubar','none');
hold on;
for n=1:nshots
	plot(fbcoord(n,:),fbtime(n,:))
	valid=find(~isnan(fbtime(n,:)));
    if (isnan(cvpavg(n,1)) ~=1)
	timei=interp1(fbcoord(n,valid),fbtime(n,valid),cvpavg(n,1));
	plot(cvpavg(n,1),timei,'color','c','linestyle','o')
    end
   if (isnan(cvpavg(n,2)) ~=1)
 	timej=interp1(fbcoord(n,valid),fbtime(n,valid),cvpavg(n,2));
	plot(cvpavg(n,2),timej,'color','r','linestyle','*')
    end
end
% Addition of the shot number to the display
xy=axis;
t=xy(4)-xy(3);
d=t/40;
for n=10:10:nshots
  str=sprintf('%d',n); 
  text(shotcoord(n),xy(3)+d,str)
end
text(xy(1)+100,xy(3)+2.5*d,'shot number')
xlabel('Coordinate (m)');
ylabel('Traveltime (ms)');
title('Traveltime for all shots with their corresponding cross over point averages (left: blue, rigth: red)');
set(gcf,'units','pixels','position',[0 0 864 576]);
figure(f); set(gcf,'menubar','none');