%first break vs shot & rec location along the survey line
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

nshots=input('What is the number of shots?  ');
nrecs=input('What is the number of receivers?  ');
%figure;
%hold;
%for i=1:nshots;
%  plot(reclocation(i,:),shotpick(i,:));
%end;
%xlabel('rec&shot location along the survey line (m)');
%ylabel('traveltime (ms)');
%title('Refracted P-wave picks for shots #1-189');
figure('menubar','none');
hold;
for i=1:nshots;
  plot(reclocation(i,:),pickuphole(i,:));
end;
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime (ms)');
title('Refracted P-wave picks for shots with uphole time #1-189');
%check reciprocal time
%diffmat = rectime(nshots, nrecs, reclocation, shotpick, r);
diffmat = rectimeup(nshots, nrecs, reclocation, pickuphole, r);
absdiffmat=abs(diffmat);
figure('menubar','none');
hold;
    for i=1:nshots-1;
	plot(absdiffmat(i,:));
    end;
xlabel('shot number');
ylabel('Reciprocal traveltime difference (ms)');
title('Reciprocal time check ')
[s1,s2]=find(absdiffmat>20);
m=size(s1);
figure('menubar','none');
for k=1:m; plot(s2,s1,'x'); end
xlabel('shot number');
ylabel('shot number');
title('Reciprocal time check, shot pairs with over 20 ms of difference ')
%Traveltime difference between two shot
i =input('What is the first shot number smallest?  ');
j =input('What is the second shot number largest?  ');
step = 1;
%[start1, end1, tdiff1, start2, end2, tdiff2] = shotsub(i,j,r,reclocation,shotpick,step);
[start1, end1, tdiff1, start2, end2, tdiff2] = shotsubup(i,j,r,reclocation,shotpick,step);
%figure;
%hold;
%plot(reclocation(i,:),shotpick(i,:));
%plot(reclocation(j,:),shotpick(j,:));
%xlabel('rec&shot location along the survey line (m)');
%ylabel('traveltime (ms)');
%title('Refracted P-wave picks for shots #i & j');
figure('menubar','none');
hold;
plot(reclocation(i,:),pickuphole(i,:));
plot(reclocation(j,:),pickuphole(j,:));
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime (ms)');
title('Refracted P-wave picks for shots #i & j');
figure('menubar','none');
x1 = start1:step:end1;
plot(x1,tdiff1,'y'); hold on;
x2 = start2:step:end2;
plot(x2, tdiff2, 'g'); hold off;
%Median filter
window=input('What is the median filter window length?  ');
[m,n]=size(tdiff1);
for a=1:m-window;
   mddiff1(a)=median(tdiff1(a:a+window));
   mdloc(a)=median(x1(a:a+window));
end
[m,n]=size(tdiff2);
for a=1:m-window;
   mddiff2(a)=median(tdiff2(a:a+window));
   mdlc(a)=median(x2(a:a+window));
end
figure('menubar','none');
plot(mdloc,mddiff1,'y');hold on;
plot(mdlc,mddiff2,'g');hold off;
%Derivative of the traveltime difference
[m,n]=size(mdloc);
%delx1=mdloc(1:n-2)-mdloc(3:n);
delt1=mddiff1(1:n-2)-mddiff1(3:n);  
avgmdloc=(mdloc(1:n-2)+mdloc(3:n))/2;
%deriv1=delx1./delt1(:,1);
%absderiv1=abs(deriv1);                     
dt1 = clip(delt1,1);
[m,n]=size(mdlc);
%delx2=mdlc(1:n-2)-mdlc(3:n);
delt2=mddiff2(1:n-2)-mddiff2(3:n);  
avgmdlc=(mdlc(1:n-2)+mdlc(3:n))/2;
%deriv2=delx2./delt2(:,1);
%absderiv2=abs(deriv2);
dt2 = clip(delt2,1);
figure('menubar','none');
plot(avgmdloc,dt1,'y');hold on;
plot(avgmdlc,dt2,'g');hold off;
xlabel('location (m)')
ylabel('derivative of the time difference (1000 m/s2)')
title('First derivative of the time difference between shots #i-j')
%figure;
%hold;
%    for i=1:188;
%	plot(abstdiff(i,:));
%    end;
%xlabel('shot number');
%ylabel('traveltime difference(ms)');
%title('Traveltime difference ');