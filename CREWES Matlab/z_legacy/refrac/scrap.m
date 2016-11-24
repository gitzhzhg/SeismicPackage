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

figure('menubar','none');
hold;
plot(reclocation(185,:),shotpick(185,:));
plot(reclocation(160,:),shotpick(160,:));
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime (ms)');
title('Refracted P-wave picks for shots #160 & 185');
figure('menubar','none');
hold;
plot(reclocation(185,:),pickuphole(185,:));
plot(reclocation(160,:),pickuphole(160,:));
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime (ms)');
title('Refracted P-wave picks with uphole time for shots #160 & 185');
%pick2=shotpick(185,:);
%m2=min(pick2);
%n2=min(find(pick2==m2));
%find(reclocation(160,:)==reclocation(185,n2));
%t2=interp1(reclocation(160,:),shotpick(160,:),reclocation(185,n2),'linear')
%find(reclocation(160,:)==r(185));
%pick1=shotpick(160,:);
%m1=min(pick1);
%n1=find(pick1==m1);
%find(reclocation(185,:)==reclocation(160,n1));
%t1=interp1(reclocation(185,:),shotpick(185,:),reclocation(160,n1),'linear')
%find(reclocation(185,:)==reclocation(160,r(160));
%check reciprocal time
t1=interp1(reclocation(185,:),shotpick(185,:),r(160),'linear');
t3=interp1(reclocation(185,:),pickuphole(185,:),r(160),'linear');
t2=interp1(reclocation(160,:),shotpick(160,:),r(185),'linear');
t4=interp1(reclocation(160,:),pickuphole(160,:),r(185),'linear');
ta=t1-t2
tb=t3-t4
%traveltime difference between adjacent shot records;
endp1x=max(reclocation(160,:));
endp2x=max(reclocation(185,:));
endx = min(endp1x, endp2x);
startp1x=min(reclocation(160,:));
startp2x=min(reclocation(185,:));
startx = max(startp1x, startp2x);
step=1;
xloc=startx:step:endx;                                         
p1=interp1(reclocation(160,:),shotpick(160,:),xloc,'linear');
p2=interp1(reclocation(185,:),shotpick(185,:),xloc,'linear');
diff1=p2-p1;
p3=interp1(reclocation(160,:),pickuphole(160,:),xloc,'linear');
p4=interp1(reclocation(185,:),pickuphole(185,:),xloc,'linear');
diff2=p4-p3;
figure('menubar','none');
plot(xloc,diff1);
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime difference (ms)');
title('Traveltime difference between refracted P-wave picks of shots 185 and 160');
figure('menubar','none');
plot(xloc,diff2);
xlabel('rec&shot location along the survey line (m)');
ylabel('traveltime difference (ms)');
title('Traveltime difference between refracted P-wave picks with uphole time of shots 185 and 160');
[m,n]=size(diff1);
for i=1:3:m-1;
   mddiff1(i)=median(diff1(i:i+3));
   mdloc(i)=median(xloc(i:i+3));
end
figure('menubar','none')
plot(mdloc(i),mddiff1(i),'o')
[m,n]=size(xloc);
delx=xloc(1:n-2)-xloc(3:n);
delt=diff1(1:n-2)-diff1(3:n);  
avgloc=(xloc(1:n-2)+xloc(3:n))/2;
deriv=2./delt(:,1);
absderiv=abs(deriv);
figure('menubar','none')
plot(avgloc,deriv,'o');
axis([5656000,5660000,-20000,20000]);                     
xlabel('offset (m)')
ylabel('velocity (1000 m/s)')
title('First derivative of the P-wave  picks for shots #1-189')