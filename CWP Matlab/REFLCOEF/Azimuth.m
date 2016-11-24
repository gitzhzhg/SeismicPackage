%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Azimuthal dependence of Rps coefficients
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

% setup some constants: xmax=max inc. angle, ymax= max. azimuth

xmax=40; %max. inc. angle to be included
xmin=0;  %min. inc angle to be concluded
dinc=1;   %increment in inc.angle plots

ymax=90; %max. azimuth angle to be concluded
ymin=0;  %min. azimuth angle to be concluded
daz=1;   %increment in azimuth
ord1=31;  %the ordinary number of the azimuth to be plotted
ord2=4;
ord3=5;
ord4=6;
ord5=7;

zmin=-0.2; %for 3-D plot
zmax=0;

ninc=3; %number of incidence angles to be computed (maximum is five)
n=10; %number of color fields or contours in error maps
SW=2; %=1 RELative error plot
      %=2 ABSolute error plot
RND=4; % maximum number of decimal numbers in the 
       % error evaluation contour graphs

% setup screens display:

set(0,'Units','centimeters');
%scnsize=get(0,'ScreenSize');

pos1=[520. 350. 700 600];
pos2=[490 280 700 600];
pos3=[460 210 700 600];
pos4=[430 140 700 600];
pos5=[400 70 700 600];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get data from Vlad's exact code: 
% the model file should be pre-processed in
% order to fix edges and limit the inc. angles

load PLOTS/SVSH.out; %sv-sh output file containing all the data needed


 % exact data S1 and S2:
 %EPP=model(:,5); P data

ERS1=SVSH(:,3);
ERS2=SVSH(:,4);

 % approximate data S1 and S2:

ARS1=SVSH(:,9);
ARS2=SVSH(:,10);

 % axes of inc. angle (x-axis) and azimuth (y-axis):

EX=SVSH(:,1);
EY=SVSH(:,2);

% axes of inc. angle and azimuth from approximate data; this is a
% formal operation; it can be used if approx. data are computed for
% a different pairs (inc.angle, azimuth):

AX=SVSH(:,1);
AY=SVSH(:,2);

% re-grid the ununiform data on a uniform and identical grid: 

dx=xmin:dinc:xmax;
dy=ymin:daz:ymax;
[XI,YI]=meshgrid(dx,dy);           %this sets up the regular grid%

EZS1=griddata(EX,EY,ERS1,XI,YI);   %this interpolates on the grid%
EZS2=griddata(EX,EY,ERS2,XI,YI);
AZS1=griddata(AX,AY,ARS1,XI,YI);
AZS2=griddata(AX,AY,ARS2,XI,YI);


% Now, plot the 2-D plots R(azim., inc.ang=CONST) with both surfaces
% and interpolated points

AZaxis=YI(:,1);
ES1axis=EZS1(:,ord1);
AS1axis=AZS1(:,ord1);
figure('Position',pos1);
plot(AZaxis,ES1axis);
hold;
plot(AZaxis,AS1axis);

fprintf(['incidence angle i=%7.2f(deg) \n'], XI(1,ord1));

figure('Position',pos2);
surf(XI,YI,EZS1);
hold
surf(XI,YI,AZS1);
% if you want both data sets in one
%hold;
%surf(XI,YI,AZS1);
%plot3(EX,EY,ERS1,'o');
axis([0 xmax 0 ymax zmin zmax]);

%%%%%%%%%%%%%%%%
% SV WAVE  EXACT
%%%%%%%%%%%%%%%

%m1=min(min(EZS1));
%m2=max(max(EZS1));
%dm=(m2-m1)/n;
%for i=0:n-1
%  a(i+1)=m1+i*dm;
%end
%b=[1 2];
%c=[a; a];
%factor=10^RND;

%figure('Position',pos1);
%axes('position',[.15 .2 .8 .5]);
%aa=(round(a*factor))/factor;
%[C,h]=contourf(XI,YI,EZS1,aa,'k-');
%set(h, 'LineWidth', 2);
%T=clabel(C,h);
%set(T,'Color','k','FontSize',15,'FontWeight','bold');
%xlabel('Incidence angle (deg)', 'FontSize', 20,'FontWeight','bold','Position',[26 -9 17]);
%ylabel('Azimuth (deg)', 'FontSize', 20, 'FontWeight','bold','Position',[-4 44 17]);
%title('Exact RSV component', 'FontSize', 25,'FontWeight','bold','Position',[26 93 17]);

% makes the colorbar

%colorbar('horiz');
%set(gca,'position',[.15 .33 .8 .45]);
% colorbar; would create a vertical bar; not too bad either

% setting up colormap: this sets gray, and assignes smallest values to
% lightest colours (white)

%colormap(gray);
%h=colormap;
%hh=1-h;
%colormap(hh);
%colormap(h);

%%%%%%%%%%%%%%%%
% SH WAVE  EXACT
%%%%%%%%%%%%%%%

%m1=min(min(EZS2));
%m2=max(max(EZS2));
%dm=(m2-m1)/n;
%for i=0:n-1
%  a(i+1)=m1+i*dm;
%end
%b=[1 2];
%c=[a; a];
%factor=10^RND;

%figure('Position',pos1);
%axes('position',[.15 .2 .8 .5]);
%aa=(round(a*factor))/factor;
%[C,h]=contourf(XI,YI,EZS2,aa,'k-');
%set(h, 'LineWidth', 2);
%T=clabel(C,h);
%set(T,'Color','k','FontSize',15,'FontWeight','bold');
%xlabel('Incidence angle (deg)', 'FontSize', 20,'FontWeight','bold','Position',[20 -10 17]);
%ylabel('Azimuth (deg)', 'FontSize', 20, 'FontWeight','bold','Position',[-4 44 17]);
%title('Exact RSH component', 'FontSize', 25,'FontWeight','bold','Position',[20 93 17]);

% makes the colorbar

%colorbar('horiz');
%set(gca,'position',[.15 .33 .8 .45]);
% colorbar; would create a vertical bar; not too bad either

% setting up colormap: this sets gray, and assignes smallest values to
% lightest colours (white)

%colormap(gray);
%h=colormap;
%hh=1-h;
%colormap(hh);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now, do the error analyses:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ABS1=abs(EZS1-AZS1);
%MX=max(abs(EZS1),abs(AZS1))+0.000001;
%REL1=(ABS1./MX)*100;

%if SW==1
%  ERR1=REL1;
%elseif SW==2
%  ERR1=ABS1;
%end

%ABS2=abs(EZS2-AZS2);
%MX=max(abs(EZS2),abs(AZS2))+0.000001;
%REL2=(ABS2./MX)*100;

%if SW==1
%  ERR2=REL2;
%elseif SW==2
%  ERR2=ABS2;
%end

%%%%%%%%%%%%%%%%%%%%
% SV error component
%%%%%%%%%%%%%%%%%%%%

% ... range of colors and contours ...

%m1=min(min(ERR1));
%m2=max(max(ERR1));
%dm=(m2-m1)/n;
%for i=0:n-1
%  a(i+1)=m1+i*dm;
%end
%b=[1 2];
%c=[a; a];
%factor=10^RND;

%... color 2-D plot
%figure('Position',pos2);
%axes('position',[.1 .1 .8 .6]);
%surface(XI,YI,ERR,'LineStyle','none');
%xlabel('Phase angle of incidence (deg)', 'FontSize', 16,'FontWeight','bold');
%ylabel('Phase azimuth (deg)', 'FontSize', 16, 'FontWeight','bold');
%axes('Ytick',[ ],'position',[.1 .8 .8 .1]);
%pcolor(a,b,c);
%set(gca,'YTick',[]);
%title('Error pseudocolor map', 'FontSize', 20);

%...contour 2-D plot

%figure('Position',pos3);
%axes('position',[.15 .2 .8 .5]);
%aa=(round(a*factor))/factor;
%[C,h]=contourf(XI,YI,ERR1,aa,'k-');
%set(h, 'LineWidth', 2);
%T=clabel(C,h);
%set(T,'Color','k','FontSize',15,'FontWeight','bold');
%xlabel('Incidence angle (deg)', 'FontSize', 20,'FontWeight','bold','Position',[20 -10 17]);
%%ylabel('Azimuth (deg)', 'FontSize', 20, 'FontWeight','bold','Position',[-5 44 17]);
%title('RSV absolute error', 'FontSize', 25,'FontWeight','bold','Position',[20 93 17]);%


% makes the colorbar

%colorbar('horiz');
%set(gca,'position',[.15 .33 .8 .45]);
% colorbar; would create a vertical bar; not too bad either

% setting up colormap: this sets gray, and assignes smallest values to
% lightest colours (white)

%colormap(gray);
%h=colormap;
%hh=1-h;
%colormap(hh);

%%%%%%%%%%%%%%%%%%%%
% SH error component
%%%%%%%%%%%%%%%%%%%%

%m1=min(min(ERR2));
%m2=max(max(ERR2));
%dm=(m2-m1)/n;
%for i=0:n-1
%  a(i+1)=m1+i*dm;
%end
%b=[1 2];
%c=[a; a];
%factor=10^RND;

% 2-D plot

%figure('Position',pos4);
%axes('position',[.15 .2 .8 .5]);
%aa=(round(a*factor))/factor;
%[C,h]=contourf(XI,YI,ERR2,aa,'k-');
%set(h, 'LineWidth', 2);
%T=clabel(C,h);
%set(T,'Color','k','FontSize',15,'FontWeight','bold');
%xlabel('Incidence angle (deg)', 'FontSize', 20,'FontWeight','bold','Position',[20 -10 17]);
%%ylabel('Azimuth (deg)', 'FontSize', 20, 'FontWeight','bold','Position',[-5 44 17]);
%title('RSH absolute error', 'FontSize', 25,'FontWeight','bold','Position',[20 93 17]);

% makes the colorbar

%colorbar('horiz');
%set(gca,'position',[.15 .33 .8 .45]);
% colorbar; would create a vertical bar; not too bad either

% setting up colormap: this sets gray, and assignes smallest values to
% lightest colours (white)

%colormap(gray);
%h=colormap;
%hh=1-h;
%colormap(hh);


%title('Error pseudocolor map', 'FontSize', 20);

%%...3-D plot
%figure('Position',pos4);
%grid;
%surface(XI,YI,ERR);
%view(3);
%xlabel('Phase angle of incidence (deg)', 'FontSize', 16,'FontWeight','bold');
%ylabel('Phase azimuth (deg)', 'FontSize', 16, 'FontWeight','bold');
%zlabel('Error','FontSize', 16,'FontWeight','bold');
%title('3-D error map', 'FontSize', 20);

%%%%%%%%%%%%%%%%%%%%%%%%%
%  END OF STORY
%%%%%%%%%%%%%%%%%%%%%%%%%
