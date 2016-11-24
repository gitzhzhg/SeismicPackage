function plotseiswell(wellfile,seisfile)
% function plotseiswell(wellfile,seisfile)
%     This function is the workhorse of seis2well.
%     The two input filenames are the output
%       from readwells (x,y,wellname stuff)
%       and from extract_raster (x,y,trno, t1,t2...)
%     These files are ascii files which can be edited.
%     This function reads the files, makes local
%       coordinates, makes well and seisobjects
%       as specified by G. Margraves architecture,
%       plots the lines and wells 
%     Note, the wells are one object, and each of
%       the lines are one object.  The lines are
%       combined into one container object, and
%       then the wells and seis are combined into
%       a master container object, which is assigned
%       a handle.
%
%	T.N.Bishop,  December 1993, CPTC Canada
%   see also seis2well, linewelltie
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

%  READ IN DATA 
%
% read in ascii well file (from seisline, originally)
disp('beginning to read the wellfile');
[x,y,name]=readwellfile(wellfile);
disp('finished reading the wellfile');
% The global coordinates are in x and y. 
%
% 
% read in ascii seis file (from seisline, originally)
disp('beginning to read the seisfile');
[lineno,tr,xtr,ytr,z1,z2,z3,z4,z5,z6]=readseisfile(seisfile);
disp('finished reading the seisfile');
% The global coordinates are in xtr and ytr. 
%
% MAKE LOCAL COORDINATES
%
% xr and yr contain local well coordinates. 
% xtrr and ytrr contain local trace coordinates. 
% xr=(x-min(x))/1000. etc.
%

xmin=min(min(x),min(xtr));
ymin=min(min(y),min(ytr));
xr = (x-xmin)/1000.;
yr = (y-ymin)/1000.;
xtrr = (xtr-xmin)/1000.;
ytrr = (ytr-ymin)/1000.;
%
% MAKE WELL OBJECT
disp('beginning to make the well object');
% now load up an object
 len=find(name(1,:)==0);
fredswells=[];   %start from scratch each time
fredswells=randobj('Freds Wells',[xr(1) yr(1)],name(1,1:len));
disp(['adding well ',name(1,1:len)]);
for k=2:length(xr)
    len=find(name(k,:)==0);
    fredswells=objset(fredswells,name(k,1:len),[xr(k) yr(k)]);
    disp(['adding well ',name(k,1:len)]);
end
% put the global coordinate info in
fredswells=objset(fredswells,'globorigin',[min(x) min(y)]);
fredswells=objset(fredswells,'xyscale',1000);
disp('finished making the well object');
disp(['there are ',num2str(length(xr)),' wells']);
%
% MAKE SEISMIC LINE (RAND) OBJECT
[lmin,lmax,nlines] = split(lineno);
disp('beginning to make the seis object');
disp(['there are ',num2str(nlines),' lines']);
% make line objects
for i = 1:nlines
  ic = num2str(i);
  [nrows,nchar] = size(lineno);
  lineic = lineno(lmin(i),1:nchar);
  disp(['adding line ',lineic]);
  linen=randobj( lineic ,tr(lmin(i):lmax(i)),...
   'traceno',xtrr(lmin(i):lmax(i)),ytrr(lmin(i):lmax(i)),'line');
  linen=objset(linen,'time 1',z1(lmin(i):lmax(i)));
  linen=objset(linen,'time 2',z2(lmin(i):lmax(i)));
  linen=objset(linen,'time 3',z3(lmin(i):lmax(i)));
  linen=objset(linen,'time 4',z4(lmin(i):lmax(i)));
  linen=objset(linen,'time 5',z5(lmin(i):lmax(i)));
  linen=objset(linen,'time 6',z6(lmin(i):lmax(i)));
% make the container objset (contains many lines, each a randobj)
	if( i==1 )
		fredslines = [];   %again, start from scratch
		fredslines=contobj('Freds lines',linen,...
            'Freddies #1');
        else
          ic = num2str(i);
          fredslines = objset(fredslines,['Freddies #' ic],...
             linen);
        end
end
disp('finished making the seis object');
% MAKE A PROJECT (LINES AND WELLS) CONTAINER OBJECT
	fredsproject = [];   %again, start from scratch
	fredsproject = contobj('Freds project',fredswells,...
           'the well object');
        fredsproject = objset(fredsproject,'the seis object',...
           fredslines);
%  store it in userdata of the menu
hch = get(gcf,'children');
for k = 1:length(hch)
  type = get(hch(k),'type');
  if strcmp(type,'uimenu')
    set(hch(k),'userdata',fredsproject);
  end
end
%  PLOT LINES AND WELLS
hold on
% delete previous plot
%hh = get(gcf,'children');
%for k = 1:length(hh)
%  type=get(hh(k),'type');
%  if(strcmp(type,'axes'))
%    h = get(hh(k),'children');
%  end
%end
%for (k = 1:length(h))
%  type=get(h(k),'type');
%  mode=get(h(k),'erasemode');
%  if(strcmp(type,'line') | strcmp(mode,'xor'))   %seislines, wells
%%                         radius, azimuth, accomp.text, green ties
%    delete(h(k));
%  end
%end
objget(fredslines,'contents');
objget(fredslines,'namesmatrix');
for il = 1:nlines
  l1=objget(fredslines,il);
  xxx = objget(l1,1);
  linename = objget(l1,'name');
  yyy = objget(l1,2);
  zzz = objget(l1,3);
  hlc(il) = line(xxx,yyy,'color','c','linestyle','-');
      %note: - is code for lines
  set(hlc(il), 'userdata', il)  %store lineindex in line handle
end
line(xr,yr,'color','y','linestyle','*')  %note: * is code for wells
axis('equal')