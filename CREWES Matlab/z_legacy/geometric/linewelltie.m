function linewelltie(action) 
% function linewelltie(action) 
% LINEWELLTIE is called by seis2well(which finds out the well and 
%    seis file names, then plots out the data) 
% LINEWELLTIE has routines for tying the wells to the seismic
% 
% ACTION can be 'init', 'fini', 'output' (file output), 
%  'radius' (define radius of circle), 'setradius' (called by 'radius'),
%  'azimuth' (define azimuth orientation), 'setazimuth' (called by 'azimuth'),
%  'doit' (compute line to well ties)
%
%  T.N. Bishop, December 1993, CPTC Canada
%
% see also: closetrace, closetraceazi, drawazi, drawazifini, 
%     drawaziinit, drawline, drawlinefini, drawlineinit, 
%     plotseiswell, selcirc, selcircfini,
%     selcircinit, twoptcircle, seis2well, readSeislineVec
%
%  a word about handles,
%
% get(gcf), 5 children, 9 userdata
%  hc = get(gcf,'children')
%  hc(1) , type=axes, 9 children
%   hcc = get(hc(1),'children')
%   hcc(1), type=line, xor, -., [0 1 0], (GREEN LINE TIE)
%   hcc(2), type=text, xor, [.5 .5 .5], string= az=174
%   hcc(3), type=line, xor, -, [.5 .5 .5], (AZIMUTH LINE)
%   hcc(4), type=text, xor, [.5 .5 .5], string= r=4.5
%   hcc(5), type=line, xor, -, [.5 .5 .5], (THE CIRCLE)
%   hcc(6), type=line, *, [1 1 0], 1 userdata,  (THE WELLS)
%   hcc(7), type=line, normal,-, [0 1 1], userdata=3, (LINE1)
%   hcc(8), type=line, normal,-, [0 1 1], userdata=2, (LINE2)
%   hcc(9), type=line, normal,-, [0 1 1], userdata=1, (LINE3)
%  note that userdata=1 for LINE3 is the index of the line
%  hc(2) , type=uicontrol, style=text, string=label
%  hc(3) , type=uicontrol, style=text, string=mot2, 1 userdata
%   hcd = get(hc(3),'userdata')
%   hcd(1), type=line, xor, -., [0 1 0], (GREEN LINE TIE)
%  hc(4) , type=uicontrol, style=text, string=mot, 2 userdata
%   hce = get(hc(4),'userdata')
%   hce(1),hce(2) = 4.8, 162, the RADIUS and AZIMUTH
%  hc(5) , type=uimenu, 8 children, 1 userdata
%   hcf = get(hc(5),'children')
%   hcf(1), type=uimenu, label=quit...etc... to...
%   hcf(8), type=uimenu, label=read well...
%   hcg = get(hc(5),'userdata')
%   hcg is the CONTAINER OBJECT, wells and seis, 2860x1 )
%  hu = get(gcf,'userdata')
%  hu(1) , type=uimenu, userdata='wells.dat',label=read wells
%  hu(2) , type=uimenu, userdata='seis.dat',label=read seis
%  hu(3) , type=uimenu, label=plot...
%  hu(4) , type=uimenu, label=set radius
%  hu(5) , type=uimenu, label=azim...
%  hu(6) , type=uimenu, label=doit
%  hu(7) , type=uimenu, label=output
%  hu(8) , type=uimenu, label=quit
%  hu(9) , type=uimenu, string='well to seis shown...' (LABEL)
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
if (strcmp(action,'init')) 
  hstore = uicontrol('style','text','string','mot','visible','off');
  hstore2 = uicontrol('style','text','string','mot2','visible','off');
  return
end
if (strcmp(action,'fini')) 
	% get the storage bucket
          h = get(gcf,'children');
          icount = 0;
          for (k = 1:length(h))
		type=get(h(k),'type');
		if(strcmp(type,'uicontrol'))
		    style = get(h(k),'style');
		    if(strcmp(style,'text'))
		      s = get(h(k),'string');
		      if(strcmp(s,'mot'))
			hstore = h(k);
			icount = icount + 1;
		      end
		      if(strcmp(s,'mot2'))
			hstore2 = h(k);
			icount = icount + 1;
		      end
			if(icount == 2)
			  break;
			end
		    end
	 	end
	 end
      delete(hstore);
      delete(hstore2);
end
if (strcmp(action,'output')) 
	% axes object is parent to line objects
          hh = get(gcf,'children');
          for (k = 1:length(hh))
            type=get(hh(k),'type');
            if(strcmp(type,'axes'))
              h = get(hh(k),'children');
            end
          end
       % well object is parent to userdata which holds
       %   vector of well, line,  and trace indicies of tie points
       %    for all lines
       % line object is parent to userdata which holds
       %    index of line index for that line
          hlines = [];   
          hwells = [];   
          for (k = 1:length(h))
            type=get(h(k),'type');
            if(strcmp(type,'line'))
              mode=get(h(k),'erasemode');
              if(strcmp(mode,'normal'))
                style = get(h(k),'linestyle');
                mstyle = get(h(k),'marker');
                if ( strcmp(style,'-')  | strcmp(style,':') |...
                     strcmp(style,'--') | strcmp(style,'-.') )
                  hlines=[hlines h(k)];
		end
		if (strcmp(mstyle,'*'))
		  hwells=[hwells h(k)];
		end
	      end
	    end
          end
          nlines = length(hlines);
%   uimenu object is parent to userdata which
%      holds the well and seis objects
          for k = 1:length(hh)
            type = get(hh(k),'type');
            if strcmp(type,'uimenu')
              freds = get(hh(k),'userdata');
            end
          end
%   freds is a container object which holds the
%      wellobj and the seisobj
%      wellobj 
	objget(freds,'contents');
        wellobj = objget(freds,1);
        wellname = objget(wellobj,'contents');
        [nw,nchw] = size(wellname);
        for k = 1:nw
          welltmp = objget(wellobj,k);
          xw = [xw,welltmp(1)];
          yw = [yw,welltmp(2)];
        end
%      seisobj
        seisobj = objget(freds,2);
        [nl,nchl] = size(objget(seisobj,'contents'));
%  NOW WRITE OUT INTO FILE
  [outf,outp]=uiputfile('*.tie','output file name',100,100);
  disp(['output file is',outp,outf]);
  fwriteid = fopen([outp,outf],'w');
%      now loop thru the ties and output data
	hind = get(hwells(1),'userdata');
        nties = length(hind)/3;
	if nties > 0
	  ind = reshape (hind, 3, nties);
	  for m = 1:nties
            indw =  ind(1,m);   %well index of tie
            indlnplot = ind(2,m);   %line index of tie
%   BUT, line index is function of when line was plotted,
%     so must go to handle of line to get actual line index
            indln = get(hlines(indlnplot),'userdata');
            indtr = ind(3,m);   %trace no of tie
            seistmp = objget(seisobj,indln);
            linename = objget(seistmp,'name');
            xtr = objget(seistmp,1);
            ytr = objget(seistmp,2);
            trno = objget(seistmp,3);
            z1tr = objget(seistmp,4);
            z2tr = objget(seistmp,5);
            z3tr = objget(seistmp,6);
            z4tr = objget(seistmp,7);
            z5tr = objget(seistmp,8);
            z6tr = objget(seistmp,9);
	    tiedist = sqrt( (xw(indw)-xtr(indtr)).^2 +...
	                    (yw(indw)-ytr(indtr)).^2 );
	    tieazi = atan2( (ytr(indtr)-yw(indw)),...
	                    (xtr(indtr)-xw(indw)) ) * 180./pi;
%  convert from math angle to compass angle, where n=0, e=90...
	    azitxt = -tieazi + 90;
	    if (azitxt < 0)
	      azitxt = azitxt + 360;
	    end
%  force names of wells and seis to be same size
	    wtmp = [wellname(indw,:),'            '];  %seisname has 12 char
	    wtmp = [wtmp(abs(wtmp)~=1),'            '];
	    stmp = [linename,'        '];  %linename has 8 char
	    stmp = [stmp(abs(stmp)~=1),'        '];
%
            fprintf(...
 '%.8s %7.2f %.12s%6.2f%6.0f%7.1f%7.1f%7.1f%7.1f%7.1f%7.1f \n',...
              stmp(1:8), trno(indtr),...
              wtmp(1:12),...
              tiedist, azitxt,...
              z1tr(indtr), z2tr(indtr),...
              z3tr(indtr), z4tr(indtr),...
              z5tr(indtr), z6tr(indtr) );
            fprintf(fwriteid,...
 '%.8s %7.2f %.12s%6.2f%6.0f%7.1f%7.1f%7.1f%7.1f%7.1f%7.1f \n',...
              stmp(1:8), trno(indtr),...
              wtmp(1:12),...
              tiedist, azitxt,...
              z1tr(indtr), z2tr(indtr),...
              z3tr(indtr), z4tr(indtr),...
              z5tr(indtr), z6tr(indtr) );
 	  end
	  disp([' output complete, number of ties = ',num2str(nties)]);
	end
  status = fclose(fwriteid);
%  NOW WRITE OUT INTO FILE
%  [outf,outp]=uiputfile('*.tie','output file name',100,100)
%  fwriteid = fopen([outp,outf],'w')
%  for i =  1:(length(xxx)-1)
%    fprintf(fwriteid,'%d %6.2f %6.2f \n',i,xxx(i),yyy(i));
%  end
%  status = fclose(fwriteid);
end
if (strcmp(action,'radius'))
%   first erase old circle, if there
	hc = get(gcf,'children');
	for (j = 1:length(hc))
	  type = get(hc(j),'type');
	  if(strcmp(type,'axes'))
	     hcc = get(hc(j),'children');
             for (k = 1:length(hcc))
               mode=get(hcc(k),'erasemode');
               color=get(hcc(k),'color');
               if(strcmp(mode,'xor') & ...
		  strcmp(color,[.5 .5 .5])) %azimuth or circle
                 typec=get(hcc(k),'type');
                 if(strcmp(typec,'line')) 
		   if(length(get(hcc(k),'xdata'))==31)
		      delete(hcc(k))   %circle line
		   end
		 end
                 if(strcmp(typec,'text')) 
		   strc = get(hcc(k),'string');
		   if(strc(1) == 'r')
		      delete(hcc(k))  %circle text
		   end
		 end
	       end
	     end
	  end
	end
 
  selcircinit('linewelltie(''setradius'')');
  
  return
end 
if (strcmp(action,'setradius'))
	  h = get(gcf,'children');
	  for (k = 1:length(h))
		type=get(h(k),'type');
		if(strcmp(type,'uicontrol'))
		    style = get(h(k),'style');
		    if(strcmp(style,'text'))
		      s = get(h(k),'string');
		      if(strcmp(s,'mot'))
			hstore = h(k);
			break;
		      end
		    end
	        end
	  end
	 stuff=selcircfini;
	 if(length(stuff)<3)
		disp('Invalid radius!! Try again');
		return;
	end
 
	 rd=stuff(3);
         disp(['valid radius, r =  ',num2str(rd)]);
 
	 dat=get(hstore,'userdata');
 
	 if( length(dat)>0 )
		 dat(1)=rd;
         else
                 dat = rd;
	 end
 
	 set(hstore,'userdata',dat);
	return;
end
if (strcmp(action,'azimuth'))
%   first erase old azimuth, if there
	hc = get(gcf,'children');
	for (j = 1:length(hc))
	  type = get(hc(j),'type');
	  if(strcmp(type,'axes'))
	     hcc = get(hc(j),'children');
             for (k = 1:length(hcc))
               mode=get(hcc(k),'erasemode');
               color=get(hcc(k),'color');
               if(strcmp(mode,'xor') & ...
		  strcmp(color,[.5 .5 .5])) %azimuth or circle
                 typec=get(hcc(k),'type');
                 if(strcmp(typec,'line')) 
		   if(length(get(hcc(k),'xdata'))==2)
		      delete(hcc(k))   %azimuth line
		   end
		 end
                 if(strcmp(typec,'text')) 
		   strc = get(hcc(k),'string');
		   if(strc(1) == 'a')
		      delete(hcc(k))  %azimuth text
		   end
		 end
	       end
	     end
	  end
	end
	drawaziinit('linewelltie(''setazimuth'')');
	return;
end
if( strcmp(action,'setazimuth') )
          h = get(gcf,'children');
          for (k = 1:length(h))
		type=get(h(k),'type');
		if(strcmp(type,'uicontrol'))
		    style = get(h(k),'style');
		    if(strcmp(style,'text'))
		      s = get(h(k),'string');
		      if(strcmp(s,'mot'))
			hstore = h(k);
			break;
		      end
		    end
		end
	end
	stuff=drawazifini;
	if length(stuff) < 3  % i.e., just a click,no drag
          stuff = zeros(1,3);
          stuff(3) = -999;
          azi = stuff(3);
          disp('azimuth turned off, will now take closest trace');
        else
          azi = stuff(3);
%  convert from math angle to compass angle, where n=0, e=90...
          azitxt = -azi + 90;
          if (azitxt < 0)
            azitxt = azitxt + 360;
          end
          disp(['valid azimuth, angle =  ',num2str(azitxt)]);
        end
	% get the radius and azimuth
	 dat=get(hstore,'userdata');
	 if( length(dat)>0 )
		 dat(2)=azi;
         else
                 dat = zeros(1,2);
		 dat(2)=azi;
	 end
 
	 set(hstore,'userdata',dat);
	return;
end
if( strcmp(action,'doit') )
	% get the storage bucket
          h = get(gcf,'children');
          icount = 0;
          for (k = 1:length(h))
		type=get(h(k),'type');
		if(strcmp(type,'uicontrol'))
		    style = get(h(k),'style');
		    if(strcmp(style,'text'))
		      s = get(h(k),'string');
		      if(strcmp(s,'mot'))
			hstore = h(k);
			icount = icount + 1;
		      end
		      if(strcmp(s,'mot2'))
			hstore2 = h(k);
			icount = icount + 1;
		      end
			if(icount == 2)
			  break;
			end
		    end
	 	end
	 end
	% get the radius and azimuth
	dat=get(hstore,'userdata');
	if(length(dat)==0) 
          disp('Warning, projection radius must be defined');
          disp('         before tying wells to seismic');
          return;
        end
	rd=dat(1);
	if( length(dat)>1 )
		azi=dat(2);
	else
		azi=-999;
	end
	% get the axes children
	h=get(gca,'children');
	  hlines=[];
	  hwells=[];
	% search the children for lines. Connected lines are seismic lines
	% while symbols mean wells
	for(k = 1:length(h))
  	  type = get(h(k),'type');
	  if (strcmp(type,'line'))
 	    mode = get(h(k),'erasemode');
   	    if(strcmp(mode,'normal'))
		    style = get(h(k),'linestyle');
		    mstyle= get(h(k),'marker');
		    color = get(h(k),'color');
		    if (strcmp(style,'-') | strcmp(style,':') |...
			strcmp(style,'--') | strcmp(style,'-.') )
			hlines=[hlines h(k)];
		    end
		    if (strcmp(mstyle,'*'))
			hwells=[hwells h(k)];
		    end
		    if (strcmp(mstyle,'+') & strcmp(color,[1 0 0]) )
		        delete(h(k));     %red cross symbol
            	    end
	    end
	 end
	end
	nlines = length(hlines);
	xwells = [];
	ywells = [];
	nwells = 0;
    	for (k = 1:length(hwells))
	  tmp = get(hwells(k),'xdata');
	  xwells = [xwells tmp];
	  tmp = get(hwells(k),'ydata');
	  ywells = [ywells tmp];
	end
	nwells = length(xwells);
%  FIND WELL TO SEISMIC MATCHES THAT ARE WITHIN RADIUS
%  first delete previous green line plots
hgreen = get(hstore2,'userdata');
if length(hgreen) > 0
  for k = 1:length(hgreen)
    delete(hgreen(k));
  end
end
hgreen = [];
indvec = [];
%  make sure well is in zoom area
xzoom = get(gca,'xlim');
yzoom = get(gca,'ylim');
for il = 1:nlines
  xxx = get(hlines(il),'xdata');
  yyy = get(hlines(il),'ydata');
  for iw = 1:nwells
    if ~isempty(between(xzoom(1),xzoom(2),xwells(iw),2))
      if ~isempty(between(yzoom(1),yzoom(2),ywells(iw),2))
        if azi == -999    %no azimuth was set
          ind = closetrace(xxx,yyy,xwells(iw),ywells(iw),rd);
        else
          ind = closetraceazi(xxx,yyy,xwells(iw),ywells(iw),rd,azi);
        end
        if ~isempty(ind)   %have valid point within the radius
          xnear = [xxx(ind) xwells(iw)];
          ynear = [yyy(ind) ywells(iw)];
          hgreen = [hgreen , line(xnear,ynear,'color','g',...
                'erasemode','xor','linestyle','-.')];
          hred = line(xxx(ind),yyy(ind),'linestyle','+','color','r');
%         PRINT OUT DESIRED FILE
          fprintf ('well at %6.2f %6.2f ,trace at %6.2f %6.2f \n',...
               xwells(iw),ywells(iw),xxx(ind),yyy(ind))
          indvec = [indvec,iw,il,ind];
        end
      end
    end
  end
end
set(hwells(1),'userdata',indvec);  %put in well handle
set(hstore2,'userdata',hgreen);
disp('finished computing and plotting ties');
end