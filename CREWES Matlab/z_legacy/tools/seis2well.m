function seis2well(action) 
% function seis2well(action) 
%
% this function ties lines to wells and outputs a file containing
% a list of many columns that can be transferred to spreadsheet
% programs for analysis.
%
% 1. get on dec machine, appropriate directory, if using ACE, open
%    up a blue xterm window
% 2. source *.env  (set the seisline catalog files)
% 3. ~vtnbi/bin/readwells >> filename,
%     you may want to name the filename something.dat, since that 
%     is what seis2well will look for (*.dat).
% 4. ~vtnbi/bin/extract_raster
%       or ~vtnbi/bin/console.dec
%     you may want to rename the filename from fort.2 to
%       something.dat, since that is what seis2well looks for.
%    It is highly recommented to use an increment of 5 or
%       more in extract_raster, to speed things up. 
% 5. xhost + andromeda
% 6. rlogin andromeda
% 7. setenv DISPLAY magpie:0
% 8. cd to appropriate directory
% 9. matlab
% 10. seis2well
% 10a. enter wellfile (from 3 above)
% 10b. enter seisfile (from 4 above)
% 10c. plot
% 10d. define circle
% 10e. define azimuth (optional)
% 10f. doit  
% 10g. output the file, a card file with well and line info
% T.N.Bishop, CPTC Canada, Dec. 1993
%   see also  linewelltie
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
if(nargin < 1) 
  action = 'init';
end
if (strcmp(action,'init')) 
% INITIALIZE THE LINEWELLTIE ROUTINES
  linewelltie('init');
% BUILD MENU
opts = uimenu(gcf,'label','Menu');
hp1 = uimenu(opts,...
       'label','Read well location file',...
        'callback','seis2well(''wellfile'')');
hp2 = uimenu(opts,...
       'label','Read seismic trace file',...
        'callback','seis2well(''seisfile'')');
hp3 = uimenu(opts,...
       'label','Plot wells and seismic',...
        'callback','seis2well(''plot'')');
hp4 = uimenu(opts,...
       'label','Set projection radius',...
        'callback','seis2well(''radius'')');
hp5 = uimenu(opts,...
       'label','Set azimuth (optional)',...
        'callback','seis2well(''azimuth'')');
hp6 = uimenu(opts,...
       'label','Doit',...
        'callback','seis2well(''doit'')');
hp7 = uimenu(opts,...
       'label','Output file',...
        'callback','seis2well(''output'')');
hp8 = uimenu(opts,...
       'label','Zoom',...
        'callback','seis2well(''zoom'')');
hp9 = uimenu(opts,...
       'label','Unzoom',...
        'callback','seis2well(''unzoom'')');
hp10 = uimenu(opts,...
       'label','Help',...
        'callback','seis2well(''help'')');
hp11 = uimenu(opts,...
       'label','Quit',...
        'callback','seis2well(''quit'')');
  %   BUILD MESSAGE 
  hmsg = uicontrol('style','text','position',[6,1,400,20],...
         'string','');
	
  % SAVE THE CONTROL HANDLES
  set(gcf,'userdata',...
          [hp1 hp2 hp3 hp4 hp5 hp6 hp7 hp8 hp9 hp10 hp11 hmsg]);
  return
end
% ENTER A CIRCLE WHICH WILL DEFINE A PROJECTION RADIUS
% EACH LINE WITHIN THIS DISTANCE FROM A WELL WILL BE TIED TO A WELL
% BY EITHER TAKING THE CLOSEST TRACE TO THE WELL OR THE CLOSEST TRACE
% ALONG A GIVEN AZIMUTH
if (strcmp(action,'radius')) 
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(hmsg,'string','enter circle with left puck button');
  set(gcf,'pointer','crosshair');
  linewelltie('radius'); 
  
  set(gcf,'pointer','arrow');
  set(hmsg,'string','projection radius is defined');
  return
end 
% ENTER A LINE SEGMENT WHICH WILL DEFINE AN AZIMUTH TO SEARCH FOR
% A WELL.  IF THE CURSOR IS CLICKED QUICKLY SO THAT A POINT IS ENTERED
% INSTEAD OF A LINE, SEARCH IS FOR CLOSEST TRACE
if (strcmp(action,'azimuth'))
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(hmsg,'string','enter azimuth with left puck button');
  set(gcf,'pointer','crosshair');
  linewelltie('azimuth');
  
  set(gcf,'pointer','arrow');
  set(hmsg,'string','azimuth is defined');
  return;
end
% THIS PLOTS THE WELLS AND SEISMIC LINES, ALSO BUILDS OBJECTS FOR BOTH
if( strcmp(action,'plot') )
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(gcf,'pointer','watch');
  hbuttonw=h(1);  %get element of h which is handle of button
  wfile = get(hbuttonw,'userdata'); %well filename saved as userdata
  if isempty(wfile)
    disp('Warning, well file must be defined before plot');
  end
  hbuttons=h(2);  %get element of h which is handle of button
  sfile = get(hbuttons,'userdata'); %well filename saved as userdata
  if isempty(sfile)
    disp('Warning, seis file must be defined before plot');
  end
  plotseiswell(wfile,sfile);
  set(gcf,'pointer','arrow');
  set(hmsg,'string','wells and seismic plotted');
  return;
end
% THIS FINDS THE TIES FROM WELLS TO LINES, WITHIN THE GIVEN RADIUS
% AND (OPTIONALLY), ALONG THE AZIMUTH.  TIES ARE PLOTTED IN GREEN.
if( strcmp(action,'doit') )
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(gcf,'pointer','watch');
  linewelltie('doit');
  set(hmsg,'string','well to seismic ties shown in green');
  set(gcf,'pointer','arrow');
  return;
end
% THIS OUTPUTS A FILE (USER IS PROMPTED FOR THE NAME).  THE FILE
% HAS WELL AND LINE INFO, AND IS CARD IMAGE FOR EXPORT INTO 
% SPREADSHEETS.
if( strcmp(action,'output') )
  h=get(gcf,'userdata');
  hmsg=h(12);
  linewelltie('output');
  set(hmsg,'string','file output complete');
  return;
end
% READ IN WELLFILE NAME.  THIS IS OUTPUT USING THE PROGRAM
% ~vtnbi/bin/wellfile WHICH READS THE SEISLINE WCAT FILE AND
% OUTPUTS A CARD FILE WITH ONE LINE FOR EACH WIFF FILE, THE
% WELLNAME, X, AND Y COORDINATES.
if( strcmp(action,'wellfile') )
  h=get(gcf,'userdata');    %get handle to button to be used as storage
  hmsg=h(12);
  set(hmsg,'string','enter name of well location file');
  [wfile, wfilepath] = uiputfile('*.dat', 'select well file',100,100);
  disp(['well file is ',wfilepath,wfile]);
  hbuttonw=h(1);  %get element of h which is handle of button
  set(hbuttonw,'userdata',[wfilepath,wfile]); %save filename as userdata
  set(hmsg,'string','well location file is named');
  return;
end
% READ IN SEISFILE NAME.  THIS IS OUTPUT USING THE PROGRAM
% ~vtnbi/bin/extract_raster WHICH READS THE SEISLINE CAT FILE AND
% OUTPUTS A CARD FILE WITH ONE LINE FOR EACH TRACE OF EACH LINE
% IN THE CATALOG FILE.  IT HAS LINENAME, TRNO., X, Y, 6 TIME FIELDS.
% ALTERNATIVELY, 3 TIME AND 3 AMPLITUDE (RMS) FIELDS.
if( strcmp(action,'seisfile') )
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(hmsg,'string','enter name of seis trace location file');
  [sfile, sfilepath] = uiputfile('*.dat', 'select seis file',100,100);
  disp(['seis file is ',sfilepath,sfile]);
  hbuttons=h(2);  %get element of h which is handle of button
  set(hbuttons,'userdata',[sfilepath,sfile]);
  set(hmsg,'string','seis trace location file is named');
  return;
end
if( strcmp(action,'zoom') | strcmp(action,'zoom2') )
    h=get(gcf,'userdata');
    hmsg=h(12);
    set(hmsg,'string','click and drag to define zoom area');
    %   this zoom stuff from vgmar/../logsec
    if(strcmp(action,'zoom2') )
        zoombox=selboxfini;
    else
        % initialize zooming with selboxinit
        selboxinit('seis2well(''zoom2'')');
        return;
    end
    
    zoombox=selboxfini;
    try
        delete(zoombox{2});
    catch
        %no selbox to delete
    end
    zoombox = zoombox{1};
    
    x1=zoombox(1);
    y1=zoombox(2);
    x2=zoombox(3);
    y2=zoombox(4);
    ymin=min([y1,y2]);
    ymax=max([y1,y2]);
    xmin=min([x1,x2]);
    xmax=max([x1,x2]);
    %         double check, guard against zero area box
    set(gca,'xlim',[xmin xmax],'ylim',[ymin,ymax]);
    set(hmsg,'string','zoom area is defined');
    return;
end
if( strcmp(action,'unzoom') )
  axis('auto');
  return;
end
if( strcmp(action,'help') )
  h=get(gcf,'userdata');
  hmsg=h(12);
  set(hmsg,'string','help will appear on matlab window');
disp(' 1. get on dec machine, appropriate directory, if using ACE, ')
disp('    open up a blue xterm window')
disp(' 2. source *.env  (set the seisline catalog files)')
disp(' 3. ~vtnbi/bin/readwells >> filename,')
disp('    you may want to name the filename something.dat, since that')
disp('    is what seis2well will look for (*.dat).')
disp(' 4. ~vtnbi/bin/extract_raster or ~vtnbi/bin/console.dec')
disp('    you may want to rename the filename from fort.2 to')
disp('    something.dat, since that is what seis2well looks for.')
disp('    It is highly recommented to use an increment of 5 or')
disp('       more in extract_raster, to speed things up. ')
disp(' 5. xhost + andromeda')
disp(' 6. rlogin andromeda')
disp(' 7. setenv DISPLAY magpie:0')
disp(' 8. cd to appropriate directory')
disp(' 9. matlab')
disp(' 10. seis2well')
disp(' 10a. enter wellfile (from 3 above)')
disp(' 10b. enter seisfile (from 4 above)')
disp(' 10c. plot')
disp(' 10d. define circle')
disp(' 10e. define azimuth (optional)')
disp(' 10f. doit  ')
disp(' 10g. output the file, a card file with well and line info')
disp(' T.N.Bishop, CPTC Canada, Dec. 1993')
  return;
end
if( strcmp(action,'quit') )
close
end