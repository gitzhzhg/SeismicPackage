function plotimage3D(seis,t,xline,iline,dname,cmap)
% PLOTIMAGE3D: provides interactive ability to browse a 3D seismic volume
%
% plotimage3D(seis,t,xline,iline,dname,cmap)
%
% seis ... 3D seismic matrix. Should be a regular grid with dimension 1
%       (row) as time, dimension 2 (column) as xline, and dimension 3 as
%       iline.
% t ... time coordinate vector for seis. Must has the same length as
%       size(seis,1)
% xline ... crossline coordinate or x coordinate. Must have length equal to
%       size(seis,2)
% iline ... inline coordinate or y coordinate. Must have length equal to
%       size(seis,3)
% dataname ... string giving a name for the dataset
% ************ default = '' ***********
% cmap ... initial colormap. This is a text string and should be one of
%     colormaps={'seisclrs','parula','jet','hsv','copper','autumn','bone'...
%         'gray','cool','winter','spring','alpine','summer','hot'};
% ************ default = 'seisclrs' ***********
% 
% NOTE: Missing traces should be filled with zeros (not nan's). The
% presence of nan's in the data volume will cause the program to fail.
% NOTE2: When read into memory using readsegy or similar, a 3D survey will
% be stored as a 2D matrix in trace sequential mode (i.e. one trace after
% another). You must move these traces into a 3D matrix in order to use
% this function. Unless the 3D survey has a perfectly square spatial
% aperture, this will generally involve padding with zero traces. You can
% use 'make3dvol' for this purpose. Here is an axample:
% NOTE3: This function is designed to work with the specific size window
% that it creates by default. If you resize this window significantly, then
% these axes labels may become incorrect. There is currently no workaround
% for this. 
%
% [traces,dt,texthead,binaryhead,extendedhead]=readsegy('my3Dsegyfile.sgy');
% dt=dt/1000;
% t=(0:size(traces.tracedata.data,1)-1)*dt;
% ilineall=SEGY_getHeader(traces.traceheader,'iline');
% xlineall=SEGY_getHeader(traces.traceheader,'xline');
% xcdpall=SEGY_getHeader(traces.traceheader,'cdpx');
% ycdpall=SEGY_getHeader(traces.traceheader,'cdpy');
% [seis3D,xline,iline,xcdp,ycdp]=make3Dvol(traces.tracedata.data,xlineall,ilineall,xcdpall,ycdpall);
%
% This example expects the inline and crossline numbers and the inline and
% crossline cdp values to be stored in the SEGY trace headers in the
% standard places. It will fail if this is not the case with your data.
%
% G.F. Margrave, Devon Energy and CREWES (U of Calgary), 2016
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

%USER DATA information
%
% control ... tagstring ... userdata
% hbasemap ... 'basemap' ... {seis, t, xline, iline, dname [smean sdev smin smax]}
% hinline ... 'inlinebox' ... ykol (yellow color user to indicate which mode is active)
% htslice ... 'tslicebox' ... dt (time sample rate)
% hb1 ... 'inlinemode' ... [hb2 hb3 hinline hxline htslice] (handles of various controls)
% hampapply ... 'ampapply' ... [hmax hmin hmaxlbl hminlbl] (handles of manual clipping controls)
% hlocate ... 'locate' ... a flag used by locate mechanism
% 
% To find any of these handles, just use h=findobj(gcf,'tag','tagstring')
% 
% 
global PLOTIMAGE3DFIGS PLOTIMAGE3DDATASIZE
if(ischar(seis))
    action=seis;
else
    action='init';
end

if(strcmp(action,'init'))
    
    if(nargin<5)
        dname='';
    end
    if(nargin<6)
        cmap='seisclrs';
    end
    seiss=single(seis);
    clear seis;
    [nt,nx,ny]=size(seiss);
    
    if(length(t)~=nt)
        error('t is the wrong size')
    end
    if(length(xline)~=nx)
        error('xline is wrong size');
    end
    if(length(iline)~=ny)
        error('iline is wrong size');
    end
    xline=xline(:)';%row vector
    iline=iline(:);%column vector
    xx=xline(ones(size(iline)),:);
    yy=iline(:,ones(size(xline)));
    
    figure
    %get plotting statistics
    %first determine where the zero traces are
    set(gcf,'name',['plotimage3d ... ' dname]);
    map=squeeze(sum(abs(seiss),1))';
    %ideadtr=find(map==0);
    ilivetr=find(map~=0);
    %map(ixlive,iylive)=1;
    xlive=xx(ilivetr);
    ylive=yy(ilivetr);
    ilive=find(seiss);%live samples (really just the nonzero samples)
    smean=mean(seiss(ilive));
    sdev=std(seiss(ilive));
    smax=max(seiss(ilive));
    smin=min(seiss(ilive));
    
    %make the basemap axes
    xnot=.05;ynot=.1;
    width=.15;ht=.2;
    ynow=1-ynot-ht;
    xnow=xnot;
    hbmap=axes('position',[xnow,ynow,width,ht],'tag','basemap');
    hh=plot(xlive(:),ylive(:),'r.','markersize',.1);flipy;
    set(hbmap,'tag','basemap');
    set(hh,'color',[.5 .5 .5]);
    set(hbmap,'yaxislocation','right');
    xlabel('crossline');ylabel('inline');
    title('basemap')
    xmin=min(xline);
    ymin=min(iline);
    xmax=max(xline);
    ymax=max(iline);
    xlim([xmin xmax]);
    ylim([ymin ymax]);
    set(hbmap,'userdata',{seiss, t, xline, iline, dname [smean sdev smin smax]})
    
    
    %mode buttons
    sep=.05;
    ht=.03;
    width=.05;
    ynow=ynow-sep-ht;
    ykol=[1 1 .5];
    fs=10;
    hb1=uicontrol(gcf,'style','pushbutton','string','Inline','tag','inlinemode',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''inlinemode'')','backgroundcolor',ykol,'fontsize',fs);
    sep=.01;
    xnow=xnow+width+sep;
    hb2=uicontrol(gcf,'style','pushbutton','string','Xline','tag','xlinemode',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''xlinemode'')','fontsize',fs);
    xnow=xnow+width+sep;
    hb3=uicontrol(gcf,'style','pushbutton','string','Tslice','tag','tslicemode',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''tslicemode'')','fontsize',fs);
    
    
    xnow=xnot;
    ynow=ynow-sep-ht;
    fs=10;
    inot=round(length(iline)/2);%first inline to display
    hinline=uicontrol(gcf,'style','edit','string',int2str(iline(inot)),'tag','inlinebox',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''inline'')','backgroundcolor',ykol,'fontsize',fs,...
        'userdata',ykol,'tooltipstring',...
        ['inline number to view (min=' int2str(min(iline)) ', max=' int2str(max(iline)) ')']);
    sep=.01;
    xnow=xnow+width+sep;
    hxline=uicontrol(gcf,'style','edit','string','all','tag','xlinebox',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''xline'')','fontsize',fs,'tooltipstring',...
        ['xline number to view (min=' int2str(min(xline)) ', max=' int2str(max(xline)) ')']);
    xnow=xnow+width+sep;
    htslice=uicontrol(gcf,'style','edit','string','all','tag','tslicebox',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''tslice'')','fontsize',fs,'userdata',t(2)-t(1),...
        'tooltipstring',...
        ['timeslice to view (min=' num2str(min(t)) ', max=' num2str(max(t)) ')']);
    
    set(hb1,'userdata',[hb2 hb3 hinline hxline htslice]);
    
    %prev, next and increment
    xnow=xnot;
    ynow=ynow-2*sep-ht;
    fs=10;
    uicontrol(gcf,'style','pushbutton','string','previous','tag','previous',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''previous'')','fontsize',fs,'tooltipstring',...
        'increment the view to the previous inline/xline/tslice');
    sep=.01;
    xnow=xnow+width+sep;
    uicontrol(gcf,'style','pushbutton','string','next','tag','next',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''next'')','fontsize',fs,'tooltipstring',...
        'increment the view to the next inline/xline/tslice');
    xnow=xnow+width+sep;
    w2=width/2-.5*sep;
    uicontrol(gcf,'style','text','string','incr->','units','normalized',...
        'position',[xnow,ynow,w2,ht],'fontsize',fs,...
        'tooltipstring','increment for prev and next');
    xnow=xnow+w2;
    uicontrol(gcf,'style','edit','string','1','tag','increment',...
        'units','normalized','position',[xnow ynow w2+sep ht],'callback',...
        'plotimage3D(''increment'')','fontsize',fs,...
        'tooltipstring','specify increment in samples');
    
    %clip control
    xnow=xnot;
    ynow=ynow-3*sep-ht;
    fs=10;
    uicontrol(gcf,'style','text','string','Clip level->','tag','cliplabel',...
        'units','normalized','position',[xnow ynow width ht],'fontsize',fs,...
        'tooltipstring','Choose clipping level, smaller means more clipping');
    sep=.01;
    xnow=xnow+width;
    cliplevels={'manual','30','20','15','10','8','7','6','5','4','3','2','1','.5','.25','.1','.05'};
    iclip=10;%starting clip level
    uicontrol(gcf,'style','popupmenu','string',cliplevels,'tag','cliplevel',...
        'units','normalized','position',[xnow ynow .75*width ht],'callback',...
        'plotimage3D(''clip'')','fontsize',fs,'value',iclip);
    
    %manual amplitude controls
    xnow=xnow+.5*width+2*sep;
    ynow=ynow+ht/2;
    vis='off';
    hmaxlbl=uicontrol(gcf,'style','text','string','max->','tag','maxamplbl',...
        'units','normalized','position',[xnow,ynow,.5*width',ht],...
        'fontsize',fs,'visible',vis,'tooltipstring',...
        'enter the maximum amplitude to be displayed without clipping');
    xnow=xnow+.5*width;
    hmax=uicontrol(gcf,'style','edit','string',num2str(smean+str2double(cliplevels{iclip})*sdev),'tag','maxamp',...
        'units','normalized','position',[xnow ynow width ht],'fontsize',...
        fs,'visible',vis,...
        'tooltipstring','value shown is the current clipping maximum');
    xnow=xnow-.5*width;
    ynow=ynow-ht;
    hminlbl=uicontrol(gcf,'style','text','string','min->','tag','minamplbl',...
        'units','normalized','position',[xnow,ynow,.5*width',ht],...
        'fontsize',fs,'visible',vis,'tooltipstring',...
        'enter the minimum amplitude to be displayed without clipping');
    xnow=xnow+.5*width;
    hmin=uicontrol(gcf,'style','edit','string',num2str(smean-str2double(cliplevels{iclip})*sdev),'tag','minamp',...
        'units','normalized','position',[xnow ynow width ht],'fontsize',...
        fs,'visible',vis,...
        'tooltipstring','value shown is the current clipping minimum');
    ynow=ynow-ht;
    uicontrol(gcf,'style','pushbutton','string','apply','tag','ampapply',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''manualclipping'')','fontsize',fs,'visible',vis,...
        'tooltipstring','push to apply manual clipping',...
        'userdata',[hmax hmin hmaxlbl hminlbl]);
    
    %colormap control
    xnow=xnot;
    ynow=ynow-ht;
    fs=10;
    uicontrol(gcf,'style','text','string','Colormap->','tag','colomaplabel',...
        'units','normalized','position',[xnow ynow width ht],'fontsize',fs);
    sep=.01;
    xnow=xnow+width;
    if(exist('parula','file')==2)
        colormaps={'seisclrs','parula','jet','hsv','copper','autumn','bone'...
            'gray','cool','winter','spring','alpine','summer','hot'};
        icolor=2;%starting colormap
    else
        colormaps={'seisclrs','jet','hsv','copper','autumn','bone'...
            'gray','cool','winter','spring','alpine','summer','hot'};
        icolor=2;%starting colormap
    end
    for k=1:length(colormaps)
        if(strcmp(colormaps{k},cmap))
            icolor=k;
        end
    end
    uicontrol(gcf,'style','popupmenu','string',colormaps,'tag','colormap',...
        'units','normalized','position',[xnow ynow width ht],'callback',...
        'plotimage3D(''colormap'')','fontsize',fs,'value',icolor);
    
    %grid lines
    xnow=xnow+width+.5*sep;
    uicontrol(gcf,'style','text','string','Grid lines','tag','grid',...
        'units','normalized','position',[xnow,ynow,.75*width,ht],'fontsize',fs,...
        'tooltipstring','turn coordinate grid on or off');
    %ynow=ynow-ht;
    xnow=xnow+.75*width;
    gridoptions={'off','.1','.2','.3','.4','.5','.6','.7','.8','.9','1'};
    uicontrol(gcf,'style','popupmenu','string',gridoptions,'tooltipstring',...
        'larger number means darker grid lines','units','normalized',...
        'position',[xnow,ynow,.5*width,ht],'callback','plotimage3D(''grid'')',...
        'value',1,'fontsize',fs,'tag','gridoptions');
    
    %copy to clipboard
    xnow=xnot;
    ynow=ynow-ht-sep;
    if(isunix)
        msg='To TIFF file with controls';
        msg2='Save current view to a TIFF file';
    else
        msg='To Clipboard with controls';
        msg2='Copy current view to the WINDOWS clipboard';
    end
    uicontrol(gcf,'style','pushbutton','string',msg,'tag','clipboard',...
        'units','normalized','position',[xnow,ynow,2.6*width ht],'callback',...
        'plotimage3D(''clipboard'')','fontsize',fs,'tooltipstring',msg2);
    ynow=ynow-ht-.5*sep;
    if(isunix)
        msg='To TIFF file without controls';
        msg2='Save current view to a TIFF file';
    else
        msg='To Clipboard without controls';
        msg2='Copy current view to the WINDOWS clipboard';
    end
    uicontrol(gcf,'style','pushbutton','string',msg,'tag','clipboardalt',...
        'units','normalized','position',[xnow,ynow,2.6*width ht],'callback',...
        'plotimage3D(''clipboardalt'')','fontsize',fs,'tooltipstring',msg2);
    
    %add to group
    ynow=ynow-ht-.5*sep;
    uicontrol(gcf,'style','pushbutton','string','Add to Group','units',...
        'normalized','position',[xnow,ynow,1.3*width,ht],'callback',...
        'plotimage3D(''group'')','fontsize',fs,'tooltipstring',...
        'Include in group of linked plotimage3D figures','tag','group');
    %remove from group
    xnow=xnow+1.3*width;
    uicontrol(gcf,'style','pushbutton','string','Remove from Group','units',...
        'normalized','position',[xnow,ynow,1.3*width,ht],'callback',...
        'plotimage3D(''ungroup'')','fontsize',fs,'tooltipstring',...
        'Remove from group of linked plotimage3D figures','tag','ungroup');
    %clear group
    xnow=xnow+1.3*width;
    uicontrol(gcf,'style','pushbutton','string','Clear Group','units',...
        'normalized','position',[xnow,ynow,1.3*width,ht],'callback',...
        'plotimage3D(''cleargroup'')','fontsize',fs,'tooltipstring',...
        'Clear the group of linked figures to start a new group',...
        'tag','cleargroup');
    
    %save views
    xnow=xnot;
    ynow=ynow-ht-.5*sep;
    uicontrol(gcf,'style','pushbutton','string','Save view','units',...
        'normalized','position',[xnow,ynow,width,ht],'callback',...
        'plotimage3D(''saveview'')','fontsize',fs,'tooltipstring',...
        'Save this view for easy return','tag','saveview');
    %forget views
    xnow=xnow+width;
    uicontrol(gcf,'style','pushbutton','string','Forget view','units',...
        'normalized','position',[xnow,ynow,width,ht],'callback',...
        'plotimage3D(''forgetview'')','fontsize',fs,'tooltipstring',...
        'Remove from list of saved views','tag','forgetview');
    %restore views
    xnow=xnow+width;
    uicontrol(gcf,'style','popupmenu','string',{'Saved views'},'units',...
        'normalized','position',[xnow,ynow,1.5*width,ht],'callback',...
        'plotimage3D(''restoreview'')','fontsize',fs,'tooltipstring',...
        'Remove from list of saved views','tag','savedviews');
    
    %cursor locate button
    xnow=xnot;
    ynow=ynow-ht-sep;
    uicontrol(gcf,'style','pushbutton','string','cursor locate on','units',...
        'normalized','position',[xnow ynow width ht],'tag','locate',...
        'fontsize',fs,'tooltipstring','Turn on location information at cursor',...
        'callback','plotimage3D(''locate'')','userdata',[]);
    %flipx button
    xnow=xnow+width;
    uicontrol(gcf,'style','pushbutton','string','flip xline','units',...
        'normalized','position',[xnow ynow width ht],'tag','locate',...
        'fontsize',fs,'tooltipstring','Reverse the x axis',...
        'callback','plotimage3D(''flipx'')','userdata',1,'tag','flipx');
    %userdata, 1 for normal -1 for reversed. This refers to the seismic
    %axis which uses the image convention for axis direction. For images, 
    %xdir normal increases to the right while ydir normal increases down.
    %For normal plots, xdir is the same but ydir increases up.
    %flipy button
    xnow=xnow+width;
    uicontrol(gcf,'style','pushbutton','string','flip inline','units',...
        'normalized','position',[xnow ynow width ht],'tag','locate',...
        'fontsize',fs,'tooltipstring','Reverse the y axis',...
        'callback','plotimage3D(''flipy'')','userdata',1,'tag','flipy');
    
    bigfig
    
    %make the seismic axes and shot the first inline
    xnow=xnot+3*sep+4*width;
    width=.6;
    ht=.8;
    ynow=ynot;
    %hseismic=axes('position',[xnow,ynow,width,ht],'tag','seismic');
    axes('position',[xnow,ynow,width,ht],'tag','seismic');
    updateview;
    
elseif(strcmp(action,'inline'))
    %hfigs=getfigs;
    hthisfig=gcf;
    hinlinemode=findobj(hthisfig,'tag','inlinemode');
    udat=get(hinlinemode,'userdata');
    hxlinemode=udat(1);
    htslicemode=udat(2);
    hinline=udat(3);
    hxline=udat(4);
    htslice=udat(5);
    kol_off=.94*ones(1,3);
    kol_on=get(hinline,'userdata');
    tmp=get(hinline,'string');
    iline_chosen=str2double(tmp);
    if(isnan(iline_chosen))
        msgbox('You must enter an integer number to chose an inline',...
            'Ooops!');
        return;
    end
    hbmap=findobj(hthisfig,'tag','basemap');
    udat=get(hbmap,'userdata');
    iline=udat{4};
    imin=min(iline);imax=max(iline);
    if(iline_chosen<imin || iline_chosen>imax)
        msgbox(['Invalid inline number, must be between ' int2str(imin) ...
            ' and ' int2str(imax)],'Ooops!');
        return;
    end
    set(hinlinemode,'backgroundcolor',kol_on);
    set(hinline,'backgroundcolor',kol_on);
    set(hxlinemode,'backgroundcolor',kol_off);
    set(hxline,'backgroundcolor',kol_off,'string','all');
    set(htslicemode,'backgroundcolor',kol_off);
    set(htslice,'backgroundcolor',kol_off,'string','all');
    updateview;
    
elseif(strcmp(action,'xline'))
    %hfigs=getfigs;
    hthisfig=gcf;
    hinlinemode=findobj(hthisfig,'tag','inlinemode');
    udat=get(hinlinemode,'userdata');
    hxlinemode=udat(1);
    htslicemode=udat(2);
    hinline=udat(3);
    hxline=udat(4);
    htslice=udat(5);
    kol_off=.94*ones(1,3);
    kol_on=get(hinline,'userdata');
    tmp=get(hxline,'string');
    xline_chosen=str2double(tmp);
    if(isnan(xline_chosen))
        msgbox('You must enter an integer number to chose an xline',...
            'Ooops!');
        return;
    end
    hbmap=findobj(hthisfig,'tag','basemap');
    udat=get(hbmap,'userdata');
    xline=udat{3};
    xlmin=min(xline);xlmax=max(xline);
    if(xline_chosen<xlmin || xline_chosen>xlmax)
        msgbox(['Invalid xline number, must be between ' int2str(xlmin) ...
            ' and ' int2str(xlmax)],'Ooops!');
        return;
    end
    set(hinlinemode,'backgroundcolor',kol_off);
    set(hinline,'backgroundcolor',kol_off,'string','all');
    set(hxlinemode,'backgroundcolor',kol_on);
    set(hxline,'backgroundcolor',kol_on);
    set(htslicemode,'backgroundcolor',kol_off);
    set(htslice,'backgroundcolor',kol_off,'string','all');
    updateview;
    
elseif(strcmp(action,'tslice'))
    %hfigs=getfigs;
    hthisfig=gcf;
    hinlinemode=findobj(hthisfig,'tag','inlinemode');
    udat=get(hinlinemode,'userdata');
    hxlinemode=udat(1);
    htslicemode=udat(2);
    hinline=udat(3);
    hxline=udat(4);
    htslice=udat(5);
    kol_off=.94*ones(1,3);
    kol_on=get(hinline,'userdata');
    tmp=get(htslice,'string');
    tslice_chosen=str2double(tmp);
    if(isnan(tslice_chosen))
        msgbox('You must enter a valid time to chose a tslice',...
            'Ooops!');
        return;
    end
    hbmap=findobj(hthisfig,'tag','basemap');
    udat=get(hbmap,'userdata');
    t=udat{2};
    tmin=min(t);tmax=max(t);
    if(tslice_chosen<tmin || tslice_chosen>tmax)
        msgbox(['Invalid time, must be between ' num2str(tmin) ...
            ' and ' num2str(tmax)],'Ooops!');
        return;
    end
    %make sure the time requested is one that we have
    it=near(t,tslice_chosen);
    tslice_chosen=t(it(1));
    set(htslice,'string',num2str(tslice_chosen));
    set(hinlinemode,'backgroundcolor',kol_off);
    set(hinline,'backgroundcolor',kol_off,'string','all');
    set(hxlinemode,'backgroundcolor',kol_off);
    set(hxline,'backgroundcolor',kol_off,'string','all');
    set(htslicemode,'backgroundcolor',kol_on);
    set(htslice,'backgroundcolor',kol_on);
    updateview;
    
elseif(strcmp(action,'previous'))
    hincr=findobj(gcf,'tag','increment');
    tmp=get(hincr,'string');
    inc=str2double(tmp);
    mode=determinemode;
    switch mode
        case 'inline'
            hinline=findobj(gcf,'tag','inlinebox');
            tmp=get(hinline,'string');
            inlinenow=str2double(tmp);
            inlinenext=inlinenow-inc;
            set(hinline,'string',int2str(inlinenext));
            updateview;          
        case 'xline'
            hxline=findobj(gcf,'tag','xlinebox');
            tmp=get(hxline,'string');
            xlinenow=str2double(tmp);
            xlinenext=xlinenow-inc;
            set(hxline,'string',int2str(xlinenext));
            updateview;
            
        case 'tslice'
            htslice=findobj(gcf,'tag','tslicebox');
            dt=get(htslice,'userdata');
            inc=inc*dt;
            tmp=get(htslice,'string');
            tslicenow=str2double(tmp);
            tslicenext=tslicenow-inc;
            set(htslice,'string',num2str(tslicenext));
            updateview;
            
    end
    
elseif(strcmp(action,'next'))
    hincr=findobj(gcf,'tag','increment');
    tmp=get(hincr,'string');
    inc=str2double(tmp);
    mode=determinemode;
    switch mode
        case 'inline'
            hinline=findobj(gcf,'tag','inlinebox');
            tmp=get(hinline,'string');
            inlinenow=str2double(tmp);
            inlinenext=inlinenow+inc;
            set(hinline,'string',int2str(inlinenext));
            updateview;
        case 'xline'
            hxline=findobj(gcf,'tag','xlinebox');
            tmp=get(hxline,'string');
            xlinenow=str2double(tmp);
            xlinenext=xlinenow+inc;
            set(hxline,'string',int2str(xlinenext));
            updateview;
            
        case 'tslice'
            htslice=findobj(gcf,'tag','tslicebox');
            dt=get(htslice,'userdata');
            inc=inc*dt;
            tmp=get(htslice,'string');
            tslicenow=str2double(tmp);
            tslicenext=tslicenow+inc;
            set(htslice,'string',num2str(tslicenext));
            updateview;
            
    end
    
elseif(strcmp(action,'clip'))
    %hfigs=getfigs;
    hthisfig=gcf;
    clipnow=getclip;
    hampapply=findobj(hthisfig,'tag','ampapply');
    hampcontrols=get(hampapply,'userdata');
    if(length(clipnow)>1)
        set([hampapply hampcontrols],'visible','on');
        return;
    else
        set([hampapply hampcontrols],'visible','off');
    end
    %ind=hfigs~=hthisfig;
    %hotherfigs=hfigs(ind);
    %get the dat amp values
    hbmap=findobj(hthisfig,'tag','basemap');
    udat=get(hbmap,'userdata');
    amp=udat{6};
    %update the values in the invisible max and min edit fields
    set(hampcontrols(1),'string',num2str(amp(1)+clipnow*amp(2)));%maximum
    set(hampcontrols(2),'string',num2str(amp(1)-clipnow*amp(2)));%minimum
    %get the seismic axes and update its clim property
    hseismic=findobj(hthisfig,'tag','seismic');
    clipnow=getclip;
    clim=[amp(1)-clipnow*amp(2), amp(1)+clipnow*amp(2)];
    set(hseismic,'clim',clim);
    
elseif(strcmp(action,'manualclipping'))
    %hfigs=getfigs;
    hthisfig=gcf;
%     ind=hfigs~=hthisfig;
%     hotherfigs=hfigs(ind);
    clim=getclip;
    if(length(clim)~=2)
        error('logic failure');
    end
    hseismic=findobj(hthisfig,'tag','seismic');
    set(hseismic,'clim',clim);
    
elseif(strcmp(action,'colormap'))
    %hfigs=getfigs;
    %hthisfig=gcf;
%     ind=hfigs~=hthisfig;
%     hotherfigs=hfigs(ind);
    cmapnow=getcolormap;
    colormap(cmapnow);
    
elseif(strcmp(action,'clipboard')||strcmp(action,'clipboardalt'))
    if(strcmp(action,'clipboardalt'))
        hidecontrols;
    end
    if(isunix)
        print -dtiff
        adon='tiff file in current directory';
    else
        fh=gcf;
        fh.Renderer='opengl';
        hgexport(fh,'-clipboard');
        adon='Windows clipboard';
    end
    if(strcmp(action,'clipboardalt'))
        restorecontrols;
    end
    msg=['Figure has been sent to ' adon];
    msgbox(msg,'Good news!');
    
elseif(strcmp(action,'grid'))
    hthisfig=gcf;
    hgridopt=findobj(hthisfig,'tag','gridoptions');
    hseismic=findobj(hthisfig,'tag','seismic');
    axes(hseismic);
    opt=get(hgridopt,'value');
    gridoptions=get(hgridopt,'string');
    if(opt==1)
        grid off;
    else
        alpha=str2double(gridoptions{opt});
        grid on;
        set(hseismic,'gridalpha',alpha); 
    end
    
elseif(strcmp(action,'group'))
    hthisfig=gcf;
    hbmap=findobj(hthisfig,'tag','basemap');
    udat=get(hbmap,'userdata');
    if(~isempty(PLOTIMAGE3DFIGS))
        %check data size for compatibility
        tmp=PLOTIMAGE3DDATASIZE;
        if(length(udat{2})~=tmp(1)||length(udat{3})~=tmp(2)||length(udat{4})~=tmp(3))
            msgbox('Cannot include this figure in group because data size is not compatible',...
                'Ooops!');
            return;
        end
    else
        PLOTIMAGE3DDATASIZE=[length(udat{2}) length(udat{3}) length(udat{4})];
    end
    ind=find(hthisfig==PLOTIMAGE3DFIGS,1);%see if we have already included it
    if(isempty(ind))
        PLOTIMAGE3DFIGS=[PLOTIMAGE3DFIGS hthisfig];
        thisname=get(hthisfig,'name');
        msgbox([thisname ' added to group of linked figures'],'Done');
    end
    
elseif(strcmp(action,'ungroup'))
    hthisfig=gcf;
    ind=find(hthisfig==PLOTIMAGE3DFIGS,1);
    if(~isempty(ind))
        PLOTIMAGE3DFIGS(ind)=[];
        thisname=get(hthisfig,'name');
        msgbox([thisname ' removed from group of linked figures'],'Done');
    end
    if(isempty(PLOTIMAGE3DFIGS))
        PLOTIMAGE3DDATASIZE=[];
    end
elseif(strcmp(action,'cleargroup'))
    PLOTIMAGE3DFIGS=[];
    PLOTIMAGE3DDATASIZE=[];
    msgbox(' Existing group cleared, start a new one! ','Done');

elseif(strcmp(action,'saveview'))
    view=currentview;
    hsavedviews=findobj(gcf,'tag','savedviews');
    viewlist=get(hsavedviews,'string');
    nviews=length(viewlist);
    switch view{4}
        case 'inline'
            viewlist{nviews+1}=[view{4} ': ' view{1}];
        case 'xline'
            viewlist{nviews+1}=[view{4} ': ' view{2}];
        case 'tslice'
            viewlist{nviews+1}=[view{4} ': ' view{3}];
    end
    %viewlist{nviews+1}=[view{4} ' inline: ' view{1} ' xline: ' view{2} ' tslice: ' view{3}];
    set(hsavedviews,'string',viewlist,'value',nviews+1);
    udat=get(hsavedviews,'userdata');
    nviews=length(udat);
    udat{nviews+1}=view;
    set(hsavedviews,'userdata',udat);
    
elseif(strcmp(action,'forgetview'))
    view=currentview;
    hsavedviews=findobj(gcf,'tag','savedviews');
    viewlist=get(hsavedviews,'userdata');
    %search for the current view amon the saved views
    nviews=length(viewlist);
    iview=[];
    for k=1:nviews
        if(strcmp(view{1},viewlist{k}{1}))
            if(strcmp(view{2},viewlist{k}{2}))
                if(strcmp(view{3},viewlist{k}{3}))
                    if(strcmp(view{4},viewlist{k}{4}))
                        iview=k;
                    end
                end
            end
        end
    end
    if(isempty(iview))
        msgbox('Current view is not in the saved list','Ooops!');
        return
    end
    viewlist(iview)=[];
    set(hsavedviews,'userdata',viewlist);
    viewnames=get(hsavedviews,'string');
    viewnames(iview+1)=[];
    set(hsavedviews,'string',viewnames,'value',1);
    
elseif(strcmp(action,'restoreview'));
    hsavedviews=findobj(gcf,'tag','savedviews');
    views=get(hsavedviews,'userdata');
    desiredview=get(hsavedviews,'value')-1;
    if(desiredview>0)
        setview(views{desiredview});
    end
    
elseif(strcmp(action,'locate')||strcmp(action,'stoplocate'))
    hfigs=getfigs;
    hthisfig=gcf;
    ind=hfigs~=hthisfig;
    hotherfigs=hfigs(ind);
    hlocate=findobj(gcf,'tag','locate');
    if(strcmp(action,'locate'))
        flag=1;
    else
        flag=0;
    end
    if(flag==1) %turn it on
        set(hlocate,'string','cursor locate off','callback','plotimage3D(''stoplocate'')');
        set(gcf,'windowbuttondownfcn','plotimage3D(''postlocation'')');
    else
        set(hlocate,'string','cursor locate on','callback','plotimage3D(''locate'')');
        set(gcf,'windowbuttondownfcn','');
        clearlocations;
    end
    
    %process other figs
    for k=1:length(hotherfigs)
        hlocate=findobj(hotherfigs(k),'tag','locate');
        if(flag==1) %turn it on
            set(hlocate,'string','cursor locate off','callback','plotimage3D(''stoplocate'')');
            set(hotherfigs(k),'windowbuttondownfcn','plotimage3D(''postlocation'')');
        else
            set(hlocate,'string','cursor locate on','callback','plotimage3D(''locate'')');
            set(hotherfigs(k),'windowbuttondownfcn','');
            clearlocations;
        end
    end
elseif(strcmp(action,'postlocation'))
    hlocate=findobj(gcf,'tag','locate');
    hfigs=getfigs;
    hthisfig=gcf;
    hseismic=findobj(hthisfig,'tag','seismic');
    existinglocations=get(hlocate,'userdata');
    currentpoint=get(hseismic,'currentpoint');
    % these are stored in a cell array, one entry per point. Each point is
    % represented by a two element vector of handles. The first is the
    % handle to the line with the point marker and the second is the text.
    % check existing locations. If we have a match, then this is a deletion
    if(isempty(currentpoint))
        return;
    end
    npts=length(existinglocations);
    badpoints=zeros(size(existinglocations));
    small=10^6*eps;
    for k=1:npts
        thispoint=existinglocations{k};
        if(isempty(thispoint))
            badpoints(k)=1;
        else
            if(~isgraphics(thispoint(1)))
                badpoints(k)=1;
            else
                xpt=get(thispoint(1),'xdata');
                ypt=get(thispoint(1),'ydata');
                test=abs(currentpoint(1,1)-xpt)+abs(currentpoint(1,2)-ypt);
                if(test<small)
                    delete(thispoint);
                    return;
                end
            end
        end
    end
    ind=find(badpoints==1);
    if(~isempty(ind))
        existinglocations(ind)=[];
    end
    mode=determinemode;
    fs=9;fcol='k';mksize=6;mkcol='k';
    if(strcmp(mode,'tslice'))
        newpoint(2)=text(currentpoint(1,1),currentpoint(1,2),...
            ['(' int2str(currentpoint(1,1)) ',' int2str(currentpoint(1,2)) ')'],...
            'fontsize',fs','color',fcol);
    else
        newpoint(2)=text(currentpoint(1,1),currentpoint(1,2),...
            ['(' int2str(currentpoint(1,1)) ',' num2str(round(1000*currentpoint(1,2))/1000) ')'],...
            'fontsize',fs','color',fcol);
    end
    newpoint(1)=line(currentpoint(1,1),currentpoint(1,2),'linestyle','none',...
        'marker','*','markersize',mksize,'color',mkcol);
    existinglocations{npts+1}=newpoint;
    set(hlocate,'userdata',existinglocations);
    
    %post the point in other figs
    ind= hfigs~=hthisfig;
    hotherfigs=hfigs(ind);
    for k=1:length(hotherfigs)
        figure(hotherfigs(k));
        hseismic=findobj(hotherfigs(k),'tag','seismic');
        hlocate=findobj(hotherfigs(k),'tag','locate');
        existinglocations=get(hlocate,'userdata');
        set(hotherfigs(k),'currentaxes',hseismic);
        if(strcmp(mode,'tslice'))
            newpoint(2)=text(currentpoint(1,1),currentpoint(1,2),...
                ['(' int2str(currentpoint(1,1)) ',' int2str(currentpoint(1,2)) ')'],...
                'fontsize',fs','color',fcol);
        else
            newpoint(2)=text(currentpoint(1,1),currentpoint(1,2),...
                ['(' int2str(currentpoint(1,1)) ',' num2str(round(1000*currentpoint(1,2))/1000) ')'],...
                'fontsize',fs','color',fcol);
        end
        newpoint(1)=line(currentpoint(1,1),currentpoint(1,2),'linestyle','none',...
            'marker','*','markersize',mksize,'color',mkcol);
        existinglocations{npts+1}=newpoint;
        set(hlocate,'userdata',existinglocations);
    end
    
    
elseif(strcmp(action,'flipx'))
    hfigs=getfigs;
    hthisfig=gcf;
    hflipx=findobj(hthisfig,'tag','flipx');
    dirflag=get(hflipx,'userdata');
    if(dirflag==1)
        set(hflipx,'userdata',-1);
    else
        set(hflipx,'userdata',1);
    end
    setaxesdir;
    
    %now handle other figs in group
    ind=hfigs~=hthisfig;
    hotherfigs=hfigs(ind);
    for k=1:length(hotherfigs)
        figure(hotherfigs(k));
        hflipx=findobj(hotherfigs(k),'tag','flipx');
        if(dirflag==1)
            set(hflipx,'userdata',-1);
        else
            set(hflipx,'userdata',1);
        end
        setaxesdir;
    end
elseif(strcmp(action,'flipy'))
    hfigs=getfigs;
    hthisfig=gcf;
    hflipy=findobj(hthisfig,'tag','flipy');
    dirflag=get(hflipy,'userdata');
    if(dirflag==1)
        set(hflipy,'userdata',-1);
    else
        set(hflipy,'userdata',1);
    end
    setaxesdir;
    
    %now handle other figs in group
    ind=hfigs~=hthisfig;
    hotherfigs=hfigs(ind);
    for k=1:length(hotherfigs)
        figure(hotherfigs(k));
        hflipy=findobj(hotherfigs(k),'tag','flipy');
        if(dirflag==1)
            set(hflipy,'userdata',-1);
        else
            set(hflipy,'userdata',1);
        end
        setaxesdir;
    end
end

end

function mode=determinemode
hthisfig=gcf;
hinlinemode=findobj(hthisfig,'tag','inlinemode');
udat=get(hinlinemode,'userdata');
% hxlinemode=udat(1);
% htslicemode=udat(2);
hinline=udat(3);
hxline=udat(4);
htslice=udat(5);
%determine mode
ykol=get(hinline,'userdata');%this is a flag to indicate mode
kol=get(hinline,'backgroundcolor');
if(kol==ykol)
    mode='inline';
end
kol=get(hxline,'backgroundcolor');
if(kol==ykol)
    mode='xline';
end
kol=get(htslice,'backgroundcolor');
if(kol==ykol)
    mode='tslice';
end
end

function updateview
global PLOTIMAGE3DMASTER
hfigs=getfigs;%get the grouped plotseis3D figures
hthisfig=gcf;
ind=hfigs~=hthisfig;
hotherfigs=hfigs(ind);%other figures in this group
%first deal with the main figure
hinlinemode=findobj(hthisfig,'tag','inlinemode');
udat=get(hinlinemode,'userdata');
% hxlinemode=udat(1);
% htslicemode=udat(2);
hinline=udat(3);
hxline=udat(4);
htslice=udat(5);
%determine mode
mode=determinemode;
%get the data
hbmap=findobj(hthisfig,'tag','basemap');
udat=get(hbmap,'userdata');
seiss=udat{1};
t=udat{2};
xline=udat{3};
iline=udat{4};
dname=udat{5};
amp=udat{6};
hseismic=findobj(gcf,'tag','seismic');
switch mode
    case 'inline'
        tmp=get(hinline,'string');
        iline_next=str2double(tmp);
        tmp=get(hxline,'string');
        if(strcmp(tmp,'all'))
            ixline=1:length(xline);
        else
            ind=strfind(tmp,':');
            ix1=str2double(tmp(1:ind-1));
            ix2=str2double(tmp(ind+1:end));
            ixline=ix1:ix2;
        end
        tmp=get(htslice,'string');
        if(strcmp(tmp,'all'))
            it=1:length(t);
        else
            ind=strfind(tmp,':');
            it1=str2double(tmp(1:ind-1));
            it2=str2double(tmp(ind+1:end));
            it=it1:it2;
        end
        axes(hseismic)
        %find the image if it exists
        hi=findobj(gca,'type','image');
        inot=near(iline,iline_next);
        if(~isempty(hi))
            tag=get(hi,'tag');
            if(~strcmp(tag,'inline'))
                delete(hi);
                hi=[];
            end
        end
        if(isempty(hi))
            %make a new image
            %xx=1:length(xline);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            clearlocations;
            hi=imagesc(xline,t,squeeze(seiss(it,ixline,inot(1))),clim);
            colorbar;
            set(hi,'tag','inline');
            %flipx;
            cmapnow=getcolormap;
            colormap(cmapnow);
            xlabel('xline number');
            ylabel('time (s)');
            title([dname ' inline ' int2str(iline(inot))]);
%             xt=get(hseismic,'xtick');
%             set(hseismic,'xticklabel',vector2textcell(xline(xt)));
            set(hseismic,'tag','seismic');
            setaxesdir;
            bigfont(hseismic,1.5,1);
            plotimage3D('grid');
        else
            %update the existing image
            set(hi,'cdata',squeeze(seiss(it,ixline,inot(1))),'tag','inline');
            title([dname ' inline ' int2str(iline(inot))]);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            set(hseismic,'tag','seismic','clim',clim);
        end
        %update the basemap
        axes(hbmap)
        hl=findobj(hbmap,'tag','currentline');
        ht=findobj(hbmap,'tag','currenttext');
        if(~isempty(hl)); delete(hl); end
        if(~isempty(ht)); delete(ht); end
        hnow=line(xline,iline(inot)*ones(size(xline)),'color','r');
        set(hnow,'tag','currentline');
        set(hbmap,'tag','basemap');
        nmid=round(length(xline)/2);
        text(xline(nmid),iline(inot),['inline ' int2str(iline(inot))],...
            'horizontalalignment','center','tag','currenttext');
    case 'xline'
        tmp=get(hxline,'string');
        xline_next=str2double(tmp);
        tmp=get(hinline,'string');
        if(strcmp(tmp,'all'))
            iiline=1:length(iline);
        else
            ind=strfind(tmp,':');
            il1=str2double(tmp(1:ind-1));
            il2=str2double(tmp(ind+1:end));
            iiline=il1:il2;
        end
        tmp=get(htslice,'string');
        if(strcmp(tmp,'all'))
            it=1:length(t);
        else
            ind=strfind(tmp,':');
            it1=str2double(tmp(1:ind-1));
            it2=str2double(tmp(ind+1:end));
            it=it1:it2;
        end
        axes(hseismic)
        %find the image if it exists
        hi=findobj(gca,'type','image');
        inot=near(xline,xline_next);
        if(~isempty(hi))
            tag=get(hi,'tag');
            if(~strcmp(tag,'xline'))
                delete(hi);
                hi=[];
            end
        end
        if(isempty(hi))
            %make a new image
            %xx=1:length(iline);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            clearlocations;
            hi=imagesc(iline,t,squeeze(seiss(it,inot(1),iiline)),clim);
            colorbar;
            set(hi,'tag','xline');
            %flipx;
            cmapnow=getcolormap;
            colormap(cmapnow);
            xlabel('inline number');
            ylabel('time (s)');
            title([dname ' xline ' int2str(xline(inot))]);
%             xt=get(hseismic,'xtick');
%             set(hseismic,'xticklabel',vector2textcell(iline(xt)));
            set(hseismic,'tag','seismic');
            setaxesdir;            
            bigfont(hseismic,1.5,1);
            plotimage3D('grid');
        else
            %update the existing image
            set(hi,'cdata',squeeze(seiss(it,inot(1),iiline)),'tag','xline');
            title([dname ' xline ' int2str(xline(inot))]);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            set(hseismic,'tag','seismic','clim',clim);
        end
        %update the basemap
        axes(hbmap)
        hl=findobj(hbmap,'tag','currentline');
        ht=findobj(hbmap,'tag','currenttext');
        if(~isempty(hl)); delete(hl); end
        if(~isempty(ht)); delete(ht); end
        hnow=line(xline(inot)*ones(size(iline)),iline,'color','r');
        set(hnow,'tag','currentline');
        set(hbmap,'tag','basemap');
        nmid=round(length(iline)/2);
        text(xline(inot),iline(nmid),['xline ' int2str(xline(inot))],...
            'horizontalalignment','center','tag','currenttext');
        
        
    case 'tslice'
        tmp=get(htslice,'string');
        tslice_next=str2double(tmp);
        tmp=get(hxline,'string');
        if(strcmp(tmp,'all'))
            ixline=1:length(xline);
        else
            ind=strfind(tmp,':');
            ix1=str2double(tmp(1:ind-1));
            ix2=str2double(tmp(ind+1:end));
            ixline=ix1:ix2;
        end
        tmp=get(hinline,'string');
        if(strcmp(tmp,'all'))
            iiline=1:length(iline);
        else
            ind=strfind(tmp,':');
            il1=str2double(tmp(1:ind-1));
            il2=str2double(tmp(ind+1:end));
            iiline=il1:il2;
        end
        axes(hseismic)
        %find the image if it exists
        hi=findobj(gca,'type','image');
        inot=near(t,tslice_next);
        if(~isempty(hi))
            tag=get(hi,'tag');
            if(~strcmp(tag,'tslice'))
                delete(hi);
                hi=[];
            end
        end
        if(isempty(hi))
            %make a new image
            %xx=1:length(xline);
            %yy=1:length(iline);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            clearlocations;
            hi=imagesc(xline,iline,squeeze(seiss(inot(1),ixline,iiline))',clim);
            colorbar;
            set(hi,'tag','tslice');
            %flipx;
            cmapnow=getcolormap;
            colormap(cmapnow);
            xlabel('xline number');
            ylabel('inline number');
            title([dname ' timeslice ' num2str(t(inot))]);
%             xt=get(hseismic,'xtick');
%             set(hseismic,'xticklabel',vector2textcell(xline(xt)));
%             yt=get(hseismic,'ytick');
%             set(hseismic,'yticklabel',vector2textcell(iline(yt)));
            set(hseismic,'tag','seismic');
            setaxesdir;            
            bigfont(hseismic,1.5,1);
            plotimage3D('grid');
        else
            %update the existing image
            set(hi,'cdata',squeeze(seiss(inot(1),ixline,iiline))','tag','tslice');
            title([dname ' timeslice ' num2str(t(inot))]);
            clipnow=getclip;
            if(length(clipnow)==1)
                clim=[amp(1)-clipnow*amp(2) amp(1)+clipnow*amp(2)];
            else
                clim=clipnow;
            end
            set(hseismic,'tag','seismic','clim',clim);
        end
        %update the basemap
        axes(hbmap)
        hl=findobj(hbmap,'tag','currentline');
        ht=findobj(hbmap,'tag','currenttext');
        if(~isempty(hl)); delete(hl); end
        if(~isempty(ht)); delete(ht); end
        xx=[xline(1) xline(end) xline(end) xline(1) xline(1)];
        yy=[iline(1) iline(1) iline(end) iline(end) iline(1)];
        hnow=line(xx,yy,'color','r');
        set(hnow,'tag','currentline');
        set(hbmap,'tag','basemap');
        xx=(xline(1)+xline(end))*.5;
        yy=(iline(1)+iline(end))*.5;
        text(xx,yy,['tslice ' num2str(t(inot))],...
            'horizontalalignment','center','tag','currenttext');
        
end

%now deal with the otherfigs
if(~isempty(hotherfigs))
    %this stuff with PLOTIMAGE3DMASTER is so that only one figure in the
    %group updates the others. Without this, they update each other
    %endlessly. So, the figure in which the click occurs calls the other
    %but the others just updatethemselves and do not call the others.
    if(isempty(PLOTIMAGE3DMASTER))
        PLOTIMAGE3DMASTER=1;
        view=currentview;
        for k=1:length(hotherfigs)
            figure(hotherfigs(k));
            setview(view);
            PLOTIMAGE3DMASTER=PLOTIMAGE3DMASTER+1;%this seems useless but it makes the editor think PLOTIMAGE3DMASTER is in use
        end
        PLOTIMAGE3DMASTER=[];
    end
end

end

function hfigs=getfigs
global PLOTIMAGE3DFIGS
%PLOTIMAGE3DFIGS is an ordinary array of figure handles
if(isempty(PLOTIMAGE3DFIGS))
    hfigs=gcf;
    return;
else
    ind=isgraphics(PLOTIMAGE3DFIGS);
    PLOTIMAGE3DFIGS=PLOTIMAGE3DFIGS(ind);
    if(isempty(PLOTIMAGE3DFIGS))
        hfigs=gcf;
        return;
    end
    ind=find(gcf==PLOTIMAGE3DFIGS, 1);
    if(isempty(ind))
        hfigs=gcf;
    else
        hfigs=PLOTIMAGE3DFIGS;
    end
    return;
end
end

function clipnow=getclip
%hfigs=getfigs;
hthisfig=gcf;
%ind=hfigs~=hthisfig;
%hotherfigs=hfigs(ind);
hclip=findobj(hthisfig,'tag','cliplevel');
cliplevels=get(hclip,'string');
iclip=get(hclip,'value');
if(strcmp(cliplevels{iclip},'manual'))
    hampapply=findobj(hthisfig,'tag','ampapply');
    hampcontrols=get(hampapply,'userdata');
    ampmax=str2double(get(hampcontrols(1),'string'));
    ampmin=str2double(get(hampcontrols(2),'string'));
    if(isnan(ampmax))
        msgbox('you have not entered a valid number for the maximum amplitude',...
            'Ooops!');
        return;
    end
    if(isnan(ampmin))
        msgbox('you have not entered a valid number for the minimum amplitude',...
            'Ooops!');
        return;
    end
    clipnow=[ampmin ampmax];
else
    clipnow=str2double(cliplevels{iclip});
end
end

function cmapnow=getcolormap
%hfigs=getfigs;
hthisfig=gcf;
% ind=hfigs~=hthisfig;
% hotherfigs=hfigs(ind);
hcolormap=findobj(hthisfig,'tag','colormap');
colormaps=get(hcolormap,'string');
icolor=get(hcolormap,'value');
cmapnow=colormaps{icolor};
end

function hidecontrols
hthisfig=gcf;
hkids=get(hthisfig,'children');
hseismic=findobj(hthisfig,'tag','seismic');
hbmap=findobj(hthisfig,'tag','basemap');
vistate=cell(size(hkids));
xnot=.1;
for k=1:length(hkids)
    if(hkids(k)==hseismic)
        seisposn=get(hseismic,'position');
        set(hseismic,'position',[xnot seisposn(2) seisposn(3)+seisposn(1)-xnot seisposn(4)])
        vistate{k}='on';
    elseif(hkids(k)==hbmap)
        vistate{k}='on';
        hkidsb=get(hbmap,'children');
        visb=cell(size(hkidsb));
        for kk=1:length(hkidsb)
            visb{kk}=get(hkidsb(kk),'visible');
            set(hkidsb(kk),'visible','off');
        end
        set(hbmap,'visible','off');
    elseif(~strcmp(get(hkids(k),'type'),'colorbar'))
        vistate{k}=get(hkids(k),'visible');
        set(hkids(k),'visible','off'); 
    end
end
hcopyalt=findobj(hthisfig,'tag','clipboardalt');
set(hcopyalt,'userdata',{vistate visb seisposn});
end

function restorecontrols
hthisfig=gcf;
hkids=get(hthisfig,'children');
hseismic=findobj(hthisfig,'tag','seismic');
hbmap=findobj(hthisfig,'tag','basemap');
hcopyalt=findobj(hthisfig,'tag','clipboardalt');
udat=get(hcopyalt,'userdata');
vistate=udat{1};
visb=udat{2};
seisposn=udat{3};
for k=1:length(hkids)
    if(hkids(k)==hseismic)
        set(hseismic,'position',seisposn);
    elseif(hkids(k)==hbmap)
        hkidsb=get(hbmap,'children');
        for kk=1:length(hkidsb)
           set(hkidsb,'visible',visb{kk}); 
        end
        set(hbmap,'visible',vistate{k});
    elseif(~strcmp(get(hkids(k),'type'),'colorbar'))
        set(hkids(k),'visible',vistate{k});
    end
end
end

function view=currentview
hthisfig=gcf;
%get the 3 handles
hinline=findobj(hthisfig,'tag','inlinebox');
hxline=findobj(hthisfig,'tag','xlinebox');
htslice=findobj(hthisfig,'tag','tslicebox');
%get the text entries defining the view
itext=get(hinline,'string');
xtext=get(hxline,'string');
ttext=get(htslice,'string');
%determine the mode
mode=determinemode;
%define the view
view={itext xtext ttext mode};
end

function setview(view)
hthisfig=gcf;
%get the 3 handles
hinline=findobj(hthisfig,'tag','inlinebox');
hxline=findobj(hthisfig,'tag','xlinebox');
htslice=findobj(hthisfig,'tag','tslicebox');
%set the view
set(hinline,'string',view{1});
set(hxline,'string',view{2});
set(htslice,'string',view{3});
%execute the proper callback
if(strcmp(view{4},'inline'))
    plotimage3D('inline');
elseif(strcmp(view{4},'xline'))
    plotimage3D('xline');
else
    plotimage3D('tslice');
end
end

function setaxesdir
hflipx=findobj(gcf,'tag','flipx');
hflipy=findobj(gcf,'tag','flipy');
hseismic=findobj(gcf,'tag','seismic');
hbmap=findobj(gcf,'tag','basemap');
dirflag=get(hflipx,'userdata');
mode=determinemode;
if(dirflag==1)
    set(hseismic,'xdir','normal');
    set(hbmap,'xdir','normal');
else
    set(hseismic,'xdir','reverse');
    set(hbmap,'xdir','reverse');
end
dirflag=get(hflipy,'userdata');
if(dirflag==1)
    if(strcmp(mode,'tslice'))
        set(hseismic,'ydir','reverse');
        set(hbmap,'ydir','reverse');
    else
        set(hseismic,'ydir','reverse');
        set(hbmap,'ydir','reverse');
    end
else
    if(strcmp(mode,'tslice'))
        set(hseismic,'ydir','normal');
        set(hbmap,'ydir','normal');
    else
        set(hseismic,'ydir','reverse');
        set(hbmap,'ydir','normal');
    end
end
end
function clearlocations
hfigs=getfigs;
hthisfig=gcf;
ind= hfigs~=hthisfig;
hotherfigs=hfigs(ind);
hlocate=findobj(hthisfig,'tag','locate');
existinglocations=get(hlocate,'userdata');
npts=length(existinglocations);
for k=1:npts
    thispoint=existinglocations{k};
    delete(thispoint);
end
set(hlocate,'userdata',[]);

%process the other figs
for k=1:length(hotherfigs)
    hlocate=findobj(hotherfigs(k),'tag','locate');
    existinglocations=get(hlocate,'userdata');
    npts=length(existinglocations);
    for kk=1:npts
        thispoint=existinglocations{kk};
        delete(thispoint);
    end
    set(hlocate,'userdata',[]);
end
end
    