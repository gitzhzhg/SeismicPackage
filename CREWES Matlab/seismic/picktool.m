function picktool(seis,t,x,hparent,pickstruc)
% PICKTOOL: Event picking on a seismic image plot
%
% picktool(seis,t,x,hparent,pickstruc) ... installs picktool on hparent
%                                       with existing picks in pickstruc
% picktool(seis,t,x,hparent) ... installs picktool on hparent and presents
%                                a file navigagtion dialog to open a saved 
%                                pickstruc file
% picktool(seis,t,x) ... same as previous with hparent=gcf
%
% Given a figure window with a seismic gather displayed, PICKTOOL
% implements an interactive event picking facility. If the figure window
% contains more than one axes, then the seimic data must be in the current
% axes. Existing picks can be provided via the calling arguments or read
% from a file. Facilities are provided to manually edit existing picked
% events, to pick new events, and to save the results to a file. Picks will
% only be allowed at the x coordinates of the seismic data and pick times
% will be made to the nearest tenth of a sample interval. 
%
%
% pickstruc defined fields
% pickstruc.hornames ... cell array of horizon names
% pickstruc.horcolors ... cell array of horizon colors (RGB triplets)
% pickstruc.horpicks ... cell array of horizon picks. For each horizon,
%           this is an npicks-by-3 matrix where column 1 is the pick times
%           and columns 2 and 3 are the x and y coordinates. For 2D column
%           3 can either be omitted or set to anything (usually 0).
% pickstruc.horattrs ... cell array of horizon attribute matrices.
%           Attributes are assumed to be scalar values associated with each
%           pick. There can be any number of attributes. This is an
%           npicks-by-nattrs matrix where nattrs is the number of
%           attrubutes and npicks is the same value as in
%           pickstruc.horpicks.
% pickstruc.horattrnames ... cell array of attribute names available for
%           each horizon.
% pickstruc.horpicktypes ... cell array of horizon picktype as defined by
%           PICKER (The command picker('methods') will give the list of
%           available pick types.)
% pickstruc.horguides ... cell array of the guide points used to guide the
%           autopicker (usually PICKER) in creating pickstruc.horpicks. For
%           each horizon this is an npts-by-3 matrix with columns of t,x,y.
% pickstruc.horfwys ... cell array of time-width's of the picking fairways
%           around the guide points for each horizon.
%
% A self-consistent pickstruc must meet the following criteria:
% 1) All fields must be cell arrays of the same length. One entry per
%    horizon.
% 2) pickstruc.horattrs{k} and pickstruc.horattrnames{k} must have
%    consistent sizes for any positive integer k. If the former is a matrix
%    with nattr columns, then the latter must be a cell array of length
%    nattr.
% 3) All fields should exist but can be filled with null entries
%
% by G.F. Margrave, CREWES, 2016
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


%userdata assignments
%hnewhor=findobj(gcf,'tag','newhor') ... {seis,t,x}
%hstart=findobj(gcf,'tag','start') ... {anchorsarray,haxe} (haxe is the
%                                       axes the seismic is in)
%hstopsave=findobj(gcf,'tag','stopsave') ... {pickstruc,fullfilename}
%hlinewidth=findobj(gcf,'tag','linewidths') ... lws (vector of linewidths)
%hrepick=findobj(gcf,'tag','repick') ... stores the windowbuttondownfcn and
%                            windowbuttonupfcn that exist before editing as
%                            {wbdfcn, wbufcn} 
%hlegend=findobj(gcf,'tag','legendoo') ... 1 or 0 indicating legend on or off
% 

if(nargin<3)
    hparent=gcf;
end

if(~ischar(seis))
    action='init';
else
    action=seis;
end

if(strcmp(action,'init'))
    %look for an image plot
    %see if it is a plotimage figure
    if(strcmp(get(hparent,'type'),'figure'))
        haxe=get(hparent,'currentaxes');
        hfig=hparent;
    elseif(strcmp(get(hparent,'type'),'axes'))
        haxe=hparent;
        hfig=get(haxe,'parent');
    else
        error('hparent must be a figure or an axes')
    end
    %turn off the standard toolbar
    set(hfig,'toolbar','none')
    
    xlim=[min(x) max(x)];
    tlim=[min(t) max(t)];
    fullfilename=[];
    if(nargin<5)
        %see if we are opening up a saved pickfile
        [filename,pathname]=uigetfile('*.mat','Locate save pick file or cancel if none');
        if(filename~=0)
            %make sure we have a .mat file
            ind=strfind(filename,'.mat');
            if(isempty(ind))
                msgbox('Pick file must be a Matlab binary ending in .mat');
                return;
            end
            matObj=matfile([pathname filename]);
            varlist=who(matObj);
            if(length(varlist)>1 || ~strcmp(varlist,'pickstruc'))
                msgbox('File chosen is not recognizable as a saved pickfile');
                return
            end
            fullfilename=[pathname filename];
            pickstruc=[];
            %eval(['load ' fullfilename]);
            load(fullfilename);
            pickstruc=populate(pickstruc);%make sure it has all the fields and their sizes are the same
        else
            hornames=[];
            horcolors=[];
            horpicks=[];
            fullfilename=[];
            pickstruc.hornames=hornames;
            pickstruc.horcolors=horcolors;
            pickstruc.horpicks=horpicks;
            pickstruc=populate(pickstruc);
        end
    end
    hornames=pickstruc.hornames;
    horcolors=pickstruc.horcolors;
    horpicks=pickstruc.horpicks;
    
    nhors=length(hornames);
    
    %parse the input picks to see if they fall inside the seismic bounds
    for k=1:nhors
        %check color spec for RGB validity
        kol=horcolors{k};
        if(ischar(kol))
            switch kol
                case 'y'
                    horcolors{k}=[1 1 0];
                case 'm'
                    horcolors{k}=[1 0 1];
                case 'c'
                    horcolors{k}=[0 1 1];
                case 'r'
                    horcolors{k}=[1 0 0];
                case 'g'
                    horcolors{k}=[0 1 0];
                case 'b'
                    horcolors{k}=[0 0 1];
                case 'w'
                    horcolors{k}=[1 1 1];
                case 'k'
                    horcolors{k}=[0 0 0];
                otherwise
                    error(['horizon ' hornames{k} ' has invalid text color spec'])
            end
        else
            [r,c]=size(kol);
            if(r~=1 && c~=3)
                error(['horizon ' hornames{k} ' has invalid RGB color spec'])
            end
            if(any(kol)<0 || any(kol)>1)
                error(['horizon ' hornames{k} ' has invalid RGB color spec'])
            end
        end
    end
    
    %check picks for location validity
    hors2delete=zeros(1,nhors);
    for k=1:nhors
        tmp=horpicks{k};
        if(isempty(tmp))
            npicks=0;
        else
            if(~isnumeric(tmp))
                error(['horizon ' hornames{k} ' has unrecognizable pick matrix'])
            end
            [r,c]=size(tmp);
            if(c>3)
                error(['horizon ' hornames{k} ' has pick matrix with more than 3 columns']);
            elseif(c<2)
                error(['horizon ' hornames{k} ' has pick matrix with less than 2 columns']);
            elseif(c==2)
                hpicks=[tmp zeros(r,1)];
                horpicks{k}=hpicks;
            else
                hpicks=tmp;
            end
            xpicks=hpicks(:,2);
            npicks=length(xpicks);
            ind=find(isnan(xpicks));
            if(~isempty(ind))
                hpicks(ind,:)=[];
                horpicks{k}=hpicks;
                xpicks=hpicks(:,2);
            end
            ind=xpicks<xlim(1) | xpicks>xlim(2);
            nout=sum(ind);
            if(nout>0)
                hpicks(ind,:)=[];
                horpicks{k}=hpicks;
                npicks=size(hpicks,1);
                msgbox({['horizon ' hornames{k} ' has ' int2str(nout) ...
                    ' picks out of x bounds of seismic'],...
                    ['there are ' int2str(npicks) ' remaining']})
            end
            tpicks=hpicks(:,1);
            ilive=find(~isnan(tpicks));
            ind=tpicks(ilive)<tlim(1) | tpicks(ilive)>tlim(2);
            nout=sum(ind);
            if(nout>0)
                hpicks(ind,:)=[];
                horpicks{k}=hpicks;
                npicks=size(hpicks,1);
                msgbox({['horizon ' hornames{k} ' has ' int2str(nout) ...
                    ' picks out of t bounds of seismic'],...
                    ['there are ' int2str(npicks) ' remaining']})
            end
        end
        if(npicks==0)
            hors2delete(k)=1;
        end
    end
    
    if(sum(hors2delete>0))
        ind=hors2delete>0;
        hornames(ind)=[];
        horcolors(ind)=[];
        horpicks(ind)=[];
        pickstruc=deletehors(pickstruc,ihors);
    end
    
    nhors=length(hornames);
    
    %prep figure for picking
    picktool_prepfig(hfig,haxe);
    
    %ok now we need to make the controls
    set(haxe,'units','normalized');
    pos=get(haxe,'position');
    width=(1-pos(1)-pos(3));%this is the available width on the RHS
    %figure out width in pixels
    fpos=get(hfig,'position');
    widpix=width*fpos(3);
    if(widpix>110)
        widpix=110;
        width=widpix/fpos(3);
    end

    height=.025;
    sep=.005;
    
    nbuttonrows=4;
    pheight=nbuttonrows*(height+sep)+sep;
    pwidth=2*(width+sep)+sep;
    plly=pos(2)+pos(4)-pheight;
    pllx=pos(1)+pos(3)+sep;
    horpanel=uipanel(hparent,'position',[pllx plly pwidth pheight],'title','Horizon Autopicking');
    
    %the new horizon button 
    %Note: seismic is in this userdata
    sepp=sep/pwidth;
    widthp=width/pwidth;
    heightp=height/pheight;
    xnow=sepp;
    ynow=1-sepp-heightp;
%     xnow=pos(1)+pos(3)+sep;
%     ynow=pos(2)+pos(4)-height-sep;
    uicontrol(horpanel,'style','pushbutton','string','New horizon','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''newhor'')',...
        'tag','newhor','userdata',{seis,t,x},...
        'tooltipstring','Create a new horizon with auto-picking');
    %the load horizons button
    xnow=xnow+widthp+sepp;
    uicontrol(horpanel,'style','pushbutton','string','Load horizons ','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''loadhor'')',...
        'tag','loadhor','tooltipstring','Load horizons from a file containing a pickstruc');
    %the repick horizon button
    xnow=sepp;
    ynow=ynow-heightp-sepp;
    uicontrol(horpanel,'style','pushbutton','string','Repick horizon','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''repick'')',...
        'tag','repick','tooltipstring','re-run the autopicker on an exsisting horizon');
    %the delete horizon button
    xnow=xnow+widthp+sepp;
    uicontrol(horpanel,'style','pushbutton','string','Delete horizon','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''delete'')',...
        'tag','delete','tooltipstring','Delete one or more horizons');
    %the sort horizons button
    xnow=sepp;
    ynow=ynow-heightp-sepp;
    uicontrol(horpanel,'style','pushbutton','string','Sort horizons','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''sorthors'')',...
        'tag','sorthors','tooltipstring','Sort horizons by increasing average time');
    %the toggle legend button
    xnow=xnow+widthp+sepp;
    uicontrol(horpanel,'style','pushbutton','string','Legend on/off','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''legend'')',...
        'tag','legendoo','tooltipstring','Toggle the legend on and off',...
        'userdata',1);%1 means legend on 0 means off
    %the change color button
    xnow=sepp;
    ynow=ynow-heightp-sepp;
    uicontrol(horpanel,'style','pushbutton','string','Change horizon color','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''changecolor'')',...
        'tag','changecolor','tooltipstring','Change a horizon''s color');
    %the show horizon types button
    xnow=xnow+widthp+sepp;
    uicontrol(horpanel,'style','pushbutton','string','List defined horizons','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''showtype'')',...
        'tag','showtype','tooltipstring','Display a list of defined horizons and their pick-type');
    
    %edit panel
    nbuttonrows=4;
    pheight2=nbuttonrows*(height+sep)+4*sep;
    pwidth=2*(width+sep)+sep;
    plly=pos(2)+pos(4)-pheight-pheight2-2*sep;
    pllx=pos(1)+pos(3)+sep;
    hedpanel=uipanel(hparent,'title','Pick editing','position',[pllx plly pwidth pheight2]);
    heightp=height/pheight2;
    sepp=sep/pheight2;
    %the start editing button
    xnow=sepp;
    ynow=1-sepp-heightp;
    hstart=uicontrol(hedpanel,'style','pushbutton','string','Start editing picks','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''start'')',...
        'tag','start','userdata',[],...%userdata will be {anchorsarray,haxe} set later
        'tooltipstring','Start the point-wise editing of auto-picked horizons');
    %the clear anchors button
    xnow=xnow+widthp+sepp;
    uicontrol(hedpanel,'style','pushbutton','string','Clear anchors','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''clearanchors'')',...
        'tag','clearanchors','tooltipstring','Free all anchored (unmovable) points');
    %the stop and save button
    xnow=sepp;
    ynow=ynow-heightp-sepp;
    uicontrol(hedpanel,'style','pushbutton','string','Stop and save','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''stopsave'')',...
        'tag','stopsave','userdata',{pickstruc,fullfilename},...
        'tooltipstring','Stop editing and save the pickstruc in the existing file');
    %the stop and saveas button
    xnow=xnow+widthp+sepp;
    uicontrol(hedpanel,'style','pushbutton','string','Stop and save as','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''stopsaveas'')',...
        'tag','stopsaveas',...
        'tooltipstring','Stop editing and save the pickstruc in a new file');
    %the drag popup
    xnow=sepp;
    ynow=ynow-heightp-2*sepp;
    uicontrol(hedpanel,'style','popupmenu','string',{'Constant drag',...
        'Elastic drag'},'value',1,'units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''drag'')',...
        'tag','drag','tooltipstring','Change the way multiple points move with MB3');
    %the motion popup
    xnow=xnow+widthp+sepp;
    uicontrol(hedpanel,'style','popupmenu','string',{'Motion: t only',...
        'Motion: x only','Motion: x&t'},'value',1,'units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''motion'')',...
        'tag','motion',...
        'tooltipstring','Points are usually free to move in time only');
    %the linewidths popup
    lws=[4 3 2 1 .5 .33 .25];
    ind=near(lws,1);
    lwstr=cell(size(lws));
    for k=1:length(lws)
        lwstr{k}=['linewidths X ' num2str(lws(k))];
    end
    xnow=sepp;
    ynow=ynow-heightp-2*sepp;
    uicontrol(hedpanel,'style','popupmenu','string',lwstr,'value',ind,'units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''linewidths'')',...
        'userdata',lws,'tag','linewidths',...
        'tooltipstring','Change the widths of the horizon lines');
    
    %utilities panel
    nbuttonrows=2;
    pheight3=nbuttonrows*(height+sep)+2*sep;
    plly=pos(2)+pos(4)-pheight-pheight2-pheight3-4*sep;
    hutil=uipanel(hparent,'title','Utilities','position',[pllx plly pwidth pheight3]);
    heightp=height/pheight3;
    %sepp=sep/pheight3;
    %the zoom button
    xnow=sepp;
    ynow=1-sepp-heightp;
    uicontrol(hutil,'style','pushbutton','string','Zoom','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''zoom'')',...
        'tag','zoom');
    %the unzoom button
    tmin=min(t);tmax=max(t);xmin=min(x);xmax=max(x);
    xnow=xnow+widthp+sepp;
    uicontrol(hutil,'style','pushbutton','string','UnZoom','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''unzoom'')',...
        'tag','unzoom','userdata',[xmin xmax tmin tmax]);
    %the help button
    xnow=sepp;
    ynow=ynow-heightp-3*sepp;
    uicontrol(hutil,'style','pushbutton','string','Help','units','normalized',...
        'position',[xnow,ynow,widthp,heightp],'callback','picktool(''help'')',...
        'tag','help');
    
    %plot the picks
    linewidths=getcurrentlinewidth*ones(size(hornames));
    if(~isempty(hornames))
        hhors=pickplot(haxe,hornames,horcolors,horpicks,linewidths);
    else
        hhors=[];
    end
    
    %make an initial anchor array with no anchors and all lines ready for
    %editing
    anchorarray=cell(nhors,3);
    for k=1:nhors
        anchorarray{k,1}=hhors(k);
        anchorarray{k,2}=eps;
        anchorarray{k,3}=[];
    end
    set(hstart,'userdata',{anchorarray,haxe})
    
    %disable any windowbuttonfcns
    set(hfig,'windowbuttondownfcn','','windowbuttonupfcn','',...
        'windowbuttonmotionfcn','');
    
elseif(strcmp(action,'start'))
    hstart=findobj(gcf,'tag','start');
    bgc=get(hstart,'backgroundcolor');
    if(sum(bgc)==2); return; end%this is in case the button gets clicked a secon time when it is already on
    udat=get(hstart,'userdata');
    anchorarray=udat{1};
    if(isempty(anchorarray))
        msgbox({'You have no horizons!!','You must define a horizon before you can edit points'})
        return
    end
    %save the windowbutton functions
    picktool('savebuttonfcns');
    %start editlines
    editlinesinit(anchorarray);
    hmotion=findobj(gcf,'tag','motion');
    val=get(hmotion,'value');
    switch val
        case 1
            editlines('yonly');
        case 2
            editlines('xonly');
        case 3
            editlines('xandy');
    end
    hdrag=findobj(gcf,'tag','drag');
    val=get(hdrag,'value');
    switch val
        case 1
            editlines('constantdrag');
        case 2
            editlines('elasticdrag');
    end
    msgbox({'Click on the horizon to edit.', ...
        'Perhaps zoom first using the eyglass tool.',...
        'Click off the line to pause editing.',...
        'Click on any other line to edit it.'...
        'Push ''Stop and save'' button to save picks to file.'})
    set(hstart,'backgroundcolor',[1 1 0]);%this is a flag that editing is on
elseif(strcmp(action,'stopsave')||strcmp(action,'stopsaveas')||strcmp(action,'stop'))
    %the 'stop' action turns off editing without saving to disk
    hstopsave=findobj(gcf,'tag','stopsave');
    hstart=findobj(gcf,'tag','start');
    val=get(hstart,'backgroundcolor');
    udat2=get(hstart,'userdata');
    if(isempty(udat2{1}))
        if(~strcmp(action,'stop'))
            msgbox('You have no horizons so there is nothing to save!!')
        end
        return
    end
    %     if(sum(val)>2)
    %         if(~strcmp(action,'stop'))
    %             msgbox('editing is not turned on so there is nothing to save');
    %         end
    %         return;
    %     end
    
    udat=get(hstopsave,'userdata');
    if(strcmp(action,'stopsaveas'))
        udat{2}=[];%ensures that we don't write on top of the input file
    end
    if(isempty(udat{2})&&~strcmp(action,'stop'))
        %need to ask for an output file
        [file,path]=uiputfile('*.mat','Designate the output file for saved picks');
        if(file==0)
            fullfilename=[];
        else
            fullfilename=[path file];
        end
    else
        fullfilename=udat{2};
    end
    
    if(sum(val)==2)
        anchorarray=editlinesfini;
    else
        anchorarray=udat2{1};
    end
    %restore the windowbutton fcns
    picktool('restorebuttonfcns')
    %now get the updated picks from the lines
    pickstruc=udat{1};
    hornames=pickstruc.hornames;
    horcolors=pickstruc.horcolors;
    horpicks=pickstruc.horpicks;
    
    %anchorarray=udat2{1};
    udat2{1}=anchorarray;
    xseis=getseismic_x;
    nhors=size(anchorarray,1);
    for k=1:nhors
        hhor=anchorarray{k,1};
        hname=get(hhor,'tag');
        if(strcmp(hname,hornames{k}))%these should match or logic failed
            tdata=get(hhor,'ydata');
            xdata=get(hhor,'xdata');
            xdata=xreconcile(xdata,xseis);
            horpicks{k}=[tdata(:) xdata(:) zeros(length(tdata),1)];
            horcolors{k}=get(hhor,'color');
        else
            horpicks{k}=[];
            horcolors{k}=[1 0 0];
        end
    end
    pickstruc.horcolors=horcolors;
    pickstruc.horpicks=horpicks;
    udat{1}=pickstruc;
    set(hstopsave,'userdata',udat);
    set(hstart,'userdata',udat2);
    if(~isempty(fullfilename)&&~strcmp(action,'stop'))
        %eval(['save ' fullfilename ' pickstruc'])
        save(fullfilename,'pickstruc');
        msgbox(['picks saved to ' fullfilename])
    else
        if(~strcmp(action,'stop'))
            msgbox('Picks were not saved to disk because you cancelled the file dialog');
        end
    end
    set(hstart,'backgroundcolor',.94*[1 1 1]);
elseif(strcmp(action,'drag'))
    hdrag=findobj(gcf,'tag','drag');
    hstart=findobj(gcf,'tag','start');
    kol=get(hstart,'backgroundcolor');
    editing='off';
    if(sum(kol)==2)
        editing='on';
    end
    if(strcmp(editing,'on'))
        val=get(hdrag,'value');
        switch val
            case 1
                editlines('constantdrag');
            case 2
                editlines('elasticdrag');
        end
    end
elseif(strcmp(action,'motion'))
    hdrag=findobj(gcf,'tag','motion');
    hstart=findobj(gcf,'tag','start');
    kol=get(hstart,'backgroundcolor');
    editing='off';
    if(sum(kol)==2)
        editing='on';
    end
    if(strcmp(editing','on'))
        val=get(hdrag,'value');
        switch val
            case 1
                editlines('yonly');
            case 2
                editlines('xonly');
            case 3
                editlines('xandy')
        end
    end
elseif(strcmp(action,'changecolor'))
    %turn off editing
    picktool('stop');
    %get the horizon names and colors
    hstopsave=findobj(gcf,'tag','stopsave');
    hstart=findobj(gcf,'tag','start');
    udat=get(hstopsave,'userdata');
    pickstruc=udat{1};
    hornames=pickstruc.hornames;
    horcolors=pickstruc.horcolors;
    udat2=get(hstart,'userdata');
    anchorarray=udat2{1};
    pos=get(gcf,'position');
    posdialog=[pos(1)+.5*pos(3),pos(2)+.5*pos(4)];
    if(isempty(hornames))
        msgbox({'You have no horizons!!',' It is difficult to change the color of nothing...'});
        return;
    end
    newcolors=picktool_colorchooser(hornames,horcolors,...
        'Choose new horizon colors',posdialog);
    if(~iscell(newcolors))
        return;
    end
    nhors=length(newcolors);
    for k=1:nhors
        set(anchorarray{k,1},'color',newcolors{k})
    end
    pickstruc.horcolors=newcolors;
    udat{1}=pickstruc;
    set(hstopsave,'userdata',udat);
elseif(strcmp(action,'newhor')||strcmp(action,'repick'))
    %turn off editing, this will also save the window button functions
    if(strcmp(action,'newhor'))
        picktool('stop');
        %get the horizon names and colors
        hstopsave=findobj(gcf,'tag','stopsave');
        hstart=findobj(gcf,'tag','start');
        udat=get(hstopsave,'userdata');
        pickstruc=udat{1};
        pickstruc=newhor(pickstruc);%expand the pickstruc
        hornames=pickstruc.hornames;
        horcolors=pickstruc.horcolors;
        nhors=length(hornames);
        hornames{nhors}='';
        horcolors{nhors}='';
        udat2=get(hstart,'userdata');
        anchorarray=udat2{1};
        pos=get(gcf,'position');
        posdialog=[pos(1)+.5*pos(3),pos(2)+.5*pos(4)];
        [newcolors,newnames]=picktool_colorchooser(hornames,horcolors,...
            'Choose a color for the new horizon',posdialog);
        if(~iscell(newcolors))
            return;
        end
        nhors=length(newcolors);
        anchorarray=[anchorarray;cell(1,3)];
        anchorarray{nhors,2}=eps;
        for k=1:nhors
            %we do this because it is possible to change any horizon's color in the
            %choose new color dialog. The new horizon does not exist yet
            %and its handle in anchorsarray is null. Matlab handles this
            %gracefully
            set(anchorarray{k,1},'color',newcolors{k})
        end
        udat2{1}=anchorarray;
        pickstruc.hornames=newnames;
        pickstruc.horcolors=newcolors;
        udat{1}=pickstruc;
        set(hstopsave,'userdata',udat);
        set(hstart,'userdata',udat2);
    end
    if(strcmp(action,'newhor')||strcmp(action,'repick'))
        if(strcmp(action,'repick'))
            picktool('stop');
            hstopsave=findobj(gcf,'tag','stopsave');
            hstart=findobj(gcf,'tag','start');
        end
        %get the horizon names and colors
        udat=get(hstopsave,'userdata');
        pickstruc=udat{1};
        hornames=pickstruc.hornames;
        horcolors=pickstruc.horcolors;
        nhors=length(hornames);
        udat2=get(hstart,'userdata');
        anchorarray=udat2{1};
        haxe=udat2{2};
        hfig=get(haxe,'parent');
        picktooldata.seismic=getseismic;
        picktooldata.seismicx=getseismic_x;
        picktooldata.seismict=getseismic_t;
        picktooldata.hparentfig=hfig;
        picktooldata.hparentaxe=haxe;
        %if repicking, choose which horizon
        if(strcmp(action,'repick'))
            ihor=choosehor(hornames);
            if(isempty(ihor))
                picktool('restorebuttonfcns');
                return;
            end
            %delete the current line
            delete(anchorarray{ihor,1});
            anchorarray{ihor,1}=[];
            udat2{1}=anchorarray;
            set(hstart,'userdata',udat2);
            msgbox({'Now adjust/refine the guide points for the event. Suggested procedure:',...
                '(1) Zoom in time (not space) around the event. Your previous guide points are already shown.',...
                '(2) Recall if you are picking a peak, trough, or zero crossing.',...
                '(3) Then cick the left mouse button to define new guide points for the autopicker.',...
                '    Make clicks sparsely every time the event changes dip or character.',...
                '    Try to click on a consistent phase (peak, trough, or zero crossing).',...
                '    Click again on an existing point to delete it.',...
                '(4) When finished defining the guide points, click the right mouse button to signal that you are done.',...
                'NOTE: the autopicker will not pick beyond the first point or the last point.',...
                'So to pick across the section you need guide points at the ends of the section.'},...
                'Instructions for re-picking the guide points');
        else
            ihor=nhors;
            msgbox({'Now define the guide points for the new event. Suggested procedure:',...
                '(1) Zoom in time (not space) around the event.',...
                '(2) Decide if you are picking a peak, trough, or zero crossing.',...
                '(3) Then cick the left mouse button to define guide points for the autopicker.',...
                '    Make clicks sparsely every time the event changes dip or character.',...
                '    Try to click on a consistent phase (peak, trough, or zero crossing).',...
                '    Click again on an existing point to delete it.',...
                '(4) When finished defining the guide points, click the right mouse button to signal that you are done.',...
                'NOTE: the autopicker will not pick beyond the first point or the last point.',...
                'So to pick across the section you need guide points at the ends of the section.'},...
                'Instructions for picking the guide points');
        end
        picktooldata.guidepoints=pickstruc.horguides{ihor};
        picktooldata.color=horcolors{ihor};
        picktooldata.name=hornames{ihor};
        if(strcmp(action,'newhor'))
            methods=picker('methods');
            picktooldata.transfer='picktool(''newhor2'')';
            picktooldata.picktype=methods{4};%default picking method is closest peak
        elseif(strcmp(action,'repick'))
            picktooldata.picktype=pickstruc.horpicktypes{ihor};
            picktooldata.transfer='picktool(''repick2'')';
        end
        picktooldata.hornumber=ihor;
        picktool_newevent('init',picktooldata);
    end
elseif(strcmp(action,'newhor2')||strcmp(action,'repick2'))
    choice=questdlg('Keep this result?','Decision time','Yes','Autopick again','No','Yes');
    switch choice
        case 'Yes'
            %ok we keep it
            hstopsave=findobj(gcf,'tag','stopsave');
            hstart=findobj(gcf,'tag','start');
            udat2=get(hstart,'userdata');
            anchorarray=udat2{1};
            haxe=udat2{2};
            hmasterfig=get(haxe,'parent');
            udat=get(hstopsave,'userdata');
            pickstruc=udat{1};
            hornames=pickstruc.hornames;
            horcolors=pickstruc.horcolors;
            
            hornew=get(gca,'userdata');
            xg=get(hornew.trajhandle,'xdata');
            tg=get(hornew.trajhandle,'ydata');
            ihor=hornew.hornumber;%horizon number
            delete(hornew.trajhandle);
            tp=hornew.tpick(:);
            xp=hornew.xpick(:);
            ap=hornew.amppick(:);
            ae=hornew.ampevent(:);
            pickstruc.horpicktypes{ihor}=hornew.picktype;
            pickstruc.horattrnames{ihor}={'amppick','ampevent'};
            pickstruc.horfwys{ihor}=hornew.delt;
            pickstruc.horguides{ihor}=[tg(:) xg(:) zeros(length(xg),1)];
            
            %check for missing picks
            xseis=getseismic_x;
            xseis=xseis(:);
            ind=find(isnan(tp), 1);
            xmin=min(xseis);
            xmax=max(xseis);
            xminp=min(xp);
            xmaxp=max(xp);
            choices={'fill and extend','fill','extend','do nothing'};
            ians=zeros(1,4);
            if(~isempty(ind))
                ians(2)=1;
            end
            if(xminp>xmin || xmaxp<xmax)
                ians(3)=1;
            end
            if(sum(ians)==2)
                ians(1)=1;
            end
            if(sum(ians)>0)
                ians(4)=1;
            end
            if(sum(ians)>0)
                if(ians(1)==1)
                    question='The autopick result contains holes (missing picks) and does not extend to the section boundaries. What do you want to do about this?';
                elseif(ians(2)==1)
                    question='The autopick result contains holes (missing picks). What do you want to do about this?';
                elseif(ians(3)==1)
                    question='The autopick result does not extend to the section boundaries. What do you want to do about this?';
                end
                pos=get(hmasterfig,'position');
                posxy=[pos(1)+.5*pos(3),pos(2)+.5*pos(4)];
                choice=multichoicedlg(question,choices(logical(ians)),'Decision required',posxy);
                if(~strcmp(choice,'do nothing'))
                    [xpe,tpe]=fillextend(xp,tp,xseis,choice);
                    [xpe,ape]=fillextend(xp,ap,xseis,choice);
                    [xpe,aee]=fillextend(xp,ae,xseis,choice);
                end
            end
            pickstruc.horpicks{ihor}=[tpe xpe];
            pickstruc.horattrs{ihor}=[ape aee];
            horpicks=pickstruc.horpicks;
            udat{1}=pickstruc;
            set(hstopsave,'userdata',udat);
            
            %delete the existing horizons
            delete(hornew.handle);
            nhors=length(hornames);
            for k=1:nhors
                hl=anchorarray{k,1};
                if(~isempty(hl))
                    delete(hl);
                end
            end
            %now replot
            linewidths=getcurrentlinewidth*ones(size(hornames));
            hhors=pickplot(haxe,hornames,horcolors,horpicks,linewidths);
            %update the anchorsarray
            for k=1:nhors
                anchorarray{k,1}=hhors(k);
            end
            udat2{1}=anchorarray;
            set(hstart,'userdata',udat2);
            picktool('restorebuttonfcns');
        case 'Autopick again'
            hornew=get(gca,'userdata');
            delete(hornew.handle);
            picktool_newevent('newptalt');%This causes a repick
        case 'No'
            hornew=get(gca,'userdata');
            delete(hornew.handle);
            delete(hornew.trajhandle);
            ihor=hornew.hornumber;%the horizon number
            hstart=findobj(gcf,'tag','start');
            udat2=get(hstart,'userdata');
            anchorarray=udat2{1};
            haxe=udat2{2};
            hstopsave=findobj(gcf,'tag','stopsave');
            udat=get(hstopsave,'userdata');
            pickstruc=udat{1};
            hornames=pickstruc.hornames;
            horcolors=pickstruc.horcolors;
            horpicks=pickstruc.horpicks;
            %nhors=length(pickstruc.hornames);
            if(~strcmp(action,'repick2'))
                pickstruc=deletehors(pickstruc,ihor);
                udat{1}=pickstruc;
                set(hstopsave,'userdata',udat);
                hstart=findobj(gcf,'tag','start');
                udat=get(hstart,'userdata');
                anchorarray=udat{1};
                anchorarray(ihor,:)=[];
                udat{1}=anchorarray;
                set(hstart,'userdata',udat);
                msgbox('Ok, new event discarded');
            else
                %now replot
                nhors=length(hornames);
                linewidths=getcurrentlinewidth*ones(1,nhors);
                hhors=pickplot(haxe,hornames,horcolors,horpicks,linewidths);
                %update the anchorsarray
                for k=1:nhors
                    anchorarray{k,1}=hhors(k);
                end
                udat2{1}=anchorarray;
                set(hstart,'userdata',udat2);
                msgbox('Ok, repick discarded, previous picks retained');
            end
            picktool('restorebuttonfcns');
    end
elseif(strcmp(action,'delete'))
    %need to delete one or more horizons
    picktool('stop');
    %get the horizon names and colors
    hstopsave=findobj(gcf,'tag','stopsave');
    hstart=findobj(gcf,'tag','start');
    udat=get(hstopsave,'userdata');
    pickstruc=udat{1};
    %     hornames=pickstruc.hornames;
    %     horcolors=pickstruc.horcolors;
    %     horpicks=pickstruc.horpicks;
    udat2=get(hstart,'userdata');
    anchorarray=udat2{1};
    haxe=udat2{2};
    if(isempty(pickstruc.hornames'))
        msgbox({'You have no horizons!!','You need at least one horizon in order to delte something...'})
        return;
    end
    tokill=listdlg('promptstring','Select one or more horizons to delete',...
        'selectionmode','muultiple','Name','Horizon deletion',...
        'liststring',pickstruc.hornames);
    if(~isempty(tokill))
        choice=questdlg(char('These horizons will be deleted: ',...
            pickstruc.hornames{1,tokill}),'Warning!','Yes','No','Yes');
        switch choice
            case 'No'
                return
            case 'Yes'
                nhors=length(pickstruc.hornames);
                %                 hornames(tokill)=[];
                %                 horcolors(tokill)=[];
                %                 horpicks(tokill)=[];
                %                 pickstruc.hornames=hornames;
                %                 pickstruc.horcolors=horcolors;
                %                 pickstruc.horpicks=horpicks;
                pickstruc=deletehors(pickstruc,tokill);
                udat{1}=pickstruc;
                set(hstopsave,'userdata',udat);
                for k=1:nhors
                    %delete them all and replot the survivors
                    delete(anchorarray{k,1});
                end
                anchorarray(tokill,:)=[];
                %replot
                hornames=pickstruc.hornames;
                horcolors=pickstruc.horcolors;
                horpicks=pickstruc.horpicks;
                linewidths=getcurrentlinewidth*ones(size(hornames));
                hhors=pickplot(haxe,hornames,horcolors,horpicks,linewidths);
                %update the anchorsarray
                nhors=length(hhors);
                for k=1:nhors
                    anchorarray{k,1}=hhors(k);
                end
                udat2{1}=anchorarray;
                set(hstart,'userdata',udat2);
        end
    end
elseif(strcmp(action,'linewidths'))
    hmasterfig=gcf;
    %get the horizon handle from the anchors array
    picktool('stop');
    hlw=findobj(hmasterfig,'tag','linewidths');
    lws=get(hlw,'userdata');
    ilw=get(hlw,'value');
    %get the horizon names and colors;
    hstart=findobj(hmasterfig,'tag','start');
    udat=get(hstart,'userdata');
    anchorarray=udat{1};
    nhors=size(anchorarray,1);
    lwnormal=2;
    for k=1:nhors
        set(anchorarray{k,1},'linewidth',lws(ilw)*lwnormal);
    end
elseif(strcmp(action,'showtype'))
    %get the pickstruc
    hstopsave=findobj(gcf,'tag','stopsave');
    udat=get(hstopsave,'userdata');
    pickstruc=udat{1};
    pos=get(gcf,'position');
    d=figure('position',[pos(1)+.5*pos(3),pos(2)+.5*pos(4), 400 300]);
    set(d,'numbertitle','off','name','Horizon Types');
    list=cell(1,length(pickstruc.hornames));
    for k=1:length(list)
        if(isempty(pickstruc.horpicktypes{k}))
            picktype='unknown';
        else
            picktype=pickstruc.horpicktypes{k};
        end
        list{k}=[pickstruc.hornames{k} ' : ' picktype];
    end
    uicontrol(d,'style','text','string','List of horizons and their picktypes',...
        'units','normalized','position',[.1 .85 .8, .05]);
    uicontrol(d,'units','normalized','position',[.1 .1 .8 .7],'style','listbox',...
        'string',list);
elseif(strcmp(action,'clearanchors'));
    hmasterfig=gcf;
    picktool('stop');
    %get the horizon names and colors
    hstart=findobj(hmasterfig,'tag','start');
    udat2=get(hstart,'userdata');
    if(isempty(udat2{1}))
        return;
    end
    anchorarray=udat2{1};
    nanch=size(anchorarray,1);
    for k=1:nanch
        anchorarray{k,2}=eps;
        anchorarray{k,3}=[];
    end
    udat2{1}=anchorarray;
    set(hstart,'userdata',udat2);
    
elseif(strcmp(action,'help'))
%     file = which('PickTool_Help.htm');
%     web(file);
    file = which('PickTool_Help.pdf');
    winopen(file);
    
elseif(strcmp(action,'loadhor'))
    hmasterfig=gcf;
    picktool('stop');
    hstart=findobj(hmasterfig,'tag','start');
    hstopsave=findobj(hmasterfig,'tag','stopsave');
    udat=get(hstopsave,'userdata');
    udat2=get(hstart,'userdata');
    anchorarray=udat2{1};
    if(~isempty(anchorarray))
        %determine if we are merging or discarding
        choice=questdlg('Discard exsiting horizons or merge with new?',...
            'Existing horizons?','discard','merge','merge');
        if(isempty(choice))%happens if they click the window close button
            choice='merge';
        end
        oldpickstruc=udat{1};
    else
        choice='new';
        oldpickstruc=[];
    end
    [filename,pathname]=uigetfile('*.mat','Locate exsisting pick file');
    if(filename~=0)
        %make sure we have a .mat file
        ind=strfind(filename,'.mat');
        if(isempty(ind))
            msgbox('Pick file must be a Matlab binary ending in .mat');
            return;
        end
        matObj=matfile([pathname filename]);
        varlist=who(matObj);
        if(length(varlist)>1 || ~strcmp(varlist,'pickstruc'))
            msgbox('File chosen is not recognizable as a saved pickfile');
            return
        end
        fullfilename=[pathname filename];
        pickstruc=[];
        eval(['load ' fullfilename]);
        pickstruc=populate(pickstruc);%make sure it has all the fields and their sizes are the same
    else
        msgbox('Load cancelled')
        return
    end
    
    if(strcmp(choice,'merge'))
        pickstruc=merge(oldpickstruc,pickstruc);
        fullfilename=[];%prevents accidental save on top of the old file
    end
    
    %delete the existing horizon lines
    nhors=size(anchorarray,1);
    for k=1:nhors
        delete(anchorarray{k,1});
    end
    
    %now plot
    nhors=length(pickstruc.hornames);
    haxe=udat2{2};
    linewidths=getcurrentlinewidth*ones(1,nhors);
    hhors=pickplot(haxe,pickstruc.hornames,pickstruc.horcolors,...
        pickstruc.horpicks,linewidths);
    
    %make a new anchorarray
    anchorarray=cell(nhors,3);
    for k=1:nhors;
        anchorarray{k,1}=hhors(k);
        anchorarray{k,2}=eps;
        anchorarray{k,3}=[];
    end
    
    %put things back in userdata
    udat2{1}=anchorarray;
    set(hstart,'userdata',udat2);
    
    udat{1}=pickstruc;
    udat{2}=fullfilename;
    set(hstopsave,'userdata',udat);
elseif(strcmp(action,'sorthors'))
    hmasterfig=gcf;
    picktool('stop');
    hstart=findobj(hmasterfig,'tag','start');
    hstopsave=findobj(hmasterfig,'tag','stopsave');
    udat=get(hstopsave,'userdata');
    udat2=get(hstart,'userdata');
    anchorarray=udat2{1};
    haxe=udat2{2};
    pickstruc=udat{1};
    nhors=length(pickstruc.hornames);
    if(nhors==0)
        msgbox('You have no horizons to sort!!')
        return;
    end
    [pickstruc,isort]=sortpicks(pickstruc);
    anchorarray=anchorarray(isort,:);
    %delete horizons
    for k=1:nhors
        delete(anchorarray{k,1});
    end
    %replot
    linewidths=getcurrentlinewidth*ones(1,nhors);
    hhors=pickplot(haxe,pickstruc.hornames,pickstruc.horcolors,...
        pickstruc.horpicks,linewidths);
    
    for k=1:nhors
        anchorarray{k,1}=hhors(k);
    end
    
    udat2{1}=anchorarray;
    udat{1}=pickstruc;
    set(hstart,'userdata',udat2);
    set(hstopsave,'userdata',udat);
elseif(strcmp(action,'restorebuttonfcns'))
    hrepick=findobj(gcf,'tag','repick');
    wbfcn=get(hrepick,'userdata');
    if(~isempty(wbfcn))
        set(gcf,'windowbuttondownfcn',wbfcn{1});
        set(gcf,'windowbuttonupfcn',wbfcn{2});
    end
elseif(strcmp(action,'savebuttonfcns'))
    hrepick=findobj(gcf,'tag','repick');
    wbdfcn=get(gcf,'windowbuttondownfcn');
    wbufcn=get(gcf,'windowbuttonupfcn');
    set(hrepick,'userdata',{wbdfcn, wbufcn});
elseif(strcmp(action,'legend'))
    hlegend=findobj(gcf,'tag','legendoo');
    state=get(hlegend,'userdata');
    if(state==1)
        %we are toggleing off
        legend off
        set(hlegend,'userdata',0);
    else
        hstopsave=findobj(gcf,'tag','stopsave');
        hstart=findobj(gcf,'tag','start');
        udat0=get(hstart,'userdata');
        anchorsarray=udat0{1};
        udat0=get(hstopsave,'userdata');
        pickstruc=udat0{1};
        anch=zeros(size(anchorsarray(:,1)));
        for kk=1:length(anch)
            anch(kk)=anchorsarray{kk,1};
        end
        legend(anch,pickstruc.hornames);
        set(hlegend,'userdata',1)
    end
elseif(strcmp(action,'zoom'))
    hzoom=findobj(gcf,'tag','zoom');
    fcnd=get(gcf,'windowbuttondownfcn');
    fcnu=get(gcf,'windowbuttonupfcn');
    fcnm=get(gcf,'windowbuttonmotionfcn');
    set(hzoom,'userdata',{fcnd fcnu fcnm});
    set(gcf,'pointer','arrow');
    set(gcf,'windowbuttondownfcn','selbox(''init'',1)');
    set(gcf,'windowbuttonmotionfcn','');
    set(gcf,'windowbuttonupfcn','selbox(''fini'',1);picktool(''zoom2'')')
elseif(strcmp(action,'zoom2'))
    box=selboxfini;
    if(isempty(box)), return; end
    try
        delete(box{2});
    catch
        %selbox rectangle already deleted
    end
    box = box{1}; %strip off cell array containing handle to selbox rectangle
    if(length(box)<4); return; end
    %get axis handle
    hstart=findobj(gcf,'tag','start');
    udat=get(hstart,'userdata');
    haxe=udat{2};
    %set(gcf,'currentaxes',haxe);
    xminnew=min(box([1 3]));
    xmaxnew=max(box([1 3]));
    tminnew=min(box([2 4]));
    tmaxnew=max(box([2 4]));
    %get the seismic x and t
    hnewhor=findobj(gcf,'tag','newhor');
    udat2=get(hnewhor,'userdata');
    t=udat2{2};
    x=udat2{3};
    xmin=min(x);xmax=max(x);
    tmin=min(t);tmax=max(t);
    if(xminnew<xmin); xminnew=xmin; end
    if(xmaxnew>xmax); xmaxnew=xmax; end
    if(tminnew<tmin); tminnew=tmin; end
    if(tmaxnew>tmax); tmaxnew=tmax; end
    set(haxe,'xlim',[xminnew xmaxnew],'ylim',[tminnew tmaxnew]);
    %restore previous window button fcns
    hzoom=findobj(gcf,'tag','zoom');
    udat=get(hzoom,'userdata');
    set(gcf,'windowbuttondownfcn',udat{1},'windowbuttonupfcn',udat{2},...
        'windowbuttonmotionfcn',udat{3});
    
    
elseif(strcmp(action,'unzoom'))
    hunzoom=findobj(gcf,'tag','unzoom');
    hstart=findobj(gcf,'tag','start');
    udat1=get(hunzoom,'userdata');
    udat2=get(hstart,'userdata');
    haxe=udat2{2};
    tmin=udat1(3);tmax=udat1(4);
    xmin=udat1(1);xmax=udat1(2);
    set(haxe,'xlim',[xmin xmax],'ylim',[tmin tmax]);
    
end

end

function xdata2=xreconcile(xdata,xseis)
% the picks may have x coordinates that differ slightly from those of the
% seismic. Here we associate each xpick with the closest xseis. If xseis is
% regular, then this is much easier to do so we test for that
xdata2=xdata;
test=sum(abs(diff(diff(xseis))));%will be zero for a regular grid.
small=.00000000001;
if(test<small)
    %seismic is on a regular grid
    dx=xseis(2)-xseis(1);
    x0=xseis(1);
    for kkk=1:length(xdata)
        xdata2(kkk)=round((xdata(kkk)-x0)/dx)*dx+x0;
    end
else
    %not regular. This will be very slow
    for kkk=1:length(xdata)
        if(~isnan(xdata(kkk)))
            indy=near(xseis,xdata(kkk));
            xdata2(kkk)=xseis(indy(1));
        end
    end
end

end

function [xp2,tp2]=fillextend(xp,tp,xseis,choice)

tp2=tp;
xp2=xp;

if(strcmp(choice,'fill')||strcmp(choice,'fill and extend'))
    %fill interior holes
    indy=find(~isnan(tp));
    xminn=min(xp(indy));xmaxx=max(xp(indy));
    ind2=between(xminn,xmaxx,xseis,2);
    tp2=interp1(xp(indy),tp(indy),xseis(ind2),'linear','extrap');
    xp2=xseis(ind2);
end
if(strcmp(choice,'extend')||strcmp(choice,'fill and extend'));
    
    indy=find(xseis<xp(1));
    if(~isempty(indy))
        xp2=[xseis(indy);xp2];
        tp2=[tp2(1)*ones(length(indy),1);tp2];
    end
    indy=find(xseis>xp2(end));
    if(~isempty(indy))
        xp2=[xp2;xseis(indy)];
        tp2=[tp2;tp2(end)*ones(length(indy),1)];
    end
end

end

function choice=multichoicedlg(question,choices,title,posxy)


width=200;
height=200;
d=dialog('position',[posxy width height],'name',title);
ynow=.5;
xnow=.1;
width=.8;
height=.4;
uicontrol(d,'style','text','string',question,'units','normalized',...
    'position',[xnow,ynow,width,height]);

ynow=.3;
height=.1;
uicontrol(d,'style','popup','units','normalized','position',...
    [xnow,ynow,width,height],'string',choices,'callback',@popupcallback);

ynow=.1;
height=.1;
uicontrol(d,'style','pushbutton','units','normalized','position',...
    [xnow,ynow,width,height],'callback','delete(gcf)','string','Done');


choice=choices{1};

uiwait(d);

    function popupcallback(popup,callbackdata)
        ichoice=popup.Value;
        popup_items=popup.String;
        choice=popup_items{ichoice};
    end

end

function pickstruc=populate(pickstruc)
% ensure that the pickstruc has all the required fields. If any fields
% exist and are of the wrong size, then they will be reset to nulls
%first make sure we have the required fields hornames, horcolors,
%horpicks
n=0;
fields=fieldnames(pickstruc);
for k=1:length(fields)
    if(strcmp(fields{k},'hornames'))
        n=n+1;
    elseif(strcmp(fields{k},'horcolors'))
        n=n+1;
    elseif(strcmp(fields{k},'horpicks'))
        n=n+1;
    end
end
if(n~=3)
    error('pick structure does not have the three requiried fields: hornames, horcolors, horpicks')
end
nhors=length(pickstruc.hornames);
if(length(pickstruc.horcolors)~=nhors)
    error('fields hornames and horcolors must be the same length')
end
if(length(pickstruc.horpicks)~=nhors)
    error('fields hornames and horpicks must be the same length')
end
if(isfield(pickstruc,'horattrs'))
    if(length(pickstruc.horattrs)~=nhors)
        pickstruc.horattrs=cell(1,nhors);
    end
else
    pickstruc.horattrs=cell(1,nhors);
end
if(isfield(pickstruc,'horattrnames'))
    if(length(pickstruc.horattrnames)~=nhors)
        pickstruc.horattrnames=cell(1,nhors);
    end
else
    pickstruc.horattrnames=cell(1,nhors);
end
if(isfield(pickstruc,'horpicktypes'))
    if(length(pickstruc.horpicktypes)~=nhors)
        pickstruc.horpicktypes=cell(1,nhors);
    end
else
    pickstruc.horpicktypes=cell(1,nhors);
end
if(isfield(pickstruc,'horguides'))
    if(length(pickstruc.horguides)~=nhors)
        pickstruc.horguides=cell(1,nhors);
    end
else
    pickstruc.horguides=cell(1,nhors);
end
if(isfield(pickstruc,'horfwys'))
    if(length(pickstruc.horfwys)~=nhors)
        pickstruc.horfwys=cell(1,nhors);
    end
else
    pickstruc.horfwys=cell(1,nhors);
end


end

function pickstruc=deletehors(pickstruc,ihors)
fields=fieldnames(pickstruc);
for k=1:length(fields)
    thisfield=getfield(pickstruc,fields{k});
    thisfield(ihors)=[];
    pickstruc=setfield(pickstruc,fields{k},thisfield);
end
end

function pickstruc=newhor(pickstruc)
fields=fieldnames(pickstruc);
nhors=length(pickstruc.hornames)+1;
for k=1:length(fields)
    thisfield=getfield(pickstruc,fields{k});
    thisfield{nhors}=[];
    pickstruc=setfield(pickstruc,fields{k},thisfield);
end
end

function pickstruc3=merge(pickstruc1,pickstruc2)
    nhors1=length(pickstruc1.hornames);
    nhors2=length(pickstruc2.hornames);

    %check for duplicate names
    for khor=1:nhors2
        for jhor=1:nhors1
            if(strcmp(pickstruc2.hornames{khor},pickstruc1.hornames{jhor}))
                pickstruc2.hornames{khor}=[pickstruc2.hornames{khor} '2'];
            end
        end
    end
    %determine mean times of each event to get sort order
    tmean1=zeros(1,nhors1);
    tmean2=zeros(1,nhors2);
    for khor=1:nhors1
        picks1=pickstruc1.horpicks{khor};
        ind1= ~isnan(picks1(:,1));
        tmean1(khor)=mean(picks1(ind1,1));
    end
    for khor=1:nhors2
        picks1=pickstruc2.horpicks{khor};
        ind1= ~isnan(picks1(:,1));
        tmean2(khor)=mean(picks1(ind1,1));
    end
    tmean3=[tmean1 tmean2];
    [tmp,isort]=sort(tmean3);
    %ok now merge hornames
    tmp=[pickstruc1.hornames pickstruc2.hornames];
    pickstruc3.hornames=tmp(isort);
    %merge horcolors
    tmp=[pickstruc1.horcolors pickstruc2.horcolors];
    pickstruc3.horcolors=tmp(isort);
    %merge horpicks
    tmp=[pickstruc1.horpicks pickstruc2.horpicks];
    pickstruc3.horpicks=tmp(isort);
    %merge horattrs
    tmp=[pickstruc1.horattrs pickstruc2.horattrs];
    pickstruc3.horattrs=tmp(isort);
    %merge horattrnames
    tmp=[pickstruc1.horattrnames pickstruc2.horattrnames];
    pickstruc3.horattrnames=tmp(isort);
    %merge horpicktypes
    tmp=[pickstruc1.horpicktypes pickstruc2.horpicktypes];
    pickstruc3.horpicktypes=tmp(isort);
    %merge horguides
    tmp=[pickstruc1.horguides pickstruc2.horguides];
    pickstruc3.horguides=tmp(isort);
    %merge horfwys
    tmp=[pickstruc1.horfwys pickstruc2.horfwys];
    pickstruc3.horfwys=tmp(isort);
end

function [pickstruc,isort]=sortpicks(pickstruc)
%sort the horizons into order of increaing average picktime
nhors1=length(pickstruc.horpicks);
tmean=1000*ones(1,nhors1);%so a horizon with no picks will have a default mean time of 1000 which will put it at the end
for khor=1:nhors1
   ilive=~isnan(pickstruc.horpicks{khor}(:,1));
   if(~isempty(ilive))
        tmean(khor)=mean(pickstruc.horpicks{khor}(ilive,1));
   end
end
[tmp,isort]=sort(tmean);
%hornames
tmp=pickstruc.hornames;
pickstruc.hornames=tmp(isort);
%horcolors
tmp=pickstruc.horcolors;
pickstruc.horcolors=tmp(isort);
%horpicks
tmp=pickstruc.horpicks;
pickstruc.horpicks=tmp(isort);
%horattrs
tmp=pickstruc.horattrs;
pickstruc.horattrs=tmp(isort);
%horattrnames
tmp=pickstruc.horattrnames;
pickstruc.horattrnames=tmp(isort);
%horpicktypes
tmp=pickstruc.horpicktypes;
pickstruc.horpicktypes=tmp(isort);
%horguides
tmp=pickstruc.horguides;
pickstruc.horguides=tmp(isort);
%horfwys
tmp=pickstruc.horfwys;
pickstruc.horfwys=tmp(isort);
end

function ihor=choosehor(hornames)
if(isempty(hornames))
    msgbox({'You have no horizons!!','A horizon cannot be chosen from an empty list.'});
    ihor=[];
    return;
end
ihor=listdlg('promptstring','Choose the horizon to repick',...
    'liststring',hornames,'selectionmode','single');
end

function lw=getcurrentlinewidth
hlw=findobj(gcf,'tag','linewidths');
lws=get(hlw,'userdata');
ilw=get(hlw,'value');
lw=lws(ilw)*2;
end

function xseis=getseismic_x
    hnewhor=findobj(gcf,'tag','newhor');
    udat=get(hnewhor,'userdata');
    xseis=udat{3};
end

function tseis=getseismic_t
    hnewhor=findobj(gcf,'tag','newhor');
    udat=get(hnewhor,'userdata');
    tseis=udat{2};
end

function seis=getseismic
    hnewhor=findobj(gcf,'tag','newhor');
    udat=get(hnewhor,'userdata');
    seis=udat{1};
end

function hhors=pickplot(hparent,hornames,horcolors,horpicks,linewidths)
% PICKPLOT: Plot picks on top of a seismic image plot (used by PICKTOOL)
% 
% Given a figure window with a seismic gather displayed as an image plot,
% plot a set of horizon picks on top of the seismic. 
%
% hparent ... handle of figure window or axes containing the seismic image display
% hornames ... cell array of horizon names of existing picks
% horcolors ... cell array of RGB color vectors used to display the horizon picks
% NOTE: hornames and norcolors must be cell arrays of exactly the same size
% horpicks ... cell array of picks for each horizon. Each entry in horpicks
%       must be a npick-by-3 matrix where npicks is the number of picks
%       currently existing on the horizon. Column 1 is the pick times,
%       column 2 is the pick x coordinates, column 3 is the pick y
%       coordinates. The y coordinates may be all zero (or omitted) if the
%       data is 2D. The pick times may be nans or actual times.
%       Picks that fall outside the spatial cooordinate range of the
%       seismic image will be discarded.
% linewidths ... vector of linewidths for each horizon. This is optional.
%       If not provided, lines will have the standard width (0.5).
%
%
% hhors ... handles of the plotted pick lines (not a cell array)
% 
% 
axes(hparent)
%now plot
nhors=length(hornames);
hhors=zeros(1,nhors);
for k=1:nhors
    picks=horpicks{k};
    hhors(k)=line(picks(:,2),picks(:,1),'color',horcolors{k},'linewidth',linewidths(k),...
        'tag',hornames{k});
end

if(nhors>0)
    legend(hhors,hornames)
end
end



