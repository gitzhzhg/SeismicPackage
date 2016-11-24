function PI_axis_options(arg1,arg2,arg3)
global IMCONTROLS PICKS ZOOM_LOCKS NUMBER_OF_COLORS
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

%global GRAY_PCT

mainax=findobj(gcf,'type','axes','tag','MAINAXES');
posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
axes1=mainax;
h=get(gcf,'userdata');
hmsg=h{2};
hi=h{5};
% hscale=h{6};
hmaster=h{10};
hvertscrol=h{16};
hhorscrol=h{17};
nm=get(gcbo,'label');
if(isempty(nm))
    % This should only occure when a plotimage window is opened and there
    % is a predefined case that the controls are shut off

    if(strcmpi(IMCONTROLS,'on'))
        nm='Show Image Controls';
    elseif(strcmpi(IMCONTROLS,'off'))
        nm='Hide Image Controls';
    else
        return
    end
end
httl=get(axes1,'title');    ttl=get(httl,'string');     udat=get(httl,'userdata');
hxax=get(axes1,'xlabel');   xax=get(hxax,'string');
hyax=get(axes1,'ylabel');   yax=get(hyax,'string');
masterfig=gcf;
switch nm
    case 'Rename Axes'
%         oxdat=get(gco,'xdata');    
%         oydat=get(gco,'ydata');
        qst={'Rename Title:' 'Rename X-Axis' 'Rename Y-Axis'};
        a={ttl xax yax};
        flags=[1 1 1];
        titlestr='Type New Names For The Axes';
        ttstr={'Limit Use Of Reserved Characters' '' ''};
        ansfini=askthingsle(masterfig,qst,a,flags,titlestr,ttstr);
        if(isempty(ansfini))
            stringinfo='Action cancelled';
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
            return
        end
        ttl=ansfini{1};
        xax=ansfini{2};
        yax=ansfini{3};
        % not allowing ttl to have Reserved Characters due to plotimage
        % using the title as a reference
        gtfigs=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
        nm=1;
%         xxnm=1;
        adon='';
        for ii=1:length(gtfigs)
            haxs=get(gtfigs(ii),'currentaxes');
            ht=get(haxs,'title');
            dat=get(ht,'userdata');
            if(~isempty(dat))
                xfile=dat{1};
                xnm=dat{2};
                if(strcmp(xfile,ttl));
                    if(gtfigs(ii)==masterfig)
                    else
                        nm=nm+1;
                        if(xnm>=nm)
                            nm=xnm+1;
                        end
                        adon=['(' num2str(nm) ')'];
                    end
                end 
            end
        end
        set(httl,'string',[ttl adon],'fontweight','bold','userdata',{ttl nm [udat{3}]},'interpreter','none');
        set(hxax,'string',xax);
        set(hyax,'string',yax);
        stringinfo='Axes propeties have been renamed';
        col=[1 1 1];
    case 'Resample Axes'
        oxdat=get(gco,'xdata');    oydat=get(gco,'ydata');
        qst={'Starting X' 'Sample Rate in X' 'Starting Y' 'Sample Rate in Y'};
        a={num2str(oxdat(1)) num2str(oxdat(2)-oxdat(1)) num2str(oydat(1)) num2str(oydat(2)-oydat(1))};
        flags=[1 1 1 1];
        titlestr='Choose New Values for Axes';
        ttstr={'Choose Only Numeric Values' 'Choose Only Numeric Values',...
                'Choose Only Numeric Values' 'Choose Only Numeric Values'};
        ansfini=askthingsle(masterfig,qst,a,flags,titlestr,ttstr);
        if(isempty(ansfini))
            stringinfo='Action cancelled';
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
            return
        end
        stringinfo=['Changing Axis Values Will Reset All Data Associate with ',...
                'and Interactions Of The Figure In Question.  Do you want to Continue?'];
        uqst=questdlg(stringinfo,'Data Reset Question','Yes','No','Yes');
        set(hmsg,'string','Warning','backgroundcolor',[1 1 0]);
        switch uqst
            case 'Yes'
            case 'No'
                stringinfo='Action cancelled';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                return
        end
        try
%             xlen=length(oxdat);
            newx=(0:1:length(oxdat)-1);
            newx=newx*str2double(ansfini{2});
            newx=newx+str2double(ansfini{1}); 
%             ylen=length(oydat);
            newy=(0:1:length(oydat)-1);
            newy=newy*str2double(ansfini{4});
            newy=newy+str2double(ansfini{3});
        catch
            errordlg('One Or More Answers Not In Proper Format.  Changes Not Made');
            return
        end
        % setting new y and x values to image
        set(hi,'xdata',newx,'ydata',newy);
        set(mainax,'xlim',[newx(1) newx(end)],'ylim',[newy(1) newy(end)]);
        lns=get(get(posax,'title'),'userdata');
        set(lns(5),'xdata',newx,'ydata',newy);
        set(posax,'xlim',[newx(1) newx(end)],'ylim',[newy(1) newy(end)]);
        PI_KillLimitLines(gcf);
        % deleting picks

        for ii=1:size(PICKS,1)
            if(PICKS{ii,1}==gcf)
                PICKS{ii,2}=[];
                delete(PICKS{ii,3});
                delete(findobj(gcf,'tag','PICKS'));
                PICKS{ii,3}=[];
                break
            end
        end
        ns=size(ZOOM_LOCKS,1);
        for ii=1:ns
            checkzoom=ZOOM_LOCKS(ii,:);
            if(checkzoom(1)==gcf||checkzoom(2)==gcf)
                ZOOM_LOCKS(ii,:)=[];
                ii=1;
                ns=ns-1;
            end            
        end
        set(hvertscrol,'min',newy(1),'max',newy(end),'visible','off');
        set(hhorscrol,'min',newx(1),'max',newx(end),'visible','off');
        PI_positionaxes_lineposition;
        set(hmaster,'value',1,'backgroundcolor',[.8314 .8157 .7843]);
        stringinfo='Axes has been resampled';
        col=[1 1 1];
    case 'Hide Image Controls'
        hcolormap=findobj(gcf,'tag','COLORMAP');
        mainaxpos=get(hcolormap,'userdata');
        set(posax,'visible','off','hittest','off');
        set(get(posax,'children'),'visible','off','hittest','off');
        hbak=findobj(gcf,'tag','BACKING');
        set(get(hbak,'userdata'),'visible','off');
        %set(mainax,'position',[.096 .117 .881 .783]);
        set(mainax,'position',[mainaxpos(5) mainaxpos(2) mainaxpos(6) mainaxpos(4)]);
        %set(hhorscrol,'position',[.096 .115 .860 .021]);
        stringinfo='Image Controls have been shut off';
        col=[1 1 1];
        set(gcbo,'label','Show Image Controls');
    case 'Show Image Controls'
        %set(hhorscrol,'position',[.291 .115 .664 .021]);
        hcolormap=findobj(gcf,'tag','COLORMAP');
        mainaxpos=get(hcolormap,'userdata');
        %set(mainax,'position',[.291 .117 .686 .783]);
        set(mainax,'position',mainaxpos(1:4))
        set(posax,'visible','on','hittest','on');
        set(get(posax,'children'),'visible','on','hittest','on');
        hbak=findobj(gcf,'tag','BACKING');
        set(get(hbak,'userdata'),'visible','on');
        stringinfo='Image Controls have been turned on';
        col=[1 1 1];
        set(gcbo,'label','Hide Image Controls');
    case 'Data Stats'
        dat=get(hmaster,'userdata');
        ms1=['data maximum: ' num2str(full(dat(1)))];
        ms2=['data mean: ' num2str(full(dat(2)))];
        ms3=['data stddev: ' num2str(full(dat(3)))];
        ms4=['number of colors: ' int2str(NUMBER_OF_COLORS)];
%         ms5=['Percentage of gray transition ' int2str(GRAY_PCT)];
        ms5=[];
        ms6=[];
        msg={ms1 ms2 ms3 ms4 ms5 ms6};
        msgbox(msg,'File Properties','help','modal');
        stringinfo='Image stats have been displayed';
        col=[1 1 1];
    case 'Send to Clipboard'
        if(isunix)
            print -dtiff
            adon='tiff file';
        else
            fh=gcf;
            fh.Renderer='opengl';
            hgexport(fh,'-clipboard');
            %print -dbitmap
            adon='clipboard';
        end
        stringinfo=['Figure has been sent to ' adon]; 
        col=[1 1 1];
    otherwise
end
set(hmsg,'string',stringinfo,'backgroundcolor',col);