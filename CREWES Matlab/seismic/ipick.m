function h=ipick(action,act2)
% IPICK ... interactive picking of a seismic display
%
% ipick
%
% IPICK is built into plotimage but will also work with seismic data
% plotted with imagesc. To use with imagesc, just type "ipick" when the
% imagesc plot is the current figure.
% IPICK simply gives the user a graphical interface to define a picking
% trajectory and then calls PICKER to make the picks. The trajectory is
% entered with simple mouse clicks. Clicking on an already defined point
% deletes it. The trajectory may be defined by points in any order. Once
% defined they are sorted to be monotonic in the x coordinate so that
% PICKER will have a single-valued trajectory.
% 
%
% Usage: MB1: press mouse button 1 to define a point on the picking
%           trajectory
%        MB3: mouse button 3 signals the trajectory is defined and 
%        MB1 on an already defined point deletes it.
% Clicking MB3 signals that the trajectory is defined and causes a dialog
% to appear allowing selection of the other parameters for PICKER (see that
% functions help). Once MB3 has been clicked, the trajectory can no longer
% be altered. Instead, clicking MB1 is taken as a signal that a new
% trajectory is being defined. Given a defined trajectory, MB3 can be
% single-clicked repeatedly, each causing the dialog to appear and then the
% event to be repicked. This allows visual comparison of different picking
% options.
% 
% When completed (as signaled by MB3) the user data of the current axis
% contains a structure called linespec with the fields:
% linespec.handle ... the handle of the created line
% linespec.xdata ... the x data of the created line
% linespec.ydata ... the y data of the created line
% PLOTIMAGE automatically retrieves this information and places it in the
% AMP_PICKS global. However, if you are using IPICK on an imagesc display,
% you will want to retireve it after each event has been picked because the
% next event will overwrite it.
%
% by G.F. Margrave, CREWES, 2010
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
if(nargin<1)
    action='init';act2='';
elseif(nargin<2)
    act2='';
end
global PARAMETERS 

%PARAMETERS is a global cell array that will have at most one entry per
%window in which iPick puts information about the current event. Each entry
%in the cell array is a structure called parmstruc. 

%olddownfcn oldupfcn oldmotionfcn figtag hfig haxe kol linespec...
%    pickspec trajfini nevents

switch action
    case 'init'
        if(~iscell('PARAMETERS'))
            PARAMETERS{gethandlenum(gcf)}={};
        end
        
        if(isempty(PARAMETERS{gethandlenum(gcf)}))
            parmstruc.hfig=gcf;
            parmstruc.olddownfcn='';
            parmstruc.oldupfcn='';
            parmstruc.oldmotionfcn='';
            parmstruc.figtag=get(gcf,'tag');
            parmstruc.haxe=gca;
            parmstruc.kol='';
            parmstruc.linespec='';
            parmstruc.pickspec='';
            parmstruc.trajfini=0;
            parmstruc.nevents=0;
            PARAMETERS{gethandlenum(gcf)}=parmstruc;
        else
            parmstruc=PARAMETERS{gethandlenum(gcf)};
        end
            
        if(strcmp(parmstruc.figtag,'PLOTIMAGEFIGURE'))
            parmstruc.kol='r';
        else
            parmstruc.kol='k';
        end
        parmstruc.olddownfcn=get(gcf,'windowbuttondownfcn');
        parmstruc.oldupfcn=get(gcf,'windowbuttonupfcn');
        parmstruc.oldmotionfcn=get(gcf,'windowbuttonmotionfcn');
        parmstruc.hfig=gcf;
        parmstruc.haxe=gca;
        set(gcf,'windowbuttondownfcn','ipick(''newpt'')');
        set(gcf,'windowbuttonupfcn','');
        set(gcf,'windowbuttonmotionfcn','');
        set(gca,'userdata',[]);
        parmstruc.linespec=[];
        parmstruc.pickspec=[];
        parmstruc.trajfini=0;
        parmstruc.nevents=0;
        parmstruc.delt=.020;
        PARAMETERS{gethandlenum(gcf)}=parmstruc;
    case 'newpt'
        parmstruc=PARAMETERS{gethandlenum(gcf)};
        pt=get(gca,'currentpoint');
        if(strcmp(act2,'otherevent'))
            button=act2;
        else
            button=get(gcf,'selectiontype');
        end
        if(strcmp(button,'normal'))
            if(parmstruc.trajfini)
                %ok, if here then an event has been finished and picked
                %with MB3 and then MB1 was clicked again. This is take as
                %the signal of a new event.
                parmstruc.linespec=[];%start with a fresh linespec
                parmstruc.trajfini=0;
            end
            if(isempty(parmstruc.linespec))
                h=line(pt(1,1),pt(1,2),'marker','*','color',parmstruc.kol,'linestyle','none');
                parmstruc.linespec.handle=h;
                parmstruc.linespec.xdata=pt(1,1);
                parmstruc.linespec.ydata=pt(1,2);
                %set(gca,'userdata',linespec);
            else
                %test to see if this is a delete click
                yscale=diff(get(gca,'ylim'));
                xscale=diff(get(gca,'xlim'));
                ind=near(parmstruc.linespec.xdata,pt(1,1));
                tol=.01;
                delx=abs(parmstruc.linespec.xdata(ind(1))-pt(1,1));
                dely=abs(parmstruc.linespec.ydata(ind(1))-pt(1,2));
                if(delx/xscale<tol && dely/yscale<tol)
                    %ok its a deletion
                    parmstruc.linespec.xdata(ind(1))=[];
                    parmstruc.linespec.ydata(ind(1))=[];
                    set(parmstruc.linespec.handle,'xdata',parmstruc.linespec.xdata,'ydata',parmstruc.linespec.ydata);
                else
                    xdata=[parmstruc.linespec.xdata pt(1,1)];
                    ydata=[parmstruc.linespec.ydata pt(1,2)];
                    set(parmstruc.linespec.handle,'xdata',xdata,'ydata',ydata);
                    parmstruc.linespec.xdata=xdata;
                    parmstruc.linespec.ydata=ydata;
                end
            end
            PARAMETERS{gethandlenum(gcf)}=parmstruc;
        elseif(strcmp(button,'alt')||strcmp(button,'otherevent'))
            parmstruc=PARAMETERS{gethandlenum(gcf)};
            if(~isempty(parmstruc.pickspec)&&parmstruc.trajfini)
                %if here, then the trajectory has been finished, the event
                %picked at least once, and we are repicking
                if(ishandle(parmstruc.pickspec.handle))
                    delete(parmstruc.pickspec.handle)
                end
            end
%             if(isempty(pickspec))
%                 return;
%             end
            if(~parmstruc.trajfini)
                %if here, then MB3 has been clicked for the first time
                %after MB1 was clicked to define the trajectory. Thus the
                %trajectory is complete.
                parmstruc.nevents=parmstruc.nevents+1;
                parmstruc.pickspec.eventname=['Event' int2str(parmstruc.nevents)];
            end
            flags=[1 1 2];
            if(strcmp(button,'otherevent'))
                %if here, then we were called by PI_copyamppicks
                if(strcmp(parmstruc.copymethod,'Pick at exact times of other event'))
                    parmstruc.delt=0;
                elseif(strcmp(parmstruc.copymethod,'Pick new event according to specs of other event'))
                    if(strcmp(parmstruc.pickspec.picktype,'pick max(abs) amp'))
                        flags(3)=1;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest peak of Hilbert env'))
                        flags(3)=2;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest peak'))
                        flags(3)=3;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest trough'))
                        flags(3)=4;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest +to- zero crossing'))
                        flags(3)=5;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest -to+ zero crossing'))
                        flags(3)=6;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick nearest zero crossing'))
                        flags(3)=7;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick first breaks (STA/LTA)'))
                        flags(3)=8;
                    elseif(strcmp(parmstruc.pickspec.picktype,'pick first breaks (experimental)'))
                        flags(3)=9;
                    else
                        error('logic failure in ipick, you''re outta luck')
                    end
                end
            end
            PARAMETERS{gethandlenum(gcf)}=parmstruc;
            %build the picker dialog
            q=char('Event name','Half-width of picking fairway (sec)',...
                'Pick what?');
            a=char(parmstruc.pickspec.eventname,num2str(parmstruc.delt),...
              ['pick max(abs) amp|pick nearest peak of Hilbert env|pick nearest peak'...
               '|pick nearest trough|pick nearest +to- zero crossing'...
               '|pick nearest -to+ zero crossing|pick nearest zero crossing'...
               '|pick first breaks (STA/LTA)|pick first breaks (experimental)']);
            %flags=[1 1 2];
            transferfcn='ipick(''picker'')';
            askthingsinit(transferfcn,q,a,flags,'Picking Dialog');
            set(gcf,'windowstyle','modal');
        elseif(strcmp(button,'extend'))
            if(strcmp(get(gco,'type'),'line'))
                delete(gco);
            end
        end
    case 'picker'
        %terminate askthings dialog
        parmstruc=PARAMETERS{gethandlenum(gcf)};
        parmstruc.trajfini=1;
        answer=askthingsfini;
        if(answer==-1)
%             msgbox('Cancel accepted');
            return;
        end
        name=answer(1,:);
        delt=str2double(answer(2,:));
        pickmode=deblank(answer(3,:));
        if(strcmp(pickmode,'pick max(abs) amp'))
            flag=1;
        elseif(strcmp(pickmode,'pick nearest peak of Hilbert env'))
            flag=2;
        elseif(strcmp(pickmode,'pick nearest peak'))
            flag=3;
        elseif(strcmp(pickmode,'pick nearest trough'))
            flag=4;
        elseif(strcmp(pickmode,'pick nearest +to- zero crossing'))
            flag=5;
        elseif(strcmp(pickmode,'pick nearest -to+ zero crossing'))
            flag=6;
        elseif(strcmp(pickmode,'pick nearest zero crossing'))
            flag=7;
        elseif(strcmp(pickmode,'pick first breaks (STA/LTA)'))
            flag=8;
        elseif(strcmp(pickmode,'pick first breaks (experimental)'))
            flag=9;
        else
            error('logic failure in ipick, you''re outta luck')
        end
        %retrieve the seismic matrix from the current figure
        if(strcmp(parmstruc.figtag,'PLOTIMAGEFIGURE'))
            [seis,t,x]=plotimage_getseismic;
        else
            %get the image handle
            if(strcmp(get(gco,'type'),'image'))
                hi=gco;
            else
                msgbox('unable to get image handle');
                return;
            end
            seis=get(hi,'cdata');
            x=get(hi,'xdata');
            t=get(hi,'ydata');
        end
        %get the trajectory
        %linespec=get(haxe,'userdata');
        xe=parmstruc.linespec.xdata;
        te=parmstruc.linespec.ydata;
        if(length(xe)<2)
            msgbox('oops! You need to pick at least two points')
            return;
        end
        [xe,ind]=sort(xe);
        te=te(ind);
        [ap,ae,tp,xp]=picker(seis,t,x,te,xe,delt,flag);
        figure(parmstruc.hfig)
        %delete(linespec.handle);
        hcntx=uicontextmenu;
        uimenu(hcntx,'label',deblank(name));
        parmstruc.pickspec.figurehandle=parmstruc.hfig;
        parmstruc.pickspec.handle=line(xp,tp,'marker','.','linestyle','none',...
            'color',parmstruc.kol,'UIContextMenu',hcntx);
        parmstruc.pickspec.eventname=deblank(name);
        parmstruc.pickspec.picktype=pickmode;
        parmstruc.pickspec.trajhandle=parmstruc.linespec.handle;
        parmstruc.pickspec.amppick=ap;
        parmstruc.pickspec.ampevent=ae;
        parmstruc.pickspec.tpick=tp;
        parmstruc.pickspec.xpick=xp;
        parmstruc.pickspec.delt=delt;
        set(parmstruc.haxe,'userdata',parmstruc.pickspec);
        
        if(strcmp(parmstruc.figtag,'PLOTIMAGEFIGURE'))
            plotimage('fromipick');
        else
            set(gcf,'windowbuttondownfcn',parmstruc.olddownfcn);
            set(gcf,'windowbuttonupfcn',parmstruc.oldupfcn);
            set(gcf,'windowbuttonmotionfcn',parmstruc.oldmotionfcn);
        end
        PARAMETERS{gethandlenum(gcf)}=parmstruc;
end
            