function h=picktool_newevent(action,picktooldata)
% picktool_newevent ... interactive picking of a seismic display
%
% picktool_newevent
%
% picktool_newevent is part of picktool. It allows the user to define a
% picking trajectory and then calls PICKER to make the picks. The
% trajectory is entered with simple mouse clicks. Clicking on an already
% defined point deletes it. The trajectory may be defined by points in any
% order. Once defined they are sorted to be monotonic in the x coordinate
% so that PICKER will have a single-valued trajectory.
%
% action ... string denoting the action. Usually 'init' if a human is
%           calling it.
% transfer ... string with a valid matlab command to execute upon completion
% hparentfig ... handle of the parent figure (where the seismic is displayed)
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
% AMP_PICKS global. However, if you are using picktool_newevent on an imagesc display,
% you will want to retireve it after each event has been picked because the
% next event will overwrite it.
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

global PARAMETERS PICKTOOLDATA

%PARAMETERS is a global cell array that will have at most one entry per
%window in which picktool_newevent puts information about the current event. Each entry
%in the cell array is a structure called parmstruc. 

%olddownfcn oldupfcn oldmotionfcn figtag hfig haxe kol linespec...
%    pickspec trajfini nevents

switch action
    case 'init'
        hmsg=gcf;%the handle of the message box
        PICKTOOLDATA=picktooldata;
        hparentaxe=PICKTOOLDATA.hparentaxe;
        hparentfig=PICKTOOLDATA.hparentfig;
        turnoffstandardtoolbar(hparentfig);
        if(~iscell('PARAMETERS'))
            PARAMETERS{gethandlenum(hparentfig)}={};
        end
        
        if(isempty(PARAMETERS{gethandlenum(hparentfig)}))
            parmstruc.hfig=hparentfig;
            parmstruc.olddownfcn='';
            parmstruc.oldupfcn='';
            parmstruc.oldmotionfcn='';
            parmstruc.figtag=get(hparentfig,'tag');
            parmstruc.haxe=hparentaxe;
            parmstruc.kol='';
            parmstruc.linespec='';
            parmstruc.pickspec='';
            parmstruc.trajfini=0;
            parmstruc.nevents=0;
            PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
        else
            parmstruc=PARAMETERS{gethandlenum(hparentfig)};
        end
        
        parmstruc.kol=PICKTOOLDATA.color;%horizon color
        if(~isempty(PICKTOOLDATA.guidepoints))
            parmstruc.linespec.xdata=PICKTOOLDATA.guidepoints(:,2);%existing guidepoints
            parmstruc.linespec.ydata=PICKTOOLDATA.guidepoints(:,1);
        end
        parmstruc.hornumber=PICKTOOLDATA.hornumber;%horizon number
        parmstruc.olddownfcn=get(hparentfig,'windowbuttondownfcn');
        parmstruc.oldupfcn=get(hparentfig,'windowbuttonupfcn');
        parmstruc.oldmotionfcn=get(hparentfig,'windowbuttonmotionfcn');
        parmstruc.hfig=hparentfig;
        parmstruc.haxe=hparentaxe;
        set(hparentfig,'windowbuttondownfcn','picktool_newevent(''newpt'')');
        set(hparentfig,'windowbuttonupfcn','');
        set(hparentfig,'windowbuttonmotionfcn','');
        set(hparentaxe,'userdata',[]);
        parmstruc.pickspec=[];
        parmstruc.trajfini=0;
        parmstruc.nevents=0;
        parmstruc.delt=.020;
        PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
        %draw the existing guide points if any
        if(~isempty(parmstruc.linespec))
            axes(hparentaxe);
            h=line(parmstruc.linespec.xdata,parmstruc.linespec.ydata,...
                'marker','*','color',parmstruc.kol,'linestyle','none');
            parmstruc.linespec.handle=h;
            PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
        end
        figure(hmsg);%bring the message box to the front
    case {'newpt','newptalt'}
        hparentaxe=PICKTOOLDATA.hparentaxe;
        hparentfig=PICKTOOLDATA.hparentfig;
        parmstruc=PARAMETERS{gethandlenum(hparentfig)};
        pt=get(hparentaxe,'currentpoint');
        %         if(strcmp(act2,'otherevent'))
        %             button=act2;
        %         else
        %             button=get(hparentfig,'selectiontype');
        %         end
        if(strcmp(action,'newpt'))
            button=get(hparentfig,'selectiontype');
        else
            button='alt';
        end
        if(strcmp(button,'normal'))
            if(parmstruc.trajfini)
                %ok, if here then an event has been finished and picked
                %with MB3 and then MB1 was clicked again. This is take as
                %the signal of a new event.
                parmstruc.linespec=[];%start with a fresh linespec
                parmstruc.trajfini=0;
            end
            if(isempty(parmstruc.linespec)||isempty(parmstruc.linespec.xdata))
                h=line(pt(1,1),pt(1,2),'marker','*','color',parmstruc.kol,'linestyle','none');
                parmstruc.linespec.handle=h;
                parmstruc.linespec.xdata=pt(1,1);
                parmstruc.linespec.ydata=pt(1,2);
                %set(hparentaxe,'userdata',linespec);
            else
                %test to see if this is a delete click
                yscale=diff(get(hparentaxe,'ylim'));
                xscale=diff(get(hparentaxe,'xlim'));
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
                    xdata=[parmstruc.linespec.xdata;pt(1,1)];
                    ydata=[parmstruc.linespec.ydata;pt(1,2)];
                    set(parmstruc.linespec.handle,'xdata',xdata,'ydata',ydata);
                    parmstruc.linespec.xdata=xdata;
                    parmstruc.linespec.ydata=ydata;
                end
            end
            PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
        elseif(strcmp(button,'alt')||strcmp(button,'otherevent'))%don't know what 'otherevent' connotes
            parmstruc=PARAMETERS{gethandlenum(hparentfig)};
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
                parmstruc.pickspec.eventname=PICKTOOLDATA.name;
            end
            
            PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
            %build the picker dialog
            flags=[1 1 4];
            q=char('Event name','Half-width of picking fairway (sec)',...
                'Pick what?');%questions to ask
            methods=picker('methods');
            method=PICKTOOLDATA.picktype;
            if(~isempty(method))
                for k=1:length(methods)
                    if(strcmp(methods{k},method))
                        flags(3)=k;
                    end
                end
            end
            a=char(parmstruc.pickspec.eventname,num2str(parmstruc.delt),cell2char(methods));
            %flags=[1 1 2];
            transferfcn='picktool_newevent(''picker'')';
            
            askthingsinit(transferfcn,q,a,flags,'Picking Dialog');
            %set(hparentfig,'windowstyle','modal');
        elseif(strcmp(button,'extend'))
            if(strcmp(get(gco,'type'),'line'))
                delete(gco);
            end
        end
    case 'picker'
        hparentaxe=PICKTOOLDATA.hparentaxe;
        hparentfig=PICKTOOLDATA.hparentfig;
        %terminate askthings dialog
        parmstruc=PARAMETERS{gethandlenum(hparentfig)};
        parmstruc.trajfini=1;
        answer=askthingsfini;
        if(answer==-1)
%             msgbox('Cancel accepted');
            return;
        end
        name=answer(1,:);
        delt=str2double(answer(2,:));
        pickmode=deblank(answer(3,:));
        [methods,pickflags]=picker('methods');
        for k=1:length(methods)
            if(strcmp(pickmode,methods{k}))
                flag=pickflags(k);
            end
        end
        %retrieve the seismic matrix from the current figure
        seis=PICKTOOLDATA.seismic;
        x=PICKTOOLDATA.seismicx;
        t=PICKTOOLDATA.seismict;
        %get the trajectory
        %linespec=get(haxe,'userdata');
        xe=parmstruc.linespec.xdata;%guide data x
        te=parmstruc.linespec.ydata;%guide data t
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
        parmstruc.pickspec.hornumber=PICKTOOLDATA.hornumber;
        set(parmstruc.haxe,'userdata',parmstruc.pickspec);
        
        set(hparentfig,'windowbuttondownfcn',parmstruc.olddownfcn);
        set(hparentfig,'windowbuttonupfcn',parmstruc.oldupfcn);
        set(hparentfig,'windowbuttonmotionfcn',parmstruc.oldmotionfcn);
        PARAMETERS{gethandlenum(hparentfig)}=parmstruc;
        eval(PICKTOOLDATA.transfer)
end
            