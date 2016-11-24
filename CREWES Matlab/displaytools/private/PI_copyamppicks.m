function PI_copyamppicks(action)
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


global AMP_PICKS PARAMETERS

if(nargin<1)
    action='init';
end

if(strcmp(action,'init'))
    h=get(gcf,'userdata');
    hzoompick=h{9};
    val=get(hzoompick,'value');
    if(val==1 || val==2 || val==3)
        set(hzoompick,'value',4);
        % call PI_zoompick to initialize iPick. This will cause iPick to build
        % a parmstruc in PARAMETERS
        PI_zoompick;
    end
    %find other plotimage windows with picks
    nevents=length(AMP_PICKS);
    hthisfig=gcf;%the figure window that the new picks will be in
    hotherfigs=zeros(1,20);%handles of the other plotimage windows
    eventfigs=zeros(1,100);%figure number for each event
    notherfigs=0;%number of other plotimage windows
    notherevents=0;%number of events in all other windows
    eventnames='';
    for k=1:nevents
        pickstruc=AMP_PICKS{k};
        hfig=pickstruc.figurehandle;
        if(hfig~=hthisfig)
            notherevents=notherevents+1;
            ind=find(hotherfigs,hfig);
            if(isempty(ind))
                notherfigs=notherfigs+1;
                hotherfigs(notherfigs)=hfig;
            end
            if(notherevents>1)
                eventnames=char(eventnames,[pickstruc.eventname ' from figure ' int2str(hfig)]);
            else
                eventnames=char([pickstruc.eventname ' from figure ' int2str(hfig)]);
            end
            eventfigs(notherevents)=hfig;
        end
    end
    if(isempty(eventnames))
        msgbox('No picked events in other Plotimage windows found');
        return
    end
    %clean up
    ind= eventfigs==0;
    eventfigs(ind)=[];
    
    %sort the event names by figure number
    [aaa,ind]=sort(eventfigs);
    eventnames=eventnames(ind,:);
    eventnames_rowvec=char(32*ones(1,numel(eventnames)+notherevents));
    kbegin=1;
    for k=1:notherevents
        name=[deblank(eventnames(k,:)) '|'];
        kend=kbegin+length(name)-1;
        eventnames_rowvec(kbegin:kend)=name;
        kbegin=kend+1;
    end
    %put up an askthings dialog
    q=char('Choose which event to copy','Choose picking option');
    a=char(eventnames_rowvec,...
        ['Pick at exact times of other event|Pick new event according to ' ...
        'specs of other event|Respecify picking using other event''s trajectory']);
    askthingsinit('plotimage(''copyamppicks2'')',q,a,[1 2],'Chose event and picking scheme');
    set(gcf,'windowstyle','modal');
elseif(strcmp(action,'pick'))
  
    a=askthingsfini;
    if(a==-1)
        return;
    end
    neweventname=deblank(a(1,:));
    eventname=neweventname;
    ind=strfind(eventname,' from figure ');
    hfig=str2double(eventname(ind(1)+12:end));%figure number of event
    eventname=eventname(1:ind(1)-1);%name of event
    method=deblank(a(2,:));%picking method

    %ok now get pickstruc
    nevents=length(AMP_PICKS);
    for k=1:nevents
        pickstruc=AMP_PICKS{k};
        hfignow=pickstruc.figurehandle;
        if(hfig==hfignow)
            if(strcmp(eventname,pickstruc.eventname))
                break;
            end
        end
    end
    pickstruc.eventname=neweventname;%rename the event
    pickstruc.figurehandle=gcf;%update the figure number   
    pickstruc.handle=[];%no handle yet
    %get the parmstruc from iPick global: PARAMETERS
    parmstruc=PARAMETERS{gcf};
    parmstruc.trajfini=1; %maybe....
    parmstruc.pickspec=pickstruc;
    %plot the trajectory from the other window in the current window
    xe=get(pickstruc.trajhandle,'xdata');
    te=get(pickstruc.trajhandle,'ydata');
    h=line(xe,te,'marker','*','color',parmstruc.kol,'linestyle','none');
    parmstruc.linespec.handle=h;
    parmstruc.pickspec.trajhandle=h;
    parmstruc.linespec.xdata=xe;
    parmstruc.linespec.ydata=te;
    parmstruc.copymethod=method;
    PARAMETERS{gcf}=parmstruc;
    %invoke ipick
    ipick('newpt','otherevent');
        
end
            
            
                