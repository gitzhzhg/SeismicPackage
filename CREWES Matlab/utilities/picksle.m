function picksout=picksle(arg1,arg2,arg3,arg4,arg5)
%***************
%*** PICKSLE ***
%***************
%
% picksle(transfer,seisstruct);
% picksle(transfer,seisstruct,masteraxes);
% picksle(transfer,seisstruct,masteraxes,preferances);
%
% PICKSLE is a new picking routine that allows users to make picks across
% seismic sections and interpolate between
%
% PICKSLE cleans up after it has calculated the initial picks and erases
% them from the axis.  The picks information is passed using the 'transfer'
% argument which executes upon the final calculation.  This transfer
% should be a routine in the users program which calls picksle and have the
% following command 'picksout=picksle('EXPORT')' to access the picks 
% information that was calculated.
%
% The seisstruct is set up in a field the following way.
% SEIS : (2D Array of seismic data) 
% T : (Depth or Time) - optional
% X : (Distance) - optional
%
% masteraxes - The specific axes where calculations will take place
% default - gca
%
% The preferance argument has several variables that can be changed to
% allow for greater flexibility in automatic picking.  The variables are
% descibed below.  It should be noted that not all properties need be
% present for this program to work.
%
% {'hmsg' 'nodetolerance' 'sampleadd' 'showprogress' 'percenttolerance' 
% 'lockto' 'askdetail'
%
% 'hmsg' :  a handle where messages from pickles can be sent to relay
% information to the user (default = empty)
%
% 'nodetolerance' : Allows the user to control how many pics are made.  The
% highter the number, the less pics.  1 will allow for one pic per trace
% (default = 5)
%
% 'sampleadd' : This controls samples size of the windowed area that the is
% taken of the initial clicked trace. (defaul = 100)
%
% 'showprogress' : This is more for debugging purposes to show the user
% where the cross correlation scanning is happing on the image
% (default = "Show Progress", leave empty to not show progress)
%
% 'percenttolerance' : percentage crosscorelation that picksle will check
% for and accept when searching for nodes, in decmil form
% 
% 'lockto' : can be set to 'open', 'peak', 'trough', 'zerocross+',
% 'zerocross-'
% (default = open)
%
% 'askdetail' : This argument allows the user to shut on or off the figure
% that allows adjustment in click location and window size.
% (default = 'on' )
%
% Christopher Harrison, 2004
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

if(nargin<=0|nargin>=5)
    stringinfo={'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
elseif(nargin==1)
    if(~ischar(arg1))
        stringinfo={'Input data not set up properly.',...
                'Please see picksle help for details in proper use'};
        helpdlg(stringinfo,'Check PICKSLE help file');
        return
    else
        checkstr=findstr('bdownbmotionbupEXPORT',arg1);
        if(isempty(checkstr))
            stringinfo={'Input data not set up properly.',...
                    'Please see picksle help for details in proper use'};
            helpdlg(stringinfo,'Check PICKSLE help file');
            return
        end
        switch arg1
            case 'bdown'
                PLE_buttondown
            case 'bmotion'
                PLE_motion
            case 'bup'
                PLE_up
            case 'EXPORT'
                %--- Getting Data ---
                pref=get(gca,'userdata');
                if(~isstruct(pref))
                    % something is wrong if this is not a structure
                    picksout=[];
                    return
                end
                % doing a last check, just in case something has gone wrong
                fnms=fieldnames(pref);
                dat=[];
                for ii=1:length(fnms)
                    if(strcmp(fnms{ii},'holddat'))
                        dat=getfield(pref,'holddat');
                        break
                    end
                end
                %----------
                picksout=dat;
        end
        return
    end
elseif(nargin==2)
    % Two arguments mean user has passed seis structure and gca
    trans=arg1;
    seisstruct=arg2;
    masteraxes=gca;
    preferances=[];
elseif(nargin==3)
    % seis structure and gca, and location of th pulldownmenu
    trans=arg1;
    seisstruct=arg2;
    masteraxes=arg3;
    preferances=[];
elseif(nargin==4)
    trans=arg1;
    seisstruct=arg2;
    masteraxes=arg3;
    preferances=arg4;
elseif(nargin==5)
    return
end
% Checking first if figure and axes are actually right
if(~ishandle(masteraxes))
    stringinfo={'Axes handle has not been choosen properly.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
elseif(~strcmp(get(masteraxes,'type'),'axes'))
    stringinfo={'Axes handle has not been choosen properly.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
end
masterfig=get(gca,'parent');
% eventually there will be an "Initial Picks"
initialpicks=[];

% checking to make sure seis data is in structure form
if(~isstruct(seisstruct))
    stringinfo={'seisstruct has not been set up properly.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
end
% checking to see that seis struct has the proper components
fnms=fieldnames(seisstruct);
checknms={'SEIS' 'T' 'X'};  % there will evenutally be a "y" for 3D 
SEIS=[];
T=[];
X=[];
for ii=1:size(checknms,2)
    checked=[];
    for jj=1:size(fnms,1)
        if(strcmp(lower(checknms{ii}),lower(fnms{jj})))
            % just in case 
            checked='Check';
            checknms{ii}=getfield(seisstruct,(fnms{jj}));
            break
        end
    end
    if(isempty(checked)&strcmp(lower(checknms{ii}),lower('SEIS')))
        stringinfo={'seisstruct has not been set up properly.',...
                'Please see picksle help for details in proper use'};
        helpdlg(stringinfo,'Check PICKSLE help file');
        return
    end
end
SEIS=checknms{1};
T=checknms{2};
X=checknms{3};
if(isempty(SEIS)||isstr(SEIS))
    stringinfo={'Make sure your SEIS array is not empty.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
end
if(isempty(T)||isstr(T))
    T=[1:1:size(SEIS,1)];
end
if(isempty(X)||isstr(X))
    X=[1:1:size(SEIS,2)];
end
seiss.SEIS=SEIS;
seiss.T=T;
seiss.X=X;
seisstruct=seiss;
% Check seistruct to make sure proper matrix lengths have been creat
if(isempty(T))
    T=[0:1:size(SEIS,1)-1]';
    seisstruct.T=T;
elseif(size(SEIS,1)~=length(T))
    stringinfo={'seisstruct has not been set up properly.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
end
if(isempty(X))
    X=[0:1:size(SEIS,2)-1];
    seisstruct.X=X;
elseif(size(SEIS,2)~=length(X))
    stringinfo={'seisstruct has not been set up properly.',...
            'Please see picksle help for details in proper use'};
    helpdlg(stringinfo,'Check PICKSLE help file');
    return
else
    if(X(1)==X(end))
    end
end
pref.seisstruct=seisstruct;
pref.axes=masteraxes;
pref.transfer=trans;
pref.holddat=[];    % This helps with the transfer of data.
if(~isempty(preferances))
    % checking preferances
    if(~isstruct(preferances))
        stringinfo={'Your preferances are not set up properly.',...
                'Please see picksle help for details in proper use'};
        helpdlg(stringinfo,'Check PICKSLE help file');
        return
    end
    % checking to make sure that user has set up the preferance file right
    % NOTE, when adding new prefereance to program, you must also add them
    % to the "checkfields" prefernce field.
    hmsg='nothing yet';
    nodetolerance='nothing yet';
    sampleadd='nothing yet';
    showprogress='Showing Progress';
    percenttolerance='nothing yet';
    lockto='open';
    askdetail='on';
    checkfield={'hmsg' 'nodetolerance' 'sampleadd' 'showprogress' 'percenttolerance' 'lockto' 'askdetail'};
    fnames=fieldnames(preferances);
    % checking to make sure the properties that have been included in the
    % preferances correspond to know preferences
    checked='no';
    for ii=1:size(fnames)
        for jj=1:size(checkfield)
            if(strcmpi(fnames(ii),checkfield(jj)))
                % This allows the user to input only one propertie 
                checked='yes';
                break
            end
        end
        if(~strcmpi(checked,'yes'))
            stringinfo={'Your preferances are not set up properly.',...
                'Please see picksle help for details in proper use'};
            helpdlg(stringinfo,'Check PICKSLE help file');
            return
        end
    end
    checkfields={'hmsg' 'nodetolerance' 'sampleadd' 'showprogress' 'percenttolerance',...
        'axes' 'transfer' 'holddat' 'lockto' 'askdetail'};
    % doing a last check of the preferences
    if(~ishandle(hmsg))        hmsg=[];    end
    if(~isnumeric(nodetolerance))        nodetolerance=1;    elseif(nodetolerance<=1) nodetolerance=1; end
    if(~isnumeric(sampleadd))        sampleadd=100;    end
    if(~isnumeric(percenttolerance))        percenttolerance=.2;    end 
    checkpreset={{'open','peak','trough','zerocross','zerocross+','zerocross-'} {'on' 'off'}};
    checkinput={lower(lockto) lower(askdetail)};
    checking={[] []};
    checkerror={'Locking for advanced picking has not been set up correctly',...
            'Detail has not been set up properly'};
    for ii=1:size(checkpreset,2)
        cpreset=checkpreset{ii};
        for jj=1:size(cpreset,2)
            if(strcmpi(checkinput{ii},cpreset{jj}))
                checking{ii}='Checked';
                break
            end
        end
        if(isempty(checking{ii}))
            stringinfo=checkerror{ii};
            helpdlg(stringinfo,'Check PICKSLE help file');
            return
        end
    end
    pref.showprogress=showprogress; % empty means no show progress
    pref.sampleadd=sampleadd;
    pref.percenttolerance=percenttolerance;
    pref.nodetolerance=nodetolerance;
    pref.hmsg=hmsg;
    pref.lockto=checkinput{1};
    pref.askdetail=checkinput{2};
    pref.checkfields=checkfields;
else
    % NOTE, when adding new prefereance to program, you must also add them
    % to the data check section
    pref.showprogress=[]; % empty means no show progress
    pref.sampleadd=[100];
    pref.percenttolerance=[.70];
    pref.nodetolerance=[5];
    pref.hmsg=[];
end    
% saveing data in handle
set(gca,'userdata',pref);
buttondownstr=['bdown'];   % info is going to be sent through one argument
% setting gcf buttons
set(masterfig,'windowbuttondownfcn',['picksle(''' buttondownstr ''');']);

%---------------
%- Button Down -
%---------------
%
% The initial button down for picksle
function PLE_buttondown(hhandle)
checkobj=get(gco,'type');
checkclick=get(gcf,'selectiontype');
if(strcmp(checkobj,'figure')|~strcmp(checkclick,'normal'))
    % doing nothing 'cause click should take place in axes, not figure
    % or click is not a left mouse button click
    return
else

end
% checking to make sure that data is still available
checkdata=PLE_checkdata(get(gca,'userdata'));
pref=get(gca,'userdata');
if(isempty(checkdata)||isempty(pref))
    % if preferences are gone, either wrong axes has been clicked on, or
    % data is gonzo
    return
end
%--- Getting Data ---
masteraxes=getfield(pref,'axes');
seisstruct=getfield(pref,'seisstruct');
%----------
masterfig=get(masteraxes,'parent');
if(gca~=masteraxes)
    % user is not clicking in master axes
    return
end
pt=get(gca,'currentpoint');

hln=line([pt(1,1) pt(1,1)],[pt(1,2) pt(1,2)],'color','r','linestyle','-',...
    'tag','PICKSLE-LINE','erasemode','xor');
% setting windowbutton functions
buttonmotionstr=['bmotion'];
set(masterfig,'windowbuttonmotionfcn',['picksle(''' buttonmotionstr ''');']);
buttonupstr=['bup'];
set(masterfig,'windowbuttonupfcn',['picksle(''' buttonupstr ''');']);
%--- Saving Data ---
pref.holddat={pt hln};
set(gca,'userdata',pref);
%-------------------

%----------
%- Motion -
%----------
%
% Motion o' the line
function PLE_motion(hhandle,transfer)
%--- Getting Data ---
pref=get(gca,'userdata');
masteraxes=getfield(pref,'axes');
seisstruct=getfield(pref,'seisstruct');
dat=getfield(pref,'holddat');
%----------
oldpt=dat{1};
hln=dat{2};
newpt=get(gca,'currentpoint');
xdat=get(gca,'xlim');
ydat=get(gca,'ylim');
xdat=sort([xdat(1) xdat(2) newpt(1,1)]);
ydat=sort([ydat(1) ydat(2) newpt(1,2)]);
xln=get(hln,'xdata');
yln=get(hln,'ydata');
set(hln,'xdata',[xln(1) xdat(2)],'ydata',[yln(1) ydat(2)]);
%--- Saving Data ---
pref.holddat={newpt hln};
set(gca,'userdata',pref);
%-------------------


%-------------
%- Button Up -
%-------------
%
% Second last step in the war on finding lines
function PLE_up(hhandle)
% setting windowbutton functions
set(gcf,'windowbuttonmotionfcn','');
set(gcf,'windowbuttonupfcn','');% checking to make sure that data is still available
% checking to make sure that data is still available
checkdata=PLE_checkdata(get(gca,'userdata'));
if(isempty(checkdata))
    return
end
%--- Getting Data ---
pref=get(gca,'userdata');
masteraxes=getfield(pref,'axes');
seisstruct=getfield(pref,'seisstruct');
dat=getfield(pref,'holddat');
hmsg=getfield(pref,'hmsg');
transfer=getfield(pref,'transfer');
sampleadd=getfield(pref,'sampleadd');
lockto=getfield(pref,'lockto');
percenttolerance=getfield(pref,'percenttolerance');
nodetolerance=getfield(pref,'nodetolerance');
showprogress=getfield(pref,'showprogress');
lockto=getfield(pref,'lockto');
askdetail=getfield(pref,'askdetail');
%----------
xlm=get(gca,'xlim');
ylm=get(gca,'ylim');
hln=dat{2};
% getting relavent line data
xlndat=get(hln,'xdata');
xdat=sort(xlndat);
xstr=xlndat(1);
xend=xlndat(2);
ylndat=get(hln,'ydata');
ydat=sort(ylndat);
% yyy=sort([ylndat(1) ylndat(2)]);
ystr=ylndat(1);
yend=ylndat(2);

delete(hln);    % deleting the guide line
seis=getfield(seisstruct,'SEIS');
t=getfield(seisstruct,'T');
x=getfield(seisstruct,'X');

% checking to make sure that T and X similr to the x and y limits of the
% axes.
if((t(end)-t(1))*(ylm(2)-ylm(1))<0)
    % values are opposite, t will be forced to be same as axes
    tcha=[];
    for ii=1:length(t)
        if(ii==length(t))
            tcha=[tcha t(1)];
        else
            tcha=[tcha t(length(t)-ii+1)];
        end
    end
    t=tcha;
end
if((x(end)-x(1))*(xlm(2)-xlm(1))<0)
    % values are opposite, t will be forced to be same as axes
    xcha=[];
    for ii=1:length(x)
        if(ii==length(x))
            xcha=[xcha x(1)];
        else
            xcha=[xcha x(length(x)-ii+1)];
        end
    end
    x=xcha;
end
% finding where user has clicked, this could appear strange on the plot
y1=find(t>=ydat(1));
y2=find(t<=ydat(2));
x1=find(x>=xdat(1));
x2=find(x<=xdat(2));
xnum=[x1(1):x2(end)];
xnew=x(x1(1):1:x2(end));
ystuff=[t(y1(1)) t(y2(end))];   % for debugging
if(isempty(xnew))
    % going to cross correlate across the the entire image, have to ask
    % user for permission
    checkwuser=questdlg('Do you want to check for picks across entire section?',...
        'Check for pics?','Yes','No','Yes');
    switch checkwuser
        case 'Yes'
            xnum=[1:1:length(x)];
        case 'No'
            return
    end
    checkx=1;
end
xnew=x(xnum);
seisnew=seis(:,xnum);
% getting data out of PicksData
% tolerance is going to have two different means
% one will be for depth in seconds
% the second one is for depth in meters, or any other unit for that matter
% nearest value, peak, trogh, and zero crossing, has to be in initiated
% lockto;
smpladd=sampleadd;
xx=find(xstr<=xnew);
if(xstr==xend)
    nmx=find(x>=xstr);
    nmx(1);
    if(isempty(nmx))
        nmx=find(x<=xend);
        nmx=nmx(end);
    else
        nmx(1);
    end
    strtrc=seisnew(:,nmx(1));
    xfirst=xnum(end);
    swt=2;
elseif(isempty(xx))
    % means line started from smaller to short
    strtrc=seisnew(:,1);
    xfirst=xnum(end);
    swt=2;
else
    strtrc=seisnew(:,end);
    xfirst=xnum(1);
    swt=1;
end
% wavelet midpoint, it may not actually be the middle of the wavelet
% inquestion, but it will be point closest to where user clicked
wvltmid=find(ystr>=t);
% finding the wavelet window
if(isempty(wvltmid))
    wvltmid=1;
elseif(wvltmid(end)-smpladd<0)
    wvltmid=wvltmid(end);
else
    wvltmid=wvltmid(end);
end
if(xstr==xend)
    trc=seisnew(:,x1(1));
elseif(swt==2)
    trc=seisnew(:,end);
elseif(swt==1)
    trc=seisnew(:,1);
else
    % something is wrong
    return
end
% setting up proper format, just in case something has gone wrong
if(size(trc,1)~=size(t,1))
    t=t';
end
if(strcmp(lockto,'open'))
    % do nothing, already set up for open
    checkinglock='checked';
else
    lockto=deblank(lower(lockto));
    nhood=10;
    dataout=nodefind(trc,wvltmid,lockto,nhood);
    wvltmid=dataout;
    if(isempty(wvltmid))
        switch lockto
            case 'peak'
                locktoadd='s';
            case 'trough'
                locktoadd='s';
            case 'zerocross'
                locktoadd='ing';
        end
        stringinfo=['No ' lockto locktoadd ' were found within tolerance area.  Please check your data.'];
        helpdlg(stringinfo,'Check PICKSLE help file');
        return
    end
end
if(strcmpi(askdetail,'on'))
    % showing the user detail of the section chosen for scanning
    trc=[trc t];
    % the following line is just a point marked for debug reference 
    % referancepoint=line([xstr xstr],[ystr ystr],'color','b','marker','*');
    % window extractor
    dataout=winextractor(gca,trc,wvltmid,smpladd,lockto);
    if(isempty(dataout))
        if(isempty(hmsg))     
            return    
        end
        stringinfo='Action Canceled';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
        return
    end
    pause(.005);    % need to pause to get rid of ghost image of winextrator figure
    wvltmid=getfield(dataout,'midpoint');
    smpladd=getfield(dataout,'samples');
end

if(isempty(wvltmid))
    wvlttop=1;
    wvltmid=1;
elseif(wvltmid(end)-smpladd<0)
    wvltmid=wvltmid(end);
    wvlttop=[1];
    wvltmid=wvltmid(end);
else
    wvltmid=wvltmid(end);
    wvlttop=wvltmid-smpladd;
end
if(size(seisnew,1)<=wvltmid+smpladd)
    wvltbot=size(seisnew,1);
else
    wvltbot=wvltmid+smpladd;
end

wvltwndw=[wvlttop:1:wvltbot]';
% wvltln=line([xstr(1) xstr(1)],[t(wvlttop) t(wvltbot)],'color','r');
% wvltln2=line([xstr(1) xstr(1)],[t(wvlttop) t(wvltbot)],'color','r');
% lns=[wvltln wvltln2];
lns=[];

% creating the "wavelet", again its not a wavelet, but for the lack of a
% better word 'cause i am using window later.
wvlt=strtrc(wvltwndw);
wvltnms=sort([wvlttop,wvltbot]);
% Tolerance checking
if(abs(t(2)-t(1))<0.2)
    % probably not the best way to decide that depth is in time
    [spec,f]=fftrl(wvlt,t(wvltnms(1):wvltnms(2)));
    domfreq=max(f);
    domperiod=1/domfreq;
else
    
end

% % angle, this based on # of rows and not the depth of distance
% ang=atan((ylimit(end)-ylimit(1))/(xlimit(end)-xlimit(1)));
% angle didn't work
if(xend-xstr==0)
    % single click for crosscorrelation across entire seciotn
    dx=length(xnum);
elseif((xend-xstr)<0)
    % click going from lower to higher
    dx=x1(1)-x2(end);
else
    % click going from higher to lower
    dx=x2(end)-x1(1);
end
if((yend-ystr)<0)
    dy=y1(1)-y2(end);
else
    dy=y2(end)-y1(1);
end
rto=dy/dx;
% tolerance, which is a percent of total length of depth (time or
% otherwise)
pertol=round(.05*size(seisnew,1)/2);

picksdat=[];
ys=[wvltmid];
xdist=xstr-xend;
ydist=ystr-yend;
y2=wvltmid;
dy=t(2)-t(1);
if(xend==xstr)
    allcrosscor={[ ]};
else
    allcrosscor={[100 wvltmid xfirst]};
end
yhold=[wvltmid];
xhold=[xfirst];
perhold=[100];
holdcrosscor=[];
hcc=1;
for ii=2:size(seisnew,2)
    % ii will have to be flipped if user has made line go from smallest to
    % largest
    if(swt==2)
        trc=seisnew(:,size(seisnew,2)-ii+1);
        dx=(size(seisnew,2)-ii+1)-(size(seisnew,2)-ii+2);
        jj=(size(seisnew,2)-ii+1);
        adon=1;
    else
        trc=seisnew(:,ii);
        dx=(ii)-(ii-1);
        jj=ii;
        adon=-1;
    end
    % geting point along initial line
    yup=dx*rto;
    y2=round(y2+yup);
    if(y2<=0)
        y2=1;
    elseif(y2>=length(t))
        y2=size(t,1);
    end
    % ajusting the trace for tolerance and angle 
    wndwtop=y2-round(pertol);
    wndwbot=y2+round(pertol);
    if(wndwtop<=0)
        wndwtop=1;
    end
    if(wndwbot>length(trc))
        wndwbot=length(trc);
    end
    wndw=[wndwtop:1:wndwbot];
    wn=size(wndw);
%     wndwln=line([xnew(jj) xnew(jj)],[t(wndw(1)) t(wndw(end))],'color','g',...
%         'erasemode','xor');
    % creating scansection, it is larger then the wndw because the wavelet
    % will scan through the window, top to bottom, from its center point.
    scansectop=wndwtop-(wvltmid-wvltwndw(1));
    scansecbot=wndwbot+(wvltwndw(end)-wvltmid);
    if(scansectop<=0)
        scansectop=1;
    end
    if(scansecbot>length(trc))
        scansecbot=length(trc);
    end
    scansec=[scansectop:1:scansecbot];
    if(~isempty(showprogress))
        % this will show user progress of picking
        scansecln=line([xnew(jj) xnew(jj)],[t(scansec(1)) t(scansec(end))],'color','g',...
            'erasemode','xor');
        lns=[lns scansecln];
    end
    % scaning 
    scantrc=trc(scansec);
    crosscor=[0 0 0];   % [%crosscorrelation position future]
    if(length(scansec)<=length(wvlt))
        % possibly have to shrink the wvlt size followed by scanning only
        % crosscorrelation only once.
        dsize=length(wvlt)-length(scansec);
        if(round(dsize/2)==dsize/2)
            % even number 
            wvlt2=wvlt((1+dsize/2):1:(length(wvlt)-dsize/2));
        else
            % odd number
            if((-.5+dsize/2)<=0)
                strp=1;
                adp=0;
            else
                strp=(-.5+dsize/2);
                adp=1;
            end
            wvlt2=wvlt(strp:1:(length(wvlt)-round(dsize/2)-adp));
        end
        % setting warning off, because sometimes zeros really screw with
        % junk and I don't feel like worrying about zeros right now
        warning off
        crossc=(sum(wvlt2.*scantrc))/((sum(wvlt2.^2)*sum(scantrc.^2))^0.5);
        warning on
        crosscor=[crossc scansec(round(length(scansec)/2)) xnum(jj)];
    else
        trcwndw=trc(scansec);
        for kk=1:(length(scansec)-length(wvlt))
            warning off
            % seeing if normalization helps
            mat1=wvlt/max(wvlt);
            mat2=scantrc(kk:length(wvlt)+kk-1)/max(scantrc(kk:length(wvlt)+kk-1));
            % setting warning off, because sometimes zeros really screw with
            % junk and I don't feel like worrying about zeros right now
            % crossc=(sum(wvlt.*scantrc(kk:length(wvlt)+kk-1)))/((sum(wvlt.^2)*sum(scantrc(kk:length(wvlt)+kk-1).^2))^0.5);
            crossc=(sum(mat1.*mat2))/((sum(mat1.^2)*sum(mat2.^2))^0.5);
            % crossc=(sum(scantrc(kk:length(wvlt)+kk-1).*wvlt))/((sum(scantrc(kk:length(wvlt+kk-1)).^2)*sum(wvlt.^2))^0.5);
            warning on
            % set(wvltln2,'xdata',[xnew(jj) xnew(jj)],'ydata',[scansec(kk) (scansec(length(wvlt))+kk-1)],...
            %     'erasemode','xor');
            if(crossc>crosscor(1))
                if(crossc>=5)
                else
                    crosscor=[crossc scansec(kk+round(length(wvlt)/2)) xnum(jj)];
                end
            end
        end
    end
    holdcrosscor(hcc)=crossc;
    hcc=hcc+1;
    xdt=[xnew(jj) xnew(jj)];
    ydt=[t((ys)) t((y2))];
    % ln=line(xdt,ydt,'erasemode','xor','color','k');
    ys=y2;
    %     if(hcc==10)
    %         % plotting cross correlation lines
    %         if(sum(holdcrosscor)/5>0.40)
    %             % only using 65% crosscorrelation
    %             allcrosscor{length(allcrosscor)+1}=crosscor;
    %             dat1=allcrosscor{length(allcrosscor)-1};
    %             crosslnl=line([x(dat1(3)) x(crosscor(3))],[t(dat1(2)) t(crosscor(2))],'color','b',...
    %                 'erasemode','xor','linewidth',2);
    %         end
    %         hcc=1;
    %     end
    
    % percenttolerance is a user or default value which determines the
    % percentage crosscorelation that will be accepted as a node
    if(crosscor(1)>percenttolerance)
        % checking the largest 
        allcrosscor{length(allcrosscor)+1}=crosscor;
        dat1=allcrosscor{length(allcrosscor)-1};
        if(isempty(dat1))
        else
            yhold=[yhold crosscor(2)];
            xhold=[xhold crosscor(3)];
            perhold=[perhold crosscor(1)];
            if(~isempty(showprogress))
                crosslnl=line([x(dat1(3)) x(crosscor(3))],[t(dat1(2)) t(crosscor(2))],'color','b',...
                    'erasemode','xor','linewidth',2);
                lns=[lns crosslnl];
            end
        end
    else
    end
end
delete(lns);
% nodetolernace is a user or default
nodetol=nodetolerance;
if(length(yhold)<=1)
    % no matches have been found
    return
else
    % if the first and last x values are not at least 6% for the size away
    % from eachother, lines will be created
    if(~abs(xhold(end)-xhold(1))>=abs(x(end)-x(1))*0.06)
        return
    end
end
ii=ceil(length(yhold)/nodetol);
if(xstr==xend)
    yholdnew=[];
    xholdnew=[];
    yhold2=[];
    xhold2=[];
else
    % I am commenting out the following becuase, well just in case the the
    % user just wants a specfic area, not end of line
    yholdnew=[t(yhold(1))];
    xholdnew=[x(xhold(1))];
    yhold2=[yhold(1)];
    xhold2=[xhold(1)];
    %     yholdnew=[];
    %     xholdnew=[];
    %     yhold2=[];
    %     xhold2=[];
end


    for kk=1:2
        for jj=1:ii
            % this section is breaking down the data set to every nodetol
            if((nodetol*jj)>=length(yhold))
                ynd=round(length(yhold));
            else
                ynd=round(nodetol*jj);
            end
            if(jj==1)
                yst=1;
            else
                yst=round(nodetol*(jj-1));
            end
            yalign=yhold(yst:ynd);
            xalign=xhold(yst:ynd);
            if(jj~=ii)
                % this section is skipping forward to the next section to see if
                % there is enough numbers to make up its own, or if it has to be
                % incorporated into present section
                if((nodetol*jj)>=length(yhold))
                    yst=round(length(yhold));
                else
                    yst=round(nodetol*jj);
                end
                if(length(yhold(yst:end))<5)
                    % less then five, will take into present checking and halt
                    % after
                    yalign=yhold(yst:end);
                    xalign=xhold(yst:end);
                    ii=jj;
                end
            end
            ydatnew=t(yalign);
            xdatnew=x(xalign);
            if(size(xdatnew,1)~=size(ydatnew,1))
                ydatnew=ydatnew';
            end
            % doing a first order polynomial fit, good enough for a single line
            [pval,s]=polyfit(xdatnew,ydatnew,0);
            ydatnew=polyval(pval,xdatnew,s);
            yholdnew=[yholdnew ydatnew(end)];
            xholdnew=[xholdnew xdatnew(end)];
            yhold2=[yhold2 yalign(end)];
            xhold2=[xhold2 xalign(end)];
        end
        if(kk==1)
            % the above loop is going to go through again, so changeing all the
            % properties
            nodetol=nodetol/2;
            ii=ceil(length(yholdnew)/(nodetol));
            yhold=yhold2;
            xhold=xhold2;
            yholdnew=[t(yhold(1))];
            xholdnew=[x(xhold(1))];
        end
    end
    
pref.holddat=[yholdnew' xholdnew'];
set(gca,'userdata',pref);
% if(~isempty(yholdnew))
%     % ploting picks
%     picksnew=[];
%     crosslnl=line(xholdnew,...
%         yholdnew,'color','r',...
%         'erasemode','normal','linewidth',2,...
%         'buttondown',[]);
%     picksnew=[picksnew crosslnl];
% end
% 
% 
% The following is taking place to get by with not using "eval" in matlab.
% The line is being created because when it is deleted it will execute the
% the transfer fucntion.
delete(line([0 0],[0 0],'deletefcn',transfer));

%--------------
%- Data Check -
%--------------
%
% Just doing a quick data check everytime to make sure that data has some
% how not been lost from the GCA.  This section should be updated if more
% preferenaces are added.
function checkdata=PLE_checkdata(dat)
checkdata=[];
if(isempty(dat))
elseif(~ishandle(dat))
else
    pref.seisstruct=seisstruct;
    pref.axes=masteraxes;
    pref.transfer=trans;
    pref.holddat=[];
    pref.checkfields=checkfield;
    fnames=fieldnames(dat);
    for ii=1:size(checkfield)
        checked=[];
        for jj=1:size(fnames)
            if(strcmp(checkfield{ii},lower(fnames{jj})))
                checked='Yup';
                checkfield{ii}=getfield(preferances,fnames{jj});
                break
            end
        end
        if(isempty(checked))
            stringinfo={'Your preferances are not set up properly.',...
                    'Please see PICKSLE help for details in proper use',...
                    'or reset PICKSLE on your program'};
            helpdlg(stringinfo,'Check PICKSLE help file');
            return
        end
    end
end
checkdata=['Data is correct'];