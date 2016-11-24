function DataOut=matinterrogator(OpenFile,n,arg3)
%----------------------------------------
%--------- Mat File Interogator --------- 
%----------------------------------------
%
% DataOut=matinerogator;
% DataOut=matinerogator(OpenFile);
% DataOut=matinerogator(OpenFile,n);
%           - arg3 is for program use only
%
% matinterrogator will display the initial arrays in a .mat file (maximum of
% 40).  Initializing matinterrogator in the command window will simply pop
% up a selection window for users to choose the .mat file they want to
% interogate.  DataOut will be released when at least one array is choosen
% for export or if matinterrogator is canceled or closed.  
%
% OpenFile  - The desired file that the user wants to interogate.
%    n      - User specified maximum number of arrays to be choosen and
%           exported.  Defaulted to max 40.
% 
% C.B. Harrison 2002
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
    action='init';
    n=[40];
    OpenFile='';
    arg3='';
elseif(nargin==1)
    action='init';
    n=[40];
    arg3='';
elseif(nargin==2)
    action='init';
    n=round(n);
    if(n>=41)
        n=40;
    end
    arg3='';
elseif(nargin==3)
    action='init';
    if(~isnumeric(n))
        n=[40];
    else
        n=round(n);
        if(n>=41)
            n=40;
        end
    end
end
if strcmp(action,'init')
    if(isempty(arg3))
        if(isempty(deblank(OpenFile)))
            [file,path]=myuifile(gcf,'.mat','Choose .mat file','get');
            if(isempty(file))
                return
            end
            OpenFile=[path file];
            if(size(OpenFile,2)<=3)|(~findstr(OpenFile(end-3:end),'.mat'))
                errordlg('Please Chose A .mat File To Interogate');
                return
            end        
        end
    end
    % must make names of number due to matinterrogator using eval
    numnames={'One' 'Two' 'Three' 'Four' 'Five' 'Six' 'Seven' 'Eight',...
            'Nine' 'Ten' 'Eleven' 'Twelve' 'Thirteen' 'Fourteen' 'Fifteen',...
            'Sixteen' 'Seventeen' 'Eighteen' 'Nineteen' 'Twenty',...
            'TwentyOne' 'TwentyTwo' 'TwentyThree' 'TwentyFour' 'TwentyFive',...
            'TwentySix' 'TwentySeven' 'TwentyEight' 'TwentyNine' 'Thirty',...
            'ThirtyOne' 'ThirtyTwo' 'ThirtyThree' 'ThirtyFour' 'ThirtyFive',...
            'ThirtySix' 'ThirtySeven' 'ThirtyEight' 'ThirtyNine' 'Forty'};
    if(isempty(deblank(arg3)))
        try
            CheckFile=load(OpenFile);
        catch
            stringinfo=['Can Not Load: ' OpenFile];
            errordlg(stringinfo);
            return
        end
        FNames=fieldnames(CheckFile);
        nm=['Mat File Interogator ' OpenFile];
    else
        if(iscell(OpenFile))
            FNames=cell(1,size(OpenFile,1)*size(OpenFile,2));
            kk=1;
            for ii=1:size(OpenFile,1)
                for jj=1:size(OpenFile,2)
                    FNames{kk}=numnames{kk};
                    kk=kk+1;
                end
            end
            CheckFile=OpenFile;
        elseif(isstruct(OpenFile))
            if(size(OpenFile,2)>=2)
                % strucut has more then one field
                FNames=cell(1,size(OpenFile,2));
                for ii=1:size(OpenFile,2)
                    FNames{ii}=numnames{ii};
                end
            else
                FNames=fieldnames(OpenFile);
            end
            CheckFile=OpenFile;
        end
        nm=['Interogator'];
    end
    narray=length(FNames);
    nwidth=80;
    for ii=1:narray
        if(50*ceil(length(FNames{ii})/10)>=80)
            nwidth=50*ceil(length(FNames{ii})/10);
        end
    end
    FigYSize=narray*20+90;
    FigXSize=nwidth+230;
    milite=figcent(FigXSize,FigYSize,'pixels');
    set(milite,'menubar','none','Name',nm,...
        'closerequestfcn',@Closematinterrogator,'tag','matinterrogator',...
        'visible','on','numbertitle','off');
    set(gca,'position',[0 0 .0001 .00001],'visible','off');
    CUnits='Pixels';
    % Control Titles of info given out by matinterrogator
    hmsg=uicontrol('style','text','units',CUnits,'position',[0 FigYSize-20 FigXSize 20],...
        'string','Choose or Export Desired Files','backgroundcolor',[1 1 1]);
    hm1=uicontrol('style','text','units',CUnits,'position',[40 FigYSize-40 nwidth 15],...
        'string','Name');
    hm1=uicontrol('style','text','units',CUnits,'position',[50+nwidth FigYSize-40 80 15],...
        'string','Size');
    hm1=uicontrol('style','text','units',CUnits,'position',[140+nwidth FigYSize-40 80 15],...
        'string','Class');
    % building controls for mat file selections
    AllCheck=[];
    for ii=1:narray
        col={[.7 .7 .7] [1 1 1]};
        if(((ii/2)-round(ii/2))==0)
            jj=1;
        else
            jj=2;
        end
        hname=uicontrol('string',FNames{ii},'position',[40 FigYSize-40-20*ii nwidth 15],...
            'backgroundcolor',col{jj},'style','pushbutton','callback',@renamearray,...
            'tooltipstring','Press To Rename Array');
        if(isstruct(CheckFile))
            if(size(CheckFile,2)>=2)
                dat=CheckFile(ii);
            else
                dat=getfield(CheckFile,FNames{ii});
            end
        else
            dat=CheckFile{ii};
        end
        sz=[num2str(size(dat,1)) 'x' num2str(size(dat,2))];
        hsize=uicontrol('string',sz,'position',[50+nwidth FigYSize-40-20*ii 80 15],...
            'backgroundcolor',col{jj},'style','text');
        cl=class(dat);
        hclass=uicontrol('string',cl,'position',[140+nwidth FigYSize-40-20*ii 80 15],...
            'backgroundcolor',col{jj},'style','pushbutton','callback',@inspectarray,...
            'tooltipstring','Press To Inspect Array');
        hclick=uicontrol('style','radiobutton','position',[0 FigYSize-40-20*ii 30 15],...
            'backgroundcolor',col{jj},'callback',@HighLightButton,...
            'tooltipstring','Press To Select Array For Export');
        set(hname,'userdata',{dat hname hsize hclass}); 
        set(hsize,'userdata',{dat hname hsize hclass});
        set(hclass,'userdata',{dat hname hsize hclass});
        set(hclick,'userdata',[hclick hname hsize hclass]);
        AllCheck=[AllCheck; hclick];
    end
    % Control Buttons
    hexport=uicontrol('string','Choose','callback',@ExportFiles,'position',[0 0 80 25],...
        'userdata',{gcf n 0 [] {}});
    hb=uicontrol('string','Close','callback',@Closematinterrogator,'position',[90 0 80 25]);
    % holding till user has closed or continued
    hb=uicontrol('string','Save','callback',@ExportFiles,'position',[180 0 80 25]);
    set(gcf,'userdata',{hmsg AllCheck {} hexport});
end
uiwait(gcf);
if(~ishandle(gcf))
else
    h=get(milite,'userdata');
    hexport=h{end};
    dat=get(hexport,'userdata');
    dat=dat{end};
    if(isempty(dat))
        DataOut={};
    else
        ndat=dat{1};
        ddat=dat{2};
        for ii=1:size(ndat,1)
            makestruct=['DataOut.' ndat{ii} '=ddat{ii};'];
            eval(makestruct);
        end
    end
    delete(gcf);
end

% RENAMEING THE ARRAYS
%----------------------
function renamearray(hObject, eventdata, handles)
h=get(gcf,'userdata');
hmsg=h{1};
hhandles=h{2};
hbut=gco;
nm=get(hbut,'string');
masterfig=gcf;
qst={'Please Type A New Name'};
a={nm};
flags=[1];
set(hmsg,'string','Change Name Of Array','backgroundcolor',[1 1 1]);
titlestr=['Choose A New Name For The Array.  Name Can Not Be Numeric or Contain Special Characters'];
ttstr={'Press Cancel To Quit Renameing'};
ansfini=askthingsle(masterfig,qst,a,flags,titlestr,ttstr);
if(isempty(ansfini))
    set(hmsg,'string','Action Canceled','backgroundcolor',[1 1 1]);
    return
else
    nm=deblank(ansfini{1});
    % due to matinterrogator using "eval" to save fils in strut format, the
    % following characters can no be used as or parts of names of Arrays
    checkchar={'[' ']' '(' ')' '{' '}' '=' '.'  ',' ';' '%' '!',...
            '+' '-' '*' '/' '\' '^' '<' '>' '=' '~' '&' '|' '@',...
            '#' '$' '`' ':' '"'};
    for ii=1:length(checkchar)
        if(~isempty(findstr(checkchar{ii},nm)))
            stringinfo=['Can Not Use "' checkchar{ii} '" In Name'];
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
            return
        end
    end
    if(~isempty(str2num(deblank(ansfini{1}))))
        % will not allow user to change name to a number
        stringinfo='Name Can Not Be Numberic';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
    else
        % checking to make sure user has not already choosen a name that is
        % presently the name of another array
        nms=cell(1,length(hhandles));
        for ii=1:length(hhandles)
            hclick=hhandles(ii);
            dat=get(hclick,'userdata');
            hname=dat(2);
            if(hbut==hname)
                % skipping
            else
                nms{ii}=deblank(get(hname,'string'));
            end
        end
        % checking names
        for ii=1:length(nms)
            if(strcmp(nms{ii},nm))
                stringinfo='Duplicattion Of Array Names Not Allowed';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
                return
            end
        end
        set(hbut,'string',nm);
    end
end

% EXPOTING FILES 
%----------------
function ExportFiles(hObject, eventdata, handles)
h=get(gcf,'userdata');
hobj=gco;
hmsg=h{1};
StringInfo=['Saving Data To Disk'];
set(hmsg,'string',StringInfo,'backgroundcolor',[1 1 1]);
hexport=h{end};
dat=get(hexport,'userdata');
exporthandles=dat{4};
if(isempty(exporthandles))
    set(hmsg,'string','No Data to Export','backgroundcolor',[1 1 0]);
    return
end
exportnames=cell(length(exporthandles),1);
exportdata=cell(length(exporthandles),1);
for ii=1:length(exporthandles)
    dat=get(exporthandles(ii),'userdata');
    exportnames{ii}=get(dat(2),'string');
    udat=get(dat(4),'userdata');
    exportdata{ii}=udat{1};
end

checkwhich=get(hobj,'string');
if(strcmp(checkwhich,'Save'))
    % not exporting but saving files
    [file,path]=myuifile(gcf,'*.mat','Save File As','put');
    if(file==0)
        set(hmsg,'string','Action Canceled','backgroundcolor',[1 1 0]);
        return
    end
    if(isempty(findstr(file,'.mat')))
        file=[path file '.mat'];
    end
    dat=[];
    for ii=1:size(exportnames,1)
        xx=[exportnames{ii} '= exportdata{ii};'];
        eval(xx);
        dat=[dat ' ' exportnames{ii}];        
    end
    saveingdata=['save ' file dat];
    eval(saveingdata);
    stringinfo=['Data saved:  ' file];
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
else
    % this will close the figure so sticking all data in to the hexport handle allowing
    % the main program to export just the pertinant data
    dat=get(hexport,'userdata');
    dat{end}={exportnames exportdata};
    set(hexport,'userdata',dat);
    uiresume(gcf);
end

% HIGH LIGHT BUTTONS
%--------------------
function HighLightButton(hObject, eventdata, handles)
% This simply will get user data of check box clicked and set their background
% and foreground colour to something different
hdat=get(gcbo,'userdata');
val=get(hdat(1),'value');
h=get(gcf,'userdata');
hmsg=h{1};
hcheck=h{2};
hexport=h{end};
CheckExport=get(hexport,'userdata');
n1=CheckExport{2};
n2=CheckExport{3};
ExportDat=CheckExport{4};
CheckButton=find(hdat(1)==hcheck);
switch val
case 1
    if(ischar(n1))
    elseif(n2>=n1)
        % Checking to make sure that user is not over selecting number of files specified
        set(hmsg,'string',['Only ' num2str(n2) ' can be selected.'],'backgroundcolor',[1 0 1]);
        set(hdat(1),'value',0);
        return
    end
    set(hdat,'backgroundcolor',[1 .5 0]);
    set(hdat(1),'tooltipstring','Press To Un-Select Array');
    sel='Selected';
    ad=[1];
    ExportDat=[ExportDat;hdat(1)];
case 0
    col={[.7 .7 .7] [1 1 1]};
    if(((CheckButton/2)-round(CheckButton/2))==0)
        % remember that the first button saved in userdata is the hmsg
        jj=1;
    else
        jj=2;
    end
    set(hdat,'backgroundcolor',col{jj});
    set(hdat(1),'tooltipstring','Press To Select Array For Export');
    sel='Unselected';
    tg='';
    ad=[-1];
    kll=find(ExportDat==hdat(1));
    ExportDat(kll)=[];
end
StringInfo=['You have ' sel ' "' get(hdat(2),'string') '".'];
set(hmsg,'string',StringInfo,'backgroundcolor',[1 1 1]);
set(hexport,'userdata',{CheckExport{1} n1 n2+ad ExportDat {}});

% INSPECTING ARRAY
%------------------
function inspectarray(hcbo,eventdata)
h=get(gcf,'userdata');
hmsg=h{1};
hhandles=h{2};
masterfig=gcf;
hbut=gco;
udat=get(hbut,'userdata');
dat=udat{1};
if(strcmp(class(dat),'char'))
    qst=cell(1,size(dat,1));
    a=qst;
    flags=[];
    if(size(dat,1)>=2)
        for ii=1:size(dat,1)
            x=0;
            qst{ii}=['Character Array ' num2str(ii)];
            a{ii}=deblank(dat(ii,:));
            flags=[flags 0];
        end
    else
        qst={'Character Array: ' 'Change Class To: '};
        a={num2str(dat) 'String|Numeric';};
        flags=[1 1];        
    end
    set(hmsg,'string','Change Array Data','backgroundcolor',[1 1 1]);
    titlestr=['Type New Array Data'];
    ansout=askthingsle(masterfig,qst,a,flags,titlestr);
    if(size(ansout,2)==2)
        checkcl=ansout{2};
        checkans=ansout{1};
        switch checkcl
            case 'Numeric'
                ansout=str2double(checkans);
                if(isnan(ansout))
                    stringinfo='Answer Was Not Numeric';
                    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
                    return
                end
            case 'String'
                ansout=ansout{1};
            otherwise
        end
    else
        if(isempty(ansout))
        else
            ans=[];
            for ii=1:size(ansout,2)
                ans=strmat(ans,ansout{ii});
            end
            ansout=ans;
        end
    end
elseif(strcmp(class(dat),'cell'))
    ansout=matinterrogator(dat,100,'Inspection');
    ansout=ansout;
elseif(strcmp(class(dat),'struct'))
    ansout=matinterrogator(dat,100,'Inspection');
    ansout=ansout;
else
    ansout='';
    if(length(dat)<=1)
        set(hmsg,'string','Change Array Data','backgroundcolor',[1 1 1]);
        qst={'Array Value: ' 'Change Class To: '};
        a={num2str(dat) 'Numeric|String'};
        flags=[1 0];
        titlestr=['Change Array Value'];
        ansout=askthingsle(masterfig,qst,a,flags,titlestr);
        if(isempty(ansout))
        else
            checkcl=ansout{2};
            checkans=ansout{1};
            switch checkcl
                case 'Numeric'
                    ansout=str2double(checkans);
                    if(isnan(ansout))
                        stringinfo='Answer Was Not Numeric';
                        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
                        return
                    end
                case 'String'
                    ansout=ansout{1};
                otherwise
            end
        end
    else
        % this will cover all the class' associated with numbedrs; double,
        % single, int8, uint8, et
        stringinfo='Desired Plot Technique';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
        butt=questdlg('Choose How To Plot','Chose Way For Array To Be Plotted',...
            'Stack Plot','Arranged Plot','Cancel','Cancel');
        switch butt;
            case 'Stack Plot'
                figure;
                set(gcf,'menubar','none');
                plot(dat);
                ansout={};
            case 'Arranged Plot'
                figure;
                set(gcf,'menubar','none');
                if(size(dat,1)>=size(dat,2))
                else
                    dat=dat';
                end
                plot([1:1:size(dat,2)],dat);
            case 'Cancel'
                stringinfo='Choose or Save Desired Files';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                return
            otherwise
                stringinfo='Choose or Save Desired Files';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                return
        end
        
    end
end

if(isempty(ansout))
    stringinfo='Choose or Save Desired Files';
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
else
    stringinfo='Array Has Been Changed';
    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
    hname=udat{2};
    hsize=udat{3};
    hclass=udat{4};
    sz=[num2str(size(ansout,1)) 'x' num2str(size(ansout,2))];
    set(hsize,'string',sz,'userdata',{ansout hname hsize hclass});
    cl=class(ansout);
    set(hclass,'string',cl,'userdata',{ansout hname hsize hclass});
    set(hname,'userdata',{ansout hname hsize hclass});
end

% CLOSING matinterrogator
%------------------------
function Closematinterrogator(hcbo,eventdata)
try
    h=get(gcf,'userdata');
    hexport=h{end};
    dat=get(hexport,'userdata');
    dat{end}='';
    set(hexport,'userdata',dat);
    uiresume(gcf);
catch
    uiresume(gcf);
end