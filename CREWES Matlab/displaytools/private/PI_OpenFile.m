function PI_OpenFile(varargin)
%function PI_OpenFile(varargin)
% Open files for Plotimage
% cases
%  varargin = {}
%  varargin = {thisfig,smat,t,dist}
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

%

% Declare globals used in this function
global SCALE_OPT AMPFLAG NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP PICKCOLOR XAXISTOP IMCONTROLS ZOOM_LOCKS
global SEISMAT PICKS ZOOM_VALUE NOBRIGHTEN
global SMEAN STDDEV MXS

ylbl='Samples';
xlbl='Columns';

switch nargin
    case 0 %no args
        thisfig = PI_GetFigureHandle();
        dataout = [];
        t = [];
        dist = [];
        val=get(gcbo,'userdata');
    case 4 %thisfig, smat, t, d
        [thisfig, dataout, t, dist] = varargin{:};
       
        if ~isempty(t)
           ylbl='Time'; 
        end
        
        if ~isempty(dist)
           xlbl='Distance'; 
        end

        val={4};
        file='Plotimage';
    otherwise
        error('crewes:displaytools:plotimage','Incorrect number of input arguments.');
end

% get information about the figure
h=get(thisfig,'userdata');
hmsg=h{2};
hscale=h{6};
hclip=h{7};
hmaster=h{10};
hvertscrol=h{16};
hhorscrol=h{17};
hposax=findobj(thisfig,'type','axes','tag','POSITIONAXES');
hmainax=findobj(thisfig,'type','axes','tag','MAINAXES');
QuickMenu=findobj(thisfig,'type','uimenu','tag','QUICK_OPENFILE');
udat=get(QuickMenu,'userdata');
quickhandles=udat{3};
try
    originalpath=udat{4};
    lastpath=udat{5};
catch % ex
    originalpath='';
    lastpath='';
end



% if(nargin<=1)
%     t=[];
%     dataout=[];
% %     tdat=[];
% %     sampint=[];
%     dist=[];
%     val=get(gcbo,'userdata');
% elseif(nargin==1)
%     t=[];
%     dataout=smat;
% %     tdat=[];
% %     sampint=[];
%     dist=[];
%     val={4};
%     file='Plotimage';
% elseif(nargin>=2)
%     dataout=smat;
%     if(isempty(t))
% %         tdat=[];
% %         sampint=[];
%     else
%         ylbl='Time';
% %         t=t;
% %         sampint=t(2)-t(1);
%     end
%     if(isempty(dist))
%         dist=[];
%     else
% %         dist=dist;
%         xlbl='Meters';
%     end
%     val={4};
%     file='Plotimage';
% end
lasterr=[];


% % try
    switch round(val{1})
        case 1    % opening PRE arranged .mat file
            [file,path]=myuifile(thisfig,'.mat','Choose .mat file','get');
            if(isempty(strfind(file,'*.mat')))
                return  % didn't choose a .mat file
            end
            cd(path);
            DataOut=load([path file]);
            nms=fieldnames(DataOut);
            if(length(nms)>=2)   
                % not set as SeisStrucutre
                DataOut=matinterrogator([path file],3);
                if(isempty(DataOut)) 
                    return
                else
                    DataOut=Check_Data(DataOut);
                    dataout=DataOut{1};
                    tdat=DataOut{2};
                    t=tdat{1};
                    ylbl=tdat{2};
                    dist=DataOut{3};
                end
            else
                if(~strcmp(nms,'SeisStruct'))
                    return
                end
                SeisStruct=DataOut.SeisStruct;
                dataout=SeisStruct.data;
                t=SeisStruct.t;
%                 samptint=t(2)-t(1);
                dist=SeisStruct.x;
            end
            filename=[path file];
        case 2    % opening segy file
            [file,path]=myuifile(thisfig,'*.*','Choose SEGY file','get');
            filename=[path file];
            if(filename(1,1)==0)
                return
            end
            if(isempty(strfind('.sgy',file(end-4:end))))
                StringInfo=['"' [path file] '" is not a SEGY file.'];
                errordlg(StringInfo);
                return
            end
            %[dataout,sampint]=altreadsegy([path file]);
            dataout = altreadsegy([path file]);
        case 3    % opening file from menu item
            filename=get(gcbo,'label');
            try
                if(~isempty(strfind('.sgy',filename(end-4:end))))
                    %[dataout,sampint] = altreadsegy(filename);
                    dataout = altreadsegy(filename);
                elseif(~isempty(strfind('.mat',filename(end-4:end))))
                    DataOut=load(filename);
                    nms=fieldnames(DataOut);
                    if(length(nms)>1)
                        % not set as SeisStrucutre
                        DataOut=matinterrogator(filename,3);
                        if(isempty(DataOut))
                            return
                        else
                            DataOut=Check_Data(DataOut);
                            dataout=DataOut{1};
                            tdat=DataOut{2};
                            t=tdat{1};
                            ylbl=tdat{2};
                            dist=DataOut{3};
                        end
                    else
                        if(~strcmp(nms,'SeisStruct'))
                            % SesiStruct's have not been created yet August 8th, 2002
                            DataOut=Check_Data(DataOut);
                            dataout=DataOut{1};
                            tdat=DataOut{2};
                            t=tdat{1};
                            ylbl=tdat{2};
                            dist=DataOut{3};
                        else
                            SeisStruct=DataOut.SeisStruct;
                            dataout=SeisStruct.data;
                            t=SeisStruct.t;
%                             samptint=t(2)-t(1);
                            dist=SeisStruct.x;
                        end
                    end
                end
                tp=strfind(filename,'\');
                file=filename(tp(end)+1:end);
            catch
                errordlg(lasterr);                    
            end
        case 4 % data has been sent via user
            % MUST MAKE USER DATA INTO A STRUCTURE!!!!
            % field names do not matter due to way data pull appart
            DataOut.dataout=dataout;
            DataOut.t=t;
            if(isempty(dist))
                dist=1:size(dataout,2);
            end
            DataOut.dist=dist;
            DataOut=Check_Data(DataOut);
            dataout=DataOut{1};
            tdat=DataOut{2};
            t=tdat{1};
            ylbl=tdat{2};
            dist=DataOut{3};
    end
%     if(isempty(SCALE_OPT)); SCALE_OPT=2; end %default to max scaling
    set(hscale,'value',SCALE_OPT);
    %set(hclip,'value',CLIP);
    if(SCALE_OPT==1)
        if(strcmp(IMCONTROLS,'on'))
            set(hclip,'visible','on');
        else
            set(hclip,'visible','off');
        end
    else
        set(hclip,'visible','off');
    end
    % checking data
    if(iscomplex(dataout))
        questinfo={'Do you want to continue?  ',...
                'Main array is complex'};
        checkwuser=questdlg(questinfo,'Main array has both imaginary and real parts.',...
            'Real','Imaginary','Cancel','Real');
        switch checkwuser
            case 'Real'
                dataout=real(dataout);
            case 'Imaginary'
                dataout=imag(dataout);
            case 'Cancel'
                stringinfo='Action Canceled';
                set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                return
        end
    end  
    if(isempty(dataout)||~isnumeric(dataout)||iscomplex(t)||iscomplex(dist))
        lasterr='Neither depth nor distance array should be complex';
        if(isempty(dataout))
            lasterr='Main array not found';
        end            
        errordlg(lasterr);
        return
    elseif(isempty(t)||length(t)<=1)
        sampint=1;
        nrows=size(dataout,1);
        t=(0:1:nrows-1)*sampint;    
    end
    ncols=size(dataout,2);
    if(isempty(dist))
        dist=1:ncols;
    else
        checkdist=dist(end)-dist(1);
        if(checkdist==0)
            % something is wrong with the distance 
            questinfo={'Do you want to continue?',...
                    'Plotimage will change horizontal sample rate if continuing'};
            checkwuser=questdlg(questinfo,'Distance is not consistant',...
                'Continue','Change sample rate','Cancel','Coninue');
            switch checkwuser
                case 'Continue'
                    dist=1:ncols;
                case 'Change sample rate'
                    masterfig=thisfig;
                    qst={'New Sample Rate'};
                    a={'1'};
                    flags=1;
                    ansfini=askthingsle(masterfig,qst,a,flags);
                    if(isempty(ansfini))
                        stringinfo='Action Canceled';
                        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                        return
                    elseif(~isnumeric(str2double(ansfini{1})))
                        stringinfo='Need a numberical value for sample rate';
                        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 0]);
                        return
                    end
                    dx=str2double(ansfini{1});
                    dist=1:dx:ncols-1;
                case 'Cancel'
                    stringinfo='Action Canceled';
                    set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
                    return
                    
            end
        end
    end
    %save the unaltered data in the picktool button
    hpicktool=findobj(thisfig,'tag','picktool');
    set(hpicktool,'userdata',{dataout, t, dist});
%     nrows=size(dataout,1);    
%     clips=[30 25 20 15 10 9 8 7 6 5 4 3 2 1 .5 .25 .1 .05 .01 .005 .001];
%     iclip=get(hclip,'userdata');
%     clip=clips(iclip); %make sure we have a sanctioned value

%     if(length(dist)>1)
%         bnds=(max(dist)-min(dist))/(length(dist)+1);
%     else
%         bnds=max(dataout-min(dataout))/2;
%     end
%     try
%         load('plotimageproperties.mat');
%     catch
%         disp('No properties');
%     end
%     if(isempty(AMPFLAG)); Ampflag='I'; else Ampflag=AMPFLAG; end

% %     if(isempty(NOBRIGHTEN)), nobrighten=0; else nobrighten=NOBRIGHTEN; end
%     if (isempty(SCALE_OPT)), scaleopt=2; else scaleopt=SCALE_OPT; end %default to max scaling
%     if (isempty(NUMBER_OF_COLORS)), number_of_colors=64; 
%     else number_of_colors=NUMBER_OF_COLORS; end %default to 64 gray levels
%     if (isempty(GRAY_PCT)), gray_pct=50; else gray_pct=GRAY_PCT; end %default to 50% gray transition
%     if (isempty(CLIP)), clip=4; else clip=CLIP; end %default to clip of 4
%     if (isempty(COLOR_MAP)||strcmp(COLOR_MAP,'seisclrs'))
%         COLOR_MAP=seisclrs(number_of_colors,gray_pct);
%         nkols=size(COLOR_MAP,1);
%     else
% %         COLOR_MAP=COLOR_MAP;
%         cm=get(thisfig,'colormap');
%         nkols=size(cm,1);
%     end
%     if (isempty(PICKCOLOR))
%         PICKCOLOR='r';
%     end
    PI_SetColorMap(thisfig);
    nkols = length(get(thisfig,'colormap'));
    color_map = get(thisfig,'colormap');
    
    mxs=full(max(max(abs(dataout))));
    %determine clipping
    smean=full(mean(mean(dataout)));
    stddev=full(sqrt( sum(sum((dataout-smean).^2 ) )...
        /numel(dataout)));
    smean2=smean;
    mxs2=mxs;
    stddev2=stddev;
    if(PI_AmpFlagValue==2)
        SMEAN=smean;
        STDDEV=stddev;
        MXS=mxs;
    elseif(PI_AmpFlagValue==3)
        if(isempty(SMEAN))
            error('Cannot create a Slave figure before a Master')
        end
        smean2=SMEAN;
        stddev2=STDDEV;
        mxs2=MXS;
    end
    
    if SCALE_OPT ==1 %mean scaling
        if(~isnan(CLIP))
            mxsprime=min([smean2+CLIP*stddev2,mxs2]);
        end
        mns=-mxsprime;
        seis = (dataout -mns)/(mxsprime-mns)*(nkols-1)+1;
        clear smat
    elseif SCALE_OPT == 2
        mns=-mxs;
        %mns=0;
        seis = (dataout -mns)/(mxs-mns)*(nkols-1)+1;
        clear dataout
    else
        error('invalid scaling option');
    end
    
%     dataout=[];
    
    ix=1:length(dist);
%     dx=dist(2)-dist(1);
%     dt=t(2)-t(1);
%     if(dx~=(dist(3)-dist(2)))
%         dx=0;
%     end
%     if(dt~=(t(3)-t(2)))
%         dt=0;
%     end
    seis=seis(:,ix);
    set(thisfig,'currentaxes',hmainax);
    % the image operation completely destroys all data associated with the
    % axes in question. 
    hi=image(dist,t,seis(:,ix));%makes the big plot
    h{5}=hi;
    set(thisfig,'userdata',h);
    set(hi,'xdata',dist,'ydata',t);
    set(hvertscrol,'min',t(1),'max',t(end),'visible','off');
    set(hhorscrol,'min',dist(1),'max',dist(end),'visible','off');
    set(thisfig,'currentaxes',hposax);
    hi2=image(dist,t,seis(:,ix));%makes the little plot
    cdat=get(hi2,'cdata');
    cdat=cdat(1:6:size(cdat,1),1:6:size(cdat,2));
    set(hi2,'xdata',dist,'ydata',t,'cdata',cdat);
    set(hposax,'visible','on','xtick',[],'ytick',[]);
    title('Holding Line Data');
    set(get(hposax,'title'),'visible','off');
    % position lines
    col='r';
    lwid=.25;
    % bottom
    ln1=line([dist(1) dist(end)],[t(end) t(end)],'color',col,'linewidth',lwid,'visible','off',...
        'buttondownfcn',@PI_positionaxes_linebuttondown);
    % top
    ln2=line([dist(1) dist(end)],[t(1) t(1)],'color',col,'linewidth',lwid,'visible','off',...
        'buttondownfcn',@PI_positionaxes_linebuttondown);
    % Left Side
    ln3=line([dist(1) dist(1)],[t(1) t(end)],'color',col,'linewidth',lwid,'visible','off',...
        'buttondownfcn',@PI_positionaxes_linebuttondown);
    % Right Side
    ln4=line([dist(end) dist(end)],[t(1) t(end)],'color',col,'linewidth',lwid,'visible','off',...
        'buttondownfcn',@PI_positionaxes_linebuttondown);
    % patch
    pt1=patch([dist(1) dist(end) dist(end) dist(1)],[t(1) t(end) t(end) t(1)],col);
    set(pt1,'visible','off','buttondownfcn',@PI_positionaxes_linebuttondown,'facealpha',.02,...
        'edgecolor','none');
    
    set(get(hposax,'title'),'userdata',[ln1 ln2 ln3 ln4 hi2 pt1]);
    set(thisfig,'currentaxes',hmainax);
    set(hposax,'tag','POSITIONAXES');
    set(hmainax,'tag','MAINAXES');


    presentpath=pwd;
    nkols=size(get(thisfig,'colormap'),1);
    if(val{1}==4)
    else
        QuickMenu=findobj(thisfig,'type','uimenu','tag','QUICK_OPENFILE');
        udat=get(QuickMenu,'userdata');
        CheckMenu=udat{3};
        HoldNames=get(CheckMenu,'label');
        set(CheckMenu(1),'label',filename,'visible','on');
        jj=1;
        ii=2;
        while jj<=3
            if(strcmp(HoldNames{jj},filename))
                jj=jj+1;
            else
                if(isempty(deblank(HoldNames{jj})))
                    vis='off';
                else
                    vis='on';
                end
                set(CheckMenu(ii),'label',HoldNames{jj},'visible',vis);
                jj=jj+1;
                ii=ii+1;
            end
        end  
%         filenames=get(CheckMenu,'label');
        cd(originalpath)
        save('plotimagedata.mat','filenames');
        cd(lastpath)
    end
    h=get(thisfig,'userdata');
    hi=h{5};
    set(thisfig,'userdata',h);
    set(hscale,'userdata',[SCALE_OPT mxs2 mns smean2 stddev2]);
    set(hmaster,'userdata',[mxs smean stddev],'value',PI_AmpFlagValue,'backgroundcolor',[.8314 .8157 .7843]);
    set(hi,'userdata',{seis dist t {nkols color_map} [SCALE_OPT mxs2 mns smean2 stddev2] [mxs smean stddev]});
    plotimage('KillLimitLines(thisfig)');
    
    h=get(thisfig,'userdata');
    
    for ii=1:length(h)
        if(ishandle(h{ii}) && ii~=5)
            set(h{ii},'enable','on');
        end
    end
    set(QuickMenu,'userdata',{3 thisfig quickhandles originalpath presentpath});
    set(gca,'xaxislocation',XAXISTOP);
    gtfigs=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
    nm=1;
%     xxnm=1;
    adon='';
    for ii=1:length(gtfigs)
        haxs=findobj(gtfigs(ii),'type','axes','tag','MAINAXES');
        ttl=get(haxs,'title');
        dat=get(ttl,'userdata');
        if(~isempty(dat))
            xfile=dat{1};
            xnm=dat{2};
            if(strcmp(xfile,file));
                nm=nm+1;
                if(xnm>=nm)
                    nm=xnm+1;
                end
                adon=['(' num2str(gethandlenum(thisfig)) ')'];
            end 
        end
    end
    title([file adon],'tag','PLOTIMAGE-TITLE','fontweight','bold',...
        'userdata',{file nm 'Seismic'},'interpreter','none');
    xlabel(xlbl,'horizontalalignment','center');
    ylabel(ylbl,'tag','PLOTIMAGE-YLABEL');
%     newlocks=[];
    if(~isempty(ZOOM_LOCKS))
        for ii=1:size(ZOOM_LOCKS,1)
            CheckLock=ZOOM_LOCKS(ii,:);
            if(find(CheckLock==thisfig))
                ZOOM_LOCKS(ii,:)=[];
                ii=ii-1;
            end
        end
    end
    hzoompick=h{9};
    set(hzoompick,'value',1);
    selboxinit('plotimage(''zoom'')',1);
%     set(thisfig,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
    %     ms1=['data maximum: ' num2str(full(mxs))];
    % 	ms2=['data mean: ' num2str(full(smean))];
    % 	ms3=['data stddev: ' num2str(full(stddev))];
    % 	ms4=['number of gray levels ' int2str(number_of_colors)];
    % 	ms5=['Percentage of gray transition ' int2str(gray_pct)];
    %     ms5=[];
    %     ms6=[];
    %     msg=[];
    %     msg={ms1 ms2 ms3 ms4 ms5 ms6};
%     save('plotimageproperties.mat','XAXISTOP','PICKCOLOR','NOBRIGHTEN','COLOR_MAP',...
%         'GRAY_PCT','NUMBER_OF_COLORS','SCALE_OPT');
    %     msgbox(msg,'File Properties','help','modal');
    stringinfo=['"' file adon '" has been opened and plotted'];
    col=[1 1 1];
% % catch
% %     % going to remove file from Quick file menu if it is there
% %     QuickMenu=findobj(thisfig,'type','uimenu','tag','QUICK_OPENFILE');
% %     udat=get(QuickMenu,'userdata');
% %     CheckMenu=udat{3};
% %     HoldNames=get(CheckMenu,'label');
% %     HoldNames{5}='';
% %     jj=1;
% %     ii=1;
% %     while jj<=4
% %         if(strcmp(HoldNames{ii},filename))
% %             if(isempty(HoldNames{ii+1}))
% %                 vis='off';
% %             else
% %                 vis='on';
% %             end
% %             set(CheckMenu(jj),'label',HoldNames{ii+1},'visible',vis);
% %             jj=jj+1;
% %             ii=ii+2;
% %         else
% %             if(isempty(HoldNames{ii}))
% %                 vis='off';
% %             else
% %                 vis='on';
% %             end
% %             set(CheckMenu(jj),'label',HoldNames{ii},'visible',vis);
% %             jj=jj+1;
% %             ii=ii+1;
% %         end
% %     end 
% %     filenames=get(CheckMenu,'label');
% %     save('plotimagedata.mat','filenames');
% %     errordlg('Data choosen not in proper format');
% %     stringinfo=['Error in data choosen, please inspect your data for anomalies'];
% %     col=[1 1 0];
% % end
set(hmsg,'string',stringinfo,'backgroundcolor',col);

end %end function PI_OpenFile

% CHECKING DATA
%---------------
%
% This is pulling apart data
function DataOut=Check_Data(CheckData)
sampint={[] 'Rows' 0};
dist=[];
nms=fieldnames(CheckData);
if(isempty(CheckData))
    dataout='No Data Found';
else
    stp=1;
    for ii=1:size(nms,1)
        % Checking to see that all data sent is numberic
        cdat=CheckData.(nms{ii});
        if(~isnumeric(cdat))
%             DataOut='Some of data choosen is not numeric';
            stp=2;
        end
    end
end
if(stp==1) % keep going
    for ii=1:size(nms,1)
        cdat=CheckData.(nms{ii});
        if(size(cdat,1)>=2 && size(cdat,2)>=2)
            % choosing the first array that has bigger more then 2 rows and
            % colums as the main array
            dataout=cdat;
            kk=ii;
            break
        end
    end
    if(isnumeric(dataout))
        % newdat=cell(size(nms,1)-1,1);
        jj=1;
        newdat={};
        for ii=1:size(nms,1)
            cdat=CheckData.(nms{ii});
            if(isempty(cdat))
                % in all truth, this could be a problem, but it is assumed
                % that if an array is empty, it will be ignored
            elseif(ii~=kk)
                newdat{jj,1}=cdat;
                jj=jj+1;
            end
        end
        if(isempty(newdat))
        elseif(size(newdat,1)==1)
            %rw = size(newdat{1},1);
            [rw,cl] = size(newdat{1});
            dat=newdat{1};
            if(rw*cl==1)
                if(dat<=.05)
                    sampint={dat 'Seconds'};
                else
                    sampint={dat 'Meters'};
                end
            else 
                chdat=find([rw cl]~=1);
                if(length(chdat)>=2)
                    dataout='Sizes of data choosen do not match';
                else
                    if(size(dat,chdat)==size(dataout,1))
                        if((dat(2)-dat(1))<=.05)
                            sampint={dat 'Seconds'};
                        else
                            sampint={dat 'Meters'};
                        end
                    elseif(size(dat,chdat)==size(dataout,2))
                        dist=dat;
                    end
                    if(isempty(dist)&&isempty(sampint{1}))
                        % back up incase something has gone wrong
                        dataout='Sizes of data choosen do not match';
                    end
                end
            end
        elseif(size(newdat,1)==2)
            for ii=1:2
                if(~isempty(newdat{ii}))
                    [rw, cl]=size(newdat{ii});
                    dat=newdat{ii};
                    if(rw*cl==1&&isempty(sampint{1}))
                        if(CheckData{jj(ii)}<=.05)
                            sampint={dat 'Seconds'};
                        else
                            sampint={dat 'Meters'};
                        end
                    else 
                        chdat=find([rw cl]~=1);
                        if(length(chdat)>=2)
                            dataout='Sizes of data choosen do not match';
                        else
                            if(size(dat,chdat)==size(dataout,1)&&isempty(sampint{1}))
                                ndat=dat;
                                if((ndat(2)-ndat(1))<=.05)
                                    sampint={ndat 'Seconds'};
                                else
                                    sampint={ndat 'Meters'};
                                end
                            elseif(size(dat,chdat)==size(dataout,2)&&isempty(dist))
                                dist=dat;
                            end
                        end
                    end
                end
            end
        end
    end
end
DataOut=cell(3,1);
DataOut{1}=dataout;
DataOut{2}=sampint;
DataOut{3}=dist; 

end %end function Check_Data