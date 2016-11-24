function PI_SaveFile
% Saving files for Plot Image
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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS
global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKCOLOR XAXISTOP
global SMEAN STDDEV MXS


h=get(gcf,'userdata');
hmsg2=h{2};
hmsg=h{4};
hi=h{5};
hscale=h{6};
hclip=h{7};
hmaster=h{10};
hlimbox=h{14};
% limboxflag=get(hlimbox,'value');
ampflag=get(hmaster,'value');
stuff=get(hmaster,'userdata');
mxs=stuff(1);
smean=stuff(2);
stddev=stuff(3);
%get the data
seis=get(hi,'cdata');
%determine old scaling
dat=get(hscale,'userdata');
oldscaleopt=dat(1);
mxsold=dat(2);smeanold=dat(4); stddevold=dat(5);
% mnsold=dat(3); 
%new opt
% newscaleopt=get(hscale,'value');
% dat(1)=newscaleopt;
%get clip value
inewclip=get(hclip,'value');
ioldclip=get(hclip,'userdata');
set(hclip,'userdata',inewclip);
clips=get(hmsg,'userdata');
clipold=clips(ioldclip);
% clipnew=clips(inewclip);

%get number of columns in colormap
clrmap=get(gcf,'colormap');
nkols=size(clrmap,1);

% flag=computer;
% flag='shit';

if(ampflag==1)
    % Figure is Independant
    col=[.8314 .8157 .7843];
    TTS='';
%     stddev2=stddev;
%     mxs2=mxs;
%     smean2=smean;
elseif(ampflag==2)
    % Figure is Master
    col=[1 .5 0];
    TTS='';

%     stddev2=stddev;
%     mxs2=mxs;
    smean2=smean;
    SMEAN=smean2;
    STDDEV=stddev;
    MXS=mxs;
elseif(ampflag==3)
    % while figure is slaved, automatically Max Scaling
    col=[1 1 0];
    TTS='Scaling Automatically Forced Master Figure Scaling';
    % Figure is Slave
    limdat=get(hlimbox,'userdata');
    if(~isempty(limdat))
        delete(limdat{1});
        delete(limdat{2});
        delete(limdat{4});
        set(hlimbox,'userdata',[]);
    end

%     stddev2=STDDEV;
%     mxs2=MXS;
%     smean2=SMEAN;
end
set(hmaster,'backgroundcolor',col,'tooltipstring',TTS);
% 		%undo the old scaling
if( oldscaleopt == 1 ) %undo mean scaling
    mxsprime = min([smeanold+clipold*stddevold,mxsold]);
    mns=-mxsprime;
    smat = (seis-1)*(mxsprime-mns)/(nkols-1) + mns;
elseif( oldscaleopt == 2) %undo max scaling
    mns=-mxsold;
    smat = (seis-1)*(mxsold-mns)/(nkols-1) + mns;
end
seis=smat;
t=get(hi,'ydata');
x=get(hi,'xdata');
% dat=get(gcbo,'userdata');
val=get(gcbo,'userdata');
switch val{1}
    case 1
        [file,path]=myuifile(gcf,'*.*','Saving File as SEGY','put');
        if(path==0)
            return
        elseif(isempty(strfind(file(end+4:end),'.sgy')))
            file=[file '.sgy'];
        end
        filename=[path file];
        dt=t(2)-t(1);
        altwritesegy(filename,seis,dt);
        AddMenu(filename);
    case 2
        [file,path]=myuifile(gcf,'*.mat','Saving File as .mat','put');
    if(path==0)
        return
    end
    if(isempty(strfind(file,'.mat')))
        file=[file '.mat'];
    end
    filename=[path file];
    SeisStruct.data=seis;
    SeisStruct.x=x;
    SeisStruct.t=t;
    SeisStruct.lastsave=['Plot Image.  ' date];
    SeisStruct.comment=['At this point, there is no way to tell whether ',...
        'the y (depth) is in time or distance.  The default ',...
        'is time.'];
    

    save(filename,'SeisStruct');
    AddMenu(filename);
case 3
    save('plotimageproperties.mat',SCALE_OPT,NUMBER_OF_COLORS,GRAY_PCT,CLIP,COLOR_MAP,NOBRIGHTEN,PICKCOLOR,XAXISTOP);
    return
end
stringinfo=['Data has been save to "' filename '"'];
set(hmsg2,'string',stringinfo,'backgroundcolor',[1 1 1]);

function AddMenu(filename)
% Adding new Menu to quick menus
QuickMenu=findobj(gcf,'type','uimenu','tag','QUICK_OPENFILE');
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
savenames=get(CheckMenu,'label');
save('pltimgfilemenu','savenames');