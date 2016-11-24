function PI_rescale(action)
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


global SMEAN STDDEV MXS
mainax=findobj(gcf,'type','axes','tag','MAINAXES');
posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
% try
h=get(gcf,'userdata');
hmsg=h{2};
hdat=h{4};
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
mxsold=dat(2); smeanold=dat(4); stddevold=dat(5);
% mnsold=dat(3); 
%new opt
newscaleopt=get(hscale,'value');
dat(1)=newscaleopt;
%get clip value
inewclip=get(hclip,'value');
ioldclip=get(hclip,'userdata');
set(hclip,'userdata',inewclip);
clips=get(hdat,'userdata');
clipold=clips(ioldclip);
clipnew=clips(inewclip);

%get number of columns in colormap
clrmap=get(gcf,'colormap');
nkols=size(clrmap,1);

%flag=computer;
%flag='shit';

if(ampflag==1)
    % Figure is Independant
    col=[.8314 .8157 .7843];
    TTS='';
    stddev2=stddev;
    mxs2=mxs;
    smean2=smean;
elseif(ampflag==2)
    % Figure is Master
    col=[1 .5 0];
    TTS='';
    %global SMEAN STDDEV MXS
    stddev2=stddev;
    mxs2=mxs;
    smean2=smean;
    SMEAN=smean2;
    STDDEV=stddev;
    MXS=mxs;
elseif(ampflag==3)
    % while figure is slaved, automatically Max Scaling
    col=[1 1 0];
    TTS='Scaling Automatically Forced Master Figure Scaling';
    % Figure is Slave
    limlns=findobj(gcf,'type','line','tag','LIMITLINE');
    limpts=findobj(gcf,'type','line','tag','LIMITPOINT');

    if(~isempty(limlns))
        delete(limlns);
        delete(limpts);
        set(hlimbox,'userdata',[]);
    end
    %global SMEAN STDDEV MXS
    stddev2=STDDEV;
    mxs2=MXS;
    smean2=SMEAN;
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
ttle=get(mainax,'title');
ttledat=get(ttle,'userdata');
nm=ttledat{3};
if(~ischar(nm))
    return
end
switch nm
    case 'Seismic'
    case 'Amp Spectrum'
        smat=abs(smat);
end
% this next section is called if the limit box has been moved
lmlns=findobj(gcf,'type','line','tag','LIMITLINE');
if(strcmp(action,'limboxrescale')||~isempty(lmlns))
    hlimbox=h{14};
    limfig=get(hlimbox,'userdata');
    if(~isempty(limfig))
%        limdat=get(limfig{3},'userdata')
        limmat=limfig{3};
%         for ii=1:4
%             limmat(ii) = str2double(get(limdat{ii},'string'));
%         end
        imxdat=get(hi,'xdata'); imydat=get(hi,'ydata');
        xdat=find(imxdat>=limmat(3) & imxdat<=limmat(4));
        ydat=find(imydat>=limmat(1) & imydat<=limmat(2));
        if(isempty(xdat)||isempty(ydat))
            return
        end
        %dddd=[xdat(1) xdat(end) ydat(1) ydat(end)];
        ttle=get(findobj(gcf,'type','axes','tag','MAINAXES'),'title');
        ttledat=get(ttle,'userdata');
        nm=ttledat{3};
%         switch nm
%             case 'Seismic'
%             case 'Amp Spectrum'
%                 %                     if(size(smat,1)>length(ydat))
%                 %                     else
%                 %                         aydat=xdat; 
%                 %                         axdat=ydat; 
%                 %                         ydat=aydat;
%                 %                         xdat=axdat;
%                 %                     end
%         end
        seis2=smat(ydat,xdat);
        if(isempty(seis2))
            return
        end
        mxs2=full(max(max(abs(seis2))));
        smean2=full(mean(mean(seis2)));
        stddev2=full(sqrt( sum(sum((seis2-smean2).^2 ) )...
            /numel(seis2)));
        %global SMEAN STDDEV MXS
        STDDEV=stddev2;
        MXS=mxs2;
        SMEAN=smean2;
    end
end
%apply new scalingp
if(newscaleopt==1) % mean scaling
    mxsprime = min([smean2+clipnew*stddev2,mxs2]);
    mns=-mxsprime;
    if(isempty(mns)||isempty(mxs2)||isempty(smat))
        return
    end
    seis = (smat -mns)/(mxsprime-mns)*(nkols-1)+1;
    set(hclip,'visible','on');
elseif(newscaleopt==2)  % max scaling
    mns=-mxs2;
    if(isempty(mns)||isempty(mxs2)||isempty(smat))
        return
    end
    seis = (smat -mns)/(mxs2-mns)*(nkols-1)+1;
    set(hclip,'visible','off');
end
switch nm
    case 'Seismic'
    case 'Amp Spectrum'
        seis=abs(seis);
end
dat(1)=newscaleopt;
dat(2)=mxs2;
dat(3)=mns;
dat(4)=smean2;
dat(5)=stddev2;
set(hscale,'userdata',dat);

set(hi,'cdata',seis);
h=get(get(posax,'title'),'userdata');
[m, n]=size(seis);
seis2=seis(1:6:m,1:6:n);
set(h(5),'cdata',seis2);

% The following section is going to check if current figure is master
% and if it is Master, find slave figures and have them change
stringinfo='Rescaling image'; 
if(ampflag==2)
    cfig=gcf;
    findfig=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
    if(length(findfig)==1)
        return;
    else
        for ii=1:length(findfig)
            if(findfig(ii)==cfig)
            else
                set(0,'currentfigure',findfig(ii))
                h=get(findfig(ii),'userdata');
                hmaster=h{10};
                if(get(hmaster,'value')==3)
                    h=get(gcf,'userdata');
                    hmsg2=h{2};
                    hscale=h{6};
                    hclip=h{7};
                    if(newscaleopt==1)
                        set(hclip,'visible','on','value',inewclip);
                        set(hscale,'value',1);
                    else
                        set(hclip,'visible','off');
                        set(hscale,'value',2);
                    end
                    plotimage('rescale');
                    stringinfo='Rescaling image and slave(s)';
                    stringinfo2='Rescaling due to master rescale';
                    set(hmsg2,'string',stringinfo2,'backgroundcolor',[1 1 1]);
                end
            end
        end
    end
    set(0,'currentfigure',gcf);
end
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
% the following is going to turn off position axes if it is supposed to be
% off... I just can't quickly find a better place for it
hbak=findobj(gcf,'tag','BACKING');
checkon=get(hbak,'visible');
if(strcmp(checkon,'off'))
    set(findobj(gcf,'type','axes','tag','POSITIONAXES'),'visible','off');
    set(get(findobj(gcf,'type','axes','tag','POSITIONAXES'),'children'),'visible','off','hittest','off');
end