function PI_zoompick(arg)
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

global PICKS AMP_PICKS
if(nargin<1)
    arg='';
end
h=get(gcf,'userdata');
hmsg=h{2};
%hi=h{5};
hzoompick=h{9};
hamp_picks_lbl=h{18};
value=get(hzoompick,'value');
delete(findobj(gcf,'type','line','tag','PICKMARKER'));
delete(findobj(gcf,'type','text','tag','PICKTEXT'));
col=[1 1 1];
switch value
case 1 %zooming
    selboxinit('plotimage(''zoom'')',1);
    set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
    stringinfo='Zooming enabled';
case 2 %picking time-dips old buffer
    drawlineinit('plotimage(''pick'')');
    set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
    stringinfo='MB1: click and drag to pick time dip. Pick as many as you want';
case 3 %picking time-dips new buffer
    % this removes all picked lines
    if(~isempty(PICKS))
        for ii=1:size(PICKS,1)
            CheckFigure=PICKS{ii,1};
            if(CheckFigure==gcf)
                PICKS{ii,3}=[];
                PICKS{ii,2}=[];
            end
        end
    end
    delete(findobj(gcf,'type','line','tag','PICKS'));
    drawlineinit('plotimage(''pick'')');
    set(gcf,'name','Seismic Image Plot, Picking new (Use MB1)');
    set(hzoompick,'userdata',[]);
    stringinfo='Existing picks discarded. MB1: click and drag to pick time dip. Pick as many as you want';
case 4 %picking amplitudes old buffer
%  code below is due to Chris Harrison and implemented his method of
%  advanced picking. This has been de-released and replaced by IPICK
%     col=[1 1 0];
%     udat=get(hi,'userdata');
%     seisstruct.SEIS=udat{1};
%     seisstruct.X=udat{2};
%     seisstruct.T=udat{3};
%     pref.hmsg=hmsg;
%     masteraxes=gca;
%     holdhandle=gca;
%     picksle('plotimage(''pick'')',seisstruct,masteraxes,pref);
%     set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
%     stringinfo='Automatic Picking has been enabled';
    if(isempty(arg))
        %here we are initializing ipick
        ipick;
        stringinfo='MB1: Make a series of discrete clicks to define trajectory for autopicking. MB3: signals done.';
    elseif(strcmp(arg,'fromipick'))
        %here we are accepting a transfer from ipick
        pickspec=get(gca,'userdata');
        showlabels=get(hamp_picks_lbl,'checked');
        if(isempty(AMP_PICKS))
            %text label
            xp=pickspec.xpick;
            tp=pickspec.tpick;
            xtext=xp(round(length(xp)/2));
            ttext=tp(round(length(xp)/2));
            pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                'visible',showlabels,'horizontalalignment','center');
            AMP_PICKS{1}=pickspec;
        else
            %check to see if this is a new event or replaces and old one
            newevent=1;
            nevents=length(AMP_PICKS);
            for k=1:nevents
                pickspec_old=AMP_PICKS{k};
                if(strcmp(deblank(pickspec.eventname),deblank(pickspec_old.eventname)))
                    if(pickspec_old.figurehandle==gcf)
                        newevent=0;
                        %text label
                        delete(pickspec_old.texthandle);%delete old label
                        xp=pickspec.xpick;
                        tp=pickspec.tpick;
                        xtext=xp(round(length(xp)/2));
                        ttext=tp(round(length(xp)/2));
                        pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                            'visible',showlabels,'horizontalalignment','center');
                        
                        AMP_PICKS{k}=pickspec;
                        break;
                    end
                end
            end
            if(newevent)
                nevents=nevents+1;
                %text label
                xp=pickspec.xpick;
                tp=pickspec.tpick;
                xtext=xp(round(length(xp)/2));
                ttext=tp(round(length(xp)/2));
                pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                    'visible',showlabels,'horizontalalignment','center');
                
                AMP_PICKS{nevents}=pickspec;
                
            end
        end
        stringinfo='MB3: repick current event, or MB1: Pick new trajectory -> MB3: done';
    end
case 5 %picking amplitudes new buffer
    if(isempty(arg))
        if(~isempty(AMP_PICKS))
            PI_delete_amp_picks(gcf);
        end
        ipick;
        stringinfo='New AMP_PICK buffer MB1: Pick trajectory for autopicking. MB3: signals done.';
    elseif(strcmp(arg,'fromipick'))
        pickspec=get(gca,'userdata');
        showlabels=get(hamp_picks_lbl,'checked');
        if(isempty(AMP_PICKS))
            %text label
            xp=pickspec.xpick;
            tp=pickspec.tpick;
            xtext=xp(round(length(xp)/2));
            ttext=tp(round(length(xp)/2));
            pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                'visible',showlabels,'horizontalalignment','center');
            AMP_PICKS{1}=pickspec;
        else
            %check to see if this is a new event or replaces an old one
            newevent=1;
            nevents=length(AMP_PICKS);
            for k=1:nevents
                pickspec_old=AMP_PICKS{k};
                if(strcmp(pickspec.eventname,pickspec_old.eventname))
                    if(pickspec_old.figurehandle==gcf)
                        newevent=0;
                        %text label
                        delete(pickspec_old.texthandle);%delete old label
                        xp=pickspec.xpick;
                        tp=pickspec.tpick;
                        xtext=xp(round(length(xp)/2));
                        ttext=tp(round(length(xp)/2));
                        pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                            'visible',showlabels,'horizontalalignment','center');
                        
                        AMP_PICKS{k}=pickspec;
                        break;
                    end
                end
            end
            if(newevent)
                nevents=nevents+1;
                %text label
                xp=pickspec.xpick;
                tp=pickspec.tpick;
                xtext=xp(round(length(xp)/2));
                ttext=tp(round(length(xp)/2));
                pickspec.texthandle=text(xtext,ttext,pickspec.eventname,...
                    'visible',showlabels,'horizontalalignment','center');
                
                AMP_PICKS{nevents}=pickspec;
            end
        end
        stringinfo='MB3: repick current event, or MB1: Pick new trajectory -> MB3: done';
    end
end
set(hmsg,'string',stringinfo,'backgroundcolor',col);