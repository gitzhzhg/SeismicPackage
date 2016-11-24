function VelocityAnalysis(seis, t, x, vmin, vmax, vstep, refv, refz, type, smoothers)

% VelocityAnalysis(seis, t, x, vmin, vmax, vstep, refv, refz, type)
%
% Begins velocity analysis for a CMP gather. Find the results with the
% function GetVelocities(). To use, simply pick velocities on the lefthand
% stack_power/semblance/panel plot. The newly nmo-corrected gather is shown
% in the second panel, the picked RMS velocity in the third, and interval
% velocities in depth are shown in the fourth, along with the input
% reference velocity for comparison.
%
% Pick velocities by clicking on the semblance. To delete a pick, click
% again within the box. The clear all picks and start picking over,
% right-click on the semblance plot. 
%
% You probably want to use a gained shot record for this. Maybe not,
% though, it's up to you.
%
% seis  ... matrix of shot record data
% t     ... time coordinate of seis
% x    ... horizontal offset coordinate of seis
% vmin  ... minimum velocity to scan
% vmax  ... maximum velocity to scan
% vstep ... velocity scan increment
% refv  ... reference velocity for comparison, from well log or a nearby
%           velocity estimate, for example.
% refz  ... depth coordinate for refv
% type  ... 0 for const-v panels, 1 for semblance plot, 2 for stack power
%           (default), 3 for abs(stack)
% smoothers ... length 2 vector specifying smoother sizes as percentages of
%           the corresponding dimension. smoothers(1) is the temporal
%           smoother and smoothers(2) is the velocity dimension smoohter.
%           A boxcar smoother of these dimensions is convolved with the
%           semblance/power/amplitude panel.
% ************ default smoothers = [5 5] *****************
%
% See also: GetVelocities
%
% Chad Hogan 2008, Gary Margrave 2014
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

%% Set up parameters

if(nargin<10)
    smoothers=[5 5];
end

if (nargin < 9)
    type = 2;
end

if (nargin < 8)
    refv = 0;
    refz = 0;
end

if (nargin < 6)
    vstep = 50;
end
    
if (nargin < 5)
    vmax = 5000;
end

if (nargin < 4)
    vmin = 1500;
end

vels = vmin:vstep:vmax;

%% generate semblances


semb = zeros(size(seis, 1), length(vels));

disp('Calculating trial nmo corrections...');

if(type ~= 1)
    panels = zeros(size(seis, 1), size(seis, 2), length(vels));
end


for idx = 1:length(vels)
    thisv = vels(idx);
    nmod = nmor(seis, t, x, thisv, 1);
%     nmod(find(isnan(nmod))) = 0;

    
    if (type == 1)
        semb(:, idx) = sum(nmod, 2).^2 ./ sum(nmod.^2, 2);
    elseif (type > 1)
        %stack
        tmp=sum(nmod,2);
        %determine fold
        ind=nmod~=0;%will be hard zero where muted
        fold=sum(ind,2);
        %fold normalization
        ilive=fold~=0;
        tmp(ilive)=tmp(ilive)./fold(ilive);
        %power
        if(type==2)
            semb(:, idx) = tmp.^2;
        elseif(type==3)
            semb(:,idx)= abs(tmp);
        else
            error('invalid type')
        end
    else
        panels(:,:,idx) = nmod;
    end
    if (mod(idx, 10) == 0)
        disp(['done ' num2str(round(idx / length(vels) * 100)) '%']);
    end
end

if (type == 0)
    % we want each shot gather to be about 20 traces in this plot
    approxpanwid   = 20;
    panstp   = round(size(panels, 2) / approxpanwid);
    panwid   = length(1:panstp:size(panels, 2));
    
    allpans  = zeros(size(panels, 1), size(panels, 3)*panwid);
    
    for idx = 1:size(panels, 3)
        squished = panels(:, 1:panstp:size(panels,2), idx);
        allpans(:, ((idx-1)*panwid+1):(idx*panwid)) = squished;
    end
    semb = allpans;
    vels = vmin:((vmax-vmin)/(size(allpans, 2)-1)):vmax;
end

%% make plots

%smax = max(semb(:));
%smin = min(semb(:));
%semb(find(semb(:) < (smax-smin)/5)) = 0;

figure;
subplot(1, 4, 1);
ntsmo=max([round(smoothers(1)*length(t)/100),1]);
nvsmo=max([round(smoothers(2)*length(vels)/100),1]);
imagesc(vels, t, conv2(semb,ones(ntsmo,nvsmo),'same'));
xlabel('V_{stk} (m/s)');
grid on;
if(type==1)
    title('Semblance')
elseif(type==2)
    title('Stack^2')
elseif(type==3)
    title('Abs(stack)')
else
    title('Constant v panels')
end
colormap(jet);
pickax  = gca;
set(pickax,'tag','pickax');

ud = get(pickax, 'userdata');

ud.mypicks = [];
ud.pointx = [];
ud.pointy = [];
ud.refv = refv;
ud.refz = refz;
ud.vrmst = NaN;
ud.intt = NaN;
ud.vave = NaN;

plotfig = gcf;

set(plotfig, 'WindowButtonDownFcn', {@wbd});
set(plotfig, 'WindowButtonUpFcn', {@wbu});

subplot(1, 5, 2);
imagesc(x, t, seis);
cax = caxis;
caxis([cax(1)/4 cax(2)/4]); drawnow;
ud.seisax = gca;
set(gca, 'XTickLabel', '');
set(gca, 'YTickLabel', '');
title('Gather')
%colormap seisclrs;
subplot(1, 5, 3);
plotseis(repmat(sum(seis, 2), 1, 5), t);
ud.stackax = gca;
set(ud.stackax,'tag','stackax');
set(gca, 'XTickLabel', '');
set(gca, 'YTickLabel', '');
title('Stack')

subplot(1, 5, 4);
plot(zeros(size(t)));
xlim([min(vels) max(vels)]);
ylim([t(1) t(end)]);flipy
ud.velax = gca;
set(ud.velax,'tag','velax');
% xlabel('m/s');
set(gca, 'YTickLabel', '');
title('V_{stk} (m/s)')
vrange=vels(end)-vels(1);
vtick1=round((vels(1)+vrange/3)/vstep)*vstep;
vtick2=round((vels(1)+2*vrange/3)/vstep)*vstep;
set(gca,'xtick',[vtick1,vtick2])
set(gca,'xticklabel','')
ht1=text(vtick1,max(t),int2str(vtick1));
set(ht1,'rotation',-90)
ht2=text(vtick2,max(t),int2str(vtick2));
set(ht2,'rotation',-90)
set(gca,'xgrid','on')

subplot(1, 5, 5);
plot(refv, refz, 'r');
v1=floor(.5*vels(1)/vstep)*vstep;v2=ceil(1.2*vels(end)/vstep)*vstep;
vrange=v2-v1;
set(gca,'xlim',[v1 v2])
flipy;
% xlabel('m/s');
ylabel('depth');
ud.vzax = gca;
set(ud.vzax,'tag','vzax');
title('V_{int}')
yl=get(gca,'ylim');
vtick1=round((v1+vrange/3)/vstep)*vstep;
vtick2=round((v1+2*vrange/3)/vstep)*vstep;
set(gca,'xtick',[vtick1,vtick2])
set(gca,'xticklabel','')
ht1=text(vtick1,yl(2),int2str(vtick1));
set(ht1,'rotation',-90)
ht2=text(vtick2,yl(2),int2str(vtick2));
set(ht2,'rotation',-90)
set(gca,'xgrid','on')
bigfont(gca,1,2)
% set(gca,'xtick',[vtick1,vtick2])
% set(gca,'xticklabel','')
% ht1=text(vtick1,max(t),int2str(vtick1));
% set(ht1,'rotation',-90)
% ht2=text(vtick2,max(t),int2str(vtick2));
% set(ht2,'rotation',-90)

set(ud.vzax, 'yaxislocation', 'right');
set(ud.seisax, 'yaxislocation', 'right');
set(pickax, 'position', [0.1 0.1 0.4 0.8]);
set(ud.seisax, 'position', [0.5 0.1 0.1 0.8]);
set(ud.stackax, 'position', [0.6 0.1 0.1 0.8]);
set(ud.velax, 'position', [0.7 0.1 0.1 0.8]);
set(ud.vzax, 'position', [0.8 0.1 0.1 0.8]);

ud.seis = seis;
ud.x = x;
ud.t = t;

prepfig
bigfont(gcf,.75)

set(pickax, 'userdata', ud);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clicky stuff
%% Window button down function
function wbd(h, evd)

% see if we're inside the range of an existing point. If so, delete that
% point and recalculate stuff.
% pickax = gca;
% ud = get(pickax, 'userdata');





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window button up function
function wbu(h, evd)
pickax = gca;
test=get(pickax,'tag');
if(~strcmp(test,'pickax'))
    return;
end

ud = get(pickax, 'userdata');

if strcmp(get(h, 'SelectionType'), 'alt')
    if(~isempty(ud.mypicks))
        delete(ud.mypicks);
        for idx = 1:length(ud.boxes)
            delete(ud.boxes(idx));
        end
        ud.pointx = [];
        ud.pointy = [];
        ud.mypicks = [];
        ud.boxes = [];
        axes(ud.seisax);
        imagesc(ud.x, ud.t, ud.seis);
        cax = caxis;
        caxis([cax(1)/4 cax(2)/4]); drawnow;
        axes(ud.stackax);
        cla;
        ind=ud.seis~=0;
        fold=sum(ind,2);
        ind=fold~=0;
        stk=sum(ud.seis,2);
        stk(ind)=stk(ind)./fold(ind);
        plotseis(repmat(stk, 1, 5), ud.t);
        axes(ud.velax);
        cla;
        axes(ud.vzax);
        cla;
        axes(pickax);
    end
else
    % If we have a proper button click (up, whatever)
    pt = get(pickax, 'currentpoint');
    
    xcoord = pt(1,1);
    ycoord = pt(2,2);
    %ignore clicks outside the axes
    vlims=get(gca,'xlim');
    tlims=get(gca,'ylim');
    if(~between(vlims(1),vlims(2),xcoord) || ~between(tlims(1),tlims(2),ycoord));
        return;
    end
    
    % this actual stores the click location
    ud.pointx(end+1) = xcoord;
    ud.pointy(end+1) = ycoord;
    
    %disp(ud.pointx);
    %disp(ud.pointy);
    
    % mypicks is the handle to the line drawing on the seismic. If there is
    % an existing line here, we'll wipe it out so we can redraw it later
    axes(pickax);
    if(~isempty(ud.mypicks))
        if(ishandle(ud.mypicks))
            delete(ud.mypicks);
        end
        for boxidx = 1:length(ud.boxes)
            if(ishandle(ud.boxes(boxidx)))
                delete(ud.boxes(boxidx));
            end
        end
        ud.boxes   = [];
        ud.mypicks = [];
    end
    % now we sort the click points
    [myy, order] = sort(ud.pointy);
    myx = ud.pointx(order);
    
    % redraw the line with the new point added
    ud.mypicks = line(myx, myy, 'linewidth', 1, 'linestyle', ':', 'color', [1 1 1]);    

    % and draw little boxes around the actual click points
    ud.boxes = drawboxes(pickax, ud.pointx, ud.pointy);
    set(pickax, 'UserData', ud); % save that last bit
    
    axes(ud.seisax);
    
    % if there is only one click point, we'll just set the velocity and be
    % done with it. 
    if(length(myx) <= 1)
        %tempv = myx * ones(size(ud.t));
        % or maybe we do nothing..?  Let's see if this is acceptable.
    elseif (length(myx) > 1)
        
        % now we find out how far away from the clickpoints we are. That is, 
        % did the click occur inside a box? 
        inbox = findboxes(pickax);
        
        if(inbox > 0)
            
            % remove the offending boxes
            delete(ud.boxes(end));
            delete(ud.boxes(inbox));
            ud.boxes = ud.boxes(1:(end-1));
            if(length(ud.boxes) == 1)
                ud.boxes = [];
            else
                ud.boxes = ud.boxes((1:length(ud.boxes)) ~= inbox);
            end
            
            
            % first we drop the newest click
            ud.pointx = ud.pointx(1:(end-1));
            ud.pointy = ud.pointy(1:(end-1));
            
            % now drop the one we were in
            
            ud.pointx = ud.pointx((1:length(ud.pointx)) ~= inbox);
            ud.pointy = ud.pointy((1:length(ud.pointy)) ~= inbox);
            
            % and remake our line.
            [myy, order] = sort(ud.pointy);
            myx = ud.pointx(order);
            delete(ud.mypicks);
            axes(pickax);
            ud.mypicks = line(myx, myy, 'linewidth', 1, 'linestyle', ':', 'color', [1 1 1]);
            set(pickax, 'UserData', ud); % save that last bit
        end
        if(length(myx)>1)
            [vrms, vint, vave, zint] = makevels(myx, myy, ud.t);

            tempv = vrms;

            nmod = nmor(ud.seis, ud.t, ud.x, tempv, 1);
            nmod(isnan(nmod)) = 0;
            axes(ud.seisax);
            imagesc(ud.x, ud.t, nmod);
            cax = caxis;
            caxis([cax(1)/4 cax(2)/4]); drawnow;
            set(gca, 'XTickLabel', '');
            set(gca, 'YTickLabel', '');
            title('Gather')
            bigfont(gca,1,2);

            ud.vrmst = tempv;
            ud.vintt = vint;
            ud.vavet = vave;
            stack = sum(nmod, 2);
            ind=nmod~=0;
            fold=sum(ind,2);
            ind=fold~=0;

            stack(ind)=stack(ind)./fold(ind);
            axes(ud.stackax);
            cla;
            plotseis(repmat(stack, 1, 5), ud.t);
            set(gca, 'XTickLabel', '');
            set(gca, 'YTickLabel', '');

            axes(ud.velax);
            xl=get(gca,'xlim');
            xt=get(gca,'xtick');
            xtlabel=get(gca,'xticklabel');
            plot(tempv, ud.t, 'linewidth', 4);
            set(gca, 'YTickLabel', '');
            title('V_{stk} (m/s)')
            set(gca,'xlim',xl);
            set(gca,'xtick',xt);
            set(gca,'xticklabel',xtlabel);
            ht1=text(xt(1),max(ud.t),int2str(xt(1)));
            fs=get(ht1,'fontsize');
            set(ht1,'rotation',-90,'fontsize',1.5*fs)
            ht2=text(xt(2),max(ud.t),int2str(xt(2)));
            set(ht2,'rotation',-90,'fontsize',1.5*fs)
            set(gca,'xgrid','on')
            bigfont(gca,1,2);
            flipy;

            axes(ud.vzax);
            xl=get(gca,'xlim');
            xt=get(gca,'xtick');
            xtlabel=get(gca,'xticklabel');
            plot(ud.refv, ud.refz, 'r', vint, zint, 'b', 'linewidth', 3);
            ylabel('depth');
            set(ud.vzax, 'yaxislocation', 'right');
            flipy;
            ud.zint = zint;
            title('V_{int} (m/s)')
            set(gca,'xlim',xl);
            set(gca,'xtick',xt);
            set(gca,'xticklabel',xtlabel);
            yl=get(gca,'ylim');
            ht1=text(xt(1),yl(2),int2str(xt(1)));
            fs=get(ht1,'fontsize');
            set(ht1,'rotation',-90,'fontsize',1.5*fs)
            ht2=text(xt(2),yl(2),int2str(xt(2)));
            set(ht2,'rotation',-90,'fontsize',1.5*fs)
            bigfont(gca,1,2);
            set(gca,'xgrid','on')
            axes(pickax);
        end
    end
    
end
axes(pickax);
set(pickax, 'userdata', ud);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utility functions

function boxes = drawboxes(pickax, pointx, pointy)

% ud = get(pickax, 'UserData');

xlims = get(pickax, 'xlim');
ylims = get(pickax, 'ylim');

boxwid = (xlims(2) - xlims(1)) / 20;
boxhgt = (ylims(2) - ylims(1)) / 25;
boxes=zeros(1,length(pointx));
for boxidx = 1:length(pointx)
    boxes(boxidx) = rectangle('position', ...
        [pointx(boxidx) - boxwid/2 pointy(boxidx) - boxhgt/2 ...
        boxwid boxhgt], 'edgecolor', [1 1 1]);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [vrms, vint, vave, zint] = makevels(myx, myy, t)
if(isempty(myy))
    vrms=[];
    vint=[];
    vave=[];
    zint=[];
    return;
end
mintidx = near(t, min(myy)) + 1;
mintidx = mintidx(1);
maxtidx = near(t, max(myy)) - 1;
maxtidx = maxtidx(1);

tempv = zeros(size(t));

tempv(1:mintidx)   = myx(1);
tempv(maxtidx:end) = myx(end);
tempv(mintidx:maxtidx) = pwlint(myy, myx, t(mintidx:maxtidx));

vrms = tempv;
vint = vrms2vint(tempv, t, 0);
vave = vrms2vave(tempv, t);


zint = vave .* t /2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function inbox = findboxes(pickax)

% our newest trial click adds a box. We ignore the new one, or really,
% we're treating it as a probationary box! The real live boxes are one less
% than the length.
inbox = 0;
ud = get(pickax, 'UserData');

numboxes = length(ud.boxes)-1;
if (numboxes < 1)
    inbox = 0;
    return
end

newx = ud.pointx(end);
newy = ud.pointy(end);

% ok this gets a bit complicated. As the axis adds children, the newest
% kids get stuck on the FRONT of the list. I don't know why, but that's
% what happens. So our actual boxes are searched from newest to oldest,
% which means we have to reverse the order. 

kids = get(pickax, 'Children');
boxhs = kids(2:(numboxes+1));

for bidx = 1:numboxes
    pos = get(boxhs(bidx), 'Position');
    
    if ((newx > pos(1)) && (newx < pos(1) + pos(3)) && ...
            (newy > pos(2)) && (newy < pos(2) + pos(4)))
        % if we're here then click is inside a box
        inbox = numboxes + 1 - bidx;
        return
    end
end
    