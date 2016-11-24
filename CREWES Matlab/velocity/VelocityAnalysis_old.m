function VelocityAnalysis(seis, t, x, vmin, vmax, vstep, refv, refz, dosemb)

% VelocityAnalysis(seis, t, os, vmin, vmax, vstep, refv, refz, type)
%
% Begins velocity analysis for a shot gather. Find the results with the
% function GetVelocities(). To use, simply pick velocities on the lefthand
% semblance/panel plot. The newly nmo-corrected gather is shown in the second
% panel, the picked RMS velocity in the third, and interval velocities in
% depth are shown in the fourth, along with the input reference velocity
% for comparison. 
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
% os    ... horizontal offset coordinate of seis
% vmin  ... minimum velocity to scan
% vmax  ... maximum velocity to scan
% vstep ... velocity scan increment
% refv  ... reference velocity for comparison, from well log or a nearby
%           velocity estimate, for example.
% refz  ... depth coordinate for refv
% type  ... 0 for const-v panels, 1 for semblance plot (default)
%
% See also: GetVelocities
%
% Chad Hogan, 2008
%
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

% $Id: VelocityAnalysis.m,v 1.1 2009/05/25 21:00:56 cmhogan Exp $



%% Set up parameters

if (nargin < 9)
    dosemb = 1;
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

%% vref sanity checking

if (refz(1) ~= 0)
    disp('Reference velocity must be extrapolated to surface.');
    disp('What surface velocity would you like to set?');
    disp('Hint: 1500 is suitable for marine, 1000 for land');
    sfcv = input('Surface velocity (m/s): ');
    
    refz = [0 refz(:)']';
    refv = [sfcv refv(:)']';
    
end

% fill out our reference velocities. 

refv = pwlint(refz, refv, 0:max(refz));
refz = 0:max(refz);

reft = cumsum(2 ./ refv);

refvrms = vint2vrms(refv, reft);


%% generate semblances

semb = zeros(size(seis, 1), length(vels));

disp('Calculating trial nmo corrections...');

if(dosemb ~= 1)
    panels = zeros(size(seis, 1), size(seis, 2), length(vels));
end

for idx = 1:length(vels)
    thisv = vels(idx);
    nmod = nmor(seis, t, x, thisv, 1);
    nmod(find(isnan(nmod))) = 0;
    if (dosemb == 1)
        semb(:, idx) = sum(nmod, 2).^2 ./ sum(nmod.^2, 2);
    else
        panels(:,:,idx) = nmod;
    end
    if (mod(idx, 10) == 0)
        disp(['done ' num2str(round(idx / length(vels) * 100)) '%']);
    end
end

if (dosemb ~= 1)
    % we want each shot gather to be about 20 traces in this plot
    approxpanwid   = 20;
    panstp   = round(size(panels, 2) / approxpanwid);
    panwid   = length(1:panstp:size(panels, 2));
    
    squished = zeros(size(panels, 1), panwid);
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
imagesc(vels, t, semb);    
ud.guideline = line(refvrms, reft, 'linewidth', 1, 'linestyle', ':', 'color', [0.2 1 0.2]);    
xlabel('V_{stack}');
grid on;
set(gca, 'xcolor', [0.6 0 0.6]);
set(gca, 'ycolor', [0.6 0 0.6]);

pickax  = gca;

ud = get(pickax, 'userdata');

ud.mypicks = [];
ud.pointx = [];
ud.pointy = [];
ud.refv = refv;
ud.refz = refz;
ud.refvrms = refvrms;
ud.reft = reft;
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

subplot(1, 5, 3);
plotseis(repmat(sum(seis, 2), 1, 5), t);
ud.stackax = gca;
set(gca, 'XTickLabel', '');
set(gca, 'YTickLabel', '');

subplot(1, 5, 4);
plot(refvrms, reft, 'r');
flipy;
ud.velax = gca;
xlabel('V_{rms} m/s');
set(gca, 'YTickLabel', '');

subplot(1, 5, 5);
plot(refv, refz, 'r');
flipy;
xlabel('m/s');
ylabel('depth');
ud.vzax = gca;

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

colormap hot;

set(pickax, 'userdata', ud);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clicky stuff
%% Window button down function
function wbd(h, evd)

% see if we're inside the range of an existing point. If so, delete that
% point and recalculate stuff.
pickax = gca;
ud = get(pickax, 'userdata');





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window button up function
function wbu(h, evd)
pickax = gca;

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
        plotseis(repmat(sum(ud.seis, 2), 1, 5), ud.t);
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
    
    % this actual stores the click location
    ud.pointx(end+1) = xcoord;
    ud.pointy(end+1) = ycoord;
    
    %disp(ud.pointx);
    %disp(ud.pointy);
    
    % mypicks is the handle to the line drawing on the seismic. If there is
    % an existing line here, we'll wipe it out so we can redraw it later
    axes(pickax);
    if(~isempty(ud.mypicks))
        delete(ud.mypicks);
        for boxidx = 1:length(ud.boxes)
            delete(ud.boxes(boxidx));
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
    if(length(myx) == 1)
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
                ud.boxes = ud.boxes(find((1:length(ud.boxes)) ~= inbox));
            end
            
            
            % first we drop the newest click
            ud.pointx = ud.pointx(1:(end-1));
            ud.pointy = ud.pointy(1:(end-1));
            
            % now drop the one we were in
            
            ud.pointx = ud.pointx(find((1:length(ud.pointx)) ~= inbox));
            ud.pointy = ud.pointy(find((1:length(ud.pointy)) ~= inbox));
            
            % and remake our line.
            [myy, order] = sort(ud.pointy);
            myx = ud.pointx(order);
            delete(ud.mypicks);
            axes(pickax);
            ud.mypicks = line(myx, myy, 'linewidth', 1, 'linestyle', ':', 'color', [1 1 1]);
            set(pickax, 'UserData', ud); % save that last bit
        end
 
        [vrms, vint, vave, zint] = makevels(myx, myy, ud.t);
        
        tempv = vrms;
                
        nmod = nmor(ud.seis, ud.t, ud.x, tempv, 1);
        nmod(find(isnan(nmod))) = 0;
        axes(ud.seisax);
        imagesc(ud.x, ud.t, nmod);
        cax = caxis;
        caxis([cax(1)/4 cax(2)/4]); drawnow;
        set(gca, 'XTickLabel', '');
        set(gca, 'YTickLabel', '');

        ud.vrmst = tempv;
        ud.vintt = vint;
        ud.vavet = vave;
        stack = sum(nmod, 2);
        axes(ud.stackax);
        cla;
        plotseis(repmat(stack, 1, 5), ud.t);
        set(gca, 'XTickLabel', '');
        set(gca, 'YTickLabel', '');

        axes(ud.velax);
        plot(tempv, ud.t, 'b', ud.refvrms, ud.reft, 'r', 'linewidth', 4);
        set(gca, 'YTickLabel', '');
        xlabel('V_{stack} m/s');
        flipy;
        
        axes(ud.vzax);
        plot(ud.refv, ud.refz, 'r', vint, zint, 'b', 'linewidth', 3);
        xlabel('m/s');
        ylabel('depth');
        set(ud.vzax, 'yaxislocation', 'right');
        flipy;
        ud.zint = zint;
        axes(pickax);
        
    end
    
end
axes(pickax);
set(pickax, 'userdata', ud);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utility functions

function boxes = drawboxes(pickax, pointx, pointy)

ud = get(pickax, 'UserData');

xlims = get(pickax, 'xlim');
ylims = get(pickax, 'ylim');

boxwid = (xlims(2) - xlims(1)) / 20;
boxhgt = (ylims(2) - ylims(1)) / 25;

for boxidx = 1:length(pointx)
    boxes(boxidx) = rectangle('position', ...
        [pointx(boxidx) - boxwid/2 pointy(boxidx) - boxhgt/2 ...
        boxwid boxhgt], 'edgecolor', [1 1 1]);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [vrms, vint, vave, zint] = makevels(myx, myy, t)

t = t(:);

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
    