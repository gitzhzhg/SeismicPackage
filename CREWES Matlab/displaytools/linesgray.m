function handles=linesgray(varargin)
% LINESGRAY ... a utility to make gray-level line plots for publications
%
% handles=linesgray(line1,line2,... )
%
% Publishing color figures for simple line plots can be expensive and is
% often not necessary. Instead of using color, the lines can be plotted
% with different shades of gray. In addition the line width and line style
% can be varied. LINESGRAY is a utility that makes this easier. Each input
% argument is a cell array with up to 7 entries specifying a line to be
% plotted. There can be any number of lines. The identity of each entry in
% a line specification is determined by it's order. The order of entries in
% each line spec (cell array) is (note the syntax): {x-coordinates,
% y-coordinates, linestyle, linewidth, gray-level, markertype, markersize}
% So there are at most 7 entries and there must be at least the first two.
% The lines are drawn with the fundamental drawing function "line" so they
% are added to the current figure rather than clearing it as "plot" does.
%
% x-coordinates ... vector of x coordinates for the line
% y-coordinates ... vector of y coordinates for the line (must be the same size as x-coordinates)
% linestyle ... (char) must be one of '-',':','-.','--', or 'none'
% **** default {'-', '-', '-', ':', ':'} cycle through in order and wrap ****
% linewidth ... (numeric) normal line with is 0.5. Should be between 0.25 and 4
% **** default [ 1.5, 1, .75, .5, .25] cycle through in order and wrap ****
% graylevel ... (numeric) should be between 0 and 1. 0 is solid black and 1 is solid white
% **** default [.7 .6 .5 .4 .3] cycle through in order and wrap ****
% markertype ... (char) must be one of '.','o','x','+','*','s','d','v','^','<','>','p','h' or 'none'
% **** default 'none' ****
% markersize (numeric) default marker size is 6. Don't vary by more than a factor of 3.
% **** default 6 ****
%
%
% handles =  ordinary array of handles, one per line
%
% In the examples below, note the use of prepfig which will enlarge the
% figure and make the line widths 4 times what is specified in linesgray.
% This makes the variation in width more obvious.
%
% Example 1: Plot three lines with default entries 
% x=0:.001:1; y1=sin(2*pi*x);y2=cos(4*pi*x);y3=sin(6*pi*x);
% figure
% linesgray({x,y1},{x,y2},{x,y3})
% prepfig
% legend('line 1','line 2','line 3')
%
% Example 2: As above except that 
% the lines will have progressively decreasing widths: 2, 1, .5
% and progressivly darker gray levels: .7 .4 .1
% x=0:.001:1; y1=sin(2*pi*x);y2=cos(4*pi*x);y3=sin(6*pi*x);
% figure
% linesgray({x,y1,'-',2,.7},{x,y2,'-',1,.4},{x,y3,'-',.5,.1})
% prepfig
% legend('line 1','line 2','line 3')
%
% Example 3: As above except that line 2 will be dotted and line 3 dashed.
% The lines will have progressively decreasing widths: 2, 1, .5
% and progressivly darker gray levels: .7 .4 .1
% x=0:.001:1; y1=sin(2*pi*x);y2=cos(4*pi*x);y3=sin(6*pi*x);
% figure
% linesgray({x,y1,'-',2,.7},{x,y2,':',1,.4},{x,y3,'--',.5,.1})
% prepfig
% legend('line 1','line 2','line 3')
%
% G.F. Margrave, CREWES, 2015
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

nlines=length(varargin);

lss={'-', '-', '-', ':', ':'};%linestyles
%lws=[.25, .5, 1.0, 2.0, 3.0];%linewidths
lws=[ 1.5, 1, .75, .5, .25];%linewidths
gls=[.7 .6 .5 .4 .3];%graylevels
mks={'none', '.', '*', 'diamond', 'square'};%markertypes
mss=[4, 6, 8, 10, 12];%marker sizes
ndef=5;%number of default values available


handles=zeros(1,nlines);
for k=1:nlines
    thisline=varargin{k};
    nparms=length(thisline);
    if(nparms<2); error(['Line ' int2str(k) ' fewer than two specifications']); end
    if(nparms>7); error(['Line ' int2str(k) ' more than seven entries']); end
    h=line(thisline{1},thisline{2});
    kdefault=rem(k,ndef);
    if(kdefault==0); kdefault=5; end
    if(nparms>2)
        if(ischar(thisline{3}))
            if(isvalidlinestyle(thisline{3}))
                set(h,'linestyle',thisline{3});
            else
                error(['Line ' int2str(k) ' has invalid linestyle'])
            end
        else
            error(['Line ' int2str(k) ' has non-char linestyle'])
        end
    else
        set(h,'linestyle',lss{kdefault});
    end
    if(nparms>3)
        if(isnumeric(thisline{4}))
            set(h,'linewidth',thisline{4});
        else
            error(['Line ' int2str(k) ' has non-numeric linewidth'])
        end
    else
        set(h,'linewidth',lws(kdefault));
    end
    if(nparms>4)
        if(isnumeric(thisline{5}))
            if(between(0,1,thisline{5},2))
                set(h,'color',thisline{5}*[1 1 1]);
            else
                error(['Line ' int2str(k) ' has invalid gray level'])
            end
        else
           error(['Line ' int2str(k) ' has non-nummeric gray level']) 
        end
    else
        set(h,'color',gls(kdefault)*[1 1 1]);
    end
    if(nparms>5)
        if(ischar(thisline{6}))
            if(isvalidmarker(thisline{6}))
                set(h,'marker',thisline{6});
            else
                error(['Line ' int2str(k) ' has invalid marker'])
            end
        else
            error(['Line ' int2str(k) ' has non-char marker'])
        end
    else
        set(h,'marker',mks{1});
    end
    if(nparms>6)
        if(isnumeric(thisline{7}))
            set(h,'markersize',thisline{7});
        else
           error(['Line ' int2str(k) ' has non-numeric marker size']) 
        end
    else
        set(h,'markersize',mss(2));
    end
    handles(k)=h;
end

function flag=isvalidlinestyle(ls)
validstyles={'-',':','-.','--','none'};
flag=0;
for k=1:length(validstyles)
    if(strcmp(ls,validstyles{k}))
        flag=1;
    end
end
function flag=isvalidmarker(mk)
validmarkers={'.','o','x','+','*','s','d','v','^','<','>','p','h','none'};
flag=0;
for k=1:length(validmarkers)
    if(strcmp(mk,validmarkers{k}))
        flag=1;
    end
end