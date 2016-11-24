function PI_ChangeProperties(arg1)

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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP
global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKCOLOR XAXISTOP

if(nargin<1)
    val=1;
else
    val=arg1;
end
switch val
case 1
    q1='Type of Color Map';
    a1='seisclrs|autumn|bone|colorcube|cool|copper|flag|gray|hot|hsv|jet|lines|pink|prism|spring|summer|white|winter';
    q2='Brighten';
    a2='Auto Brighten|Clipped Linear';
    q3='Number of Colors (if seisclrs)';
    a3='64';
    q4='Grey Percentage (if seisclrs)';
    a4='50';
    q5='Picks Color';
    a5='Red|Green|Blue';
    q6='X-Axis Location';
    a6='Bottom|Top';
    qs={q1 q2 q3 q4 q5 q6};
    as={a1 a2 a3 a4 a5 a6};
    a=askthingsle('questions',qs, 'answers', as, 'name','Plot Image Properties');
    set(gcf,'windowstyle','modal');
    if isempty(a)
        return;
    end
    COLOR_MAP=deblank(a{1});
    nobrighten=deblank(a{2});
    switch nobrighten
    case 'Auto Brighten'
        NOBRIGHTEN=0;
    case 'Clipped Linear'
        NOBRIGHTEN=1;
    end
    number_of_colors=sort([4 round(deblank(a{3})) 150]);
    NUMBER_OF_COLORS=number_of_colors(2);
    gray_pct=sort([10 round(deblank(a{4})) 100]);
    GRAY_PCT=gray_pct(2);
    allpifig=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
%     allaxis=get(allpifig,'currentaxes');
    pickcolor=strunpad(a{5});
    PICKCOLOR=lower(pickcolor(1,:));
    if(strcmp(COLOR_MAP,'seisclrs'))
        clrmap=seisclrs(NUMBER_OF_COLORS,GRAY_PCT);
        set(allpifig,'colormap',clrmap);
    else
       hgcf=gcf;
       for ii=1:length(allpifig)
           set(0,'currentfigure',allpifig(ii));
           % seisclrs|autumn|bone|colorcube|cool
           % copper|flag|gray|hot|hsv|jet|lines
           % pink|prism|spring|summer|white|winter
           %#function autumn
           %#function bone
           %#function colorcube
           %#function cool
           %#function copper
           %#function flag
           %#function gray
           %#function hot
           %#function hsv
           %#function jet 
           %#function lines
           %#function pink
           %#function prism
           %#function spring
           %#function summer
           %#function white
           %#function winter
           colormap(COLOR_MAP);
           set(findobj(gcf,'type','line','tag','PICKS'),'color',PICKCOLOR);
       end
       set(0,'currentfigure',hgcf); 
    end
    set(findobj(gcf,'type','line','tag','PICKS'),'color',PICKCOLOR);
    xaxistop=lower(strunpad(a{6}));
    switch xaxistop
    case 'top'
        XAXISTOP=1;
    case 'bottom'
        XAXISTOP=0;
    end
    hgcf=gcf;
    for ii=1:length(allpifig)
        set(0,'currentfigure',allpifig(ii));
        set(gca,'xaxislocation',xaxistop);
    end
    set(0,'currentfigure',hgcf); 
case 3
end
h=get(gcf,'userdata');
hscale=h{6};
SCALE_OPT=get(hscale,'value');
hclip=h{7};
% hslider=h{12};
val=get(hclip,'value');
% dat=get(hclip,'userdata');
clips=[30 25 20 15 10 9 8 7 6 5 4 3 2 1 .5 .25 .1 .05 .01 .005 .001];
CLIP=clips(val);
save('plotimageproperties.mat','XAXISTOP','PICKCOLOR','NOBRIGHTEN','COLOR_MAP',...
    'GRAY_PCT','NUMBER_OF_COLORS','SCALE_OPT');