function bigfont(hdaddy,fontsize,fontweight,tickflag,fontcolor)
%
% bigfont(hdaddy,fontsize,fontweight,tickflag,fontcolor)
%
% NOTE: If your figure has a legend, turn it off before using this function
% and then recreate it afterwards.
%
% hdaddy ... handle of a figure or axes object or other parent object
%		which contains text objects
%   ******** default = gcf ******
% fontsize ... desired fontsize expressed as a ratio of output
%		to input size
%   ******** default 2 ********
% fontwt ... 1 = normal
%			 2 = bold
%   *********  default = 2 *********
% tickflag ... 1 ... change tick labels
%			   0 ... dont change tick labels
%   ********* default = 1 *********
% fontcolor  .... 'k','r','b',ect.
%   ********* dont't change font colors *******
%
% You can customize the default behavior of bigfont by defining some
%  globals: BIGFONT_FS and BIGFONT_FW . Set these to have your desired
%  default values of fontsize and fontweight. A good place to define
%  these is in your startup.m file.
% Gary Margrave, CREWES
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

global BIGFONT_FW BIGFONT_FS

if nargin<5
    fontcolor='';
end
if(nargin<4)
    tickflag=1;
end
if(nargin<3)
    if(isempty(BIGFONT_FW))
        fontweight=2;
    else
        fontweight=BIGFONT_FW;
    end
end
if(nargin<2)
    if(isempty(BIGFONT_FS))
        fontsize=2;
    else
        fontsize=BIGFONT_FS;
    end
end
if(nargin<1)
    hdaddy=gcf;
end

haxes     = findobj(hdaddy,'type','axes');

if verLessThan('matlab','8.4') % 8.4 == R2014b
    htext     = findall(hdaddy,'type','text');
else
    htext     = findobj(hdaddy,'type','text');
end

hlegend   = findobj(hdaddy,'tag','legend');

if verLessThan('matlab','8.4') % 8.4 == R2014b
    %remove double references to hlegend text
    hlegendkids = findall(hlegend,'type','text');
    for kk = 1:length(hlegendkids)
        htext(htext == hlegendkids(kk)) = [];
    end
end

hcolorbar   = findobj(hdaddy,'type','colorbar');  %empty if less than R2014b

hcontour    = findobj(hdaddy,'type','contour');   %empty if less than R2014b

htextarrow  = findall(hdaddy,'type','textarrow'); %empty if less than R2014b

htextbox    = findall(hdaddy,'type','textbox');   %empty if less than R2014b

fontweight = getFontWeightAsString(fontweight);

if(~isempty(fontcolor))
    if tickflag
        setFontProperties(haxes,'axes',get(haxes,'fontsize'),fontsize,fontweight,fontcolor);
        setFontProperties(hlegend,'legend',get(hlegend,'fontsize'),fontsize,fontweight,fontcolor);
        setFontProperties(hcolorbar,'colorbar',get(hcolorbar,'fontsize'),fontsize,fontweight,fontcolor);
    end
    
    setFontProperties(htext,'text',get(htext,'fontsize'),fontsize,fontweight,fontcolor);
    setFontProperties(htextarrow,'textarrow',get(htextarrow,'fontsize'),fontsize,fontweight,fontcolor);
    setFontProperties(htextbox,'textbox',get(htextbox,'fontsize'),fontsize,fontweight,fontcolor);
else
    if tickflag
        setFontPropertiesNoColor(haxes,'axes',get(haxes,'fontsize'),fontsize,fontweight);
        setFontPropertiesNoColor(hlegend,'legend',get(hlegend,'fontsize'),fontsize,fontweight);
        setFontPropertiesNoColor(hcolorbar,'colorbar',get(hcolorbar,'fontsize'),fontsize,fontweight);
    end
    
    setFontPropertiesNoColor(htext,'text',get(htext,'fontsize'),fontsize,fontweight);
    setFontPropertiesNoColor(htextarrow,'textarrow',get(htextarrow,'fontsize'),fontsize,fontweight);
    setFontPropertiesNoColor(htextbox,'textbox',get(htextbox,'fontsize'),fontsize,fontweight);
end
%note that the fontsize passed the next function is the same as for the axes
% setFontProperties(hcontour,'contour',get(haxes,'fontsize'),fontsize,fontweight,fontcolor);

end %end function bigfont

%%
function s = getFontWeightAsString(fontweight)
%function s = getfontweight(fontweight)
switch fontweight %convert numeric fontweight to a string
    case 2
        s = 'bold';
    otherwise
        s = 'normal';
end

end %end function getfontweight

%%
function setFontProperties(h,htype,fontsize,fsize,fontweight,fontcolor)
%function setFontProperties

if ~iscell(fontsize)
    fontsize = {fontsize};
end

for kk = 1:length(h) %bigfont the annotation and axes labels and title
    
    switch(htype)
        case 'axes'
            %         disp('axes')
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight,...
                'Xcolor',fontcolor,...
                'Ycolor',fontcolor,...
                'Zcolor',fontcolor...
                );
            
            switch get(h(kk),'tag')
                case 'legend'
                    hlegendkids = findall(h(kk),'type','text');
                    for jj = 1:length(hlegendkids)
                        set(hlegendkids(jj),'color',fontcolor);
                    end
                case 'otherwise'
                    % do something? colorbar?
            end
            
        case 'legend' %R2014b only
            %         disp('legend')
            set(h(kk),...
                'edgecolor',fontcolor,...
                'textcolor',fontcolor...
                );
        case 'colorbar' %R2014b only
            %         disp('colorbar')
            set(h(kk),...
                'color',fontcolor...
                );
        case 'textbox'
%              disp('textbox')
%             won't ever happen for matlab versions less that R2014b
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight,...
                'edgecolor',fontcolor,...
                'color',fontcolor...
                );
        case 'textarrow'
%             disp('textarrow')
            %won't ever happen for matlab versions less that R2014b
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight,...
                'color',fontcolor...
                );
        case 'contour'
            %won't ever happen for matlab versions less that R2014b
%             disp('contour')
            
            %         %this is super clunky, but (R2014b) won't let us get handles for text
            %         %objects belonging to 'managed labels,' whatever that means. Note
            %         %that fontsize is a cell array of fontsizes for all axes. We'll
            %         %reference the contour label size to this for now.
            %         fs = min(cell2mat(fontsize)); %minimum fontsize of all axes
            %         hprops = get(h(kk))          %get publicly available contour properties
            %         delete(h(kk));                %and delete the contour object
            %
            %         %recreate the contour object
            %         [c,h] = contour(...
            %             hprops.Parent,...
            %             hprops.XData,...
            %             hprops.YData,...
            %             hprops.ZData,...
            %             hprops.LevelList...
            %             );
            %
            %         %recreate contour labels
            %         clabel(c,...
            %             'fontsize',fs*fsize,...
            %             'fontweight',fontweight,...
            %             'color',fontcolor...
            %             );
        otherwise
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight,...
                'color',fontcolor);
            
    end %end switch
end %end for

end %end function setFontProperties

%%
function setFontPropertiesNoColor(h,htype,fontsize,fsize,fontweight)
%function setFontProperties

if ~iscell(fontsize)
    fontsize = {fontsize};
end

for kk = 1:length(h) %bigfont the annotation and axes labels and title
    
    switch(htype)
        case 'axes'
            %         disp('axes')
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight);
            
            switch get(h(kk),'tag')
                case 'legend'
%                     hlegendkids = findall(h(kk),'type','text');
%                     for jj = 1:length(hlegendkids)
%                         set(hlegendkids(jj),'color',fontcolor);
%                     end
                case 'otherwise'
                    % do something? colorbar?
            end
            
        case 'legend' %R2014b only
            %         disp('legend')
%             set(h(kk),...
%                 'edgecolor',fontcolor,...
%                 'textcolor',fontcolor...
%                 );
        case 'colorbar' %R2014b only
            %         disp('colorbar')
%             set(h(kk),...
%                 'color',fontcolor...
%                 );
        case 'textbox'
%              disp('textbox')
%             won't ever happen for matlab versions less that R2014b
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight);
        case 'textarrow'
%             disp('textarrow')
            %won't ever happen for matlab versions less that R2014b
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight);
        case 'contour'
            %won't ever happen for matlab versions less that R2014b
%             disp('contour')
            
            %         %this is super clunky, but (R2014b) won't let us get handles for text
            %         %objects belonging to 'managed labels,' whatever that means. Note
            %         %that fontsize is a cell array of fontsizes for all axes. We'll
            %         %reference the contour label size to this for now.
            %         fs = min(cell2mat(fontsize)); %minimum fontsize of all axes
            %         hprops = get(h(kk))          %get publicly available contour properties
            %         delete(h(kk));                %and delete the contour object
            %
            %         %recreate the contour object
            %         [c,h] = contour(...
            %             hprops.Parent,...
            %             hprops.XData,...
            %             hprops.YData,...
            %             hprops.ZData,...
            %             hprops.LevelList...
            %             );
            %
            %         %recreate contour labels
            %         clabel(c,...
            %             'fontsize',fs*fsize,...
            %             'fontweight',fontweight,...
            %             'color',fontcolor...
            %             );
        otherwise
            set(h(kk),...
                'fontsize',fontsize{kk}*fsize,...
                'fontweight',fontweight);
            
    end %end switch
end %end for

end