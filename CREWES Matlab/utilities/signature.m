function  th=signature(name,logo,pos,fontsize,offset,ishm,color,ftname)
% SIGNATURE: put a signature on a figure window
%
% th=signature(name,logo,pos,fontsize,offset,ishm,color,ftname)
%
% SIGNATURE  Produces a "signature" with author's name and
%         creation time at the specified position of a figure.
%     SIGNATURE(NAME,LOGO,POS,FONTSIZE,OFFSET)
%         creates 2 text objects:
%         the first containing the NAME string and the creation
%         time, and puts it into a specified position POS of
%         the figure; the second - containing a string LOGO
%         which is put just below the first text or at the
%         position specified by a second row of POS.
%         The fontsizes of the texts can be also specified by
%         FONTSIZE argument (one or two numbers).
%         OFFSET specify the relative distance between two lines
%         of text.
%         All input arguments are optional, but must be input in
%         the given order. If some or all arguments are not
%         specified, the default values are entered.
%         Additional properties can be specified within the program:
%         ISHM - (1 or 0) if hours:minutes to be added to date;
%         COLOR -  color of the text (black by default);
%         FTNAME - fontname.
%    TH = SIGNATURE ... also returns handle(s) of the created
%         text object(s).
%
% NOTE: If using Matlab version 4.x, must set color to non-black  
%
% Kirill K. Pankratov,   kirill@plume.mit.edu
%  April 8, 1994;  April 27, 1994
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

% Totally altered by G.F. Margrave, Feb 12, 1996

% Defaults and setup ........................................
namedflt = 'MyPlot';   % Name default
logodflt = '';         % "Logo" default
posdflt = [.65 1.06];  % (Normalized) position default
ftszdflt = 10;         % Font size default
offsetdflt = .75;      % Space between the first and the second line

ishmdflt = 1;              % Is hours and minutes to be added to date
if(get(gcf,'color')<=[.5 .5 .5]) % color default 
	 colordflt = [1 1 1]; 
else
	 colordflt = [0 0 0];
end
ftnamedflt = 'helvetica';  % Fontname

% Handle input ..............................................
if nargin<8, ftname = ftnamedflt; end
if nargin<7, color = colordflt; end
if nargin<6, ishm = ishmdflt; end
if nargin<5, offset = offsetdflt; end
if nargin<4, fontsize = ftszdflt; end
if nargin<3, pos = posdflt; end
if nargin<2, logo = logodflt; end
if nargin<1, name = namedflt; end
if length(fontsize)<2, fontsize = fontsize([1 1]'); end

% Create a time string ............
time = clock;
cstr = num2str(time(5));
if length(cstr)==1, cstr = ['0' cstr]; end  % Add '0' for minutes
d = date;
if ishm, d = [d ', ' num2str(time(4)) ':' cstr]; end
string = [name '  ' d];

 % Make an invisible axes ..........
hax=get(gcf,'currentaxes');
%ah = axes('units','normal','pos',[0 0 1 1]);
%col = get(gcf,'color');
%set(ah,'xlim',[0 1],'ylim',[0 1])
%set(ah,'xcolor',col,'ycolor',col)
%set(ah,'xtick',[], 'ytick',[])
xlims=get(hax,'xlim');
ylims=get(hax,'ylim');

pos(1,1)=xlims(1)+pos(1,1)*(xlims(2)-xlims(1));
pos(1,2)=ylims(1)+pos(1,2)*(ylims(2)-ylims(1));


th = text(pos(1,1),pos(1,2),string);  % The first text
set(th,'fontsize',fontsize(1),'fontname',ftname,'color',color,...
	'units','normal')
drawnow
ext = get(th,'extent');
if size(pos,1)<2
  pos(2) = pos(2)-offset*ext(4);
else, pos = pos(2,:);
end
if(~isempty(logo))                          % If "logo" is added
 th(2) = text(pos(1),pos(2),logo);
 set(th(2),'fontsize',fontsize(2),'fontname',ftname,'color',color)
end

th = th(:);    % Output handles