function refrac(action)
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

% Function establishing the menu of the main figure
if( nargin < 1 )
        action = 'init';
end
if( action == 'init' )
   % Make a new figure window
   fig = figcent(.5,.5);
   set(fig, 'name', 'Plus-Minus time analysis','menubar','none');
   more off;
filem = uimenu('label','File');
   loadPMm = uimenu(filem, 'label','Load from ProMAX');
      set(loadPMm, 'callback', 'loadPMcb');
   loadsyntm=uimenu(filem,'label','Load from synthetic data or other format');
      set(loadsyntm, 'callback', 'loadsyntcb');
   saveprojm = uimenu(filem, 'label', 'Save project');
      set(saveprojm,'callback','refracproject(''save'')');
   loadprojm = uimenu(filem, 'label', 'Load project');
      set(loadprojm,'callback','refracproject(''load'')');
   exportm = uimenu(filem, 'label', 'Export');
      expvelm = uimenu(exportm, 'label','Velocity model');
         set(expvelm, 'enable','off');
         set(expvelm,'callback','expvel');
      expdepthm  = uimenu(exportm, 'label','Depth model');
         set(expdepthm,'enable','off');
         set(expdepthm,'callback','expdepth');
      expstatm = uimenu(exportm, 'label','Static corrections');
         set(expstatm,'enable','off');
         set(expstatm,'callback','expstat');
   quitm = uimenu(filem,'label','Quit');
      set(quitm,'callback','close');
processm = uimenu('label','Process');
   rectimem = uimenu(processm,'label','Reciprocal Time check');
	set(rectimem,'enable','off');
	set(rectimem,'callback','rectimecb')
   autopickm = uimenu(processm, 'label','Auto Picking');
	set(autopickm,'enable','off');
	set(autopickm,'callback','autopickcb')
   autorejectm = uimenu(processm,'label','Autoreject CVP');
      set(autorejectm,'enable','off');
      set(autorejectm,'callback','autorejectcb');
   avgcvpm = uimenu(processm, 'label', 'Average CVP');
      set(avgcvpm, 'enable',  'off');
      set(avgcvpm, 'callback','avgcvpcb');
   repickcvpm = uimenu(processm, 'label', 'Repick CVP');
      set(repickcvpm, 'enable',  'off');
      set(repickcvpm, 'callback','repickcvp');
   calcvelm = uimenu(processm, 'label', 'Calculate velocities (1st and 2nd)');
      set(calcvelm, 'enable','off');
      set(calcvelm,'callback','calcvelcb');
   timeanalm = uimenu(processm, 'label', 'Plus Time Analysis');
      set(timeanalm, 'enable','off');
      set(timeanalm,'callback','timeanalcb');
   timerejectm = uimenu(processm, 'label', 'Plus Time Rejection');
      set(timerejectm, 'enable', 'off');
      set(timerejectm, 'callback', 'timerejectcb');
   delaytm = uimenu(processm, 'label', 'Delay Time Analysis');
      set(delaytm, 'enable','off');
      set(delaytm,'callback','delaytcb');
   avgdepthm = uimenu(processm, 'label', 'Depth Calculation');
      set(avgdepthm, 'enable','off');
      set(avgdepthm,'callback','avgdepthcb');
   staticm = uimenu(processm, 'label', 'Static computation');
      set(staticm, 'enable','off');
      set(staticm,'callback','staticcb');
displaym = uimenu('label', 'Display');
   dispshotsm =  uimenu(displaym, 'label', 'Refracted arrivals');
      set(dispshotsm,'enable','off');
      set(dispshotsm, 'callback', 'dispshotscb');
   disprectimem = uimenu(displaym,'label','Reciprocal times');
      set(disprectimem,'enable','off');
      set(disprectimem,'callback','disprectimecb');
   dispcvpstatm = uimenu(displaym, 'label', 'CVP statistics');
      set(dispcvpstatm,'enable','off');
      set(dispcvpstatm, 'callback', 'dispcvpstat' );
%   dispcvpstat2m = uimenu(displaym, 'label', 'CVP statistics (Shot number)');
%      set(dispcvpstat2m,'enable','off');
%      set(dispcvpstat2m, 'callback', 'dispcvpstat2' );
   disppolym = uimenu(displaym, 'label','CVP averages with Polyfit curves');
      set(disppolym,'enable','off');
      set(disppolym, 'callback', 'disppoly' );
   dispcvpavgm=uimenu(displaym,'label','CVP average with refracted arrivals');
      set(dispcvpavgm,'enable','off');
      set(dispcvpavgm, 'callback', 'dispcvpavg' );
   dispvelm = uimenu(displaym, 'label', 'Velocity model' );
      set(dispvelm,'enable','off');
      set(dispvelm, 'callback', 'dispvel' );
   dispplustm = uimenu(displaym, 'label', 'Plus time' );
      set(dispplustm,'enable','off');
      set(dispplustm, 'callback', 'dispplust' );
   dispdepthm = uimenu(displaym, 'label', 'Depth model' );
      set(dispdepthm,'enable','off');
      set(dispdepthm, 'callback', 'dispdepth' );
   dispstaticm = uimenu(displaym, 'label', 'Static solution' );
      set(dispstaticm,'enable','off');
      set(dispstaticm, 'callback', 'dispstatic' );
editm = uimenu('label','Edit');
   editcvpm = uimenu(editm, 'label','Cross over points');
        set(editcvpm,'enable','off');
	set(editcvpm,'callback','editcvp');
   editcvpavgm = uimenu(editm, 'label', 'Cross over point average');
        set(editcvpavgm,'enable','off');
	set(editcvpavgm,'callback','editcvpavg');
   
   editvelm = uimenu(editm, 'label', 'Velocity model');
        set(editvelm,'enable','off');
        set(editvelm,'callback','editvelcb');
   editdepthm = uimenu(editm, 'label', 'Depth model');
        set(editdepthm,'enable','off');
        set(editdepthm,'callback','editdepthcb');
 
   % Save the various menu handles in the figure userdata space, so that
   % the refdata function can access them.  See the file 'refdata.m'
   % There must be one menu here for each matrix that is stored in
   % the refdata function.
   set(gcf, 'userdata',...
       [filem processm editm editcvpm editcvpavgm editvelm displaym ...
	rectimem autopickm avgcvpm autorejectm calcvelm timeanalm quitm ...
       	avgdepthm staticm dispshotsm dispcvpstatm exportm]);
   refdata('init', 'figure', gcf);
   refdata('set','autorejectm',autorejectm);
   refdata('set','avgcvpm',avgcvpm);
   refdata('set','calcvelm',calcvelm);
   refdata('set','timeanalm',timeanalm);
   refdata('set','delaytm',delaytm)
   refdata('set','timerejectm',timerejectm);
   refdata('set','avgdepthm',avgdepthm);
   refdata('set','staticm',staticm);
   refdata('set','rectimem',rectimem);
   refdata('set','autopickm',autopickm);
   refdata('set','repickcvpm',repickcvpm);
   refdata('set','dispshotsm',dispshotsm);
   refdata('set','disprectimem',disprectimem);
   refdata('set','dispcvpstatm',dispcvpstatm);
%   refdata('set','dispcvpstat2m',dispcvpstat2m);
   refdata('set','dispcvpavgm',dispcvpavgm);
   refdata('set','dispvelm',dispvelm);
   refdata('set','dispdepthm',dispdepthm);
   refdata('set','dispplustm',dispplustm);
   refdata('set','dispstaticm',dispstaticm);
   refdata('set','disppolym',disppolym);
   refdata('set','editcvpm',editcvpm);
   refdata('set','editcvpavgm',editcvpavgm);
   refdata('set','editvelm',editvelm);
   refdata('set','editdepthm',editdepthm);
   refdata('set','expvelm',expvelm);
   refdata('set','expdepthm',expdepthm);
   refdata('set','expstatm',expstatm);
end