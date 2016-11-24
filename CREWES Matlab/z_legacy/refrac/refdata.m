% Function storing all the useful variables, vectors and matrices
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

% under the handle menu of the main figure (Refrac)
function answer = refdata(action, parm, value)
if( strcmp(action,'set'))         % Sets a value
   ;
elseif( strcmp(action,'get'))     % Returns a value
   ;
elseif( strcmp(action,'init'))    % Init is the first-time action
   ;
elseif( strcmp(action,'clear'))   % Clears all variables except menu handles
   ;
else
  errs = sprintf('refdata: illegal action: %s',action);
  error(errs);
end
if( strcmp(action,'set') & nargin < 3 )
   error('Wrong number of parameters for REFDATA function. SET needs 3 or 4.');
end
if( strcmp(action,'get') & nargin ~= 2 )
   error('Wrong number of parameters for REFDATA function. GET needs 2.');
end
% Define the size of the parameter array and the menu handle array.
pasize = 25;
mhsize = 27;
if( strcmp(action, 'init') )    % ----------------------------------------
   if( nargin < 3 )
      error('refdata: init action needs 3 parmeters (''figure'' and gcf)');
   end
   figure = value;
   userdata = get(figure, 'userdata');   
   if( length(userdata) ~= 19 )
      error('Internal error: Figure userdata not setup for refdata.');
   end
   pahandle = userdata(1);   % Store parameter array in (1)
   fbthandle = userdata(2);  % Store the FB times matrix in (2)
   fbchandle = userdata(3);  % Store the FB coordinate matrix in (3)
   schandle  = userdata(4);  % Store the shot coordinate array in (4)
   dmhandle  = userdata(5);  % Store the Reciprocal difference matrix (5)
   cihandle  = userdata(6);  % Store the crossoverpoint for shot i matrix (6)
   cjhandle  = userdata(7);  % Store the crossoverpoint for shot j matrix (7)
   cahandle = userdata(8);   % Store the averaged crossover points (8)
   v1handle = userdata(9);   % Store the velocity of the first layer (9)
   v2handle = userdata(10);  % Store the velocity of the second layer (10)
   td1handle = userdata(11); % Store the Plus time of the first layer (11)
   hgahandle = userdata(12); % Store the depth average of the first layer (12)
   sehandle = userdata(13);  % Store the shot elevation (13)
   rehandle = userdata(14);  % Store the receiver elevation (14)
   uphandle = userdata(15);  % Store the uphole time (15)
   rshandle = userdata(16);  % Store the receiver elevation and weathering statics (16)
   sshandle = userdata(17);  % Store the shot elevation and weathering statics (17)
   plthandle = userdata(18); % Store the Plus time average and statistic (18)
   menuhandle = userdata(19); % Store the array of menu handles
   % Define the array that will hold all of the parameters.
   pa = zeros([1 pasize]);
   pa(1) = 0;        % rtrange   - (0=all 1=single pair)
   pa(2) = 1;        % rtpair1   - first shot in pair (i) (only if rtrange=1)
   pa(3) = 1;        % rtpair2   - 2nd shot in pair (j)
   pa(4) = 5;        % mint      - reciprocal time check min. time.
   pa(5) = 0;        % aprange   - autopick range (0=all 1=single shot pair)
   pa(6) = 1;        % appair1   - first shot in pair (only if aprange = 1)
   pa(7) = 1;        % appair2   - 2nd shot in pair
   pa(8) = 0;        % apsub     - subtract if rt bigger than rtmax
   pa(9) = 0;        % rtmax     - maximum rt for subtraction
   pa(10) = 5;       % window    - median filter window 
   pa(11) = 0;       % nshots    - Number of shots
   pa(12) = 0;       % nrecs     - Number of receivers/shot
   pa(13) = 0;	     % dev       - Standard deviation constant
   pa(14) = 0;       % standard  - Use standard or constant deviation
   pa(15) = 0;       % plustreject -Do Plus Time rejection (Y/N)
   pa(16) = 0;       % datum     - Datum elevation
   pa(17) = 0;       % repvel    - Replacement velocity
   pa(18) = 1;       % nd        - Seperation for differentiation
   pa(19) = 1;       % shotgap   - Used in TD stacking - edit CVP average
   pa(20) = 1;       % shotlength- # of shots/side stacked - edit CVP average
   pa(21) = 0.01;    % flatslope - Maximum slope considered flat
   pa(22) = NaN;     % windmn    - Window length for median filter
   pa(23) = NaN;     % offsetrange1 -Offset limit range for CVPautopick
   pa(24) = NaN;     % offsetrange2 -Offset limit range for CVPautopick
   pa(25) = NaN;     % offsetpt  - Offset limit for Time analysis function
   set(pahandle, 'userdata', pa);
   % Define the menu handles
   mh = zeros([1 mhsize]);
   mh(1)  = 0;       % autorejectm - Handle of the autoreject menu
   mh(2)  = 0;       % avgcvpm   - Handle of the avgcvp menu
   mh(3)  = 0;       % calcvelm  - Handle of the calcvelm menu
   mh(4)  = 0;       % timeanalm - Handle of the timeanalm menu
   mh(5)  = 0;       % delaytm   - Handle of the delaytm menu
   mh(6)  = 0;       % avgdepthm - Handle of the avgdepthm mean
   mh(7)  = 0;       % staticm   - Handle of the staticm menu
   mh(8)  = 0;       % timerejectm - Handle of the time rejection menu
   mh(9)  = 0;	     % rectimem  - Handle of the rectimem menu
   mh(10)  = 0;	     % autopickm  - Handle of the autopickm menu
   mh(11) = 0;	     % dispshotsm  - Handle of the dispshotsm menu
   mh(12) = 0;	     % disprectimem  - Handle of the disprectimem menu
   mh(13) = 0;	     % dispcvpstatm  - Handle of the dispcvpstatm menu
%  mh(13) = 0;	     % dispcvpstat2m  - Handle of the dispcvpstat2m menu
   mh(14) = 0;	     % dispcvpavgm  - Handle of the dispcvpavgm menu
   mh(15) = 0;	     % dispvelm  - Handle of the dispvelm menu
   mh(16) = 0;	     % dispdepthm  - Handle of the dispdepthm menu
   mh(17) = 0;	     % dispstaticm  - Handle of the dispstaticm menu
   mh(18) = 0;	     % editcvpm  - Handle of the editcvpm menu
   mh(19) = 0;	     % editcvpavgm  - Handle of the editcvpavgm menu
   mh(20) = 0;	     % editvelm  - Handle of the editvelm menu
   mh(21) = 0;	     % editdepthm  - Handle of the editdepthm menu
   mh(22) = 0;	     % dispplustm  - Handle of the dispplustm menu
   mh(23) = 0;       % expvelm    - Handle of the expvelm menu
   mh(24) = 0;       % expdepthm  - Handle of the expdepthm menu
   mh(25) = 0;       % expstatm   - Handle of the expstatm menu
   mh(26) = 0;       % disppolym  - Handle of the disppoly menu
   mh(27) = 0;       % repickcvpm - Handle of the repickcvp menu
   set(menuhandle, 'userdata', mh);
   % Define the FB time matrix (fbtime).
   fbtime = [];
   set(fbthandle, 'userdata', fbtime);
   % Define the FB coordinate matrix (fbcoord)
   fbcoord = [];
   set(fbchandle, 'userdata', fbcoord);
   % Define the shot coordinate array (shotcoord)
   shotcoord = [];
   set(schandle, 'userdata', shotcoord);
   % Define the difference matrix (diffmat)
   diffmat = [];
   set(dmhandle, 'userdata', diffmat);
 
   % Define crossoverpoints for shot i matrix (cvpi)
   cvpi = [];
   set(cihandle, 'userdata', cvpi);
   % Define crossoverpoints for shot j matrix (cvpi)
   cvpj = [];
   set(cjhandle, 'userdata', cvpj);
   % Define the averaged crossover point matrix (cvpavg)
   % This is a matrix of two columns (col 1 is avg(cvpi)(left) 
   % and col 2 is avg(cvpj)(right)
   cvpavg = [];
   set(cahandle, 'userdata', cvpavg);
   % Define the velocity V1 matrix (v1)
   v1rec = [];
   set(v1handle, 'userdata', v1rec);
   % Define the velocity V2 matrix (v2)
   v2rec = [];
   set(v2handle, 'userdata', v2rec);
   % Define the first layer Plus time  matrix (hg1)
   % This is a matrix of (nshots*nshots)*nfbcoord
   td1 = [];
   set(td1handle, 'userdata', td1);
   % Define the first layer average depth array (with fold, standard deviation)
   % This is an array of the total fbcoord possible
   depth = [];
   set(hgahandle, 'userdata', depth);
   % Define the receiver elevation array
   recelev = [];
   set(rehandle, 'userdata', recelev);
   % Define the shot elevation array
   shotelev = [];
   set(sehandle, 'userdata', shotelev);
   % Define the shot uphole time array
   uphole = [];
   set(uphandle, 'userdata', uphole);
   % Define the receiver statics (elevation and weathering)
   recstat = [];
   set(rshandle, 'userdata', recstat);
   % Define the shot statics (elevation and weathering)
   shotstat = [];
   set(sshandle, 'userdata', shotstat);
   % Define the Plus time average and statistic
   plust = [];
   set(plthandle,'userdata',plust);
else               % ------------------------------------------------------
   % If we are not doing an 'init' action, then we need to get at the
   % userdata matricies from the various menus.
   userdata = get( gcf, 'userdata' );
   [tmp hdls] = size(userdata);
   % This magic number is the number of userdata spaces used: 19
   if( hdls ~= 19 )
      error('Please make the REFRAC window the current figure and re-try.');
   end
   pahandle = userdata(1);
   fbthandle = userdata(2);
   fbchandle = userdata(3);
   schandle  = userdata(4);
   dmhandle = userdata(5);
   cihandle = userdata(6);
   cjhandle = userdata(7);
   cahandle = userdata(8);
   v1handle = userdata(9);
   v2handle = userdata(10);
   td1handle = userdata(11);
   hgahandle = userdata(12);
   rehandle = userdata(13);
   sehandle = userdata(14);
   uphandle = userdata(15);
   rshandle = userdata(16);
   sshandle = userdata(17);
   plthandle = userdata(18);
   mhhandle = userdata(19);
   pa = get(pahandle, 'userdata');
   mh = get(mhhandle, 'userdata');
   fbtime = get(fbthandle, 'userdata');
   fbcoord = get(fbchandle, 'userdata');
   shotcoord = get(schandle, 'userdata');
   diffmat = get(dmhandle, 'userdata');
   cvpi = get(cihandle,'userdata');
   cvpj = get(cjhandle,'userdata');
   cvpavg = get(cahandle, 'userdata');
   v1rec = get(v1handle,'userdata');
   v2rec = get(v2handle,'userdata');
   td1 = get(td1handle,'userdata');
   depth = get(hgahandle,'userdata');
   recelev = get(rehandle,'userdata');
   shotelev = get(sehandle,'userdata');
   uphole = get(uphandle,'userdata');
   recstat = get(rshandle,'userdata');
   shotstat = get(sshandle,'userdata');
   plust = get(plthandle,'userdata');
   if( length(pa) ~= pasize )
      error('refdata: whoa! parameter array is wrong size (%d).\n', length(pa));
   end
end
if( strcmp(action,'clear') )   % ---------------------------------------
   % Clear out the parameter array (pa) and all of the matricies
   pa = zeros([1 pasize]);
   % For these initial values, see the pa initialization in the 'init' action
   pa(2) = 1;   pa(3) = 1;   pa(4) = 5; 
   pa(6) = 1;   pa(7) = 1;   pa(10) = 5;
   pa(18) = 1;  pa(19) = 1;  pa(20) = 1;
   pa(21) = 0.01; pa(22) = NaN;  pa(23) = NaN;
   pa(24) = NaN;  pa(25) = NaN;
   set(fbthandle, 'userdata', [] );
   set(schandle, 'userdata', [] );
   set(dmhandle, 'userdata', [] );
   set(cihandle, 'userdata', [] );
   set(cjhandle, 'userdata', [] );
   set(cahandle, 'userdata', [] );
   set(v1handle, 'userdata', [] );
   set(v2handle, 'userdata', [] );
   set(td1handle, 'userdata', [] );
   set(hgahandle, 'userdata', [] );
   set(rehandle, 'userdata', [] );
   set(sehandle, 'userdata', [] );
   set(uphandle, 'userdata', [] );
   set(rshandle, 'userdata', [] );
   set(sshandle, 'userdata', [] );
   set(plthandle,'userdata', [] );
end
if( strcmp(action,'get') | strcmp(action,'set') )
   if( strcmp( parm, 'rtrange') )        % Set parameter array values
      index = 1;
      parmset = 1;
   elseif( strcmp( parm, 'rtpair1') )
      index = 2;
      parmset = 1;
   elseif( strcmp( parm, 'rtpair2') )
      index = 3;
      parmset = 1;
   elseif( strcmp( parm, 'mint') )
      index = 4;
      parmset = 1;
   elseif( strcmp( parm, 'aprange') )
      index = 5;
      parmset = 1;
   elseif( strcmp( parm, 'appair1') )
      index = 6;
      parmset = 1;
   elseif( strcmp( parm, 'appair2') )
      index = 7;
      parmset = 1;
   elseif( strcmp( parm, 'apsub') )
      index = 8;
      parmset = 1;
   elseif( strcmp( parm, 'rtmax') )
      index = 9;
      parmset = 1;
   elseif( strcmp( parm, 'window') )
      index = 10;
      parmset = 1;
   elseif( strcmp( parm, 'nshots') )
      index = 11;
      parmset = 1;
   elseif( strcmp( parm, 'nrecs') )
      index = 12;
      parmset = 1;
   elseif( strcmp( parm, 'dev') )
      index = 13;
      parmset = 1;
   elseif( strcmp( parm, 'standard') )
      index = 14;
      parmset = 1;
   elseif( strcmp( parm, 'plustreject') )
      index = 15;
      parmset = 1;
   elseif( strcmp( parm, 'datum') )
      index = 16;
      parmset = 1;
   elseif( strcmp( parm, 'repvel') )
      index = 17;
      parmset = 1;
   elseif( strcmp( parm, 'nd') )
      index = 18;
      parmset = 1;
   elseif( strcmp( parm, 'shotgap') )
      index = 19;
      parmset = 1;
   elseif( strcmp( parm, 'shotlength') )
      index = 20;
      parmset = 1;
   elseif( strcmp( parm, 'flatslope') )
      index = 21;
      parmset = 1;
   elseif( strcmp( parm, 'windmn') )
      index = 22;
      parmset = 1;
   elseif( strcmp( parm, 'offsetrange1') )
      index = 23;
      parmset = 1;
   elseif( strcmp( parm, 'offsetrange2') )
      index = 24;
      parmset = 1;
   elseif( strcmp( parm, 'offsetpt') )
      index = 25;
      parmset = 1;
   elseif( strcmp(parm, 'fbtime'))          % Set matrix index values
      index = 1;
      parmset = 2;
   elseif( strcmp(parm, 'fbcoord') )
      index = 2;
      parmset = 2;
   elseif( strcmp(parm, 'shotcoord') )
      index = 3;
      parmset = 2;
   elseif( strcmp(parm, 'diffmat') )
      index = 4;
      parmset = 2;
   elseif( strcmp(parm, 'cvpi') )
      index = 5;
      parmset = 2;
   elseif( strcmp(parm, 'cvpj') )
      index = 6;
      parmset = 2;
   elseif( strcmp(parm, 'cvpavg') )
      index = 7;
      parmset = 2;
   elseif( strcmp(parm, 'v1rec') )
      index = 8;
      parmset = 2;
   elseif( strcmp(parm, 'v2rec') )
      index = 9;
      parmset = 2;
   elseif( strcmp(parm, 'td1') )
      index = 10;
      parmset = 2;
   elseif( strcmp(parm, 'depth') )
      index = 11;
      parmset = 2;
   elseif( strcmp(parm, 'recelev') )
      index = 12;
      parmset = 2;
   elseif( strcmp(parm, 'shotelev') )
      index = 13;
      parmset = 2;
   elseif( strcmp(parm, 'uphole') )
      index = 14;
      parmset = 2;
   elseif( strcmp(parm, 'recstat') )
      index = 15;
      parmset = 2;
   elseif( strcmp(parm, 'shotstat') )
      index = 16;
      parmset = 2;
   elseif( strcmp(parm, 'plust') )
      index = 17;
      parmset = 2;
   elseif( strcmp( parm, 'autorejectm') )    % Set menu handle index values
      index = 1;
      parmset = 3;
   elseif( strcmp( parm, 'avgcvpm') )
      index = 2;
      parmset = 3;
   elseif( strcmp( parm, 'calcvelm') )
      index = 3;
      parmset = 3;
   elseif( strcmp( parm, 'timeanalm') )
      index = 4;
      parmset = 3;
   elseif( strcmp( parm, 'delaytm' ) )
      index = 5;
      parmset = 3;
   elseif( strcmp( parm, 'avgdepthm') )
      index = 6;
      parmset = 3;
   elseif( strcmp( parm, 'staticm') )
      index = 7;
      parmset = 3;
   elseif( strcmp( parm, 'timerejectm') )
      index = 8;
      parmset = 3;
   elseif( strcmp( parm, 'rectimem') )
      index = 9;
      parmset = 3;
   elseif( strcmp( parm, 'autopickm') )
      index = 10;
      parmset = 3;
   elseif( strcmp( parm, 'dispshotsm') )
      index = 11;
      parmset = 3;
   elseif( strcmp( parm, 'disprectimem') )
      index = 12;
      parmset = 3;
   elseif( strcmp( parm, 'dispcvpstatm') )
      index = 13;
      parmset = 3;
%   elseif( strcmp( parm, 'dispcvpstat2m') )
%      index = 13;
%      parmset = 3;
   elseif( strcmp( parm, 'dispcvpavgm') )
      index = 14;
      parmset = 3;
   elseif( strcmp( parm, 'dispvelm') )
      index = 15;
      parmset = 3;
   elseif( strcmp( parm, 'dispdepthm') )
      index = 16;
      parmset = 3;
   elseif( strcmp( parm, 'dispstaticm') )
      index = 17;
      parmset = 3;
   elseif( strcmp( parm, 'editcvpm') )
      index = 18;
      parmset = 3;
   elseif( strcmp( parm, 'editcvpavgm') )
      index = 19;
      parmset = 3;
   elseif( strcmp( parm, 'editvelm') )
      index = 20;
      parmset = 3;
   elseif( strcmp( parm, 'editdepthm') )
      index = 21;
      parmset = 3;
   elseif( strcmp( parm, 'dispplustm') )
      index = 22;
      parmset = 3;
   elseif( strcmp( parm, 'expvelm') )
      index = 23;
      parmset = 3;
   elseif( strcmp( parm, 'expdepthm') )
      index = 24;
      parmset = 3;
   elseif( strcmp( parm, 'expstatm') )
      index = 25;
      parmset = 3;
   elseif( strcmp( parm, 'disppolym') )
      index = 26;
      parmset = 3;
   elseif( strcmp( parm, 'repickcvpm') )
      index = 27;
      parmset = 3;
   else
      errs = sprintf('refdata: parameter %s not found!\n', parm);
      error(errs);
   end
end
if( strcmp(action, 'get') )    % ---------------------------------------
   if( parmset == 1 )
      answer = pa(index);
      fprintf(1,'refdata: getting %s (%d) is %f\n',parm,index,answer);
   elseif( parmset == 3 )
      answer = mh(index);
   elseif( parmset == 2 )
      if( index == 1 )
         answer = get(fbthandle, 'userdata');
      elseif( index == 2 )
         answer = get(fbchandle, 'userdata');
      elseif( index == 3 )
         answer = get(schandle, 'userdata');
      elseif( index == 4 )
         answer = get(dmhandle, 'userdata');
      elseif( index == 5 )
         answer = get(cihandle, 'userdata');
      elseif( index == 6 )
         answer = get(cjhandle, 'userdata');
      elseif( index == 7 )
         answer = get(cahandle, 'userdata');
      elseif( index == 8 )
         answer = get(v1handle, 'userdata');
      elseif( index == 9 )
         answer = get(v2handle, 'userdata');
      elseif( index == 10 )
         answer = get(td1handle, 'userdata');
      elseif( index == 11 )
         answer = get(hgahandle, 'userdata');
      elseif( index == 12 )
         answer = get(rehandle, 'userdata');
      elseif( index == 13 )
         answer = get(sehandle, 'userdata');
      elseif( index == 14 )
         answer = get(uphandle, 'userdata');
      elseif( index == 15 )
         answer = get(rshandle, 'userdata');
      elseif( index == 16 )
         answer = get(sshandle, 'userdata');
      elseif( index == 17 )
         answer = get(plthandle, 'userdata');
      end
   end
end
if( strcmp(action, 'set') )     % ---------------------------------------
   if( parmset == 1 )
      % If the value is null, it will screw up the parameter array
      if( isempty(value) )   
         value = 0;
      end
      pa(index) = value;
      fprintf(1,'refdata: setting %s (%d) to %f\n',parm,index,value);
      set(pahandle, 'userdata', pa);
   elseif( parmset == 3 )
      % If the value is null, it will screw up the parameter array
      if( isempty(value) )   
         value = 0;
      end
      mh(index) = value;
      set(mhhandle, 'userdata', mh);
   elseif( parmset == 2 )
      if( index == 1 )
         set(fbthandle, 'userdata', value);
      elseif( index == 2 )
         set(fbchandle, 'userdata', value);
      elseif( index == 3 )
         set(schandle, 'userdata', value);
      elseif( index == 4 )
         set(dmhandle, 'userdata', value);
      elseif( index == 5 )
         set(cihandle, 'userdata', value);      
      elseif( index == 6 )
         set(cjhandle, 'userdata', value);
      elseif( index == 7 )
         set(cahandle, 'userdata', value);
      elseif( index == 8 )
         set(v1handle, 'userdata', value);
      elseif( index == 9 )
         set(v2handle, 'userdata', value);
      elseif( index == 10 )
         set(td1handle, 'userdata', value);
      elseif( index == 11 )
         set(hgahandle, 'userdata', value);
     elseif( index == 12 )
         set(rehandle, 'userdata', value);
     elseif( index == 13 )
         set(sehandle, 'userdata', value);
     elseif( index == 14 )
         set(uphandle, 'userdata', value);
     elseif( index == 15 )
         set(rshandle, 'userdata', value);
     elseif( index == 16 )
         set(sshandle, 'userdata', value);
     elseif( index == 17 )
         set(plthandle, 'userdata', value);
     end
   end
end