%surfB3FD2d.m - Generates a model with an emphasis on surface waves
%Finite-difference elastic modelling code
%Some parameter coding is done in this script, most set in a .parm file
%The main program control is from a menu set up here
%
% P.M. Manning, Dec 2011
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

if (nargin==1)
    modelDir = project_dir;
else
    modelDir = uigetdir([],'Select mFD2D project directory');
end

if (modelDir == 0)
   warning('mFD2D:cancelled','User cancelled project directory selection');
   return;
end
saved = 'no';
testSave = exist('saved','var');
if testSave == 0; saved = 'yes'; end
if strcmpi(saved,'No')
    kOptn = menu('Choose computation','Save computations',.....
        'Check .geo (via .parm)','Load prior',.....
        'Start afresh','Continue from previous','Plot last frame (K 5)',.....
        'Plot WiggleVA (K 6)','Plot Vdensity','Plot surface at t''s','Play movie',.....
        'Make AVI movie file (K 9)','Output SEGY (K 10)');
else
    %kO = menu('Choose computation','Load prior','Start afresh',.....
    %    'Continue from previous','Plot last frame (K 5)',.....
    %    'Plot WiggleVA (K 6)','Plot Vdensity','Play movie','Make AVI movie file (K 9)',.....
    %    'Output SEGY (K 10)');
    kO = menu('Choose computation','Check .geo (via .parm)','Load prior',.....
        'Start afresh','Continue from previous','Plot last frame (K 5)',.....
        'Plot WiggleVA (K 6)','Plot Vdensity','Plot surface at t''s','Play movie',.....
        'Make AVI movie file (K 9)','Output SEGY (K 10)');
    kOptn = kO+1;
end
%Key code 1 - Save the model calculations to disc *****************************
if kOptn==1
    disp('The existing finite-difference computed models are ')
    ls *.fdc
%     fdcFile = input('Type file name within quotes (.fdc will be appended)');
%     %eval(['save ',fdcFile,'.fdc'])
%     kOptn = 5;
%     save([fdcFile,'.fdc'])
    disp(['Basic file name will be ',parmFile,'_.fdc']);
    fdcFile = input('Insert unique part of file name within quotes');
    kOptn = 5;
    saveSt = [parmFile,'_',fdcFile];
    saveStr = [saveSt,'.fdc'];
    disp(saveStr)
    save(saveStr)
    fdSaveRetr('s',saveSt)
    saved = 'Yes';
end
%Key code 2 - Check model  ****************************************************
if kOptn==2;
    [parmFile] = fdFindParms;
    %[~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,....
    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,....
        gfdFile] = readParmsB(parmFile);
    %disp(gfdFile)
    fdReadgeo2(gfdFile);
end
%Key code 3 - Load previous model calculations ********************************
if kOptn==3;
    %clear
    kOptn = 3;
    [oldFile] = fdLoadPrior;
    disp(oldFile)
    load(oldFile,'-mat')
    lStr = length(oldFile);
    retrSt = oldFile(1:lStr-4);
    disp(retrSt)
    %fdSaveRetr('r',retrSt)
    saved = 'Yes';
    %whos
end
%Key code 4 - Start model from time zero **************************************
if kOptn==4;
    kOptn = 4;
    %Main input parameters
        initStep = 1;               %Always 1 at this point
    [parmFile] = fdFindParms;
        %centreFreq,energySrc,trX,trZ,iLbnd,iRbnd,iTbnd,iBbnd,....
    [Dt,Dxz,xMin,lengthX,lengthZ,nstep,nframes,shotDepth,shotX,....
        centreFreq,energySrc,cycleSrc,timeSrc,trX,trZ,iLbnd,iRbnd,iTbnd,iBbnd,....
        mvXmax,mvZmax,mvPlot,mvAmp,mvClip,gfdFile,wncvar,mvTif,contBlk]....
        = readParmsB(parmFile);
        iTrAcq = 'dumb';
        nShot = 1; %2; %1;

    %Various ASCII files are written to folder tracePath.
    %Their names are suffixed version of the traceFile variable.
    %See fdSEGY2.m, see the man pages to ascii2segy on how to add more.
    %Output in seismic trace format - .txt file, '' if none
    %suffix files.
        tracePath = 'c:\cygwin\tmp\a2s\';            % path to ASCII repository.
        traceFile = 'testF';
        traceZlevel = 0;            %In metres

        %Movie size restrictions (to save space)
            nx = round(lengthX/Dxz)+1;
            nz = round(lengthZ/Dxz)+1;
            mvTinit = 1;                                         %Index
            %mvXmax = lengthX; mvZtop = 0; mvZmax = lengthZ;  %In metres
            mvZtop = -30; %In metres
% 
    if Dt>.004
        error('Dt is coded in seconds, the maximum allowed is now .004')
    end
    if timeSrc == 1
        [wave,tw] = rickWrap(Dt,centreFreq);    %Ricker
        [sMax,iMax] = max(wave);
    end
    if timeSrc == 2
        [wave,tw] = singleF(Dt,centreFreq,cycleSrc);     %cosine in a gaussian
        [sMax,iMax] = max(wave);
    end
    if timeSrc == 3
        [wave,tw] = fdMinphase(Dt,centreFreq,cycleSrc);    %Minphase by double decon
        iMax = 0;
    end
    if(any(isnan(wave)))
        error('Filter frequency and time sample rate are incompatible')
    end
    disp(['Wavelet lag is ' num2str(iMax*1000*Dt) ' msec'])
    %disp(wave(1:5))
    %[wave,tw] = joeWave(centreFreq,Dt);
    %[wave1,tw]=ricker(Dt,centreFreq,.06);wave = tomin(wave1,0);
    %%% ng [wave1,tw]=ricker(Dt,centreFreq,.06);wave = toinv(wave1,0);
    %[wave1,tw]=ricker(Dt,centreFreq,.06);wave = ddcon(wave1,60);
    %[wave,tw] = wavemin(Dt,centreFreq);
    figure; plot(tw,wave);title('Source wavelet');xlabel('time in seconds.')
      bigfont(gcf,1.5,1)
      %set(gcf,'Color',[0 .8 .608],'InvertHardcopy','off')
      print -dtiff -r150 sourceWavelet.tif
%     disp(wave)
%    stop
%[M,Ux,Uz,ix1,nxplot,iz1,nzplot,surfUz,surfUx] = fdLayDoc2.....
    %Ux = [];Uxt0 = [];Uz = [];Uzt0 = [];isfX = [];isfZ = [];
    Ux = [];Uxt0 = [];Uz = [];Uzt0 = [];isfZ = [];
    %surfUz = [];surfUx = [];wellUx = [];wellUz = [];jfr = [];mint = [];iweX = [];
    jfr = [];mint = [];iweX = [];
    actiF = figure;
    M = getframe;
    nOld = 4; %99; %0;   %0 - new/v0, 2 - new/v2, else - old
    if nOld == 4
%         [M,Ux,Uxt0,Uz,Uzt0,surfUx,surfUz,wellUx,wellUz,isfZ,iweX,zMin,...
%             jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
%     compB2AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
%         iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,mvTinit,...
%         iTrAcq,trX,trZ,surfUx,surfUz,wellUx,wellUz,isfZ,iweX,...
%         mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
%         M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint);
        [M,Ux,Uxt0,Uz,Uzt0,isfZ,iweX,zMin,...
        jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
    compB4AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
        iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,contBlk,mvTinit,...
        iTrAcq,trX,trZ,isfZ,iweX,...
        mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
        M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint);
    disp(parmFile); disp('*****')
    end
    %disp(size(Ux)); disp(size(Uxt0)); 
    %disp(Uz(137,150:169))
    saved = 'No';
    fclose('all');
    
end
%Key code 5 - Continue modelling **********************************************
if kOptn==5;
    %Some carefully selected parameters may be changed here
    initStep = nstep+1;
      disp(['The model has been completed through ',num2str(nstep),....
          ' steps'])
      valu = input('Enter new total number of steps ');
      %mint = round(nstep/nframes);
      nstep = round(valu);
      nframes = round((nstep-1)/mint)+1;
%         [M,Ux,Uxt0,Uz,Uzt0,surfUx,surfUz,wellUx,wellUz,isfZ,iweX,zMin,...
%             jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
%     compB2AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
%         iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,mvTinit,...
%         iTrAcq,trX,trZ,surfUx,surfUz,wellUx,wellUz,isfZ,iweX,...
%         mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
%         M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint);
        [M,Ux,Uxt0,Uz,Uzt0,isfZ,iweX,zMin,...
        jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
    compB4AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
        iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,contBlk,mvTinit,...
        iTrAcq,trX,trZ,isfZ,iweX,...
        mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
        M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint);
        %iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,mvTinit,...& From B3 - 2nd

    saved = 'No';

end
%Key code 6 - Plot final snapshot *********************************************
if kOptn==6
    %Change plotting parameters here, for final frame
    disp('here')
    plotFF2(mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,iz1,zMin,nzplot,.....
        Dt,nstep,parmFile,ixp,izp)
end
%Key code 7 - Plot traces WiggleVA  **************************************
if kOptn==7
    kOut = 1;
    Getr4F(kOut)
end
%Key code 8 - Plot traces Vdensity **************************************
if kOptn==8
    kOut = 2;
    Getr4F(kOut)
end
%Key code 9 - Plot surfaces at times WiggleVA  ***************************
if kOptn==9
    kOut = 3;
    Getr4F(kOut)
end
%Key code 10 - Run Matlab movie ************************************************
if kOptn==10
    %figure
    nRepeats = 5;       %Number of times the movie is repeated, once loaded
    movie(M,nRepeats)
end
%Key code 11 - Convert movie (M) file to a movie .avi file       ***************
if kOptn==11
    movieFile = 'fps5'; %'movie';
    %movie2avi(M,'fileM10W','fps',10,'compression','None','colormap','jet')
    gfdF = gfdFile;
    n = findstr('.',gfdF);
    gfdF(n) = '_';
      fileName = [gfdF '_' movieFile 'M'];
      disp(['Output movie file "' fileName '.avi"'])
    movie2avi(M,fileName,'fps',5,'compression','Cinepak')
end
%Key code 12 - Prepare for SEGY output (intermediate text files) ********
if kOptn==12
    kOut = 9;
    Getr4F(kOut)
%     fdSEGY4(parmFile,Dt,Dxz,nShot,xMin,surfUx,surfUz,....
%         zMin,wellUx,wellUz,shotDepth,shotX)
end