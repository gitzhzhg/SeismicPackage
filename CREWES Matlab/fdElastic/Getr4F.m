function Getr4F(modelDir,kOut)
% function Getr4F(kOut)
%Function to read a set of trace information from disc,
% assuming a standard name and format
%The input parameters are
%kOut .... The switch that gives the type of trace output required
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
  %clear
  %load header
  %load headrFile
  dirHdr = [modelDir,'\headrFile'];
  disp(dirHdr)
  load(dirHdr)
  %whos
  disp(parmFile)
  exten = ''; %''; %'R';
  seisTif = ''; SFfile = '';

% Select which string of traces (phones)
  lineXZ = 'x';
  str1 = 'Load traces';
  str2 = ['Trace string ',lineXZ,' vs t'];
  %str3 = ['Plot U',comp];
  str99 = 'String right';
kOptn = 0;
while kOptn < 2  
    kOptn = menu(str1,str2,str99);
    if kOptn==1
        if ~strcmpi(lineXZ,'x'); lineXZ = 'x'; 
        else lineXZ = 'z';
        end
        str2 = ['Trace string ',lineXZ,' vs t'];
    end
end
if lineXZ=='x'
    spMin = xMin;
else
    spMin = zMin;
end

%If output is not SEGY, choose component to plot
%if kOut ~= 3
if kOut < 9
    comp = 'x';
    str1 = 'Load component';
    str2 = ['Load U',comp];
    str99 = 'Component right';
    kOptn = 0;
    while kOptn < 2  
        kOptn = menu(str1,str2,str99);
        if kOptn==1
            if ~strcmpi(comp,'x'); comp = 'x'; 
            else comp = 'z';
            end
            str2 = ['Load U',comp];
        end
    end
%     [fact,frameTif,fXmax,fZmax]....
%     = plotTra2G(fact,Dxz,ix1,iz1,nxf,nzf);

    tracesx = getrLoad(modelDir,lineXZ,comp,exten);                 %Get traces
    [nSpf,nStep] = size(tracesx);
    figure
    if kOut < 3
        [factt,indX,Xarr,seisTif] = ....
            resampSplot(fact,spMin,nSpf,Dxz,kOut);          %*****
        nPh = length(Xarr);
        Xdec = zeros(nPh,nSpf);
        for idd = 1:nPh
            inx = indX(idd);
            Xdec(idd,inx) = 1;
        end
        %disp(size(Xdec)); disp(size(tracesx)); 
        recUx = Xdec*tracesx;
        Tarr = (1:nStep)*Dt*1000;
        if kOut == 1
            %figure
            %rAmp = plotseis(Uxyz(ix1:intpl:nxf,:)',tSAx,xSAx,1,fact,1,1,'k');
            plotseis(recUx',Tarr,Xarr,1,factt,1,1,'k');             %plotseis
            %plotseis(recUx,Xarr,Tarr,1,factt,1,1,'k');             %*****
        else
            recMax = max(max(recUx)); recMin = min(min(recUx));
            if recMax<-recMin
                recMax = -recMin;
            end
            fact1 = 32/recMax;
            recSc = recUx*fact1*factt+31.5;
            %figure
            image(Xarr,Tarr,recSc')                         %*****
            colormap(seisclrs)
            %colorbar
        end
    else
        tMin = 0;
        [factt,indX,Xarr,indT,Tarr,DtPh,SFfile] = ....
            resampSFplot(spMin,nSpf,Dxz,tMin,Dt,nStep);
        nPh = length(Xarr);
        nTr = length(Tarr);
        Xdec = zeros(nPh,nSpf);
        for idd = 1:nPh
            inx = indX(idd);
            Xdec(idd,inx) = 1;
        end
        Tdec = zeros(nStep,nTr);
        for idd = 1:nTr
            Tdec(indT(idd),idd) = 1;
        end
        %recUx = Xdec*tracesx;
        recUx = Xdec*tracesx*Tdec;
        h = plotseis(recUx,Xarr,Tarr,1,factt,1,1,'k');             %plotseis
        %get(gca)
        set(gca,'YMinorTick','on')
    end
    if strcmpi(lineXZ,'x')
        dNear = Xarr-shotX;
        dPerp = shotDepth-trZ;
        title(['U',comp,' displacement in X line, Y = ',....
            num2str(0),', Z = ',num2str(trZ)])
        %xlabel('X-coordinate (m)')
        label = 'X-coordinate (m)';
    end
    if strcmpi(lineXZ,'z')
        dNear = Xarr-shotDepth;
        dPerp = shotX-trX;
        title(['U',comp,' displacement in Z line, X = ',....
            num2str(trX),', Y = ',num2str(0)])
        %xlabel('Z-coordinate (m)')
        label = 'Z-coordinate (m)';
    end
    %Plot normal moveout curves         %*********************
    dist = sqrt(dNear.^2+dPerp.^2);
    timeSh = dist*1000/shotVs+shift;
    timeP = dist*1000/shotVp+shift;
    %disp(shotVp); disp(shotVs);
    hold on
    lw = 1;
    if kOut < 3
        plot(Xarr,timeSh,'g','lineWidth',lw)
        plot(Xarr,timeP,'r','lineWidth',lw)
        ylabel('Time (ms)')
        xlabel(label)
    else
        plot(timeSh,Xarr,'g','lineWidth',lw)
        plot(timeP,Xarr,'r','lineWidth',lw)
        xlabel('Time (ms)')
        ylabel(label)
    end
    bigfont(gca,1.5,2,1)
    whitefig
    if ~isempty(seisTif)
      %fileName = [parmFile '_' seisTif 'U' comp dir 'S.tif'];
      fileName = [parmFile '_' seisTif 'U' comp '_' lineXZ 'S.tif'];
      eval(['print -dtiff -r150 ' fileName])
      disp(fileName)
    end
    if ~isempty(SFfile)
      fileName = [parmFile '_' SFfile 'U' comp '_' lineXZ 'SF.tif'];
      eval(['print -dtiff -r150 ' fileName])
      disp(fileName)
    end
%     nxP = round(fXmax/Dxz)+1;
%     if nxf<nxP; nxP = nxf; end
%     nzP = round(fZmax/Dxz)+1;
%     if nzf<nzP; nzP = nzf; end
%     plUy = [];
%     plotTraces3(comp,Dxz,Dt,nxP,0,nzP,nstep,99,0,99,ix1,iz1,...
%         shotX,shotDepth,shotVp,shotVs,shift,dir,recUx,0,recUx,fact)

else
    comp = 'x';
    tracesx = getrLoad(modelDir,lineXZ,comp,exten);
    comp = 'z';
    tracesz = getrLoad(modelDir,lineXZ,comp,exten);
    nShot = 1;
    fdSEGYgetr(modelDir,parmFile,Dt,Dxz,nShot,spMin,.....
        tracesx,tracesz,lineXZ,shotDepth,shotX)
    %plotimage(traces)
end  