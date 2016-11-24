function [generic] = fdReadgeo2(modelDir,fileName)
%function [generic] = fdReadgeo2(fileName)
%Read a geological finite-difference model in text format, .geo
%Build with Matlab editor, Crimson editor, notepad
%Called directly from the modelling program (fdInitMod...)
%
%The input parameters are
%fileName ... The geological definition file (within quotes) (gfdfile)
%The output parameters are
%generic .... A disc .mat file passed to fdInitMod...
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
%fid = fopen('gfdTest2.geo','rt');
%disp(fileName)
[fid,mess] = fopen(fileName,'rt');
%disp(mess)
nBoxes = 0; nTbox = 0;
iLayer = 0; iBox = 0;
Tops = zeros(40,65);
nEnt = zeros(40,1);
Boxes = zeros(30,45);
nBox = zeros(30,1);
while 1
    tline = fgetl(fid);
    if ~ischar(tline); break; end
    k = strfind(tline,'%');
    if isempty(k); k = length(tline)+1; end
    if k>1
        dline = tline(1:k-1);
        %disp(dline)
        isbox = sscanf(dline,'%c',3);
        if ~strcmpi(isbox,'box')
            %This line of text is assumed to define an horizon
            [dlayer nent] = sscanf(dline,'%f'); %Find number of entries
            if nent>3
                iLayer = iLayer+1;
                nEnt(iLayer) = nent;
                Tops(iLayer,1:nent) = dlayer;
            else if nent==2                     %End of horizons
                    xMin = dlayer(1); xMax = dlayer(2);
                else
                    modName = dline;
                end
            end
            %disp(dlayer(1:nent)')
        else
            %This line of text assumed to define an enclosed box
            %disp(dline)
            [token, remain] = strtok(dline);
            %disp(remain)
            [dlayer nent] = sscanf(remain,'%f'); %Find number of entries
            if nent>3
                iBox = iBox+1;
                nBox(iBox) = nent;
                Boxes(iBox,1:nent) = dlayer;
                %disp(Boxes(1,:))
            else if nent==2                     %End of horizons
                    xMin = dlayer(1); xMax = dlayer(2);
                else
                    modName = dline;
                end
            end

        end
    end
end

nLayers = iLayer; nBoxes = iBox; nTotal = nLayers + nBoxes;
maxLlayers = max(nEnt);
maxLboxes = max(nBox);
% if nBoxes > 0
%     disp(nBoxes)
%     for iBox = 1:nBoxes
%         for iB = 1:nBox(iBox)
%             disp(Boxes(iBox,iB))
%         end
%     end
% end

%nEntMax = max(nEnt);
nXZtops = (maxLlayers-3)/2;
nXZbox = (maxLboxes-3)/2 + 1;
%disp(nXZtops)

Vp = zeros(nTotal,1);
Vs = zeros(nTotal,1);
rho = zeros(nTotal,1);
Vp = Tops(1:nLayers,1);         %These values passed to 'generic' file
Vs = Tops(1:nLayers,2);
rho = Tops(1:nLayers,3);
UpDown = 'Down';                %Code defining type of model - don't change!

Xtops1 = zeros(nLayers,nXZtops);
Ztops1 = zeros(nLayers,nXZtops);
%disp(Tops);
%Xtops1(:,1:nXZtops) = Tops(1:nLayers,4:2:nEntMax);    %Get Xpositions
%Ztops1(:,1:nXZtops) = Tops(1:nLayers,5:2:nEntMax);    %Get Zlevels
Xtops1(:,1:nXZtops) = Tops(1:nLayers,4:2:maxLlayers);    %Get Xpositions
Ztops1(:,1:nXZtops) = Tops(1:nLayers,5:2:maxLlayers);    %Get Zlevels

Xtops = Xtops1;
Ztops = Ztops1;
%Add extra points for calculation purposes

%nTop = zeros(1,nXZtops+5);
nTop = zeros(nLayers,1);
for iL = 1:nLayers
    nT = (nEnt(iL)-3)/2;

    %Add a point on left of each layer (if necessary)
    if Xtops1(iL,1)>xMin
        Xtops(iL,2:nT+1) = Xtops1(iL,1:nT);
        Xtops(iL,1) = xMin;
        Ztops(iL,2:nT+1) = Ztops1(iL,1:nT);
        Ztops(iL,1) = Ztops1(iL,1);
        nT = nT+1;
        nTop(iL) = nT;
    end
    %Add a point on right of each layer
    Xtops(iL,nT+1) = xMax;
    Ztops(iL,nT+1) = Ztops(iL,nT);
    nTop(iL) = nT+1;
end
nTopMax = max(nTop);
%disp(nTopMax)

%Fill out X & Z matrix rows to the same length
for iL = 1:nLayers
    nT = nTop(iL);
    if nT<nTopMax
        for ix = nT+1:nTopMax
            Xtops(iL,ix) = Xtops(iL,nT);
            Ztops(iL,ix) = Ztops(iL,nT);
        end
    end
end
%Analyze boxes to include within parameters and tops
if nBoxes > 0
    nTbox = zeros(nBoxes,1);
    %disp(nBoxes)
    for iBox = 1:nBoxes
        iLay = nLayers + iBox;
        Vp(iLay) = Boxes(iBox,1);
        Vs(iLay) = Boxes(iBox,2);
        rho(iLay) = Boxes(iBox,3);
        nTb = (nBox(iBox)-3)/2;
        for iB = 1:nTb
            Xtops(iLay,iB) = Boxes(iBox,iB*2 + 2);
            Ztops(iLay,iB) = Boxes(iBox,iB*2 + 3);
            %disp(Boxes(iBox,iB))
        end
        %Loop the box around to the first values
        nTb = nTb + 1;
        %disp(iLay); disp(nTb)
        Xtops(iLay,nTb) = Boxes(iBox,4);
        Ztops(iLay,nTb) = Boxes(iBox,5);
        nTbox(iBox) = nTb;
    end
    %disp(Xtops(iLay,:));
    %disp(Ztops(iLay,:));
    %disp(Vp); disp(Vs); disp(rho);
end
%disp('Xtops');disp(Xtops)
%disp('Ztops');disp(Ztops)

%disp(modName)    
%Plot model for quality control
    figure
    %Plot layers
    plot(Xtops(1:nLayers,1:nTopMax)',Ztops(1:nLayers,1:nTopMax)')
    hold on
    %Add plots of boxes
    for iBox = 1:nBoxes
        iLay = nLayers + iBox;
        plot(Xtops(iLay,1:nTbox(iBox))',Ztops(iLay,1:nTbox(iBox))')
    end
    hold off
    axis ij
    title(modName)
    xlabel('X co-ordinate')
    ylabel('Depth')
    %boldlines
    bigfont(gca,1.5,2,1)
    whitefig
    grid off
    %print -dtiff -r150 Gmodel.tif
    Fplace = [modelDir,'\Gmodel.tif'];
    disp(Fplace)
    %print('-dtiff', '-r150', Fplace)
    
generic = [modelDir,'\generic.gfd'];
%generic = 'generic.gfd';
save(generic,'Vp','Vs','rho','Xtops','Ztops','modName','UpDown',....
    'nTopMax','nBoxes','nTbox')
%disp(size(Xtops))