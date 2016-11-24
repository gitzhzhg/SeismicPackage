function [indX,Xarr,indT,Tarr,DtPhout,SEGYfile] = ....
        resampSEGYg(spMin,nSpf,Dxz,tMin,Dt,nStep)
% function [indX,Xarr,indT,Tarr,DtPh,SEGYfile] = ....
%         resampSEGYg(spMin,nSpf,Dxz,tMin,Dt,nStep)
%Change trace selection parameters here, set resample vectors
%The input parameters are
%spMin ... The offset or depth of the first available trace
%nSpf .... The number of traces available
%Dxz ..... Spatial sample rate
%tMin .... Now zero
%Dt   .... FD sample rate in seconds
%nStep ... The number of time steps
%The output parameters are
%indX .... The index array of the chosen traces
%Xarr .... The offset or depth array of the chosen traces
%indT .... The index array of the chosen times
%Tarr .... The time array of the chosen traces
%DtPhout . The time interval of the chosen traces
%SEGYfile  The name of the output SEGY file
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
persistent phMin phSp nPh tZero DtPh

spMax = spMin + Dxz * nSpf;
 
if isempty(phMin),   phMin = spMin; end;
if phMin < spMin,    phMin = spMin; end;
if isempty(phSp),    phSp = Dxz; end;
if phSp < Dxz,       phSp = Dxz; end;
%if isempty(nPh),     nPh = nSpf; end;
nPhMax = floor((spMax - spMin)/phSp);
%if nPh > nPhMax,     nPh = nPhMax; end;
nPh = nPhMax;                               %Best option?

if isempty(tZero),   tZero = tMin; end;
if tZero < tMin,     tZero = tMin; end;
DtPhR = Dt*1000;
if isempty(DtPh),    DtPh = 1; end;
if DtPh < DtPhR,     DtPh = DtPhR; end;
%disp(DtPh)

%tZero = tMin; phSp = Dxz; nPh = nSpf; DtPh = DtPhR; %phMax = spMin+nSpf*Dxz;
%spMax = spMin+Dxz*nSpf;

spMax = phMin + phSp * nPh;
nPhMax = nSpf;
SEGYfile = 'default';
  str1 = 'SEGY parameters';
  str6 = ['First time = ',num2str(tZero)];
  str7 = ['Delta t (ms) = ',num2str(DtPh)];
  str8 = ['File name = ',SEGYfile];
  str99 = 'All OK';
kOptn = 0;
while kOptn < 7
    str3 = ['First phone = ',num2str(phMin)];
    str4 = ['Phone space. = ',num2str(phSp)];
    str5 = ['No. of phones. = ',num2str(nPh)];
    kOptn = menu(str1,str3,str4,str5,str6,str7,str8,str99);
    if kOptn==1
        phMin = input('Type first position ');
        %str3 = ['First phone = ',num2str(phMin)];
        if (phMin<spMin)
            phMin = spMin;
        end
        nPhMax = floor((spMax-phMin)/phSp)+1;
    end
    if kOptn==2
        phSp = input('Type phone spacing ');
            %nPhMax = floor((spMax-phMin)/phSp)+1;
            nPhMax = floor((spMax-phMin)/phSp);
    end
    if kOptn==3
        nPh = input('Type no. of phones ');
        if nPh > nPhMax
            nPh = nPhMax;
        end
    end
    if kOptn==4
        tZero = input('Type first time ');
        str6 = [' First time = ',num2str(tZero)];
    end
    if kOptn==5
        DtPh = input('Type time sample rate (ms)');
        if DtPh < DtPhR 
            DtPh = DtPhR; end;
        str7 = ['Delta t (ms) = ',num2str(DtPh)];
    end
    if kOptn==6
        SEGYfile = input('Name the SEGY file ','s');
        str8 = ['File name = ',SEGYfile];
    end
    if nPh>nPhMax 
        nPh = nPhMax;
    end
end
    %disp([phMin nPh phSp])
indX = zeros(nPh,1);    %Indices to select in X
Xarr = zeros(nPh,1);
for iPh = 1:nPh
    pos = phMin+(iPh-1)*phSp;
    indX(iPh) = round((pos-spMin)/Dxz)+1;
    %disp(indX(iPh))
    Xarr(iPh) = pos;
end
tMax = tMin+nStep*DtPhR;
DtPhout = DtPh;
nTsamp = round((tMax-tZero)/DtPh);
indT = zeros(nTsamp,1);    %Indices to select in time
Tarr = zeros(nTsamp,1);
for iTime = 1:nTsamp
    time = tZero+(iTime-1)*DtPh;
    indT(iTime) = round((time-tMin)/DtPhR)+1;
    Tarr(iTime) = time;
end
%disp(DtPh)
%disp(Xarr)