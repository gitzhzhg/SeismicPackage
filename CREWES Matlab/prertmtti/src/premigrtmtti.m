function premigrtmtti(shotsfile, velfile, epsilonfile, deltafile, phifile,...
                      dx, dz, nxo, nz, nshot,lpad, rpad,  fpeak, ixshot_start,migfile)
%
% premigrtmtti(shotsfile, velfile, epsilonfile, deltafile, phifile,...
%                      dx, dz, nxo, nz, nshot,lpad, rpad, fpeak, ixshot_start, migfile)
%
%
% premigrtmtti: implement tti reverse-time migration for P-wave
% velocity files, ansiotropic parameters and some parameters.
%
%	shotsfile: SEGY file with I-EEE type
%
%	velfile: vertical binary velocity file
%
%	epsilonfile: epsilon binary file
%
%	deltafile: delta binary file
%
%   phifile: dipping angle binary file
%
%   dx: the lateral interval
%
%   dz: the depth interval
%
%   nxo: the size of lateral direction
%
%   nz: the size of depth direction
%
%   nshot: the number of shots to be migrated
%
%   lpad:  the trace number of left padding
%
%   rpad: the trace number of right padding
%
%   fpeak: the peak frequency of seismic data
%
%   ixshot_start: the first shot to be migrated
%
%   migfile: final migration file
%
%   Xiang Du, May   2007, $1.0
%             Sept. 2008, $1.1
%             Nov.  2008, $1.2
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


% read the velocity file
fidvel=fopen(velfile,'r');
vel=fread(fidvel,[nz,nxo],'float');
fclose(fidvel);

% read the anisotropic parameters
fidepsilon=fopen(epsilonfile,'r');
epsilon=fread(fidepsilon,[nz,nxo],'float');
fclose(fidepsilon);

fiddelta=fopen(deltafile,'r');
delta=fread(fiddelta,[nz,nxo],'float');
fclose(fiddelta);

fidphi=fopen(phifile,'r');
phi=fread(fidphi,[nz,nxo],'float');
fclose(fidphi);

%pad the top for absorbing boundary condition
temp=zeros(nz+20,nxo);

%velocity
temp(21:nz+20,:)=vel;
for i=1:20;temp(i,:)=vel(1,:);end;
clear vel;
vel=temp;

%epsilon
temp(21:nz+20,:)=epsilon;
for i=1:20;temp(i,:)=epsilon(1,:);end;
clear epsilon;
epsilon=temp;

%delta
temp(21:nz+20,:)=delta;
for i=1:20;temp(i,:)=delta(1,:);end;
clear delta;
delta=temp;

%theta
temp(21:nz+20,:)=phi;
for i=1:20;temp(i,:)=phi(1,:);end;
clear phi;
phi=temp;

clear temp;

%update the grid size of z direction
nz=nz+20;
izs=21;
nxsponge=20;
nzsponge=20;
epow=0.03;

% read in the segy file, read the headers from a segyfile
fidr=fopen(shotsfile,'r','ieee-be');        % the segy file to read data from
textheader=fread(fidr,3200,'uchar');        % READ: the text header in the first 3200 bytes of a segyfile
bheader1=fread(fidr,3,'int32');
bheader2=fread(fidr,24,'int16');
bheader3=fread(fidr,170,'int16');
binaryheader=[bheader1;bheader2;bheader3];  % READ: the binary header in the 400 bytes following the text header
dtold=binaryheader(6)/1e6;                  % sample rate
ntold=binaryheader(8);                      % number of samples per trace
ntr=0;                                      % initiate the total trace number in the file.
itr=1;                                      % count the trace number
numEnsamble=0;                              % initiate the number of ensamble
icount=0;

%Initial migration result
finalcresult=zeros(nz,nxo);

while ~feof(fidr) & numEnsamble<nshot

%count the time cost
  tic;


% initiate the real number of traces in each ensamble
  Data=zeros(ntold,nxo);

  while (1)
    position=3600+ntr*(240+ntold*4);    % 4 bytes for each sample
    fseek(fidr,position,-1);
    theader1=fread(fidr, 7,'int32');
	theader2=fread(fidr, 4,'int16');
	theader3=fread(fidr, 8,'int32');
	theader4=fread(fidr, 2,'int16');
	theader5=fread(fidr, 4,'int32');
	theader6=fread(fidr,46,'int16');
    theader7=fread(fidr,30,'int16');
	theader=[theader1;theader2;theader3;theader4;theader5;theader6;theader7];
    if length(theader) == 0
      break;
    end

    Receiver = theader(24);
    Scalco=theader(21);
    if Scalco > 0
      iReceiver = Receiver*Scalco/dx+1;
    elseif Scalco ==0
      iReceiver = Receiver/dx+1;
    else
      iReceiver = Receiver/abs(Scalco)/dx+1;
    end

    SOURCE=theader(22);
    if Scalco > 0
      ix1 = SOURCE*Scalco/dx+1;
    elseif Scalco ==0
      ix1 = SOURCE/dx+1;
    else
      ix1 = SOURCE/abs(Scalco)/dx+1;
    end

    if itr == 1
      itr=itr+1;
      ntr=ntr+1;
      ix2=iReceiver;
      ix3=iReceiver;
      ix1same = ix1;
    else
      if ix1 == ix1same;
        itr=itr+1;
        ntr=ntr+1;
        if(ix2>iReceiver) ix2=iReceiver; end
        if(ix3<iReceiver) ix3=iReceiver; end
      else
        itr=1;
        ix1=ix1same;
        break;
      end
    end

    fseek(fidr,position+240,-1);
    tracedata=fread(fidr,ntold,'float32');
    Data(:,iReceiver)=tracedata;
  end

  icount=icount+1;

if icount>=ixshot_start
  %for each shot-gather: Get the coordinate of receiver
  ix2=ix2-lpad;
  ix3=ix3+rpad;

  if(ix1>=ix3) ix3=ix1;end
  if(ix1<=ix2) ix2=ix1;end

  if(ix2<1) ix2=1;     end;
  if(ix3>nxo)ix3=nxo;  end;

  nx=ix3-ix2+1;

  disp(['Reading the ',num2str(icount),'th shot gather ...', 'trace number is ', num2str(nx)]);

  velpart=zeros(nz,nx);
  epsilonpart=zeros(nz,nx);
  deltapart=zeros(nz,nx);
  phipart=zeros(nz,nx);
  tr=zeros(ntold,nx);

  velpart(:,:)=vel(:,ix2:nx+ix2-1);
  epsilonpart(:,:)=epsilon(:,ix2:nx+ix2-1);
  deltapart(:,:)=delta(:,ix2:nx+ix2-1);
  phipart(:,:)=phi(:,ix2:nx+ix2-1);
  tr(:,:)=Data(:,ix2:nx+ix2-1);

  %check the stability condition and decide if the input data need to be
  %interpolation
  if (numEnsamble==0)
    vnmo=vel.*sqrt(1+2*epsilon);
    vnmomax=max(max(vnmo));
    dh=min(dx,dz);
    dtcon=sqrt(2)*dh/(pi*vnmomax);
    clear vnmo;
  end

  if(dtold>dtcon)
    mt=round(dtold/dtcon)+1;
    dtnew= dtold/mt;
    [m,n]=size(tr);
    for ix=1:n
      trnew(:,ix)=resample(tr(:,ix),mt,1);
    end
    clear tr;
    tr = trnew;
    clear trnew;
    nt = ntold*mt;
    dt = dtnew;

  else
    dt = dtold;
    nt = ntold;
  end

  ixs=ix1-ix2+1;

  phi=-phi*pi/180;

  %define the axes
  x=(0:nx-1)*dx;
  z=(0:nz-1)*dz;

  %define source wavelet
  wavelet=zeros(nt,1);
  [wavelet1,tw]=rickerw(dt,fpeak);
  [m,n]=size(tw);
  wavelet(1:n)=wavelet1;
  clear wavelet1;

  %Initial wavefield
  unow=zeros(nz,nx);             %Initializing current wavefield
  uthen=zeros(nz,nx);            %Initialzing previous wavefield
  cresult=zeros(nz,nx);          %Initialzing the migration result
  factor=(dt.*velpart).^2;

  fidwavefield=fopen('wavefield.bin','w');

  for i=1:nx
    kx(i)=(2*pi/(nx*dx-dx)*(i-nx/2-1));
  end

  for i=1:nz
    kz(i)=(2*pi/(nz*dz-dz)*(i-nz/2-1));
  end

  tempA=(deltapart.*power(sin(2*phipart),2)/2+2*epsilonpart.*power(cos(phipart),4));
  tempB=(deltapart.*power(sin(2*phipart),2)/2+2*epsilonpart.*power(sin(phipart),4));
  tempC=((-deltapart+3*epsilonpart).*power(sin(2*phipart),2)+2*deltapart.*power(cos(2*phipart),2));
  tempD=(deltapart.*sin(4*phipart)-4*epsilonpart.*sin(2*phipart).*cos(phipart).*cos(phipart));
  tempE =(-deltapart.*sin(4*phipart)-4*epsilonpart.*sin(2*phipart).*sin(phipart).*sin(phipart));

  %wavenumber calculation
  for i=1:nz
    for j=1:nx
      if((power(kx(j),2)+power(kz(i),2))~=0)
        spec(i,j)=power(kx(j),2)+power(kz(i),2);
        spectempA(i,j)=power(kx(j),4)/(kx(j)*kx(j)+kz(i)*kz(i));
        spectempB(i,j)=power(kz(i),4)/(kx(j)*kx(j)+kz(i)*kz(i));
        spectempC(i,j)=power(kx(j)*kz(i),2)/(kx(j)*kx(j)+kz(i)*kz(i));
        spectempD(i,j)=power(kx(j),3)*kz(i)/(kx(j)*kx(j)+kz(i)*kz(i));
        spectempE(i,j)=power(kz(i),3)*kx(j)/(kx(j)*kx(j)+kz(i)*kz(i));
      end
    end
  end

  %Implement the modeling
  for k=1:nt
   if(mod(k,500)==0)
      sprintf('Modeling:step=%d/%d',k,nt)
    end
    unow(izs,ixs)=unow(izs,ixs)+wavelet(k);
    specxz=fft2(unow);

    %Shift to the center
    specxzshift=fftshift(specxz);

    %wavenumber shift
    specd2ud=-specxzshift.*spec;
    specd2udA=-specxzshift.*spectempA;
    specd2udB=-specxzshift.*spectempB;
    specd2udC=-specxzshift.*spectempC;
    specd2udD=-specxzshift.*spectempD;
    specd2udE=-specxzshift.*spectempE;

    sum = ifft2(ifftshift(specd2ud));
    sumA= ifft2(ifftshift(specd2udA)).*tempA;
    sumB= ifft2(ifftshift(specd2udB)).*tempB;
    sumC= ifft2(ifftshift(specd2udC)).*tempC;
    sumD= ifft2(ifftshift(specd2udD)).*tempD;
    sumE= ifft2(ifftshift(specd2udE)).*tempE;

    %Computation of wavefield for current time step
    utemp=2*unow+factor.*(real(sum+sumA+sumB+sumC+sumD+sumE))-uthen;
    uthen=unow;
    unow(:,:)=utemp;

    %write the forward wavefield
    if(mod(k,2*mt)==0)
      fwrite(fidwavefield,unow,'float');
    end

    %apply the sponge boundary condition
    [unow,uthen]=spongeABC(unow,uthen,nx,nz,nxsponge,nzsponge,epow);

  end

  fclose(fidwavefield);

  unow=zeros(nz,nx);        %Initializing current wavefield
  uthen=zeros(nz,nx);       %Initialzing previous wavefield

  fid=fopen('wavefield.bin','r');

  %the storage snapshot for imaging.
  ntimg=floor(nt/(2*mt));
  %count number for cross-correlating.
  kk=1;

  %Implementation of wavefield backpropagation
  for k=1:nt
    if(mod(k,500)==0)
      sprintf('Migration:step=%d/%d',k,nt)
    end;

    unow(izs,:) = tr(nt-k+1,:);

    specxz=fft2(unow);

    %Shift to the center
    specxzshift=fftshift(specxz);

    specd2ud=-specxzshift.*spec;
    specd2udA=-specxzshift.*spectempA;
    specd2udB=-specxzshift.*spectempB;
    specd2udC=-specxzshift.*spectempC;
    specd2udD=-specxzshift.*spectempD;
    specd2udE=-specxzshift.*spectempE;

    sum = ifft2(ifftshift(specd2ud));
    sumA =ifft2(ifftshift(specd2udA)).*tempA;
    sumB= ifft2(ifftshift(specd2udB)).*tempB;
    sumC= ifft2(ifftshift(specd2udC)).*tempC;
    sumD= ifft2(ifftshift(specd2udD)).*tempD;
    sumE= ifft2(ifftshift(specd2udE)).*tempE;

    %Computation of wavefield for current time step
    utemp=2*unow+factor.*(real(sum+sumA+sumB+sumC+sumD+sumE))-uthen;

    if(mod(k,2*mt)==0)
      fseek(fid,nx*nz*4*(ntimg-kk),-1);
      kk=kk+1;
      uread=fread(fid,[nz,nx],'float');
      cresult=cresult+utemp.*uread;
    end

    uthen=unow;
    unow(:,:)=utemp;

    %apply the boundary condition
    [unow,uthen]=spongeABC(unow,uthen,nx,nz,nxsponge,nzsponge,epow);

  end

  fclose(fid);

  numEnsamble=numEnsamble+1;

  %sum each migration shots together
  for ix=1:nx
    finalcresult(:,ix+ix2-1)=finalcresult(:,ix+ix2-1)+cresult(:,ix);
  end

 clear spec spectempA spectempB spectempC spectempD spectempE;
 clear tempA tempB tempC tempD tempE;
 clear specxz specxzshift specd2ud specd2udA specd2udB specd2udC specd2udD specd2udE sum sumA sumB sumC sumD sumE utemp;

 %save the final migration image
 fid=fopen(migfile,'w');fwrite(fid,finalcresult(21:nz,:),'float');fclose(fid);
 timecost=toc;
 disp(['the ', num2str(numEnsamble), 'th shot: migration time cost is  ',num2str(timecost), '(s)']);
 end
end

disp(['There are ',num2str(numEnsamble),' migration shots(s) in this file.']);