function mdata=pspi_zero_mig(fdata,f,parms,pspi_parms,dx,dz)
%mdata=pspi_zero_mig(fdata,f,parms,pspi_parms,dx,dz)
%
%***pspi zero-offset migration***
%
%To see an example, type 'pspi_salt_zero_script' in matlab.
%
%mdata...depth migrated output.
%fdata...f-kx spectrum of zero-offset 'fdata=ifft(fft(data),[],2);'.
%        NOTE f axis is band-limited and +ve only.
%        NOTE kx axis is uncentred.
%f...frequency axis (Hz, must correspond to fdata).
%parms...velocity model*1/2 (m/s).
%pspi_parms...blocky version of parms.
%dx...trace spacing (m).
%dz...depth interval (m).
%
%R. J. Ferguson, 2009
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

%***get sizes of things***
[Nz,Nx]=size(parms);
[rd,cd]=size(fdata);
[rf,cf]=size(f(:));
%*************************

%***Err check***
if rf~=rd; error('freq axis wrong');end
if Nx~=cd; error('x axis wrong');end
%***************

mdata=zeros(Nz,cd);
dtemp=zeros(rd,cd);
for j=1:Nz-1
	disp([' pspi zero mig working on depth ',num2str(j),' of ',num2str(Nz)])
	dtemp=pspi_ips(fdata,f,dx,parms(j,:),pspi_parms(j,:),dz);
	mdata(j+1,:)=real(sum(dtemp)+sum(dtemp(1:rd-1,:)))/(2*rd-1)/2/pi;
	fdata=ifft(dtemp,[],2);
end