function mdata=gs_zero_mig(fdata,f,parms,dx,dz)
%mdata=gs_zero_mig(fdata,f,parms,dx,dz);
%
%***generalized screen zero-offset migration***
%
%To see an example, type 'gs_salt_psdm_script' in matlab.
%
%Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
%Planned seismic imaging using explicit one-way operators, Geophysics, V70,
%NO5, S101 - S109' for details.
%
%mdata...depth migrated output.
%fdata...f-kx spectrum of zero-offset 'fdata=ifft(fft(data),[],2);'.
%        NOTE f axis is band-limited and +ve only.
%        NOTE kx axis is uncentred.
%f...frequency axis (Hz, must correspond to fdata).
%parms...velocity model*1/2 (m/s).
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
temp=zeros(rd,cd);
for j=1:Nz-1
	disp([' gs zero mig working on depth ',num2str(j),' of ',num2str(Nz)])
	temp=gs_ips(fdata,f,dx,parms(j,:),dz);
	fdata=ips(temp,f,dx,min(parms(j,:)),dz);
	temp=ips(fdata,f,dx,min(parms(j,:)),-dz);
	fdata=fft(temp,[],2);
	mdata(j+1,:)=real(sum(fdata)+sum(fdata(1:rd-1,:)))/(2*rd-1)/2/pi;
	fdata=temp;
end