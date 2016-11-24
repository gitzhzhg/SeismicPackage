function vspout=VSP_medfilt(vspin,nfilt)
% VSP_medfilt: apply spatial median filtering to a VSP
%
% vspout=VSP_medfilt(vspin,nfilt)
%
% This is used in wavefield separation. After flattening the VSP on the
% downgoing wave, a median filter is applied along the spatial axis which
% surpresses the upgoing wave.
%
% vspin ... input vsp flattenedon the downgoing wave
% nfilt ... number of points in the median filter. It should be an odd
%           number. If even it will be increased by 1.
%
% vspout ... median filtered result
%
%
% G.F. Margrave, 2014, CREWES
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

%ensure odd length filter
n2=2*floor(nfilt/2);
if(n2==nfilt)
    nfilt=nfilt+1;
end

vspout=zeros(size(vspin));
for k=1:size(vspin,1)
    vspout(k,:)=medfilt(vspin(k,:),nfilt);
end