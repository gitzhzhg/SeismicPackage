function [jfr,iweX,isfZ].....
    = fdInit5Trace(iTrAcq,trX,trZ,Dxz,ix1,iz1)
% function [jfr,iweX,isfZ].....
%     = fdInit5Trace(iTrAcq,trX,trZ,Dxz,ix1,iz1)
%Set up for output of surface acquired traces at each time step
%The input parameters are
%iTrAcq  .... Not used (indicator of structured surface)
%trX     .... X-position of the 'well' where trace (time) data will be collected
%trZ     .... Z-depth of the 'line' where trace (time) data will be collected
%Dxz     .... FD sample rate in (metres)
%ix1   ...... X co-ordinate of start of model in arrays
%iz1   ...... Z co-ordinate of start of model in arrays
%The output parameters are
%jfr    ..... The index of the next frame (for movies)
%isfZ    .... From trZ (if no structured surface)
%iweX    .... From trX (if no structured surface)
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

% isurf = 0;
jfr = 0;
% disp(traceFile)
% if ~isempty(traceFile)
%     isurf = round(traceZlevel/Dxz)+iz1;
%     disp(isurf)
%     surfUx = zeros(nxf,nstep);
%     surfUz = zeros(nxf,nstep);
%     surfT = zeros(1,nstep);
%     surfS =  [0 0 shotDepth];
%     surfX = (0:nxf-1)*Dxz;
%     surfZ = ones(1,nxf)*traceZlevel;
% end
if ~isempty(iTrAcq)
    isfZ = round(trZ/Dxz)+iz1;    %Modify if structured surface
end
%     if strcmpi(iTrAcq,'x')
%         nf = nxf;
%         else if strcmpi(iTrAcq,'z')
%                 nf = nzf;
%              end
%     end
if trX > 0
    iweX = round(trX/Dxz)+ix1;
end
% surfUx = zeros(nxf,nstep);
% surfUz = zeros(nxf,nstep);
% wellUx = zeros(nzf,nstep);
% wellUz = zeros(nzf,nstep);
% surfUx = single(zeros(nxf,nstep));
% surfUz = single(zeros(nxf,nstep));
% wellUx = single(zeros(nzf,nstep));
% wellUz = single(zeros(nzf,nstep));
% end