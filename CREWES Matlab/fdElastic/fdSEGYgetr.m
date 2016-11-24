function fdSEGYgetr(modelDir,parmFile,Dt,Dxz,nShot,spMin,....
    tracesx,tracesz,dir,shotDepth,shotX)
% function fdSEGYgetr(parmFile,Dt,Dxz,nShot,spMin,....
%     tracesx,tracesz,dir,shotDepth,shotX)
%Build ascii files - passed to UNIX a2s for standardized SEGY output
%The input parameters are
%parmFile. Name of file containing finite-difference parameters,
%ending in .parm, put in SEGY file
%Dt   .... FD sample rate in seconds
%Dxz ..... Spatial sample rate
%nShot ... Shot number (now just 1)
%spMin ... The offset or depth of the first available trace
%tracesx . The array of X co-ordinates of the traces
%tracesz . The array of Z co-ordinates of the traces
%dir ..... The line direction of the data
%shotDepth Z (depth) of the FD initializing source
%shotX ... X (from the FD model left) of the initializing source
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

pathSGY = [modelDir,'\'];
[nSpf,nStep] = size(tracesx);
%Create .Ux .Uz .GRP_X or .GRP_Z files
%     nSs = size(surfUx);
%     nWs = size(wellUx);
%     nxf = nSs(1); nStep = nSs(2);
%     nzf = nWs(1);
    tMin = 0;
%     [indX,Xarr,indT,Tarr,DtPh,dir,SEGYfile] = ....
%         resampSEGY2(xMin,nxf,zMin,nzf,Dxz,tMin,Dt,nStep);
    [indX,Xarr,indT,Tarr,DtPh,SEGYfile] = ....
        resampSEGYg(spMin,nSpf,Dxz,tMin,Dt,nStep);
    nPh = length(Xarr);
    nTr = length(Tarr);
%     nTr = nxf;
%     if dir~='x'
%         nTr = nzf;
%     end
    Xdec = zeros(nPh,nSpf);
    %disp(size(Xdec))
    for idd = 1:nPh
        inx = indX(idd);
        %Xdec(idd,indX(idd)) = 1;
        Xdec(idd,inx) = 1;
        %if inx<=nPh
    end
    disp([num2str(nPh) ' phones specified'])
    %nTsamp = length(indT);
    Tdec = zeros(nStep,nTr);
    for idd = 1:nTr
        Tdec(indT(idd),idd) = 1;
    end
    horz = 1;
    textNo = num2str(nShot);
    disp([pathSGY SEGYfile textNo '   (** output pathSGY & name **)'])
    %Xdec (in particular) will require x and z versions for multiple shots
            %option
    if dir == 'x'
        angl = 'flat';
        %disp(size(Xdec));disp(size(surfUx));disp(size(Tdec));
        recUx = Xdec*tracesx*Tdec;
        eval(['save ' pathSGY SEGYfile textNo angl '.Ux  recUx -ascii'])  %*****
        recUz = Xdec*tracesz*Tdec;
        eval(['save ' pathSGY SEGYfile textNo angl '.Uz  recUz -ascii'])
        %Create .GRP_X file
        st = ['fid = fopen(''' pathSGY SEGYfile textNo angl '.GRP_X'',' '''wt'');'];
        eval(st)
        fprintf(fid, '%6d',Xarr);
        fclose(fid);
    end

    if dir ~= 'x'
        angl = 'vert';
        %disp(size(Xdec));disp(size(tracesx));disp(size(Tdec));
        recUx = Xdec*tracesx*Tdec;
        eval(['save ' pathSGY SEGYfile textNo angl '.Ux  recUx -ascii'])  %*****
        recUz = Xdec*tracesz*Tdec;
        %disp(size(recUx))
        eval(['save ' pathSGY SEGYfile textNo angl '.Uz  recUz -ascii'])
        %Create .GRP_Z file
        st = ['fid = fopen(''' pathSGY SEGYfile textNo angl '.GRP_Z'',' '''wt'');'];
        eval(st)
        fprintf(fid, '%6d',Xarr);
        fclose(fid);
    end
%Create .parm file
    st2 = ['fid = fopen(''' pathSGY SEGYfile textNo angl '.para'',' '''wt'');'];
    eval(st2)
    fprintf (fid, ['ASCIIDataInfile ', SEGYfile, textNo angl ,'.Uz\n']);
    fprintf (fid, ['SEGYOutfile ', SEGYfile, textNo angl ,'_Uz.sgy\n']);
    fprintf (fid, ['FileNameRoot ', SEGYfile, textNo angl,'\n']);
    fprintf (fid, ['TxtHdrFile ', SEGYfile, textNo angl ,'.c80\n']);
    fprintf (fid, 'FMT\t\tR\n');
    fprintf (fid, 'GatherType\t5\n');
    fprintf (fid, 'NtrPerGather\t%d\n', nPh);
    fprintf (fid, 'Nsamp\t\t%d\n', nStep);
    DtInt = round(DtPh*1000);
    fprintf (fid, 'dt\t\t%d\n', DtInt);
    fprintf (fid, 'unit\t\t1\n');
    fprintf (fid, 'Scaling\t\t1\nvertMultiplier\t1\n');
    fprintf (fid, 'horzMultiplier\t-%d\n',horz);
    fprintf (fid, 'Stn1\t%d\n', 0);
    fprintf (fid, 'dStn1\t%d\n', 0);
    fprintf (fid, 'dStn\t%d\n', 0);
    fprintf (fid, 'X1\t%d\n', 0);
    fprintf (fid, 'dX1\t%d\n', 1);
    fprintf (fid, 'dx\t%d\n', Dxz);
    fprintf (fid, 'FFID\t%d\n', 0);
    fprintf (fid, 'dFFID\t%d\n', 0);
    fprintf (fid, 'SP\t%d\n', nShot);
    fprintf (fid, 'dSP\t%d\n', 0);
    fprintf (fid, 'SP_x\t%d\n', spMin);
    fprintf (fid, 'dSP_x\t%d\n', 0);
    fprintf (fid, 'THV_SRC_DEPTH\t%d\n', shotDepth);
    fprintf (fid, 'THV_SRC_X\t%d\n', shotX);
    fprintf (fid, 'THV_SRC_Z\t%d\n', 0);
    fprintf (fid, 'CDP\t%d\n', 0);
    fprintf (fid, 'dCDP\t%d\n', 0);
    fprintf (fid, 'CDP_x\t%d\n', 0);
    fprintf (fid, 'dCDP_x\t%d\n', 0);
    fclose(fid);
%Create .c80 file
    %Build and output edcdic header,
    % directly from Matlab input parameter file
    %disp(parmFile)
    fidin = fopen(parmFile, 'r');
    st = ['fid = fopen(''' pathSGY SEGYfile textNo angl '.c80'',' '''wt'');'];
    eval(st)
    str = ['Listing of ' parmFile ' ' date];
    fprintf (fid, '%s\n', str);
    disp(str)
    iC = 1;
    for iCard = 1:37
        card = fgets(fidin,80);
        if ~ischar(card); break; end
        %fprintf (fid, '%s80\n', card);
        fprintf (fid, '%s', card);
        iC = iC+1;
    end
    card = blanks(80);
    for iCard = iC+1:40
        fprintf (fid, '%s', card);
    end
    
    fclose(fidin);
    fclose(fid);
% td = Dt*1000;
% tSAx = td:td:nstep*td;
figure
plotseis(recUx',Tarr,Xarr);
figure
plotseis(recUz',Tarr,Xarr);
%disp(size(recUz))
%disp('here')