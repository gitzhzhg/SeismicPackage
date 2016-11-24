function SEGY_WriteShotGathers(fileout,shots,time,xyzshots,xyzrecs)
%function SEGY_WriteShotGathers(fileout,gathers,time,xyzshots,xyzrecs)
% Where:
%    fileout = character string containing filename to output segy to
%      shots = cell array of 2D matrices of shot gather data
%       time = either the sample rate or a time vector
%              (optional) default value is 1 ms
%   xyzshots = shot geometry matrix where row 1 is x, row 2 is y and row 3
%              is z; columns correspond to shots.
%              (optional) default values are 0.0
%   xyzrecs  = receiver geometry cell array where each cell contains a 2D
%              matrix of receiver coordinated for each shot. row 1 is x,
%              row 2 is y and row 3 is z.
%              (optional) default values are 0.0;
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

%

try

%% check number of input arguments
switch(nargin)
    case 0
      %disp('Only have a filename')
      throw(MException('CREWES:SEGY_WriteShotGathers',...
      ['Minimum number of inputs is 2: '...
       'eg. SEGY_WriteShotGathers(fileout,shots)']));    
    case 1
      %disp('Only have a filename')
      throw(MException('CREWES:SEGY_WriteShotGathers',...
      ['Minimum number of inputs is 2: '...
       'eg. SEGY_WriteShotGathers(fileout,shots)']));
    case 2
      %disp('Only have a filename and shots')
      time = 1;
      xyzshots = [];
      xyzrecs  = [];
    case 3
      %disp('Only have a filename, shots and time')
      xyzshots = [];
      xyzrecs  = [];      
    case 4
      %disp('Only have a filename, shots, time and xyzshots')
      xyzrecs  = [];       
    case 5
      %disp('Have all five possible inputs')
    otherwise
      throw(MException('CREWES:SEGY_WriteShotGathers',...
      ['Maximum number of inputs is 5: '...
       'eg. SEGY_WriteShotGathers(fileout,shots,time,xyzshots,xyzrecs)']));       
end
                
%% check fileout input variable
%check that fileout is a filename rather than a fid
if ~ischar(fileout)
    throw(MException('CREWES:SEGY_WriteShotGathers',...
        'fileout must be a character string'));
end

%check file extension and add .sgy if not already present
if ~strncmpi(fliplr(fileout),'ygs.',4) || ...
        strncmpi(fliplr(fileout),'yges.',5)
    fileout = [fileout '.sgy'];
end

%% check shots input variable
%make certain shots is a cell array
if ~iscell(shots)
    shots = {shots}; %assume matrix
end

%get number of traces per shot and number of samples per shot
nshots         = numel(shots);
size_shots     = cellfun(@size, shots, 'UniformOutput', false);
nsamp_per_shot = cellfun(@(x) x(1), size_shots);
ntr_per_shot   = cellfun(@(x) x(2), size_shots);

%check number of samples per trace to make sure all traces are the same length
if sum(diff(nsamp_per_shot)) ~= 0
    throw(MException('CREWES:SEGY_WriteShotGathers',...
          'Cowardly refusing to output variable trace length SEG-Y'));
end

%% check time input variable
%get sample rate
if iscell(time)
    time = time{:};
end


%% check xyzshots input variable
if ~isempty(xyzshots)
    
    %make certain xyzshots is a matrix
    if iscell(xyzshots) %xyzshots was input as a cell array, try to recover
        xyzshots = cell2mat(xyzshots);
    end
    
    %check number of gathers in xyzshots geometry
    [m, n] = size(xyzshots);
    if nshots ~= n
        throw(MException('CREWES:SEGY_WriteShotGathers',...
            'Number of gathers in xyzshots does not equal the number of gathers in shots'));
    end
    
    %pad zeros if sy and/or sz are missing
    switch (m)
        case 1
            xyzshots(2:3,:) = zeros(2,n);
        case 2
            xyzshots(3,:) = zeros(1,n);
        case 3
            %         disp('Alls''s well, continuing');
        otherwise
            throw(MException('CREWES:SEGY_WriteShotGathers',...
                'xyzshots must contain just 1 (x) , 2 (xy), or 3 (xyz) rows'));
    end
    
end
%% check xyzrecs input variable
if ~isempty(xyzrecs)
    %make certain xyzrecs is a cell array
    if ~iscell(xyzrecs) %xyzrecs was input as a matrix, try to recover
        %(this only works for a single gather! otherwise will fail at next exception)
        xyzrecs = {xyzrecs};
    end
    
    %check number of gathers in xyzrecs geometry
    [m, n] = size(xyzrecs);
    if nshots ~= n
        throw(MException('CREWES:SEGY_WriteShotGathers',...
            'Number of gathers in xyzrecs does not equal the number of gathers in shots'));
    end
    
    %check number of traces per gather in xyzrecs geometry
    size_xyzrecs    = cellfun(@size,xyzrecs(1,:),'UniformOutput',false);
    ntr_per_shotrec = cellfun(@(x) x(2), size_xyzrecs);
    
    if sum(ntr_per_shot -ntr_per_shotrec) ~= 0
        throw(MException('CREWES:SEGY_WriteShotGathers',...
            'number of traces in xyzrecs does not match number of traces in shots'));
    end
    
    %pad zeros if ry and/or rz are missing
    for k = 1:nshots
        switch (m)
            case 1
                xyzrecs(2:3,k) = {zeros(1,ntr_per_shot(k))};
            case 2
                xyzrecs(3,k) = {zeros(1,ntr_per_shot(k))};
            case 3
                %         disp('Alls''s well, continuing');
            otherwise
                throw(MException('CREWES:SEGY_WriteShotGathers',...
                    'xyzrecs must contain just 1 (x) , 2 (xy), or 3 (xyz) rows'));
        end
    end
    
end
%% write SEGY to fileout

%create text header
% [texthead,binaryhead]=AFD_makeSEGYheaders(dt,ns,numtraces,mfeet,fold)
[texthead,binaryhead] = ...
    AFD_makeSEGYheaders(time,nsamp_per_shot,max(ntr_per_shot),1,1);

%write text and binary headers to fileout
%function SEGY_writeHeaders(fileout,texthead,binaryhead,extendedhead,permission)
SEGY_writeHeaders(fileout,texthead,binaryhead);

%write gathers to fileout
numcurtraces=0;
numcurshots=1;

for k = 1:nshots
    if isempty(xyzshots)
      sx = zeros(1,ntr_per_shot(k));
      sy = sx;
      sz = sx;
    else
      sx = xyzshots(1,k) *ones(1,ntr_per_shot(k));
      sy = xyzshots(2,k) *ones(1,ntr_per_shot(k));
      sz = xyzshots(3,k) *ones(1,ntr_per_shot(k));
    end
    if isempty(xyzrecs)
      rx = zeros(1,ntr_per_shot(k));
      ry = rx;
      rz = rx;
    else
      rx = xyzrecs{1,k};
      ry = xyzrecs{2,k};
      rz = xyzrecs{3,k};
    end
    
    %calculate absolute value of source-receiver offset
    dx = xyzshots(1,k)-xyzrecs{1,k};
    dy = xyzshots(2,k)-xyzrecs{2,k};    
    sro=sqrt(dx.^2 +dy.^2);
    
    tracehead = AFD_makeSEGYtraceheaders(rx,ry,time,sx,sy);

    %for some reason AFD_makeSEGYtraceheaders does not set z, ffid or
    %source-receiver offset
    tracehead(3,:)  = k *ones(1,ntr_per_shot(k)); %ffid == seq. shot #
    tracehead(12,:) = sro;
    tracehead(13,:) = rz; 
    tracehead(14,:) = sz;
    
    SEGY_writeTraces(fileout,{tracehead,shots{k}},numcurtraces,numcurshots);
    
    [~, n] = size(shots{k});
    numcurtraces = numcurtraces +n;
    numcurshots  = numcurshots  +k;
end
    

catch ME
    disp([ME.identifier '; ' ME.message])
end
    

end %end SEGY_WriteShotGathers