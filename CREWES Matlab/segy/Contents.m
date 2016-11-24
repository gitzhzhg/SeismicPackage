% CREWES SEGY toolbox
% Tools to perform SEGY I/O
%
% Please bear with us. You can do most things with the tools here but some
% tasks are very easy while others are quite difficult. If you are just
% reading and writing a small 2D dataset, consider using the 'alt' tools.
% If you need more flexibility and need to interrogate and/or update
% headers, then you will need to use the other tools. A complete re-write
% of this facility is in progress (summer 2016).
%
% Basic tools:
% The alt versions are easiest to use but the ability to use and
% interrogate headers is limited. The non-alt versions are more general but
% require greater effort. By default these tools read your entire dataset
% into a 2D matrix. If that is not what you want then you will have to dig
% deeper. See the help for SEGY_toolbox and the CREWES research report "New
% MATLAB functions for reading, writing and modifying SEG-Y files" by
% Lloyd, Hall, and Margrave, 2010 at
% http://www.crewes.org/ForOurSponsors/ResearchReports/reports.php?year=2010
%
% altreadsegy ... Read SEGY files, easy but headers not very useful
% altwritesegy ... Write SEGY files, easy but you might lose your headers
% readsegy ... a wrapper around SEGY_read that also gets the time sample size
% SEGY_read ... Read SEGY files, create trace objects and header objects
% SEGY_write ... Write SEGY files, write trace objects and header objects
% SEGY_getHeader ... use this to interrogate the header object from readsegy.
% SEGY_setHeader ... use this to update the header object from readsegy
% traceheaderdump ... useful utility to see what is in the trace header object.
% traceheaderdump_g ... GUI interface to traceheaderdump. Allows plotting of header values.
%
% Example: 
% [traces,dt,texthead,binaryhead,extendedhead]=readsegy('myfile.sgy');
% dt=dt/1000;
% t=(0:size(traces.tracedata.data,1)-1)*dt;
% ilineall=SEGY_getHeader(traces.traceheader,'iline');%get the inline numbers
% xlineall=SEGY_getHeader(traces.traceheader,'xline');%get the xline numbers
% xcdpall=SEGY_getHeader(traces.traceheader,'cdpx');%get cdpx
% ycdpall=SEGY_getHeader(traces.traceheader,'cdpy');%get cdpy
% seis=traces.tracedata.data;% this is your seismic data in a big matrix
% plotimage(seis,t);%examine your data
%
% If your data is 3D, consider using make3Dvol to form it into a 3D matrix
% and plotimage3D to view it.
% Example (following after the above)
% [seis3DB,xline,iline,xcdp,ycdp]=make3Dvol(traces.tracedata.data,xlineall,ilineall,xcdpall,ycdpall);
% plotimage3D(seis3DB
%
% ******************* Warning: HERE BE DRAGONS *********************
% TRY NOT TO USE THE REST OF THIS STUFF UNTIL THE CURRENT REWRITE COMPLETES
% These things usually work but there can be issues, especially with old
% SEGY files.
%
% SEGY Toolbox ... a collection of higher level tools, 
%       type: >> help segy_toolbox 
%       for more information
%
% SEGY_* A set of functions for manipulating rev 1 (2002) SEGY data.
% 
% Written mostly by Chad Hogan, Kevin Hall, and Heather Lloyd
%
% SEGY_OpenFile ... Opens a SEGY formatted disk file, MUST BE USED!
% ^^^^^^ USE THIS FIRST ^^^^^^
%
% SEGY_FindCMPs ... Finds all CMP locations in a SEGY file
% SEGY_FindIndex ... Finds all [trace header value] in a SEGY file
% SEGY_FindShots ... Finds all shot locations in a SEGY file
% SEGY_ModifyTextHeaderLine .. Changes a text header line in-place
% SEGY_ReadBinaryHeader ... Reads the binary header from a SEGY file
% SEGY_ReadCMPGather ... Reads a CMP gather from a SEGY file
% SEGY_ReadShotGather ... Reads a shot gather from a SEGY file
% SEGY_ReadTextHeader ... Reads a text header from a SEGY file
% SEGY_ReadTrace ... Reads a trace from a SEGY file
% SEGY_ReleaseFile ... Closes a SEGY file after you're done with it
% SEGY_ReplaceShotGather ... Replaces a shot gather in a SEGY file
% SEGY_ReplaceTrace ... Replace a single trace in a SEGY file
%
% FUNCTIONS DESIGNED TO CREATE A NEW SEGY FORMATTED FILE
%
% SEGY_GetBinaryHeader ... Returns a binary header template
% SEGY_GetTextHeader ... Returns a text header template
% SEGY_GetTrace ... Returns a complete trace template, header and data.
%
% SEGY_WriteTextHeader ... Write a text header into a SEGY file
% SEGY_WriteBinaryHeader ... Writes a binary header into a SEGY file
% SEGY_WriteTrace ... Appends a trace (header and data) onto a SEGY file
% SEGY_WriteGathers ... example code
% SEGY_WriteStack ... example code
%
% UTILITY FUNCTIONS YOU PROBABLY WON'T USE MUCH
%
% SEGY_TraceSeek ... Seeks the file pointer to a trace in a SEGY file
%
% $Id: Contents.m,v 1.4 2008/03/05 00:48:00 cmhogan Exp $