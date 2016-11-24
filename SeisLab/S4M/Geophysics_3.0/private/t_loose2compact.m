function table=t_loose2compact(table)
% Convert table from loose to compact format.
%
% Written by: E. Rietsch: February 16, 2004
% Last updated: February 7, 2008: Handle vectors of tables
%
%         table=t_loose2compact(table);
% INPUT
% table   table or vector of tables in loose format
% OUTPUT
% table   table or vector of tables in compact format

% UPDATE HISTORY
%         February 14, 2007: Adapt to R14


global S4M

if ~istype(table(1),'table')
   error('Input argument is not a table.')
end

if strcmp(table(1).format,'compact')
   return
end

fields=table(1).column_info(:,1);
if ~S4M.case_sensitive
   fields=lower(fields);
end

nfields=length(fields);
[table.columns]=deal([]);

for jj=1:length(table)
   [nrows,ncols]=tablesize(table(jj));
   matrix=zeros([nrows,ncols]);
   for ii=1:nfields
      matrix(:,ii)=table.(fields{ii});
   end
   table(jj).columns=matrix;
end

table=rmfield(table,fields);
[table.format]=deal('compact');
