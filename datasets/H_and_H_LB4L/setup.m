function setup(varargin)

[p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
parts = strsplit(p.file_dir,'\');
p.root_dir = fullfile(parts{1:end-2});
path(path,genpath(p.root_dir));
rawdata = dataset('file',fullfile(p.file_dir,'group_means.csv'), ...
    'format','%s%s%s%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
rawdata = replaceWithMissing(rawdata,'Strings','NA');
rawdata.cues = ones(length(rawdata),1);
rawdata.cues(ismember(rawdata.timepoint,[1 2]) & strcmp(rawdata.other_type,'T')) = 2;
rawdata.other_type = [];
rawdata.timepoint(isnan(rawdata.acc)) = NaN;
end