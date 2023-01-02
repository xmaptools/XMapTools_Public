function ProjectName = CleanProjectName(ProjectName)
%

for i = 1:length(ProjectName)
    if isequal(ProjectName(i),':')
        ProjectName(i) = '-';
    end
end

return
