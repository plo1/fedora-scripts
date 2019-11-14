function startup(path)
    if nargin == 0
        return
    elseif isfile(path)
        [filepath, name, ext] = fileparts(path);
        cd(filepath)
        edit(strcat(name, ext))
    elseif isfolder(path)
        cd(path)
    else
        fprintf('recieved invalid arg in starttask function\n')
    end
end
