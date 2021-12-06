function [OptString,Valid] = ReadOptionXTT(FileName)
%

OptString{1} = 'H(1)O(?)';
OptString{2} = '*  ';

OptString{3} = '500';
OptString{4} = '10000';

OptString{5} = '100';
OptString{6} = '40';
OptString{7} = '8,500';
OptString{8} = '2';

OptString{9} = '300:10:700';
OptString{10} = '2000:500:25000';

Valid = 0;

if exist([cd,'/',FileName]);
    Valid = 1;
    fid = fopen([cd,'/',FileName]);
    while 1
        TheL = fgetl(fid);

        if isequal(TheL,-1);
            break
        end

        if length(TheL)
            if isequal(TheL(1),'>')
                TheStr = strread(TheL(2:end),'%s');
                switch str2double(TheStr{1})
                    case 1
                        OptString{1} = fgetl(fid);
                        OptString{2} = fgetl(fid);
                    case 2
                        OptString{3} = fgetl(fid);
                        OptString{4} = fgetl(fid);
                    case 3
                        OptString{5} = fgetl(fid);
                        OptString{6} = fgetl(fid);
                        OptString{7} = fgetl(fid);
                        OptString{8} = fgetl(fid);
                    case 4
                        OptString{9} = fgetl(fid);
                        OptString{10} = fgetl(fid);

                end
            end
        end

    end
    fclose(fid);
end
return

