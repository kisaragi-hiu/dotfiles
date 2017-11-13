function paste-file
    for i in (paste)
        cp -r (echo $i | sed s~file://~~g) ./
    end
end

