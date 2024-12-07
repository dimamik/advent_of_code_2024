open("input.txt") do f
    total = 0
    for line in eachline(f)
        line = strip(line)
        if isempty(line)
            continue
        end
        parts = split(line, ":")
        target = parse(Int, strip(parts[1]))
        nums = parse.(Int, split(strip(parts[2])))

        n = length(nums)
        if n == 1
            if nums[1] == target
                total += target
            end
        else
            found = false
            for ops_mask in 0:(2^(n-1)-1)
                val = nums[1]
                for i in 2:n
                    if ((ops_mask >> (i - 2)) & 1) == 1
                        val = val * nums[i]
                    else
                        val = val + nums[i]
                    end
                end
                if val == target
                    found = true
                    break
                end
            end
            if found
                total += target
            end
        end
    end
    println(total)
end
