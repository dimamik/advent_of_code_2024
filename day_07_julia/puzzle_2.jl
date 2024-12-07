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
            ops_count = 3^(n - 1)
            for ops_mask in 0:(ops_count-1)
                val = nums[1]
                mask = ops_mask
                valid = true
                for i in 2:n
                    op = mask % 3
                    mask ÷= 3
                    if op == 0
                        val = val + nums[i]
                    elseif op == 1
                        val = val * nums[i]
                    else
                        val = parse(Int, string(val, nums[i]))
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
