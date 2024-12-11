function split_number(num)
    local num_str = tostring(num)
    local mid = math.floor(#num_str / 2)
    local left = tonumber(num_str:sub(1, mid))
    local right = tonumber(num_str:sub(mid + 1))
    return left, right
end

function count_stones_efficiently(initial_stones, n_blinks)
    local stone_counts = {}

    for _, stone in ipairs(initial_stones) do
        stone_counts[stone] = (stone_counts[stone] or 0) + 1
    end

    for _ = 1, n_blinks do
        local new_stone_counts = {}

        for stone, count in pairs(stone_counts) do
            if stone == 0 then
                -- Rule 1: Turn 0 into 1
                new_stone_counts[1] = (new_stone_counts[1] or 0) + count
            elseif #tostring(stone) % 2 == 0 then
                -- Rule 2: Split even-digit stone into two stones
                local left, right = split_number(stone)
                new_stone_counts[left] = (new_stone_counts[left] or 0) + count
                new_stone_counts[right] = (new_stone_counts[right] or 0) + count
            else
                -- Rule 3: Multiply stone by 2024
                local new_value = stone * 2024
                new_stone_counts[new_value] = (new_stone_counts[new_value] or 0) + count
            end
        end

        stone_counts = new_stone_counts
    end

    local total_stones = 0
    for _, count in pairs(stone_counts) do
        total_stones = total_stones + count
    end

    return total_stones
end

local function main()
    local input_file = io.open("input.txt", "r")
    local initial_stones = {}

    if input_file then
        local line = input_file:read("*line")
        for stone in line:gmatch("%d+") do
            table.insert(initial_stones, tonumber(stone))
        end
        input_file:close()
    else
        error("Unable to open input file.")
    end

    print("Part 1: ", count_stones_efficiently(initial_stones, 25))
    print("Part 2: ", count_stones_efficiently(initial_stones, 75))
end

main()
