def read_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    return [int(char) for char in lines[0].strip()]

def convert_to_memory_blocks(input):
    memory_blocks = []
    current_index = 0
    for index, blocks_count in enumerate(input):
        if index % 2 == 0:
            memory_blocks.extend([current_index] * blocks_count)
            current_index+=1
        else:
            memory_blocks.extend(['.'] * blocks_count)
    return memory_blocks

def move_blocks(memory_blocks):
    file_ids = sorted(set([block for block in memory_blocks if block != '.']), reverse=True)

    for file_id in file_ids:
        file_indices = [i for i, block in enumerate(memory_blocks) if block == file_id]
        file_size = len(file_indices)

        if file_size == 0:
            continue

        leftmost_position = None
        for start in range(len(memory_blocks) - file_size + 1):
            if all(memory_blocks[i] == '.' for i in range(start, start + file_size)):
                leftmost_position = start
                break

        if leftmost_position is not None and leftmost_position < file_indices[0]:
            for i in file_indices:
                memory_blocks[i] = '.'

            for i in range(file_size):
                memory_blocks[leftmost_position + i] = file_id

    return memory_blocks

def print_memory_blocks(memory_blocks):
    print(''.join([str(i) for i in memory_blocks]))
             
def checksum(memory_blocks):
    result_sum = 0
    for index, block in enumerate(memory_blocks):
        if block != '.':
            result_sum+=(index*block)
    return result_sum

if __name__ == "__main__":
    input = read_input('input.txt')
    memory_blocks = convert_to_memory_blocks(input)
    moved_blocks = move_blocks(memory_blocks)
    print(checksum(moved_blocks))