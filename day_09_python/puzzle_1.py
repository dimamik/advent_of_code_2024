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
    left_index = 0
    right_index = len(memory_blocks) - 1
    while True:
        if right_index == left_index:
            break
        right_el = memory_blocks[right_index]
        left_el = memory_blocks[left_index]
        if right_el != '.' and left_el == '.':
            memory_blocks[left_index] = right_el
            memory_blocks[right_index] = '.'
        if right_el == '.':
            right_index-=1
        if left_el != '.':
            left_index+=1
    return memory_blocks
             
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
    
