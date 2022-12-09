import sys
file_name = "entanglement_swapping.toml"

def generate(file_name):
    with open(file_name, "w") as f:
        num_node = int(sys.argv[1])
        distance = [1]*num_node
        f.write(f"distance = {distance}")

generate(file_name=file_name)