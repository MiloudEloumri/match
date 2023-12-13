import os
import rdflib
from rdflib import Graph
import glob

input_dir = "C:\\Users\\milou\\Mirror\\Documents\\me\\4-Protege\\protege-win\\my-ontologies\\"
output_dir = "C:\\Users\\milou\\match\\apps\\job_match\\priv\\data\\"

output_formats = {
    "1": {"format": "xml", "extension": ".rdf"},
    "2": {"format": "json-ld", "extension": ".jsonld"},
    "3": {"format": "turtle", "extension": ".ttl"},
    "4": {"format": "n3", "extension": ".n3"},
    "5": {"format": "trig", "extension": ".trig"},
    "6": {"format": "trix", "extension": ".trix"},
    "7": {"format": "ntriples", "extension": ".nt"},
    "8": {"format": "nquads", "extension": ".nq"}
}

print("Select output format:")
for key, value in output_formats.items():
    print(key + ": " + value["format"])
selected_format = input("Enter the number of the desired output format: ")

overwrite = input("Do you want to overwrite existing files? (yes/no): ")

if overwrite.lower() not in ["yes", "no"]:
    print("Invalid input. Please enter 'yes' or 'no'.")
    exit()

for file in glob.glob(input_dir + "*"):
    try:
        g = Graph()
        g.parse(file)
        format_ext = output_formats[selected_format]["extension"]
        format_name = output_formats[selected_format]["format"]

        output_file = output_dir + format_name + '-' + os.path.basename(file).split('.')[0] + format_ext

        if overwrite.lower() == "no":
            count = 2
            while os.path.isfile(output_file):
                output_file = output_dir + format_name + '-' + str(count) + '-' + os.path.basename(file).split('.')[0] + format_ext
                count += 1

        g.serialize(destination=output_file, format=format_name)
        print(f"The file {file} has been successfully converted to {format_name} format in {output_file}")

    except Exception as e:
        print(f"Error occurred while parsing the file {file}: {str(e)}")
