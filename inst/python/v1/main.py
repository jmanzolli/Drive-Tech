from inst.python.functions import *

data = read_file("inst/input.xlsx")
model = setModel(data,time_limit=60,mipgap=0,solver='gurobi',status=True)
save(model)
