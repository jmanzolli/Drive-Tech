from inst.python.functions import *

def run_gurobi(data, log_file=None):
    try:
        # data = read_file(file)
        model = setModel(data,time_limit=60,mipgap=0,solver='gurobi',log_file=log_file, status=True)
        return result(model)
    except Exception as e:
        print(e)
        return False