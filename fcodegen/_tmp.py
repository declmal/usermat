import subprocess
from os import path
from ctypes import byref, cdll, c_double

object_dir = path.abspath(
    path.join("build", "codegen", "fortran"))

if __name__ == "__main__":
    source_path = path.abspath(
        path.join("codegen", "fortran", "test.f"))
    # compile to shared library
    source_file = path.basename(source_path)
    source_name, _ = path.splitext(source_file)
    object_file = source_name + ".so"
    object_path = path.join(object_dir, object_file)
    cmd = " ".join([
        "ifort -shared -fPIC -g -o",
        object_path, source_path])
    subprocess.check_output(cmd, shell=True)
    # get function pointer
    func_name = source_name + "_"
    object_dll = cdll.LoadLibrary(object_path)
    func = getattr(object_dll, func_name)
    # test
    args = [
        3.4e304,
        1.7e308,
        1.2e306,
        0.0
    ]
    cargs = [c_double(arg) for arg in args]
    crefs = [byref(carg) for carg in cargs]
    func(*crefs)
    nargs = [arg.value for arg in cargs]
