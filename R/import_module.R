# This version of import module is used by gymnast internally when built as
# a package. It is designed to be overriden in the global environment if
# using install_module_imports() from the bootstrap module.
# NOTE: import_module() isn't exactly the same as modules::use(), despite what
# you see here. import_module() imports a module and also imports the function
# import_module() to the new module. Thus, functions within the imported module
# can use import_module() as well. Using modules::use() instead of import_module()
# for modules that use import_module() will cause the module to break.
import_module <- modules::use
