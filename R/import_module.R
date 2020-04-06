# This version of import module is used by gymnast internally when built as
# a package. It is designed to be overriden in the global environment if
# using install_module_imports() from the bootstrap module.
import_module <- modules::use
