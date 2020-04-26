# -*- Python -*-

import os
import platform
import re
import subprocess
import tempfile

import lit.formats
import lit.util

from lit.llvm import llvm_config
from lit.llvm.subst import ToolSubst
from lit.llvm.subst import FindTool

# Configuration file for the 'lit' test runner.

# name: The name of this test suite.
config.name = 'M2lang'

# testFormat: The test format to use to interpret tests.
#
# For now we require '&&' between commands, until they get globally killed and
# the test runner updated.
config.test_format = lit.formats.ShTest(not llvm_config.use_lit_shell)

# suffixes: A list of file extensions to treat as test files.
config.suffixes = ['.c', '.cpp', '.cppm', '.m', '.mm', '.cu',
                   '.ll', '.cl', '.s', '.S', '.modulemap', '.test', '.rs']

# excludes: A list of directories to exclude from the testsuite. The 'Inputs'
# subdirectories contain auxiliary inputs for various tests in their parent
# directories.
config.excludes = ['Inputs', 'CMakeLists.txt', 'README.txt', 'LICENSE.txt', 'debuginfo-tests']

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)

# test_exec_root: The root path where tests should be run.
config.test_exec_root = os.path.join(config.m2lang_obj_root, 'test')

# TODO Improve code
# In case of a standalone build, use_default_substitutions() fails because the
# tools are not in config.llvm_tools_dir but in config.m2lang_tools_dir.
# The lines below are a verbatim copy of use_default_substitutions() with only
# config.m2lang_tools_dir added.
# llvm_config.use_default_substitutions()
tool_patterns = [
    ToolSubst('FileCheck', unresolved='fatal'),
    # Handle these specially as they are strings searched for during testing.
    ToolSubst(r'\| \bcount\b', command=FindTool(
        'count'), verbatim=True, unresolved='fatal'),
    ToolSubst(r'\| \bnot\b', command=FindTool('not'), verbatim=True, unresolved='fatal')]

config.substitutions.append(('%python', sys.executable))
llvm_config.add_tool_substitutions(tool_patterns,
           [config.llvm_tools_dir, config.m2lang_tools_dir])


# This is in LLVM/utils/lit/llvm/config.py
#llvm_config.use_m2lang()

config.substitutions.append(
    ('%src_include_dir', config.m2lang_src_dir + '/include'))


# Propagate path to symbolizer for ASan/MSan.
llvm_config.with_system_environment(
    ['ASAN_SYMBOLIZER_PATH', 'MSAN_SYMBOLIZER_PATH'])

config.substitutions.append(('%PATH%', config.environment['PATH']))


# For each occurrence of a m2lang tool name, replace it with the full path to
# the build directory holding that tool.  We explicitly specify the directories
# to search to ensure that we get the tools just built and not some random
# tools that might happen to be in the user's PATH.
tool_dirs = [config.m2lang_tools_dir, config.llvm_tools_dir]

tools = [
    'm2lang-tblgen',
    ToolSubst('%m2lang_extdef_map', command=FindTool(
        'm2lang-extdef-mapping'), unresolved='ignore'),
]

llvm_config.add_tool_substitutions(tools, tool_dirs)

config.substitutions.append(
    ('%hmaptool', "'%s' %s" % (config.python_executable,
                             os.path.join(config.m2lang_tools_dir, 'hmaptool'))))

# Plugins (loadable modules)
# TODO: This should be supplied by Makefile or autoconf.
if sys.platform in ['win32', 'cygwin']:
    has_plugins = config.enable_shared
else:
    has_plugins = True

if has_plugins and config.llvm_plugin_ext:
    config.available_features.add('plugins')

# As of 2011.08, crash-recovery tests still do not pass on FreeBSD.
if platform.system() not in ['FreeBSD']:
    config.available_features.add('crash-recovery')

# ANSI escape sequences in non-dumb terminal
if platform.system() not in ['Windows']:
    config.available_features.add('ansi-escape-sequences')

# Capability to print utf8 to the terminal.
# Windows expects codepage, unless Wide API.
if platform.system() not in ['Windows']:
    config.available_features.add('utf8-capable-terminal')

# Support for libgcc runtime. Used to rule out tests that require
# m2lang to run with -rtlib=libgcc.
if platform.system() not in ['Darwin', 'Fuchsia']:
    config.available_features.add('libgcc')

# Case-insensitive file system


def is_filesystem_case_insensitive():
    handle, path = tempfile.mkstemp(
        prefix='case-test', dir=config.test_exec_root)
    isInsensitive = os.path.exists(
        os.path.join(
            os.path.dirname(path),
            os.path.basename(path).upper()
        ))
    os.close(handle)
    os.remove(path)
    return isInsensitive


if is_filesystem_case_insensitive():
    config.available_features.add('case-insensitive-filesystem')

# Tests that require the /dev/fd filesystem.
if os.path.exists('/dev/fd/0') and sys.platform not in ['cygwin']:
    config.available_features.add('dev-fd-fs')

# Not set on native MS environment.
if not re.match(r'.*-(windows-msvc)$', config.target_triple):
    config.available_features.add('non-ms-sdk')

# Not set on native PS4 environment.
if not re.match(r'.*-scei-ps4', config.target_triple):
    config.available_features.add('non-ps4-sdk')

# [PR8833] LLP64-incompatible tests
if not re.match(r'^x86_64.*-(windows-msvc|windows-gnu)$', config.target_triple):
    config.available_features.add('LP64')

# [PR12920] "m2lang-driver" -- set if gcc driver is not used.
if not re.match(r'.*-(cygwin)$', config.target_triple):
    config.available_features.add('m2lang-driver')

# [PR18856] Depends to remove opened file. On win32, a file could be removed
# only if all handles were closed.
if platform.system() not in ['Windows']:
    config.available_features.add('can-remove-opened-file')


def calculate_arch_features(arch_string):
    features = []
    for arch in arch_string.split():
        features.append(arch.lower() + '-registered-target')
    return features


llvm_config.feature_config(
    [('--assertion-mode', {'ON': 'asserts'}),
     ('--cxxflags', {r'-D_GLIBCXX_DEBUG\b': 'libstdcxx-safe-mode'}),
        ('--targets-built', calculate_arch_features)
     ])

if lit.util.which('xmllint'):
    config.available_features.add('xmllint')

if config.enable_backtrace:
    config.available_features.add('backtrace')

# Check if we should allow outputs to console.
run_console_tests = int(lit_config.params.get('enable_console', '0'))
if run_console_tests != 0:
    config.available_features.add('console')

lit.util.usePlatformSdkOnDarwin(config, lit_config)
macOSSDKVersion = lit.util.findPlatformSdkVersionOnMacOS(config, lit_config)
if macOSSDKVersion is not None:
    config.available_features.add('macos-sdk-' + macOSSDKVersion)

if os.path.exists('/etc/gentoo-release'):
    config.available_features.add('gentoo')
