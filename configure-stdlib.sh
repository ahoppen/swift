# ninja llvm-tblgen clang-headers

mkdir -p /home/build-user/build/stdlib
cd /home/build-user/build/stdlib

# -DLLVM_ENABLE_LLD=ON \


cmake \
-DCMAKE_BUILD_TYPE:STRING=Release \
-DSWIFT_PATH_TO_CMARK_SOURCE=/home/build-user/src/cmark \
-DSWIFT_PATH_TO_CMARK_BUILD=/home/build-user/build/releaseassert/tools/cmark \
-DSWIFT_NATIVE_SWIFT_TOOLS_PATH=/home/build-user/build/releaseassert/bin \
-DLLVM_DIR=/home/build-user/build/releaseassert/lib/cmake/llvm \
-DClang_DIR=/home/build-user/build/releaseassert/lib/cmake/clang \
-DSWIFT_INCLUDE_TOOLS=FALSE \
-DSWIFT_BUILD_PERF_TESTSUITE=NO \
-DSWIFT_INCLUDE_DOCS=NO \
-DSWIFT_BUILD_SOURCEKIT=NO \
-DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=NO \
-DSWIFT_STDLIB_BUILD_TYPE=Release \
-DSWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS=FALSE \
-DSWIFT_NATIVE_CLANG_TOOLS_PATH:PATH=/home/build-user/build/releaseassert/bin \
-DSWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY:BOOL=TRUE \
-DCMAKE_CXX_COMPILER=clang++ \
-DCMAKE_C_COMPILER=clang \
-DSWIFT_HOST_VARIANT=linux \
-DSWIFT_HOST_VARIANT_SDK=LINUX \
-DSWIFT_HOST_VARIANT_ARCH=x86_64 \
-DSWIFT_PATH_TO_LIBDISPATCH_SOURCE:PATH=/home/build-user/src/swift-corelibs-libdispatch \
-GNinja \
/home/build-user/src/swift

# run_shell_cmd('ln -sf ../../../../Release/lib/lib_InternalSwiftSyntaxParser.so stdlib-release/lib/swift/linux', cwd=nbuild_dir)

# reflection_test_name = "swift-reflection-test-linux-x86_64"
# for config in build_configs:
ln -sf /home/build-user/build/stdlib/lib/swift /home/build-user/build/releaseassert/lib/swift
ln -sf /home/build-user/build/stdlib/include/swift /home/build-user/build/releaseassert/include/swift
# run_shell_cmd('ln -sf ../../stdlib-release/bin/' + reflection_test_name + ' ' + config + '/bin', cwd=nbuild_dir)
# run_shell_cmd('ln -sfh ../../../%s/swift/include/swift-c/SyntaxParser lib/swift/_InternalSwiftSyntaxParser' % rel_src, cwd=build_dir)

cd /home/build-user/build/stdlib
# build_swift(ninja, nbuild_dir, 'ra', ninja_args=['swift-frontend', 'clang', 'clang-headers', 'compiler-rt'])

ninja stdlib #swift-reflection-test-linux-x86_64