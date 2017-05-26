#ifndef WINDOWS_ENV_CCALL_H
#define WINDOWS_ENV_CCALL_H

#if defined(i386_HOST_ARCH)
#define WINDOWS_ENV_CCALL stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_ENV_CCALL ccall
#else
#define WINDOWS_ENV_CCALL ccall
#warning "Unsupported architecture"
#endif

#endif
